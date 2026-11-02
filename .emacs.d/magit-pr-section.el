;;; magit-pr-section.el --- magit-status に今のブランチの PR を出す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; magit-status の見出しの下に、今のブランチの GitHub の pull request を
;; 1 section 出す (見出しに番号・タイトル・状態、本文に URL と、base <- head・差分量・
;; チェックの結果・レビューの判定の 1 行)。section の上で RET を押すとブラウザで開く。
;;
;; forge は使わず `gh pr view --json' を非同期に呼ぶ。refresh のたびに待つと
;; status が遅くなるので、結果は (ワークツリー . ブランチ) ごとにキャッシュして
;; 描き、`wamei/magit-pr-ttl' 秒より古いか g (magit-refresh) のときだけ裏で
;; 取り直す。取り終えて中身が変わっていたら status を描き直す。
;; PR が無い・GitHub でない・gh が失敗したときは何も出さない。リモート
;; (TRAMP) のリポジトリでは呼ばない。
;;
;; magit-section のクラスを定義するので magit を読んだ後に読むこと
;; (init.el は magit の :config で読む)。テストは magit-pr-section-test.el。
;;; Code:

(require 'magit-section)

(declare-function magit-toplevel "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-refresh-buffer "magit-mode")

(defvar wamei/magit-pr-command
  '("gh" "pr" "view" "--json"
    "number,title,state,isDraft,url,baseRefName,headRefName,additions,deletions,reviewDecision,statusCheckRollup")
  "今のブランチの PR を JSON で出すコマンド。")

(defvar wamei/magit-pr-ttl 60
  "キャッシュした PR をこの秒数を過ぎたら取り直す。")

(defvar wamei/magit-pr--cache (make-hash-table :test #'equal)
  "(ワークツリー . ブランチ) → (取得時刻 . PR の alist か :none)。")

(defvar wamei/magit-pr--pending (make-hash-table :test #'equal)
  "取得中の (ワークツリー . ブランチ)。同じものを重ねて取りに行かない。")

;;; 整形

(defun wamei/magit-pr--parse (json)
  "gh の出力 JSON を alist にする。null と false は nil。"
  (json-parse-string json :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun wamei/magit-pr--check-outcome (check)
  "statusCheckRollup の要素 CHECK を success / failure / pending / skipped に分ける。
CheckRun は status と conclusion、StatusContext は state を持つ。"
  (let ((state (or (alist-get 'conclusion check)
                   (alist-get 'state check)
                   (alist-get 'status check))))
    (pcase state
      ("SUCCESS" 'success)
      ((or "NEUTRAL" "SKIPPED" "STALE") 'skipped)
      ((or "FAILURE" "ERROR" "CANCELLED" "TIMED_OUT" "ACTION_REQUIRED"
           "STARTUP_FAILURE")
       'failure)
      (_ 'pending))))

(defun wamei/magit-pr--checks (rollup)
  "ROLLUP を数え分けた ((success . n) (failure . n) (pending . n) (skipped . n))。"
  (let ((counts (list (cons 'success 0) (cons 'failure 0)
                      (cons 'pending 0) (cons 'skipped 0))))
    (dolist (check rollup counts)
      (cl-incf (alist-get (wamei/magit-pr--check-outcome check) counts)))))

(defun wamei/magit-pr--state-label (pr)
  "PR の状態。open の draft は DRAFT。"
  (if (and (alist-get 'isDraft pr) (equal (alist-get 'state pr) "OPEN"))
      "DRAFT"
    (alist-get 'state pr)))

(defun wamei/magit-pr--state-face (label)
  (pcase label
    ("OPEN" 'success)
    ("MERGED" 'font-lock-keyword-face)
    ("CLOSED" 'error)
    (_ 'shadow)))

(defun wamei/magit-pr--checks-string (rollup)
  "ROLLUP の件数を ✓2 ✗1 ●1 -1 の形で。0 件の分類は省く。"
  (mapconcat
   (lambda (entry)
     (pcase-let ((`(,mark . ,face) (alist-get (car entry)
                                              '((success "✓" . success)
                                                (failure "✗" . error)
                                                (pending "●" . warning)
                                                (skipped "-" . shadow)))))
       (propertize (format "%s%d" mark (cdr entry)) 'face face)))
   (seq-filter (lambda (entry) (> (cdr entry) 0))
               (wamei/magit-pr--checks rollup))
   " "))

(defun wamei/magit-pr--review-face (decision)
  (pcase decision
    ("APPROVED" 'success)
    ("CHANGES_REQUESTED" 'error)
    (_ 'warning)))

;;; section

(defvar-keymap wamei/magit-pr-section-map
  :doc "PR の section の上のキー。"
  "<remap> <magit-visit-thing>" #'wamei/magit-pr-browse
  ;; URL の文字列は follow-link なので mouse-1 のクリックも mouse-2 になる
  "<mouse-2>" #'wamei/magit-pr-browse-at-mouse)

(defclass wamei/magit-pr-section (magit-section)
  ((keymap :initform 'wamei/magit-pr-section-map)))

(defun wamei/magit-pr-browse ()
  "point の PR をブラウザで開く。"
  (interactive)
  (browse-url (oref (magit-current-section) value)))

(defun wamei/magit-pr-browse-at-mouse (event)
  "EVENT でクリックした先が PR の URL ならブラウザで開く。
magit は section の範囲に section のキーマップを `keymap' プロパティで
上書きするので、URL の文字列だけにキーマップを付けられない。section の
キーマップで受けて、クリック位置の `wamei/magit-pr-url' で判定する。"
  (interactive "e")
  (let* ((posn (event-end event))
         (url (with-current-buffer (window-buffer (posn-window posn))
                (get-text-property (posn-point posn) 'wamei/magit-pr-url))))
    (when url
      (browse-url url))))

(defun wamei/magit-pr--insert (pr)
  "PR の section を挿入する。"
  (let ((url (alist-get 'url pr))
        (label (wamei/magit-pr--state-label pr))
        (rollup (alist-get 'statusCheckRollup pr))
        (review (alist-get 'reviewDecision pr)))
    (magit-insert-section (wamei/magit-pr-section url)
      (magit-insert-heading
        (propertize "Pull request" 'font-lock-face 'magit-section-heading)
        (format " #%d " (alist-get 'number pr))
        (alist-get 'title pr) " "
        (propertize (format "[%s]" label)
                    'font-lock-face (wamei/magit-pr--state-face label)))
      (insert "  " (propertize url 'font-lock-face 'link
                               'wamei/magit-pr-url url
                               'mouse-face 'highlight
                               'follow-link t
                               'help-echo "mouse-1: ブラウザで開く")
              "\n")
      (insert (format "  %s <- %s  " (alist-get 'baseRefName pr)
                      (alist-get 'headRefName pr))
              (propertize (format "+%d" (alist-get 'additions pr))
                          'font-lock-face 'magit-diffstat-added)
              " "
              (propertize (format "-%d" (alist-get 'deletions pr))
                          'font-lock-face 'magit-diffstat-removed))
      (when rollup
        (insert "  " (wamei/magit-pr--checks-string rollup)))
      (when (and review (not (string-empty-p review)))
        (insert "  " (propertize review 'font-lock-face
                                 (wamei/magit-pr--review-face review))))
      (insert "\n\n"))))

;;; 取得

(defun wamei/magit-pr--fetch (dir callback)
  "DIR で `wamei/magit-pr-command' を非同期に走らせ、PR か :none で CALLBACK を呼ぶ。"
  (let* ((default-directory dir)
         (out (generate-new-buffer " *magit-pr*"))
         (err (generate-new-buffer " *magit-pr stderr*")))
    (make-process
     :name "magit-pr"
     :command wamei/magit-pr-command
     :buffer out
     :stderr err
     :noquery t
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (let ((pr (and (eq (process-exit-status proc) 0)
                        (ignore-errors
                          (with-current-buffer out
                            (wamei/magit-pr--parse (buffer-string)))))))
           (kill-buffer out)
           (kill-buffer err)
           (funcall callback (or pr :none))))))))

(defun wamei/magit-pr--refresh-status (dir)
  "DIR のワークツリーの magit-status バッファを描き直す。"
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'magit-status-mode)
                 (file-equal-p default-directory dir))
        ;; g で始めた取得の後に this-command が magit-refresh のまま残っていると
        ;; 描き直しがまた取得を始めてしまう
        (let ((this-command nil))
          (magit-refresh-buffer))))))

(defun wamei/magit-pr--start (key)
  "KEY (ワークツリー . ブランチ) の PR を取りに行く。取得中なら何もしない。"
  (unless (gethash key wamei/magit-pr--pending)
    (puthash key t wamei/magit-pr--pending)
    (wamei/magit-pr--fetch
     (car key)
     (lambda (pr)
       (remhash key wamei/magit-pr--pending)
       (let ((old (cdr (gethash key wamei/magit-pr--cache))))
         (puthash key (cons (float-time) pr) wamei/magit-pr--cache)
         (unless (equal old pr)
           (wamei/magit-pr--refresh-status (car key))))))))

(defun wamei/magit-insert-pull-request ()
  "今のブランチの PR の section を挿入する。`magit-status-sections-hook' 用。
キャッシュを描き、無いか古いか g のときは裏で取り直す。"
  (when-let* (((not (file-remote-p default-directory)))
              ((executable-find (car wamei/magit-pr-command)))
              (dir (magit-toplevel))
              (branch (magit-get-current-branch)))
    (let* ((key (cons dir branch))
           (entry (gethash key wamei/magit-pr--cache)))
      (when (or (null entry)
                (> (- (float-time) (car entry)) wamei/magit-pr-ttl)
                (memq this-command '(magit-refresh magit-refresh-all)))
        (wamei/magit-pr--start key))
      (when (consp (cdr entry))
        (wamei/magit-pr--insert (cdr entry))))))

(provide 'magit-pr-section)
;;; magit-pr-section.el ends here
