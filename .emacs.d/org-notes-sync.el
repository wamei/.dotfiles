;;; org-notes-sync.el --- Apple メモとの同期を保存から蹴る -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; 本体は tools/org-notes-sync の CLI で、ここは保存をきっかけに蹴るだけ。
;; 対象は org-directory 配下で先頭に `#+notes_sync: yes' を書いたメモ (opt-in)。
;;
;; - CLI を Emacs の子プロセスとして動かさず、launchd のジョブを kickstart する。
;;   子プロセスだと TCC (フルディスクアクセス / オートメーション) の判定を受ける
;;   のは起動した Emacs.app で、Emacs.app を更新するたびに許可が外れる。launchd
;;   から動かせば判定されるのは証明書で署名した CLI 自身だけになる
;; - kickstart の前に state ディレクトリへ pending を置く。ジョブが止まっていれば
;;   起動した CLI が冒頭で pending を消すので 1 周、動いていれば kickstart は何も
;;   せず、走っている CLI が終わり際に pending を拾ってもう 1 周する
;;   (tools/org-notes-sync/src/lock.ts)。どちらでも保存を取りこぼさない
;; - CLI の stderr は launchd が launchd.err に追記する。kickstart した時点の
;;   大きさを覚えておき、ジョブが止まったら (`launchctl print' を間隔をあけて
;;   見る) それより後ろの警告行を出す
;; - tty での画像表示は後日 (GUI だけ inline 画像を出す)
;;
;;; Code:

(require 'org)

(defvar wamei/org-notes-sync-label "jp.wamei.org-notes-sync"
  "org-notes-sync の launchd ジョブのラベル。")

(defvar wamei/org-notes-sync-state-directory
  (expand-file-name "~/.local/state/org-notes-sync/")
  "CLI の state ディレクトリ。pending と launchd.err がここにある。")

(defvar wamei/org-notes-sync-poll-interval 1
  "ジョブの終了を見る間隔 (秒)。")

(defvar wamei/org-notes-sync-timeout 600
  "ジョブの終了をこの秒数まで待つ。過ぎたら知らせて見張りをやめる。")

(defvar wamei/org-notes-sync--watch nil
  "ジョブの終了を見張っている間の状態。
(:timer TIMER :offset 起動時の launchd.err の大きさ :deadline 期限の float-time)。")

(defun wamei/org-notes-sync--target ()
  "launchctl に渡すジョブの指定。"
  (format "gui/%d/%s" (user-uid) wamei/org-notes-sync-label))

(defun wamei/org-notes-sync--err-file ()
  "launchd が CLI の stderr を追記するファイル。"
  (expand-file-name "launchd.err" wamei/org-notes-sync-state-directory))

(defun wamei/org-notes-sync--launchctl (&rest args)
  "launchctl を ARGS で同期実行し、(終了コード . 出力) を返す。"
  (with-temp-buffer
    (let ((status (apply #'call-process "launchctl" nil t nil args)))
      (cons status (buffer-string)))))

(defun wamei/org-notes-sync--parse-print (output)
  "`launchctl print' の OUTPUT から (:state STATE :exit-code CODE) を読む。
入れ子の辞書にも state があるので、タブ 1 つで始まる最上位の行だけを見る。
まだ 1 回も終わっていなければ CODE は nil。"
  (list :state (and (string-match "^\tstate = \\(.+\\)$" output)
                    (match-string 1 output))
        :exit-code (and (string-match "^\tlast exit code = \\(-?[0-9]+\\)" output)
                        (string-to-number (match-string 1 output)))))

(defun wamei/org-notes-sync--file-size (file)
  "FILE の大きさ (バイト)。無ければ 0。"
  (or (file-attribute-size (file-attributes file)) 0))

(defun wamei/org-notes-sync--new-warnings (file offset)
  "FILE の OFFSET バイト目より後ろにある警告行 (`org-notes-sync: ' 始まり) のリスト。
FILE が OFFSET より短ければ切り詰められたとみなして頭から読む。"
  (when (file-readable-p file)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents
         file nil (if (> offset (wamei/org-notes-sync--file-size file)) 0 offset)))
      (let (lines)
        (while (re-search-forward "^org-notes-sync: .*$" nil t)
          (push (match-string 0) lines))
        (nreverse lines)))))

(defun wamei/org-notes-sync--stop-watch ()
  "ジョブの見張りをやめる。"
  (when-let* ((timer (plist-get wamei/org-notes-sync--watch :timer)))
    (cancel-timer timer))
  (setq wamei/org-notes-sync--watch nil))

(defun wamei/org-notes-sync--poll ()
  "ジョブが止まっていれば、起動後に出た警告を知らせて見張りをやめる。"
  (when wamei/org-notes-sync--watch
    (let* ((status (wamei/org-notes-sync--parse-print
                    (cdr (wamei/org-notes-sync--launchctl
                          "print" (wamei/org-notes-sync--target)))))
           (err (wamei/org-notes-sync--err-file)))
      (cond
       ((equal (plist-get status :state) "not running")
        (let ((warnings (wamei/org-notes-sync--new-warnings
                         err (plist-get wamei/org-notes-sync--watch :offset)))
              (code (plist-get status :exit-code)))
          (wamei/org-notes-sync--stop-watch)
          (dolist (line warnings)
            (message "%s" line))
          ;; 警告行も無いのに失敗したときは、黙って壊れないよう必ず知らせる
          (when (and (null warnings) code (/= code 0))
            (message "org-notes-sync: 異常終了しました (exit %d)。%s を確認してください"
                     code err))))
       ((> (float-time) (plist-get wamei/org-notes-sync--watch :deadline))
        (wamei/org-notes-sync--stop-watch)
        (message "org-notes-sync: %d 秒経っても同期が終わりません。%s を確認してください"
                 wamei/org-notes-sync-timeout err))))))

(defun org-notes-sync ()
  "launchd の org-notes-sync ジョブに 1 回同期させる。
走っている最中なら、そのジョブが終わり際にもう 1 周する。"
  (interactive)
  (make-directory wamei/org-notes-sync-state-directory t)
  (let ((offset (wamei/org-notes-sync--file-size (wamei/org-notes-sync--err-file))))
    (write-region "" nil (expand-file-name "pending" wamei/org-notes-sync-state-directory)
                  nil 'silent)
    (let ((result (wamei/org-notes-sync--launchctl
                   "kickstart" (wamei/org-notes-sync--target))))
      (if (/= (car result) 0)
          (message "org-notes-sync: launchd に %s がありません (%s)。mise run org-notes-sync:enable で読み込んでください"
                   wamei/org-notes-sync-label (string-trim (cdr result)))
        (unless wamei/org-notes-sync--watch
          (setq wamei/org-notes-sync--watch
                (list :offset offset
                      :deadline (+ (float-time) wamei/org-notes-sync-timeout)
                      :timer (run-at-time wamei/org-notes-sync-poll-interval
                                          wamei/org-notes-sync-poll-interval
                                          #'wamei/org-notes-sync--poll))))))))

(defun wamei/org-notes-sync-buffer-p (&optional buffer)
  "BUFFER (省略時は現在) が同期対象の org メモなら non-nil。
リモートのファイルと org-directory 未設定は対象外。"
  (with-current-buffer (or buffer (current-buffer))
    (and buffer-file-name
         org-directory
         (not (file-remote-p buffer-file-name))
         (file-in-directory-p buffer-file-name org-directory)
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (let ((case-fold-search t))
               ;; CRLF のファイルでも合うよう行末の \r を許す
               (re-search-forward "^#\\+notes_sync:[ \t]*yes[ \t\r]*$"
                                  (min (point-max) 4097) t)))))))

(defvar-local wamei/org-notes-sync--viewing nil
  "このバッファで自動 revert・inline 画像を有効にしたら non-nil。")

(defun wamei/org-notes-sync--enable-view ()
  "自動 revert と inline 画像 (GUI) を有効にする。2 回目以降は何もしない。
Notes 側の変更が CLI でファイルに書き戻されるので auto-revert で追従する。"
  (unless wamei/org-notes-sync--viewing
    (setq wamei/org-notes-sync--viewing t)
    (auto-revert-mode 1)
    (when (display-graphic-p)
      (setq-local org-image-actual-width '(480))
      (wamei/org-notes-sync--display-images)
      (add-hook 'after-revert-hook #'wamei/org-notes-sync--display-images nil t))))

(defun wamei/org-notes-sync--display-images ()
  "inline 画像を表示する。画像リンクはファイルからの相対パスなので、
ファイルの場所を基準に解決する (プロジェクトのメモとして開くと
default-directory はプロジェクトの root を向いている)。"
  (let ((default-directory (file-name-directory buffer-file-name)))
    (org-display-inline-images)))

(defun wamei/org-notes-sync--after-save ()
  "同期対象のメモなら保存後に org-notes-sync を蹴る。
after-save-hook に全体で 1 回だけ入れ、対象かどうかは保存のたびに見る
(開いた後で `#+notes_sync: yes' を書いても開き直さずに効く)。"
  (when (and (derived-mode-p 'org-mode) (wamei/org-notes-sync-buffer-p))
    (wamei/org-notes-sync--enable-view)
    (org-notes-sync)))

(defun wamei/org-notes-sync--setup-buffer ()
  "同期対象のメモなら、開いたときに自動 revert・inline 画像 (GUI) を有効にする。"
  (when (wamei/org-notes-sync-buffer-p)
    (wamei/org-notes-sync--enable-view)))

(defun wamei/org-notes-sync-setup ()
  "保存で同期を蹴り、同期対象のメモを開いたら表示を整えるフックを入れる。"
  (add-hook 'after-save-hook #'wamei/org-notes-sync--after-save)
  (add-hook 'org-mode-hook #'wamei/org-notes-sync--setup-buffer))

(provide 'org-notes-sync)
;;; org-notes-sync.el ends here
