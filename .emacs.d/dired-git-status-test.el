;;; dired-git-status-test.el --- tests for dired-git-status -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-git-status-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(package-initialize)
(require 'project)
(require 'vc)
(load (expand-file-name "dired-git-status.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun wamei/dired-git-status-test--alist (table)
  "TABLE をソート済み alist にする。"
  (let (acc)
    (maphash (lambda (k v) (push (cons k v) acc)) table)
    (sort acc (lambda (a b) (string< (car a) (car b))))))

;;; フィクスチャ

(defmacro wamei/dired-git-status-test--with-temp-dir (var &rest body)
  "一時ディレクトリを VAR に束縛して BODY を評価し、後で削除する。"
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "dgs-" t))))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(defmacro wamei/dired-git-status-test--with-dired (dir var &rest body)
  "DIR の dired バッファを VAR に束縛して BODY を評価し、後で kill する。
BODY の途中で assertion が失敗しても unwind-protect で必ず kill される。"
  (declare (indent 2))
  `(let ((,var (dired-noselect ,dir)))
     (unwind-protect
         (with-current-buffer ,var
           ,@body)
       (kill-buffer ,var))))

;;; XY → state

(ert-deftest wamei/dired-git-status-code-to-state ()
  (should (eq (wamei/dired-git-status--code-to-state "??") 'untracked))
  (should (eq (wamei/dired-git-status--code-to-state " M") 'modified))
  (should (eq (wamei/dired-git-status--code-to-state "M ") 'modified))
  (should (eq (wamei/dired-git-status--code-to-state " D") 'modified))
  (should (eq (wamei/dired-git-status--code-to-state "A ") 'added))
  (should (eq (wamei/dired-git-status--code-to-state "AM") 'added))
  (should (eq (wamei/dired-git-status--code-to-state "R ") 'renamed))
  (should (eq (wamei/dired-git-status--code-to-state "UU") 'conflict))
  (should (eq (wamei/dired-git-status--code-to-state "AA") 'conflict))
  (should-not (wamei/dired-git-status--code-to-state "!!")))

;;; パース

(ert-deftest wamei/dired-git-status-parse-handles-z-separated-entries ()
  (let ((table (wamei/dired-git-status--parse
                (concat " M src/a.el\0?? new.txt\0A  b.el\0")
                "/r/")))
    (should (equal (wamei/dired-git-status-test--alist table)
                   '(("/r/b.el" . added)
                     ("/r/new.txt" . untracked)
                     ("/r/src/a.el" . modified))))))

(ert-deftest wamei/dired-git-status-parse-rename-uses-new-path ()
  ;; -z ではリネームは "R  新\0旧\0" の順で来る
  (let ((table (wamei/dired-git-status--parse "R  new.el\0old.el\0" "/r")))
    (should (equal (wamei/dired-git-status-test--alist table)
                   '(("/r/new.el" . renamed))))))

(ert-deftest wamei/dired-git-status-parse-ignores-empty-output ()
  (should (= (hash-table-count (wamei/dired-git-status--parse "" "/r")) 0)))

;;; 伝播

(ert-deftest wamei/dired-git-status-propagate-marks-ancestors-modified ()
  (let* ((table (wamei/dired-git-status--parse " M src/deep/a.el\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should (equal (wamei/dired-git-status-test--alist out)
                   '(("/r/src" . modified)
                     ("/r/src/deep" . modified)
                     ("/r/src/deep/a.el" . modified))))))

(ert-deftest wamei/dired-git-status-propagate-conflict-wins ()
  (let* ((table (wamei/dired-git-status--parse " M src/a.el\0UU src/b.el\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should (eq (gethash "/r/src" out) 'conflict))))

(ert-deftest wamei/dired-git-status-propagate-does-not-mark-root ()
  (let* ((table (wamei/dired-git-status--parse "?? x.txt\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should-not (gethash "/r" out))))

;;; 描画とルート判定

(ert-deftest wamei/dired-git-status-root-is-nil-outside-git ()
  (wamei/dired-git-status-test--with-temp-dir dir
    (wamei/dired-git-status-test--with-dired dir buf
      (should-not (wamei/dired-git-status--root)))))

(ert-deftest wamei/dired-git-status-root-is-nil-for-remote-directory ()
  (with-temp-buffer
    (setq default-directory "/ssh:example.invalid:/tmp/")
    (should-not (wamei/dired-git-status--root))))

(ert-deftest wamei/dired-git-status-decorate-puts-face-on-filename ()
  (wamei/dired-git-status-test--with-temp-dir dir
    (write-region "" nil (expand-file-name "a.el" dir))
    (write-region "" nil (expand-file-name "b.el" dir))
    (wamei/dired-git-status-test--with-dired dir buf
      (let ((table (make-hash-table :test 'equal)))
        (puthash (expand-file-name "a.el" dir) 'modified table)
        (wamei/dired-git-status--decorate table)
        (dired-goto-file (expand-file-name "a.el" dir))
        (should (seq-find (lambda (ov) (overlay-get ov 'wamei/dired-git-status-overlay))
                          (overlays-at (point))))
        (should (eq (overlay-get (seq-find (lambda (ov) (overlay-get ov 'wamei/dired-git-status-overlay))
                                           (overlays-at (point)))
                                 'face)
                    'wamei/dired-git-status-modified))
        (dired-goto-file (expand-file-name "b.el" dir))
        (should-not (seq-find (lambda (ov) (overlay-get ov 'wamei/dired-git-status-overlay))
                              (overlays-at (point))))))))

(ert-deftest wamei/dired-git-status-decorate-replaces-old-overlays ()
  (wamei/dired-git-status-test--with-temp-dir dir
    (write-region "" nil (expand-file-name "a.el" dir))
    (wamei/dired-git-status-test--with-dired dir buf
      (let ((table (make-hash-table :test 'equal)))
        (puthash (expand-file-name "a.el" dir) 'modified table)
        (wamei/dired-git-status--decorate table)
        (wamei/dired-git-status--decorate (make-hash-table :test 'equal))
        (should-not (seq-find (lambda (ov) (overlay-get ov 'wamei/dired-git-status-overlay))
                              (overlays-in (point-min) (point-max))))))))

(ert-deftest wamei/dired-git-status-fetch-colors-modified-file-in-real-repo ()
  "実際に git init したリポジトリで非同期取得が終わるまで待ち、色が付くこと。"
  (skip-unless (executable-find "git"))
  (wamei/dired-git-status-test--with-temp-dir dir
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" "-q")
      (write-region "x" nil (expand-file-name "tracked.el" dir))
      (call-process "git" nil nil nil "add" "tracked.el")
      (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
      (write-region "y" nil (expand-file-name "tracked.el" dir))
      (write-region "" nil (expand-file-name "new.el" dir))
      (wamei/dired-git-status-test--with-dired dir buf
        (wamei/dired-git-status-mode 1)
        (let ((deadline (+ (float-time) 5)))
          (while (and (< (float-time) deadline)
                      (not (gethash (directory-file-name dir) wamei/dired-git-status--cache)))
            (accept-process-output nil 0.1)))
        (let ((table (gethash (directory-file-name dir) wamei/dired-git-status--cache)))
          (should table)
          (should (eq (gethash (expand-file-name "tracked.el" dir) table) 'modified))
          (should (eq (gethash (expand-file-name "new.el" dir) table) 'untracked)))))))

;;; 伝播 (フォルダの色)

(ert-deftest wamei/dired-git-status-propagate-uniform-untracked-dir-is-untracked ()
  (let* ((table (wamei/dired-git-status--parse "?? src/a.el\0?? src/deep/b.el\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should (eq (gethash "/r/src" out) 'untracked))
    (should (eq (gethash "/r/src/deep" out) 'untracked))))

(ert-deftest wamei/dired-git-status-propagate-uniform-added-dir-is-added ()
  (let* ((table (wamei/dired-git-status--parse "A  src/a.el\0A  src/b.el\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should (eq (gethash "/r/src" out) 'added))))

(ert-deftest wamei/dired-git-status-propagate-mixed-dir-is-modified ()
  (let* ((table (wamei/dired-git-status--parse "?? src/x/a.el\0 M src/b.el\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    ;; x の中は untracked だけなので untracked、src は混在なので modified
    (should (eq (gethash "/r/src/x" out) 'untracked))
    (should (eq (gethash "/r/src" out) 'modified))))

;;; 更新契機 (実リポジトリ)

(defun wamei/dired-git-status-test--init-repo (dir)
  "DIR を git リポジトリにして tracked.el を 1 つコミットする。"
  (let ((default-directory dir))
    (call-process "git" nil nil nil "init" "-q")
    (write-region "x" nil (expand-file-name "tracked.el" dir))
    (call-process "git" nil nil nil "add" "tracked.el")
    (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                  "commit" "-q" "-m" "init")))

(defun wamei/dired-git-status-test--wait-until (pred &optional seconds)
  "PRED が非 nil を返すまで最大 SECONDS (既定 5) 秒、プロセス出力とタイマーを回す。"
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (< (float-time) deadline) (not (funcall pred)))
      ;; プロセスの出力と file-notify の通知を受け、sit-for でタイマーも進める
      (accept-process-output nil 0.1)
      (sit-for 0.05))
    (funcall pred)))

(defun wamei/dired-git-status-test--state (dir file)
  "DIR のルートのキャッシュにおける FILE の state。"
  (when-let* ((table (gethash (directory-file-name dir) wamei/dired-git-status--cache)))
    (gethash (expand-file-name file dir) table)))

(ert-deftest wamei/dired-git-status-revert-refetches-status ()
  "auto-revert 等で dired が revert されたら、キャッシュがあっても再取得する (D)。"
  (skip-unless (executable-find "git"))
  (wamei/dired-git-status-test--with-temp-dir dir
    (wamei/dired-git-status-test--init-repo dir)
    (wamei/dired-git-status-test--with-dired dir buf
      (wamei/dired-git-status-mode 1)
      (should (wamei/dired-git-status-test--wait-until
               (lambda () (gethash (directory-file-name dir) wamei/dired-git-status--cache))))
      (write-region "" nil (expand-file-name "new.el" dir))
      (revert-buffer)
      (should (wamei/dired-git-status-test--wait-until
               (lambda () (eq (wamei/dired-git-status-test--state dir "new.el") 'untracked)))))))

(ert-deftest wamei/dired-git-status-save-refetches-status ()
  "Emacs でファイルを保存したら、そのルートを見ている dired の色を再取得する (A)。"
  (skip-unless (executable-find "git"))
  (wamei/dired-git-status-test--with-temp-dir dir
    (wamei/dired-git-status-test--init-repo dir)
    (wamei/dired-git-status-test--with-dired dir buf
      (wamei/dired-git-status-mode 1)
      (should (wamei/dired-git-status-test--wait-until
               (lambda () (gethash (directory-file-name dir) wamei/dired-git-status--cache))))
      (should-not (wamei/dired-git-status-test--state dir "tracked.el"))
      (let ((fbuf (find-file-noselect (expand-file-name "tracked.el" dir))))
        (unwind-protect
            (with-current-buffer fbuf
              (goto-char (point-max))
              (insert "y")
              (let ((inhibit-message t)) (save-buffer)))
          (kill-buffer fbuf)))
      (should (wamei/dired-git-status-test--wait-until
               (lambda () (eq (wamei/dired-git-status-test--state dir "tracked.el") 'modified)))))))

(ert-deftest wamei/dired-git-status-git-dir-change-refetches-status ()
  "git add など .git の中身が変わったら再取得する (B)。
file-notify のイベントは command loop 経由で届き batch では配送されないので、
監視の登録を確かめたうえで、通知ハンドラを直接呼んで debounce → 再取得を検証する。"
  (skip-unless (executable-find "git"))
  (wamei/dired-git-status-test--with-temp-dir dir
    (wamei/dired-git-status-test--init-repo dir)
    (write-region "" nil (expand-file-name "new.el" dir))
    (wamei/dired-git-status-test--with-dired dir buf
      (wamei/dired-git-status-mode 1)
      (should (wamei/dired-git-status-test--wait-until
               (lambda () (eq (wamei/dired-git-status-test--state dir "new.el") 'untracked))))
      (let ((root (directory-file-name dir)))
        (should (gethash root wamei/dired-git-status--git-watches))
        (let ((default-directory dir))
          (call-process "git" nil nil nil "add" "new.el"))
        (wamei/dired-git-status--git-dir-changed root)
        (should (wamei/dired-git-status--test-timer-pending-p root))
        (should (wamei/dired-git-status-test--wait-until
                 (lambda () (eq (wamei/dired-git-status-test--state dir "new.el") 'added))))
        ;; バッファを閉じたら監視も外れる
        (kill-buffer buf)
        (should-not (gethash root wamei/dired-git-status--git-watches))))))

(ert-deftest wamei/dired-git-status-git-dir-event-ignores-lock-files ()
  "`git status' 自身が .git/index.lock を作って消すので、lock の出入りで再取得すると
git status → index.lock → 通知 → git status の自己持続ループになる。debounce は
間隔を空けるだけで止められない。"
  (should-not (wamei/dired-git-status--git-dir-event-p
               (list 1 'created "/r/.git/index.lock")))
  (should-not (wamei/dired-git-status--git-dir-event-p
               (list 1 'deleted "/r/.git/index.lock")))
  (should-not (wamei/dired-git-status--git-dir-event-p
               (list 1 'created "/r/.git/HEAD.lock"))))

(ert-deftest wamei/dired-git-status-git-dir-event-ignores-stopped ()
  "`stopped' は監視が終わった通知で、.git の変化ではない。"
  (should-not (wamei/dired-git-status--git-dir-event-p
               (list 1 'stopped "/r/.git"))))

(ert-deftest wamei/dired-git-status-git-dir-event-keeps-real-changes ()
  "lock でない実体の変化は取りこぼさない。index.lock からの rename も index の変化。"
  (should (wamei/dired-git-status--git-dir-event-p
           (list 1 'created "/r/.git/COMMIT_EDITMSG")))
  (should (wamei/dired-git-status--git-dir-event-p
           (list 1 'changed "/r/.git/HEAD")))
  (should (wamei/dired-git-status--git-dir-event-p
           (list 1 'renamed "/r/.git/index.lock" "/r/.git/index"))))

(defun wamei/dired-git-status--test-timer-pending-p (root)
  "ROOT の debounce タイマーが予約されているか。"
  (timerp (gethash root wamei/dired-git-status--git-timers)))

(provide 'dired-git-status-test)
;;; dired-git-status-test.el ends here
