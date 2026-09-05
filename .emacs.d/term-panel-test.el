;;; term-panel-test.el --- tests for term-panel -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l term-panel-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "term-panel.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(wamei/term-panel-setup)

;;; フィクスチャ

(defvar wamei/term-panel-test--roots nil
  "テスト中に transient プロジェクトとして扱うディレクトリ。")

(defun wamei/term-panel-test--find-project (dir)
  "`project-find-functions' 用。DIR を含むテスト用ルートを transient プロジェクトにする。"
  (seq-some (lambda (root)
              (when (string-prefix-p root (file-truename (expand-file-name dir)))
                (cons 'transient root)))
            wamei/term-panel-test--roots))

(defun wamei/term-panel-test--fake-vterm (name)
  "vterm の代わり。NAME のバッファを作って `default-directory' を引き継ぎ、切り替える。"
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create name)
      (setq default-directory dir)
      (switch-to-buffer (current-buffer)))))

(defmacro wamei/term-panel-test--with-projects (vars &rest body)
  "VARS のそれぞれを一時ディレクトリの transient プロジェクトに束縛して BODY を評価する。
ディレクトリ名末尾 (プロジェクト名) は変数名になる。端末は vterm を使わず空バッファで代える。"
  (declare (indent 1))
  `(let* ((base (file-name-as-directory (file-truename (make-temp-file "term-panel-" t))))
          ,@(mapcar (lambda (var)
                      `(,var (file-name-as-directory
                              (expand-file-name ,(symbol-name var) base))))
                    vars)
          (wamei/term-panel-test--roots (list ,@vars))
          (project-find-functions (list #'wamei/term-panel-test--find-project))
          (wamei/term--last nil)
          (wamei/term--previous-window nil)
          (wamei/term--previous-buffer nil))
     (unwind-protect
         (cl-letf (((symbol-function 'vterm) #'wamei/term-panel-test--fake-vterm))
           ,@(mapcar (lambda (var) `(make-directory ,var t)) vars)
           ,@body)
       (dolist (buf (buffer-list))
         (when (string-match-p "\\`\\*term\\(?:: \\|inals\\)" (buffer-name buf))
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buf))))
       (delete-other-windows)
       (delete-directory base t))))

(defmacro wamei/term-panel-test--in (root &rest body)
  "ROOT のバッファにいるつもりで BODY を評価する。"
  (declare (indent 1))
  `(with-temp-buffer
     (setq default-directory ,root)
     ,@body))

(defun wamei/term-panel-test--list-entries (list-buffer)
  "LIST-BUFFER の各行が指す端末バッファ名。"
  (with-current-buffer list-buffer
    (save-excursion
      (goto-char (point-min))
      (let (names)
        (while (not (eobp))
          (when-let* ((buffer (get-text-property (point) 'wamei/term-buffer)))
            (push (buffer-name buffer) names))
          (forward-line 1))
        (nreverse names)))))

;;; バッファ名

(ert-deftest wamei/term-panel-buffer-name-uses-project-and-index ()
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (should (equal (wamei/term--buffer-name 1) "*term: alpha*"))
      (should (equal (wamei/term--buffer-name 3) "*term: alpha 3*")))))

(ert-deftest wamei/term-panel-next-index-fills-gap ()
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (wamei/term--create 3)
      (should (= (wamei/term--next-index) 2))
      (should (equal (mapcar #'buffer-name (wamei/term--buffers))
                     '("*term: alpha*" "*term: alpha 3*"))))))

(ert-deftest wamei/term-panel-buffers-ignore-prefix-match-project ()
  (wamei/term-panel-test--with-projects (alpha alphabet)
    (wamei/term-panel-test--in alphabet (wamei/term--create 1))
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (should (equal (mapcar #'buffer-name (wamei/term--buffers))
                     '("*term: alpha*"))))))

;;; 一覧はプロジェクトごと

(ert-deftest wamei/term-panel-list-buffer-is-per-project ()
  (wamei/term-panel-test--with-projects (alpha beta)
    (let ((list-a (wamei/term-panel-test--in alpha (wamei/term--list-buffer)))
          (list-b (wamei/term-panel-test--in beta (wamei/term--list-buffer))))
      (should-not (eq list-a list-b))
      (should (equal (buffer-name list-a) "*terminals: alpha*"))
      (should (equal (buffer-local-value 'default-directory list-a) alpha))
      (should (equal (buffer-local-value 'default-directory list-b) beta)))))

(ert-deftest wamei/term-panel-list-refresh-shows-only-current-project ()
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (wamei/term--create 2)
      (wamei/term--list-refresh))
    (wamei/term-panel-test--in beta
      (wamei/term--create 1)
      (wamei/term--create 2)
      (should (equal (wamei/term-panel-test--list-entries (wamei/term--list-refresh))
                     '("*term: beta*" "*term: beta 2*"))))
    ;; alpha の一覧は beta の操作で変わらない
    (should (equal (wamei/term-panel-test--list-entries
                    (wamei/term-panel-test--in alpha (wamei/term--list-buffer)))
                   '("*term: alpha*" "*term: alpha 2*")))))

(ert-deftest wamei/term-panel-list-refresh-in-list-buffer-uses-its-project ()
  "一覧バッファの中から描き直しても (幅変更の hook)、そのプロジェクトの端末が並ぶ。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in alpha (wamei/term--create 1) (wamei/term--create 2))
    (wamei/term-panel-test--in beta (wamei/term--create 1))
    (let ((list-a (wamei/term-panel-test--in alpha (wamei/term--list-buffer))))
      (with-current-buffer list-a (wamei/term--list-refresh))
      (should (equal (wamei/term-panel-test--list-entries list-a)
                     '("*term: alpha*" "*term: alpha 2*"))))))

(ert-deftest wamei/term-panel-record-title-updates-label ()
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (with-current-buffer (wamei/term--create 2)
        (wamei/term--record-title "make test"))
      (should (string-match-p "2: make test"
                              (with-current-buffer (wamei/term--list-refresh)
                                (buffer-string)))))))

(ert-deftest wamei/term-panel-list-hides-cursor-when-not-selected ()
  "一覧は選択していない window ではカーソルを出さない (sidebar と同じ)。"
  (wamei/term-panel-test--with-projects (alpha)
    (should-not (buffer-local-value 'cursor-in-non-selected-windows
                                    (wamei/term-panel-test--in alpha (wamei/term--list-buffer))))))

(ert-deftest wamei/term-panel-terminal-setup-hides-cursor-when-not-selected ()
  "端末バッファも `wamei/term--setup-buffer' でカーソルを非選択時に隠す。"
  (with-temp-buffer
    (wamei/term--setup-buffer)
    (should-not cursor-in-non-selected-windows)))

;;; パネル (window)

(ert-deftest wamei/term-panel-show-opens-list-for-two-terminals ()
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--show (wamei/term--create 1))
      (should (wamei/term--window))
      (should-not (wamei/term--list-window))
      (wamei/term--show (wamei/term--create 2))
      (should (wamei/term--list-window))
      (should (equal (buffer-name (window-buffer (wamei/term--list-window)))
                     "*terminals: alpha*")))))

(ert-deftest wamei/term-panel-list-window-follows-displayed-project ()
  "表示中の端末が別プロジェクトのものに替わったら、一覧もそのプロジェクトのものに替わる。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (wamei/term--show (wamei/term--create 2)))
    (wamei/term-panel-test--in beta
      (wamei/term--create 1)
      (wamei/term--show (wamei/term--create 2))
      (should (equal (buffer-name (window-buffer (wamei/term--window))) "*term: beta 2*"))
      (should (equal (buffer-name (window-buffer (wamei/term--list-window)))
                     "*terminals: beta*"))
      (should (equal (wamei/term-panel-test--list-entries (window-buffer (wamei/term--list-window)))
                     '("*term: beta*" "*term: beta 2*"))))))

(ert-deftest wamei/term-panel-list-update-from-unrelated-buffer-uses-panel-project ()
  "kill 後のタイマなど無関係なバッファから呼ばれても、パネルの端末のプロジェクトで判断する。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (wamei/term--show (wamei/term--create 2)))
    (wamei/term-panel-test--in beta
      (wamei/term--list-update)
      (should (equal (buffer-name (window-buffer (wamei/term--list-window)))
                     "*terminals: alpha*")))))

(ert-deftest wamei/term-panel-kill-hands-over-and-closes-list ()
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (let ((first (wamei/term--create 1))
            (second (wamei/term--create 2)))
        (wamei/term--show second)
        (with-current-buffer second
          (add-hook 'kill-buffer-hook #'wamei/term--on-kill nil t))
        (kill-buffer second)
        (should (eq (window-buffer (wamei/term--window)) first))
        (wamei/term--list-update)
        (should-not (wamei/term--list-window))))))

(ert-deftest wamei/term-panel-cycle-wraps ()
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (let ((first (wamei/term--create 1))
            (second (wamei/term--create 2)))
        (wamei/term--show first)
        (wamei/term-next)
        (should (eq (window-buffer (wamei/term--window)) second))
        (wamei/term-next)
        (should (eq (window-buffer (wamei/term--window)) first))
        (wamei/term-previous)
        (should (eq (window-buffer (wamei/term--window)) second))))))

(provide 'term-panel-test)
;;; term-panel-test.el ends here
