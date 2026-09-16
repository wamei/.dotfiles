;;; term-panel-test.el --- tests for term-panel -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l term-panel-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)

;; ghostel 本体の buffer-local 変数。ghostel を読まない batch でも
;; setq-local / buffer-local-value できるよう special にしておく。
(defvar-local ghostel-title nil
  "端末が報告したタイトル (テスト用のスタブ定義)。")

(load (expand-file-name "term-panel.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; タブに紐づいたプロジェクト (wamei/project-tabs-current-root) を使うため。
;; init.el では tab-bar ブロックで読まれる。
(load (expand-file-name "project-tabs.el"
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

(defun wamei/term-panel-test--fake-ghostel-create (&optional name _display _identity)
  "`ghostel-create' の代わり。NAME のバッファを作って `default-directory' を引き継ぐ。"
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create name)
      (setq default-directory dir)
      (current-buffer))))

(defmacro wamei/term-panel-test--with-projects (vars &rest body)
  "VARS のそれぞれを一時ディレクトリの transient プロジェクトに束縛して BODY を評価する。
ディレクトリ名末尾 (プロジェクト名) は変数名になる。端末は ghostel を使わず空バッファで代える。"
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
         (cl-letf (((symbol-function 'ghostel-create)
                    #'wamei/term-panel-test--fake-ghostel-create))
           ,@(mapcar (lambda (var) `(make-directory ,var t)) vars)
           ,@body)
       (dolist (buf (buffer-list))
         (when (string-match-p "\\`\\*term\\(?:: \\|inals\\)" (buffer-name buf))
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buf))))
       (delete-other-windows)
       (delete-directory base t))))

(defmacro wamei/term-panel-test--with-tab-root (root &rest body)
  "カレントタブに ROOT を紐づけて BODY を評価する (project-tabs.el)。
`wamei/project-tabs-set-root' は frame の tabs パラメータを直接書き換えるので、
後始末はパラメータごと捨てる (batch では tab-bar が作り直す)。"
  (declare (indent 1))
  `(unwind-protect
       (progn (wamei/project-tabs-set-root ,root) ,@body)
     (set-frame-parameter nil 'tabs nil)))

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

;;; ghostel のロード

(ert-deftest wamei/term-panel-ghostel-create-is-autoloaded ()
  "ghostel 未ロードのまま端末を作れる。
`ghostel-create' には ghostel 側に autoload cookie が無く、パネルのコマンドは
term-panel.el で defun されているので leaf の `:bind' が張る autoload も
上書きされる。term-panel.el 自身が autoload を張らないと、ghostel を
まだ読んでいないセッションの最初の C-z が void-function で落ちる。"
  (let ((def (symbol-function 'ghostel-create)))
    (should (autoloadp def))
    (should (equal (cadr def) "ghostel"))))

;;; 起点になるプロジェクト

(ert-deftest wamei/term-panel-root-uses-tab-project-outside-project ()
  "プロジェクト外のバッファ (*scratch* など) から呼んでもタブのプロジェクトを起点にする。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--with-tab-root alpha
      (wamei/term-panel-test--in base
        (should (equal (wamei/term--root) alpha))
        (should (equal (wamei/term--buffer-name 1) "*term: alpha*"))))))

(ert-deftest wamei/term-panel-root-prefers-tab-over-buffer-project ()
  "別プロジェクトのファイルを開いていても、タブのプロジェクトの端末を出す。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--with-tab-root alpha
      (wamei/term-panel-test--in beta
        (should (equal (wamei/term--root) alpha))))))

(ert-deftest wamei/term-panel-root-in-panel-buffers-keeps-their-project ()
  "端末と一覧の中では、タブが別プロジェクトでもそのバッファのプロジェクトを見る。
一覧の再描画やタイトル変更 (プロセスフィルタ) はパネルに出ている端末を基準に
動くので、ここでタブに引っぱられると別プロジェクトの一覧を描いてしまう。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in beta (wamei/term--create 1) (wamei/term--create 2))
    (let ((list-b (wamei/term-panel-test--in beta (wamei/term--list-buffer))))
      (wamei/term-panel-test--with-tab-root alpha
        (with-current-buffer "*term: beta*"
          (should (equal (wamei/term--root) beta))
          (should (equal (mapcar #'buffer-name (wamei/term--buffers))
                         '("*term: beta*" "*term: beta 2*"))))
        (with-current-buffer list-b
          (should (equal (wamei/term--root) beta)))))))

(ert-deftest wamei/term-panel-root-falls-back-to-buffer-project-without-tab ()
  "タブにプロジェクトが紐づいていなければ、従来どおりバッファ基準。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (should (equal (wamei/term--root) alpha)))))

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

(ert-deftest wamei/term-panel-label-comes-from-ghostel-title ()
  "一覧のラベルは端末が報告したタイトル (`ghostel-title') を出す。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (with-current-buffer (wamei/term--create 2)
        (setq-local ghostel-title "make test"))
      (should (string-match-p "2: make test"
                              (with-current-buffer (wamei/term--list-refresh)
                                (buffer-string)))))))

(ert-deftest wamei/term-panel-title-change-refreshes-list ()
  "`ghostel-buffer-name-function' として呼ばれると一覧を描き直し、
現在のバッファ名を返す (`ghostel--rename-managed' が必ず no-op になる値)。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (let ((list-buffer (wamei/term--list-buffer))
            (refresh (symbol-function 'wamei/term--list-refresh))
            (calls 0))
        (with-current-buffer (get-buffer "*term: alpha*")
          (setq-local ghostel-title "make test")
          (cl-letf (((symbol-function 'wamei/term--list-refresh)
                     (lambda () (setq calls (1+ calls)) (funcall refresh))))
            (should (equal (wamei/term--on-title-change "make test")
                           "*term: alpha*"))))
        (should (>= calls 1))
        (should (string-match-p "make test"
                                (with-current-buffer list-buffer (buffer-string))))))))

(ert-deftest wamei/term-panel-title-change-ignores-other-buffers ()
  "端末以外のバッファでは一覧を描き直さない。

`wamei/term--on-title-change' の返り値は実装が何をしても一定なので、
返り値ではなく `wamei/term--list-refresh' の呼び出し回数で見る
\(そうしないとプレフィックスのガードを消しても通ってしまう)。
一覧バッファは先に作っておき、ガードのうちバッファ名の判定だけを残す。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (wamei/term--list-buffer))
    (let ((calls 0))
      (cl-letf (((symbol-function 'wamei/term--list-refresh)
                 (lambda () (setq calls (1+ calls)) nil)))
        (wamei/term-panel-test--in alpha
          ;; 端末以外でも返り値は自分のバッファ名 (改名は起きない)
          (should (equal (wamei/term--on-title-change "x") (buffer-name))))
        (should (= calls 0))))))

(ert-deftest wamei/term-panel-title-change-survives-refresh-error ()
  "一覧の再描画が signal しても外へ漏らさない。

`ghostel--set-title' / `ghostel--set-directory' はこの関数の呼び出しを
`condition-case' で包まないので、漏らすと端末の出力処理 (プロセスフィルタ)
の中でエラーになる。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (wamei/term--list-buffer))
    (cl-letf (((symbol-function 'wamei/term--list-refresh)
               (lambda () (error "boom"))))
      (with-current-buffer (get-buffer "*term: alpha*")
        (should (equal (wamei/term--on-title-change "x") "*term: alpha*"))))))

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

;;; 下端揃えの端数

(ert-deftest wamei/term-anchor-vscroll-keeps-the-fraction-on-the-top-row ()
  "ghostel がカーソル行のために 0 にした vscroll を端数へ戻す。"
  (should (= (wamei/term-anchor-vscroll 0 11 500 '(500 7 607)) 7)))

(ert-deftest wamei/term-anchor-vscroll-without-fraction ()
  "本文高さが行高で割り切れているなら払う端数が無い。"
  (should (= (wamei/term-anchor-vscroll 0 0 500 '(500 7 607)) 0)))

(ert-deftest wamei/term-anchor-vscroll-keeps-a-clamped-start ()
  "start が下端揃えの位置と違うなら、カーソル行が上に居るのでそのまま。
ここで端数を払うとカーソル行が切れる (ghostel の `ghostel--anchor-window')。"
  (should (= (wamei/term-anchor-vscroll 0 11 480 '(500 7 607)) 0)))

(ert-deftest wamei/term-anchor-vscroll-keeps-an-unfilled-grid ()
  "中身がウィンドウより短いときは下端揃えでも端数が出ない。"
  (should (= (wamei/term-anchor-vscroll 0 11 500 '(500 0 300)) 0))
  (should (= (wamei/term-anchor-vscroll 0 11 500 nil) 0)))

(ert-deftest wamei/term-anchor-vscroll-passes-other-values-through ()
  "ghostel が 0 以外を要求したときは触らない。"
  (should (= (wamei/term-anchor-vscroll 7 11 500 '(500 7 607)) 7)))

(provide 'term-panel-test)
;;; term-panel-test.el ends here
