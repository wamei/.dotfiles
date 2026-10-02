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

;; タブは header-tabs.el で描く (term-panel.el の (require 'header-tabs) を満たす)。
(load (expand-file-name "header-tabs.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; タブの ● は term-modeline.el の状態から作るので先に読む (term-panel.el の
;; (require 'term-modeline) を満たす)。
(load (expand-file-name "term-modeline.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

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
         (when (string-match-p "\\`\\*term: " (buffer-name buf))
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

(defun wamei/term-panel-test--tabs (string)
  "header-line のタブ文字列 STRING を、タブごとの (端末バッファ名 . 文字列) にする。"
  (let ((pos 0) tabs)
    (while (< pos (length string))
      (let ((next (or (next-single-property-change pos 'wamei/term-buffer string)
                      (length string)))
            (buffer (get-text-property pos 'wamei/term-buffer string)))
        (when buffer
          (push (cons (buffer-name buffer) (substring string pos next)) tabs))
        (setq pos next)))
    (nreverse tabs)))

(defun wamei/term-panel-test--tab-names (string)
  "STRING のタブが指す端末バッファ名。"
  (mapcar #'car (wamei/term-panel-test--tabs string)))

(defun wamei/term-panel-test--faces-at (string pos)
  "STRING の POS に付いている face の一覧。"
  (let ((face (get-text-property pos 'face string)))
    (if (and (listp face) (not (keywordp (car face)))) face (list face))))

(defun wamei/term-panel-test--render (&optional current width)
  "カレントバッファのプロジェクトのタブを描く。CURRENT は今出ている端末、WIDTH は桁数。"
  (wamei/term--tabs-string (wamei/term--buffers) current (or width 80) 800))


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
  "端末の中では、タブが別プロジェクトでもそのバッファのプロジェクトを見る。
端末タブの描画 (header-line) やタイトル変更 (プロセスフィルタ) は端末を基準に
動くので、ここでタブに引っぱられると別プロジェクトのタブを描いてしまう。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in beta (wamei/term--create 1) (wamei/term--create 2))
    (wamei/term-panel-test--with-tab-root alpha
      (with-current-buffer "*term: beta*"
        (should (equal (wamei/term--root) beta))
        (should (equal (mapcar #'buffer-name (wamei/term--buffers))
                       '("*term: beta*" "*term: beta 2*")))))))

(ert-deftest wamei/term-panel-root-falls-back-to-buffer-project-without-tab ()
  "タブにプロジェクトが紐づいていなければ、従来どおりバッファ基準。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (should (equal (wamei/term--root) alpha)))))

;;; cd してもプロジェクトの端末のまま

(defun wamei/term-panel-test--cd (buffer dir)
  "BUFFER の端末で DIR へ cd したときの OSC 7 を、パネルの advice 越しに処理する。
ghostel の `ghostel--update-directory' の代わりに `default-directory' だけ書き換える。"
  (with-current-buffer buffer
    (wamei/term--keep-in-project
     (lambda (d) (setq default-directory (file-name-as-directory d)
                       list-buffers-directory default-directory))
     dir)))

(ert-deftest wamei/term-panel-cd-outside-project-keeps-project-root ()
  "プロジェクトの外へ cd しても `default-directory' はプロジェクトルートに留まる。
project.el (`project-buffers') も consult のプロジェクトバッファも
`default-directory' の前方一致で所属を決めるので、外に出すと消えてしまう。"
  (wamei/term-panel-test--with-projects (alpha)
    (let ((term (wamei/term-panel-test--in alpha (wamei/term--create 1))))
      (wamei/term-panel-test--cd term base)
      (with-current-buffer term
        (should (equal default-directory alpha))
        (should (equal list-buffers-directory alpha)))
      (should (memq term (project-buffers (cons 'transient alpha)))))))

(ert-deftest wamei/term-panel-cd-inside-project-follows-the-shell ()
  "プロジェクト内の cd はそのまま追従する (C-x C-f の起点がシェルと揃う)。"
  (wamei/term-panel-test--with-projects (alpha)
    (let ((term (wamei/term-panel-test--in alpha (wamei/term--create 1)))
          (sub (file-name-as-directory (expand-file-name "sub" alpha))))
      (make-directory sub)
      (wamei/term-panel-test--cd term sub)
      (should (equal (buffer-local-value 'default-directory term) sub)))))

(ert-deftest wamei/term-panel-cd-leaves-other-ghostel-buffers-alone ()
  "パネルの端末以外 (claude のバッファなど) の cd には手を出さない。"
  (wamei/term-panel-test--with-projects (alpha)
    (with-temp-buffer
      (setq default-directory alpha)
      (wamei/term-panel-test--cd (current-buffer) base)
      (should (equal default-directory base)))))

(ert-deftest wamei/term-panel-terminal-remembers-its-project ()
  "端末は作ったときのプロジェクトを覚えていて、`default-directory' が
別プロジェクトを指しても名前・一覧はそのプロジェクトのまま。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in alpha (wamei/term--create 1) (wamei/term--create 2))
    (with-current-buffer "*term: alpha 2*"
      (setq default-directory beta)
      (should (equal (wamei/term--root) alpha))
      (should (equal (mapcar #'buffer-name (wamei/term--buffers))
                     '("*term: alpha*" "*term: alpha 2*"))))))

(ert-deftest wamei/term-panel-setup-buffer-finds-project-by-buffer-name ()
  "desktop で復元した端末は、保存時の作業ディレクトリがプロジェクト内の
別リポジトリ (サブモジュールなど) でも、バッファ名のプロジェクトを覚える。"
  (let* ((base (file-name-as-directory (file-truename (make-temp-file "term-panel-" t))))
         (alpha (file-name-as-directory (expand-file-name "alpha" base)))
         (inner (file-name-as-directory (expand-file-name "inner" alpha)))
         ;; 内側を先に並べて、inner の中では inner が見つかるようにする
         (wamei/term-panel-test--roots (list inner alpha))
         (project-find-functions (list #'wamei/term-panel-test--find-project)))
    (unwind-protect
        (progn
          (make-directory inner t)
          (with-current-buffer (get-buffer-create "*term: alpha 2*")
            (setq default-directory inner)
            (wamei/term--setup-buffer)
            (should (equal wamei/term--project-root alpha))))
      (kill-buffer "*term: alpha 2*")
      (delete-directory base t))))

(ert-deftest wamei/term-panel-setup-buffer-skips-unknown-project ()
  "バッファ名のプロジェクトが上にたどっても見つからなければ覚えない。
既に外へ cd していた端末に別プロジェクトを覚えさせると、名前と一覧がずれる。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (with-current-buffer (get-buffer-create "*term: alpha*")
      (setq default-directory beta)
      (wamei/term--setup-buffer)
      (should-not wamei/term--project-root))))

(ert-deftest wamei/term-panel-setup-buffer-ignores-non-panel-buffers ()
  "パネルの端末でない ghostel バッファにはプロジェクトを覚えさせない。"
  (with-temp-buffer
    (wamei/term--setup-buffer)
    (should-not wamei/term--project-root)))

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

;;; タブ (header-line)

(ert-deftest wamei/term-panel-tabs-show-only-current-project ()
  "タブには同じプロジェクトの端末だけを番号順に並べる。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (wamei/term--create 2))
    (wamei/term-panel-test--in beta
      (wamei/term--create 2)
      (wamei/term--create 1)
      (should (equal (wamei/term-panel-test--tab-names (wamei/term-panel-test--render))
                     '("*term: beta*" "*term: beta 2*"))))))

(ert-deftest wamei/term-panel-tabs-in-terminal-use-its-project ()
  "端末の header-line から描いても (タブが別プロジェクトでも) その端末のプロジェクト。"
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in alpha (wamei/term--create 1) (wamei/term--create 2))
    (wamei/term-panel-test--in beta (wamei/term--create 1))
    (wamei/term-panel-test--with-tab-root beta
      (with-current-buffer (get-buffer "*term: alpha*")
        (should (equal (wamei/term-panel-test--tab-names (wamei/term-panel-test--render))
                       '("*term: alpha*" "*term: alpha 2*")))))))

(ert-deftest wamei/term-panel-tab-label-comes-from-ghostel-title ()
  "タブのラベルは端末が報告したタイトル (`ghostel-title')、無ければシェル名。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (with-current-buffer (wamei/term--create 2)
        (setq-local ghostel-title "make test"))
      (let ((tabs (wamei/term-panel-test--tabs (wamei/term-panel-test--render))))
        (should (string-match-p (concat "● " (regexp-quote
                                              (file-name-nondirectory shell-file-name)))
                                (cdr (nth 0 tabs))))
        (should (string-match-p "● make test" (cdr (nth 1 tabs))))))))

(ert-deftest wamei/term-panel-tab-mark-follows-the-command-result ()
  "● は直前の実行状態の色。開いただけなら未実行 (黄)、終われば結果の色。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (wamei/term--create 2)
      (let* ((string (wamei/term-panel-test--render))
             (mark (string-match "●" string)))
        (should (memq 'wamei/term-modeline-status-idle
                      (wamei/term-panel-test--faces-at string mark))))
      (with-current-buffer (get-buffer "*term: alpha*")
        (setq-local wamei/term-modeline--start-time 1
                    wamei/term-modeline--end-time 2
                    wamei/term-modeline--exit-status 1))
      (let* ((string (wamei/term-panel-test--render))
             (mark (string-match "●" string)))
        (should (memq 'wamei/term-modeline-status-failure
                      (wamei/term-panel-test--faces-at string mark)))))))

(ert-deftest wamei/term-panel-tab-current-is-highlighted ()
  "今パネルに出ている端末のタブは `wamei/header-tab-current'、他は `wamei/header-tab'。
タブの face は ● の後ろに足す (● の状態の色を潰さない)。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (let* ((second (wamei/term--create 2))
             (string (wamei/term-panel-test--render second))
             (tabs (wamei/term-panel-test--tabs string))
             (second-start (length (cdr (car tabs))))
             (second-mark (string-match "●" string second-start)))
        (should (memq 'wamei/header-tab (wamei/term-panel-test--faces-at string 0)))
        (should (memq 'wamei/header-tab-current
                      (wamei/term-panel-test--faces-at string second-start)))
        (should (eq (car (wamei/term-panel-test--faces-at string second-mark))
                    'wamei/term-modeline-status-idle))
        (should (memq 'wamei/header-tab-current
                      (wamei/term-panel-test--faces-at string second-mark)))))))

(defun wamei/term-panel-test--click (string pos)
  "header-line の STRING の POS をクリックしたイベント。"
  (list 'mouse-1 (list (selected-window) 'header-line '(0 . 0) 0
                       (cons string pos) nil '(0 . 0) nil nil nil)))

(ert-deftest wamei/term-panel-tab-click-shows-the-terminal ()
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (let* ((first (wamei/term--create 1))
             (second (wamei/term--create 2)))
        (wamei/term--show first)
        (let* ((string (with-current-buffer first (wamei/term-panel-test--render first)))
               (pos (cdr (wamei/term-panel-test--tab-start string second))))
          (wamei/term-tab-select (wamei/term-panel-test--click string pos)))
        (should (eq (window-buffer (wamei/term--window)) second))))))

(ert-deftest wamei/term-panel-tab-middle-click-kills-the-terminal ()
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (let* ((first (wamei/term--create 1))
             (second (wamei/term--create 2)))
        (wamei/term--show first)
        (let* ((string (with-current-buffer first (wamei/term-panel-test--render first)))
               (pos (cdr (wamei/term-panel-test--tab-start string second))))
          (wamei/term-tab-kill (wamei/term-panel-test--click string pos)))
        (should-not (buffer-live-p second))
        (should (eq (window-buffer (wamei/term--window)) first))))))

(defun wamei/term-panel-test--tab-start (string buffer)
  "STRING で BUFFER のタブが始まる位置を (BUFFER . POS) で返す。"
  (cons buffer (text-property-any 0 (length string) 'wamei/term-buffer buffer string)))

(ert-deftest wamei/term-panel-tabs-redraw-on-tick ()
  "実行中の ● の呼吸は term-modeline.el の tick に相乗りする。"
  (should (memq #'wamei/term--tabs-tick
                (default-value 'wamei/term-modeline-tick-functions))))

(ert-deftest wamei/term-panel-command-state-redraws-tabs ()
  "コマンドの開始・終了でもタブを描き直す。
タイトルは変わらないので `wamei/term--on-title-change' では拾えない。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1))
    (let ((calls 0))
      (cl-letf (((symbol-function 'wamei/term--tabs-redraw)
                 (lambda () (setq calls (1+ calls)) nil)))
        (wamei/term--on-command-state (get-buffer "*term: alpha*"))
        (should (= calls 1))
        ;; 終了フックは状態も渡す
        (wamei/term--on-command-state (get-buffer "*term: alpha*") 1)
        (should (= calls 2))
        ;; 端末以外のバッファでは描き直さない
        (wamei/term-panel-test--in alpha
          (wamei/term--on-command-state (current-buffer)))
        (should (= calls 2))))))

(ert-deftest wamei/term-panel-command-state-survives-redraw-error ()
  "タブの再描画が signal しても外へ漏らさない (端末の出力処理の中で呼ばれる)。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1))
    (cl-letf (((symbol-function 'wamei/term--tabs-redraw)
               (lambda () (error "boom"))))
      (should-not (wamei/term--on-command-state (get-buffer "*term: alpha*"))))))

(ert-deftest wamei/term-panel-command-state-hooks-run-last ()
  "フックには後ろから足す。状態を記録する term-modeline.el のほうが先に
走らないと、タブに 1 つ前の ● が出る。"
  (should (eq (car (last ghostel-command-start-functions))
              #'wamei/term--on-command-state))
  (should (eq (car (last ghostel-command-finish-functions))
              #'wamei/term--on-command-state)))

(ert-deftest wamei/term-panel-title-change-redraws-tabs ()
  "`ghostel-buffer-name-function' として呼ばれるとタブを描き直し、
現在のバッファ名を返す (`ghostel--rename-managed' が必ず no-op になる値)。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (let ((calls 0))
        (with-current-buffer (get-buffer "*term: alpha*")
          (cl-letf (((symbol-function 'wamei/term--tabs-redraw)
                     (lambda () (setq calls (1+ calls)))))
            (should (equal (wamei/term--on-title-change "make test")
                           "*term: alpha*"))))
        (should (= calls 1))))))

(ert-deftest wamei/term-panel-title-change-ignores-other-buffers ()
  "端末以外のバッファではタブを描き直さない。
返り値は実装が何をしても一定なので、呼び出し回数で見る。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1))
    (let ((calls 0))
      (cl-letf (((symbol-function 'wamei/term--tabs-redraw)
                 (lambda () (setq calls (1+ calls)) nil)))
        (wamei/term-panel-test--in alpha
          ;; 端末以外でも返り値は自分のバッファ名 (改名は起きない)
          (should (equal (wamei/term--on-title-change "x") (buffer-name))))
        (should (= calls 0))))))

(ert-deftest wamei/term-panel-title-change-survives-redraw-error ()
  "タブの再描画が signal しても外へ漏らさない。

`ghostel--set-title' / `ghostel--set-directory' はこの関数の呼び出しを
`condition-case' で包まないので、漏らすと端末の出力処理 (プロセスフィルタ)
の中でエラーになる。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1))
    (cl-letf (((symbol-function 'wamei/term--tabs-redraw)
               (lambda () (error "boom"))))
      (with-current-buffer (get-buffer "*term: alpha*")
        (should (equal (wamei/term--on-title-change "x") "*term: alpha*"))))))

(ert-deftest wamei/term-panel-terminal-setup-hides-cursor-when-not-selected ()
  "端末バッファは `wamei/term--setup-buffer' でカーソルを非選択時に隠す。"
  (with-temp-buffer
    (wamei/term--setup-buffer)
    (should-not cursor-in-non-selected-windows)))

;;; パネル (window)

(defun wamei/term-panel-test--header-p (name)
  "端末バッファ NAME に header-line (タブ) が出るか。"
  (buffer-local-value 'header-line-format (get-buffer name)))

(ert-deftest wamei/term-panel-show-adds-tabs-for-two-terminals ()
  "端末が 2 つ以上になったら、同じプロジェクトの端末すべてにタブを出す。
1 つのうちは出さない。一覧の window は作らない。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--show (wamei/term--create 1))
      (should (wamei/term--window))
      (should-not (wamei/term-panel-test--header-p "*term: alpha*"))
      (wamei/term--show (wamei/term--create 2))
      (should (wamei/term-panel-test--header-p "*term: alpha*"))
      (should (wamei/term-panel-test--header-p "*term: alpha 2*"))
      (should (= (length (window-list nil 'no-mini)) 2)))))

(ert-deftest wamei/term-panel-tabs-leave-other-projects-alone ()
  (wamei/term-panel-test--with-projects (alpha beta)
    (wamei/term-panel-test--in beta (wamei/term--create 1))
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (wamei/term--show (wamei/term--create 2)))
    (should-not (wamei/term-panel-test--header-p "*term: beta*"))))

(ert-deftest wamei/term-panel-kill-hands-over-and-drops-tabs ()
  "端末が 1 つに戻ったらタブを消す。パネルは残りの端末に引き継ぐ。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (let ((first (wamei/term--create 1))
            (second (wamei/term--create 2)))
        (wamei/term--show second)
        (with-current-buffer second
          (add-hook 'kill-buffer-hook #'wamei/term--on-kill nil t))
        (kill-buffer second)
        (should (eq (window-buffer (wamei/term--window)) first))
        (should-not (wamei/term-panel-test--header-p "*term: alpha*"))))))

(ert-deftest wamei/term-panel-kill-of-hidden-terminal-drops-tabs ()
  "パネルに出ていない端末を消しても、残りが 1 つならタブを消す。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (let ((first (wamei/term--create 1))
            (second (wamei/term--create 2)))
        (wamei/term--show first)
        (with-current-buffer second
          (add-hook 'kill-buffer-hook #'wamei/term--on-kill nil t))
        (kill-buffer second)
        (should-not (wamei/term-panel-test--header-p "*term: alpha*"))))))

(ert-deftest wamei/term-panel-new-terminal-gets-tabs ()
  "`wamei/term-new' で増やした端末にもタブが付く。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--show (wamei/term--create 1))
      (wamei/term-new)
      (should (wamei/term-panel-test--header-p "*term: alpha 2*")))))

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
