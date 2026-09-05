;;; desktop-side-windows-test.el --- tests for desktop-side-windows -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l desktop-side-windows-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'frameset)
(load (expand-file-name "desktop-side-windows.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defconst wamei/dsw-test--header
  '((min-height . 4) (min-width . 10) (min-height-ignore . 3)
    (min-width-ignore . 5) (min-height-safe . 1) (min-width-safe . 2)
    (min-pixel-height . 80) (min-pixel-width . 90)
    (min-pixel-height-ignore . 60) (min-pixel-width-ignore . 45)
    (min-pixel-height-safe . 20) (min-pixel-width-safe . 18))
  "`window-state-get' が返す先頭の制約 alist。")

(defun wamei/dsw-test--leaf (name width height &rest options)
  "NAME を表示する WIDTH x HEIGHT の leaf 状態を作る。
OPTIONS は plist: :last :normal-width :normal-height :side :slot :selected。
1 文字 = 10px として pixel サイズも埋める。"
  (let ((side (plist-get options :side))
        (slot (plist-get options :slot)))
    `(leaf
      ,@(when (plist-get options :last) '((last . t)))
      (pixel-width . ,(* 10 width))
      (pixel-height . ,(* 10 height))
      (total-width . ,width)
      (total-height . ,height)
      (normal-height . ,(or (plist-get options :normal-height) 1.0))
      (normal-width . ,(or (plist-get options :normal-width) 1.0))
      (parameters ,@(when side `((window-side . ,side)
                                 (window-slot . ,slot)
                                 (no-other-window . t)
                                 (no-delete-other-windows . t)))
                  (context))
      (buffer ,name
              (selected . ,(plist-get options :selected))
              (hscroll . 0) (fringes 8 8 nil nil) (margins 1)
              (scroll-bars nil 0 t nil 0 t nil) (vscroll . 0)
              (dedicated . ,(and side 'side))
              (point . 1) (start . 1)))))

(defun wamei/dsw-test--combo (type width height children &rest options)
  "TYPE (hc / vc) の WIDTH x HEIGHT の内部 window 状態を作る。"
  `(,type
    ,@(when (plist-get options :last) '((last . t)))
    (pixel-width . ,(* 10 width))
    (pixel-height . ,(* 10 height))
    (total-width . ,width)
    (total-height . ,height)
    (normal-height . ,(or (plist-get options :normal-height) 1.0))
    (normal-width . ,(or (plist-get options :normal-width) 1.0))
    (combination-limit . t)
    ,@children))

(defun wamei/dsw-test--state (tree)
  "TREE を根とする `window-state-get' 形式の状態。"
  (cons wamei/dsw-test--header tree))

(defun wamei/dsw-test--vscode-layout ()
  "sidebar (左) / claude (右) / 端末 (下) を含む典型的な構成。
hc [sidebar 35, vc [main 65 行, term 30 行], claude 68] で幅 353、高さ 95。"
  (wamei/dsw-test--state
   (wamei/dsw-test--combo
    'hc 353 95
    (list
     (wamei/dsw-test--leaf " *sidebar: a*" 35 95
                           :normal-width 0.1 :side 'left :slot -1)
     (wamei/dsw-test--combo
      'vc 250 95
      (list
       (wamei/dsw-test--leaf "init.el" 250 65 :normal-height 0.68 :selected t)
       (wamei/dsw-test--leaf "*term: dotfiles*" 250 30 :last t
                             :normal-height 0.32 :side 'bottom :slot 0))
      :normal-width 0.7)
     (wamei/dsw-test--leaf "*claude-code[dotfiles]*" 68 95 :last t
                           :normal-width 0.2 :side 'right :slot 0)))))

(defun wamei/dsw-test--children (node)
  "NODE の子 window 状態のリスト。"
  (seq-filter (lambda (item) (memq (car-safe item) '(leaf vc hc))) (cdr node)))

(defun wamei/dsw-test--attr (node key)
  "NODE の属性 KEY の値。"
  (cdr (assq key (cdr node))))

;;; wamei/desktop-side-strip-state

(ert-deftest wamei/dsw-strip-state-keeps-plain-leaf ()
  "side window を含まない状態はそのまま返し、spec は空。"
  (let ((state (wamei/dsw-test--state (wamei/dsw-test--leaf "init.el" 80 24))))
    (pcase-let ((`(,stripped . ,specs) (wamei/desktop-side-strip-state state)))
      (should (equal stripped state))
      (should (null specs)))))

(ert-deftest wamei/dsw-strip-state-collapses-to-main-window ()
  "side window を全部外すと主領域の leaf だけになり、寸法は根のものを継ぐ。"
  (pcase-let ((`(,stripped . ,_) (wamei/desktop-side-strip-state
                                  (wamei/dsw-test--vscode-layout))))
    (let ((tree (cdr stripped)))
      (should (equal (car stripped) wamei/dsw-test--header))
      (should (eq (car tree) 'leaf))
      (should (equal (cadr (assq 'buffer (cdr tree))) "init.el"))
      (should (= (wamei/dsw-test--attr tree 'total-width) 353))
      (should (= (wamei/dsw-test--attr tree 'total-height) 95))
      (should (= (wamei/dsw-test--attr tree 'pixel-width) 3530))
      (should (= (wamei/dsw-test--attr tree 'normal-width) 1.0))
      (should (= (wamei/dsw-test--attr tree 'normal-height) 1.0))
      ;; 根になったので last は付かない
      (should-not (assq 'last (cdr tree)))
      ;; 選択状態は保たれる
      (should (eq (cdr (assq 'selected (cddr (assq 'buffer (cdr tree))))) t)))))

(ert-deftest wamei/dsw-strip-state-collects-specs ()
  "外した side window ごとに side / slot / buffer / 寸法を記録する。"
  (pcase-let ((`(,_ . ,specs) (wamei/desktop-side-strip-state
                               (wamei/dsw-test--vscode-layout))))
    (should (= (length specs) 3))
    (let ((by-buffer (lambda (name)
                       (seq-find (lambda (spec) (equal (plist-get spec :buffer) name))
                                 specs))))
      (let ((sidebar (funcall by-buffer " *sidebar: a*")))
        (should (eq (plist-get sidebar :side) 'left))
        (should (= (plist-get sidebar :slot) -1))
        ;; 左右は幅
        (should (= (plist-get sidebar :size) 35)))
      (let ((term (funcall by-buffer "*term: dotfiles*")))
        (should (eq (plist-get term :side) 'bottom))
        (should (= (plist-get term :slot) 0))
        ;; 上下は高さ
        (should (= (plist-get term :size) 30)))
      (let ((claude (funcall by-buffer "*claude-code[dotfiles]*")))
        (should (eq (plist-get claude :side) 'right))
        (should (= (plist-get claude :size) 68))))))

(ert-deftest wamei/dsw-spec-records-directory-of-live-buffer ()
  "spec には保存時のバッファの default-directory を :directory として残す。
復元時に選択 window のバッファが揃っていなくても、restorer が正しいディレクトリ
(プロジェクト) で開き直せるようにする。バッファが無ければ nil。"
  (let ((buffer (generate-new-buffer "*claude-code[dsw-dir]*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq default-directory "/tmp/dsw-project/"))
          (let ((spec (wamei/desktop-side--spec
                       (wamei/dsw-test--leaf (buffer-name buffer) 68 40 :side 'right :slot 1))))
            (should (equal (plist-get spec :directory) "/tmp/dsw-project/"))))
      (kill-buffer buffer)))
  (let ((spec (wamei/desktop-side--spec
               (wamei/dsw-test--leaf "*dsw-no-such-buffer*" 68 40 :side 'right :slot 1))))
    (should (null (plist-get spec :directory)))))

(ert-deftest wamei/dsw-strip-state-rescales-remaining-siblings ()
  "兄弟が 2 つ以上残るときは、残った分で寸法を比例配分し last を付け直す。"
  (let* ((state (wamei/dsw-test--state
                 (wamei/dsw-test--combo
                  'hc 100 20
                  (list
                   (wamei/dsw-test--leaf "left" 20 20 :normal-width 0.2 :side 'left :slot 0)
                   (wamei/dsw-test--leaf "a" 40 20 :normal-width 0.4)
                   (wamei/dsw-test--leaf "b" 40 20 :normal-width 0.4 :last t)))))
         (stripped (car (wamei/desktop-side-strip-state state)))
         (tree (cdr stripped))
         (children (wamei/dsw-test--children tree)))
    (should (eq (car tree) 'hc))
    (should (= (length children) 2))
    (pcase-let ((`(,a ,b) children))
      (should-not (assq 'last (cdr a)))
      (should (assq 'last (cdr b)))
      (should (= (wamei/dsw-test--attr a 'total-width) 50))
      (should (= (wamei/dsw-test--attr b 'total-width) 50))
      (should (= (wamei/dsw-test--attr a 'pixel-width) 500))
      (should (< (abs (- (wamei/dsw-test--attr a 'normal-width) 0.5)) 1e-6))
      (should (< (abs (- (wamei/dsw-test--attr b 'normal-width) 0.5)) 1e-6))
      ;; 直交方向はそのまま
      (should (= (wamei/dsw-test--attr a 'total-height) 20)))))

(ert-deftest wamei/dsw-strip-state-neutralizes-side-only-state ()
  "side window しかない状態は、属性を外した通常の leaf に置き換える。
復元時に window--sides-check が失敗して無限再帰する構成を作らないため。"
  (let* ((state (wamei/dsw-test--state
                 (wamei/dsw-test--leaf "*term: x*" 80 24 :side 'bottom :slot 0 :selected t)))
         (result (wamei/desktop-side-strip-state state))
         (tree (cdr (car result)))
         (parameters (cdr (assq 'parameters (cdr tree))))
         (buffer (cdr (assq 'buffer (cdr tree)))))
    (should (eq (car tree) 'leaf))
    (should-not (assq 'window-side parameters))
    (should-not (assq 'window-slot parameters))
    (should-not (assq 'no-other-window parameters))
    (should-not (assq 'no-delete-other-windows parameters))
    (should (equal (car buffer) "*scratch*"))
    (should (null (cdr (assq 'dedicated (cdr buffer)))))
    (should (= (wamei/dsw-test--attr tree 'total-width) 80))
    ;; 記録は残す
    (should (= (length (cdr result)) 1))
    (should (equal (plist-get (car (cdr result)) :buffer) "*term: x*"))))

(ert-deftest wamei/dsw-strip-state-does-not-mutate-input ()
  "入力の状態は書き換えない (tab-bar が保持する ws と共有されているため)。"
  (let* ((state (wamei/dsw-test--vscode-layout))
         (snapshot (copy-tree state)))
    (wamei/desktop-side-strip-state state)
    (should (equal state snapshot))))

;;; wamei/desktop-side-strip-frameset

(defun wamei/dsw-test--frameset (root-state tabs)
  "ROOT-STATE と TABS を持つ 1 フレームの frameset。"
  (frameset--make
   :app '(desktop . "208")
   :name "test"
   :states (list (cons `((frameset--id . "ID-1")
                         (tabs . ,tabs)
                         (width . 353))
                       root-state))))

(ert-deftest wamei/dsw-strip-frameset-strips-root-and-tabs ()
  "現在タブは根の window 状態、他のタブは ws を対象に side window を外す。"
  (let* ((other-ws (wamei/dsw-test--vscode-layout))
         (frameset (wamei/dsw-test--frameset
                    (wamei/dsw-test--vscode-layout)
                    `((tab (name . "other") (ws . ,other-ws))
                      (current-tab (name . "current")))))
         (specs (wamei/desktop-side-strip-frameset frameset))
         (state (car (frameset-states frameset)))
         (params (car state))
         (tabs (cdr (assq 'tabs params))))
    ;; 根
    (should (eq (car (cdr (cdr state))) 'leaf))
    ;; 他タブの ws
    (should (eq (car (cdr (alist-get 'ws (cdr (nth 0 tabs))))) 'leaf))
    ;; タブ以外のパラメータは残る
    (should (= (cdr (assq 'width params)) 353))
    (should (equal (alist-get 'name (cdr (nth 1 tabs))) "current"))
    ;; spec は frameset--id とタブ位置で引ける
    (let ((by-tab (cdr (assoc "ID-1" specs))))
      (should (= (length (cdr (assq 0 by-tab))) 3))
      (should (= (length (cdr (assq 1 by-tab))) 3)))))

(ert-deftest wamei/dsw-strip-frameset-without-tabs ()
  "tab-bar を使っていないフレームはタブ 0 として扱う。"
  (let* ((frameset (frameset--make
                    :app '(desktop . "208") :name "test"
                    :states (list (cons '((frameset--id . "ID-2"))
                                        (wamei/dsw-test--vscode-layout)))))
         (specs (wamei/desktop-side-strip-frameset frameset)))
    (should (= (length (cdr (assq 0 (cdr (assoc "ID-2" specs))))) 3))
    (should (eq (car (cdr (cdr (car (frameset-states frameset))))) 'leaf))))

(ert-deftest wamei/dsw-strip-frameset-does-not-mutate-tab-ws ()
  "タブの ws は tab-bar の実体と共有されているので書き換えない。"
  (let* ((other-ws (wamei/dsw-test--vscode-layout))
         (snapshot (copy-tree other-ws))
         (frameset (wamei/dsw-test--frameset
                    (wamei/dsw-test--state (wamei/dsw-test--leaf "init.el" 353 95))
                    `((current-tab (name . "current"))
                      (tab (name . "other") (ws . ,other-ws))))))
    (wamei/desktop-side-strip-frameset frameset)
    (should (equal other-ws snapshot))))

;;; 復元順序と振り分け

(ert-deftest wamei/dsw-sort-specs-left-right-top-bottom-then-slot ()
  "window-sides-vertical が t のとき下部の寸法は左右に依存するので左右を先に開く。"
  (let ((sorted (wamei/desktop-side-sort-specs
                 '((:side bottom :slot 1) (:side right :slot 0)
                   (:side bottom :slot 0) (:side left :slot -1)))))
    (should (equal (mapcar (lambda (spec) (list (plist-get spec :side) (plist-get spec :slot)))
                           sorted)
                   '((left -1) (right 0) (bottom 0) (bottom 1))))))

(ert-deftest wamei/dsw-restorer-for-matches-buffer-name ()
  "バッファ名にマッチした復元関数を選び、無ければ既定を返す。"
  (let ((wamei/desktop-side-restorers '(("\\`\\*term: " . term-restorer)
                                        ("\\` \\*sidebar: " . sidebar-restorer))))
    (should (eq (wamei/desktop-side-restorer-for '(:buffer "*term: x*")) 'term-restorer))
    (should (eq (wamei/desktop-side-restorer-for '(:buffer " *sidebar: a*"))
                'sidebar-restorer))
    (should (eq (wamei/desktop-side-restorer-for '(:buffer "*Help*"))
                #'wamei/desktop-side-display))))

;;; 実 window での往復

(defun wamei/dsw-test--with-clean-frame (fn)
  "side window を片付けた 1 window の状態で FN を呼ぶ。"
  (dolist (window (window-list nil 'no-mini))
    (when (window-parameter window 'window-side)
      (delete-window window)))
  (delete-other-windows)
  (unwind-protect (funcall fn)
    (dolist (window (window-list nil 'no-mini))
      (when (window-parameter window 'window-side)
        (ignore-errors (delete-window window))))
    (delete-other-windows)))

(ert-deftest wamei/dsw-directory-picks-first-in-restore-order ()
  "タブのディレクトリは、開き直す順 (左→右→上→下) で最初に :directory を持つ spec のもの。"
  (should (equal (wamei/desktop-side-directory
                  '((:buffer "*term: a*" :side bottom :slot 0 :directory "/tmp/term/")
                    (:buffer "*claude-code[a]*" :side right :slot 0 :directory "/tmp/claude/")
                    (:buffer " *sidebar: Tab a" :side left :slot 0)))
                 "/tmp/claude/"))
  (should (null (wamei/desktop-side-directory
                 '((:buffer " *sidebar: Tab a" :side left :slot 0))))))

(ert-deftest wamei/dsw-restore-specs-binds-directory-for-restorer ()
  "restorer は spec の :directory を default-directory として呼ばれる。
:directory が無い spec では呼び出し側の default-directory のまま。"
  (let* ((seen nil)
         (default-directory "/tmp/dsw-caller/")
         (wamei/desktop-side-restorers
          (list (cons "\\`\\*dsw-" (lambda (spec)
                                    (push (cons (plist-get spec :buffer) default-directory)
                                          seen))))))
    (wamei/desktop-side-restore-specs
     '((:buffer "*dsw-with*" :side right :slot 0 :directory "/tmp/dsw-project/")
       (:buffer "*dsw-without*" :side bottom :slot 0)))
    (should (equal (assoc "*dsw-with*" seen) '("*dsw-with*" . "/tmp/dsw-project/")))
    (should (equal (assoc "*dsw-without*" seen) '("*dsw-without*" . "/tmp/dsw-caller/")))))

(ert-deftest wamei/dsw-round-trip-restores-without-side-windows ()
  "実際の side window 構成を取り、外した状態を window-state-put しても落ちない。
side window のバッファが消えている (再起動後) 状況を再現する。"
  (wamei/dsw-test--with-clean-frame
   (lambda ()
     (let ((window-sides-vertical t)
           (left (get-buffer-create " *sidebar: Test*"))
           (term (get-buffer-create "*term: test*")))
       (display-buffer-in-side-window left '((side . left) (slot . -1) (window-width . 20)))
       (display-buffer-in-side-window term '((side . bottom) (slot . 0) (window-height . 6)))
       (should (= (length (window-list nil 'no-mini)) 3))
       (pcase-let ((`(,stripped . ,specs)
                    (wamei/desktop-side-strip-state
                     (window-state-get (frame-root-window) t))))
         (should (= (length specs) 2))
         ;; 再起動後はこれらのバッファは存在しない
         (dolist (window (window-list nil 'no-mini))
           (when (window-parameter window 'window-side) (delete-window window)))
         (kill-buffer left)
         (kill-buffer term)
         (window-state-put stripped (frame-root-window) 'safe)
         (should (= (length (window-list nil 'no-mini)) 1))
         (should-not (window-parameter (selected-window) 'window-side))
         (should (equal (buffer-name (window-buffer (selected-window))) "*scratch*")))))))

(ert-deftest wamei/dsw-display-reopens-side-window-from-spec ()
  "既定の復元関数は生きているバッファを記録した side / slot / 寸法で開き直す。"
  (wamei/dsw-test--with-clean-frame
   (lambda ()
     (let ((window-sides-vertical t)
           (buffer (get-buffer-create "*Help*")))
       (unwind-protect
           (let ((window (wamei/desktop-side-display
                          `(:buffer "*Help*" :side left :slot 2 :size 20 :dedicated side))))
             (should (window-live-p window))
             (should (eq (window-parameter window 'window-side) 'left))
             (should (eql (window-parameter window 'window-slot) 2))
             (should (= (window-total-width window) 20))
             (should (window-dedicated-p window)))
         (kill-buffer buffer))))))

(ert-deftest wamei/dsw-display-skips-dead-buffer ()
  "バッファが無ければ何もしない。"
  (should (null (wamei/desktop-side-display '(:buffer "*no such buffer*" :side left
                                              :slot 0 :size 20)))))

;;; desktop-side-windows-test.el ends here

;;; desktop との統合

(ert-deftest wamei/dsw-integration-desktop-save-and-read ()
  "desktop-save は side window を外した frameset と記録を書き、desktop-read は記録から開き直す。"
  (require 'desktop)
  (wamei/dsw-test--with-clean-frame
   (lambda ()
     (let* ((dir (make-temp-file "dsw-desktop" t))
            (window-sides-vertical t)
            (desktop-restore-frames t)
            (desktop-globals-to-save (copy-sequence desktop-globals-to-save))
            (desktop-after-read-hook nil)
            (desktop-dirname nil)
            (desktop-file-modtime nil)
            (desktop-base-file-name ".emacs.desktop")
            (wamei/desktop-side-saved nil)
            (restored nil)
            (wamei/desktop-side-restorers
             (list (cons "\\`\\*term: " (lambda (spec) (push spec restored)))))
            (term (get-buffer-create "*term: it*")))
       (unwind-protect
           (progn
             (wamei/desktop-side-setup)
             (display-buffer-in-side-window term '((side . bottom) (slot . 0) (window-height . 6)))
             (should (= (length (window-list nil 'no-mini)) 2))
             (desktop-save dir)
             ;; 保存後も live な side window はそのまま
             (should (= (length (window-list nil 'no-mini)) 2))
             (should (window-parameter (get-buffer-window term) 'window-side))
             (with-temp-buffer
               (insert-file-contents (expand-file-name ".emacs.desktop" dir))
               (should (search-forward "(setq wamei/desktop-side-saved" nil t))
               (goto-char (point-min))
               (should-not (search-forward "(window-side . bottom)" nil t)))
             ;; 再起動を模す: side window とバッファを消してから読み直す
             (delete-window (get-buffer-window term))
             (kill-buffer term)
             (setq wamei/desktop-side-saved nil)
             ;; desktop-read は batch では no-op なので、その中身を順に呼ぶ
             (load (expand-file-name ".emacs.desktop" dir) nil t)
             (desktop-restore-frameset)
             (run-hooks (quote desktop-after-read-hook))
             ;; 外した frameset が適用され、side window は残っていない
             (should (= (length (window-list nil (quote no-mini))) 1))
             (should (= (length restored) 1))
             (should (eq (plist-get (car restored) :side) 'bottom))
             (should (= (plist-get (car restored) :size) 6)))
         (advice-remove 'desktop-save-frameset #'wamei/desktop-side--after-save-frameset)
         (remove-hook 'desktop-after-read-hook #'wamei/desktop-side-restore)
         (ignore-errors (desktop-release-lock dir))
         (delete-directory dir t))))))

;;; 繰り上げ後の子の寸法

(ert-deftest wamei/dsw-strip-state-refits-children-of-promoted-node ()
  "side window を外して親に繰り上げた node の子は、新しい幅いっぱいに比例配分する。
左右 2 window の合計が親より小さいままだと、復元時に左は保存値・右は残り全部と
なり、再起動のたびに右が広がる。"
  (let* ((state
          (wamei/dsw-test--state
           (wamei/dsw-test--combo
            'hc 353 95
            (list
             (wamei/dsw-test--leaf " *sidebar: Tab a*" 35 95
                                   :normal-width 0.1 :side 'left :slot 0)
             (wamei/dsw-test--combo
              'vc 216 95
              (list
               (wamei/dsw-test--combo
                'hc 216 66
                (list (wamei/dsw-test--leaf "a.el" 108 66 :normal-width 0.5)
                      (wamei/dsw-test--leaf "b.el" 108 66 :normal-width 0.5 :last t))
                :normal-height 0.7)
               (wamei/dsw-test--leaf "*term: a*" 216 29 :last t
                                     :normal-height 0.3 :side 'bottom :slot 0))
              :normal-width 0.6)
             (wamei/dsw-test--leaf "*claude-code[a]*" 102 95 :last t
                                   :normal-width 0.3 :side 'right :slot 0)))))
         (tree (cdr (car (wamei/desktop-side-strip-state state))))
         (children (wamei/dsw-test--children tree)))
    (should (eq (car tree) 'hc))
    (should (= (wamei/dsw-test--attr tree 'total-width) 353))
    (should (= (wamei/dsw-test--attr tree 'total-height) 95))
    (pcase-let ((`(,a ,b) children))
      ;; 幅は親いっぱいに半分ずつ
      (should (= (+ (wamei/dsw-test--attr a 'total-width)
                    (wamei/dsw-test--attr b 'total-width))
                 353))
      (should (<= (abs (- (wamei/dsw-test--attr a 'total-width)
                          (wamei/dsw-test--attr b 'total-width)))
                  1))
      (should (= (+ (wamei/dsw-test--attr a 'pixel-width)
                    (wamei/dsw-test--attr b 'pixel-width))
                 3530))
      (should (< (abs (- (wamei/dsw-test--attr a 'normal-width) 0.5)) 0.01))
      ;; 高さは親と同じ
      (should (= (wamei/dsw-test--attr a 'total-height) 95))
      (should (= (wamei/dsw-test--attr a 'pixel-height) 950))
      (should (= (wamei/dsw-test--attr b 'total-height) 95))
      (should (= (wamei/dsw-test--attr a 'normal-height) 1.0))
      (should-not (assq 'last (cdr a)))
      (should (assq 'last (cdr b))))))

(ert-deftest wamei/dsw-round-trip-keeps-two-main-windows-balanced ()
  "左右 2 window + side window の構成を外して復元しても、2 window は等分のまま。"
  (wamei/dsw-test--with-clean-frame
   (lambda ()
     (let ((window-sides-vertical t)
           (left (get-buffer-create " *sidebar: Test*"))
           (term (get-buffer-create "*term: test*"))
           (a (get-buffer-create "a.el"))
           (b (get-buffer-create "b.el")))
       (unwind-protect
           (progn
             (set-window-buffer (selected-window) a)
             (set-window-buffer (split-window-right) b)
             (display-buffer-in-side-window left '((side . left) (slot . 0) (window-width . 20)))
             (display-buffer-in-side-window term '((side . bottom) (slot . 0) (window-height . 6)))
             (balance-windows)
             (let ((before (window-total-width (get-buffer-window a))))
               (should (<= (abs (- before (window-total-width (get-buffer-window b)))) 1))
               (let ((stripped (car (wamei/desktop-side-strip-state
                                     (window-state-get (frame-root-window) t)))))
                 (dolist (window (window-list nil 'no-mini))
                   (when (window-parameter window 'window-side) (delete-window window)))
                 (kill-buffer left)
                 (kill-buffer term)
                 (window-state-put stripped (frame-root-window) 'safe)
                 (should (= (length (window-list nil 'no-mini)) 2))
                 (let ((wa (window-total-width (get-buffer-window a)))
                       (wb (window-total-width (get-buffer-window b))))
                   (should (<= (abs (- wa wb)) 1))
                   (should (> wa before))))))
         (dolist (buffer (list a b))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))
