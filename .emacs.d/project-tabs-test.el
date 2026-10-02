;;; project-tabs-test.el --- tests for project-tabs -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-tabs-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'tab-bar)
(package-initialize)
(load (expand-file-name "project-tabs.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defmacro wamei/project-tabs-test--with-project (root &rest body)
  "ROOT 配下だけをプロジェクトとみなす環境で BODY を評価する。"
  (declare (indent 1))
  `(let ((project-find-functions
          (list (lambda (dir)
                  (when (string-prefix-p ,root (expand-file-name dir))
                    (cons 'transient ,root))))))
     ,@body))

(defmacro wamei/project-tabs-test--with-tab-bar (&rest body)
  "タブを 1 つ (未固定) にリセットした tab-bar-mode で BODY を評価する。"
  (declare (indent 0))
  `(let ((tab-bar-tab-name-function #'wamei/tab-bar-tab-name-project))
     (tab-bar-mode 1)
     (set-frame-parameter nil 'tabs nil)
     (unwind-protect (progn ,@body)
       (set-frame-parameter nil 'tabs nil))))

(defun wamei/project-tabs-test--show (buffer dir)
  "BUFFER の default-directory を DIR にして選択 window に出す。"
  (with-current-buffer buffer
    (setq default-directory dir))
  (set-window-buffer (selected-window) buffer)
  buffer)

;;; タブ名

(ert-deftest wamei/project-tabs-name-uses-project-name ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (with-temp-buffer
      (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/src/")
      (should (equal (wamei/tab-bar-tab-name-project) "proj-a")))))

(ert-deftest wamei/project-tabs-name-falls-back-to-buffer-name ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (with-temp-buffer
      (wamei/project-tabs-test--show (current-buffer) "/tmp/elsewhere/")
      (should (equal (wamei/tab-bar-tab-name-project) (buffer-name))))))

(ert-deftest wamei/project-tabs-name-looks-past-side-window ()
  "選択 window が no-other-window なら直近の通常 window のバッファで決める。"
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (delete-other-windows)
    (let ((main (generate-new-buffer " main"))
          (side (generate-new-buffer " side")))
      (unwind-protect
          (progn
            (wamei/project-tabs-test--show main "/tmp/proj-a/")
            (let ((side-window (split-window nil nil 'left)))
              (set-window-buffer side-window side)
              (set-window-parameter side-window 'no-other-window t)
              (select-window side-window)
              (should (equal (wamei/tab-bar-tab-name-project) "proj-a"))))
        (delete-other-windows)
        (kill-buffer main)
        (kill-buffer side)))))

(ert-deftest wamei/project-tabs-main-window-skips-side-window ()
  (let* ((main (selected-window))
         (side (split-window main nil 'left)))
    (unwind-protect
        (progn
          (set-window-parameter side 'no-other-window t)
          (select-window side)
          (should (eq (wamei/project-tabs-main-window) main))
          (select-window main)
          (should (eq (wamei/project-tabs-main-window) main)))
      (delete-window side))))

;;; タブ名の固定

(ert-deftest wamei/project-tabs-pin-renames-unpinned-tab-to-project ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (wamei/project-tabs-test--with-tab-bar
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/")
        (should (equal (wamei/project-tabs-pin-name) "proj-a"))
        (let ((tab (tab-bar--current-tab)))
          (should (equal (alist-get 'name tab) "proj-a"))
          (should (alist-get 'explicit-name tab)))))))

(ert-deftest wamei/project-tabs-pin-keeps-explicit-name ()
  "既に固定されたタブは別プロジェクトのバッファを出しても変えない。"
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (wamei/project-tabs-test--with-tab-bar
      (tab-rename "pinned")
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/")
        (should-not (wamei/project-tabs-pin-name))
        (should (equal (alist-get 'name (tab-bar--current-tab)) "pinned"))))))

(ert-deftest wamei/project-tabs-pin-skips-buffer-outside-project ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (wamei/project-tabs-test--with-tab-bar
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/elsewhere/")
        (should-not (wamei/project-tabs-pin-name))
        (should-not (alist-get 'explicit-name (tab-bar--current-tab)))))))

(ert-deftest wamei/project-tabs-pin-is-noop-without-tab-bar-mode ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (tab-bar-mode -1)
    (with-temp-buffer
      (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/")
      (should-not (wamei/project-tabs-pin-name)))))


;;; タブに紐づくプロジェクト

(defmacro wamei/project-tabs-test--with-projects (&rest body)
  "/tmp/proj-NAME/ 以下をそれぞれ別のプロジェクトとみなす環境で BODY を評価する。"
  (declare (indent 0))
  `(let ((project-find-functions
          (list (lambda (dir)
                  (let ((dir (expand-file-name dir)))
                    (when (string-match "\\`\\(/tmp/proj-[a-z]+/\\)" dir)
                      (cons 'transient (match-string 1 dir))))))))
     ,@body))

(defmacro wamei/project-tabs-test--with-setup (&rest body)
  "`project-current' への advice を張った状態で BODY を評価する。"
  (declare (indent 0))
  `(progn
     (wamei/project-tabs-setup)
     (unwind-protect (progn ,@body)
       (advice-remove 'project-current #'wamei/project-tabs--use-tab-root))))

(defun wamei/project-tabs-test--root-in (dir command)
  "DIR を `default-directory'、COMMAND を `this-command' として見えるプロジェクトの root。"
  (with-temp-buffer
    (setq default-directory dir)
    (let ((this-command command))
      (when-let* ((project (project-current nil)))
        (project-root project)))))

(ert-deftest wamei/project-tabs-set-root-round-trips ()
  (wamei/project-tabs-test--with-tab-bar
    (wamei/project-tabs-set-root "/tmp/proj-a")
    (should (equal (wamei/project-tabs-current-root) "/tmp/proj-a/"))))

(ert-deftest wamei/project-tabs-root-survives-tab-switch ()
  "独自パラメータはタブを切り替えても残る (実体の cdr につないでいる)。"
  (wamei/project-tabs-test--with-tab-bar
    (wamei/project-tabs-set-root "/tmp/proj-a/")
    (tab-new)
    (should-not (wamei/project-tabs-current-root))
    (tab-bar-select-tab 1)
    (should (equal (wamei/project-tabs-current-root) "/tmp/proj-a/"))))

(ert-deftest wamei/project-tabs-pin-records-root ()
  "タブ名を固定するときに root も紐づける。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/src/")
        (should (equal (wamei/project-tabs-pin-name) "proj-a"))
        (should (equal (wamei/project-tabs-current-root) "/tmp/proj-a/"))))))

(ert-deftest wamei/project-tabs-pin-records-root-for-pinned-tab ()
  "名前が固定済みのタブ (desktop 復元など) も、名前が一致するプロジェクトなら紐づける。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (tab-rename "proj-a")
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/src/")
        (should-not (wamei/project-tabs-pin-name))
        (should (equal (wamei/project-tabs-current-root) "/tmp/proj-a/"))))))

(ert-deftest wamei/project-tabs-pin-skips-root-for-other-project ()
  "名前が固定済みのタブに別プロジェクトのバッファが出ても紐づけない。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (tab-rename "proj-a")
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-b/")
        (should-not (wamei/project-tabs-pin-name))
        (should-not (wamei/project-tabs-current-root))))))

(ert-deftest wamei/project-tabs-pin-keeps-recorded-root ()
  "既に紐づいた root は上書きしない。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (wamei/project-tabs-set-root "/tmp/proj-a/")
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-b/")
        (wamei/project-tabs-pin-name)
        (should (equal (wamei/project-tabs-current-root) "/tmp/proj-a/"))))))

;;; project 系コマンドの起点

(ert-deftest wamei/project-tabs-command-uses-tab-root ()
  "別プロジェクトのバッファにいても project 系コマンドはタブの root を見る。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (wamei/project-tabs-test--with-setup
        (wamei/project-tabs-set-root "/tmp/proj-a/")
        (should (equal (wamei/project-tabs-test--root-in
                        "/tmp/proj-b/" 'project-find-file)
                       "/tmp/proj-a/"))))))

(ert-deftest wamei/project-tabs-command-uses-tab-root-outside-project ()
  "プロジェクト外のバッファ (*scratch* 等) でもタブの root を見る。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (wamei/project-tabs-test--with-setup
        (wamei/project-tabs-set-root "/tmp/proj-a/")
        (should (equal (wamei/project-tabs-test--root-in
                        "/tmp/elsewhere/" 'project-find-file)
                       "/tmp/proj-a/"))))))

(ert-deftest wamei/project-tabs-extra-command-uses-tab-root ()
  "`wamei/project-tabs-commands' に入れたコマンドもタブの root を見る。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (wamei/project-tabs-test--with-setup
        (wamei/project-tabs-set-root "/tmp/proj-a/")
        (let ((wamei/project-tabs-commands '(consult-project-buffer)))
          (should (equal (wamei/project-tabs-test--root-in
                          "/tmp/proj-b/" 'consult-project-buffer)
                         "/tmp/proj-a/")))))))

(ert-deftest wamei/project-tabs-other-command-uses-buffer-project ()
  "project 系でないコマンド (eglot や apheleia の経路) はバッファのプロジェクトのまま。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (wamei/project-tabs-test--with-setup
        (wamei/project-tabs-set-root "/tmp/proj-a/")
        (should (equal (wamei/project-tabs-test--root-in "/tmp/proj-b/" 'find-file)
                       "/tmp/proj-b/"))
        (should (equal (wamei/project-tabs-test--root-in "/tmp/proj-b/" nil)
                       "/tmp/proj-b/"))))))

(ert-deftest wamei/project-tabs-respects-directory-override ()
  "`project-current-directory-override' を立てている呼び出し (C-x t p 等) には触らない。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (wamei/project-tabs-test--with-setup
        (wamei/project-tabs-set-root "/tmp/proj-a/")
        (let ((project-current-directory-override "/tmp/proj-b/"))
          (should (equal (wamei/project-tabs-test--root-in
                          "/tmp/elsewhere/" 'project-find-file)
                         "/tmp/proj-b/")))))))

(ert-deftest wamei/project-tabs-respects-explicit-directory ()
  "DIRECTORY 引数を明示した呼び出しはそのまま通す。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (wamei/project-tabs-test--with-setup
        (wamei/project-tabs-set-root "/tmp/proj-a/")
        (let ((this-command 'project-find-file))
          (should (equal (project-root (project-current nil "/tmp/proj-b/"))
                         "/tmp/proj-b/")))))))

(ert-deftest wamei/project-tabs-without-tab-root-uses-buffer-project ()
  "root 未記録のタブでは従来どおりバッファのプロジェクトを見る。"
  (wamei/project-tabs-test--with-projects
    (wamei/project-tabs-test--with-tab-bar
      (wamei/project-tabs-test--with-setup
        (should (equal (wamei/project-tabs-test--root-in
                        "/tmp/proj-b/" 'project-find-file)
                       "/tmp/proj-b/"))
        (should-not (wamei/project-tabs-test--root-in
                     "/tmp/elsewhere/" 'project-find-file))))))

;;; child frame からの解決

(ert-deftest wamei/project-tabs-base-frame-returns-frame-itself-without-parent ()
  (should (eq (wamei/project-tabs-base-frame (selected-frame)) (selected-frame))))

(ert-deftest wamei/project-tabs-base-frame-walks-up-parent-frames ()
  ;; 親子関係だけをスタブする。'child2 → 'child1 → selected-frame。
  (let ((parents (list (cons 'child2 'child1)
                       (cons 'child1 (selected-frame)))))
    (cl-letf (((symbol-function 'frame-parent)
               (lambda (frame) (alist-get frame parents))))
      (should (eq (wamei/project-tabs-base-frame 'child2) (selected-frame)))
      (should (eq (wamei/project-tabs-base-frame 'child1) (selected-frame))))))

(ert-deftest wamei/project-tabs-current-root-reads-the-base-frame ()
  ;; child frame には tabs が無い。親の tabs を読めていれば root が返る。
  (let ((tabs '((current-tab (name . "proj") (wamei-project . "/tmp/proj/")))))
    (cl-letf (((symbol-function 'frame-parent)
               (lambda (frame) (when (eq frame 'child) (selected-frame))))
              ((symbol-function 'frame-parameter)
               (lambda (frame param)
                 (cond ((eq frame 'child) nil)
                       ((eq param 'tabs) tabs)
                       (t nil)))))
      (should (equal (wamei/project-tabs-current-root 'child) "/tmp/proj/")))))

(ert-deftest wamei/project-tabs-main-window-ignores-child-frames ()
  ;; child frame にフォーカスがあっても、親フレームの選択 window を返す。
  ;; `selected-frame' まで child frame にすり替えて実際の状況を作る
  ;; (posframe にフォーカスがあるとき `selected-frame' は child frame)。
  ;; child frame の window (`child-window') を返す実装なら落ちる。
  (let ((parent (selected-frame))
        (main (selected-window)))
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'child-frame))
              ((symbol-function 'selected-window) (lambda () 'child-window))
              ((symbol-function 'frame-parent)
               (lambda (frame) (when (eq frame 'child-frame) parent)))
              ((symbol-function 'frame-selected-window)
               (lambda (&optional frame)
                 (if (eq frame parent) main 'child-window))))
      (should (eq (wamei/project-tabs-main-window) main)))))

(ert-deftest wamei/project-tabs-pin-name-uses-the-base-frame ()
  ;; `wamei/project-tabs--pin-name-soon' は `window-buffer-change-functions'
  ;; 経由なので、メモの posframe が出るたびに child frame を FRAME として
  ;; 渡してくる。child frame をそのまま選択すると、そこに幻の tabs パラメータ
  ;; が生えて親のタブの代わりに rename されてしまう。タブは常に最上位の
  ;; フレームのものを触る。
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (wamei/project-tabs-test--with-tab-bar
      (let ((parent (selected-frame)))
        (with-temp-buffer
          (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/")
          (cl-letf (((symbol-function 'frame-parent)
                     (lambda (frame) (when (eq frame 'child-frame) parent))))
            (should (equal (wamei/project-tabs-pin-name 'child-frame) "proj-a")))
          ;; 名前も root も親フレームのタブに入る。
          (let ((tab (tab-bar--current-tab)))
            (should (equal (alist-get 'name tab) "proj-a"))
            (should (alist-get 'explicit-name tab)))
          (should (equal (wamei/project-tabs-current-root parent) "/tmp/proj-a/")))))))

;;; タブを閉じたときの後始末

(defmacro wamei/project-tabs-test--with-buffers (specs &rest body)
  "SPECS の (VAR NAME DIR) ごとにバッファを作って BODY を評価し、最後に消す。"
  (declare (indent 1))
  `(let ,(mapcar (lambda (spec)
                   `(,(car spec) (with-current-buffer (generate-new-buffer ,(nth 1 spec))
                                  (setq default-directory ,(nth 2 spec))
                                  (current-buffer))))
                 specs)
     (unwind-protect (progn ,@body)
       (dolist (buffer (list ,@(mapcar #'car specs)))
         (when (buffer-live-p buffer)
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buffer)))))))

(defmacro wamei/project-tabs-test--with-cleanup-env (&rest body)
  "/tmp/proj-a/ と /tmp/proj-b/ をプロジェクトにし、タブを 1 つにして BODY を評価する。
選択 window は *scratch* にしておく (テストのバッファが表示中扱いにならないように)。"
  (declare (indent 0))
  `(let ((project-find-functions
          (list (lambda (dir)
                  (let ((dir (expand-file-name dir)))
                    (cond ((string-prefix-p "/tmp/proj-a/" dir) (cons 'transient "/tmp/proj-a/"))
                          ((string-prefix-p "/tmp/proj-b/" dir) (cons 'transient "/tmp/proj-b/")))))))
         (wamei/project-tabs-extra-buffer-functions nil)
         (wamei/project-tabs-before-kill-functions nil))
     (wamei/project-tabs-test--with-tab-bar
       (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
       ,@body)))

(ert-deftest wamei/project-tabs-root-open-p-sees-other-tabs ()
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-set-root "/tmp/proj-a/")
    (tab-new)
    (wamei/project-tabs-set-root "/tmp/proj-b/")
    (should (wamei/project-tabs-root-open-p "/tmp/proj-a/"))
    (should (wamei/project-tabs-root-open-p "/tmp/proj-b"))
    (should-not (wamei/project-tabs-root-open-p "/tmp/proj-c/"))))

(ert-deftest wamei/project-tabs-cleanup-kills-project-buffers ()
  "どのタブにも紐づいていないプロジェクトのバッファは消し、他のプロジェクトのものは残す。"
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-test--with-buffers ((a "a.el" "/tmp/proj-a/")
                                            (a-sub "sub.el" "/tmp/proj-a/src/")
                                            (b "b.el" "/tmp/proj-b/"))
      (wamei/project-tabs-cleanup-closed "/tmp/proj-a/")
      (should-not (buffer-live-p a))
      (should-not (buffer-live-p a-sub))
      (should (buffer-live-p b)))))

(ert-deftest wamei/project-tabs-cleanup-keeps-buffers-when-another-tab-has-root ()
  "同じプロジェクトのタブが他に残っていれば何も消さない。"
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-set-root "/tmp/proj-a/")
    (wamei/project-tabs-test--with-buffers ((a "a.el" "/tmp/proj-a/"))
      (wamei/project-tabs-cleanup-closed "/tmp/proj-a/")
      (should (buffer-live-p a)))))

(ert-deftest wamei/project-tabs-cleanup-keeps-buffers-shown-in-other-tabs ()
  "別のタブの window に出ているバッファは残す (タブを切り替えた後の ws から拾う)。"
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-test--with-buffers ((shown "shown.el" "/tmp/proj-a/")
                                            (hidden "hidden.el" "/tmp/proj-a/"))
      (wamei/project-tabs-set-root "/tmp/proj-b/")
      (set-window-buffer (selected-window) shown)
      (tab-new)
      (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
      (wamei/project-tabs-cleanup-closed "/tmp/proj-a/")
      (should (buffer-live-p shown))
      (should-not (buffer-live-p hidden)))))

(ert-deftest wamei/project-tabs-cleanup-keeps-buffers-shown-in-live-windows ()
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-test--with-buffers ((shown "shown.el" "/tmp/proj-a/"))
      (set-window-buffer (selected-window) shown)
      (wamei/project-tabs-cleanup-closed "/tmp/proj-a/")
      (should (buffer-live-p shown)))))

(ert-deftest wamei/project-tabs-cleanup-skips-hidden-buffers ()
  "空白で始まる内部バッファは project-buffers に入っていても消さない。"
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-test--with-buffers ((internal " *internal*" "/tmp/proj-a/"))
      (wamei/project-tabs-cleanup-closed "/tmp/proj-a/")
      (should (buffer-live-p internal)))))

(ert-deftest wamei/project-tabs-cleanup-kills-extra-buffers ()
  "`wamei/project-tabs-extra-buffer-functions' が返すバッファも消す (メモや sidebar)。"
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-test--with-buffers ((memo "proj-a.org" "/tmp/org/")
                                            (sidebar " *sidebar: proj-a*" "/tmp/proj-a/"))
      (let ((wamei/project-tabs-extra-buffer-functions
             (list (lambda (root)
                     (should (equal root "/tmp/proj-a/"))
                     (list memo sidebar)))))
        (wamei/project-tabs-cleanup-closed "/tmp/proj-a/")
        (should-not (buffer-live-p memo))
        (should-not (buffer-live-p sidebar))))))

(ert-deftest wamei/project-tabs-cleanup-runs-before-kill-functions ()
  "消す直前に ROOT と消すバッファを渡す。渡された時点ではまだ生きている。"
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-test--with-buffers ((a "a.el" "/tmp/proj-a/"))
      (let* ((seen nil)
             (wamei/project-tabs-before-kill-functions
              (list (lambda (root buffers)
                      (push (list root buffers (mapcar #'buffer-live-p buffers)) seen)))))
        (wamei/project-tabs-cleanup-closed "/tmp/proj-a/")
        (should (equal seen `(("/tmp/proj-a/" (,a) (t)))))))))

(ert-deftest wamei/project-tabs-cleanup-does-nothing-without-buffers ()
  (wamei/project-tabs-test--with-cleanup-env
    (let* ((called nil)
           (wamei/project-tabs-before-kill-functions
            (list (lambda (&rest _) (setq called t)))))
      (wamei/project-tabs-cleanup-closed "/tmp/proj-a/")
      (should-not called))))

(ert-deftest wamei/project-tabs-cleanup-kills-process-buffers-without-asking ()
  "端末や claude のようにプロセスが動いているバッファも確認なしで消す。"
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-test--with-buffers ((term "*term: proj-a*" "/tmp/proj-a/"))
      ;; make-pipe-process はディレクトリが無くても作れる (/tmp/proj-a/ は実在しない)
      (let ((process (make-pipe-process :name "term" :buffer term)))
        (unwind-protect
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) (error "Asked"))))
              (wamei/project-tabs-cleanup-closed "/tmp/proj-a/")
              (should-not (buffer-live-p term)))
          (when (process-live-p process) (delete-process process)))))))

(ert-deftest wamei/project-tabs-close-cleans-up-closed-project ()
  "タブを閉じると、そのプロジェクトのバッファを次のコマンド境界で消す。"
  (wamei/project-tabs-test--with-cleanup-env
    (wamei/project-tabs-test--with-buffers ((a "a.el" "/tmp/proj-a/")
                                            (b "b.el" "/tmp/proj-b/"))
      (let ((scheduled nil))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_time _repeat fn &rest args)
                     (push (cons fn args) scheduled))))
          (let ((tab-bar-tab-pre-close-functions '(wamei/project-tabs--on-close)))
            (wamei/project-tabs-set-root "/tmp/proj-b/")
            (tab-new)
            (wamei/project-tabs-set-root "/tmp/proj-a/")
            (tab-close)
            ;; 閉じている最中には消さない
            (should (buffer-live-p a))
            (pcase-dolist (`(,fn . ,args) scheduled) (apply fn args))
            (should-not (buffer-live-p a))
            (should (buffer-live-p b))))))))

(ert-deftest wamei/project-tabs-close-skips-last-tab-and-unbound-tab ()
  "最後のタブ (閉じられない) と、プロジェクトが紐づいていないタブでは何もしない。"
  (cl-letf (((symbol-function 'run-at-time)
             (lambda (&rest _) (error "Scheduled"))))
    (wamei/project-tabs--on-close '(tab (wamei-project . "/tmp/proj-a/")) t)
    (wamei/project-tabs--on-close '(tab (name . "x")) nil)))

(ert-deftest wamei/project-tabs-set-root-runs-root-set-functions ()
  "root が新しく紐づいたときだけ `wamei/project-tabs-root-set-functions' を呼ぶ。"
  (wamei/project-tabs-test--with-tab-bar
    (let* ((seen nil)
           (wamei/project-tabs-root-set-functions
            (list (lambda (root) (push root seen)))))
      (wamei/project-tabs-set-root "/tmp/proj-a")
      (wamei/project-tabs-set-root "/tmp/proj-a/")
      (wamei/project-tabs-set-root "/tmp/proj-b/")
      (should (equal seen '("/tmp/proj-b/" "/tmp/proj-a/"))))))

(provide 'project-tabs-test)
;;; project-tabs-test.el ends here
