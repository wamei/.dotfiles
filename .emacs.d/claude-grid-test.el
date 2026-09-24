;;; claude-grid-test.el --- tests for claude-grid -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l claude-grid-test.el -f ert-run-tests-batch-and-exit
;;
;; claude-code-ide 本体 (と依存の websocket / web-server) は package-initialize
;; で ~/.emacs.d/elpa から読む。MCP サーバーは立てず、セッション構造体を直接
;; 作ってレジストリに入れる (claude-panel-test.el と同じ流儀)。
;;
;; batch のフレームは 80x25 だが、set-frame-width / set-frame-height で実際の
;; window の寸法は変わる (frame-width の戻り値は 80 のまま。レイアウトは
;; main window の window-total-width を見ているので影響しない)。
;;; Code:

(require 'ert)
(package-initialize)
(require 'claude-code-ide)
(require 'tab-bar)
(require 'tab-line)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "claude-panel.el" dir) nil t)
  (load (expand-file-name "claude-grid.el" dir) nil t))

;;; フィクスチャ

(defvar wamei/claude-grid-test--counter 0)

(defun wamei/claude-grid-test--session (project-dir &optional instance-name)
  "PROJECT-DIR の Claude セッションをバッファ付きで作り、レジストリに登録する。"
  (let* ((id (format "test-%d" (cl-incf wamei/claude-grid-test--counter)))
         (buffer (generate-new-buffer (format "*claude-code[%s%s]*"
                                              (file-name-nondirectory
                                               (directory-file-name project-dir))
                                              (if instance-name
                                                  (concat ":" instance-name)
                                                ""))))
         (session (make-claude-code-ide-mcp-session
                   :session-id id
                   :instance-name instance-name
                   :buffer buffer
                   :project-dir project-dir
                   :window-slot 0)))
    (with-current-buffer buffer
      (setq-local claude-code-ide--session session))
    (puthash id session claude-code-ide-mcp--sessions)
    buffer))

(defmacro wamei/claude-grid-test--with-env (&rest body)
  "空のセッションレジストリ・広いフレーム・tab-bar 有効で BODY を評価する。
作ったバッファとタブは最後に片付ける。"
  (declare (indent 0))
  `(let ((claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         (claude-code-ide--last-accessed-buffer nil)
         (wamei/claude-panel--order nil)
         (buffers nil))
     (tab-bar-mode 1)
     (set-frame-width nil 300)
     (set-frame-height nil 100)
     (let ((initial (length (funcall tab-bar-tabs-function))))
       (save-window-excursion
         (delete-other-windows)
         (unwind-protect
             (progn ,@body)
           (tab-bar-select-tab 1)
           (while (> (length (funcall tab-bar-tabs-function)) initial)
             (tab-bar-close-tab (length (funcall tab-bar-tabs-function))))
           (maphash (lambda (_id session)
                      (push (claude-code-ide-mcp-session-buffer session) buffers))
                    claude-code-ide-mcp--sessions)
           (dolist (buffer buffers)
             (when (buffer-live-p buffer)
               (let ((kill-buffer-hook nil))
                 (kill-buffer buffer)))))))))

(defun wamei/claude-grid-test--tab-names ()
  "選択フレームのタブ名のリスト。"
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

(defun wamei/claude-grid-test--rows ()
  "選択フレームのグリッドを行ごとのバッファのリストにして返す。
side window とミニバッファは除く。行は上から、各行は左から。"
  (let ((windows (seq-remove (lambda (window)
                               (window-parameter window 'window-side))
                             (window-list nil 'no-mini)))
        (rows nil))
    (dolist (window (sort windows (lambda (a b)
                                    (let ((ta (window-top-line a))
                                          (tb (window-top-line b)))
                                      (if (= ta tb)
                                          (< (window-left-column a)
                                             (window-left-column b))
                                        (< ta tb))))))
      (let ((cell (assq (window-top-line window) rows)))
        (if cell
            (setcdr cell (append (cdr cell) (list (window-buffer window))))
          (push (cons (window-top-line window) (list (window-buffer window)))
                rows))))
    (mapcar #'cdr (nreverse rows))))

;;; レイアウト

(ert-deftest wamei/claude-grid-layout-puts-everything-in-one-row-when-it-fits ()
  "1 個あたりが最小幅を保てる限り左右に並べる。"
  (let ((wamei/claude-grid-min-window-width 80))
    (should (equal (wamei/claude-grid--layout 1 300 100) '(1)))
    (should (equal (wamei/claude-grid--layout 2 300 100) '(2)))
    (should (equal (wamei/claude-grid--layout 3 300 100) '(3)))))

(ert-deftest wamei/claude-grid-layout-wraps-when-too-narrow ()
  "最小幅を切るなら行を足してグリッドにする。"
  (let ((wamei/claude-grid-min-window-width 80))
    ;; 幅 300 に 80 桁は 3 個まで。4 個は 2 行に均等 (3 + 1 ではない)
    (should (equal (wamei/claude-grid--layout 4 300 100) '(2 2)))
    (should (equal (wamei/claude-grid--layout 5 300 100) '(3 2)))
    (should (equal (wamei/claude-grid--layout 6 300 100) '(3 3)))
    (should (equal (wamei/claude-grid--layout 7 300 100) '(3 2 2)))))

(ert-deftest wamei/claude-grid-layout-falls-back-to-single-column ()
  "フレームが最小幅より狭くても 1 列にはする。"
  (let ((wamei/claude-grid-min-window-width 80))
    (should (equal (wamei/claude-grid--layout 3 60 100) '(1 1 1)))))

(ert-deftest wamei/claude-grid-layout-truncates-when-too-short ()
  "高さが足りないぶんは並べない。"
  (let ((wamei/claude-grid-min-window-width 80)
        (wamei/claude-grid-min-window-height 10))
    ;; 高さ 25 なら 2 行まで。6 個のうち 6 個は入るが 9 個は 6 個で打ち切る
    (should (equal (wamei/claude-grid--layout 6 300 25) '(3 3)))
    (should (equal (wamei/claude-grid--layout 9 300 25) '(3 3)))))

(ert-deftest wamei/claude-grid-layout-returns-nil-for-no-buffers ()
  (should-not (wamei/claude-grid--layout 0 300 100)))

;;; タブ名

(ert-deftest wamei/claude-grid-tab-name-without-project ()
  (should (equal (wamei/claude-grid--tab-name nil) "Claude Code")))

(ert-deftest wamei/claude-grid-tab-name-with-project ()
  (should (equal (wamei/claude-grid--tab-name "/tmp/proj/") "Claude Code - proj"))
  (should (equal (wamei/claude-grid--tab-name "/tmp/proj") "Claude Code - proj")))

;;; バッファ一覧

(ert-deftest wamei/claude-grid-buffers-lists-every-session-by-name ()
  "引数なしでは全プロジェクトのセッションをセッション名の昇順で返す。
パッケージのセッション一覧は順序不定なので、並びは登録順に依存しない。"
  (wamei/claude-grid-test--with-env
    (let ((b (wamei/claude-grid-test--session "/tmp/proj/" "b"))
          (plain (wamei/claude-grid-test--session "/tmp/proj/"))
          (other (wamei/claude-grid-test--session "/tmp/other/")))
      ;; other < proj < proj:b
      (should (equal (wamei/claude-grid--buffers nil) (list other plain b))))))

(ert-deftest wamei/claude-grid-buffers-limits-to-project ()
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/"))
          (other (wamei/claude-grid-test--session "/tmp/other/")))
      (ignore other)
      (should (equal (wamei/claude-grid--buffers "/tmp/proj/") (list a))))))

(ert-deftest wamei/claude-grid-buffers-skips-dead-buffers ()
  (wamei/claude-grid-test--with-env
    (let ((dead (wamei/claude-grid-test--session "/tmp/proj/" "dead"))
          (live (wamei/claude-grid-test--session "/tmp/proj/" "live")))
      (kill-buffer dead)
      (should (equal (wamei/claude-grid--buffers nil) (list live))))))

;;; 組み立て

(ert-deftest wamei/claude-grid-grid-window-avoids-the-minibuffer ()
  "土台にミニバッファの window を選ばない (`delete-other-windows' が失敗する)。
組み直しはタイマから走るので、ミニバッファ入力中に呼ばれることがある。"
  (wamei/claude-grid-test--with-env
    (select-window (minibuffer-window))
    (unwind-protect
        (let ((window (wamei/claude-grid--grid-window)))
          (should (window-live-p window))
          (should-not (window-minibuffer-p window)))
      (select-window (frame-first-window)))))

(ert-deftest wamei/claude-grid-build-works-from-the-minibuffer ()
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (select-window (minibuffer-window))
      (unwind-protect
          (progn
            (wamei/claude-grid--build (list a))
            (should (equal (wamei/claude-grid-test--rows) (list (list a)))))
        (unless (window-live-p (selected-window))
          (select-window (frame-first-window)))))))


(ert-deftest wamei/claude-grid-build-fills-one-row ()
  (wamei/claude-grid-test--with-env
    (let* ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
           (b (wamei/claude-grid-test--session "/tmp/proj/" "b"))
           (windows (wamei/claude-grid--build (list a b))))
      (should (= 2 (length windows)))
      (should (equal (wamei/claude-grid-test--rows) (list (list a b)))))))

(ert-deftest wamei/claude-grid-build-fills-grid-row-major ()
  "左上から行方向に詰める。"
  (wamei/claude-grid-test--with-env
    (let* ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
           (b (wamei/claude-grid-test--session "/tmp/proj/" "b"))
           (c (wamei/claude-grid-test--session "/tmp/proj/" "c"))
           (d (wamei/claude-grid-test--session "/tmp/proj/" "d"))
           (wamei/claude-grid-min-window-width 80)
           (windows (wamei/claude-grid--build (list a b c d))))
      (should (= 4 (length windows)))
      (should (equal (wamei/claude-grid-test--rows)
                     (list (list a b) (list c d))))
      ;; 戻り値も左上からの順
      (should (equal (mapcar #'window-buffer windows) (list a b c d))))))

(ert-deftest wamei/claude-grid-build-replaces-existing-windows ()
  "組み直しでは前の window 構成を捨てる。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (split-window (selected-window) nil 'below)
      (split-window (selected-window) nil 'right)
      (wamei/claude-grid--build (list a))
      (should (equal (wamei/claude-grid-test--rows) (list (list a)))))))

(ert-deftest wamei/claude-grid-build-removes-side-windows ()
  "同じバッファを side window と両方に出すと端末バッファの pty サイズが競合するので消す。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (display-buffer-in-side-window (get-buffer-create "*side*")
                                     '((side . right) (slot . 0)))
      (should (seq-find (lambda (window) (window-parameter window 'window-side))
                        (window-list nil 'no-mini)))
      (wamei/claude-grid--build (list a))
      (should-not (seq-find (lambda (window) (window-parameter window 'window-side))
                            (window-list nil 'no-mini)))
      (kill-buffer "*side*"))))

(ert-deftest wamei/claude-grid-build-selects-first-window ()
  (wamei/claude-grid-test--with-env
    (let* ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
           (b (wamei/claude-grid-test--session "/tmp/proj/" "b")))
      (wamei/claude-grid--build (list a b))
      (should (eq (window-buffer (selected-window)) a)))))

(ert-deftest wamei/claude-grid-build-sets-up-tab-line ()
  "グリッドで初めて表示したセッションにも tab-line と巡回キーを付ける。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (wamei/claude-grid--build (list a))
      (with-current-buffer a
        (should tab-line-mode)
        ;; グローバルの C-tab を上書きするバッファローカルなマイナーモードが付く
        ;; (init.el は tab-bar-mode-map の C-tab を外しているので、実機では
        ;; これがそのまま効く)
        (should wamei/claude-panel-keys-mode)
        (should (eq (key-binding (kbd "<C-tab>")) #'wamei/claude-panel-next))))))

(ert-deftest wamei/claude-grid-build-windows-are-usable ()
  "グリッドの window は side window ではなく、C-x o の巡回にも乗る。"
  (wamei/claude-grid-test--with-env
    (let* ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
           (windows (wamei/claude-grid--build (list a))))
      (dolist (window windows)
        (should-not (window-parameter window 'window-side))
        (should-not (window-parameter window 'no-other-window))
        (should-not (window-dedicated-p window))))))

;;; コマンド

(ert-deftest wamei/claude-grid-tab-creates-named-tab ()
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (wamei/claude-grid-tab t)
      (should (member "Claude Code" (wamei/claude-grid-test--tab-names)))
      (should (equal (alist-get 'name (tab-bar--current-tab-find)) "Claude Code"))
      (should (equal (wamei/claude-grid-test--rows) (list (list a)))))))

(ert-deftest wamei/claude-grid-tab-reuses-its-tab ()
  "再実行では同じタブを使い、今のセッションで組み直す。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (wamei/claude-grid-tab t)
      (let ((count (length (wamei/claude-grid-test--tab-names)))
            (b (wamei/claude-grid-test--session "/tmp/proj/" "b")))
        (tab-bar-select-tab 1)
        (wamei/claude-grid-tab t)
        (should (= count (length (wamei/claude-grid-test--tab-names))))
        (should (equal (wamei/claude-grid-test--rows) (list (list a b))))))))

(ert-deftest wamei/claude-grid-tab-uses-separate-tab-per-project ()
  "引数なしではカレントプロジェクトだけを別のタブに並べる。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
          (other (wamei/claude-grid-test--session "/tmp/other/")))
      (wamei/claude-grid-tab t)
      ;; other < proj:a
      (should (equal (wamei/claude-grid-test--rows) (list (list other a))))
      ;; default-directory はバッファローカルなので、プロジェクトのバッファから
      ;; 叩いた状況を作る
      (with-temp-buffer
        (setq default-directory "/tmp/proj/")
        (wamei/claude-grid-tab))
      (should (equal (alist-get 'name (tab-bar--current-tab-find))
                     "Claude Code - proj"))
      (should (equal (wamei/claude-grid-test--rows) (list (list a))))
      (should (member "Claude Code" (wamei/claude-grid-test--tab-names)))
      (should (member "Claude Code - proj" (wamei/claude-grid-test--tab-names))))))

(ert-deftest wamei/claude-grid-tab-uses-session-project-in-claude-buffer ()
  "グリッドの中で叩いたら、見ているセッションのプロジェクトが対象になる。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
          (other (wamei/claude-grid-test--session "/tmp/other/")))
      (ignore other)
      (with-current-buffer a
        (wamei/claude-grid-tab))
      (should (equal (alist-get 'name (tab-bar--current-tab-find))
                     "Claude Code - proj"))
      (should (equal (wamei/claude-grid-test--rows) (list (list a)))))))

(ert-deftest wamei/claude-grid-tab-does-nothing-without-sessions ()
  (wamei/claude-grid-test--with-env
    (let ((before (wamei/claude-grid-test--tab-names)))
      (wamei/claude-grid-tab t)
      (should (equal before (wamei/claude-grid-test--tab-names))))))

;;; タブの範囲

(ert-deftest wamei/claude-grid-tab-scope-reads-the-tab-name ()
  "グリッドタブかどうかと対象はタブ名から決まる。"
  (should (eq (wamei/claude-grid--tab-scope "Claude Code") t))
  (should (equal (wamei/claude-grid--tab-scope "Claude Code - proj") "proj")))

(ert-deftest wamei/claude-grid-tab-scope-rejects-other-tabs ()
  (should-not (wamei/claude-grid--tab-scope nil))
  (should-not (wamei/claude-grid--tab-scope "proj"))
  (should-not (wamei/claude-grid--tab-scope "Claude Code X")))

(ert-deftest wamei/claude-grid-scope-buffers-takes-everything-for-the-plain-tab ()
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
          (other (wamei/claude-grid-test--session "/tmp/other/")))
      (should (equal (wamei/claude-grid--scope-buffers t) (list other a))))))

(ert-deftest wamei/claude-grid-scope-buffers-limits-to-the-tab-project ()
  "プロジェクト名だけでも対象を絞れる (タブ名に残っているのは名前だけ)。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
          (other (wamei/claude-grid-test--session "/tmp/other/")))
      (ignore other)
      (should (equal (wamei/claude-grid--scope-buffers "proj") (list a)))
      (should-not (wamei/claude-grid--scope-buffers "none")))))

;;; 組み直し

(ert-deftest wamei/claude-grid-rearrange-drops-a-dead-session ()
  "セッションが終わったらその window も消える。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
          (b (wamei/claude-grid-test--session "/tmp/proj/" "b"))
          (c (wamei/claude-grid-test--session "/tmp/proj/" "c")))
      (wamei/claude-grid-tab t)
      (should (equal (wamei/claude-grid-test--rows) (list (list a b c))))
      (let ((kill-buffer-hook nil)) (kill-buffer b))
      (wamei/claude-grid-rearrange)
      (should (equal (wamei/claude-grid-test--rows) (list (list a c)))))))

(ert-deftest wamei/claude-grid-rearrange-adds-a-new-session ()
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (wamei/claude-grid-tab t)
      (let ((b (wamei/claude-grid-test--session "/tmp/proj/" "b")))
        (wamei/claude-grid-rearrange)
        (should (equal (wamei/claude-grid-test--rows) (list (list a b))))))))

(ert-deftest wamei/claude-grid-rearrange-keeps-the-focused-session ()
  "組み直しでも見ていたセッションから離れない (`--build' 単体は先頭を選ぶ)。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
          (b (wamei/claude-grid-test--session "/tmp/proj/" "b"))
          (c (wamei/claude-grid-test--session "/tmp/proj/" "c")))
      (wamei/claude-grid-tab t)
      (select-window (get-buffer-window c))
      (let ((kill-buffer-hook nil)) (kill-buffer a))
      (wamei/claude-grid-rearrange)
      (should (eq (window-buffer (selected-window)) c)))))

(ert-deftest wamei/claude-grid-rearrange-keeps-a-project-tab-to-its-project ()
  "プロジェクト指定のタブは他のプロジェクトのセッションを取り込まない。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (with-temp-buffer
        (setq default-directory "/tmp/proj/")
        (wamei/claude-grid-tab))
      (wamei/claude-grid-test--session "/tmp/other/")
      (wamei/claude-grid-rearrange)
      (should (equal (wamei/claude-grid-test--rows) (list (list a))))
      (let ((b (wamei/claude-grid-test--session "/tmp/proj/" "b")))
        (wamei/claude-grid-rearrange)
        (should (equal (wamei/claude-grid-test--rows) (list (list a b))))))))

(ert-deftest wamei/claude-grid-rearrange-ignores-other-tabs ()
  "グリッドタブ以外の window 構成は触らない。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (ignore a)
      (split-window (selected-window) nil 'below)
      (let ((windows (length (window-list nil 'no-mini))))
        (wamei/claude-grid-rearrange)
        (should (= windows (length (window-list nil 'no-mini))))))))

(ert-deftest wamei/claude-grid-rearrange-closes-the-emptied-tab ()
  "最後のセッションが終わったらタブごと閉じる。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a")))
      (wamei/claude-grid-tab t)
      (should (member "Claude Code" (wamei/claude-grid-test--tab-names)))
      (let ((kill-buffer-hook nil)) (kill-buffer a))
      (wamei/claude-grid-rearrange)
      (should-not (member "Claude Code" (wamei/claude-grid-test--tab-names))))))

(ert-deftest wamei/claude-grid-rearrange-keeps-the-sole-tab ()
  "フレームに 1 枚しかないタブは閉じられないので、window を畳むだけにする。"
  (wamei/claude-grid-test--with-env
    (should (= 1 (length (funcall tab-bar-tabs-function))))
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
          (b (wamei/claude-grid-test--session "/tmp/proj/" "b")))
      (unwind-protect
          (progn
            (wamei/claude-grid--build (list a b))
            (tab-bar-rename-tab wamei/claude-grid-tab-name)
            (let ((kill-buffer-hook nil))
              (kill-buffer a)
              (kill-buffer b))
            (wamei/claude-grid-rearrange)
            (should (= 1 (length (funcall tab-bar-tabs-function))))
            (should (= 1 (length (window-list nil 'no-mini)))))
        ;; 自動のタブ名に戻す
        (tab-bar-rename-tab "")))))

;;; 予約

(ert-deftest wamei/claude-grid-schedule-coalesces-into-one-rearrange ()
  "増減が重なっても組み直しは 1 回にまとめる。"
  (let ((wamei/claude-grid--rearrange-timer nil))
    (unwind-protect
        (progn
          (wamei/claude-grid--schedule-rearrange)
          (let ((timer wamei/claude-grid--rearrange-timer))
            (should (timerp timer))
            (wamei/claude-grid--schedule-rearrange)
            (should (eq timer wamei/claude-grid--rearrange-timer))))
      (when (timerp wamei/claude-grid--rearrange-timer)
        (cancel-timer wamei/claude-grid--rearrange-timer)))))

(ert-deftest wamei/claude-grid-schedule-runs-the-rearrange-later ()
  "予約は次の機会に走り、タイマは 1 回で外れる。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
          (b (wamei/claude-grid-test--session "/tmp/proj/" "b"))
          (wamei/claude-grid--rearrange-timer nil))
      (wamei/claude-grid-tab t)
      (let ((kill-buffer-hook nil)) (kill-buffer b))
      (wamei/claude-grid--schedule-rearrange)
      ;; バッファを消した直後は組み直していない (sentinel の中では動かさない)
      (should (= 2 (length (window-list nil 'no-mini))))
      (sleep-for 0.2)
      (should-not wamei/claude-grid--rearrange-timer)
      (should (equal (wamei/claude-grid-test--rows) (list (list a)))))))

(ert-deftest wamei/claude-grid-schedule-waits-for-the-minibuffer ()
  "ミニバッファ入力中は組み直さず、少し後に出直す。
入力を出しているコマンドが `save-window-excursion' で元に戻すと、
組み直した構成もろとも消える。"
  (wamei/claude-grid-test--with-env
    (let ((a (wamei/claude-grid-test--session "/tmp/proj/" "a"))
          (b (wamei/claude-grid-test--session "/tmp/proj/" "b"))
          (wamei/claude-grid--rearrange-timer nil))
      (wamei/claude-grid-tab t)
      (let ((kill-buffer-hook nil)) (kill-buffer b))
      (unwind-protect
          (progn
            (select-window (minibuffer-window))
            (wamei/claude-grid--run-rearrange)
            ;; 組み直さずに予約し直す
            (should (timerp wamei/claude-grid--rearrange-timer))
            (should (= 2 (length (window-list nil 'no-mini))))
            ;; 出直しの予約は後続のテストに漏らさない
            (cancel-timer wamei/claude-grid--rearrange-timer)
            (setq wamei/claude-grid--rearrange-timer nil)
            ;; ミニバッファから出れば組み直す
            (select-window (frame-first-window))
            (wamei/claude-grid--run-rearrange)
            (should (equal (wamei/claude-grid-test--rows) (list (list a)))))
        (when (timerp wamei/claude-grid--rearrange-timer)
          (cancel-timer wamei/claude-grid--rearrange-timer))))))

(ert-deftest wamei/claude-grid-enable-schedules-on-session-change ()
  "セッションの終了と表示で組み直しを予約する。"
  (wamei/claude-grid-test--with-env
    (let ((wamei/claude-grid--rearrange-timer nil))
      (unwind-protect
          (progn
            (wamei/claude-grid-enable)
            (should (advice-member-p #'wamei/claude-grid--schedule-rearrange
                                     'claude-code-ide--cleanup-session))
            (should (advice-member-p #'wamei/claude-grid--schedule-rearrange
                                     'claude-code-ide--display-buffer-in-side-window))
            ;; パッケージの後始末を実際に通して予約されることを確かめる
            (let ((buffer (wamei/claude-grid-test--session "/tmp/proj/" "a")))
              (claude-code-ide--cleanup-session
               (claude-code-ide--buffer-session buffer))
              (should (timerp wamei/claude-grid--rearrange-timer))))
        (when (timerp wamei/claude-grid--rearrange-timer)
          (cancel-timer wamei/claude-grid--rearrange-timer))
        (advice-remove 'claude-code-ide--cleanup-session
                       #'wamei/claude-grid--schedule-rearrange)
        (advice-remove 'claude-code-ide--display-buffer-in-side-window
                       #'wamei/claude-grid--schedule-rearrange)))))

(provide 'claude-grid-test)
;;; claude-grid-test.el ends here
