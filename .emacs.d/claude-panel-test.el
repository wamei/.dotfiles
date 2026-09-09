;;; claude-panel-test.el --- tests for claude-panel -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l claude-panel-test.el -f ert-run-tests-batch-and-exit
;;
;; claude-code-ide 本体 (と依存の websocket / web-server) は package-initialize
;; で ~/.emacs.d/elpa から読む。MCP サーバーは立てず、セッション構造体を直接
;; 作ってレジストリに入れる。
;;; Code:

(require 'ert)
(package-initialize)
(require 'claude-code-ide)
(require 'tab-line)
(load (expand-file-name "claude-panel.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; ghostel 本体の buffer-local 変数 (テスト用のスタブ定義)。
(defvar-local ghostel-title nil
  "端末が報告したタイトル。")

;; wamei/claude-panel-map が s-v に束縛するコマンドの実体。
(load (expand-file-name "term-input.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defvar wamei/claude-panel-test--counter 0)

(defun wamei/claude-panel-test--session (project-dir &optional instance-name)
  "PROJECT-DIR の Claude セッションをバッファ付きで作り、レジストリに登録する。"
  (let* ((id (format "test-%d" (cl-incf wamei/claude-panel-test--counter)))
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
                   :window-slot (* 10 wamei/claude-panel-test--counter))))
    (with-current-buffer buffer
      (setq-local claude-code-ide--session session))
    (puthash id session claude-code-ide-mcp--sessions)
    session))

(defmacro wamei/claude-panel-test--with-env (&rest body)
  "空のセッションレジストリと 1 window のフレームで BODY を評価する。
作ったバッファは最後に消す。"
  (declare (indent 0))
  `(let ((claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         (claude-code-ide--last-accessed-buffer nil)
         (claude-code-ide-focus-on-open t)
         (wamei/claude-panel--order nil)
         (buffers nil))
     (save-window-excursion
       (delete-other-windows)
       (unwind-protect
           (progn
             (wamei/claude-panel-enable)
             ,@body)
         (maphash (lambda (_id session)
                    (push (claude-code-ide-mcp-session-buffer session) buffers))
                  claude-code-ide-mcp--sessions)
         (dolist (buffer buffers)
           (when (buffer-live-p buffer)
             (let ((kill-buffer-hook nil))
               (kill-buffer buffer))))))))

(defun wamei/claude-panel-test--claude-windows ()
  "選択フレームで Claude セッションを表示している window のリスト。"
  (seq-filter (lambda (window)
                (claude-code-ide--buffer-session (window-buffer window)))
              (window-list nil 'no-mini)))

;;; セッション一覧

(ert-deftest wamei/claude-panel-buffers-lists-project-sessions-in-registration-order ()
  (wamei/claude-panel-test--with-env
    (let* ((b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (a (wamei/claude-panel-test--session "/tmp/proj/"))
           (other (wamei/claude-panel-test--session "/tmp/other/")))
      (wamei/claude-panel--register (claude-code-ide-mcp-session-buffer b))
      (wamei/claude-panel--register (claude-code-ide-mcp-session-buffer other))
      (wamei/claude-panel--register (claude-code-ide-mcp-session-buffer a))
      (should (equal (wamei/claude-panel--buffers "/tmp/proj/")
                     (list (claude-code-ide-mcp-session-buffer b)
                           (claude-code-ide-mcp-session-buffer a)))))))

(ert-deftest wamei/claude-panel-buffers-skips-dead-buffers ()
  (wamei/claude-panel-test--with-env
    (let* ((dead (wamei/claude-panel-test--session "/tmp/proj/" "dead"))
           (live (wamei/claude-panel-test--session "/tmp/proj/" "live")))
      (wamei/claude-panel--register (claude-code-ide-mcp-session-buffer dead))
      (wamei/claude-panel--register (claude-code-ide-mcp-session-buffer live))
      (kill-buffer (claude-code-ide-mcp-session-buffer dead))
      (should (equal (wamei/claude-panel--buffers "/tmp/proj/")
                     (list (claude-code-ide-mcp-session-buffer live)))))))

(ert-deftest wamei/claude-panel-buffers-appends-unregistered-sessions ()
  "モジュール読み込み前から動いているセッションもタブに出る。"
  (wamei/claude-panel-test--with-env
    (let* ((early (wamei/claude-panel-test--session "/tmp/proj/" "early"))
           (shown (wamei/claude-panel-test--session "/tmp/proj/" "shown")))
      (wamei/claude-panel--register (claude-code-ide-mcp-session-buffer shown))
      (should (equal (wamei/claude-panel--buffers "/tmp/proj/")
                     (list (claude-code-ide-mcp-session-buffer shown)
                           (claude-code-ide-mcp-session-buffer early))))
      ;; 一度並んだら順序は固定される
      (should (equal (wamei/claude-panel--buffers "/tmp/proj/")
                     (list (claude-code-ide-mcp-session-buffer shown)
                           (claude-code-ide-mcp-session-buffer early)))))))

(ert-deftest wamei/claude-panel-enable-sets-up-existing-sessions ()
  "読み込み時点で動いているセッションのバッファにも tab-line を付ける。"
  (wamei/claude-panel-test--with-env
    (let* ((early (wamei/claude-panel-test--session "/tmp/proj/"))
           (buffer (claude-code-ide-mcp-session-buffer early)))
      (wamei/claude-panel-enable)
      (with-current-buffer buffer
        (should tab-line-mode)
        (should (eq (key-binding (kbd "<C-tab>")) #'wamei/claude-panel-next))))))

(ert-deftest wamei/claude-panel-register-is-idempotent ()
  (wamei/claude-panel-test--with-env
    (let ((a (wamei/claude-panel-test--session "/tmp/proj/")))
      (wamei/claude-panel--register (claude-code-ide-mcp-session-buffer a))
      (wamei/claude-panel--register (claude-code-ide-mcp-session-buffer a))
      (should (= 1 (length (wamei/claude-panel--buffers "/tmp/proj/")))))))

;;; タブ名

(ert-deftest wamei/claude-panel-tab-name-uses-session-display-name ()
  (wamei/claude-panel-test--with-env
    (let ((plain (wamei/claude-panel-test--session "/tmp/proj/"))
          (named (wamei/claude-panel-test--session "/tmp/proj/" "refactor")))
      (should (equal (wamei/claude-panel--tab-name
                      (claude-code-ide-mcp-session-buffer plain))
                     "proj"))
      (should (equal (wamei/claude-panel--tab-name
                      (claude-code-ide-mcp-session-buffer named))
                     "proj:refactor")))))

;;; タブ名: Claude が端末タイトルに出すセッション名

(ert-deftest wamei/claude-panel-tab-name-comes-from-ghostel-title ()
  "タブ名は端末が報告したタイトルから状態表示を外したもの。"
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/" "a"))
           (buffer (claude-code-ide-mcp-session-buffer a)))
      (with-current-buffer buffer
        (setq-local ghostel-title "✳ 会話の名前"))
      (should (equal (wamei/claude-panel--tab-name buffer) "会話の名前")))))

(ert-deftest wamei/claude-panel-tab-name-keeps-fallback-on-blank-title ()
  "状態表示だけのタイトルや空のタイトルではセッション名に落とす。"
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/" "a"))
           (buffer (claude-code-ide-mcp-session-buffer a)))
      (with-current-buffer buffer
        (setq-local ghostel-title "✳ "))
      (should (equal (wamei/claude-panel--tab-name buffer) "proj:a"))
      (with-current-buffer buffer
        (setq-local ghostel-title nil))
      (should (equal (wamei/claude-panel--tab-name buffer) "proj:a")))))

(ert-deftest wamei/claude-panel-cache-key-changes-with-title ()
  "タイトルが変わったら tab-line のキャッシュが無効になる。"
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (buffer (claude-code-ide-mcp-session-buffer a)))
      (claude-code-ide--display-buffer-in-side-window buffer)
      (with-current-buffer buffer
        (should (eq tab-line-cache-key-function #'wamei/claude-panel--cache-key))
        (let ((before (wamei/claude-panel--cache-key (list buffer))))
          (with-current-buffer buffer (setq-local ghostel-title "✳ 新しい名前"))
          (should-not (equal before (wamei/claude-panel--cache-key (list buffer)))))))))

(ert-deftest wamei/claude-panel-map-binds-super-v-to-paste ()
  "Claude のバッファでは Cmd+V がクリップボードの画像を端末へ渡す。"
  (should (eq (lookup-key wamei/claude-panel-map (kbd "s-v"))
              #'wamei/term-input-paste)))

;;; 表示: 1 つのパネルに差し替える

(ert-deftest wamei/claude-panel-display-reuses-existing-panel-window ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (window (claude-code-ide--display-buffer-in-side-window
                    (claude-code-ide-mcp-session-buffer a))))
      (should (window-live-p window))
      (claude-code-ide--display-buffer-in-side-window
       (claude-code-ide-mcp-session-buffer b))
      (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer b)))
      (should (= 1 (length (wamei/claude-panel-test--claude-windows))))
      (should (window-dedicated-p window)))))

(ert-deftest wamei/claude-panel-display-reuses-panel-across-projects ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (other (wamei/claude-panel-test--session "/tmp/other/"))
           (window (claude-code-ide--display-buffer-in-side-window
                    (claude-code-ide-mcp-session-buffer a))))
      (claude-code-ide--display-buffer-in-side-window
       (claude-code-ide-mcp-session-buffer other))
      (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer other)))
      (should (= 1 (length (wamei/claude-panel-test--claude-windows)))))))

(ert-deftest wamei/claude-panel-display-enables-tab-line-in-buffer ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (buffer (claude-code-ide-mcp-session-buffer a)))
      (claude-code-ide--display-buffer-in-side-window buffer)
      (with-current-buffer buffer
        (should tab-line-mode)
        (should (eq tab-line-tabs-function #'wamei/claude-panel--tabs))
        (should (eq tab-line-tab-name-function #'wamei/claude-panel--tab-name))
        (should-not tab-line-new-button-show)
        (should-not tab-line-close-button-show)
        (should (memq buffer (wamei/claude-panel--buffers "/tmp/proj/")))))))

(ert-deftest wamei/claude-panel-tabs-returns-current-project-buffers ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (_other (wamei/claude-panel-test--session "/tmp/other/")))
      (dolist (session (list a b _other))
        (claude-code-ide--display-buffer-in-side-window
         (claude-code-ide-mcp-session-buffer session)))
      (with-current-buffer (claude-code-ide-mcp-session-buffer a)
        (should (equal (wamei/claude-panel--tabs)
                       (list (claude-code-ide-mcp-session-buffer a)
                             (claude-code-ide-mcp-session-buffer b))))))))

;;; 切り替え

(ert-deftest wamei/claude-panel-tab-line-click-switches-dedicated-panel ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (window (claude-code-ide--display-buffer-in-side-window
                    (claude-code-ide-mcp-session-buffer a))))
      (claude-code-ide--display-buffer-in-side-window
       (claude-code-ide-mcp-session-buffer b))
      ;; tab-line のクリックは tab-line-select-tab-buffer に届く
      (tab-line-select-tab-buffer (claude-code-ide-mcp-session-buffer a) window)
      (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer a)))
      (should (= 1 (length (wamei/claude-panel-test--claude-windows)))))))

(ert-deftest wamei/claude-panel-tab-line-click-leaves-other-buffers-alone ()
  (wamei/claude-panel-test--with-env
    (let ((plain (generate-new-buffer "plain")))
      (unwind-protect
          (progn
            (tab-line-select-tab-buffer plain (selected-window))
            (should (eq (window-buffer (selected-window)) plain)))
        (kill-buffer plain)))))

(ert-deftest wamei/claude-panel-next-and-previous-cycle-with-wraparound ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (c (wamei/claude-panel-test--session "/tmp/proj/" "c"))
           (window nil))
      (dolist (session (list a b c))
        (setq window (claude-code-ide--display-buffer-in-side-window
                      (claude-code-ide-mcp-session-buffer session))))
      (with-selected-window window
        (wamei/claude-panel-next)
        (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer a)))
        (wamei/claude-panel-previous)
        (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer c)))
        (wamei/claude-panel-previous)
        (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer b)))))))

(ert-deftest wamei/claude-panel-next-with-single-session-is-noop ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (window (claude-code-ide--display-buffer-in-side-window
                    (claude-code-ide-mcp-session-buffer a))))
      (with-selected-window window
        (wamei/claude-panel-next)
        (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer a)))))))

(ert-deftest wamei/claude-panel-binds-cycle-keys-locally ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (buffer (claude-code-ide-mcp-session-buffer a)))
      (claude-code-ide--display-buffer-in-side-window buffer)
      (with-current-buffer buffer
        (should (eq (key-binding (kbd "<C-tab>")) #'wamei/claude-panel-next))
        (should (eq (key-binding (kbd "<C-S-tab>")) #'wamei/claude-panel-previous))
        (should (eq (key-binding (kbd "<C-S-iso-lefttab>"))
                    #'wamei/claude-panel-previous)))
      ;; 他のバッファには漏れない
      (with-temp-buffer
        (should-not (eq (key-binding (kbd "<C-tab>")) #'wamei/claude-panel-next))))))

(ert-deftest wamei/claude-panel-cycle-keys-survive-local-map-replacement ()
  "ghostel がローカルマップを入れ替えても巡回キーが残る。

ghostel は copy mode / Emacs mode の出入りごとに `use-local-map' で
ローカルマップを差し替える。スクロールで copy mode に入っただけで
C-tab がグローバル (端末パネルの巡回) に戻ってしまってはいけない。"
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (buffer (claude-code-ide-mcp-session-buffer a)))
      (claude-code-ide--display-buffer-in-side-window buffer)
      (with-current-buffer buffer
        ;; ghostel--enter-readonly / ghostel-semi-char-mode 相当
        (use-local-map (make-sparse-keymap))
        (should (eq (key-binding (kbd "<C-tab>")) #'wamei/claude-panel-next))
        (should (eq (key-binding (kbd "<C-S-tab>")) #'wamei/claude-panel-previous))
        (should (eq (key-binding (kbd "s-v")) #'wamei/term-input-paste))))))

;;; セッション終了時の差し替え

(ert-deftest wamei/claude-panel-hand-over-shows-sibling-when-displayed-session-dies ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (window nil))
      (dolist (session (list a b))
        (setq window (claude-code-ide--display-buffer-in-side-window
                      (claude-code-ide-mcp-session-buffer session))))
      (select-window (frame-first-window))
      (wamei/claude-panel--hand-over b)
      (should (window-live-p window))
      (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer a)))
      ;; フォーカスは動かさない
      (should (eq (selected-window) (frame-first-window))))))

(ert-deftest wamei/claude-panel-hand-over-does-nothing-without-sibling ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (_other (wamei/claude-panel-test--session "/tmp/other/"))
           (window (claude-code-ide--display-buffer-in-side-window
                    (claude-code-ide-mcp-session-buffer a))))
      (wamei/claude-panel--hand-over a)
      (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer a))))))

(ert-deftest wamei/claude-panel-hand-over-does-nothing-when-session-hidden ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (window nil))
      (dolist (session (list a b))
        (setq window (claude-code-ide--display-buffer-in-side-window
                      (claude-code-ide-mcp-session-buffer session))))
      ;; a は隠れている。a の終了でパネル (b) は動かない
      (wamei/claude-panel--hand-over a)
      (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer b))))))

(ert-deftest wamei/claude-panel-hand-over-ignores-non-panel-windows ()
  "パネル (side window) 以外に出ているセッションの終了では差し替えない。
グリッド (claude-grid.el) の window は side window ではなく、そちらは
セッションの増減で全体を組み直す。ここで別セッションを side window に
出すと、同じ端末バッファが 2 つの window に出て pty のサイズが競合する。"
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (_b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (window (selected-window)))
      (set-window-buffer window (claude-code-ide-mcp-session-buffer a))
      (wamei/claude-panel--hand-over a)
      (should (= 1 (length (window-list nil 'no-mini))))
      (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer a))))))

(ert-deftest wamei/claude-panel-hand-over-runs-before-cleanup-session ()
  (wamei/claude-panel-test--with-env
    (should (advice-member-p #'wamei/claude-panel--before-cleanup
                             #'claude-code-ide--cleanup-session))))

(ert-deftest wamei/claude-panel-cleanup-session-hands-panel-to-sibling ()
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (window nil))
      (dolist (session (list a b))
        (setq window (claude-code-ide--display-buffer-in-side-window
                      (claude-code-ide-mcp-session-buffer session))))
      ;; パッケージの後始末 (バッファ削除を含む) を実際に通す
      (claude-code-ide--cleanup-session b)
      (should-not (buffer-live-p (claude-code-ide-mcp-session-buffer b)))
      (should (window-live-p window))
      (should (eq (window-buffer window) (claude-code-ide-mcp-session-buffer a))))))

(ert-deftest wamei/claude-panel-cleanup-session-redraws-tab-line ()
  "終了したセッションのバッファが消えても window は変わらないので、
tab-line は明示的に描き直しを要求しないと古いタブが残る。
kill-buffer は mode-line の更新フラグを立てない (Emacs の Fkill_buffer)。
redisplay のフラグは Lisp から見えないため、要求の呼び出しを記録して確認する。"
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/"))
           (b (wamei/claude-panel-test--session "/tmp/proj/" "b"))
           (calls nil))
      (dolist (session (list a b))
        (claude-code-ide--display-buffer-in-side-window
         (claude-code-ide-mcp-session-buffer session)))
      ;; a は隠れている。隠れたセッションの終了でも一覧は描き直される
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&optional all) (push all calls))))
        (claude-code-ide--cleanup-session a))
      (should-not (buffer-live-p (claude-code-ide-mcp-session-buffer a)))
      (should (memq t calls)))))

;;; claude-panel-test.el ends here
