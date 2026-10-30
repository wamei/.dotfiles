;;; ai-panel-test.el --- Mixed AI panel tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'claude-code-ide)
(require 'codex)
(require 'codex-panel)
(require 'ai-panel)
(require 'ai-session)
(require 'desktop-side-windows)

(defvar wamei/ai-panel-test--buffers nil)

(defun wamei/ai-panel-test--claude (directory &optional instance)
  (let* ((buffer (generate-new-buffer (format "*claude-code[%s]*" (or instance "test"))))
         (session (make-claude-code-ide-mcp-session
                   :session-id (buffer-name buffer) :buffer buffer
                   :project-dir directory :instance-name instance :window-slot 0)))
    (push buffer wamei/ai-panel-test--buffers)
    (puthash (buffer-name buffer) session claude-code-ide-mcp--sessions)
    (with-current-buffer buffer (setq-local claude-code-ide--session session))
    buffer))

(defun wamei/ai-panel-test--codex (directory &optional instance)
  (let ((buffer (generate-new-buffer (format "*codex:%s:%s*" directory (or instance "test")))))
    (push buffer wamei/ai-panel-test--buffers)
    (with-current-buffer buffer
      (setq-local codex--buffer-directory directory codex--buffer-instance-name instance))
    ;; Codex の live 判定にはバッファに紐づくプロセスが必要。
    (make-pipe-process :name "ai-panel-test" :buffer buffer :noquery t)
    buffer))

(defmacro wamei/ai-panel-test--with-env (&rest body)
  (declare (indent 0))
  `(save-window-excursion
     (delete-other-windows)
     (let ((claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
           (claude-code-ide-focus-on-open nil)
           (claude-code-ide-window-width 30)
           (wamei/codex-panel-width 30)
           (wamei/ai-panel-enabled nil)
           (wamei/ai-panel--order nil)
           (wamei/ai-panel--previous-window nil)
           (wamei/ai-panel--previous-buffer nil)
           (wamei/ai-panel--grid-timer nil)
           (wamei/ai-panel-test--buffers nil)
           (wamei/claude-panel--order nil)
           (wamei/codex-panel--order nil))
       (unwind-protect
           (progn (wamei/ai-panel-enable) ,@body)
         (advice-remove 'wamei/claude-panel--setup #'wamei/ai-panel--setup)
         (advice-remove 'wamei/codex-panel--setup #'wamei/ai-panel--setup)
         (advice-remove 'claude-code-ide--display-buffer-in-side-window #'wamei/ai-panel--display-claude)
         (advice-remove 'wamei/codex-panel-show #'wamei/ai-panel--display-codex)
         (advice-remove 'wamei/claude-panel--hand-over #'wamei/ai-panel--claude-hand-over)
         (advice-remove 'wamei/desktop-side--spec #'wamei/ai-panel--save-spec)
         (remove-hook 'ghostel-exit-functions #'wamei/ai-panel--exited)
         (dolist (buffer wamei/ai-panel-test--buffers)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let ((kill-buffer-hook nil))
                 (when-let* ((process (get-buffer-process buffer))) (delete-process process))
                 (kill-buffer buffer)))))
         (when (timerp wamei/ai-panel--grid-timer) (cancel-timer wamei/ai-panel--grid-timer))))))

(defmacro wamei/ai-panel-test--with-code-env (&rest body)
  "本体の window が 2 つ (選択中の上と、その下) ある状態で BODY を評価する。
本体が 1 つだと AI はその window を使うので、分割の挙動はこちらで見る。"
  (declare (indent 0))
  `(wamei/ai-panel-test--with-env
     (split-window nil nil 'below)
     ,@body))

(ert-deftest wamei/ai-panel-reuses-one-window-in-both-directions ()
  (wamei/ai-panel-test--with-code-env
    (let* ((claude (wamei/ai-panel-test--claude "/tmp/"))
           (codex (wamei/ai-panel-test--codex "/tmp/"))
           (window (wamei/ai-panel-show claude))
           (width (window-total-width window)))
      (should (eq window (wamei/ai-panel-show codex)))
      (should (= (window-total-width window) width))
      (should (eq window (wamei/ai-panel-show claude)))
      (should (= (length (window-list nil 'no-mini)) 3))
      (should (= (window-total-width window) width))
      (should (buffer-local-value 'wamei/ai-panel-keys-mode claude))
      (should (buffer-local-value 'wamei/ai-panel-keys-mode codex))
      (should-not (buffer-local-value 'wamei/claude-panel-keys-mode claude))
      (should-not (buffer-local-value 'wamei/codex-panel-keys-mode codex)))))

(ert-deftest wamei/ai-panel-codex-first-and-mixed-tabs-filter-project ()
  (wamei/ai-panel-test--with-code-env
    (let* ((codex (wamei/ai-panel-test--codex "/tmp/" "Codex task"))
           (claude (wamei/ai-panel-test--claude "/tmp/" "Claude task"))
           (_other (wamei/ai-panel-test--claude "/other/" "Other"))
           (window (wamei/ai-panel-show codex)))
      (wamei/ai-panel-show claude)
      (should (eq window (wamei/ai-panel--window t)))
      (should (= (length (window-list nil 'no-mini)) 3))
      (with-current-buffer claude
        (let ((tabs (wamei/ai-panel--tabs)))
          (should (= (length tabs) 2))
          (should (string-prefix-p "Codex:" (plist-get (car tabs) :label)))
          (should (string-prefix-p "Claude:" (plist-get (cadr tabs) :label)))
          (should (plist-get (cadr tabs) :current)))))))

(ert-deftest wamei/ai-panel-cycles-across-backends-from-editor-and-panel ()
  (wamei/ai-panel-test--with-env
    (let* ((editor (selected-window))
           (claude (wamei/ai-panel-test--claude "/tmp/"))
           (codex (wamei/ai-panel-test--codex "/tmp/"))
           (panel (wamei/ai-panel-show claude)))
      (wamei/ai-panel-show codex)
      (wamei/ai-next)
      (should (eq (window-buffer panel) claude))
      (should (eq (selected-window) editor))
      (select-window panel)
      (should (eq (key-binding (kbd "<C-tab>")) #'wamei/ai-panel-next))
      (wamei/ai-previous)
      (should (eq (window-buffer panel) codex))
      (should (eq (selected-window) panel)))))

(ert-deftest wamei/ai-panel-toggle-keeps-origin-across-backend-switch ()
  (wamei/ai-panel-test--with-code-env
    (let* ((origin (selected-window))
           (claude (wamei/ai-panel-test--claude "/tmp/"))
           (codex (wamei/ai-panel-test--codex "/tmp/")))
      (wamei/ai-panel-show claude)
      (wamei/ai-toggle)
      (let ((panel (selected-window)))
        (wamei/ai-panel-show codex)
        (wamei/ai-toggle)
        (should (eq (selected-window) origin))
        (wamei/ai-toggle)
        (should (eq (selected-window) panel))
        (wamei/ai-toggle '(4))
        (should-not (window-live-p panel))
        (should (buffer-live-p codex))
        (should (buffer-live-p claude))))))

(ert-deftest wamei/ai-panel-hand-over-to-other-backend-on-exit ()
  (wamei/ai-panel-test--with-env
    (let* ((claude (wamei/ai-panel-test--claude "/tmp/"))
           (codex (wamei/ai-panel-test--codex "/tmp/"))
           (window (wamei/ai-panel-show claude)))
      (wamei/ai-panel-show codex)
      (delete-process (get-buffer-process codex))
      (wamei/ai-panel--exited codex "closed")
      (should (eq (window-buffer window) claude))
      ;; Claude が終了するときは MCP cleanup の前に引き継ぐ。
      (make-pipe-process :name "ai-panel-test" :buffer codex :noquery t)
      (wamei/claude-panel--hand-over (claude-code-ide--buffer-session claude))
      (should (eq (window-buffer window) codex)))))

(ert-deftest wamei/ai-panel-restores-only-saved-visible-session ()
  (wamei/ai-panel-test--with-env
    (let* ((claude (wamei/ai-panel-test--claude "/tmp/"))
           (codex (wamei/ai-panel-test--codex "/tmp/" "saved"))
           (window (wamei/ai-panel-show claude))
           (wamei/ai-session--names (make-hash-table :test 'equal)))
      (wamei/ai-panel-show codex)
      (puthash (buffer-name codex) codex wamei/ai-session--names)
      (let ((spec (wamei/desktop-side--spec (cdr (window-state-get window t)))))
        (wamei/ai-panel-show claude)
        (cl-letf (((symbol-function 'wamei/ai-session-restore-all) #'ignore))
          (wamei/ai-panel-restore spec)
          (should (eq (window-buffer window) codex))
          (should (buffer-live-p claude)))))))

(ert-deftest wamei/ai-panel-grid-combines-both-and-keeps-usage ()
  (wamei/ai-panel-test--with-env
    (let* ((claude (wamei/ai-panel-test--claude "/tmp/"))
           (codex (wamei/ai-panel-test--codex "/tmp/"))
           (wamei/claude-grid-min-window-width 20)
           (wamei/claude-grid-min-window-height 4)
           name)
      (cl-letf (((symbol-function 'wamei/ai-panel--directory) (lambda (&optional _) "/tmp/"))
                ((symbol-function 'wamei/claude-grid--select-tab) (lambda (tab) (setq name tab)))
                ((symbol-function 'wamei/claude-usage--setup)
                 (lambda (buffer) (with-current-buffer buffer (setq-local mode-line-format '("Claude usage")))))
                ((symbol-function 'wamei/codex-usage--setup)
                 (lambda (buffer) (with-current-buffer buffer (setq-local mode-line-format '("Codex usage"))))))
        (wamei/ai-grid-tab)
        (should (equal name "AI - tmp"))
        (should (get-buffer-window claude))
        (should (get-buffer-window codex))
        (should (equal (buffer-local-value 'mode-line-format claude) '("Claude usage")))
        (should (equal (buffer-local-value 'mode-line-format codex) '("Codex usage")))
        (wamei/ai-grid-tab '(4))
        (should (equal name "AI"))))))

(provide 'ai-panel-test)

;;; 本体の左端の window

(defun wamei/ai-panel-test--main-windows ()
  "side window を除いた window を左から並べる。"
  (sort (seq-remove (lambda (window) (window-parameter window 'window-side))
                    (window-list nil 'no-mini))
        (lambda (a b) (< (window-left-column a) (window-left-column b)))))

(ert-deftest wamei/ai-panel-opens-at-left-of-main-area-for-both-backends ()
  "AI は side window ではなく本体の左端に、本体の幅の `wamei/ai-panel-width' で出る。"
  (dolist (backend '(claude codex))
    (wamei/ai-panel-test--with-code-env
      (let* ((wamei/ai-panel-width 0.5)
             (editor (selected-window))
             (sidebar (display-buffer-in-side-window
                       (get-buffer-create "*ai-panel-test-sidebar*")
                       '((side . left) (window-width . 10))))
             (term (display-buffer-in-side-window
                    (get-buffer-create "*ai-panel-test-term*")
                    '((side . right) (window-width . 20))))
             (main-width (window-total-width (window-main-window)))
             (buffer (if (eq backend 'claude)
                         (wamei/ai-panel-test--claude "/tmp/")
                       (wamei/ai-panel-test--codex "/tmp/"))))
        ;; 本文の余白があっても、パネル全体の幅は同じ。
        (with-current-buffer buffer (setq-local left-margin-width 2))
        (let ((window (wamei/ai-panel-show buffer)))
          (should-not (window-parameter window 'window-side))
          (should (eq (car (wamei/ai-panel-test--main-windows)) window))
          (should (memq editor (wamei/ai-panel-test--main-windows)))
          (should (= (window-total-height window) (window-total-height (window-main-window))))
          (should (= (window-left-column window) (window-total-width sidebar)))
          (should (window-live-p term))
          (should (= (window-total-width window) (round (* main-width 0.5))))
          (should (eq window (wamei/ai-panel--window t))))))))

(ert-deftest wamei/ai-panel-closes-on-delete-other-windows ()
  "コードの window で C-x 1 すると AI の window も消える (普通の window と同じ)。
AI 側で C-x 1 すれば AI だけが残る。"
  (wamei/ai-panel-test--with-code-env
    (let* ((editor (selected-window))
           (claude (wamei/ai-panel-test--claude "/tmp/"))
           (window (wamei/ai-panel-show claude)))
      (should-not (window-parameter window 'no-delete-other-windows))
      (delete-other-windows editor)
      (should-not (window-live-p window))
      (should (equal (window-list nil 'no-mini) (list editor)))
      (let ((window (wamei/ai-panel-show claude)))
        (should-not (window-parameter window 'no-delete-other-windows))
        (delete-other-windows window)
        (should (equal (window-list nil 'no-mini) (list window)))))))

(ert-deftest wamei/ai-panel-desktop-strips-and-reopens-main-window ()
  "desktop には AI の window を残さず、本体の左端の spec として開き直す。"
  (wamei/ai-panel-test--with-code-env
    (let* ((claude (wamei/ai-panel-test--claude "/tmp/"))
           (window (wamei/ai-panel-show claude))
           (wamei/ai-session--names (make-hash-table :test 'equal)))
      (puthash (buffer-name claude) claude wamei/ai-session--names)
      (window-resize window 3 t)
      (let* ((width (window-total-width window))
             (stripped (wamei/desktop-side-strip-state
                        (window-state-get (frame-root-window) t)))
             (spec (car (cdr stripped))))
        (should (= (length (cdr stripped)) 1))
        (should (eq (plist-get spec :side) 'main))
        (should (equal (plist-get spec :buffer) (buffer-name claude)))
        (should (= (plist-get spec :size) width))
        (should-not (string-match-p "claude-code" (format "%S" (car stripped))))
        (delete-window window)
        (cl-letf (((symbol-function 'wamei/ai-session-restore-all) #'ignore))
          (let ((restored (wamei/ai-panel-restore spec)))
            (should (eq (window-buffer restored) claude))
            (should (= (window-total-width restored) width))))))))

(ert-deftest wamei/ai-panel-grid-replaces-main-window ()
  "グリッドを組むときは本体の AI window も畳む。"
  (wamei/ai-panel-test--with-code-env
    (let* ((claude (wamei/ai-panel-test--claude "/tmp/"))
           (codex (wamei/ai-panel-test--codex "/tmp/"))
           (wamei/claude-grid-min-window-width 20)
           (wamei/claude-grid-min-window-height 4))
      (wamei/ai-panel-show claude)
      (cl-letf (((symbol-function 'wamei/ai-panel--directory) (lambda (&optional _) "/tmp/"))
                ((symbol-function 'wamei/claude-grid--select-tab) #'ignore))
        (wamei/ai-grid-tab)
        (should (= (length (window-list nil 'no-mini)) 2))
        (should-not (seq-some (lambda (window) (window-parameter window 'wamei/ai-panel))
                              (window-list nil 'no-mini)))
        (should (get-buffer-window codex))))))

(ert-deftest wamei/ai-panel-takes-whole-main-area-when-single-window ()
  "本体の window が 1 つなら分割せず、その window に AI を出す。"
  (dolist (backend '(claude codex))
    (wamei/ai-panel-test--with-env
      (let* ((editor (selected-window))
             (code (window-buffer editor))
             (term (display-buffer-in-side-window
                    (get-buffer-create "*ai-panel-test-term*")
                    '((side . right) (window-width . 20))))
             (buffer (if (eq backend 'claude)
                         (wamei/ai-panel-test--claude "/tmp/")
                       (wamei/ai-panel-test--codex "/tmp/")))
             (window (wamei/ai-panel-show buffer)))
        (should (eq window editor))
        (should (eq (window-buffer window) buffer))
        (should (equal (wamei/ai-panel-test--main-windows) (list window)))
        (should (window-live-p term))
        (should (eq window (wamei/ai-panel--window t)))
        ;; 閉じると元のバッファに戻り、普通の window に戻る。
        (wamei/ai-toggle '(4))
        (should (window-live-p editor))
        (should (eq (window-buffer editor) code))
        (should-not (window-parameter editor 'wamei/ai-panel))
        (should-not (window-parameter editor 'no-delete-other-windows))
        (should-not (window-dedicated-p editor))))))

(ert-deftest wamei/ai-panel-single-window-releases-when-last-session-ends ()
  "本体全体を使っていた AI が最後の 1 つなら、終了時に元のバッファへ戻す。"
  (wamei/ai-panel-test--with-env
    (let* ((editor (selected-window))
           (code (window-buffer editor))
           (claude (wamei/ai-panel-test--claude "/tmp/")))
      (wamei/ai-panel-show claude)
      (wamei/ai-panel--hand-over claude)
      (should (eq (window-buffer editor) code))
      (should-not (window-parameter editor 'wamei/ai-panel))
      (should-not (window-dedicated-p editor)))))

(ert-deftest wamei/ai-panel-links-open-beside-without-more-splits ()
  "AI パネルから開くファイル (find-file-other-window) は右の window で入れ替わる。
本体いっぱいの AI からの 1 つ目だけ左右に分け、2 つ目以降は分けない。"
  (wamei/ai-panel-test--with-env
    (let* ((claude (wamei/ai-panel-test--claude "/tmp/"))
           (panel (wamei/ai-panel-show claude))
           (one (get-buffer-create "*ai-panel-test-one*"))
           (two (get-buffer-create "*ai-panel-test-two*"))
           (split-height-threshold 10))
      (unwind-protect
          (progn
            (select-window panel)
            (switch-to-buffer-other-window one)
            (should (= (length (window-list nil 'no-mini)) 2))
            (let ((code (selected-window)))
              (should-not (eq code panel))
              (should (> (window-left-column code) (window-left-column panel)))
              (select-window panel)
              (switch-to-buffer-other-window two)
              (should (eq (selected-window) code))
              (should (eq (window-buffer code) two))
              (should (= (length (window-list nil 'no-mini)) 2))
              (should (eq (window-buffer panel) claude))))
        (kill-buffer one)
        (kill-buffer two)))))

(ert-deftest wamei/ai-panel-switch-to-buffer-replaces-in-place ()
  "AI パネルは dedicated ではなく、switch-to-buffer (C-x b / consult のプレビュー) は
普通の window と同じくその場で替わる。window は増えない。"
  (wamei/ai-panel-test--with-code-env
    (let* ((claude (wamei/ai-panel-test--claude "/tmp/"))
           (panel (wamei/ai-panel-show claude))
           (count (length (window-list nil 'no-mini)))
           (other (get-buffer-create "*ai-panel-test-other*"))
           (split-height-threshold 10))
      (unwind-protect
          (progn
            (should-not (window-dedicated-p panel))
            (select-window panel)
            (switch-to-buffer other)
            (should (= (length (window-list nil 'no-mini)) count))
            (should (eq (window-buffer panel) other))
            ;; C-x C-a で同じ window に AI が戻る
            (should (eq (wamei/ai-panel-show claude) panel)))
        (kill-buffer other)))))

(ert-deftest wamei/ai-panel-closes-split-window-when-last-session-ends ()
  "最後の AI が終わったら、切り出したパネルの window は閉じる。"
  (wamei/ai-panel-test--with-code-env
    (let* ((claude (wamei/ai-panel-test--claude "/tmp/"))
           (panel (wamei/ai-panel-show claude)))
      (wamei/ai-panel--hand-over claude)
      (should-not (window-live-p panel)))))
