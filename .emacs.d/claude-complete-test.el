;;; claude-complete-test.el --- tests for claude-complete -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "claude-cli.el" dir) nil t)
  (load (expand-file-name "claude-complete.el" dir) nil t))

;;; フィクスチャ

(defun wamei/claude-complete-test--stub (script)
  "SCRIPT を本文とする実行可能なシェルスクリプトを作り、そのパスを返す。"
  (let ((path (make-temp-file "claude-stub-" nil ".sh"
                              (concat "#!/bin/sh\n" script "\n"))))
    (set-file-modes path #o755)
    path))

(defmacro wamei/claude-complete-test--with-stub (script &rest body)
  "SCRIPT のスタブを `wamei/claude-cli-program' にして BODY を評価する。"
  (declare (indent 1))
  `(let ((wamei/claude-cli-program (wamei/claude-complete-test--stub ,script)))
     (unwind-protect (progn ,@body)
       (delete-file wamei/claude-cli-program))))

(defun wamei/claude-complete-test--wait (process)
  "PROCESS が終了し sentinel が走り終わるまで待つ。"
  (while (process-live-p process)
    (accept-process-output process 0.1))
  (accept-process-output nil 0.1))

(defmacro wamei/claude-complete-test--with-buffer (text &rest body)
  "TEXT を入れた一時バッファで BODY を評価する。
TEXT 中の `|' を取り除いてその位置に点を置く。`|' が無ければ末尾。"
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,text)
     (goto-char (point-min))
     (if (search-forward "|" nil t)
         (delete-char -1)
       (goto-char (point-max)))
     ,@body))

;;; 文脈抽出

(ert-deftest wamei/claude-complete-language-strips-ts-mode-and-mode-suffix ()
  (with-temp-buffer
    (setq major-mode 'typescript-ts-mode)
    (should (equal (wamei/claude-complete--language) "typescript"))
    (setq major-mode 'emacs-lisp-mode)
    (should (equal (wamei/claude-complete--language) "emacs-lisp"))
    (setq major-mode 'tsx-ts-mode)
    (should (equal (wamei/claude-complete--language) "tsx"))))

(ert-deftest wamei/claude-complete-path-falls-back-to-buffer-name ()
  (with-temp-buffer
    (rename-buffer "*scratch-x*" t)
    (should (equal (wamei/claude-complete--path) (buffer-name)))))

(ert-deftest wamei/claude-complete-path-uses-file-name-without-project ()
  (let ((dir (make-temp-file "claude-complete-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory (file-name-as-directory dir))
          (setq buffer-file-name (expand-file-name "sub/util.ts" dir))
          (should (equal (wamei/claude-complete--path) "util.ts")))
      (delete-directory dir t))))

(ert-deftest wamei/claude-complete-path-is-relative-to-project-root ()
  (let ((dir (make-temp-file "claude-complete-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (make-directory (expand-file-name "src" dir))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "src/util.ts" dir))
            (setq default-directory (expand-file-name "src/" dir))
            (should (equal (wamei/claude-complete--path) "src/util.ts"))))
      (delete-directory dir t))))

(ert-deftest wamei/claude-complete-context-splits-at-point ()
  (wamei/claude-complete-test--with-buffer "abc|def"
    (let ((context (wamei/claude-complete--context)))
      (should (equal (plist-get context :prefix) "abc"))
      (should (equal (plist-get context :suffix) "def"))
      (should (equal (plist-get context :language) "emacs-lisp"))
      (should (stringp (plist-get context :path))))))

(ert-deftest wamei/claude-complete-context-limits-prefix-and-suffix-length ()
  (wamei/claude-complete-test--with-buffer "0123456789|abcdefghij"
    (let* ((wamei/claude-complete-prefix-chars 4)
           (wamei/claude-complete-suffix-chars 3)
           (context (wamei/claude-complete--context)))
      (should (equal (plist-get context :prefix) "6789"))
      (should (equal (plist-get context :suffix) "abc")))))

;;; プロンプト

(ert-deftest wamei/claude-complete-prompt-wraps-file-and-marks-cursor ()
  (let ((prompt (wamei/claude-complete--prompt
                 '(:prefix "abc" :suffix "def" :path "src/a.ts" :language "typescript")
                 nil)))
    (should (equal prompt
                   "<file path=\"src/a.ts\" language=\"typescript\">\nabc<CURSOR>def\n</file>\n"))))

(ert-deftest wamei/claude-complete-prompt-lists-identifiers-when-given ()
  (let ((prompt (wamei/claude-complete--prompt
                 '(:prefix "" :suffix "" :path "a.ts" :language "typescript")
                 '("clamp" "Math"))))
    (should (string-suffix-p "</file>\n<identifiers>\nclamp, Math\n</identifiers>\n" prompt))))

(ert-deftest wamei/claude-complete-prompt-omits-identifiers-block-when-empty ()
  (let ((prompt (wamei/claude-complete--prompt
                 '(:prefix "" :suffix "" :path "a.ts" :language "typescript")
                 nil)))
    (should-not (string-match-p "<identifiers>" prompt))))

(ert-deftest wamei/claude-complete-system-prompt-forbids-fences-and-mentions-cursor ()
  (should (string-match-p "<CURSOR>" wamei/claude-complete-system-prompt))
  (should (string-match-p "fence" wamei/claude-complete-system-prompt))
  (should (string-match-p "<identifiers>" wamei/claude-complete-system-prompt)))

;;; 出力整形

(defun wamei/claude-complete-test--clean (text prefix suffix)
  (wamei/claude-complete--clean text (list :prefix prefix :suffix suffix
                                           :path "a.ts" :language "typescript")))

(ert-deftest wamei/claude-complete-clean-strips-code-fences ()
  (should (equal (wamei/claude-complete-test--clean "```ts\nreturn 1;\n```" "" "")
                 "return 1;"))
  (should (equal (wamei/claude-complete-test--clean "```\nreturn 1;\nfoo();\n```\n" "" "")
                 "return 1;\nfoo();")))

(ert-deftest wamei/claude-complete-clean-keeps-text-without-fences ()
  (should (equal (wamei/claude-complete-test--clean "return 1;" "" "") "return 1;")))

(ert-deftest wamei/claude-complete-clean-drops-repeated-line-head ()
  ;; 点の手前が「  return 」で、モデルが行全体を返してきた場合
  (should (equal (wamei/claude-complete-test--clean "  return Math.max(a, b);"
                                                    "function f() {\n  return " "\n}")
                 "Math.max(a, b);"))
  ;; インデント無しで行を繰り返した場合
  (should (equal (wamei/claude-complete-test--clean "return Math.max(a, b);"
                                                    "function f() {\n  return " "\n}")
                 "Math.max(a, b);")))

(ert-deftest wamei/claude-complete-clean-keeps-output-when-line-head-is-blank ()
  (should (equal (wamei/claude-complete-test--clean "  return 1;" "function f() {\n  " "\n}")
                 "return 1;"))
  (should (equal (wamei/claude-complete-test--clean "return 1;" "function f() {\n" "\n}")
                 "return 1;")))

(ert-deftest wamei/claude-complete-clean-drops-overlap-with-suffix ()
  ;; foo(<CURSOR>) で「a, b)」が返った場合、末尾の ) は既にある
  (should (equal (wamei/claude-complete-test--clean "a, b)" "foo(" ")") "a, b"))
  ;; 閉じ括弧の行が既にある場合
  (should (equal (wamei/claude-complete-test--clean "return x;\n}" "{\n  " "\n}")
                 "return x;")))

(ert-deftest wamei/claude-complete-clean-keeps-closer-the-completion-opens ()
  ;; foo(<CURSOR>) で「compute(x)」が返った場合、末尾の ) は compute( を閉じている
  (should (equal (wamei/claude-complete-test--clean "compute(x)" "foo(" ")")
                 "compute(x)"))
  ;; 補完が開いた { を閉じる } は残す
  (should (equal (wamei/claude-complete-test--clean "if (a) {\n  b();\n}" "{\n  " "\n}")
                 "if (a) {\n  b();\n}")))

(ert-deftest wamei/claude-complete-clean-drops-closer-already-balanced ()
  ;; 補完内で [ は閉じているので、余分な ] は suffix と重なるぶんとして落とす
  (should (equal (wamei/claude-complete-test--clean "[1, 2]]" "x = [" "]") "[1, 2]")))

(ert-deftest wamei/claude-complete-clean-strips-leading-newlines ()
  (should (equal (wamei/claude-complete-test--clean "\nreturn 1;" "" "") "return 1;"))
  (should (equal (wamei/claude-complete-test--clean "\n\n  return 1;" "" "") "  return 1;")))

(ert-deftest wamei/claude-complete-clean-ignores-suffix-beyond-first-line ()
  (should (equal (wamei/claude-complete-test--clean "return x;" "{\n  " "\n}\nreturn x;")
                 "return x;")))

(ert-deftest wamei/claude-complete-clean-trims-trailing-whitespace ()
  (should (equal (wamei/claude-complete-test--clean "return 1;  \n\n" "" "") "return 1;")))

(ert-deftest wamei/claude-complete-clean-returns-nil-when-empty ()
  (should-not (wamei/claude-complete-test--clean "" "" ""))
  (should-not (wamei/claude-complete-test--clean "   \n" "" ""))
  (should-not (wamei/claude-complete-test--clean "```\n```" "" ""))
  (should-not (wamei/claude-complete-test--clean ")" "foo(" ")")))

;;; 表示・確定・破棄

(ert-deftest wamei/claude-complete-show-places-shadow-overlay-at-point ()
  (wamei/claude-complete-test--with-buffer "foo(|)"
    (wamei/claude-complete--show "a, b")
    (should (wamei/claude-complete--visible-p))
    (let ((overlay wamei/claude-complete--overlay))
      (should (= (overlay-start overlay) (point)))
      (should (= (overlay-end overlay) (point)))
      (should (equal (overlay-get overlay 'after-string) "a, b"))
      (should (eq (get-text-property 0 'face (overlay-get overlay 'after-string)) 'shadow))
      ;; cursor プロパティが無いとカーソルが after-string の末尾に描かれる
      (should (get-text-property 0 'cursor (overlay-get overlay 'after-string)))
      (should (windowp (overlay-get overlay 'window))))
    ;; バッファ本文は変わらない
    (should (equal (buffer-string) "foo()"))))

(ert-deftest wamei/claude-complete-show-replaces-previous-overlay ()
  (wamei/claude-complete-test--with-buffer "x|"
    (wamei/claude-complete--show "one")
    (let ((first wamei/claude-complete--overlay))
      (wamei/claude-complete--show "two")
      (should-not (overlay-buffer first))
      (should (equal (overlay-get wamei/claude-complete--overlay 'after-string) "two")))))

(ert-deftest wamei/claude-complete-accept-inserts-text-and-removes-overlay ()
  (wamei/claude-complete-test--with-buffer "foo(|)"
    (wamei/claude-complete--show "a, b")
    (wamei/claude-complete-accept)
    (should (equal (buffer-string) "foo(a, b)"))
    (should (= (point) 9))
    (should-not (wamei/claude-complete--visible-p))))

(ert-deftest wamei/claude-complete-accept-does-nothing-without-overlay ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-accept)
    (should (equal (buffer-string) "foo"))))

(ert-deftest wamei/claude-complete-dismiss-removes-overlay ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete--show "bar")
    (wamei/claude-complete-dismiss)
    (should-not (wamei/claude-complete--visible-p))
    (should (equal (buffer-string) "foo"))))

(ert-deftest wamei/claude-complete-pre-command-dismisses-unless-accept ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete--show "bar")
    (let ((this-command 'wamei/claude-complete-accept))
      (wamei/claude-complete--pre-command))
    (should (wamei/claude-complete--visible-p))
    (let ((this-command 'self-insert-command))
      (wamei/claude-complete--pre-command))
    (should-not (wamei/claude-complete--visible-p))))

;;; 要求

(ert-deftest wamei/claude-complete-request-shows-cleaned-output ()
  (wamei/claude-complete-test--with-stub "printf '```ts\\nreturn 1;\\n```'"
    (wamei/claude-complete-test--with-buffer "function f() {\n  |\n}"
      (wamei/claude-complete-request)
      (should (process-live-p wamei/claude-complete--process))
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (should (wamei/claude-complete--visible-p))
      (should (equal (overlay-get wamei/claude-complete--overlay 'wamei/claude-complete-text)
                     "return 1;"))
      (should-not wamei/claude-complete--process))))

(ert-deftest wamei/claude-complete-request-sends-prompt-and-system-prompt ()
  ;; stub は引数と stdin をまとめて返す。応答は整形されてそのまま overlay に載る
  (wamei/claude-complete-test--with-stub "printf '%s\\n' \"$@\"; cat"
    (wamei/claude-complete-test--with-buffer "abc|def"
      (wamei/claude-complete-request)
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (let ((shown (overlay-get wamei/claude-complete--overlay 'wamei/claude-complete-text)))
        (should (string-match-p "--model\nhaiku\n" shown))
        (should (string-match-p (regexp-quote wamei/claude-complete-system-prompt) shown))
        (should (string-match-p "abc<CURSOR>def" shown))))))

(ert-deftest wamei/claude-complete-request-discards-stale-response ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-request)
      (let ((process wamei/claude-complete--process))
        ;; 応答を受け取る前にバッファを変える
        (insert "x")
        (wamei/claude-complete-test--wait process))
      (should-not (wamei/claude-complete--visible-p))
      (should (equal (buffer-string) "foox")))))

(ert-deftest wamei/claude-complete-request-discards-response-after-point-moves ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "fo|o"
      (wamei/claude-complete-request)
      (let ((process wamei/claude-complete--process))
        (forward-char 1)
        (wamei/claude-complete-test--wait process))
      (should-not (wamei/claude-complete--visible-p)))))

(ert-deftest wamei/claude-complete-request-does-not-show-empty-output ()
  (wamei/claude-complete-test--with-stub "printf '   '"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-request)
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (should-not (wamei/claude-complete--visible-p)))))

(ert-deftest wamei/claude-complete-request-cancels-previous-process-silently ()
  (wamei/claude-complete-test--with-stub "sleep 5"
    (wamei/claude-complete-test--with-buffer "foo|"
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when fmt (push (apply #'format fmt args) messages)))))
          (wamei/claude-complete-request)
          (let ((first wamei/claude-complete--process))
            (wamei/claude-complete-request)
            (should-not (eq first wamei/claude-complete--process))
            (should-not (process-live-p first))
            (wamei/claude-complete-test--wait first)
            (wamei/claude-complete--cancel-process)
            (accept-process-output nil 0.1)))
        (should-not messages)))))

(ert-deftest wamei/claude-complete-request-is-blocked-while-completion-in-region ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (let ((completion-in-region-mode t))
        (wamei/claude-complete-request))
      (should-not wamei/claude-complete--process)
      (should-not wamei/claude-complete--request))))

(ert-deftest wamei/claude-complete-request-does-not-show-while-completion-in-region ()
  ;; 要求時は許されていても、応答までの間に corfu のポップアップが出たら表示しない
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-request)
      (let ((process wamei/claude-complete--process))
        (let ((completion-in-region-mode t))
          (wamei/claude-complete-test--wait process)))
      (should-not (wamei/claude-complete--visible-p)))))

(ert-deftest wamei/claude-complete-request-is-blocked-in-read-only-buffer ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (setq buffer-read-only t)
      (wamei/claude-complete-request)
      (should-not wamei/claude-complete--process))))

(ert-deftest wamei/claude-complete-command-is-interactive-and-requests ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (should (commandp #'wamei/claude-complete))
      (call-interactively #'wamei/claude-complete)
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (should (equal (overlay-get wamei/claude-complete--overlay 'wamei/claude-complete-text)
                     "bar")))))

;;; idle 自動トリガー

(ert-deftest wamei/claude-complete-post-command-arms-idle-timer ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (let ((wamei/claude-complete-idle-delay 100))
      (unwind-protect
          (progn
            (wamei/claude-complete--post-command)
            (should (timerp wamei/claude-complete--timer))
            (should (eq (timer--function wamei/claude-complete--timer)
                        #'wamei/claude-complete--on-idle))
            (should (equal (timer--args wamei/claude-complete--timer)
                           (list (current-buffer))))
            (should (memq wamei/claude-complete--timer timer-idle-list)))
        (wamei/claude-complete--cancel-timer)))))

(ert-deftest wamei/claude-complete-post-command-replaces-existing-timer ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (let ((wamei/claude-complete-idle-delay 100))
      (unwind-protect
          (progn
            (wamei/claude-complete--post-command)
            (let ((first wamei/claude-complete--timer))
              (wamei/claude-complete--post-command)
              (should-not (eq first wamei/claude-complete--timer))
              (should-not (memq first timer-idle-list))))
        (wamei/claude-complete--cancel-timer)))))

(ert-deftest wamei/claude-complete-post-command-does-not-arm-timer-when-auto-off ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (let ((wamei/claude-complete-auto nil))
      (wamei/claude-complete--post-command)
      (should-not wamei/claude-complete--timer))))

(ert-deftest wamei/claude-complete-post-command-cancels-process-after-edit ()
  (wamei/claude-complete-test--with-stub "sleep 5"
    (wamei/claude-complete-test--with-buffer "foo|"
      (let ((wamei/claude-complete-auto nil))
        (wamei/claude-complete-request)
        (let ((process wamei/claude-complete--process))
          (insert "x")
          (wamei/claude-complete--post-command)
          (should-not (process-live-p process))
          (should-not wamei/claude-complete--process)
          (should-not wamei/claude-complete--request)
          (wamei/claude-complete-test--wait process))))))

(ert-deftest wamei/claude-complete-post-command-keeps-process-when-unchanged ()
  (wamei/claude-complete-test--with-stub "sleep 5"
    (wamei/claude-complete-test--with-buffer "foo|"
      (let ((wamei/claude-complete-auto nil))
        (wamei/claude-complete-request)
        (let ((process wamei/claude-complete--process))
          (wamei/claude-complete--post-command)
          (should (process-live-p process))
          (should (eq process wamei/claude-complete--process))
          (wamei/claude-complete--cancel-process)
          (wamei/claude-complete-test--wait process))))))

(ert-deftest wamei/claude-complete-on-idle-requests-in-current-buffer ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-mode 1)
      (let ((wamei/claude-complete-auto nil))
        (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
          (wamei/claude-complete--on-idle (current-buffer)))
        (should wamei/claude-complete--process)
        (wamei/claude-complete-test--wait wamei/claude-complete--process)
        (should (wamei/claude-complete--visible-p))))))

(ert-deftest wamei/claude-complete-on-idle-skips-when-request-stamp-unchanged ()
  ;; 提案を消しただけで tick も点も動いていない (C-g など) 場合、同じ内容を再要求しない
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-mode 1)
      (let ((wamei/claude-complete-auto nil))
        (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
          (setq wamei/claude-complete--request (wamei/claude-complete--stamp))
          (wamei/claude-complete--on-idle (current-buffer))
          (should-not wamei/claude-complete--process))))))

(ert-deftest wamei/claude-complete-on-idle-skips-when-buffer-not-selected ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-mode 1)
      (let ((other (generate-new-buffer " *other*")))
        (unwind-protect
            (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) other)))
              (wamei/claude-complete--on-idle (current-buffer))
              (should-not wamei/claude-complete--process))
          (kill-buffer other))))))

(ert-deftest wamei/claude-complete-on-idle-skips-while-visible-or-running ()
  (wamei/claude-complete-test--with-stub "sleep 5"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-mode 1)
      (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
        ;; 表示中は要求しない
        (wamei/claude-complete--show "bar")
        (wamei/claude-complete--on-idle (current-buffer))
        (should-not wamei/claude-complete--process)
        (wamei/claude-complete--delete-overlay)
        ;; 走行中は要求しない
        (wamei/claude-complete-request)
        (let ((process wamei/claude-complete--process))
          (wamei/claude-complete--on-idle (current-buffer))
          (should (eq process wamei/claude-complete--process))
          (wamei/claude-complete--cancel-process)
          (wamei/claude-complete-test--wait process))))))

(ert-deftest wamei/claude-complete-on-idle-skips-when-mode-off ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
        (wamei/claude-complete--on-idle (current-buffer))
        (should-not wamei/claude-complete--process)))))

;;; eglot 文脈

(require 'eglot)

(ert-deftest wamei/claude-complete-completion-labels-reads-item-array ()
  (should (equal (wamei/claude-complete--completion-labels
                  [(:label "clamp" :kind 3) (:label "Math" :kind 6)])
                 '("clamp" "Math"))))

(ert-deftest wamei/claude-complete-completion-labels-reads-completion-list ()
  (should (equal (wamei/claude-complete--completion-labels
                  '(:isIncomplete :json-false :items [(:label "a") (:label "b")]))
                 '("a" "b"))))

(ert-deftest wamei/claude-complete-completion-labels-dedupes-and-limits ()
  (let ((wamei/claude-complete-max-identifiers 2))
    (should (equal (wamei/claude-complete--completion-labels
                    [(:label "a") (:label "a") (:label "b") (:label "c")])
                   '("a" "b")))))

(ert-deftest wamei/claude-complete-completion-labels-handles-nil-and-missing-label ()
  (should-not (wamei/claude-complete--completion-labels nil))
  (should (equal (wamei/claude-complete--completion-labels [(:kind 3) (:label "x")])
                 '("x"))))

(defvar wamei/claude-complete-test--eglot-calls nil
  "fake eglot が呼ばれた順序 (新しいものが先頭)。")

(defmacro wamei/claude-complete-test--with-fake-eglot (respond &rest body)
  "eglot が動いているように見せ、`jsonrpc-async-request' を RESPOND で置き換えて BODY を評価する。
RESPOND は (lambda (server method params &rest keys)) で、keys から :success-fn 等を取り出して呼ぶ。
`eglot--signal-textDocument/didChange' は `wamei/claude-complete-test--eglot-calls' に
記録するだけのスタブに差し替える。"
  (declare (indent 1))
  `(let ((wamei/claude-complete-test--eglot-calls nil))
     (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
               ((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
               ((symbol-function 'eglot-current-server) (lambda () 'fake-server))
               ((symbol-function 'eglot--TextDocumentPositionParams) (lambda () '(:fake t)))
               ((symbol-function 'eglot--signal-textDocument/didChange)
                (lambda (&rest _) (push 'didChange wamei/claude-complete-test--eglot-calls)))
               ((symbol-function 'jsonrpc-async-request) ,respond))
       ,@body)))

(ert-deftest wamei/claude-complete-eglot-identifiers-returns-nil-when-not-managed ()
  (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil)))
    (let ((calls nil))
      (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls)))
      (should (equal calls '(nil))))))

(ert-deftest wamei/claude-complete-eglot-identifiers-returns-labels-on-success ()
  (let ((calls nil) (seen nil))
    (wamei/claude-complete-test--with-fake-eglot
        (lambda (server method params &rest keys)
          (push 'request wamei/claude-complete-test--eglot-calls)
          (setq seen (list server method params (plist-get keys :timeout)))
          (funcall (plist-get keys :success-fn) [(:label "clamp") (:label "Math")])
          '(1))
      (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls)))
      ;; 問い合わせの前に保留中の変更を eglot に流しておく
      (should (equal (reverse wamei/claude-complete-test--eglot-calls)
                     '(didChange request))))
    (should (equal calls '(("clamp" "Math"))))
    (should (equal seen (list 'fake-server :textDocument/completion '(:fake t)
                              wamei/claude-complete-eglot-timeout)))))

(ert-deftest wamei/claude-complete-eglot-identifiers-returns-nil-on-error-or-timeout ()
  (dolist (key '(:error-fn :timeout-fn))
    (let ((calls nil))
      (wamei/claude-complete-test--with-fake-eglot
          (lambda (_server _method _params &rest keys)
            (funcall (plist-get keys key) (list :code -1 :message "boom"))
            '(1))
        (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls))))
      (should (equal calls '(nil))))))

(ert-deftest wamei/claude-complete-eglot-identifiers-calls-back-once-even-if-both-fire ()
  (let ((calls nil))
    (wamei/claude-complete-test--with-fake-eglot
        (lambda (_server _method _params &rest keys)
          (funcall (plist-get keys :timeout-fn))
          (funcall (plist-get keys :success-fn) [(:label "late")])
          '(1))
      (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls))))
    (should (equal calls '(nil)))))

(ert-deftest wamei/claude-complete-eglot-identifiers-returns-nil-when-request-signals ()
  (let ((calls nil))
    (wamei/claude-complete-test--with-fake-eglot
        (lambda (&rest _) (signal 'error '("no server")))
      (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls))))
    (should (equal calls '(nil)))))

(ert-deftest wamei/claude-complete-request-puts-eglot-labels-into-prompt ()
  (wamei/claude-complete-test--with-stub "cat"
    (wamei/claude-complete-test--with-buffer "abc|"
      (wamei/claude-complete-test--with-fake-eglot
          (lambda (_server _method _params &rest keys)
            (funcall (plist-get keys :success-fn) [(:label "clamp") (:label "Math")])
            '(1))
        (wamei/claude-complete-request))
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (should (string-match-p "<identifiers>\nclamp, Math\n</identifiers>"
                              (overlay-get wamei/claude-complete--overlay
                                           'wamei/claude-complete-text))))))

;;; minor mode

(ert-deftest wamei/claude-complete-mode-binds-tab-only-while-visible ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-mode 1)
    (should-not (eq (key-binding (kbd "TAB")) 'wamei/claude-complete-accept))
    (wamei/claude-complete--show "bar")
    (should (eq (key-binding (kbd "TAB")) 'wamei/claude-complete-accept))
    (should (eq (key-binding (kbd "<tab>")) 'wamei/claude-complete-accept))
    (wamei/claude-complete--delete-overlay)
    (should-not (eq (key-binding (kbd "TAB")) 'wamei/claude-complete-accept))))

(ert-deftest wamei/claude-complete-mode-binds-manual-trigger ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-mode 1)
    (should (eq (key-binding (kbd "C-c C-.")) 'wamei/claude-complete))))

(ert-deftest wamei/claude-complete-mode-installs-and-removes-hooks ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-mode 1)
    (should (memq #'wamei/claude-complete--pre-command pre-command-hook))
    (should (memq #'wamei/claude-complete--post-command post-command-hook))
    (should (memq #'wamei/claude-complete--teardown kill-buffer-hook))
    (wamei/claude-complete-mode -1)
    (should-not (memq #'wamei/claude-complete--pre-command pre-command-hook))
    (should-not (memq #'wamei/claude-complete--post-command post-command-hook))
    (should-not (memq #'wamei/claude-complete--teardown kill-buffer-hook))))

(ert-deftest wamei/claude-complete-mode-off-clears-overlay-timer-and-request ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-mode 1)
    (wamei/claude-complete--show "bar")
    (setq wamei/claude-complete--timer (run-with-idle-timer 100 nil #'ignore))
    (setq wamei/claude-complete--request (cons 1 1))
    (wamei/claude-complete-mode -1)
    (should-not (wamei/claude-complete--visible-p))
    (should-not wamei/claude-complete--timer)
    (should-not wamei/claude-complete--request)))

(provide 'claude-complete-test)
;;; claude-complete-test.el ends here
