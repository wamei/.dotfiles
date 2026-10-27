;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'codex-ghostel)
(ert-deftest codex-mouse-selection-copies-and-resumes ()
  (with-temp-buffer
    (insert "selected text")
    (setq major-mode 'ghostel-mode)
    (setq-local wamei/codex-ghostel-arrow-keys-mode t)
    (setq-local ghostel--input-mode 'semi-char)
    (let ((kill-ring nil)
          (transient-mark-mode t)
          (ghostel-readonly-default-mode 'emacs))
      (cl-letf (((symbol-function 'ghostel--mouse-drag) (lambda (_) nil))
                ((symbol-function 'mouse-set-region)
                 (lambda (_) (goto-char (point-max)) (set-mark (point-min))
                   (setq mark-active t))))
        (ghostel-mouse-drag-or-set-region
         (list 'drag-mouse-1 (list nil 1 '(0 . 0) 0)
               (list nil 14 '(0 . 0) 0)))
        (should (equal (car kill-ring) "selected text"))
        (should (equal wamei/codex-ghostel--copy-notice
                       "copied 13 chars to clipboard"))
        (should (eq ghostel--input-mode 'semi-char))
        (should-not mark-active)))))

(ert-deftest codex-mouse-selection-preserves-manual-readonly-and-other-terminals ()
  (dolist (state '((nil semi-char) (t emacs) (t copy)))
    (with-temp-buffer
      (insert "selected text")
      (setq major-mode 'ghostel-mode)
      (setq-local wamei/codex-ghostel-arrow-keys-mode (car state))
      (setq-local ghostel--input-mode (cadr state))
      (let ((kill-ring nil)
            (transient-mark-mode t)
            (ghostel-readonly-default-mode 'emacs))
        (cl-letf (((symbol-function 'ghostel--mouse-drag) (lambda (_) nil))
                  ((symbol-function 'mouse-set-region)
                   (lambda (_) (goto-char (point-max)) (set-mark (point-min))
                     (setq mark-active t))))
          (ghostel-mouse-drag-or-set-region
           (list 'drag-mouse-1 (list nil 1 '(0 . 0) 0)
                 (list nil 14 '(0 . 0) 0)))
          (should-not kill-ring)
          (should mark-active)
          (should (eq ghostel--input-mode
                      (if (car state) (cadr state) 'emacs))))))))

(ert-deftest codex-mouse-click-resumes-without-copying ()
  (with-temp-buffer
    (insert "text")
    (setq major-mode 'ghostel-mode)
    (setq-local wamei/codex-ghostel-arrow-keys-mode t)
    (setq-local ghostel--input-mode 'semi-char)
    (setq-local ghostel--mouse-press-was-selected t)
    (let ((kill-ring nil)
          (ghostel-readonly-default-mode 'emacs))
      (cl-letf (((symbol-function 'ghostel--mouse-release) (lambda (_) nil))
                ((symbol-function 'mouse-set-point) (lambda (&rest _) nil)))
        (ghostel-mouse-release-or-set-point
         (list 'mouse-1 (list nil 1 '(0 . 0) 0)))
        (should-not kill-ring)
        (should (eq ghostel--input-mode 'semi-char))))))

(ert-deftest codex-copy-notice-expires-and-restarts ()
  (with-temp-buffer
    (wamei/codex-ghostel--show-copy-notice 230)
    (let ((first-timer wamei/codex-ghostel--copy-notice-timer))
      (wamei/codex-ghostel--show-copy-notice 42)
      (should-not (memq first-timer timer-list))
      (should (equal wamei/codex-ghostel--copy-notice
                     "copied 42 chars to clipboard"))
      (sleep-for 2.1)
      (should-not wamei/codex-ghostel--copy-notice)
      (should-not wamei/codex-ghostel--copy-notice-timer))))
