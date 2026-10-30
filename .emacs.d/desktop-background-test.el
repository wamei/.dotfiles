;;; desktop-background-test.el --- Nonblocking desktop persistence -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(package-initialize)
(load (expand-file-name "desktop-background.el" (file-name-directory load-file-name)) nil t)
(defvar wamei/desktop-test-value nil)
(load (expand-file-name "term-restore.el" (file-name-directory load-file-name)) nil t)

(defmacro wamei/desktop-test--with-directory (&rest body)
  (declare (indent 0))
  `(let* ((desktop-dirname (file-name-as-directory (make-temp-file "desktop-bg-" t)))
          (desktop-base-file-name ".emacs.desktop")
          (desktop-base-lock-name ".emacs.desktop.lock")
          (desktop-file-modtime nil) (desktop-file-checksum nil)
          (desktop-save-mode t) (desktop-auto-save-timeout 10)
          (desktop-lazy-timer nil) (desktop-save-frames nil)
          (desktop-buffers-not-to-save ".*")
          (desktop-globals-to-save '(wamei/desktop-test-value))
          (wamei/desktop-test-value "round trip")
          (desktop-save-hook nil))
     (unwind-protect (progn (desktop-claim-lock) ,@body)
       (wamei/background-cancel 'desktop-save)
       (delete-directory desktop-dirname t))))

(ert-deftest wamei/desktop-background-test-saves-without-ui-file-write ()
  (wamei/desktop-test--with-directory
    (let ((file-name-handler-alist
           (cons (cons (concat "\\`" (regexp-quote (desktop-full-file-name)) "\\'")
                       (lambda (operation &rest args)
                         (if (memq operation '(write-region insert-file-contents file-attributes))
                             (ert-fail (format "UI performed target I/O: %S" operation))
                           (let ((file-name-handler-alist nil)) (apply operation args)))))
                 file-name-handler-alist)))
      (wamei/desktop-background--auto-save #'desktop-auto-save)
      (should (wamei/background-running-p 'desktop-save))
      (let ((deadline (+ (float-time) 8)))
        (while (and (wamei/background-running-p 'desktop-save) (< (float-time) deadline))
          (accept-process-output nil 0.02))))
    (should desktop-file-modtime)
    (let ((wamei/desktop-test-value nil))
      (load (desktop-full-file-name) nil t)
      (should (equal wamei/desktop-test-value "round trip")))))

(ert-deftest wamei/desktop-background-test-refuses-foreign-lock ()
  (wamei/desktop-test--with-directory
    (write-region "old data" nil (desktop-full-file-name) nil 'silent)
    (write-region "99999999" nil (desktop-full-lock-name) nil 'silent)
    (should-error
     (wamei/desktop-background--write (desktop-full-file-name)
                                    (desktop-full-lock-name) (emacs-pid) nil "new data"))
    (with-temp-buffer
      (insert-file-contents (desktop-full-file-name))
      (should (equal (buffer-string) "old data")))))

(ert-deftest wamei/desktop-background-test-refuses-changed-file ()
  (wamei/desktop-test--with-directory
    (write-region "external update" nil (desktop-full-file-name) nil 'silent)
    (should-error
     (wamei/desktop-background--write (desktop-full-file-name)
                                    (desktop-full-lock-name) (emacs-pid) nil "stale data"))
    (with-temp-buffer
      (insert-file-contents (desktop-full-file-name))
      (should (equal (buffer-string) "external update")))))

(ert-deftest wamei/desktop-background-test-defers-terminal-scrollback-too ()
  (wamei/desktop-test--with-directory
    (let ((wamei/term-restore-directory (expand-file-name "scrollback/" desktop-dirname))
          ;; As in init.el: the record makes each snapshot differ.  Without it
          ;; the second save is skipped as unchanged once the frame is stable.
          (desktop-globals-to-save '(wamei/desktop-test-value wamei/term-restore-saved))
          (desktop-save-hook '(wamei/term-restore-save))
          (terminal (generate-new-buffer "*term: background-test*")))
      (unwind-protect
          (progn
            (with-current-buffer terminal (insert "terminal output\n"))
            (make-directory wamei/term-restore-directory t)
            (write-region "previous output" nil
                          (expand-file-name "background-test-1.txt" wamei/term-restore-directory)
                          nil 'silent)
            (wamei/desktop-background--auto-save #'desktop-auto-save)
            (let ((deadline (+ (float-time) 8)))
              (while (and (wamei/background-running-p 'desktop-save) (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (with-temp-buffer
              (insert-file-contents (plist-get (car wamei/term-restore-saved) :scrollback))
              (should (string-match-p "terminal output" (buffer-string))))
            (let ((previous (plist-get (car wamei/term-restore-saved) :scrollback)))
              (with-current-buffer terminal (goto-char (point-max)) (insert "more output\n"))
              (wamei/desktop-background--auto-save #'desktop-auto-save)
              (let ((deadline (+ (float-time) 8)))
                (while (and (or (wamei/background-running-p 'desktop-save)
                                (wamei/background-running-p (cons 'desktop-prune (desktop-full-file-name))))
                            (< (float-time) deadline))
                  (accept-process-output nil 0.02)))
              (should-not (file-exists-p previous))
              (with-temp-buffer
                (insert-file-contents (plist-get (car wamei/term-restore-saved) :scrollback))
                (should (string-match-p "more output" (buffer-string)))))
            (with-temp-buffer
              (insert-file-contents (expand-file-name "background-test-1.txt" wamei/term-restore-directory))
              (should (equal (buffer-string) "previous output"))))
        (kill-buffer terminal)))))

(ert-deftest wamei/desktop-background-test-manual-save-cancels-pending-snapshot ()
  (wamei/desktop-test--with-directory
    (unwind-protect
        (progn
          (wamei/desktop-background-enable)
          (desktop-auto-save)
          (should (wamei/background-running-p 'desktop-save))
          (setq wamei/desktop-test-value "manual save")
          (desktop-save desktop-dirname)
          (should-not (wamei/background-running-p 'desktop-save))
          (let ((wamei/desktop-test-value nil))
            (load (desktop-full-file-name) nil t)
            (should (equal wamei/desktop-test-value "manual save"))))
      (advice-remove 'desktop-auto-save #'wamei/desktop-background--auto-save)
      (advice-remove 'desktop-save #'wamei/desktop-background--manual-save))))

(defun wamei/desktop-test--commit-without-acknowledgement ()
  "Start an auto-save and let it commit, but drop its result.
This is what happens when the timeout fires first, e.g. after sleep."
  (wamei/desktop-background--auto-save #'desktop-auto-save)
  (let ((process (car (gethash 'desktop-save wamei/background--jobs)))
        (deadline (+ (float-time) 8)))
    (remhash 'desktop-save wamei/background--jobs)
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (file-exists-p (desktop-full-file-name)))))

(ert-deftest wamei/desktop-background-test-adopts-own-unacknowledged-commit ()
  (wamei/desktop-test--with-directory
    (wamei/desktop-test--commit-without-acknowledgement)
    (setq wamei/desktop-test-value "after lost acknowledgement")
    (wamei/desktop-background--auto-save #'desktop-auto-save)
    (let ((deadline (+ (float-time) 8)))
      (while (and (wamei/background-running-p 'desktop-save) (< (float-time) deadline))
        (accept-process-output nil 0.02)))
    (should (time-equal-p desktop-file-modtime
                          (file-attribute-modification-time
                           (file-attributes (desktop-full-file-name)))))
    (let ((wamei/desktop-test-value nil))
      (load (desktop-full-file-name) nil t)
      (should (equal wamei/desktop-test-value "after lost acknowledgement")))))

(ert-deftest wamei/desktop-background-test-manual-save-adopts-unacknowledged-commit ()
  (wamei/desktop-test--with-directory
    (unwind-protect
        (progn
          (wamei/desktop-background-enable)
          (wamei/desktop-test--commit-without-acknowledgement)
          (setq wamei/desktop-test-value "manual after lost acknowledgement")
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (ert-fail "asked about our own desktop write"))))
            (desktop-save desktop-dirname))
          (let ((wamei/desktop-test-value nil))
            (load (desktop-full-file-name) nil t)
            (should (equal wamei/desktop-test-value "manual after lost acknowledgement"))))
      (advice-remove 'desktop-auto-save #'wamei/desktop-background--auto-save)
      (advice-remove 'desktop-save #'wamei/desktop-background--manual-save))))
