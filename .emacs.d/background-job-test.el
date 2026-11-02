;;; background-job-test.el --- Background I/O regression tests -*- lexical-binding: t; -*-
(require 'ert)
(package-initialize)
(load (expand-file-name "background-job.el" (file-name-directory load-file-name)) nil t)

(defun wamei/background-test--wait (predicate)
  (let ((deadline (+ (float-time) 8)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))))

(ert-deftest wamei/background-test-slow-worker-keeps-event-loop-responsive ()
  (let (result heartbeat)
    (unwind-protect
        (progn
          (wamei/background-start 'test '(progn (sleep-for 0.6) 42)
                                  (lambda (value) (setq result value)))
          (run-at-time 0.05 nil (lambda () (setq heartbeat t)))
          (wamei/background-test--wait (lambda () heartbeat))
          (should heartbeat)
          (should-not result)
          (wamei/background-test--wait (lambda () result))
          (should (equal result '(:ok 42))))
      (wamei/background-cancel 'test))))

(ert-deftest wamei/background-test-deduplicates-and-recovers-after-timeout ()
  (let (result)
    (unwind-protect
        (progn
          (wamei/background-start 'test '(sleep-for 30)
                                  (lambda (value) (setq result value)) 0.2)
          (should-not (wamei/background-start 'test '99 #'ignore))
          (wamei/background-test--wait (lambda () result))
          (should (equal result '(:error timeout)))
          (should-not (wamei/background-running-p 'test))
          (setq result nil)
          (wamei/background-start 'test '99 (lambda (value) (setq result value)))
          (wamei/background-test--wait (lambda () result))
          (should (equal result '(:ok 99))))
      (wamei/background-cancel 'test))))

(ert-deftest wamei/background-test-errors-do-not-leak-worker-data ()
  (let (result)
    (wamei/background-start 'test '(error "private token")
                            (lambda (value) (setq result value)))
    (wamei/background-test--wait (lambda () result))
    (should (equal result '(:error worker)))))

(ert-deftest wamei/background-test-cancelled-result-cannot-replace-new-job ()
  (let (result)
    (wamei/background-start 'test '(progn (sleep-for 0.4) 'old)
                            (lambda (value) (setq result value)))
    (wamei/background-cancel 'test)
    (wamei/background-start 'test ''new (lambda (value) (setq result value)))
    (wamei/background-test--wait (lambda () result))
    (should (equal result '(:ok new)))
    (let ((deadline (+ (float-time) 0.6)))
      (while (< (float-time) deadline) (accept-process-output nil 0.02)))
    (should (equal result '(:ok new)))))

(ert-deftest wamei/background-test-cancel-after-exit-drops-unread-output ()
  "Late output must not reach a filter whose buffer the cancel killed.
After sleep the timeout timer can run before the parent has read the
output of a worker that already exited, so the process is no longer live."
  (let (errors proc)
    (cl-letf (((symbol-function 'wamei/background-test--record-filter-error)
               (lambda (orig &rest args)
                 (condition-case err (apply orig args)
                   (error (push err errors))))))
      (advice-add 'async-read-from-client :around
                  'wamei/background-test--record-filter-error)
      (unwind-protect
          (progn
            (setq proc (wamei/background-start 'test '42 #'ignore))
            ;; Block without reading process output until the worker exits.
            (call-process "sleep" nil nil nil "3")
            (should-not (process-live-p proc))
            (wamei/background-cancel 'test)
            (let ((deadline (+ (float-time) 0.5)))
              (while (< (float-time) deadline) (accept-process-output nil 0.02)))
            (should-not errors))
        (advice-remove 'async-read-from-client
                       'wamei/background-test--record-filter-error)
        (wamei/background-cancel 'test)))))
