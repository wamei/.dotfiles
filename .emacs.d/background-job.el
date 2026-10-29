;;; background-job.el --- Bounded subprocess jobs for UI I/O -*- lexical-binding: t; -*-
;;; Code:
(require 'async)
(require 'cl-lib)

(defvar wamei/background--jobs (make-hash-table :test 'equal))

(defun wamei/background-running-p (key)
  "Return non-nil if KEY has an unfinished job."
  (gethash key wamei/background--jobs))

(defun wamei/background-cancel (key)
  "Cancel KEY without delivering its stale result."
  (when-let* ((job (gethash key wamei/background--jobs)))
    (remhash key wamei/background--jobs)
    (when (timerp (cdr job)) (cancel-timer (cdr job)))
    (when (processp (car job))
      (set-process-sentinel (car job) #'ignore)
      (when (process-live-p (car job)) (delete-process (car job)))
      (when (buffer-live-p (process-buffer (car job)))
        (kill-buffer (process-buffer (car job)))))))

(defun wamei/background-start (key form callback &optional timeout)
  "Evaluate FORM outside the UI process, at most once per KEY.
CALLBACK receives (:ok VALUE) or (:error REASON).  No worker error
text is returned, since external errors can contain credentials.
TIMEOUT defaults to 45 seconds.  Forms are transmitted over a pipe."
  (unless (wamei/background-running-p key)
    (let* ((job (cons nil nil))
           (async-send-over-pipe t)
           (async-process-noquery-on-exit t))
      (puthash key job wamei/background--jobs)
      (condition-case nil
          (progn
            (setcar job
                    (async-start
                     `(lambda ()
                        (condition-case nil (list :ok ,form)
                          (error '(:error worker))))
                     (lambda (result)
                       (when (eq job (gethash key wamei/background--jobs))
                         (remhash key wamei/background--jobs)
                         (when (timerp (cdr job)) (cancel-timer (cdr job)))
                         (funcall callback result)))))
            (setcdr job
                    (run-at-time
                     (or timeout 45) nil
                     (lambda ()
                       (when (eq job (gethash key wamei/background--jobs))
                         (wamei/background-cancel key)
                         (funcall callback '(:error timeout))))))
            (car job))
        (error
         (wamei/background-cancel key)
         (funcall callback '(:error start))
         nil)))))

(provide 'wamei-background)
;;; background-job.el ends here
