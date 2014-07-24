(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-files)

(defvar helm-filelist-file-name "/tmp/all.filelist")
(defvar helm-filelist-case-fold-search helm-case-fold-search)
(defvar helm-filelist-grep-command "LANG=C grep")
(defvar helm-filelist-candidate-number-limit 200)
(defvar helm-filelist-async t)

(defun helm-filelist-split-pattern (patterns)
  (delq "" (split-string patterns " ")))

(defun helm-filelist-make-grep-cmd (pattern file ignore-case)
  (format "%s %s %s %s 2>/dev/null"
          helm-filelist-grep-command
          (if ignore-case "-i" "")
          (shell-quote-argument pattern)
          (if file (shell-quote-argument file) "")))

(defun helm-filelist-ignore-case-p (pattern)
  (case helm-filelist-case-fold-search
        (smart (let ((case-fold-search nil))
                 (not (string-match-p "[A-Z]" pattern))))
        (t (if helm-filelist-case-fold-search t nil))))

(defun helm-filelist-make-cmd-line (patterns file &optional limit)
  (with-temp-buffer
    (loop for pattern in (helm-filelist-split-pattern patterns)
          for ignore-case-flag = (helm-filelist-ignore-case-p pattern)
          for i from 0
          do
          (unless (zerop i) (insert " | "))
          (insert (helm-filelist-make-grep-cmd pattern
                                               (if (zerop i) file nil)
                                               ignore-case-flag)))
    (when limit (insert (format " | head -n %d" limit)))
    (buffer-string)))

(defun helm-filelist-init-async ()
  (let ((process-connection-type))
    (prog1
        (start-process-shell-command
         "filelist-grep-process" helm-buffer
         (helm-filelist-make-cmd-line helm-pattern
                                      helm-filelist-file-name
                                      helm-filelist-candidate-number-limit))
      (set-process-sentinel
       (get-process "filelist-grep-process")
       #'(lambda (process event)
           (unless (string= event "finished\n")
             (helm-log "Error: Filelist %s"
                       (replace-regexp-in-string "\n" "" event))))))))

(defun helm-filelist-init ()
  (split-string
   (shell-command-to-string
    (helm-filelist-make-cmd-line helm-pattern
                                 helm-filelist-file-name
                                 helm-filelist-candidate-number-limit))
   "\n"))

(defun helm-source-filelist ()
  `((name . "FileList")
    ,@(if helm-filelist-async
          '((candidates-process . helm-filelist-init-async)
            (delayed))
        '((candidates . helm-filelist-init)
          (volatile)))
    (type . file)
    (requires-pattern . 3)
    (history . ,'helm-file-name-history)
    (keymap . ,helm-generic-files-map)
    (helm-message . helm-generic-file-help-message)
    (candidate-number-limit . ,helm-filelist-candidate-number-limit)
    (mode-line . helm-generic-file-mode-line-string)))

(defun helm-filelist ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer (helm-source-filelist) "*helm filelist*")))

(defun helm-filelist+ ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer  `(helm-source-buffers-list
                          helm-source-recentf
                          helm-source-file-cache
                          helm-source-files-in-current-dir
                          ,(helm-source-filelist))
                        "*helm filelist+*")))

(provide 'helm-filelist)
