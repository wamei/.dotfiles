(require 'cl-lib)

(defun set-drvfs-alist ()
  (interactive)
  (setq drvfs-alist
        (mapcar (lambda (x)
                  (setq x (replace-regexp-in-string "|/.\\."
                                                    (lambda (y)
                                                      (format "|//%o." (string-to-char (substring y 2 3))))
                                                    x))
                  (when (string-match "\\(.*\\)|\\(.*\\)" x)
                    (cons (match-string 1 x) (match-string 2 x))))
                (delete "" (split-string
                            (shell-command-to-string
                             "mount | grep 'type drvfs' | sed -r 's/(.*) on (.*) type drvfs .*/\\2\\|\\1/' | sed 's!\\\\!/!g'")
                            "\n")))))

(set-drvfs-alist)

(defconst windows-path-style-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\\\.*\\|[a-zA-Z]:/.*\\|\\\\\\\\.*\\|//.*\\)")

(defun windows-path-convert-file-name (name)
  (setq name (replace-regexp-in-string windows-path-style-regexp "\\2" name t nil))
  (setq name (replace-regexp-in-string "\\\\" "/" name))
  (let ((case-fold-search t))
    (cl-loop for drvfs in drvfs-alist
             if (string-match (concat "^" (regexp-quote (cdr drvfs))) name)
             return (replace-match (car drvfs) t t name)
             finally return name)))

(defun windows-path-run-real-handler (operation args)
  "Run OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (append '(windows-path-map-drive-hook-function)
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun windows-path-map-drive-hook-function (operation name &rest args)
  "Run OPERATION on cygwin NAME with ARGS."
  (windows-path-run-real-handler
   operation
   (cons (windows-path-convert-file-name name)
         (if (stringp (car args))
             (cons (windows-path-convert-file-name (car args))
                   (cdr args))
           args))))

(add-to-list 'file-name-handler-alist
             (cons windows-path-style-regexp
                   'windows-path-map-drive-hook-function))
(provide 'windows-path)
