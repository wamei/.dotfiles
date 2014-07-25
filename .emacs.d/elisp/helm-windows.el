(require 'helm)
(require 'windows)

(defvar my/helm-source-windows
  `((name . "Windows")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (let ((i 1)
                            (windows-list nil)
                            (form (format "%%s[%%c] %%-%ds [%%s]" (+ win:names-maxl 2))))
                        (while (< i win:max-configs)
                          (setq windows-list
                                (cons (cons (format form
                                                    (cond ((= i win:current-config) "*")
                                                          ((= i win:last-config) "+")
                                                          (t " "))
                                                    (+ win:base-key i)
                                                    (if (aref win:configs i)
                                                        (format "\"%s\"" (aref win:names-prefix i))
                                                      "")
                                                    (if (aref win:configs i) (aref win:names i)
                                                      "")
                                                    )
                                            (format "(win-switch-to-window 1 %c)" (+ win:base-key i)))
                                      windows-list))
                          (setq i (1+ i)))
                        (setq windows-list (append windows-list '((" [0] Previous Window" . "(win-toggle-window)"))))
                        (setq windows-list (append '((" [-] Switch to windows menu" . "(win-switch-menu)")) windows-list))
                        (setq windows-list (reverse windows-list))
                        ))))
    (type . sexp)))

(defun helm-windows-list ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer `(my/helm-source-windows) "*helm windows*")))

(provide 'helm-windows)
