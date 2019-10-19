;;; tide-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tide" "../../../../.emacs.d/elpa/tide-20191004.1231/tide.el"
;;;;;;  "8af94a0072f70664a3b9208662cf959c")
;;; Generated autoloads from ../../../../.emacs.d/elpa/tide-20191004.1231/tide.el

(autoload 'company-tide "tide" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'tide-format-before-save "tide" "\
Before save hook to format the buffer before each save.

\(fn)" t nil)

(autoload 'tide-format "tide" "\
Format the current region or buffer.

\(fn)" t nil)

(autoload 'tide-setup "tide" "\
Setup `tide-mode' in current buffer.

\(fn)" t nil)

(autoload 'tide-mode "tide" "\
Minor mode for Typescript Interactive Development Environment.

\\{tide-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'tide-project-errors "tide" "\


\(fn)" t nil)

(autoload 'tide-unhighlight-identifiers "tide" "\
Remove highlights from previously highlighted identifier.

\(fn)" nil nil)

(autoload 'tide-hl-identifier "tide" "\
Highlight all instances of the identifier under point. Removes
highlights from previously highlighted identifier.

\(fn)" t nil)

(autoload 'tide-hl-identifier-mode "tide" "\
Highlight instances of the identifier at point after a short
timeout.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "tide" "../../../../.emacs.d/elpa/tide-20191004.1231/tide.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/tide-20191004.1231/tide.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tide" '("tide-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "tide-lv" "../../../../.emacs.d/elpa/tide-20191004.1231/tide-lv.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/tide-20191004.1231/tide-lv.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tide-lv" '("tide-lv-")))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/tide-20191004.1231/tide-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/tide-20191004.1231/tide-lv.el"
;;;;;;  "../../../../.emacs.d/elpa/tide-20191004.1231/tide-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/tide-20191004.1231/tide.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tide-autoloads.el ends here
