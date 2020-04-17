;;; phi-search-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "phi-replace" "phi-replace.el" (0 0 0 0))
;;; Generated autoloads from phi-replace.el

(autoload 'phi-replace "phi-replace" "\
replace command using phi-search

\(fn)" t nil)

(autoload 'phi-replace-query "phi-replace" "\
replace command using phi-search

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "phi-replace" '("phi-replace-")))

;;;***

;;;### (autoloads nil "phi-search" "phi-search.el" (0 0 0 0))
;;; Generated autoloads from phi-search.el

(autoload 'phi-search "phi-search" "\
incremental search command compatible with \"multiple-cursors\"

\(fn &optional USE-ISEARCH)" t nil)

(autoload 'phi-search-backward "phi-search" "\
incremental search command compatible with \"multiple-cursors\"

\(fn &optional USE-ISEARCH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "phi-search" '("phi-search-")))

;;;***

;;;### (autoloads nil "phi-search-core" "phi-search-core.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from phi-search-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "phi-search-core" '("phi-search-")))

;;;***

;;;### (autoloads nil nil ("phi-search-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; phi-search-autoloads.el ends here
