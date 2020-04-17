;;; robe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ac-robe" "../../../../.emacs.d/elpa/robe-20190521.58/ac-robe.el"
;;;;;;  "ac9a0897772134da26ef30d7bf8ae584")
;;; Generated autoloads from ../../../../.emacs.d/elpa/robe-20190521.58/ac-robe.el

(autoload 'ac-robe-available "ac-robe" "\
Return t if `robe-mode' completions are available, otherwise nil.

\(fn)" nil nil)

(autoload 'ac-robe-setup "ac-robe" "\


\(fn)" nil nil)

(defconst ac-source-robe '((available . ac-robe-available) (prefix . ac-robe-prefix) (candidates . ac-robe-candidates) (document . ac-robe-doc) (symbol . "r")) "\
`auto-complete' completion source for Ruby using `robe-mode'.")

;;;***

;;;### (autoloads nil "company-robe" "../../../../.emacs.d/elpa/robe-20190521.58/company-robe.el"
;;;;;;  "6d779558c1e3e24b3d300285c409c301")
;;; Generated autoloads from ../../../../.emacs.d/elpa/robe-20190521.58/company-robe.el

(autoload 'company-robe "company-robe" "\
A `company-mode' completion back-end for `robe-mode'.

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

;;;***

;;;### (autoloads nil "robe" "../../../../.emacs.d/elpa/robe-20190521.58/robe.el"
;;;;;;  "b3261aed7c300c360ba5b13748128702")
;;; Generated autoloads from ../../../../.emacs.d/elpa/robe-20190521.58/robe.el

(autoload 'robe-mode "robe" "\
Improved navigation for Ruby.

The following commands are available:

\\{robe-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-robe-mode nil "\
Non-nil if Global Robe mode is enabled.
See the `global-robe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-robe-mode'.")

(custom-autoload 'global-robe-mode "robe" nil)

(autoload 'global-robe-mode "robe" "\
Toggle Robe mode in all buffers.
With prefix ARG, enable Global Robe mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Robe mode is enabled in all buffers where
`robe-mode-on' would do it.
See `robe-mode' for more information on Robe mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/robe-20190521.58/ac-robe.el"
;;;;;;  "../../../../.emacs.d/elpa/robe-20190521.58/company-robe.el"
;;;;;;  "../../../../.emacs.d/elpa/robe-20190521.58/robe-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/robe-20190521.58/robe-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/robe-20190521.58/robe.el") (24198
;;;;;;  52337 260888 400000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; robe-autoloads.el ends here
