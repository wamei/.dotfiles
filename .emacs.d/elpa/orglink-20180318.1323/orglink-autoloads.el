;;; orglink-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "orglink" "../../../../.emacs.d/elpa/orglink-20180318.1323/orglink.el"
;;;;;;  "ffd77bfcc07a680e271257d6bb0469c2")
;;; Generated autoloads from ../../../../.emacs.d/elpa/orglink-20180318.1323/orglink.el

(autoload 'orglink-mode "orglink" "\
Toggle display Org-mode links in other major modes.

On the links the following commands are available:

\\{orglink-mouse-map}

\(fn &optional ARG)" t nil)

(defvar global-orglink-mode nil "\
Non-nil if Global-Orglink mode is enabled.
See the command `global-orglink-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-orglink-mode'.")

(custom-autoload 'global-orglink-mode "orglink" nil)

(autoload 'global-orglink-mode "orglink" "\
Toggle Orglink mode in all buffers.
With prefix ARG, enable Global-Orglink mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Orglink mode is enabled in all buffers where
`turn-on-orglink-mode-if-desired' would do it.
See `orglink-mode' for more information on Orglink mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/orglink-20180318.1323/orglink-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/orglink-20180318.1323/orglink.el")
;;;;;;  (23253 26446 605033 500000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; orglink-autoloads.el ends here
