;;; orglink-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "orglink" "orglink.el" (22546 59664 0 0))
;;; Generated autoloads from orglink.el

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

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; orglink-autoloads.el ends here
