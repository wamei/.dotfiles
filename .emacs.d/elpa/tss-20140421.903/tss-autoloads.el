;;; tss-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "tss" "../../../../.emacs.d/elpa/tss-20140421.903/tss.el"
;;;;;;  "d39b021d97c20acadb23e8b344bac78b")
;;; Generated autoloads from ../../../../.emacs.d/elpa/tss-20140421.903/tss.el

(autoload 'tss-popup-help "tss" "\
Popup help about anything at point.

\(fn)" t nil)

(autoload 'tss-jump-to-definition "tss" "\
Jump to method definition at point.

\(fn)" t nil)

(autoload 'tss-run-flymake "tss" "\
Run check by flymake for current buffer.

\(fn)" t nil)

(autoload 'tss-reload-current-project "tss" "\
Reload project data for current buffer.

\(fn)" t nil)

(autoload 'tss-restart-current-buffer "tss" "\
Restart TSS for current buffer.

\(fn)" t nil)

(autoload 'tss-setup-current-buffer "tss" "\
Do setup for using TSS in current buffer.

\(fn)" t nil)

(autoload 'tss-config-default "tss" "\
Do setting recommemded configuration.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "typescript" "../../../../.emacs.d/elpa/tss-20140421.903/typescript.el"
;;;;;;  "1f5e24f513cdda43eae13d1046b52297")
;;; Generated autoloads from ../../../../.emacs.d/elpa/tss-20140421.903/typescript.el

(autoload 'typescript-mode "typescript" "\
Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

\(fn)" t nil)

(eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}")))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/tss-20140421.903/tss-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/tss-20140421.903/tss-pkg.el" "../../../../.emacs.d/elpa/tss-20140421.903/tss.el"
;;;;;;  "../../../../.emacs.d/elpa/tss-20140421.903/typescript.el")
;;;;;;  (21443 42557 732207 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; tss-autoloads.el ends here
