;;; editorconfig-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "editorconfig" "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig.el"
;;;;;;  "4977f5666d712e5920c2c45fcc1bd2f3")
;;; Generated autoloads from ../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig.el

(autoload 'editorconfig-apply "editorconfig" "\
Get and apply EditorConfig properties to current buffer.

This function does not respect the values of `editorconfig-exclude-modes' and
`editorconfig-exclude-regexps' and always applies available properties.
Use `editorconfig-mode-apply' instead to make use of these variables.

\(fn)" t nil)

(defvar editorconfig-mode nil "\
Non-nil if Editorconfig mode is enabled.
See the `editorconfig-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `editorconfig-mode'.")

(custom-autoload 'editorconfig-mode "editorconfig" nil)

(autoload 'editorconfig-mode "editorconfig" "\
Toggle EditorConfig feature.

To disable EditorConfig in some buffers, modify
`editorconfig-exclude-modes' or `editorconfig-exclude-regexps'.

\(fn &optional ARG)" t nil)

(autoload 'editorconfig-find-current-editorconfig "editorconfig" "\
Find the closest .editorconfig file for current file.

\(fn)" t nil)

(autoload 'editorconfig-display-current-properties "editorconfig" "\
Display EditorConfig properties extracted for current buffer.

\(fn)" t nil)

(defalias 'describe-editorconfig-properties 'editorconfig-display-current-properties)

(autoload 'editorconfig-format-buffer "editorconfig" "\
Format buffer according to .editorconfig indent_style and indent_width.

\(fn)" t nil)

(autoload 'editorconfig-version "editorconfig" "\
Get EditorConfig version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

\(fn &optional SHOW-VERSION)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "editorconfig"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "editorconfig" '("editorconfig-")))

;;;***

;;;***

;;;### (autoloads nil "editorconfig-conf-mode" "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-conf-mode.el"
;;;;;;  "cbea126d23cf22724dee7d8b5017fce9")
;;; Generated autoloads from ../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-conf-mode.el

(autoload 'editorconfig-conf-mode "editorconfig-conf-mode" "\
Major mode for editing .editorconfig files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.editorconfig\\'" . editorconfig-conf-mode))

;;;### (autoloads "actual autoloads are elsewhere" "editorconfig-conf-mode"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-conf-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-conf-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "editorconfig-conf-mode" '("editorconfig-conf-mode-")))

;;;***

;;;***

;;;### (autoloads nil "editorconfig-core" "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-core.el"
;;;;;;  "e1890946539ad5265626a4d466646795")
;;; Generated autoloads from ../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-core.el

(autoload 'editorconfig-core-get-nearest-editorconfig "editorconfig-core" "\
Return path to .editorconfig file that is closest to DIRECTORY.

\(fn DIRECTORY)" nil nil)

(autoload 'editorconfig-core-get-properties "editorconfig-core" "\
Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This functions returns alist of properties.  Each element will look like
'(KEY . VALUE) .

\(fn &optional FILE CONFNAME CONFVERSION)" nil nil)

(autoload 'editorconfig-core-get-properties-hash "editorconfig-core" "\
Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This function is almost same as `editorconfig-core-get-properties', but returns
hash object instead.

\(fn &optional FILE CONFNAME CONFVERSION)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "editorconfig-core"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "editorconfig-core" '("editorconfig-core--")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "editorconfig-core-handle"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-core-handle.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-core-handle.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "editorconfig-core-handle" '("editorconfig-core-handle")))

;;;***

;;;### (autoloads nil "editorconfig-fnmatch" "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-fnmatch.el"
;;;;;;  "e2204bee76ff4478d84e820408de0dd3")
;;; Generated autoloads from ../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-fnmatch.el

(autoload 'editorconfig-fnmatch-p "editorconfig-fnmatch" "\
Test whether STRING match PATTERN.

Matching ignores case if `case-fold-search' is non-nil.

PATTERN should be a shell glob pattern, and some zsh-like wildcard matchings can
be used:

*           Matches any string of characters, except path separators (/)
**          Matches any string of characters
?           Matches any single character
\[name]      Matches any single character in name
\[^name]     Matches any single character not in name
{s1,s2,s3}  Matches any of the strings given (separated by commas)
{min..max}  Matches any number between min and max

\(fn STRING PATTERN)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "editorconfig-fnmatch"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-fnmatch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-fnmatch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "editorconfig-fnmatch" '("editorconfig-fnmatch-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-conf-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-core-handle.el"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-core.el"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-fnmatch.el"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/editorconfig-20200730.1329/editorconfig.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; editorconfig-autoloads.el ends here
