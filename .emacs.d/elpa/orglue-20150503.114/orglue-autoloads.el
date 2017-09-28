;;; orglue-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "orglue" "orglue.el" (22988 25330 891823 741000))
;;; Generated autoloads from orglue.el

(autoload 'orglue-decompose-last-org-bracket-link "orglue" "\
Find LINK-STRING backward and decompose it.
Decompose means that [[URL][DESCRIPTION]] is converted into
URL
DESCRIPTION
.

On conversion, URL is normalized by
``orglue-normalize-webpage-title'' and DESCRIPTION is
normalized by ``orglue-normalize-webpage-url''.

\(fn)" t nil)

(autoload 'orglue-indent-rigidly-to-current-level "orglue" "\
Same with ``indent-rigidly'', if not in ``org-mode''.
Takes three arguments, START, END and ARG.
If in ``org-mode'' and ARG is 4 (called with universal-prefix),
adjust indent level best suited to org-style.

\(fn START END ARG)" t nil)

(autoload 'org-table-renumber "orglue" "\
Renumber current columns on org-table.
No effect if current columns contain any non-number chars.

\(fn)" t nil)

(defalias 'orglue-evernote-insert-selected-note-as-org-links 'epic-insert-selected-note-as-org-links)

(autoload 'orglue-ns-insert-file "orglue" "\


\(fn)" t nil)

(autoload 'orglue-ns-insert-text "orglue" "\


\(fn)" t nil)

(autoload 'orglue-screencapture-and-insert "orglue" "\


\(fn)" t nil)

(autoload 'orglue-headline-string "orglue" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("orglue-pkg.el" "orglue-publish.el") (22988
;;;;;;  25330 902491 470000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; orglue-autoloads.el ends here
