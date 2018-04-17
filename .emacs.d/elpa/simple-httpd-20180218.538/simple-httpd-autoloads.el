;;; simple-httpd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "simple-httpd" "../../../../.emacs.d/elpa/simple-httpd-20180218.538/simple-httpd.el"
;;;;;;  "7df04ad8952b33be1896ee632121a80d")
;;; Generated autoloads from ../../../../.emacs.d/elpa/simple-httpd-20180218.538/simple-httpd.el

(autoload 'httpd-start "simple-httpd" "\
Start the web server process. If the server is already
running, this will restart the server. There is only one server
instance per Emacs instance.

\(fn)" t nil)

(autoload 'httpd-stop "simple-httpd" "\
Stop the web server if it is currently running, otherwise do nothing.

\(fn)" t nil)

(autoload 'httpd-running-p "simple-httpd" "\
Return non-nil if the simple-httpd server is running.

\(fn)" nil nil)

(autoload 'httpd-serve-directory "simple-httpd" "\
Start the web server with given `directory' as `httpd-root'.

\(fn DIRECTORY)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/simple-httpd-20180218.538/simple-httpd-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/simple-httpd-20180218.538/simple-httpd.el")
;;;;;;  (23253 26416 535800 400000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; simple-httpd-autoloads.el ends here
