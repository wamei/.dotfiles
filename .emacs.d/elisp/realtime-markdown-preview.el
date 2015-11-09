;;; realtime-markdown-preview.el --- Realtime preview by eww -*- lexical-binding: t; -*-

;; Copyright (c) 2015 wamei

;; Author: wamei <wamei.cho@gmail.com>
;; Version 0.0.1
;; Package-Requires: ((emacs "24.4") (markdown "2.0"))


;; This file is not part of GNU Emacs.

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This package provides the realtime preview by eww.

;;; Code:

(require 'markdown-mode)

(defvar realtime-preview-waiting-idling-second 1
  "Seconds of convert waiting")

(defun rmp--do-convert ()
  (let ((cb (current-buffer))
        (browse-url-browser-function 'eww-browse-url))
    (browse-url-of-buffer (markdown-standalone markdown-output-buffer-name))
    (switch-to-buffer cb)))

(defun rmp-preview ()
  (interactive)
  (let ((browse-url-browser-function 'eww-browse-url))
    (browse-url-of-buffer (markdown-standalone markdown-output-buffer-name))))

;;;### autoload
(defun rmp-start ()
  "Start realtime preview."
  (interactive)
  (run-with-idle-timer realtime-preview-waiting-idling-second nil 'realtime-preview--do-convert))

(provide 'realtime-markdown-preview)
;;; realtime-markdown-preview.el ends here
