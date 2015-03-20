;;; sticky-notes.el --- Post it

;; Author: wamei <wamei.cho@gmail.com>
;; Keywords: memo, overlay, post it
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (popup "0.5.2"))

;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Post it.
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'popup)

(defun* sticky-notes-post (memo &key
                           (position (point))
                           (background "khaki1")
                           (foreground "black")
                           (width 30)
                           (height 5))
  (interactive "sMemo: ")
  (when popup (popup-delete popup))
  (let* ((org-capture-templates
          '(("m" "Sticky Notes" entry (file+headline (concat org-directory org-code-reading-file) "Sticky Notes") "** %(identity memo)\n   %a\n   %T" :immediate-finish t)))
         (capture-return (org-capture nil "m")))
    (when capture-return
      (let* ((memo-length (string-width memo))
             (memo-line)
             (memo-list))
        (while (> memo-length 0)
          (setq memo-line (truncate-string-to-width memo 30))
          (setq memo (substring memo (length memo-line)))
          (setq memo-list (append memo-list (list memo-line)))
          (setq memo-length (string-width memo)))
        (setq popup (popup-create position width height :face `(:background ,background :foreground ,foreground)))
        (popup-set-list popup memo-list)
        (popup-draw popup)
        ))
    capture-return
    )
  )

(provide 'sticky-notes)

;;; sticky-notes.el ends here
