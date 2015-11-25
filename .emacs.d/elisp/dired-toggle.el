;;; dired-toggle.el --- provide a simple way to toggle dired buffer for current directory
;;
;; Copyright (C) 2013, Xu FaSheng
;;
;; Author: Xu FaSheng <fasheng.xu@gmail.com>
;; Maintainer: Xu FaSheng
;; Version: 0.1
;; URL: https://github.com/fasheng/dired-toggle
;;
;; Edited by: wamei
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;; Code:

(require 's)
(require 'dired)
(require 'dired-subtree)

(defvar dired-toggle-buffer-name "*FileTree* "
  "Target buffer name for `dired-toggle'.")

(defvar dired-toggle-modeline-lighter " FileTree"
  "Modeline lighter for `dired-toggle-mode'.")

(defvar dired-toggle-dired-mode-name 'dired-mode
  "Setup the default dired mode working with `dired-toggle-mode'.")

(defvar dired-toggle-pop-to-buffer-function 'pop-to-buffer)

(defun dired-toggle-list-dir (buffer dir &optional mode)
  "List target directory in a buffer."
  (let ((mode (or mode dired-toggle-dired-mode-name)))
    (with-current-buffer buffer
      (setq default-directory dir)
      (if (eq mode major-mode)
          (setq dired-directory dir)
        (funcall mode dir))
      (dired-hide-details-mode t)
      (toggle-truncate-lines t)
      (dired-toggle-mode 1)
      ;; default-directory and dired-actual-switches are set now
      ;; (buffer-local), so we can call dired-readin:
      (unwind-protect
          (progn (dired-readin)))
      )))

(defun dired-toggle-mode-buffer-p (buffer)
  (s-starts-with-p dired-toggle-buffer-name (buffer-name buffer)))

(defun dired-toggle-action-quit ()
  "Custom quit action under `dired-toggle-mode'."
  (interactive)
  (if (one-window-p)
      (quit-window)
    (delete-window)))

(defun dired-toggle-action-dired-find-alternate-file ()
  "Custom item action under `dired-toggle-mode'."
  (interactive)
  (dired-toggle-action-dired-find-file 1 t))

(defun dired-toggle-action-dired-find-file (n &optional alternate)
  "Custom item action under `dired-toggle-mode'."
  (interactive "p")
  (let* ((buffer (current-buffer))
         (file (dired-get-file-for-visit))
         (dir-p (file-directory-p file))
         (alternate (or alternate nil)))
    (if (and dir-p (= n 1))
        (if alternate
            (dired-toggle-list-dir buffer (file-name-as-directory file))
          (dired-subtree-toggle))
      (cl-letf ((orig-func (symbol-function 'switch-to-buffer))
                ((symbol-function 'switch-to-buffer)
                 (lambda (buffer) (other-window 1) (funcall orig-func buffer))))
        (dired-find-file))
      )))

(defun dired-toggle-action-find-file ()
  "Custom `find-file' action under `dired-toggle-mode'."
  (interactive)
  (cl-letf ((default-directory (dired-default-directory))
            (orig-func (symbol-function 'switch-to-buffer))
            ((symbol-function 'switch-to-buffer)
             (lambda (buffer) (other-window 1) (funcall orig-func buffer))))
   (call-interactively 'find-file)))

(defun dired-toggle-action-up-directory ()
  "Custom up directory action under `dired-toggle-mode'."
  (interactive)
  (let* ((buffer (current-buffer))
         (dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (dired-toggle-list-dir buffer up)
          (dired-goto-file dir)))))

(defvar dired-toggle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q"         'dired-toggle-action-quit)
    (define-key map (kbd "RET") 'dired-toggle-action-dired-find-file)
    (define-key map [mouse-1]   'dired-toggle-action-dired-find-file)
    (define-key map [mouse-2]   'dired-toggle-action-dired-find-file)
    (define-key map (kbd "a")   'dired-toggle-action-dired-find-alternate-file)
    (define-key map "^"         'dired-toggle-action-up-directory)
    (define-key map "\C-c\C-u"  'dired-toggle-action-up-directory)
    (define-key map "\C-x\C-f"  'dired-toggle-action-find-file)
    map)
  "Keymap for `dired-toggle-mode'.")

(defvar dired-toggle-mode-hook nil
  "Function(s) to call after `dired-toggle-mode' enabled.")

(define-minor-mode dired-toggle-mode
  "Assistant minor mode for `dired-toggle'."
  :lighter dired-toggle-modeline-lighter
  :keymap dired-toggle-mode-map
  :after-hook dired-toggle-mode-hook
  )

(defun dired-toggle-get-or-create-buffer (dir)
  "Get or create dired toggle buffer for DIR."
  (let* ((win (selected-window))
         (file (buffer-file-name))
         (target-bufname (concat dired-toggle-buffer-name dir))
         (new-dired-buffer-p (not (get-buffer target-bufname)))
         (target-buf (get-buffer-create target-bufname))
         (dired-buffer-with-same-dir (dired-find-buffer-nocreate dir)))
    (with-current-buffer target-buf
      ;; init dired-mode
      (if new-dired-buffer-p
          (dired-toggle-list-dir target-buf dir))
      ;; try to select target file
      (if file
          (or (dired-goto-file file)
              ;; Toggle omitting, if it is on, and try again.
              (when dired-omit-mode
                (dired-omit-mode 0)
                (dired-goto-file file)))))
    target-buf))

(defun dired-toggle-other-window:after (count &optional all-frames)
  "Skip dired file tree window."
  (let ((count (or count 1)))
    (when (s-starts-with-p dired-toggle-buffer-name (buffer-name (window-buffer (selected-window))))
      (other-window count))))
(advice-add 'other-window :after 'dired-toggle-other-window:after)

(defun dired-toggle-find-buffer-nocreate:around (orign dirname &optional mode)
  (let ((dired-buffers (cl-remove-if
                        (lambda (elm)
                          (dired-toggle-mode-buffer-p (cdr elm)))
                        dired-buffers)))
    (funcall orign dirname mode)))
(advice-add 'dired-find-buffer-nocreate :around 'dired-toggle-find-buffer-nocreate:around)

(when (require 'elscreen nil t)
  (defun dired-toggle-elscreen-tab-update:around (orig &optional force)
    (cl-letf ((orig-func (symbol-function 'frame-first-window))
              ((symbol-function 'frame-first-window)
               (lambda ()
                 (let* ((win (funcall orig-func))
                       (ret win))
                   (if (and (dired-toggle-mode-buffer-p (window-buffer win))
                            (not (one-window-p)))
                       (save-selected-window
                         (advice-remove 'other-window 'dired-toggle-other-window:after)
                         (select-window ret)
                         (other-window 1)
                         (setq ret (selected-window))
                         (advice-add 'other-window :after 'dired-toggle-other-window:after)))
                   ret))))
      (funcall orig force)))
  (advice-add 'elscreen-tab-update :around 'dired-toggle-elscreen-tab-update:around))

;;;###autoload
(defun dired-toggle (&optional dir)
  "Toggle current buffer's directory."
  (interactive)
  (let* ((file (buffer-file-name))
         (dir (or dir (if file (file-name-directory file) default-directory)))
         (buffer (dired-toggle-get-or-create-buffer dir))
         (is-current-buffer (dired-toggle-mode-buffer-p (current-buffer)))
         (is-shown nil)
         (window))
    (cond ((and (not (one-window-p)) is-current-buffer)
           (other-window 1))
          ((not is-current-buffer)
           (walk-windows
            (lambda (win)
              (when (dired-toggle-mode-buffer-p (window-buffer win))
                (setq window win)
                (setq buffer (window-buffer win))
                (setq is-shown t))))
           (cond (is-shown
                  (select-window window)
                  (switch-to-buffer buffer))
                 (t
                  (funcall dired-toggle-pop-to-buffer-function buffer)
                  ))))))

(provide 'dired-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-toggle.el ends here
