;;; elscreen-separate-buffer-list.el --- Separate buffer list manager for elscreen

;; Author: Kazuaki Hamada <wamei.cho@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'elscreen)

(defvar elscreen-separate-buffer-list-default '("*scratch*" "*Messages*"))
(defvar elscreen-separate-buffer-list '())

(defun elscreen-make-default-separate-buffer-list ()
  "Make default separate buffer list."
  (cl-loop for i in elscreen-separate-buffer-list-default
           collect (get-buffer i)))

(defun elscreen-separate-buffer-list-init-hook ()
  "."
  (setq elscreen-separate-buffer-list (elscreen-make-default-separate-buffer-list)))
(add-hook 'after-init-hook 'elscreen-separate-buffer-list-init-hook)

(defun elscreen-get-separate-buffer-list (screen)
  "Return buffer list of SCREEN."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (assoc-default 'separate-buffer-list screen-property)))

(defun elscreen-set-separate-buffer-list (screen buflist)
  "Set buffer list of SCREEN to BUFLIST."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'separate-buffer-list buflist)
    (elscreen-set-screen-property screen screen-property)))

(defun elscreen-get-separate-window-history (screen)
  "Return window-history of SCREEN."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (assoc-default 'separate-window-history screen-property)))

(defun elscreen-set-separate-window-history (screen winHistory)
  "Set window-history of SCREEN to WINHISTORY."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'separate-window-history winHistory)
    (elscreen-set-screen-property screen screen-property)))

(defun elscreen-save-separate-buffer-list (&optional screen)
  "Save the buffer list order for SCREEN, or current screen."
  (let ((screen (or screen (elscreen-get-current-screen))))
    (elscreen-set-separate-buffer-list screen elscreen-separate-buffer-list)))

(defun elscreen-load-separate-buffer-list (&optional screen)
  "Set EMACS buffer list order to that of SCREEN, or current screen."
  (let* ((screen (or screen (elscreen-get-current-screen)))
         (buffList (elscreen-get-separate-buffer-list screen)))
    (if buffList
        (setq elscreen-separate-buffer-list buffList)
      (setq elscreen-separate-buffer-list (elscreen-make-default-separate-buffer-list)))))

(defun elscreen-add-separate-buffer-list (buffer)
  "BUFFER."
  (if (not (member buffer elscreen-separate-buffer-list))
      (setq elscreen-separate-buffer-list (append (list buffer) elscreen-separate-buffer-list))))

(defun elscreen-remove-separate-buffer-list (buffer)
  "BUFFER."
  (setq elscreen-separate-buffer-list (cl-loop for i in elscreen-separate-buffer-list
                                            unless (equal i buffer)
                                            collect i)))

(defun elscreen-update-separate-buffer-list ()
  "."
  (setq elscreen-separate-buffer-list (cl-loop for i in elscreen-separate-buffer-list
                                            if (buffer-live-p i)
                                            collect i)))

(defun elscreen-separate-buffer-list-goto-hook ()
  "Manage screen-specific buffer lists."
  (when (elscreen-screen-live-p (elscreen-get-previous-screen))
    (elscreen-save-separate-buffer-list (elscreen-get-previous-screen)))
  (elscreen-load-separate-buffer-list (elscreen-get-current-screen)))
(add-hook 'elscreen-goto-hook 'elscreen-separate-buffer-list-goto-hook)

(defun elscreen-goto:restore-window-history (origin &rest args)
  (elscreen-set-separate-window-history (elscreen-get-current-screen) (elscreen-get-all-window-history-alist))
  (apply origin args)
  (elscreen-restore-all-window-history-alist (elscreen-get-separate-window-history (elscreen-get-current-screen))))
(advice-add 'elscreen-goto :around 'elscreen-goto:restore-window-history)

(defun elscreen-separate-buffer-list-kill-hook ()
  "Just load current buffer list' state when screen is killed."
  (elscreen-load-separate-buffer-list (elscreen-get-current-screen))
  )
(add-hook 'elscreen-kill-hook 'elscreen-separate-buffer-list-kill-hook)

(defun elscreen-clone:load-buffer-list (&rest _)
  "Cloning scleen also copy the buffer list' state."
  (let ((stack (elscreen-get-separate-buffer-list (elscreen-get-previous-screen))))
    (elscreen-set-separate-buffer-list (elscreen-get-current-screen) stack)
    (elscreen-load-separate-buffer-list (elscreen-get-current-screen)))
  )
(advice-add 'elscreen-clone :after 'elscreen-clone:load-buffer-list)

(defun elscreen-kill-buffer-hook ()
  "."
  (elscreen-remove-separate-buffer-list (current-buffer)))
(add-hook 'kill-buffer-hook 'elscreen-kill-buffer-hook)

(defun elscreen-buffer-list-update-hook ()
  "."
  (elscreen-update-separate-buffer-list))
(add-hook 'buffer-list-update-hook 'elscreen-buffer-list-update-hook)

(defun switch-to-buffer:elscreen-separate-buffer-list (buffer &rest _)
  (elscreen-add-separate-buffer-list (get-buffer buffer)))
(advice-add 'switch-to-buffer :after 'switch-to-buffer:elscreen-separate-buffer-list)

(defun get-buffer-create:elscreen-separate-buffer-list (buffer &rest _)
  (elscreen-add-separate-buffer-list (get-buffer buffer)))
(advice-add 'get-buffer-create :after 'get-buffer-create:elscreen-separate-buffer-list)

(defun buffer-list:return-separate-buffer-list (origin &rest _)
  (cl-loop for i in (apply origin _)
           if (member (get-buffer i) elscreen-separate-buffer-list)
           collect i))
(advice-add 'buffer-list :around 'buffer-list:return-separate-buffer-list)


(defun elscreen-get-all-window-history-alist ()
  "."
  (mapcar (lambda (window)
            (let ((prevs (window-prev-buffers window))
                  (nexts (window-next-buffers window)))
              (cons window (cons prevs nexts))))
          (window-list)))

(defun elscreen-restore-all-window-history-alist (history-alist)
  "HISTORY-ALIST."
  (mapc (lambda (entry)
          (let* ((window (car entry))
                 (histories (cdr entry))
                 (prevs (car histories))
                 (nexts (cdr histories)))
            (when (window-valid-p window)
              (set-window-prev-buffers window prevs)
              (set-window-next-buffers window nexts))))
        history-alist))

(defun helm-buffer-list:filtering (origin &rest args)
  (cl-loop for i in (apply origin args)
           if (member (get-buffer i) elscreen-separate-buffer-list)
           collect i))
(advice-add 'helm-buffer-list :around 'helm-buffer-list:filtering)

(provide 'elscreen-separate-buffer-list)

;;; elscreen-separate-buffer-list.el ends here
