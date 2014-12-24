(eval-when-compile (require 'cl))
(require 'elscreen)

(defvar elscreen-separate-buffer-list-default (list (get-buffer "*scratch*")
                                                    (get-buffer "*Messages*")))
(defvar elscreen-separate-buffer-list elscreen-separate-buffer-list-default)

(defun reorder-buffer-list (new-list)
  "Set buffers in NEW-LIST to be the most recently used, in order."
  (when new-list
    (let (firstbuf buf)
      (while new-list
        (setq buf (car new-list))
        (when (stringp buf)
          (setq buf (get-buffer buf)))
        (unless (not (buffer-live-p buf))
          (bury-buffer buf)
          (unless firstbuf
            (setq firstbuf buf)))
        (setq new-list (cdr new-list)))
      (setq new-list (buffer-list))
      (while (not (eq (car new-list) firstbuf))
        (bury-buffer (car new-list))
        (setq new-list (cdr new-list))))))

(defun elscreen-get-buffer-list (screen)
  "Return buffer-list of SCREEN."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (assoc-default 'buffer-list screen-property)))

(defun elscreen-set-buffer-list (screen buflist)
  "Set buffer-list of SCREEN to BUFLIST."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'buffer-list buflist)
    (elscreen-set-screen-property screen screen-property)))

(defun elscreen-get-separate-buffer-list (screen)
  "Return buffer-list of SCREEN."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (assoc-default 'separate-buffer-list screen-property)))

(defun elscreen-set-separate-buffer-list (screen buflist)
  "Set buffer-list of SCREEN to BUFLIST."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'separate-buffer-list buflist)
    (elscreen-set-screen-property screen screen-property)))

(defun elscreen-save-buffer-list (&optional screen)
  "Save the buffer-list order for SCREEN, or current screen"
  (let ((screen (or screen (elscreen-get-current-screen))))
    (elscreen-set-separate-buffer-list screen elscreen-separate-buffer-list)
    (elscreen-set-buffer-list screen (buffer-list))
    ))

(defun elscreen-load-buffer-list (&optional screen)
  "Set emacs' buffer-list order to that of SCREEN, or current screen"
  (let* ((screen (or screen (elscreen-get-current-screen)))
         (buffList (elscreen-get-separate-buffer-list screen)))
    (if buffList
        (setq elscreen-separate-buffer-list buffList)
      (setq elscreen-separate-buffer-list elscreen-separate-buffer-list-default))
    (reorder-buffer-list (elscreen-get-buffer-list screen))
    ))

(defun elscreen-add-separate-buffer-list (buffer)
  (if (not (member buffer elscreen-separate-buffer-list))
      (setq elscreen-separate-buffer-list (append (list buffer) elscreen-separate-buffer-list))))

(defun elscreen-remove-separate-buffer-list (buffer)
  (setq elscreen-separate-buffer-list (cl-loop for i in elscreen-separate-buffer-list
                                            unless (equal i buffer)
                                            collect i)))

(defun elscreen-update-separate-buffer-list ()
  (setq elscreen-separate-buffer-list (cl-loop for i in elscreen-separate-buffer-list
                                            if (buffer-live-p i)
                                            collect i)))

(defun elscreen-separate-buffer-list-goto-hook ()
  "Manage screen-specific buffer lists."
  (when (elscreen-screen-live-p (elscreen-get-previous-screen))
    (elscreen-save-buffer-list (elscreen-get-previous-screen)))
  (elscreen-load-buffer-list (elscreen-get-current-screen)))
(add-hook 'elscreen-goto-hook 'elscreen-separate-buffer-list-goto-hook)

(defun elscreen-separate-buffer-list-kill-hook ()
  "Just load current buffer list' state when screen is killed"
  (elscreen-load-buffer-list (elscreen-get-current-screen))
  )
(add-hook 'elscreen-kill-hook 'elscreen-separate-buffer-list-kill-hook)

(defun elscreen-clone:load-buffer-list (&rest _)
  "Cloning scleen also copy the buffer list' state."
  (let ((stack (elscreen-get-buffer-list (elscreen-get-previous-screen))))
    (elscreen-set-buffer-list (elscreen-get-current-screen) stack)
    (elscreen-load-buffer-list (elscreen-get-current-screen)))
  )
(advice-add 'elscreen-clone :after 'elscreen-clone:load-buffer-list)

(defun elscreen-kill-buffer-hook ()
  (elscreen-remove-separate-buffer-list (current-buffer)))
(add-hook 'kill-buffer-hook 'elscreen-kill-buffer-hook)

(defun elscreen-buffer-list-update-hook ()
  (elscreen-update-separate-buffer-list))
(add-hook 'buffer-list-update-hook 'elscreen-buffer-list-update-hook)

(defun switch-to-buffer:elscreen-separate-buffer-list (buffer &rest _)
  (elscreen-add-separate-buffer-list (get-buffer buffer)))
(advice-add 'switch-to-buffer :after 'switch-to-buffer:elscreen-separate-buffer-list)

(defun get-buffer-create:elscreen-separate-buffer-list (buffer &rest _)
  (elscreen-add-separate-buffer-list (get-buffer buffer)))
(advice-add 'get-buffer-create :after 'get-buffer-create:elscreen-separate-buffer-list)

(provide 'elscreen-separate-buffer-list)
