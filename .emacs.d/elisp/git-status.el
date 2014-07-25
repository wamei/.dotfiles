;;設定
;;(require 'git-status)
;;psvn.el http://www.xsteve.at/prg/emacs/psvn.el からの移植

(eval-when-compile (require 'cl))
(require 'vc-git)
(add-to-list 'vc-handled-backends 'Git)

(defvar git-status-state-mark-modeline t
  "modeline mark display or not")

(defun git-status-update-modeline ()
  "Update modeline state dot mark properly"
  (force-mode-line-update t))

(defun git-substring-no-properties (string &optional from to)
  (if (fboundp 'substring-no-properties)
      (substring-no-properties string from to)
    (substring string (or from 0) to)))

(defun git-status-in-vc-mode? ()
  "Is vc-git active?"
  (interactive)
  (and vc-mode (string-match "^ GIT" (git-substring-no-properties vc-mode))))

(defun git-status-state-mark-modeline-dot (stat)
  (case stat
    ('edited     (propertize "● " 'face '(:height 0.8 :foreground "tomato")))
    ('up-to-date (propertize "● " 'face '(:height 0.8 :foreground "GreenYellow")))
    ('unknown    (propertize "● " 'face '(:height 0.8 :foreground "gray")))
    ('added      (propertize "● " 'face '(:height 0.8 :foreground  "blue")))
    ('deleted    (propertize "● " 'face '(:height 0.8 :foreground "red")))
    ('unmerged   (propertize "● " 'face '(:height 0.8 :foreground  "purple")))
    (t           (propertize "● " 'face '(:height 0.8 :foreground "red")))
    ))

(defadvice vc-after-save (after git-status-vc-git-after-save activate)
  (when (git-status-in-vc-mode?) (git-status-update-modeline)))

(defadvice vc-find-file-hook (after git-status-vc-git-find-file-hook activate)
  (when (git-status-in-vc-mode?) (git-status-update-modeline)))

;; http://d.hatena.ne.jp/kitokitoki/20100824/p1#c1282700989 より。
;; ToDo あとで検証
(defadvice vc-git-checkin (after git-status-vc-git-after-checkin activate)
  (when (git-status-in-vc-mode?) (git-status-update-modeline)))

(provide 'git-status)
