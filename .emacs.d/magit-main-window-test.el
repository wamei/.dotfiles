;;; magit-main-window-test.el --- tests for magit-main-window -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l magit-main-window-test.el -f ert-run-tests-batch-and-exit
;;
;; magit 本体は読まない。保存と復元はカレントバッファ (magit のバッファ役) の
;; 変数と window だけを見るので、普通のバッファで magit の流れを模す。
;;; Code:

(require 'ert)
(defvar magit-inhibit-save-previous-winconf nil) ; magit-mode.el
(load (expand-file-name "magit-main-window.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defmacro magit-main-window-test--with-frame (&rest body)
  "window 構成を戻し、作ったバッファを消す後始末付きで BODY を評価する。
BODY の中では `buffers' に作ったバッファを積む。"
  (declare (indent 0))
  `(let ((buffers nil))
     (save-window-excursion
       (delete-other-windows)
       (unwind-protect
           (progn ,@body)
         (dolist (window (window-list nil 'no-mini))
           (when (window-parameter window 'window-side)
             (delete-window window)))
         (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers))))))

(defun magit-main-window-test--buffer (name)
  "NAME のバッファを作って返す。"
  (get-buffer-create name))

(defun magit-main-window-test--side (buffer side width)
  "BUFFER を SIDE の side window に WIDTH 桁で出して window を返す。"
  (display-buffer-in-side-window
   buffer `((side . ,side) (slot . 0) (window-width . ,width)
            (window-parameters . ((no-delete-other-windows . t))))))

(defun magit-main-window-test--main-buffers ()
  "本体の window (side window 以外) に出ているバッファの名前。"
  (mapcar (lambda (window) (buffer-name (window-buffer window)))
          (seq-remove (lambda (window) (window-parameter window 'window-side))
                      (window-list nil 'no-mini (frame-first-window)))))

(defun magit-main-window-test--open (magit)
  "MAGIT を magit の status のように開く (保存してから本体いっぱいに出す)。"
  (with-current-buffer magit
    (wamei/magit-save-main-window))
  ;; magit-display-buffer-fullframe-status-v1 は delete-other-windows で広げる。
  ;; side window は no-delete-other-windows で残る。本体の window は分割されて
  ;; いると内部 window なので、中の生きた window を選ぶ。
  (select-window (seq-find (lambda (window) (not (window-parameter window 'window-side)))
                           (window-list nil 'no-mini (frame-first-window))))
  (switch-to-buffer magit)
  (delete-other-windows))

;;; 復元

(ert-deftest wamei/magit-main-window-restores-the-main-area ()
  "閉じると、開く前の本体の分割と表示していたバッファに戻る。"
  (magit-main-window-test--with-frame
    (let ((a (magit-main-window-test--buffer "a"))
          (b (magit-main-window-test--buffer "b"))
          (magit (magit-main-window-test--buffer "magit: x")))
      (setq buffers (list a b magit))
      (switch-to-buffer a)
      (set-window-buffer (split-window-right) b)
      (magit-main-window-test--open magit)
      (should (equal (magit-main-window-test--main-buffers) '("magit: x")))
      (with-current-buffer magit
        (wamei/magit-restore-main-window))
      (should (equal (magit-main-window-test--main-buffers) '("a" "b"))))))

(ert-deftest wamei/magit-main-window-leaves-side-windows-alone ()
  "開いている間に side window を開いたり幅を変えたりしても、閉じたときに巻き戻さない。
magit 既定の `magit-restore-window-configuration' はフレーム全体を書き戻すので、
その間に開いた claude パネルが消え、変えたサイドバーの幅も戻ってしまう。"
  (magit-main-window-test--with-frame
    (let ((a (magit-main-window-test--buffer "a"))
          (sidebar (magit-main-window-test--buffer "sidebar"))
          (claude (magit-main-window-test--buffer "claude"))
          (magit (magit-main-window-test--buffer "magit: x")))
      (setq buffers (list a sidebar claude magit))
      (switch-to-buffer a)
      (let ((left (magit-main-window-test--side sidebar 'left 20)))
        (magit-main-window-test--open magit)
        ;; magit を開いている間の変更
        (window-resize left (- 30 (window-total-width left)) t)
        (let ((width (window-total-width left))
              (right (magit-main-window-test--side claude 'right 25)))
          (with-current-buffer magit
            (wamei/magit-restore-main-window))
          (should (window-live-p left))
          (should (= (window-total-width left) width))
          (should (window-live-p right))
          (should (eq (window-buffer right) claude))
          (should (equal (magit-main-window-test--main-buffers) '("a"))))))))

(ert-deftest wamei/magit-main-window-buries-without-saved-state ()
  "保存が無ければ (別の magit バッファから開いた場合など) その window を閉じるだけ。"
  (magit-main-window-test--with-frame
    (let ((a (magit-main-window-test--buffer "a"))
          (magit (magit-main-window-test--buffer "magit: x")))
      (setq buffers (list a magit))
      (switch-to-buffer a)
      (switch-to-buffer magit)
      (with-current-buffer magit
        (wamei/magit-restore-main-window))
      (should (equal (magit-main-window-test--main-buffers) '("a"))))))

(ert-deftest wamei/magit-main-window-kill-buffer-argument ()
  "KILL-BUFFER が非 nil ならバッファを消す (magit の bury 関数の約束)。"
  (magit-main-window-test--with-frame
    (let ((a (magit-main-window-test--buffer "a"))
          (magit (magit-main-window-test--buffer "magit: x")))
      (setq buffers (list a magit))
      (switch-to-buffer a)
      (magit-main-window-test--open magit)
      (with-current-buffer magit
        (wamei/magit-restore-main-window t))
      (should-not (buffer-live-p magit))
      (should (equal (magit-main-window-test--main-buffers) '("a"))))))

;;; 保存

(ert-deftest wamei/magit-main-window-save-skips-a-buffer-already-shown ()
  "そのバッファが既に出ているなら保存しない (magit の保存と同じ条件)。
出ているバッファを出し直すたびに上書きすると、戻る先が magit 自身になる。"
  (magit-main-window-test--with-frame
    (let ((magit (magit-main-window-test--buffer "magit: x")))
      (setq buffers (list magit))
      (switch-to-buffer magit)
      (with-current-buffer magit
        (wamei/magit-save-main-window)
        (should-not wamei/magit--main-window-state)))))

(ert-deftest wamei/magit-main-window-save-respects-inhibit ()
  "`magit-inhibit-save-previous-winconf' が unset なら保存を消す (magit と同じ)。"
  (magit-main-window-test--with-frame
    (let ((magit (magit-main-window-test--buffer "magit: x")))
      (setq buffers (list magit))
      (with-current-buffer magit
        (wamei/magit-save-main-window)
        (should wamei/magit--main-window-state)
        (let ((magit-inhibit-save-previous-winconf 'unset))
          (wamei/magit-save-main-window))
        (should-not wamei/magit--main-window-state)))))

(provide 'magit-main-window-test)
;;; magit-main-window-test.el ends here
