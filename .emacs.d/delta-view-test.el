;;; delta-view-test.el --- tests for delta-view -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l delta-view-test.el -f ert-run-tests-batch-and-exit
;;
;; magit は読まない (magit から何を出すかは magit のバッファが要るので、ここでは
;; 扱わない)。delta に通す部分は delta と xterm-color (ELPA) があるときだけ走る。
;;; Code:

(require 'ert)
(require 'package)
(package-initialize)
(load (expand-file-name "delta-view.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst delta-view-test--diff
  "diff --git a/f.el b/f.el
index 1..2 100644
--- a/f.el
+++ b/f.el
@@ -1,2 +1,2 @@
 (a)
-(b)
+(c)
"
  "delta に通す生の diff。")

(defmacro delta-view-test--with-delta (&rest body)
  "delta と xterm-color があるときだけ BODY を評価する。"
  (declare (indent 0))
  `(progn
     (skip-unless (and (executable-find "delta") (require 'xterm-color nil t)))
     ,@body))

(ert-deftest delta-view-side-by-side-only-when-wide ()
  "幅が `wamei/delta-view-side-by-side-min-width' 以上のときだけ 2 列にする。"
  (let ((wamei/delta-view-side-by-side-min-width 160))
    (should (member "--side-by-side" (wamei/delta-view--delta-args 160 nil)))
    (should-not (member "--side-by-side" (wamei/delta-view--delta-args 159 nil)))
    (should (equal (member "--width" (wamei/delta-view--delta-args 120 nil))
                   (list "--width" "120" "--dark")))
    (should (member "--light" (wamei/delta-view--delta-args 120 t)))))

(ert-deftest delta-view-renders-parts-with-titles ()
  "空の diff は飛ばし、残りを見出し付きで delta の表示にする。制御コードは残さない。"
  (delta-view-test--with-delta
    (with-temp-buffer
      (wamei/delta-view--render `(("Unstaged changes" . "")
                                  ("Staged changes" . ,delta-view-test--diff))
                                100)
      (let ((text (buffer-string)))
        (should-not (string-search "Unstaged changes" text))
        (should (string-prefix-p "Staged changes\n" text))
        (should (string-search "Δ f.el" text))
        (should-not (string-search "\e" text))))))

(ert-deftest delta-view-renders-side-by-side-when-wide ()
  "幅が広ければ旧と新が 1 行に並ぶ。"
  (delta-view-test--with-delta
    (let ((wamei/delta-view-side-by-side-min-width 160))
      (with-temp-buffer
        (wamei/delta-view--render `((nil . ,delta-view-test--diff)) 160)
        (goto-char (point-min))
        (should (re-search-forward "(b).*(c)" nil t))))))

(ert-deftest delta-view-says-when-empty ()
  "どの diff も空なら、その旨を出す。"
  (with-temp-buffer
    (wamei/delta-view--render '(("Unstaged changes" . "")) 100)
    (should (equal (buffer-string) "差分はありません\n"))))

(ert-deftest delta-view-extends-line-backgrounds ()
  "delta が ESC[0K で右端まで塗らせる行は、改行に :extend t の背景を載せる。"
  (with-temp-buffer
    (insert "ctx\n\e[48;2;0;40;0m+add\e[0m\e[48;2;0;40;0m\e[0K\e[0m\n")
    (let ((eols (wamei/delta-view-eol-backgrounds)))
      (should (equal eols '((2 . "#002800"))))
      (erase-buffer)
      (insert "ctx\n+add\n")
      (wamei/delta-view-extend-backgrounds eols)
      (let ((faces (mapcar (lambda (overlay) (overlay-get overlay 'face))
                           (overlays-at 9))))
        (should (equal faces '((:background "#002800" :extend t))))))))

(defun delta-view-test--sections-buffer ()
  "見出し・ファイル 2 つ・hunk 1 つを持つ delta の表示に印を付けたバッファを作る。"
  (insert (wamei/delta-view--title "Staged changes") "\n\n"
          "Δ a.el\n────\n\n─────┐\n• 1: │\n─────┘\n  1 ⋮  1 │x\n"
          "added: b.el\n────\nbody\n")
  (wamei/delta-view--mark-sections (point-min) (point-max))
  (goto-char (point-min)))

(ert-deftest delta-view-n-p-move-between-sections ()
  "n / p は見出し・ファイル・hunk のどれにでも移る。\nn は最後のセクションから末尾へ送る。p は先頭のセクションで留まる。"
  (with-temp-buffer
    (delta-view-test--sections-buffer)
    (wamei/delta-view-next-section)
    (should (looking-at "Δ a.el"))
    (wamei/delta-view-next-section)
    (should (looking-at "• 1: │"))
    (wamei/delta-view-next-section)
    (should (looking-at "added: b.el"))
    ;; 最後のセクションからは末尾まで送り、そこで留まる
    (wamei/delta-view-next-section)
    (should (eobp))
    (wamei/delta-view-next-section)
    (should (eobp))
    (wamei/delta-view-previous-section)
    (should (looking-at "added: b.el"))
    (wamei/delta-view-previous-section)
    (should (looking-at "• 1: │"))
    (wamei/delta-view-previous-section)
    (wamei/delta-view-previous-section)
    (should (looking-at "Staged changes"))
    (wamei/delta-view-previous-section)
    (should (looking-at "Staged changes"))))

(ert-deftest delta-view-m-n-m-p-move-between-files ()
  "M-n / M-p はファイルの見出しの間だけを移る。"
  (with-temp-buffer
    (delta-view-test--sections-buffer)
    (wamei/delta-view-next-file)
    (should (looking-at "Δ a.el"))
    (wamei/delta-view-next-file)
    (should (looking-at "added: b.el"))
    (wamei/delta-view-previous-file)
    (should (looking-at "Δ a.el"))))

(ert-deftest delta-view-binds-section-keys ()
  "n / p はセクション、M-n / M-p はファイルの移動。"
  (should (eq (lookup-key wamei/delta-view-mode-map "n") #'wamei/delta-view-next-section))
  (should (eq (lookup-key wamei/delta-view-mode-map "p") #'wamei/delta-view-previous-section))
  (should (eq (lookup-key wamei/delta-view-mode-map (kbd "M-n")) #'wamei/delta-view-next-file))
  (should (eq (lookup-key wamei/delta-view-mode-map (kbd "M-p")) #'wamei/delta-view-previous-file)))

(provide 'delta-view-test)
;;; delta-view-test.el ends here
