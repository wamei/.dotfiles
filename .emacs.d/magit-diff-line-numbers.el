;;; magit-diff-line-numbers.el --- magit の diff に行番号を出す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; magit の diff の本文行の左に、旧・新ファイルの行番号を出す
;; (" 9 10 │" の形。削除行は旧だけ、追加行は新だけ)。delta と同じく
;; 削除行の番号は赤、追加行は緑にする。
;;
;; 番号はテキストに入れず `line-prefix' テキストプロパティで見せるだけにする。
;; magit の stage は本文の行をそのまま patch にするので、テキストに番号が
;; 混ざると (delta の --line-numbers がそう) 何も stage されなくなる。
;;
;; `magit-diff-wash-diffs-hook' に入れる。フックに渡るのは magit が section に
;; 整形する前の生の git の出力で、magit は本文の行をそのまま残して section を
;; 作るので、ここで付けたプロパティは整形後も残る。magit-delta は同じフックで
;; バッファを入れ直す (プロパティが消える) ので、それより後に走らせること
;; (init.el で深さ 90 を指定している)。テストは magit-diff-line-numbers-test.el。
;;; Code:

(defface wamei/magit-diff-line-numbers
  '((t :inherit line-number))
  "magit の diff の行番号 (文脈行)。"
  :group 'magit-diff)

;; 色は magit の diffstat の +/- から取る。文字色だけの face で、magit-delta が
;; default に付け替える magit-diff-added / -removed にも入っていない。
(defface wamei/magit-diff-line-numbers-removed
  '((t :inherit (magit-diffstat-removed wamei/magit-diff-line-numbers)))
  "magit の diff の削除行の行番号。"
  :group 'magit-diff)

(defface wamei/magit-diff-line-numbers-added
  '((t :inherit (magit-diffstat-added wamei/magit-diff-line-numbers)))
  "magit の diff の追加行の行番号。"
  :group 'magit-diff)

(defconst wamei/magit-diff-line-numbers--hunk-re
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
  "通常の (combined でない) hunk の見出し。旧と新の開始行・行数を取る。")

(defun wamei/magit-diff-line-numbers--width ()
  "カレントバッファの diff の最大の行番号の桁数。hunk が無ければ nil。"
  (save-excursion
    (let ((largest nil))
      (while (re-search-forward wamei/magit-diff-line-numbers--hunk-re nil t)
        (dolist (group '((1 . 2) (3 . 4)))
          (let ((end (+ (string-to-number (match-string (car group)))
                        (string-to-number (or (match-string (cdr group)) "1")))))
            (setq largest (max end (or largest 0))))))
      (and largest (length (number-to-string largest))))))

(defun wamei/magit-diff-line-numbers--prefix (old new width face)
  "OLD と NEW (nil なら空欄) を WIDTH 桁で並べた、FACE の行頭の文字列。"
  ;; Emacs の format は %*s (幅を引数で渡す) を持たないので書式に埋め込む
  (propertize (format (format "%%%ds %%%ds │" width width) (or old "") (or new ""))
              'face face))

(defun wamei/magit-diff-line-numbers ()
  "point から後ろの diff の本文行に行番号の `line-prefix' を付ける。
`magit-diff-wash-diffs-hook' 用。point は動かさない。"
  (when-let* ((width (wamei/magit-diff-line-numbers--width)))
    (save-excursion
      (while (re-search-forward wamei/magit-diff-line-numbers--hunk-re nil t)
        (let ((old (string-to-number (match-string 1)))
              (new (string-to-number (match-string 3))))
          (forward-line)
          (while (and (not (eobp)) (memq (char-after) '(?\s ?- ?+ ?\\)))
            (let ((marker (char-after)))
              (unless (eq marker ?\\)
                (put-text-property
                 (point) (line-end-position) 'line-prefix
                 (wamei/magit-diff-line-numbers--prefix
                  (and (not (eq marker ?+)) old)
                  (and (not (eq marker ?-)) new)
                  width
                  (pcase marker
                    (?- 'wamei/magit-diff-line-numbers-removed)
                    (?+ 'wamei/magit-diff-line-numbers-added)
                    (_ 'wamei/magit-diff-line-numbers))))
                (unless (eq marker ?+) (setq old (1+ old)))
                (unless (eq marker ?-) (setq new (1+ new)))))
            (forward-line)))))))

(provide 'magit-diff-line-numbers)
;;; magit-diff-line-numbers.el ends here
