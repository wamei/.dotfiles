;;; view-vim.el --- view-mode を vim のキー配置で読む -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; 組み込みの `view-mode' は less 風のキー配置 (d/u で半ページ、w でページ
;; サイズ設定、h でヘルプ) で、vim の指と噛み合わない。ここでは
;; `view-mode-map' を vim 優先で作り直し、read-only なバッファを vim の
;; ように読めるようにする。
;;
;; - 入り口は `view-read-only'。read-only なファイルを開いたときと
;;   `C-x C-q' で read-only にしたときに自動で `view-mode' に入る
;; - 抜けるのは `q' (View-quit)、その場で編集に戻るのは `i'
;;   (View-exit-and-edit。vim の insert の指で編集可能になる)
;; - `C-f' / `C-b' / `C-e' / `C-y' には触れない。vim ではページ・行
;;   スクロールだが、Emacs 側の `forward-char' / `backward-char' /
;;   行末移動 / ヤンクを view-mode のときだけ奪うと、同じ指が
;;   バッファによって別の意味になる。ページ送りは `SPC' / `DEL'
;;   (view-mode 既定) と `C-d' / `C-u' で足りる
;; - `h' / `H' / `?' は組み込みでは `describe-mode' だが 3 つとも vim に
;;   渡した。ヘルプは `C-h m' で出す
;;
;; 既知の制限:
;;
;; - `/' `?' `n' `N' `*' `#' は view-mode 自前の検索 (`view-search') を
;;   使う。あれは前方検索を行末から始めるので、同じ行に残っている次の
;;   出現は飛ばして次の行以降を探す。vim の `*' は同じ行の次の出現にも
;;   止まるので、そこだけ挙動が違う
;; - `w' は `forward-to-word'、`b' / `e' は `backward-word' /
;;   `forward-word'。vim の単語境界 (記号のまとまりを 1 単語と見る) では
;;   なく Emacs の単語境界なので、記号の多い行では止まる位置がずれる

;;; Code:

(require 'view)

;;; 行内の移動

(defun wamei/view-vim-beginning-of-line (arg)
  "行頭へ移動する。ただし数引数の途中なら 0 を桁として扱う。

vim の `0' は行頭だが、`10j' のように数引数の 2 桁目にもなる。どちらで
あるかは打った時点の ARG (`current-prefix-arg') で決まる。"
  (interactive "P")
  (if arg
      (digit-argument arg)
    (beginning-of-line)))

;;; 行間の移動

(defun wamei/view-vim-goto-first-line (&optional arg)
  "1 行目 (ARG があれば ARG 行目) のインデント末へ移動する。vim の `gg'。"
  (interactive "P")
  (goto-char (point-min))
  (when arg
    (forward-line (1- (prefix-numeric-value arg))))
  (back-to-indentation))

(defun wamei/view-vim-goto-last-line (&optional arg)
  "最終行 (ARG があれば ARG 行目) のインデント末へ移動する。vim の `G'。"
  (interactive "P")
  (if arg
      (progn
        (goto-char (point-min))
        (forward-line (1- (prefix-numeric-value arg))))
    (goto-char (point-max))
    ;; 末尾が改行で終わるファイルでは `point-max' は本文の無い空行に乗る。
    ;; vim の G はそこではなく最後の本文行に止まるので 1 行戻す。
    (when (and (bolp) (not (bobp)))
      (forward-line -1)))
  (back-to-indentation))

;;; window 内の移動 (H / M / L)

(defun wamei/view-vim-window-top ()
  "window の先頭行へ移動する。vim の `H'。`scroll-margin' の分は空ける。"
  (interactive)
  (move-to-window-line scroll-margin)
  (back-to-indentation))

(defun wamei/view-vim-window-middle ()
  "window の中央の行へ移動する。vim の `M'。"
  (interactive)
  (move-to-window-line nil)
  (back-to-indentation))

(defun wamei/view-vim-window-bottom ()
  "window の最終行へ移動する。vim の `L'。`scroll-margin' の分は空ける。"
  (interactive)
  (move-to-window-line (- -1 scroll-margin))
  (back-to-indentation))

;;; 画面の寄せ (zz / zt / zb)

(defun wamei/view-vim-recenter-center ()
  "カーソル行を window の中央に置く。vim の `zz'。"
  (interactive)
  (recenter nil))

(defun wamei/view-vim-recenter-top ()
  "カーソル行を window の先頭に置く。vim の `zt'。"
  (interactive)
  (recenter scroll-margin))

(defun wamei/view-vim-recenter-bottom ()
  "カーソル行を window の下端に置く。vim の `zb'。"
  (interactive)
  (recenter (- -1 scroll-margin)))

;;; 対応する括弧 (%)

(defun wamei/view-vim--next-paren-position ()
  "カーソル位置から行末までで最初に見つかる括弧の位置を返す。無ければ nil。"
  (save-excursion
    (let ((eol (line-end-position)))
      (catch 'found
        (while (< (point) eol)
          (when (memq (char-syntax (char-after)) '(?\( ?\)))
            (throw 'found (point)))
          (forward-char 1))
        nil))))

(defun wamei/view-vim-match-paren ()
  "対応する括弧へ移動する。vim の `%'。

括弧の上にいないときは、vim と同じく行内の次の括弧まで進んでから飛ぶ。"
  (interactive)
  (let ((pos (wamei/view-vim--next-paren-position)))
    (unless pos
      (user-error "この行には括弧がありません"))
    (goto-char pos)
    (if (eq (char-syntax (char-after)) ?\()
        (progn
          (forward-sexp 1)
          ;; `forward-sexp' は閉じ括弧の次に止まる。vim は閉じ括弧の上。
          (backward-char 1))
      (forward-char 1)
      (backward-sexp 1))))

;;; カーソル下のシンボルの検索 (* / #)

(defun wamei/view-vim--symbol-regexp ()
  "カーソル下のシンボルに一致する正規表現を返す。シンボルが無ければエラー。"
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol
      (user-error "カーソル位置にシンボルがありません"))
    (concat "\\_<" (regexp-quote symbol) "\\_>")))

(defun wamei/view-vim-search-symbol-forward (&optional n)
  "カーソル下のシンボルを前方に N 個先まで検索する。vim の `*'。

`view-search' 経由なので、見つけた語はそのまま `n' / `N' で繰り返せる。"
  (interactive "p")
  (view-search (or n 1) (wamei/view-vim--symbol-regexp)))

(defun wamei/view-vim-search-symbol-backward (&optional n)
  "カーソル下のシンボルを後方に N 個先まで検索する。vim の `#'。"
  (interactive "p")
  (view-search (- (or n 1)) (wamei/view-vim--symbol-regexp)))

;;; キーマップ

(defvar-keymap wamei/view-vim-g-map
  :doc "view-mode の `g' プレフィクス (vim の `gg')。"
  "g" #'wamei/view-vim-goto-first-line)

(defvar-keymap wamei/view-vim-z-map
  :doc "view-mode の `z' プレフィクス (vim の `zz' / `zt' / `zb')。"
  "z" #'wamei/view-vim-recenter-center
  "t" #'wamei/view-vim-recenter-top
  "b" #'wamei/view-vim-recenter-bottom)

(defconst wamei/view-vim-bindings
  '(;; 行内の移動
    ("h" . backward-char)
    ("j" . next-line)
    ("k" . previous-line)
    ("l" . forward-char)
    ("w" . forward-to-word)
    ("b" . backward-word)
    ("e" . forward-word)
    ("0" . wamei/view-vim-beginning-of-line)
    ("^" . back-to-indentation)
    ("$" . end-of-line)
    ;; 行間の移動
    ("G" . wamei/view-vim-goto-last-line)
    ("{" . backward-paragraph)
    ("}" . forward-paragraph)
    ("%" . wamei/view-vim-match-paren)
    ;; スクロールと window 内の移動
    ("C-d" . View-scroll-half-page-forward)
    ("C-u" . View-scroll-half-page-backward)
    ("H" . wamei/view-vim-window-top)
    ("M" . wamei/view-vim-window-middle)
    ("L" . wamei/view-vim-window-bottom)
    ;; 検索
    ("/" . View-search-regexp-forward)
    ("?" . View-search-regexp-backward)
    ("n" . View-search-last-regexp-forward)
    ("N" . View-search-last-regexp-backward)
    ("*" . wamei/view-vim-search-symbol-forward)
    ("#" . wamei/view-vim-search-symbol-backward)
    ;; 出入り口
    ("i" . View-exit-and-edit))
  "`view-mode-map' に載せる vim のキー。")

(defconst wamei/view-vim-dropped-keys
  '("d" "u" "y" "o" "r" "s" "p" "\\")
  "`view-mode-map' から外す less 風のキー。

vim では別の意味 (削除・undo・ヤンク・置換など) を持つ指なので、
less のスクロールや検索が残っていると誤爆する。read-only バッファでは
どれも本来の vim の動作ができないため、再割り当てではなく外す。")

(defun wamei/view-vim--install-keys ()
  "`view-mode-map' を vim のキー配置に作り替える。"
  (dolist (key wamei/view-vim-dropped-keys)
    (keymap-unset view-mode-map key t))
  (keymap-set view-mode-map "g" wamei/view-vim-g-map)
  (keymap-set view-mode-map "z" wamei/view-vim-z-map)
  (pcase-dolist (`(,key . ,command) wamei/view-vim-bindings)
    (keymap-set view-mode-map key command)))

;;;###autoload
(defun wamei/view-vim-setup ()
  "view-mode を vim のキー配置にし、read-only バッファで自動的に入るようにする。"
  (setq view-read-only t)
  (wamei/view-vim--install-keys))

(provide 'view-vim)
;;; view-vim.el ends here
