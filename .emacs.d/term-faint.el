;;; term-faint.el --- vterm に届く前に faint (SGR 2) を色へ置き換える -*- lexical-binding: t; -*-
;;; Commentary:
;; libvterm は SGR 2 (faint) を実装していない。pen.c の SGR 分岐は 0, 1, 3, 4 …
;; と続いて 2 が無く、`VTermScreenCellAttrs' にも faint のビットが無いので、
;; 端末が送ってきた薄字の指定はセルへ届く前に落ちる。結果、vterm では faint の
;; 文字が通常の前景色で描かれ、周りの本文と見分けが付かない。
;;
;; 例えば Claude Code は入力欄の推奨プロンプト (TAB で受け入れるゴーストテキスト)
;; とプレースホルダを `ESC[2m' … `ESC[22m' だけで描き、色は指定しない。tmux や
;; iTerm2 は dim を実装しているので薄く出るが、vterm では入力済みの文字と同じ色に
;; なってしまう。
;;
;; 直せるのは libvterm へ渡る前しかないので、`vterm--filter' の入力を通しながら
;; SGR を読み、faint を明示的な前景色に書き換える。閉じる側の SGR 22 は太字も
;; 閉じるため、faint が開いているときだけ「元の前景色に戻す」引数を足す。
;;
;; - `wamei/term-faint-enable'  advice を付ける
;; - `wamei/term-faint-disable' advice を外す
;; - `wamei/term-faint-color'   faint を描く色 (nil なら `shadow' の前景色)
;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; 設定

(defvar wamei/term-faint-color nil
  "faint (SGR 2) の文字を描く色。nil なら `shadow' の前景色を使う。")

(defconst wamei/term-faint--fallback-params '("38" "5" "244")
  "色を解釈できなかったときに使う SGR の前景色引数 (256 色の灰)。")

;;; 色 → SGR 引数

(defun wamei/term-faint--color-params (color)
  "COLOR を SGR の前景色引数のリストにする。解釈できなければ 256 色の灰に落とす。
COLOR は \"#RRGGBB\" か、`color-values' が解釈できる色名。"
  (let ((rgb (cond
              ((not (stringp color)) nil)
              ((string-match
                (concat "\\`#\\([0-9a-fA-F][0-9a-fA-F]\\)"
                        "\\([0-9a-fA-F][0-9a-fA-F]\\)"
                        "\\([0-9a-fA-F][0-9a-fA-F]\\)\\'")
                color)
               (list (string-to-number (match-string 1 color) 16)
                     (string-to-number (match-string 2 color) 16)
                     (string-to-number (match-string 3 color) 16)))
              (t
               ;; `color-values' は 16bit で返すので 8bit へ落とす。
               ;; batch では display が無く nil になることがある。
               (when-let* ((values (ignore-errors (color-values color))))
                 (mapcar (lambda (v) (/ v 256)) values))))))
    (if rgb
        (cons "38" (cons "2" (mapcar #'number-to-string rgb)))
      wamei/term-faint--fallback-params)))

(defvar wamei/term-faint--params nil
  "`wamei/term-faint--color-params' の結果のキャッシュ。テーマ変更で捨てる。")

(defun wamei/term-faint--params ()
  "faint を描く前景色の SGR 引数。"
  (or wamei/term-faint--params
      (setq wamei/term-faint--params
            (wamei/term-faint--color-params
             (or wamei/term-faint-color
                 (face-attribute 'shadow :foreground nil t))))))

(defun wamei/term-faint--forget-params (&rest _)
  "色のキャッシュを捨てる。`enable-theme-functions' から呼ぶ。"
  (setq wamei/term-faint--params nil))

;;; SGR の書き換え

(defconst wamei/term-faint--sgr-regexp "\e\\[\\([0-9;]*\\)m"
  "SGR (Select Graphic Rendition) の制御列。")

(defconst wamei/term-faint--partial-regexp "\e\\[?[0-9;]*\\'"
  "末尾で切れていて、続きが来れば SGR になりうる断片。")

(defconst wamei/term-faint--default-fg '("39")
  "既定の前景色を指す SGR 引数。")

(defun wamei/term-faint--initial-state ()
  "書き換えの初期状態。:faint は faint が開いているか、:fg は現在の前景色引数。"
  (list :faint nil :fg wamei/term-faint--default-fg))

(defun wamei/term-faint--foreground-p (param)
  "PARAM が単独で前景色を決める SGR 引数なら non-nil。"
  (let ((n (string-to-number param)))
    (or (= n 39) (<= 30 n 37) (<= 90 n 97))))

(defun wamei/term-faint--extended-length (params)
  "38 / 48 の直後に続く PARAMS のうち、色指定として連れている引数の個数。"
  (cond ((equal (car params) "5") 2)     ; 5;N
        ((equal (car params) "2") 4)     ; 2;R;G;B
        (t 0)))

(defun wamei/term-faint--rewrite-params (params state faint)
  "SGR の引数リスト PARAMS を書き換える。
FAINT は faint の代わりに入れる前景色引数。書き換えた引数リストと更新後の
STATE を (PARAMS . STATE) で返す。"
  (let ((faint-open (plist-get state :faint))
        (fg (plist-get state :fg))
        ;; 逆順に貯めて最後に返す。追記していくと fg と構造を共有してしまう。
        (out nil))
    (cl-flet ((emit (values) (dolist (value values) (push value out))))
      (while params
        (let ((param (pop params)))
          (cond
           ;; 拡張色は引数を連れているので、まとめて取り出す。背景 (48) の引数を
           ;; 前景色と取り違えないためにも、ここで消費しておく必要がある。
           ((member param '("38" "48"))
            (let* ((count (wamei/term-faint--extended-length params))
                   (args (seq-take params count)))
              (setq params (nthcdr count params))
              (when (equal param "38")
                (setq fg (cons param args)))
              (emit (cons param args))))
           ;; 引数なしの SGR は 0 と同じ
           ((or (equal param "0") (equal param ""))
            (setq faint-open nil
                  fg wamei/term-faint--default-fg)
            (emit (list param)))
           ((equal param "2")
            (setq faint-open t)
            (emit faint))
           ;; 22 は太字と faint の両方を閉じる。faint を色で表している間だけ、
           ;; 元の前景色に戻す引数を足す (太字だけなら色を触らない)。
           ((equal param "22")
            (emit (list param))
            (when faint-open
              (setq faint-open nil)
              (emit fg)))
           ((wamei/term-faint--foreground-p param)
            (setq fg (list param))
            (emit (list param)))
           (t (emit (list param)))))))
    (cons (nreverse out) (list :faint faint-open :fg fg))))

(defun wamei/term-faint--translate (input state faint)
  "INPUT の SGR を書き換えた文字列と、更新後の STATE を (STRING . STATE) で返す。
FAINT は faint の代わりに入れる前景色引数。"
  (let ((pos 0)
        (parts nil))
    (while (string-match wamei/term-faint--sgr-regexp input pos)
      ;; `split-string' が match-data を壊すので、位置と引数を先に取り出す
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (params (match-string 1 input)))
        (push (substring input pos beg) parts)
        (let ((rewritten (wamei/term-faint--rewrite-params
                          (split-string params ";") state faint)))
          (setq state (cdr rewritten))
          (push (concat "\e[" (string-join (car rewritten) ";") "m") parts))
        (setq pos end)))
    (push (substring input pos) parts)
    (cons (apply #'concat (nreverse parts)) state)))

(defun wamei/term-faint--split-carry (input)
  "INPUT を、いま渡せる部分と次回へ持ち越す断片に分けて (BODY . CARRY) で返す。
末尾が SGR の途中で切れていると書き換えられないので、そこで切る。CARRY が
無ければ nil。"
  (if (string-match wamei/term-faint--partial-regexp input)
      (cons (substring input 0 (match-beginning 0))
            (substring input (match-beginning 0)))
    (cons input nil)))

;;; vterm への差し込み

(defvar-local wamei/term-faint--state nil
  "このバッファの SGR 書き換え状態。")

(defvar-local wamei/term-faint--carry nil
  "次の入力の先頭へ回す、切れた制御列の断片。")

(defun wamei/term-faint--filter (fn process input)
  "PROCESS の INPUT の faint を色へ書き換えてから FN (`vterm--filter') へ渡す。"
  (let ((buffer (process-buffer process)))
    (if (not (buffer-live-p buffer))
        (funcall fn process input)
      (let ((translated
             (with-current-buffer buffer
               (let* ((input (if wamei/term-faint--carry
                                 (concat wamei/term-faint--carry input)
                               input))
                      (split (wamei/term-faint--split-carry input))
                      (rewritten (wamei/term-faint--translate
                                  (car split)
                                  (or wamei/term-faint--state
                                      (wamei/term-faint--initial-state))
                                  (wamei/term-faint--params))))
                 (setq wamei/term-faint--carry (cdr split)
                       wamei/term-faint--state (cdr rewritten))
                 (car rewritten)))))
        (funcall fn process translated)))))

(defun wamei/term-faint-enable ()
  "faint の書き換えを有効にする。"
  (advice-add 'vterm--filter :around #'wamei/term-faint--filter)
  (add-hook 'enable-theme-functions #'wamei/term-faint--forget-params))

(defun wamei/term-faint-disable ()
  "faint の書き換えを無効にする。"
  (advice-remove 'vterm--filter #'wamei/term-faint--filter)
  (remove-hook 'enable-theme-functions #'wamei/term-faint--forget-params))

(provide 'term-faint)
;;; term-faint.el ends here
