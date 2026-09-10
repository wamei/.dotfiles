;;; term-input.el --- ghostel への入力を Emacs 側の操作と結びつける -*- lexical-binding: t; -*-

;;; Commentary:

;; ghostel は semi-char モードでほとんどのキーを端末へ流すので、Emacs 側の
;; kill-ring やクリップボードとは結びつかない。ここでは次の 3 つを補う。
;;
;; - `wamei/term-input-copy'
;;   M-w と Cmd+C。リージョンがあれば `kill-ring-save'、マークが一度も無いバッファでは
;;   error にしない。Claude Code はマウス追跡を有効にしていてドラッグ選択を自分で
;;   クリップボードへコピーする (ドラッグは ghostel が Claude へ転送する) ので、
;;   その直後に習慣で M-w を押すと Emacs 側にはマークが無く、`kill-ring-save' が
;;   "The mark is not set now" で error になり debug-on-error でデバッガが開く。
;;
;; - `wamei/term-input-kill-line'
;;   C-k を送る前に point から行末までを kill-ring に入れる。行の削除自体は
;;   従来どおりシェル (zsh の ZLE) が行うので、シェル側の挙動は変わらない。
;;   画面幅を超えて折り返した入力行は、見えている行末までしか拾えない。
;;
;; - `wamei/term-input-paste'
;;   Cmd+V でクリップボードの画像を端末のプログラムに渡す。Cmd+V を
;;   `ghostel-yank' に割り当てるとテキストしか送れず、画像をコピーしても何も
;;   起きない。Claude Code は C-v を受けると自分で osascript を叩いて macOS の
;;   クリップボードから画像を取り出すので、Emacs 側で画像を運ぶ必要はなく、
;;   キーだけ端末へ流せばよい。シェルでは C-v が quoted-insert になって固まるので、
;;   キーの束縛は Claude のバッファだけで行う (claude-panel.el)。
;;
;; マウスホイールは ghostel 本体が SGR のマウス報告として子プロセスへ転送する
;; ので、ここでは何もしない。

;;; Code:

(require 'seq)

(declare-function ghostel-send-key "ghostel")
(declare-function ghostel-yank "ghostel")

;;; kill-line

(defun wamei/term-input--line-rest ()
  "point から行末までの文字列。端末が埋める行末の空白は落とす。"
  (string-trim-right
   (buffer-substring-no-properties (point) (line-end-position))))

(defun wamei/term-input-kill-line ()
  "point から行末までを kill-ring に入れてから C-k を端末へ送る。
連続して呼ぶと Emacs の `kill-line' と同じく 1 つの kill に連結する。
行末では zsh の kill-line 同様に何も切り取らないので kill-ring も触らない。"
  (interactive)
  (let ((text (wamei/term-input--line-rest)))
    (unless (string-empty-p text)
      (if (eq last-command #'wamei/term-input-kill-line)
          (kill-append text nil)
        (kill-new text))))
  (ghostel-send-key "k" "ctrl"))

;;; コピー

(defun wamei/term-input-copy ()
  "リージョンがあれば `kill-ring-save'。マークが無ければ error にせずメッセージだけ出す。
`kill-ring-save' はマークが一度も設定されていないバッファで error を signal する。
Claude Code のパネルではドラッグが Claude へ転送され、Claude が選択を自分で
クリップボードへコピーするため、Emacs 側にマークができない。その直後の M-w は
余計な操作なので、コピー済みであることを伝えて終わる。マークがあるときは
非アクティブでも `kill-ring-save' と同じ (mark-even-if-inactive) に振る舞う。"
  (interactive)
  (if (mark t)
      (call-interactively #'kill-ring-save)
    (message "No selection to copy (a drag inside Claude Code is copied by Claude itself)")))

;;; クリップボードの画像を端末へ渡す

(defun wamei/term-input--clipboard-image-p ()
  "システムのクリップボードに画像があれば非 nil。
NS では画像をコピーすると TARGETS に image/png と image/tiff が並ぶ
\(テキストだけなら STRING)。外部プロセスを起こさずに判定できる。
選択が取れない環境 (tty など) では nil。"
  (seq-some (lambda (target)
              (string-prefix-p "image/" (symbol-name target)))
            (append (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS)) nil)))

(defun wamei/term-input-paste ()
  "クリップボードに画像があれば C-v を端末へ送り、無ければ通常の貼り付け。
Claude Code は C-v を受けると osascript で macOS のクリップボードを
«class PNGf» として読み、ファイルに書き出して添付する。画像そのものは
Emacs を経由しないので、ここで送るのはキーだけでよい。"
  (interactive)
  (if (wamei/term-input--clipboard-image-p)
      (ghostel-send-key "v" "ctrl")
    (ghostel-yank)))

;;; IME の変換中文字

(defvar ns-working-overlay)             ; nsterm.m
(defvar ghostel--cursor-char-pos)       ; ghostel.el (buffer-local)

(defun wamei/term-input--ns-keep-working-overlay ()
  "macOS の IME の変換中 overlay を端末カーソルの位置へ張り直す。
NS の変換中文字はバッファに挿入されず、`point' に置いた長さ 0 の
`ns-working-overlay' の after-string として描かれる。ghostel は 30fps で
バッファを書き換えるので、この overlay は書き換えの前後で別の位置に
取り残され、変換中の文字が端末カーソルと無関係な行に出る。
`ghostel-inhibit-redraw-functions' で変換中だけ再描画を止める手もあるが、
それだと変換の途中で IME が確定させた文字が端末へ送られても画面に出ない。
再描画は止めず、書き換えのたびにカーソルへ張り直す。"
  (let ((ov (bound-and-true-p ns-working-overlay)))
    (when (and (overlayp ov)
               (eq (overlay-buffer ov) (current-buffer))
               (bound-and-true-p ghostel--cursor-char-pos))
      (move-overlay ov ghostel--cursor-char-pos ghostel--cursor-char-pos))))

(defun wamei/term-input--ns-after-redraw (buffer &rest _)
  "BUFFER の変換中 overlay を張り直す。`ghostel--redraw-now' の :after。
再描画は引数のバッファに対して行われるが、advice はその
`with-current-buffer' の外で走るので自分でカレントにする。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (wamei/term-input--ns-keep-working-overlay))))

(provide 'term-input)
;;; term-input.el ends here
