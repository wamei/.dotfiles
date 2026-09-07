;;; term-input.el --- vterm への入力を Emacs 側の操作と結びつける -*- lexical-binding: t; -*-

;;; Commentary:

;; vterm は 1 バイトのキーをほぼ全て `vterm--self-insert' で端末へ流すだけなので、
;; Emacs 側の kill-ring やマウスイベントとは結びつかない。ここでは次の 2 つを補う。
;;
;; - `wamei/term-input-kill-line'
;;   C-k を送る前に point から行末までを kill-ring に入れる。行の削除自体は
;;   従来どおりシェル (zsh の ZLE) が行うので、シェル側の挙動は変わらない。
;;   画面幅を超えて折り返した入力行は、見えている行末までしか拾えない。
;;
;; - `wamei/term-input-mouse-mode'
;;   マウスホイールを SGR (DECSET 1006) 形式のマウス報告として端末へ送る。
;;   vterm はホイールを pty へ渡さないため、alt-screen で動く TUI (Claude Code の
;;   fullscreen 表示など) の内蔵スクロールが効かない。マウス追跡を有効にしていない
;;   プログラムには文字列として届いてしまうので、常時ではなく該当バッファでだけ有効にする。
;;   `vterm-copy-mode' 中は Emacs の通常スクロールに任せる。
;;
;; - `wamei/term-input-paste-mode'
;;   Cmd+V でクリップボードの画像を端末のプログラムに渡す。macOS の Cmd+V は
;;   `vterm-yank' (kill-ring) に割り当ててあるためテキストしか送れず、画像を
;;   コピーしても何も起きない。Claude Code は C-v を受けると自分で osascript を
;;   叩いて macOS のクリップボードから画像を取り出すので、Emacs 側で画像を運ぶ
;;   必要はなく、キーだけ端末へ流せばよい。シェルでは C-v が quoted-insert に
;;   なって固まるので、常時ではなく該当バッファでだけ有効にする。

;;; Code:

(require 'mwheel)
(require 'seq)

(declare-function vterm-send-key "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-yank "vterm")
(defvar vterm-copy-mode)
(defvar vterm-timer-delay)

;;; kill-line

(defun wamei/term-input--line-rest ()
  "point から行末までの文字列。vterm が埋める行末の空白は落とす。"
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
  (vterm-send-key "k" nil nil t))

;;; マウスホイール転送

(defun wamei/term-input--sgr-mouse (button col row)
  "SGR 形式のマウス報告を組み立てる。COL / ROW は 0 始まりで受け取り 1 始まりで載せる。"
  (format "\e[<%d;%d;%dM" button (1+ col) (1+ row)))

(defun wamei/term-input--wheel-button (type)
  "イベント種別 TYPE に対応する xterm のホイールボタン番号。ホイール以外は nil。"
  (pcase (event-basic-type type)
    ((or 'wheel-up 'mouse-4) 64)
    ((or 'wheel-down 'mouse-5) 65)))

(defun wamei/term-input-forward-wheel (event)
  "ホイール EVENT をイベント位置の SGR マウス報告として端末へ送る。
送り先はポインタの下の window の端末なので、その window が選択されていなくても効く。
`vterm-copy-mode' 中は `mwheel-scroll' に委譲する。"
  (interactive "e")
  ;; マウスイベントのキー引きはポインタ下のバッファのキーマップで行われるが、
  ;; コマンド実行時の `current-buffer' は選択中の window のバッファになる。
  ;; `vterm--term' と `vterm--process' はバッファローカルなので、current-buffer の
  ;; まま送ると非選択の端末では `vterm-send-string' が (when vterm--term ...) で
  ;; 黙って何もしない。`mwheel-scroll' と同じくイベント側の window を基準にする。
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (buffer (if (window-live-p window)
                     (window-buffer window)
                   (current-buffer))))
    (with-current-buffer buffer
      (if (bound-and-true-p vterm-copy-mode)
          (mwheel-scroll event)
        (when-let* ((button (wamei/term-input--wheel-button (event-basic-type event)))
                    ;; actual-col-row は文字セル単位で正確だが、文字の無い場所では nil
                    (pos (or (posn-actual-col-row posn) (posn-col-row posn))))
          ;; `vterm-send-string' は末尾で
          ;;   (accept-process-output PROC vterm-timer-delay nil t)
          ;; を呼び、端末が何か返すまで最大 `vterm-timer-delay' (既定 0.1 秒)
          ;; ブロックする。TUI が末端 (履歴の一番下など) に達して再描画を返さなく
          ;; なると 1 イベントごとに満額待つため、トラックパッドの慣性で積まれた
          ;; 数百イベントが直列に効いて Emacs 全体が数十秒止まる。C-g は実行中の
          ;; 1 コマンドを止めるだけで、キューに残ったイベントが同じことを繰り返す
          ;; ので復帰しない。0 にすると届いている出力だけ読んで即座に返る
          ;; (待たないだけで送信はする)。
          (let ((vterm-timer-delay 0))
            (vterm-send-string
             (wamei/term-input--sgr-mouse button (car pos) (cdr pos)))))))))

(defvar wamei/term-input-mouse-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (type '(wheel-up wheel-down mouse-4 mouse-5))
      (dolist (prefix '("" "double-" "triple-"))
        (define-key map (vector (intern (concat prefix (symbol-name type))))
                    #'wamei/term-input-forward-wheel)))
    ;; 修飾キー付き (S-wheel など) は mwheel-scroll 経由で届くのでまとめて拾う
    (define-key map [remap mwheel-scroll] #'wamei/term-input-forward-wheel)
    map)
  "`wamei/term-input-mouse-mode' のキーマップ。")

(define-minor-mode wamei/term-input-mouse-mode
  "マウスホイールを端末へマウス報告として転送する。"
  :lighter nil
  :keymap wamei/term-input-mouse-mode-map)

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
      (vterm-send-key "v" nil nil t)
    (vterm-yank)))

(defvar wamei/term-input-paste-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-v") #'wamei/term-input-paste)
    map)
  "`wamei/term-input-paste-mode' のキーマップ。")

(define-minor-mode wamei/term-input-paste-mode
  "Cmd+V でクリップボードの画像を端末のプログラムに渡す。"
  :lighter nil
  :keymap wamei/term-input-paste-mode-map)

(provide 'term-input)
;;; term-input.el ends here
