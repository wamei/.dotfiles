;;; term-panel.el --- フレーム下部の端末パネルと端末一覧 -*- lexical-binding: t; -*-
;;; Commentary:
;; ghostel の端末をプロジェクト (タブ) ごとにまとめ、フレーム下部の side window に
;; 出す。端末が 2 つ以上あるときは右隣に一覧 (`wamei/term-list-mode') を出す。
;;
;; 端末バッファは "*term: <project>[ N]*"、一覧は "*terminals: <project>*" と
;; プロジェクト名で分ける。一覧は表示中の端末と同じプロジェクトのものを出すので、
;; タブ (プロジェクト) を切り替えても別プロジェクトの端末が並ばない。
;;
;; ghostel そのものへの結線 (display-buffer-alist、ghostel-mode-hook、
;; ghostel-buffer-name-function) は init.el の ghostel ブロックで行う。
;; テストは term-panel-test.el。
;;; Code:

(require 'project)
(require 'seq)

(defvar ghostel-shell)                  ; ghostel.el
(defvar ghostel-title)                  ; ghostel.el (buffer-local)
(declare-function ghostel-create "ghostel" (&optional name display identity))

(defvar wamei/term-height 0.3
  "端末ウィンドウの高さ (フレームに対する割合)。
手動でリサイズすると更新され、次に開くときも同じ割合になる。")

(defvar wamei/term-list-width 36
  "端末一覧ウィンドウの幅 (文字数)。
手動でリサイズすると更新され、次に開くときも同じ幅になる。")

(defconst wamei/term-list-buffer-prefix "*terminals: "
  "端末一覧のバッファ名の接頭辞。後ろにプロジェクト名が付く。
display-buffer-alist で端末本体と別扱いにするため、`*term: ' で始まらない名前にする。")

(defconst wamei/term-list-buffer-regexp
  (concat "\\`" (regexp-quote wamei/term-list-buffer-prefix))
  "端末一覧のバッファ名にマッチする正規表現。display-buffer-alist と desktop の復元で使う。")

(defvar wamei/term--previous-window nil
  "パネルへ移動する直前に選択していた window。")

(defvar wamei/term--previous-buffer nil
  "パネルへ移動する直前に選択していたバッファ。
パネルを閉じて開き直すなどで window オブジェクトは無効になりうるため、
戻り先はバッファでも覚えておく。")

(defvar wamei/term--last nil
  "最後に表示した端末バッファ。プロジェクトごとの復帰先として使う。")

;;; 高さの記憶

(defun wamei/term--set-height (window)
  "WINDOW を wamei/term-height の割合にリサイズする。
display-buffer-alist の window-height に関数として渡す。数値を直接書くと
alist 登録時の値で固定されてしまい、リサイズを覚えられない。"
  (let ((delta (- (round (* wamei/term-height (frame-height)))
                  (window-total-height window))))
    (unless (zerop delta)
      (ignore-errors (window-resize window delta nil t)))))

(defun wamei/term--set-list-width (window)
  "WINDOW を wamei/term-list-width の幅にする。
display-buffer-alist の window-width は bottom の side window では
数値を書いても効かず、変数シンボルは関数扱いで無視される。関数で明示的に
リサイズする必要がある。preserve-size は縮小前の幅で固定してしまうため使わない。"
  (let ((delta (- wamei/term-list-width (window-total-width window))))
    (unless (zerop delta)
      (ignore-errors (window-resize window delta t t)))))

(defun wamei/term--remember-height ()
  "現在の高さの割合を wamei/term-height に覚える。"
  (when-let* ((window (get-buffer-window (current-buffer))))
    (when (window-parameter window 'window-side)
      (let ((ratio (/ (float (window-total-height window)) (frame-height))))
        (when (< 0.05 ratio 0.95)
          (setq wamei/term-height ratio))))))

(defun wamei/term--remember-list-width ()
  "現在の一覧の幅を wamei/term-list-width に覚える。
一覧は端末が 1 つになると閉じ、2 つに戻ると display-buffer で作り直されるので、
window に付いた幅は残らない。変数に覚えておき wamei/term--set-list-width が
作り直すたびに当てる。幅が変わったら一覧の切り詰め幅も追従させる。"
  (when-let* ((window (get-buffer-window (current-buffer))))
    (when (window-parameter window 'window-side)
      (let ((width (window-total-width window)))
        (when (and (< 8 width (* 0.8 (frame-width)))
                   (/= width wamei/term-list-width))
          (setq wamei/term-list-width width)
          (wamei/term--list-refresh))))))

;;; 端末バッファの管理

(defun wamei/term--root ()
  "端末を開くディレクトリ。プロジェクト内ならそのルート。"
  (if-let* ((project (project-current nil)))
      (project-root project)
    default-directory))

(defun wamei/term--project-name ()
  "タブ (プロジェクト) を識別する名前。"
  (file-name-nondirectory (directory-file-name (wamei/term--root))))

(defun wamei/term--buffer-name (&optional index)
  "INDEX 番目の端末バッファ名。1 は番号なし。"
  (if (and index (> index 1))
      (format "*term: %s %d*" (wamei/term--project-name) index)
    (format "*term: %s*" (wamei/term--project-name))))

(defun wamei/term--buffer-regexp ()
  "現在のプロジェクトの端末バッファ名にマッチする正規表現。
別プロジェクトの前方一致 (foo と foobar) を拾わないよう末尾まで固定する。"
  (concat "\\`" (regexp-quote (format "*term: %s" (wamei/term--project-name)))
          "\\(?: \\([0-9]+\\)\\)?\\*\\'"))

(defun wamei/term--buffers ()
  "現在のプロジェクトの端末バッファを番号順に返す。"
  (let ((regexp (wamei/term--buffer-regexp)))
    (sort (seq-filter (lambda (buffer)
                        (string-match-p regexp (buffer-name buffer)))
                      (buffer-list))
          (lambda (a b)
            (< (wamei/term--index a) (wamei/term--index b))))))

(defun wamei/term--index (buffer)
  "BUFFER の端末番号。番号なしは 1。"
  (if (string-match (wamei/term--buffer-regexp) (buffer-name buffer))
      (string-to-number (or (match-string 1 (buffer-name buffer)) "1"))
    0))

(defun wamei/term--next-index ()
  "未使用の最小の端末番号。"
  (let ((used (mapcar #'wamei/term--index (wamei/term--buffers)))
        (index 1))
    (while (memq index used) (setq index (1+ index)))
    index))

(defun wamei/term--window ()
  "端末本体を表示している window。"
  (seq-find (lambda (window)
              (and (eq (window-parameter window 'window-side) 'bottom)
                   (eql (window-parameter window 'window-slot) 0)
                   (string-match-p "\\`\\*term: " (buffer-name (window-buffer window)))))
            (window-list nil 'no-mini)))

(defun wamei/term--current ()
  "パネルに出すべき端末バッファ。無ければ nil。"
  (let ((buffers (wamei/term--buffers)))
    (or (seq-find (lambda (b) (eq b wamei/term--last)) buffers)
        (car buffers))))

(defun wamei/term--setup-buffer ()
  "端末バッファのパネル向け設定。`ghostel-mode-hook' から呼ぶ。
非選択の window では Emacs が point の位置に中抜きカーソルを描くが、端末では
シェルのカーソル位置と重なって紛らわしいだけなので、パネルが非アクティブの
ときは出さない。高さの記憶と kill 時の後始末もここで登録する。"
  (setq-local cursor-in-non-selected-windows nil)
  (add-hook 'window-configuration-change-hook #'wamei/term--remember-height nil t)
  ;; シェル終了などでバッファが消えたらパネルと一覧を追従させる
  (add-hook 'kill-buffer-hook #'wamei/term--on-kill nil t))

(defun wamei/term--create (index)
  "INDEX 番目の端末を作って返す。
`ghostel-create' は DISPLAY を渡さなければ表示しないので、window 構成は変わらない
\(パネルへの表示は `wamei/term--show' が display-buffer で行う)。"
  (let ((default-directory (wamei/term--root)))
    (ghostel-create (wamei/term--buffer-name index))))

;;; 一覧

(defvar wamei/term-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wamei/term-list-select)
    (define-key map (kbd "d") #'wamei/term-list-kill)
    (define-key map [mouse-1] #'wamei/term-list-select)
    (define-key map (kbd "<C-tab>") #'wamei/term-next)
    (define-key map (kbd "<C-S-tab>") #'wamei/term-previous)
    map)
  "端末一覧のキーマップ。")

(define-derived-mode wamei/term-list-mode special-mode "Terminals"
  "端末一覧のメジャーモード。"
  ;; パネルが非アクティブのときは中抜きカーソルを出さない (sidebar と同じ)
  (setq-local cursor-in-non-selected-windows nil))

(defun wamei/term--list-buffer-name ()
  "現在のプロジェクトの端末一覧のバッファ名。"
  (concat wamei/term-list-buffer-prefix (wamei/term--project-name) "*"))

(defun wamei/term--list-buffer ()
  "現在のプロジェクトの端末一覧のバッファ。無ければ作る。
`default-directory' をプロジェクトルートにしておくので、一覧バッファの中で
`wamei/term--buffers' を呼んでも同じプロジェクトの端末が返る。"
  (let ((name (wamei/term--list-buffer-name))
        (root (wamei/term--root)))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (wamei/term-list-mode)
          (setq default-directory root)
          (setq-local mode-line-format nil)
          (add-hook 'window-configuration-change-hook
                    #'wamei/term--remember-list-width nil t)
          (current-buffer)))))

(defun wamei/term--list-window ()
  "端末一覧を表示している window。"
  (seq-find (lambda (window)
              (and (eq (window-parameter window 'window-side) 'bottom)
                   (string-match-p wamei/term-list-buffer-regexp
                                   (buffer-name (window-buffer window)))))
            (window-list nil 'no-mini)))

(defun wamei/term--on-title-change (_title)
  "端末のタイトルが変わったら一覧を描き直す。バッファ名は変えない。

`ghostel-buffer-name-function' に設定して使う。この変数の既定は nil
\(= 改名機構そのものが off) なので、これは「改名を抑止する」設定ではなく
「一覧の再描画という副作用のために改名機構を on にする」設定。
現在のバッファ名をそのまま返すことで `ghostel--rename-managed' の
\(not (equal new-name (buffer-name))) が偽になり、必ず no-op になる。
nil を返すとタイトルがクリアされたときだけ `ghostel--set-title' の `or' が
`ghostel--initial-name' に落ちて `ghostel--rename-managed' を呼ぶので、
\(今は no-op でも) rename の経路を武装した状態になってしまう。

呼ばれるのはタイトル変更 (OSC 0/2) のときだけではなく cd (OSC 7) のときも。
ghostel の zsh 統合は precmd ごとに OSC 7 を出すので、1 コマンドにつき
一覧の再描画が 2 回走る (旧 `vterm--set-title' advice は 1 回だった)。
そのたびに `project-current' と `buffer-list' の走査、一覧バッファの
作り直しが起きるので、ここに重い処理を足さないこと。

`ghostel--set-title' と `ghostel--set-directory' はこの関数の呼び出しを
`condition-case' で包まないため、ここで signal すると端末の出力処理
\(プロセスフィルタ) の中でエラーになる。一覧の再描画は `project-current' を
通り、消えたディレクトリや remote な `default-directory' で signal しうるので
`with-demoted-errors' で押さえる。

呼ばれた時点で `ghostel-title' は新しい値になっている。
別タブで見えていない一覧も描き直しておく (戻ったときに古いままにしない)。"
  (with-demoted-errors "端末一覧の再描画に失敗しました: %S"
    (when (and (string-prefix-p "*term: " (buffer-name))
               (get-buffer (wamei/term--list-buffer-name)))
      (wamei/term--list-refresh)))
  (buffer-name))

(defun wamei/term--label (buffer)
  "一覧に出す BUFFER の表示名。最後に実行したコマンド、無ければシェル名。
タイトルは .zshrc の preexec が OSC 0 で流し、ghostel が `ghostel-title' に入れる。
`boundp' で守るのは、この file 冒頭の `(defvar ghostel-title)' (値なし) が
symbol を special にするだけで束縛はしないため。ghostel 未ロードのまま
呼ばれると `buffer-local-value' が void-variable になる
\(term-restore.el / claude-panel.el の参照と同じ形に揃えている)。"
  (or (and (boundp 'ghostel-title) (buffer-local-value 'ghostel-title buffer))
      (file-name-nondirectory (if (boundp 'ghostel-shell) ghostel-shell shell-file-name))))

(defun wamei/term--list-refresh ()
  "現在のプロジェクトの端末一覧を描き直し、そのバッファを返す。"
  (with-current-buffer (wamei/term--list-buffer)
    (let ((inhibit-read-only t)
          (current (wamei/term--current))
          (width (max 8 (- wamei/term-list-width 2))))
      (erase-buffer)
      (dolist (buffer (wamei/term--buffers))
        (let* ((index (wamei/term--index buffer))
               (label (format "%d: %s" index
                              (truncate-string-to-width
                               (wamei/term--label buffer) width nil nil t)))
               (start (point)))
          (insert label "\n")
          (add-text-properties
           start (1- (point))
           (list 'wamei/term-buffer buffer
                 'mouse-face 'highlight
                 'keymap wamei/term-list-mode-map
                 'help-echo "mouse-1: 切り替え / d: 削除"))
          (when (eq buffer current)
            (add-face-text-property start (1- (point)) 'highlight)))))
    (goto-char (point-min))
    (current-buffer)))

(defun wamei/term--list-update ()
  "パネルの端末と同じプロジェクトの端末が 2 つ以上のときだけ一覧を表示する。

どのプロジェクトの一覧を出すかはカレントバッファではなく、パネルに表示中の
端末で決める。kill-buffer 後のタイマや別プロジェクトのバッファから呼ばれても、
タブ (window 構成) に出ている端末に一覧が追従する。"
  (let ((term-window (wamei/term--window))
        (list-window (wamei/term--list-window)))
    (if (null term-window)
        ;; パネル自体が閉じているなら一覧も出さない
        (when (window-live-p list-window) (delete-window list-window))
      (with-current-buffer (window-buffer term-window)
        (if (< (length (wamei/term--buffers)) 2)
            (when (window-live-p list-window) (delete-window list-window))
          (let ((list-buffer (wamei/term--list-refresh)))
            (cond
             ((not (window-live-p list-window))
              (display-buffer list-buffer))
             ;; 別プロジェクトの一覧が出ていれば差し替える (dedicated なので一時的に外す)
             ((not (eq (window-buffer list-window) list-buffer))
              (set-window-dedicated-p list-window nil)
              (set-window-buffer list-window list-buffer)
              (set-window-dedicated-p list-window t)))))))))

(defun wamei/term-list-select ()
  "一覧で選んだ端末に切り替える。"
  (interactive)
  (when-let* ((buffer (get-text-property (point) 'wamei/term-buffer)))
    (wamei/term--show buffer)))

(defun wamei/term-list-kill ()
  "一覧で選んだ端末を削除する。"
  (interactive)
  (when-let* ((buffer (get-text-property (point) 'wamei/term-buffer)))
    ;; 端末はプロセスが生きているため、そのままだと
    ;; process-kill-buffer-query-function が確認を求めて止まる
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))
    ;; パネルの差し替え (または最後の端末なら window の削除) は
    ;; kill-buffer 側で済んでいる。ここでは残った端末へフォーカスを移す。
    (when-let* ((next (wamei/term--current)))
      (wamei/term--show next))
    (wamei/term--list-update)))

;;; パネル操作

(defun wamei/term--hand-over ()
  "消えようとしている端末がパネルに出ていれば、別の端末に差し替える。

kill-buffer-hook から呼ぶ。パネルは dedicated な side window なので、
表示中のバッファが消えると kill-buffer が window ごと削除してしまう。
削除前に同じプロジェクトの別端末へ差し替えておけばパネルは残る。
他に端末が無ければ何もせず、従来どおりパネルは閉じる。"
  (let* ((dying (current-buffer))
         (window (wamei/term--window))
         (others (remq dying (wamei/term--buffers))))
    (when (eq dying wamei/term--last) (setq wamei/term--last nil))
    (when (and window (eq (window-buffer window) dying) others)
      ;; フォーカスは動かさない。端末内で exit した場合はパネルが
      ;; 選択されたまま次の端末に切り替わる。
      (wamei/term--show (or (car (memq wamei/term--last others)) (car others))
                        t))))

(defun wamei/term--on-kill ()
  "端末バッファが消えるときの後始末。シェル終了や kill-buffer から呼ばれる。"
  (wamei/term--hand-over)
  ;; 一覧はバッファが実際に消えた後に描き直す
  (run-at-time 0 nil #'wamei/term--list-update))

(defun wamei/term--show (buffer &optional no-select)
  "BUFFER をパネルに出す。NO-SELECT が非 nil ならフォーカスは移さない。

既に端末 window があるときは dedicated を一時的に外して差し替える。
dedicated のままだと set-window-buffer が失敗する。"
  (setq wamei/term--last buffer)
  (let ((window (wamei/term--window)))
    (if window
        (progn (set-window-dedicated-p window nil)
               (set-window-buffer window buffer)
               (set-window-dedicated-p window t))
      (setq window (display-buffer buffer)))
    (unless no-select
      (when (window-live-p window) (select-window window))))
  (wamei/term--list-update)
  (get-buffer-window buffer))

(defun wamei/term--close ()
  "パネル (端末と一覧) を閉じる。"
  (when-let* ((window (wamei/term--list-window)))
    (delete-window window))
  (when-let* ((window (wamei/term--window)))
    (delete-window window)))

(defvar wamei/term-cycle-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-tab>") #'wamei/term-next)
    (define-key map (kbd "<C-S-tab>") #'wamei/term-previous)
    ;; 端末によっては Shift-Tab が iso-lefttab として報告される
    (define-key map (kbd "<C-S-iso-lefttab>") #'wamei/term-previous)
    map)
  "端末内で tab-bar-mode の C-tab 割り当てを上書きするキーマップ。")

(defun wamei/term--cycle (offset)
  "現在の端末から OFFSET 個ずれた端末に切り替える。端は巻き戻る。"
  (let* ((buffers (wamei/term--buffers))
         (count (length buffers)))
    (when (> count 1)
      (let* ((current (wamei/term--current))
             (index (or (seq-position buffers current) 0)))
        ;; 切り替えだけを行い、フォーカスは呼び出し元に残す
        (wamei/term--show (nth (mod (+ index offset) count) buffers) t)))))

(defun wamei/term-next ()
  "次の端末に切り替える。"
  (interactive)
  (wamei/term--cycle 1))

(defun wamei/term-previous ()
  "前の端末に切り替える。"
  (interactive)
  (wamei/term--cycle -1))

(defun wamei/term-new ()
  "新しい端末を作ってパネルに出す。"
  (interactive)
  (wamei/term--show (wamei/term--create (wamei/term--next-index))))

(defun wamei/term--remember-previous ()
  "パネルへ移動する直前の window とバッファを覚える。"
  (setq wamei/term--previous-window (selected-window)
        wamei/term--previous-buffer (current-buffer)))

(defun wamei/term--back-window ()
  "パネルから戻る先の window。

記録した window が生きていればそれを使う。window が閉じられて開き直されて
いると window オブジェクトは死ぬので、そのときは同じ
バッファを表示している window を探す (claude-code-ide や sidebar の
パネルから C-z で入った場合、これが無いと無関係な window に戻ってしまう)。
どちらも無ければ直近の window。パネル自身は no-other-window なので
NO-OTHER 指定で候補から外れる。"
  (or (and (window-live-p wamei/term--previous-window)
           (eq (window-buffer wamei/term--previous-window)
               wamei/term--previous-buffer)
           wamei/term--previous-window)
      (and (buffer-live-p wamei/term--previous-buffer)
           (get-buffer-window wamei/term--previous-buffer))
      (and (window-live-p wamei/term--previous-window)
           wamei/term--previous-window)
      (get-mru-window nil t t t)))

(defun wamei/term-toggle (&optional arg)
  "端末パネルへ出入りする。

- 非表示なら開いてフォーカスする
- 表示中でフォーカスが無ければフォーカスを移す
- フォーカス中なら元の window へ戻る (パネルは開いたまま)
- ARG (C-u) 付きならパネルを閉じる

sidebar 側の move-back と同じ考え方に揃えている。"
  (interactive "P")
  (let ((window (wamei/term--window)))
    (cond
     (arg
      (wamei/term--close))
     ((and window (eq window (selected-window)))
      (let ((back (wamei/term--back-window)))
        (when (and back (not (eq back window)))
          (select-window back))))
     (window
      (wamei/term--remember-previous)
      (select-window window))
     (t
      (wamei/term--remember-previous)
      (wamei/term--show (or (wamei/term--current)
                            (wamei/term--create 1)))))))
;;; 結線

(defun wamei/term-panel-setup ()
  "端末と一覧の display-buffer-alist を登録する。

端末は下部 side window の slot 0、一覧は同じ side の slot 1 (右隣) へ。
ghostel がロードされる前に登録しておく必要があるので、init.el の :init から呼ぶ。"
  (add-to-list 'display-buffer-alist
               '("\\`\\*term: "
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . wamei/term--set-height)
                 (dedicated . t)
                 ;; no-other-window: C-x o (other-window) の巡回対象から外す。
                 ;; select-window は影響を受けないので C-z のトグルは通る。
                 ;; no-delete-other-windows: C-x 1 や magit の全画面化
                 ;; (delete-other-windows) で消えないようにする。sidebar と
                 ;; claude-code-ide は同じパラメータをパッケージ側で付けている。
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  ;; window-width は数値か関数のみ有効 (変数シンボルは関数扱いされ無視される)。
  ;; bottom の side window では数値も効かないため関数でリサイズする。
  (add-to-list 'display-buffer-alist
               `(,wamei/term-list-buffer-regexp
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 1)
                 (window-width . wamei/term--set-list-width)
                 (dedicated . t)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t))))))

(provide 'term-panel)
;;; term-panel.el ends here
