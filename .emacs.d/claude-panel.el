;;; claude-panel.el --- claude-code-ide のセッションを 1 パネル + tab-line で切り替える -*- lexical-binding: t; -*-

;;; Commentary:

;; claude-code-ide はセッションごとに別 slot の side window を横に並べる。
;; ここでは端末パネル (C-z) と同じく「1 つの side window にバッファを差し替える」
;; 方式にし、window 上部の tab-line に同じプロジェクトのセッションを並べる。
;;
;; - 表示: `claude-code-ide--display-buffer-in-side-window' の前で、選択フレームに
;;   既に Claude の window があればセッションの slot をその window の slot に揃える。
;;   `display-buffer-in-side-window' は同じ slot の window を dedicated でも再利用
;;   するので、パッケージ側の表示処理 (フォーカス・寸法同期など) はそのまま通る。
;; - 一覧: 表示後にバッファで `tab-line-mode' を有効にし、タブの中身と名前を
;;   セッションから引く。並び順は最初に表示された順 (`wamei/claude-panel--order')。
;;   パッケージのセッション一覧は順序不定なので自前で持つ。タブ名は Claude が
;;   端末タイトルで流してくる会話名を優先し、無ければセッション名 (proj:name)。
;; - 切り替え: tab-line のクリックは dedicated window では switch-to-buffer が
;;   失敗するため `tab-line-select-tab-buffer' に advice で割り込む。C-tab /
;;   C-S-tab はバッファローカルなマイナーモード
;;   (`wamei/claude-panel-keys-mode') で端末パネルの巡回を上書きする。
;; - 終了: `claude-code-ide--cleanup-session' の前で、消えるセッションがパネルに
;;   出ていれば同じプロジェクトの別セッションに差し替え、パネルを残す。後で
;;   tab-line を描き直す (バッファが消えるだけでは redisplay が走らない)。
;;   差し替えるのは side window に出ているときだけで、グリッド (claude-grid.el)
;;   の window はあちらの組み直しに任せる。

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'tab-line)
;; setf (claude-code-ide-mcp-session-window-slot ...) の setter を
;; byte-compile 時に知らせる。実行時は claude-code-ide が先に読み込む。
(eval-when-compile (require 'claude-code-ide-mcp nil t))

(declare-function claude-code-ide--buffer-session "claude-code-ide")
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide")
(declare-function claude-code-ide--session-display-name "claude-code-ide")
(declare-function claude-code-ide-mcp--sessions-for-project "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp--active-sessions "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-session-buffer "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-session-project-dir "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-session-window-slot "claude-code-ide-mcp")
(defvar claude-code-ide-focus-on-open)

;;; セッション一覧

(defvar wamei/claude-panel--order nil
  "パネルに表示されたことのある Claude バッファ。表示された順。
タブの並び順に使う。死んだバッファは一覧を取るときに除く。")

(defun wamei/claude-panel--register (buffer)
  "BUFFER を並び順の末尾に加える。既にあれば何もしない。"
  (unless (memq buffer wamei/claude-panel--order)
    (setq wamei/claude-panel--order (append wamei/claude-panel--order (list buffer)))))

(defun wamei/claude-panel--buffers (project-dir)
  "PROJECT-DIR のセッションのうち生きているバッファを表示された順で返す。
まだ並び順に無いバッファ (このモジュールを読み込む前から動いていたセッション)
は末尾に加える。加えた時点で順序は固定される。"
  (setq wamei/claude-panel--order (seq-filter #'buffer-live-p wamei/claude-panel--order))
  (let ((buffers (seq-filter #'buffer-live-p
                             (mapcar #'claude-code-ide-mcp-session-buffer
                                     (claude-code-ide-mcp--sessions-for-project project-dir)))))
    (mapc #'wamei/claude-panel--register buffers)
    (seq-filter (lambda (buffer) (memq buffer buffers)) wamei/claude-panel--order)))

(defun wamei/claude-panel--project-dir (buffer)
  "BUFFER のセッションのプロジェクトディレクトリ。セッションが無ければ nil。"
  (when-let* ((session (claude-code-ide--buffer-session buffer)))
    (claude-code-ide-mcp-session-project-dir session)))

;;; tab-line

(defun wamei/claude-panel--tabs ()
  "カレントバッファと同じプロジェクトの Claude バッファ。`tab-line-tabs-function' 用。"
  (when-let* ((project-dir (wamei/claude-panel--project-dir (current-buffer))))
    (wamei/claude-panel--buffers project-dir)))

(defconst wamei/claude-panel--title-prefix "\\`[[:space:]✳✶✻✽✢·*]+"
  "端末タイトル先頭の状態表示 (✳ など) にマッチする正規表現。タブ名からは外す。")

(defun wamei/claude-panel--clean-title (title)
  "TITLE から先頭の状態表示と前後の空白を取る。残らなければ nil。"
  (let ((name (string-trim (replace-regexp-in-string
                            wamei/claude-panel--title-prefix "" title))))
    (unless (string-empty-p name) name)))

(defvar ghostel-title)                  ; ghostel.el (buffer-local)
(declare-function wamei/term-input-paste "term-input")

(defun wamei/claude-panel--title (buffer)
  "BUFFER の会話名。端末が報告したタイトルから状態表示を外したもの。
ghostel は OSC 0/2 のタイトルを `ghostel-title' に入れる。claude-code-ide は
Claude のバッファでバッファ名の自動リネームを切るが、`ghostel-title' 自体は
設定されるので値は読める。
`boundp' で守るのは、上の `(defvar ghostel-title)' (値なし) が symbol を
special にするだけで束縛はしないため。ghostel 未ロードのまま呼ばれると
`buffer-local-value' が void-variable になる
\(term-panel.el / term-restore.el の参照と同じ形に揃えている)。"
  (when-let* (((boundp 'ghostel-title))
              (title (buffer-local-value 'ghostel-title buffer)))
    (wamei/claude-panel--clean-title title)))

(defun wamei/claude-panel--tab-name (buffer &optional _buffers)
  "BUFFER のタブ名。Claude の会話名、無ければセッション名 (proj または proj:name)。"
  (or (wamei/claude-panel--title buffer)
      (if-let* ((session (claude-code-ide--buffer-session buffer)))
          (claude-code-ide--session-display-name session)
        (buffer-name buffer))))

(defun wamei/claude-panel--cache-key (tabs)
  "tab-line のキャッシュキー。会話名の変化でも描き直すよう既定のキーに加える。"
  (append (tab-line-cache-key-default tabs)
          (mapcar #'wamei/claude-panel--title tabs)))

(defvar wamei/claude-panel-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-tab>") #'wamei/claude-panel-next)
    (define-key map (kbd "<C-S-tab>") #'wamei/claude-panel-previous)
    ;; 端末によっては Shift-Tab が iso-lefttab として報告される
    (define-key map (kbd "<C-S-iso-lefttab>") #'wamei/claude-panel-previous)
    ;; Cmd+V でクリップボードの画像を Claude に渡す (term-input.el)。
    ;; シェルでは C-v が quoted-insert になるので端末パネル全体には掛けない。
    (define-key map (kbd "s-v") #'wamei/term-input-paste)
    map)
  "Claude バッファでセッションを巡回し、Cmd+V を横取りするキーマップ。
グローバルの C-tab (端末パネルの巡回) と Cmd+V をバッファ内だけ上書きする。")

(define-minor-mode wamei/claude-panel-keys-mode
  "Claude バッファで `wamei/claude-panel-map' を有効にする。

`use-local-map' で合成キーマップを重ねる方法だと ghostel に消される。
ghostel はローカルマップにキーマップ (`ghostel-semi-char-mode-map' や
`ghostel--readonly-keymap') を直接入れるので、copy mode / Emacs mode への
出入りごとに `use-local-map' で差し替えが起きる。スクロールで copy mode に
入っただけで C-tab がグローバルの端末巡回に戻り、semi-char mode に戻っても
合成キーマップは失われたままになる。

マイナーモードのキーマップはローカルマップより先に引かれるので、ghostel が
何度入れ替えても残る。char mode だけは `emulation-mode-map-alists' 経由で
更に手前に入るため、これも含めて全てのキーが端末へ行く (意図どおり)。"
  :keymap wamei/claude-panel-map)

(defun wamei/claude-panel--setup (buffer)
  "BUFFER を一覧に登録し、tab-line と巡回キーを有効にする。何度呼んでもよい。"
  (when (buffer-live-p buffer)
    (wamei/claude-panel--register buffer)
    (with-current-buffer buffer
      ;; ghostel のキーマップは全端末で共有なので触らず、巡回キーは
      ;; このバッファのマイナーモードで持つ
      (wamei/claude-panel-keys-mode 1)
      (unless (bound-and-true-p tab-line-mode)
        (setq-local tab-line-tabs-function #'wamei/claude-panel--tabs
                    tab-line-tab-name-function #'wamei/claude-panel--tab-name
                    tab-line-cache-key-function #'wamei/claude-panel--cache-key
                    tab-line-new-button-show nil
                    tab-line-close-button-show nil)
        (tab-line-mode 1)))))

;;; 表示

(defun wamei/claude-panel--window ()
  "選択フレームで Claude セッションを表示している window。
選択中の window が該当すればそれを優先する。"
  (let ((windows (seq-filter
                  (lambda (window)
                    (claude-code-ide--buffer-session (window-buffer window)))
                  (window-list nil 'no-mini))))
    (or (car (memq (selected-window) windows))
        (car windows))))

(defun wamei/claude-panel--redirect-slot (buffer)
  "BUFFER のセッションの slot を既存パネルの slot に揃える。
`claude-code-ide--display-buffer-in-side-window' の :before advice。
同じ slot なら `display-buffer-in-side-window' が既存 window を再利用する。"
  (when-let* ((window (wamei/claude-panel--window))
              (session (claude-code-ide--buffer-session buffer))
              (slot (window-parameter window 'window-slot)))
    (setf (claude-code-ide-mcp-session-window-slot session) slot)))

(defun wamei/claude-panel--after-display (buffer)
  "表示した BUFFER に tab-line を付ける。
`claude-code-ide--display-buffer-in-side-window' の :after advice。"
  (when (get-buffer-window buffer)
    (wamei/claude-panel--setup buffer)))

(defun wamei/claude-panel--show (buffer)
  "BUFFER をパネルに出す。パッケージの表示処理を経由する。"
  (claude-code-ide--display-buffer-in-side-window buffer))

;;; 切り替え

(defun wamei/claude-panel--select-tab-buffer (orig buffer &optional window)
  "tab-line のクリックで Claude バッファならパネルに差し替える。
`tab-line-select-tab-buffer' の :around advice。dedicated な window では
switch-to-buffer が失敗するので、パッケージの表示処理に回す。"
  (if (claude-code-ide--buffer-session buffer)
      (with-selected-window (or window (selected-window))
        (when-let* ((shown (wamei/claude-panel--show buffer)))
          (select-window shown)))
    (funcall orig buffer window)))

(defun wamei/claude-panel--cycle (offset)
  "カレントの Claude バッファから OFFSET 個ずれたセッションに切り替える。端は巻き戻る。"
  (let* ((buffers (wamei/claude-panel--tabs))
         (count (length buffers)))
    (when (> count 1)
      (let ((index (or (seq-position buffers (current-buffer)) 0)))
        (wamei/claude-panel--show (nth (mod (+ index offset) count) buffers))))))

(defun wamei/claude-panel-next ()
  "次の Claude セッションに切り替える。"
  (interactive)
  (wamei/claude-panel--cycle 1))

(defun wamei/claude-panel-previous ()
  "前の Claude セッションに切り替える。"
  (interactive)
  (wamei/claude-panel--cycle -1))

;;; 終了時の差し替え

(defun wamei/claude-panel--hand-over (session)
  "SESSION のバッファがパネルに出ていれば、同じプロジェクトの別セッションに差し替える。
他に無ければ何もしない (従来どおりパネルは閉じる)。フォーカスは動かさない。

差し替え先はパネル、つまり side window に限る。グリッド (claude-grid.el) の
window は side window ではなく、あちらはセッションの増減で全体を組み直す。
そこで別セッションを side window に出すと、同じ端末バッファが 2 つの window に
出て pty のサイズが競合する。"
  (let* ((dying (claude-code-ide-mcp-session-buffer session))
         (window (and (buffer-live-p dying)
                      (seq-find (lambda (window)
                                  (window-parameter window 'window-side))
                                (get-buffer-window-list dying nil 'visible))))
         (others (remq dying (wamei/claude-panel--buffers
                              (claude-code-ide-mcp-session-project-dir session)))))
    (when (and window others)
      (let ((claude-code-ide-focus-on-open nil))
        (with-selected-window window
          (wamei/claude-panel--show (car others)))))))

(defun wamei/claude-panel--before-cleanup (session &optional _buffer-dying)
  "`claude-code-ide--cleanup-session' の :before advice。"
  (wamei/claude-panel--hand-over session))

(defun wamei/claude-panel--after-cleanup (&rest _)
  "`claude-code-ide--cleanup-session' の :after advice。tab-line を描き直す。
終了したセッションのバッファは消えるが、残ったセッションの window 自体は
変わらないので redisplay が走らず古いタブが残る (kill-buffer は mode-line の
更新フラグを立てない)。全 window の mode-line 更新を要求して描き直させる。"
  (force-mode-line-update t))

;;; 有効化

(defun wamei/claude-panel-enable ()
  "advice を入れてパネル方式を有効にする。何度呼んでもよい。"
  (advice-add 'claude-code-ide--display-buffer-in-side-window
              :before #'wamei/claude-panel--redirect-slot)
  (advice-add 'claude-code-ide--display-buffer-in-side-window
              :after #'wamei/claude-panel--after-display)
  (advice-add 'tab-line-select-tab-buffer
              :around #'wamei/claude-panel--select-tab-buffer)
  (advice-add 'claude-code-ide--cleanup-session
              :before #'wamei/claude-panel--before-cleanup)
  (advice-add 'claude-code-ide--cleanup-session
              :after #'wamei/claude-panel--after-cleanup)
  ;; 読み込み前から動いているセッションにも tab-line と巡回キーを付ける
  (when (fboundp 'claude-code-ide-mcp--active-sessions)
    (dolist (session (claude-code-ide-mcp--active-sessions))
      (wamei/claude-panel--setup (claude-code-ide-mcp-session-buffer session)))))

(provide 'claude-panel)
;;; claude-panel.el ends here
