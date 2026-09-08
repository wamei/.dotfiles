;;; claude-grid.el --- 実行中の claude-code-ide を 1 タブに並べる -*- lexical-binding: t; -*-

;;; Commentary:

;; 普段の claude-code-ide は右の 1 パネルにセッションを差し替えて出す
;; (claude-panel.el)。ここでは逆に「今動いているセッションを一度に見る」ための
;; タブを作る。`wamei/claude-grid-tab' は専用タブへ移動し、その中を window 分割
;; してセッションを並べる。
;;
;; - 対象: 引数なしなら全プロジェクトのセッション、C-u 付きならカレント
;;   プロジェクトのセッションだけ。タブ名が別 ("Claude Code" と
;;   "Claude Code - <project>") なのでタブも別に持てる。
;; - タブ: 名前で探して使い回す。移動のたびに今のセッションで組み直すので、
;;   セッションが増減してもタブは増えない。
;; - 並べ方: 基本は左右に 1 行。1 個あたりが `wamei/claude-grid-min-window-width'
;;   を切るなら行を足してグリッドにする (Claude の TUI は幅が狭いと崩れる)。
;; - side window: 組む前に消す。同じ端末バッファを side window とグリッドの
;;   両方に出すと、pty のサイズをどちらの window に合わせるかで競合する。
;; - 並び順はセッション名 (proj または proj:name) の昇順。パッケージのセッション
;;   一覧は順序不定なので並べ替えが要る。表示履歴を見る claude-panel の tab-line
;;   と違い、同じプロジェクトが隣に来て、実行のたびに同じ位置に出る。

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'tab-bar)
(require 'claude-panel)

(declare-function claude-code-ide--buffer-session "claude-code-ide")
(declare-function claude-code-ide--get-working-directory "claude-code-ide")
(declare-function claude-code-ide--session-display-name "claude-code-ide")
(declare-function claude-code-ide-mcp--active-sessions "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp--sessions-for-project "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-session-buffer "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-session-project-dir "claude-code-ide-mcp")

(defgroup wamei/claude-grid nil
  "実行中の claude-code-ide のセッションを 1 タブに並べる。"
  :group 'convenience)

(defcustom wamei/claude-grid-min-window-width 80
  "グリッドの 1 window に確保する最小の幅 (桁)。
これを切るなら列を減らして行を足す。Claude の TUI が崩れない下限。"
  :type 'integer)

(defcustom wamei/claude-grid-min-window-height 10
  "グリッドの 1 window に確保する最小の高さ (行)。
これを切るなら行を足さない (並べきれないセッションは出さない)。"
  :type 'integer)

(defconst wamei/claude-grid-tab-name "Claude Code"
  "グリッドを置くタブの名前。プロジェクト指定時は \" - <project>\" が付く。")

;;; 対象

(defun wamei/claude-grid--project-dir ()
  "C-u のときに対象にするプロジェクト。
Claude バッファにいればそのセッションのプロジェクト (グリッドの中から
C-u を叩いたときに、見ているセッションのプロジェクトが対象になる)。
それ以外はカレントバッファの作業ディレクトリ。"
  (or (when-let* ((session (claude-code-ide--buffer-session (current-buffer))))
        (claude-code-ide-mcp-session-project-dir session))
      (claude-code-ide--get-working-directory)))

(defun wamei/claude-grid--sessions (project-dir)
  "PROJECT-DIR のセッション。nil なら動いている全セッション。"
  (if project-dir
      (claude-code-ide-mcp--sessions-for-project project-dir)
    (claude-code-ide-mcp--active-sessions)))

(defun wamei/claude-grid--buffers (&optional project-dir)
  "並べる Claude バッファ。PROJECT-DIR が nil なら全プロジェクトのセッション。
セッション名 (proj または proj:name) の昇順。同じプロジェクトが隣に並び、
セッションが増減しても残りの並びは変わらない。"
  (mapcar #'cdr
          (sort (mapcar (lambda (session)
                          (cons (claude-code-ide--session-display-name session)
                                (claude-code-ide-mcp-session-buffer session)))
                        (seq-filter (lambda (session)
                                      (buffer-live-p
                                       (claude-code-ide-mcp-session-buffer session)))
                                    (wamei/claude-grid--sessions project-dir)))
                :key #'car :lessp #'string<)))

;;; レイアウト

(defun wamei/claude-grid--layout (count width height)
  "COUNT 個を WIDTH x HEIGHT の領域に並べるときの、各行に入れる個数のリスト。

基本は 1 行に左右へ並べ、1 個あたりが `wamei/claude-grid-min-window-width' を
切るなら列を減らして行を足す。行は個数が均等になるように分ける (4 個を 3 列
まで置ける幅なら 3 + 1 ではなく 2 + 2)。

高さが足りず全部を置けないときは置ける行数までを返すので、戻り値の合計が
COUNT より少なくなることがある。COUNT が 0 なら nil。"
  (when (> count 0)
    (let* ((columns (max 1 (min count (/ width wamei/claude-grid-min-window-width))))
           (max-rows (max 1 (/ height wamei/claude-grid-min-window-height)))
           ;; 列数で割った切り上げが必要な行数
           (rows (min (/ (+ count columns -1) columns) max-rows))
           (placed (min count (* rows columns)))
           (base (/ placed rows))
           (extra (% placed rows)))
      ;; 余りは上の行から 1 個ずつ配る
      (cl-loop for row from 0 below rows
               collect (+ base (if (< row extra) 1 0))))))

;;; 組み立て

(defun wamei/claude-grid--delete-side-windows ()
  "選択フレームの side window を消す。バッファは残る。"
  (dolist (window (window-list nil 'no-mini))
    (when (and (window-live-p window)
               (window-parameter window 'window-side)
               (not (eq window (frame-root-window))))
      (delete-window window))))

(defun wamei/claude-grid--grid-window ()
  "グリッドを作る土台にする window。side window は避ける。"
  (or (seq-find (lambda (window) (not (window-parameter window 'window-side)))
                (cons (selected-window) (window-list nil 'no-mini)))
      (selected-window)))

(defun wamei/claude-grid--split (window count side)
  "WINDOW を SIDE (`below' か `right') 方向に COUNT 個へ分ける。
上または左から順に window のリストを返す。COUNT が 1 なら WINDOW だけ。"
  (let ((windows (list window)))
    (dotimes (_ (1- count))
      (push (split-window (car windows) nil side) windows))
    (nreverse windows)))

(defun wamei/claude-grid--build (buffers)
  "選択フレームを BUFFERS のグリッドにする。左上から行方向に詰める。
並べた window を同じ順で返す。高さが足りなければ並べられた分だけ返す。"
  (wamei/claude-grid--delete-side-windows)
  (let ((base (wamei/claude-grid--grid-window)))
    (delete-other-windows base)
    (let* ((rows (wamei/claude-grid--layout (length buffers)
                                            (window-total-width base)
                                            (window-total-height base)))
           (remaining buffers)
           (windows nil))
      (cl-loop for count in rows
               for row in (wamei/claude-grid--split base (length rows) 'below)
               do (dolist (window (wamei/claude-grid--split row count 'right))
                    (set-window-buffer window (pop remaining))
                    (push window windows)))
      (setq windows (nreverse windows))
      (balance-windows)
      (dolist (window windows)
        (wamei/claude-panel--setup (window-buffer window)))
      (when windows
        (select-window (car windows)))
      windows)))

;;; タブ

(defun wamei/claude-grid--tab-name (&optional project-dir)
  "グリッドを置くタブの名前。PROJECT-DIR があればその名前を付ける。"
  (if project-dir
      (format "%s - %s" wamei/claude-grid-tab-name
              (file-name-nondirectory (directory-file-name project-dir)))
    wamei/claude-grid-tab-name))

(defun wamei/claude-grid--select-tab (name)
  "NAME のタブへ移動する。無ければ作って NAME を付ける。
`tab-rename' で付けた名前は explicit-name が立つので、`project-tabs' の
タブ名固定 (プロジェクト名への書き換え) は走らない。"
  (if-let* ((index (seq-position (funcall tab-bar-tabs-function) nil
                                 (lambda (tab _)
                                   (equal (alist-get 'name tab) name)))))
      (tab-bar-select-tab (1+ index))
    (tab-new)
    (tab-rename name)))

;;; コマンド

(defun wamei/claude-grid-tab (&optional arg)
  "実行中の claude-code-ide のバッファを分割して並べたタブへ移動する。

引数なしでは全プロジェクトのセッションを \"Claude Code\" タブに並べる。
ARG (C-u) 付きではカレントプロジェクト (Claude バッファにいればその
セッションのプロジェクト) のセッションだけを \"Claude Code - <project>\"
タブに並べる。

タブは名前で使い回し、移動のたびに今のセッションで組み直す。
セッションが無ければ何もしない。"
  (interactive "P")
  ;; 自前のコマンドなのでパッケージの autoload は効かない
  (require 'claude-code-ide)
  (let* ((project-dir (and arg (wamei/claude-grid--project-dir)))
         (buffers (wamei/claude-grid--buffers project-dir)))
    (if (null buffers)
        (message "実行中の Claude セッションがありません")
      (wamei/claude-grid--select-tab (wamei/claude-grid--tab-name project-dir))
      (let ((windows (wamei/claude-grid--build buffers)))
        (when (< (length windows) (length buffers))
          (message "高さが足りないため %d 個のうち %d 個だけ並べました"
                   (length buffers) (length windows)))))))

(provide 'claude-grid)
;;; claude-grid.el ends here
