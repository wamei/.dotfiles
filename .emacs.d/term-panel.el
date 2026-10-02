;;; term-panel.el --- フレーム下部の端末パネルと端末タブ -*- lexical-binding: t; -*-
;;; Commentary:
;; ghostel の端末をプロジェクト (タブ) ごとにまとめ、フレーム下部の side window に
;; 出す。端末が 2 つ以上あるときは端末 window の上端 (header-line) にタブを並べる。
;; タブは window の幅を等分し、" ● ls -al" の形で、● はその端末の実行状態
;; (term-modeline.el)。
;;
;; どのプロジェクトの端末かはタブに紐づいたプロジェクト (project-tabs.el) で
;; 決める。カレントバッファ基準ではないので、*scratch* や claude パネルに
;; いても、そのタブの端末が出る (詳細は `wamei/term--root')。
;;
;; 端末バッファは "*term: <project>[ N]*" とプロジェクト名で分ける。端末タブは
;; 表示中の端末と同じプロジェクトのものを出すので、タブ (プロジェクト) を
;; 切り替えても別プロジェクトの端末が並ばない。
;;
;; 端末は作ったときのプロジェクト (`wamei/term--project-root') を覚えていて、
;; シェルがプロジェクトの外へ cd しても `default-directory' はルートに留める
;; (`wamei/term--keep-in-project')。所属を `default-directory' で決める
;; project-buffers や consult のプロジェクトバッファから消えないように。
;;
;; ghostel そのものへの結線 (display-buffer-alist、ghostel-mode-hook、
;; ghostel-buffer-name-function) は init.el の ghostel ブロックで行う。
;; テストは term-panel-test.el。
;;; Code:

(require 'project)
(require 'seq)
;; 端末タブに出す ● (実行状態) は term-modeline.el が持っている。
(require 'term-modeline)
;; 端末タブは header-line に等分のタブを描く部品で描く。
(require 'header-tabs)

(defvar ghostel-shell)                  ; ghostel.el
(defvar ghostel-title)                  ; ghostel.el (buffer-local)
(declare-function ghostel-create "ghostel" (&optional name display identity))
(declare-function ghostel--buffer-identification-update "ghostel" ())
(declare-function wamei/project-tabs-current-root "project-tabs" (&optional frame))

;; `ghostel-create' には ghostel 側に autoload cookie が無いので自分で張る。
;; このファイルは init.el の leaf ghostel ブロックの `:preface' で読まれ、
;; ghostel 本体はまだロードされていない。leaf の `:bind' も autoload を張るが
;; 対象は `wamei/term-toggle' で、その defun (このファイル) が上書きしてしまう
;; ので、C-z では ghostel はロードされない。これが無いと ghostel をまだ
;; 読んでいないセッション (desktop に端末が無く claude-code-ide も起動して
;; いない) の最初の C-z が void-function ghostel-create で落ちる。
(autoload 'ghostel-create "ghostel")

(defvar wamei/term-height 0.3
  "端末ウィンドウの高さ (フレームに対する割合)。
手動でリサイズすると更新され、次に開くときも同じ割合になる。")

(defgroup wamei/term-panel nil
  "フレーム下部の端末パネルと端末タブ。"
  :group 'tools)

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

(defun wamei/term--remember-height ()
  "現在の高さの割合を wamei/term-height に覚える。"
  (when-let* ((window (get-buffer-window (current-buffer))))
    (when (window-parameter window 'window-side)
      (let ((ratio (/ (float (window-total-height window)) (frame-height))))
        (when (< 0.05 ratio 0.95)
          (setq wamei/term-height ratio))))))

;;; 端末バッファの管理

(defun wamei/term--panel-buffer-p ()
  "カレントバッファが端末パネルの端末か。"
  (string-prefix-p "*term: " (buffer-name)))

(defvar-local wamei/term--project-root nil
  "端末バッファが属するプロジェクトのルート。パネルの端末以外では nil。
`wamei/term--create' と `wamei/term--setup-buffer' (desktop で復元した端末) が
入れる。モードを掛け直しても消えないよう permanent-local にしてある。")
(put 'wamei/term--project-root 'permanent-local t)

(defun wamei/term--tab-root ()
  "カレントタブに紐づいたプロジェクトルート。無ければ nil。

紐づけは project-tabs.el が行う。init.el はこのファイルを ghostel ブロックで、
project-tabs.el を後の tab-bar ブロックで読むので `fboundp' で守る
\(端末が動くのは init を読み終えた後なので、実際には常に定義済み)。"
  (and (fboundp 'wamei/project-tabs-current-root)
       (wamei/project-tabs-current-root)))

(defun wamei/term--root ()
  "端末を開くディレクトリ。

タブ 1 つにプロジェクト 1 つの運用なので、パネルの外から呼ばれたとき
\(C-z / C-S-z / C-tab など) はカレントバッファではなくタブに紐づいた
プロジェクトを起点にする。プロジェクト外の *scratch* や claude パネル、
別プロジェクトのファイルにいても、そのタブの端末が出る。

端末の中から呼ばれたときはそのバッファのプロジェクトを見る。
端末タブの描画 (header-line) やタイトル変更 (プロセスフィルタ) は端末を
基準に動くので、ここでタブに引っぱられると別プロジェクトのタブを描いて
しまう。タブに紐づけが無ければ従来どおりバッファ基準。

端末本体は作ったときのプロジェクト (`wamei/term--project-root') を優先する。
シェルの cd で `default-directory' が動いても、名前と端末タブが別プロジェクトに
すり替わらないように。"
  (or (and (not (wamei/term--panel-buffer-p)) (wamei/term--tab-root))
      wamei/term--project-root
      (if-let* ((project (project-current nil)))
          (project-root project)
        default-directory)))

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
  (when-let* (((not wamei/term--project-root))
              (name (wamei/term--name-project (buffer-name))))
    (setq wamei/term--project-root (wamei/term--locate-root default-directory name)))
  (add-hook 'window-configuration-change-hook #'wamei/term--remember-height nil t)
  ;; シェル終了などでバッファが消えたらパネルと端末タブを追従させる
  (add-hook 'kill-buffer-hook #'wamei/term--on-kill nil t))

(defun wamei/term--create (index)
  "INDEX 番目の端末を作って返す。
`ghostel-create' は DISPLAY を渡さなければ表示しないので、window 構成は変わらない
\(パネルへの表示は `wamei/term--show' が display-buffer で行う)。"
  (let* ((root (wamei/term--root))
         (default-directory root))
    (with-current-buffer (ghostel-create (wamei/term--buffer-name index))
      (setq wamei/term--project-root root)
      (current-buffer))))

(defun wamei/term--name-project (name)
  "端末バッファ名 NAME (\"*term: <project>[ N]*\") のプロジェクト名。違えば nil。"
  (when (string-match "\\`\\*term: \\(.+?\\)\\(?: [0-9]+\\)?\\*\\'" name)
    (match-string 1 name)))

(defun wamei/term--locate-root (dir name)
  "DIR から上へたどって、名前が NAME のプロジェクトのルートを返す。無ければ nil。
desktop で復元した端末は保存時の作業ディレクトリで起動するので、そこが
プロジェクト内の別リポジトリ (サブモジュールなど) だと `project-current'
だけでは内側を拾ってしまう。バッファ名のプロジェクトを手がかりに外側を探す。
既にプロジェクトの外にいる (この仕組みより前に cd した端末) と見つからない。
そのときは別のプロジェクトを覚えて名前とずれるより、nil で従来どおりにする。"
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (catch 'found
      (while-let ((project (project-current nil dir)))
        (let* ((root (file-name-as-directory (expand-file-name (project-root project))))
               (parent (file-name-directory (directory-file-name root))))
          (when (equal (file-name-nondirectory (directory-file-name root)) name)
            (throw 'found root))
          ;; ルート (/) まで来たら打ち切る
          (when (equal parent root) (throw 'found nil))
          (setq dir parent))))))

(defun wamei/term--keep-in-project (orig dir)
  "`ghostel--update-directory' (ORIG) の :around advice。
パネルの端末がプロジェクトの外へ cd したら、`default-directory' を
プロジェクトルートに留める。project.el の `project-buffers' も consult の
プロジェクトバッファも `default-directory' の前方一致で所属を決めるので、
外に出すと端末がプロジェクトから消える。プロジェクト内の cd はそのまま追う。"
  (funcall orig dir)
  (when-let* ((root wamei/term--project-root)
              ((not (string-prefix-p root (expand-file-name default-directory)))))
    (setq default-directory root
          list-buffers-directory root)
    (when (fboundp 'ghostel--buffer-identification-update)
      (ghostel--buffer-identification-update))))

;;; タブ (header-line)

;; 端末が 2 つ以上あるとき、端末 window の header-line に同じプロジェクトの
;; 端末をタブで並べる。タブは window の幅を等分する。
;;
;; header-line は端末バッファごとの変数なので、同じプロジェクトの端末すべてに
;; 同じ `:eval' を入れておく (`wamei/term--tabs-sync')。どの端末をパネルに
;; 出しても、そのバッファのプロジェクトのタブが描かれる。中身は描くたびに
;; 作り直すので、タイトルや ● が変わったときは redisplay を促すだけでよい
;; (`wamei/term--tabs-redraw')。

(defvar wamei/term-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'wamei/term-tab-select)
    (define-key map [header-line mouse-2] #'wamei/term-tab-kill)
    map)
  "端末タブの上でのマウス操作。")

(defconst wamei/term--tabs-header-line '(:eval (wamei/term--tabs-format))
  "端末バッファに入れる `header-line-format'。")

(defun wamei/term--label (buffer)
  "タブに出す BUFFER の表示名。最後に実行したコマンド、無ければシェル名。
タイトルは .zshrc の preexec が OSC 0 で流し、ghostel が `ghostel-title' に入れる。
`boundp' で守るのは、この file 冒頭の `(defvar ghostel-title)' (値なし) が
symbol を special にするだけで束縛はしないため。ghostel 未ロードのまま
呼ばれると `buffer-local-value' が void-variable になる
\(term-restore.el / claude-panel.el の参照と同じ形に揃えている)。"
  (or (and (boundp 'ghostel-title) (buffer-local-value 'ghostel-title buffer))
      (file-name-nondirectory (if (boundp 'ghostel-shell) ghostel-shell shell-file-name))))

(defun wamei/term--tabs (buffers current)
  "BUFFERS のタブ (`wamei/header-tabs-render' の形)。CURRENT は今出ている端末。
各タブは \" ● ラベル\"。● はその端末の実行状態。"
  (mapcar (lambda (buffer)
            (list :head (concat (wamei/term-modeline-mark
                                 (wamei/term-modeline-status buffer))
                                " ")
                  :label (wamei/term--label buffer)
                  :current (eq buffer current)
                  :properties (list 'wamei/term-buffer buffer
                                    'local-map wamei/term-tab-map
                                    'help-echo "mouse-1: 切り替え / mouse-2: 削除")))
          buffers))

(defun wamei/term--tabs-string (buffers current width pixel-width &optional pixel-offset)
  "BUFFERS のタブを header-line 1 行にする。CURRENT は今出ている端末。
幅の扱いは `wamei/header-tabs-render' を参照。"
  (wamei/header-tabs-render (wamei/term--tabs buffers current)
                            width pixel-width pixel-offset))

(defun wamei/term--tabs-format ()
  "端末の header-line の中身。`wamei/term--tabs-header-line' から呼ぶ。
header-line はそのバッファの中で評価されるので、並ぶのはその端末の
プロジェクトの端末。"
  (wamei/header-tabs-format (wamei/term--tabs (wamei/term--buffers) (current-buffer))))

(defun wamei/term--tabs-sync (buffers)
  "BUFFERS (同じプロジェクトの端末) の header-line を揃える。
2 つ以上ならタブを出し、1 つなら消す。値が変わるときだけ書く
\(`header-line-format' の有無で本文の高さが変わり、端末が作り直される)。"
  (let ((format (and (cdr buffers) wamei/term--tabs-header-line)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (unless (equal header-line-format format)
          (setq header-line-format format))))))

(defun wamei/term--tabs-update ()
  "パネルに出ている端末のプロジェクトのタブを揃える。
どのプロジェクトかはカレントバッファではなく、パネルに表示中の端末で決める。"
  (when-let* ((window (wamei/term--window)))
    (with-current-buffer (window-buffer window)
      (wamei/term--tabs-sync (wamei/term--buffers)))))

(defun wamei/term--tabs-redraw ()
  "パネルのタブを描き直させる。中身は描くたびに作るので redisplay を促すだけ。"
  (when-let* ((window (wamei/term--window)))
    (with-current-buffer (window-buffer window)
      (force-mode-line-update))))

(defun wamei/term--tabs-tick ()
  "実行中の ● の色を進める。`wamei/term-modeline-tick-functions' から毎秒 10 回
呼ばれる (tick は何かが実行中のときだけ回っている)。タブが出ていなければ何もしない。"
  (when-let* ((window (wamei/term--window)))
    (when (buffer-local-value 'header-line-format (window-buffer window))
      (wamei/term--tabs-redraw))))

(defun wamei/term--on-title-change (_title)
  "端末のタイトルが変わったらタブを描き直す。バッファ名は変えない。

`ghostel-buffer-name-function' に設定して使う。この変数の既定は nil
\(= 改名機構そのものが off) なので、これは「改名を抑止する」設定ではなく
「タブの再描画という副作用のために改名機構を on にする」設定。
現在のバッファ名をそのまま返すことで `ghostel--rename-managed' の
\(not (equal new-name (buffer-name))) が偽になり、必ず no-op になる。
nil を返すとタイトルがクリアされたときだけ `ghostel--set-title' の `or' が
`ghostel--initial-name' に落ちて `ghostel--rename-managed' を呼ぶので、
\(今は no-op でも) rename の経路を武装した状態になってしまう。

呼ばれるのはタイトル変更 (OSC 0/2) のときだけではなく cd (OSC 7) のときも。
ghostel の zsh 統合は precmd ごとに OSC 7 を出すので、1 コマンドにつき 2 回
走る。ここに重い処理を足さないこと。

`ghostel--set-title' と `ghostel--set-directory' はこの関数の呼び出しを
`condition-case' で包まないため、ここで signal すると端末の出力処理
\(プロセスフィルタ) の中でエラーになる。`with-demoted-errors' で押さえる。"
  (with-demoted-errors "端末タブの再描画に失敗しました: %S"
    (when (string-prefix-p "*term: " (buffer-name))
      (wamei/term--tabs-redraw)))
  (buffer-name))

(defun wamei/term--on-command-state (buffer &rest _)
  "BUFFER でコマンドが始まった・終わったらタブの ● を描き直す。
`ghostel-command-start-functions' と `-finish-functions' の両方から呼ぶ
\(終了のフックは終了ステータスも渡すので捨てる)。

タイトルは変わらないので `wamei/term--on-title-change' では拾えない。
フックには後ろから足すこと — 状態を記録する term-modeline.el の
`wamei/term-modeline--on-command-start' / `-finish' が先に走らないと、
タブに 1 つ前の ● が出る (`wamei/term-panel-setup' で append している)。

`wamei/term--on-title-change' と同じく端末の出力処理 (プロセスフィルタ) の
中で呼ばれるので、再描画の signal は外へ漏らさない。"
  (with-demoted-errors "端末タブの再描画に失敗しました: %S"
    (when (and (buffer-live-p buffer)
               (string-prefix-p "*term: " (buffer-name buffer)))
      (wamei/term--tabs-redraw)))
  nil)

(defun wamei/term--tab-buffer (event)
  "EVENT がクリックしたタブの端末バッファ。"
  (wamei/header-tabs-event-property event 'wamei/term-buffer))

(defun wamei/term-tab-select (event)
  "クリックしたタブの端末に切り替える。"
  (interactive "e")
  (when-let* ((buffer (wamei/term--tab-buffer event)))
    (wamei/term--show buffer)))

(defun wamei/term-tab-kill (event)
  "クリックしたタブの端末を削除する。"
  (interactive "e")
  (when-let* ((buffer (wamei/term--tab-buffer event)))
    ;; 端末はプロセスが生きているため、そのままだと
    ;; process-kill-buffer-query-function が確認を求めて止まる
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

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
  ;; 消えるバッファを除いた残りでタブを揃える (1 つに戻ればタブを消す)
  (wamei/term--tabs-sync (remq (current-buffer) (wamei/term--buffers))))

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
  (wamei/term--tabs-update)
  (get-buffer-window buffer))

(defun wamei/term--close ()
  "パネルを閉じる。"
  (when-let* ((window (wamei/term--window)))
    (delete-window window)))

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
;;; 下端揃えの端数

;; ghostel は端末グリッドをウィンドウの下端に揃える (`ghostel--anchor-window')。
;; ウィンドウの本文高さが行高で割り切れないぶんは `window-vscroll' として払われ、
;; 先頭行がその端数ぶん切れた状態になる。切れていること自体は問題ないが、
;; **端数が動くと端末の中身が丸ごと数 px 上下する**。
;;
;; 動く経路は 2 つある。
;;
;; 1. mode-line の高さが変わって本文高さが変わる。スピナーの出入りで起きるので、
;;    mode-line 側で高さを固定してある (term-modeline.el の「mode-line の高さの
;;    固定」)。
;; 2. ghostel が端末カーソルの行を切らないよう、カーソルが最上行に来たフレーム
;;    だけ vscroll を 0 にする。画面を毎フレーム上から描き直す TUI では
;;    カーソルが最上行を通るたびに端数が出入りして画面が跳ねる。
;;
;; 2 をここで潰す。下端揃えができているウィンドウなら、カーソルが最上行に来ても
;; 端数を保つ (カーソル行の上端が少し切れるが、揺れないことを取る)。ウィンドウが
;; カーソル行にクランプされているとき (start が下端揃えの位置と違うとき) は
;; ghostel の判断どおり 0 のままにする。そこで端数を払うとカーソル行が本当に
;; 見えなくなる。

(declare-function ghostel--pixel-anchor "ghostel" (window target))

(defun wamei/term-anchor-vscroll (vscroll fraction start anchor)
  "ghostel が要求した VSCROLL の代わりに使う値を返す。
FRACTION はウィンドウの本文高さを行高で割った余り、START はウィンドウの
`window-start'、ANCHOR は `ghostel--pixel-anchor' の戻り値
\(START VSCROLL HEIGHT)。

VSCROLL が 0 でも、端数があり・ウィンドウが下端揃えの位置にあり・下端揃えが
端数を要求しているなら、その端数を返す。それ以外は VSCROLL をそのまま返す。"
  (if (and (eql vscroll 0)
           (not (eql fraction 0))
           anchor
           (eql start (nth 0 anchor))
           (not (eql (nth 1 anchor) 0)))
      (nth 1 anchor)
    vscroll))

(defun wamei/term--pin-anchor-vscroll (fn window vscroll &optional pixels-p preserve-p)
  "`ghostel--set-window-vscroll' の :around アドバイス。
FN に渡す VSCROLL を `wamei/term-anchor-vscroll' で差し替える。
`ghostel--pixel-anchor' の測り直しは、ghostel が 0 を要求していて端数がある
ときだけ (= カーソルが最上行に来たフレームだけ) なので、毎フレームの負荷には
ならない。"
  (let ((fraction (and pixels-p (eql vscroll 0)
                       (mod (window-body-height window t)
                            (default-line-height)))))
    (funcall fn window
             (if (and fraction (not (eql fraction 0)))
                 (wamei/term-anchor-vscroll
                  vscroll fraction (window-start window)
                  (ghostel--pixel-anchor window (point-max)))
               vscroll)
             pixels-p preserve-p)))

;;; 結線

(defun wamei/term-panel-setup ()
  "端末の display-buffer-alist と、端末タブを描き直すフックを登録する。

端末は下部 side window の slot 0 へ。
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
  ;; コマンドの開始・終了でタブの ● を描き直す。append で足すのは、状態を
  ;; 記録する term-modeline.el のフックより後に走らせるため
  ;; (`wamei/term--on-command-state')。
  (add-hook 'ghostel-command-start-functions #'wamei/term--on-command-state t)
  (add-hook 'ghostel-command-finish-functions #'wamei/term--on-command-state t)
  ;; 実行中の ● の呼吸は term-modeline.el のタイマーに相乗りする。
  (add-hook 'wamei/term-modeline-tick-functions #'wamei/term--tabs-tick))

(provide 'term-panel)
;;; term-panel.el ends here
