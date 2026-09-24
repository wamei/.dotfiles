;;; term-panel.el --- フレーム下部の端末パネルと端末一覧 -*- lexical-binding: t; -*-
;;; Commentary:
;; ghostel の端末をプロジェクト (タブ) ごとにまとめ、フレーム下部の side window に
;; 出す。端末が 2 つ以上あるときは右隣に一覧 (`wamei/term-list-mode') を出す。
;; 一覧の各行は " ● ls -al" の形で、● はその端末の実行状態 (term-modeline.el)。
;;
;; どのプロジェクトの端末かはタブに紐づいたプロジェクト (project-tabs.el) で
;; 決める。カレントバッファ基準ではないので、*scratch* や claude パネルに
;; いても、そのタブの端末が出る (詳細は `wamei/term--root')。
;;
;; 端末バッファは "*term: <project>[ N]*"、一覧は "*terminals: <project>*" と
;; プロジェクト名で分ける。一覧は表示中の端末と同じプロジェクトのものを出すので、
;; タブ (プロジェクト) を切り替えても別プロジェクトの端末が並ばない。
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
;; 一覧の行頭に出す ● (実行状態) は term-modeline.el が持っている。
(require 'term-modeline)

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
  "フレーム下部の端末パネルと端末一覧。"
  :group 'tools)

(defface wamei/term-list-current-row '((t :inherit hl-line :extend t))
  "一覧で今パネルに出ている端末の行の背景。
sidebar の現在行 (`wamei/project-sidebar-current-row') と同じ作りにしてある。
ずっと出ている背景なので、マウスが乗っている間だけの `mouse-face'
\(dired と同じ `highlight') とは別にする。`highlight' は背景がテーマの
アクセント色 (doom-molokai ではオレンジ) で派手なうえ前景色 (base0) も持つ
ため、常時これだと ● の色も失われる。`hl-line' は背景色しか持たない。"
  :group 'wamei/term-panel)

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

(defun wamei/term--panel-buffer-p ()
  "カレントバッファが端末パネルのバッファ (端末本体か一覧) か。"
  (let ((name (buffer-name)))
    (or (string-prefix-p "*term: " name)
        (string-prefix-p wamei/term-list-buffer-prefix name))))

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

端末本体と一覧の中から呼ばれたときはそのバッファのプロジェクトを見る。
一覧の再描画やタイトル変更 (プロセスフィルタ) はパネルに出ている端末を
基準に動くので、ここでタブに引っぱられると別プロジェクトの一覧を描いて
しまう。タブに紐づけが無ければ従来どおりバッファ基準。

端末本体は作ったときのプロジェクト (`wamei/term--project-root') を優先する。
シェルの cd で `default-directory' が動いても、名前と一覧が別プロジェクトに
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
  ;; シェル終了などでバッファが消えたらパネルと一覧を追従させる
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

(defconst wamei/term--list-row-prefix " "
  "一覧の各行の頭に置く詰め物。
● のグリフは送り幅 8px に対して左へ 1px はみ出す (lbearing -1) ので、
行頭に置くと window の左端で欠ける。1 桁ぶん右へずらして逃がす。")

(defun wamei/term--list-line-width ()
  "一覧の 1 行に入る桁数。
window が出ていればその本文の桁数 (フリンジを除いた幅)。ちょうどこの桁数の
行は折り返さない。まだ出ていなければ、覚えている幅からフリンジのぶんを引いた
見積もり (実測でフリンジは左右 1 桁ずつ、= `window-body-width' と一致する)。

`window-max-chars-per-line' はフリンジの設定をカレントバッファから読むので、
別のバッファから呼ぶと小さすぎる値を返す (実測: 本文 34 桁の window で 30)。"
  (let ((window (wamei/term--list-window)))
    (if (window-live-p window)
        (window-body-width window)
      (max 8 (- wamei/term-list-width 2)))))

(defun wamei/term--list-refresh ()
  "現在のプロジェクトの端末一覧を描き直し、そのバッファを返す。
各行はコマンド名と、その直前の実行状態の ● (term-modeline.el) だけ。
端末番号は出さない (並び順と名前で足りる)。"
  (with-current-buffer (wamei/term--list-buffer)
    (let* ((inhibit-read-only t)
           (current (wamei/term--current))
           (head-width (+ (string-width wamei/term--list-row-prefix)
                          (string-width wamei/term-modeline-mark-string)
                          1))
           (line-width (wamei/term--list-line-width))
           (width (max 8 (- line-width head-width))))
      (erase-buffer)
      (dolist (buffer (wamei/term--buffers))
        (let* ((start (point))
               (mark-start (+ start (length wamei/term--list-row-prefix)))
               (label (format "%s%s %s"
                              wamei/term--list-row-prefix
                              (wamei/term-modeline-mark
                               (wamei/term-modeline-status buffer))
                              (truncate-string-to-width
                               (wamei/term--label buffer) width nil nil t))))
          ;; 行末まで選べるように、残りを空白で埋めてからプロパティを張る
          ;; (プロパティの無いところはクリックしても何も起きず、マウス強調も
          ;; 文字のぶんで途切れる)。
          (insert label
                  (make-string (max 0 (- line-width (string-width label))) ?\s)
                  "\n")
          (add-text-properties
           start (1- (point))
           (list 'wamei/term-buffer buffer
                 'keymap wamei/term-list-mode-map
                 'help-echo "mouse-1: 切り替え / d: 削除"))
          ;; ● の位置を覚えておく (アニメーションで face だけ差し替えるため)。
          (put-text-property mark-start (1+ mark-start) 'wamei/term-mark t)
          ;; マウス強調は dired と同じ `highlight'。行の中は ● も含めて同じ値
          ;; にし、改行では切る。`mouse-face' が光るのは「マウス位置から同じ値が
          ;; 続く範囲」なので、行の中で値を変えると 1 行が分断され、改行にも
          ;; 同じ値を張ると全行が一続きになって一度に光る。
          (put-text-property start (1- (point)) 'mouse-face 'highlight)
          ;; 今パネルに出ている端末の行。行末の改行まで掛けるのは、face の
          ;; `:extend' が効くのが改行の face だから (掛けないと背景が文字の
          ;; ぶんで途切れる)。後ろに足すのは、前景色を持つ face に差し替えた
          ;; ときでも ● の色を残すため。
          (when (eq buffer current)
            (add-face-text-property start (point) 'wamei/term-list-current-row t)))))
    (goto-char (point-min))
    (current-buffer)))

(defun wamei/term--list-mark-face (old new)
  "● に付いている OLD の 1 つ目を NEW に差し替えた face の値。
現在行の背景 (`add-face-text-property' が後ろに足したもの) は残す。
OLD が単独の色指定 (`(:foreground ...)') のときは、それ全体で 1 つの face
なので丸ごと置き換える。"
  (let ((rest (cond ((not (consp old)) nil)
                    ((keywordp (car old)) nil)
                    (t (cdr old)))))
    (if rest (cons new rest) new)))

(defun wamei/term--list-animate-marks ()
  "一覧に出ている実行中の端末の ● を、今の色に差し替える。
`wamei/term-modeline-tick-functions' から毎秒 10 回呼ばれるので、一覧を
描き直さず (`project-current' や `buffer-list' の走査を通さず)、● 1 文字の
face だけを書き換える。一覧が出ていなければ何もしない。"
  (when-let* ((window (wamei/term--list-window))
              (buffer (window-buffer window)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (modified (buffer-modified-p))
            (color (wamei/term-modeline--running-color (float-time))))
        (when color
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let ((term (get-text-property (point) 'wamei/term-buffer))
                    (mark (text-property-any (point) (line-end-position)
                                             'wamei/term-mark t)))
                (when (and mark (buffer-live-p term)
                           (eq (wamei/term-modeline-status term) 'running))
                  (put-text-property
                   mark (1+ mark) 'face
                   (wamei/term--list-mark-face
                    (get-text-property mark 'face)
                    (list :foreground color)))))
              (forward-line 1))))
        ;; テキストプロパティの変更でもバッファは modified になる。一覧は
        ;; ファイルではないので実害は無いが、印を付け替えない。
        (set-buffer-modified-p modified)))))

(defun wamei/term--on-command-state (buffer &rest _)
  "BUFFER でコマンドが始まった・終わったら一覧の ● を描き直す。
`ghostel-command-start-functions' と `-finish-functions' の両方から呼ぶ
\(終了のフックは終了ステータスも渡すので捨てる)。

タイトルは変わらないので `wamei/term--on-title-change' では拾えない。
フックには後ろから足すこと — 状態を記録する term-modeline.el の
`wamei/term-modeline--on-command-start' / `-finish' が先に走らないと、
一覧に 1 つ前の ● が出る (`wamei/term-panel-setup' で append している)。

`wamei/term--on-title-change' と同じく端末の出力処理 (プロセスフィルタ) の
中で呼ばれるので、再描画の signal は外へ漏らさない。"
  (with-demoted-errors "端末一覧の再描画に失敗しました: %S"
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (string-prefix-p "*term: " (buffer-name))
                   (get-buffer (wamei/term--list-buffer-name)))
          (wamei/term--list-refresh)))))
  nil)

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
                                       (no-delete-other-windows . t)))))
  ;; コマンドの開始・終了で一覧の ● を描き直す。append で足すのは、状態を
  ;; 記録する term-modeline.el のフックより後に走らせるため
  ;; (`wamei/term--on-command-state')。
  (add-hook 'ghostel-command-start-functions #'wamei/term--on-command-state t)
  (add-hook 'ghostel-command-finish-functions #'wamei/term--on-command-state t)
  ;; 実行中の ● の呼吸は term-modeline.el のタイマーに相乗りする。
  (add-hook 'wamei/term-modeline-tick-functions #'wamei/term--list-animate-marks))

(provide 'term-panel)
;;; term-panel.el ends here
