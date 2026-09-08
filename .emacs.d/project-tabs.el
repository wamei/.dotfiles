;;; project-tabs.el --- タブ = プロジェクトの運用を支える -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; タブ 1 つにプロジェクト 1 つを対応させて運用している。このファイルは
;; そのための 3 つの部品を持つ。
;;
;; 1. タブに紐づくプロジェクト (`wamei/project-tabs-root')
;;    タブの独自パラメータ wamei-project に root を持たせる。tab-bar--tab と
;;    tab-bar--current-tab-make はどちらも既知のキー以外をそのまま引き継ぐので、
;;    タブ切り替えを跨いで残る。
;;
;; 2. タブ名の決定と固定 (`wamei/tab-bar-tab-name-project' /
;;    `wamei/project-tabs-pin-name')
;;    既定の tab-bar-tab-name-current はカレントバッファ名を使うため、sidebar
;;    のような side window にフォーカスした状態だと内部バッファ名がタブ名になる。
;;    プロジェクト名を優先し、side window にいるときは直近の通常 window を見る。
;;    さらに tab-bar はタブ名を再描画のたびに再計算するので、それだけだと
;;    カレントバッファのプロジェクトが変わるたびにタブ名も変わる。タブに
;;    プロジェクトのバッファが初めて出た時点で tab-rename して explicit-name を
;;    立て、以後は名前を動かさない。このとき 1. の root も一緒に記録するので、
;;    C-x t p や desktop 復元で作られたタブも同じ経路でプロジェクトに紐づく。
;;
;; 3. project 系コマンドの起点 (`wamei/project-tabs-setup')
;;    タブに root が紐づいていれば、project 系の対話コマンドは開いている
;;    バッファに関係なくその root を起点にする。project-current は eglot や
;;    apheleia からも呼ばれるため、対象コマンドの実行中だけ差し替える。
;;
;;; Code:

(require 'project)
(require 'tab-bar)

;;; タブに紐づくプロジェクト

(defun wamei/project-tabs--normalize-root (root)
  "ROOT をタブに持たせる形 (末尾スラッシュ付きの絶対パス) にする。"
  (expand-file-name (file-name-as-directory root)))

(defun wamei/project-tabs-root (tab)
  "TAB に紐づけたプロジェクトルート。無ければ nil。"
  (alist-get 'wamei-project tab))

(defun wamei/project-tabs-current-root (&optional frame)
  "FRAME のカレントタブに紐づけたプロジェクトルート。無ければ nil。

tab-bar-tabs (や tab-bar--current-tab-find) を通すとカレントタブ名の
再計算が走り、その中で project-current が呼ばれる。この関数は
project-current の advice から呼ぶので、それでは再帰する。
独自パラメータは frame の tabs にそのまま入っているため直接読む。"
  (wamei/project-tabs-root (assq 'current-tab (frame-parameter frame 'tabs))))

(defun wamei/project-tabs-set-root (root)
  "カレントタブに ROOT を紐づける。

タブは (current-tab (KEY . VALUE) ...) という構造で先頭がシンボルのため、
setf alist-get だと局所変数へ push されるだけで実体に残らない。
保存されているリストへ直接つなぐ必要がある。"
  (tab-bar-tabs)                        ; frame の tabs パラメータを確実に用意する
  (when-let* ((tab (tab-bar--current-tab-find))
              (root (wamei/project-tabs--normalize-root root)))
    (if-let* ((cell (assq 'wamei-project (cdr tab))))
        (setcdr cell root)
      (setcdr tab (cons (cons 'wamei-project root) (cdr tab))))
    root))

;;; タブ名

(defun wamei/project-tabs-main-window ()
  "タブの本文とみなす window。
選択 window が side window (no-other-window 付き) なら直近の通常 window。
タブ名の根拠、サイドバーが従うバッファ、サイドバーからファイルを開く先に使う。"
  (if (window-parameter (selected-window) 'no-other-window)
      (or (get-mru-window nil nil t t) (selected-window))
    (selected-window)))

(defalias 'wamei/project-tabs--name-window #'wamei/project-tabs-main-window)

(defun wamei/project-tabs--main-project ()
  "タブの本文とみなす window のバッファが属するプロジェクト。無ければ nil。"
  (with-current-buffer (window-buffer (wamei/project-tabs-main-window))
    (project-current nil)))

(defun wamei/project-tabs--project-name ()
  "カレントタブが属するプロジェクトの名前。プロジェクト外なら nil。"
  (when-let* ((project (wamei/project-tabs--main-project)))
    (project-name project)))

(defun wamei/tab-bar-tab-name-project ()
  "プロジェクト名をタブ名にする。プロジェクト外ではバッファ名を使う。
`tab-bar-tab-name-function' 用。tab-bar-tabs 内でカレントタブ名の再計算に
使われ、タブバーの再描画ごとに呼ばれる。project-current は 2 回目以降
0.004ms 程度なのでキャッシュは置かない。"
  (or (wamei/project-tabs--project-name)
      (buffer-name (window-buffer (wamei/project-tabs-main-window)))))

;;; タブ名の固定

(defun wamei/project-tabs-pin-name (&optional frame)
  "FRAME のカレントタブが未固定でプロジェクトに属していれば、その名前で固定する。
併せてそのプロジェクトの root をタブに紐づける。
固定したときだけ新しい名前を返す。tab-bar-mode が無効、既に固定済み、
プロジェクト外のいずれかなら名前は動かさず nil。

root の紐づけは名前の固定より条件が緩い。名前が既に固定されたタブ
(この関数を通る前からあるタブ、desktop から復元したタブ) にも root を
持たせたいが、そこでカレントバッファのプロジェクトを無条件に採ると、
別プロジェクトのファイルを一度開いただけで紐づけ先が変わってしまう。
固定済みのタブはタブ名が紐づけ先の宣言なので、名前が一致するときだけ
記録する。既に root があれば触らない。"
  (with-selected-frame (or frame (selected-frame))
    (when tab-bar-mode
      (when-let* ((project (wamei/project-tabs--main-project))
                  (name (project-name project))
                  (tab (tab-bar--current-tab)))
        (let ((unpinned (not (alist-get 'explicit-name tab))))
          (when (and (not (wamei/project-tabs-current-root))
                     (or unpinned (equal (alist-get 'name tab) name)))
            (wamei/project-tabs-set-root (project-root project)))
          (when unpinned
            (tab-rename name)
            name))))))

(defun wamei/project-tabs--pin-name-soon (frame)
  "`window-buffer-change-functions' 用。FRAME のタブ名固定を次のコマンド境界に回す。
tab-rename は window 構成を変える処理を呼ぶことがあるので、再表示中に走る
window change 関数の中では直接呼ばない。"
  (run-at-time 0 nil #'wamei/project-tabs-pin-name frame))

;;; project 系コマンドの起点

(defvar wamei/project-tabs-commands '(consult-project-buffer)
  "`project-' で始まらないが、タブのプロジェクト起点で動かしたいコマンド。")

(defun wamei/project-tabs--command-p (command)
  "COMMAND がタブのプロジェクト起点で動かす対象か。"
  (and command
       (symbolp command)
       (or (string-prefix-p "project-" (symbol-name command))
           (memq command wamei/project-tabs-commands))))

(defun wamei/project-tabs--use-tab-root (orig &optional maybe-prompt directory)
  "`project-current' (ORIG) をタブに紐づいたプロジェクト起点で呼ぶ。

差し替えるのは次が揃ったときだけ。

- `this-command' が対象コマンド。project-current は eglot・apheleia・vc
  などからも呼ばれ、そちらは開いているファイルのプロジェクトに従うべき。
- DIRECTORY も `project-current-directory-override' も未指定。
  project-switch-project や project-other-tab-command は自分で起点を
  指定するので、それを勝たせる。
- カレントタブに root が紐づいている。無ければ従来どおりバッファ基準。"
  (if-let* (((null directory))
            ((null project-current-directory-override))
            ((wamei/project-tabs--command-p this-command))
            (root (wamei/project-tabs-current-root)))
      (let ((project-current-directory-override root))
        (funcall orig maybe-prompt))
    (funcall orig maybe-prompt directory)))

(defun wamei/project-tabs-setup ()
  "project 系の対話コマンドをタブのプロジェクト起点にする。"
  (advice-add 'project-current :around #'wamei/project-tabs--use-tab-root))

(provide 'project-tabs)
;;; project-tabs.el ends here
