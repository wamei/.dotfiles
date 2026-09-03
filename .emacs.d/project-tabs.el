;;; project-tabs.el --- タブ = プロジェクトの運用を支える -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; タブ 1 つにプロジェクト 1 つを対応させて運用している。このファイルは
;; そのための 3 つの部品を持つ。
;;
;; 1. タブ名の決定 (`wamei/tab-bar-tab-name-project')
;;    既定の tab-bar-tab-name-current はカレントバッファ名を使うため、treemacs
;;    のような side window にフォーカスした状態だと内部バッファ名がタブ名になる。
;;    プロジェクト名を優先し、side window にいるときは直近の通常 window を見る。
;;
;; 2. タブ名の固定 (`wamei/project-tabs-pin-name')
;;    tab-bar はタブ名を再描画のたびに再計算するので、1. だけだとカレント
;;    バッファのプロジェクトが変わるたびにタブ名も変わる。treemacs-tab-bar は
;;    タブ名をそのままスコープ (バッファ / workspace の単位) に使うため、
;;    別プロジェクトのファイルを開いた瞬間にスコープだけが先に変わり、window
;;    に残った treemacs バッファと食い違って treemacs-project->position が
;;    (wrong-type-argument arrayp nil) で落ちる。副作用として "Tab init.el" や
;;    "Tab *scratch*" のような使い捨て workspace も増え続ける。
;;    タブにプロジェクトのバッファが初めて出た時点で tab-rename して
;;    explicit-name を立て、以後は名前 = スコープを動かさない。
;;
;; 3. treemacs のガード (`wamei/treemacs--find-file-node-guard')
;;    2. でも tab-rename 直後や desktop 復元時の取りこぼしは残るので、
;;    treemacs-find-file-node が「このバッファに root が描画されていない
;;    プロジェクト」を渡されたときは探索せず nil を返す。呼び出し側
;;    (treemacs--follow / treemacs--flatten-dirs など) は nil を「見つからず」
;;    として扱うので、落ちる代わりに追従しないだけで済む。
;;
;;; Code:

(require 'project)
(require 'tab-bar)

(declare-function treemacs-find-in-dom "treemacs-dom")
(declare-function treemacs-project->path "treemacs-workspaces")
(declare-function treemacs--find-project-for-path "treemacs-workspaces")

;;; タブ名

(defun wamei/project-tabs--name-window ()
  "タブ名の根拠にする window を返す。
選択 window が side window (no-other-window 付き) なら直近の通常 window。"
  (if (window-parameter (selected-window) 'no-other-window)
      (or (get-mru-window nil nil t t) (selected-window))
    (selected-window)))

(defun wamei/project-tabs--project-name ()
  "カレントタブが属するプロジェクトの名前。プロジェクト外なら nil。"
  (with-current-buffer (window-buffer (wamei/project-tabs--name-window))
    (when-let* ((project (project-current nil)))
      (project-name project))))

(defun wamei/tab-bar-tab-name-project ()
  "プロジェクト名をタブ名にする。プロジェクト外ではバッファ名を使う。
`tab-bar-tab-name-function' 用。tab-bar-tabs 内でカレントタブ名の再計算に
使われ、タブバーの再描画ごとに呼ばれる。project-current は 2 回目以降
0.004ms 程度なのでキャッシュは置かない。"
  (or (wamei/project-tabs--project-name)
      (buffer-name (window-buffer (wamei/project-tabs--name-window)))))

;;; タブ名の固定

(defun wamei/project-tabs-pin-name (&optional frame)
  "FRAME のカレントタブが未固定でプロジェクトに属していれば、その名前で固定する。
固定したら新しい名前を返す。tab-bar-mode が無効、既に固定済み、
プロジェクト外のいずれかなら何もせず nil。"
  (with-selected-frame (or frame (selected-frame))
    (when (and tab-bar-mode
               (not (alist-get 'explicit-name (tab-bar--current-tab))))
      (when-let* ((name (wamei/project-tabs--project-name)))
        (tab-rename name)
        name))))

(defun wamei/project-tabs--pin-name-soon (frame)
  "`window-buffer-change-functions' 用。FRAME のタブ名固定を次のコマンド境界に回す。
tab-rename は treemacs-tab-bar の advice 経由で window 構成を変えるので、
再表示中に走る window change 関数の中では直接呼ばない。"
  (run-at-time 0 nil #'wamei/project-tabs-pin-name frame))

;;; treemacs のガード

(defun wamei/treemacs--find-file-node-guard (fn path &optional project)
  "`treemacs-find-file-node' の :around advice。
PROJECT (省略時は現在の workspace から解決) の root がこのバッファの DOM に
なければ FN を呼ばず nil を返す。"
  (let ((project (or project (treemacs--find-project-for-path path))))
    (when (and project
               (treemacs-find-in-dom (treemacs-project->path project)))
      (funcall fn path project))))

(provide 'project-tabs)
;;; project-tabs.el ends here
