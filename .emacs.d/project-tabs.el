;;; project-tabs.el --- タブ = プロジェクトの運用を支える -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; タブ 1 つにプロジェクト 1 つを対応させて運用している。このファイルは
;; そのための 2 つの部品を持つ。
;;
;; 1. タブ名の決定 (`wamei/tab-bar-tab-name-project')
;;    既定の tab-bar-tab-name-current はカレントバッファ名を使うため、sidebar
;;    のような side window にフォーカスした状態だと内部バッファ名がタブ名になる。
;;    プロジェクト名を優先し、side window にいるときは直近の通常 window を見る。
;;
;; 2. タブ名の固定 (`wamei/project-tabs-pin-name')
;;    tab-bar はタブ名を再描画のたびに再計算するので、1. だけだとカレント
;;    バッファのプロジェクトが変わるたびにタブ名も変わる。タブにプロジェクトの
;;    バッファが初めて出た時点で tab-rename して explicit-name を立て、
;;    以後は名前を動かさない。
;;
;;; Code:

(require 'project)
(require 'tab-bar)

;;; タブ名

(defun wamei/project-tabs-main-window ()
  "タブの本文とみなす window。
選択 window が side window (no-other-window 付き) なら直近の通常 window。
タブ名の根拠、サイドバーが従うバッファ、サイドバーからファイルを開く先に使う。"
  (if (window-parameter (selected-window) 'no-other-window)
      (or (get-mru-window nil nil t t) (selected-window))
    (selected-window)))

(defalias 'wamei/project-tabs--name-window #'wamei/project-tabs-main-window)

(defun wamei/project-tabs--project-name ()
  "カレントタブが属するプロジェクトの名前。プロジェクト外なら nil。"
  (with-current-buffer (window-buffer (wamei/project-tabs-main-window))
    (when-let* ((project (project-current nil)))
      (project-name project))))

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
tab-rename は window 構成を変える処理を呼ぶことがあるので、再表示中に走る
window change 関数の中では直接呼ばない。"
  (run-at-time 0 nil #'wamei/project-tabs-pin-name frame))

(provide 'project-tabs)
;;; project-tabs.el ends here
