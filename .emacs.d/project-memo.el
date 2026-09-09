;;; project-memo.el --- org のメモ (プロジェクト別 / 全体) -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; プロジェクトごとのメモと、プロジェクトに紐づかない全体メモを org で持つ。
;;
;; - 実体は `wamei/project-memo-directory' (既定 ~/org/) 直下のフラットな
;;   org ファイル。プロジェクトメモは <project-name>.org、全体メモは global.org
;; - 保存は意識しなくてよい。アイドル中は `auto-save-visited-mode' が、
;;   メモから離れるときは `wamei/project-memo-save-all' が実ファイルへ書く
;; - 復元は desktop に任せる。メモは通常のファイルバッファなので専用処理は要らない
;; - プロジェクトタブを開いた直後の画面 (`wamei/project-memo-switch-setup') は
;;   左に sidebar、本文 window にそのプロジェクトのメモ
;;
;; 置き場をフラットにしたので、同名の repo が複数あると同じメモを共有する。
;; 「どこにある repo でも扱えること」を優先した結果として受け入れている。
;;
;;; Code:

(require 'project)
(require 'project-tabs)

(defgroup wamei/project-memo nil
  "org のメモ (プロジェクト別 / 全体)。"
  :group 'convenience)

(defcustom wamei/project-memo-directory "~/org/"
  "メモを置くディレクトリ。"
  :type 'directory
  :group 'wamei/project-memo)

(defcustom wamei/project-memo-global-name "global.org"
  "全体メモのファイル名。`wamei/project-memo-directory' からの相対。"
  :type 'string
  :group 'wamei/project-memo)

;;; パス解決

(defun wamei/project-memo--sanitize (name)
  "NAME をファイル名に使える形にする。ディレクトリ区切りを - に潰す。"
  (replace-regexp-in-string "/" "-" name))

(defun wamei/project-memo--directory ()
  "メモのディレクトリ (末尾 / 付き)。無ければ作る。"
  (let ((dir (file-name-as-directory (expand-file-name wamei/project-memo-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun wamei/project-memo-file (project)
  "PROJECT のメモファイルの絶対パス。

名前は `project-name' を使う。タブに出ている名前 (project-tabs.el) と
同じものにして、タブとメモの対応を見た目から追えるようにする。"
  (expand-file-name (concat (wamei/project-memo--sanitize (project-name project)) ".org")
                    (wamei/project-memo--directory)))

(defun wamei/project-memo-global-file ()
  "全体メモの絶対パス。"
  (expand-file-name wamei/project-memo-global-name (wamei/project-memo--directory)))

(defun wamei/project-memo-buffer-p (&optional buffer)
  "BUFFER (既定はカレント) がメモファイルを訪れているか。

バッファローカルの目印ではなくパスで判定する。desktop から復元された
メモバッファには目印が付かないが、自動保存はそれにも効く必要がある。"
  (let ((file (buffer-file-name (or buffer (current-buffer)))))
    (and file
         (equal (file-name-extension file) "org")
         (file-in-directory-p file (expand-file-name wamei/project-memo-directory))
         t)))

;;; バッファ

(defun wamei/project-memo-buffer (&optional project)
  "PROJECT のメモバッファ。PROJECT が nil なら全体メモ。

ファイルがまだ無ければ #+title: の 1 行だけ入れる。ファイルは最初の保存で
生まれる (自動保存があるので、開いたまま数秒放置すれば実体ができる)。

プロジェクトメモには `project-current-directory-override' をバッファ
ローカルで持たせる。メモの実体は ~/org/ にあってプロジェクト外なので、
これが無いとタブ名の判定 (`wamei/tab-bar-tab-name-project') が外れ、
project-find-file などの起点もメモのディレクトリになってしまう。"
  (let* ((file (if project
                   (wamei/project-memo-file project)
                 (wamei/project-memo-global-file)))
         (new (not (file-exists-p file)))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (when (and new (zerop (buffer-size)))
        (insert "#+title: " (if project (project-name project) (file-name-base file)) "\n\n"))
      (if project
          (setq-local project-current-directory-override
                      (file-name-as-directory (expand-file-name (project-root project))))
        (kill-local-variable 'project-current-directory-override)))
    buffer))

;;; 表示

(defun wamei/project-memo--project ()
  "メモの対象にするプロジェクト。無ければ nil。

タブに紐づいた root (project-tabs.el) を先に見る。本文 window に別
プロジェクトのファイルや *scratch* が出ていても、タブの宣言に従わせる。

タブに root が無いときは本文 window のバッファで判定するが、そこに
既にメモが出ているなら退避してある元のバッファを見る。メモの実体は
`wamei/project-memo-directory' 直下でプロジェクト外にあるため、メモ
自身を基準にすると (全体メモ表示中の 2 回目の toggle のように) 常に
「プロジェクト外」判定になってしまう。"
  (if-let* ((root (wamei/project-tabs-current-root)))
      (project-current nil root)
    (let* ((window (wamei/project-tabs-main-window))
           (buffer (window-buffer window)))
      (when (wamei/project-memo-buffer-p buffer)
        (let ((back (window-parameter window 'wamei/project-memo-back)))
          (when (buffer-live-p back)
            (setq buffer back))))
      (with-current-buffer buffer
        (project-current nil)))))

(defun wamei/project-memo--restore (window)
  "WINDOW をメモを出す前のバッファに戻す。記録が無ければ直前のバッファ。"
  (let ((back (window-parameter window 'wamei/project-memo-back)))
    (set-window-parameter window 'wamei/project-memo-back nil)
    (if (buffer-live-p back)
        (set-window-buffer window back)
      (switch-to-prev-buffer window))
    (select-window window)))

(defun wamei/project-memo-toggle (&optional global)
  "本文 window にメモを出す。既に出ていれば元のバッファに戻る。

GLOBAL (`C-u') が非 nil なら全体メモ。タブがプロジェクトに紐づいて
いないときは GLOBAL 無しでも全体メモになる。

出す先は `wamei/project-tabs-main-window'。sidebar や端末パネルに
フォーカスがあっても本文 window に出す。

戻り先は window パラメータに退避する。メモから別のメモへ切り替えた
ときは上書きせず、最初にメモを出す前のバッファを保つ。"
  (interactive "P")
  (let* ((project (unless global (wamei/project-memo--project)))
         (buffer (wamei/project-memo-buffer project))
         (window (wamei/project-tabs-main-window)))
    (if (eq (window-buffer window) buffer)
        (wamei/project-memo--restore window)
      (unless (wamei/project-memo-buffer-p (window-buffer window))
        (set-window-parameter window 'wamei/project-memo-back (window-buffer window)))
      (set-window-buffer window buffer)
      (select-window window))))

(provide 'project-memo)
;;; project-memo.el ends here
