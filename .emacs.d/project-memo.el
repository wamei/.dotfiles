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

(provide 'project-memo)
;;; project-memo.el ends here
