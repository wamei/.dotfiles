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
(require 'project-sidebar)

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

タブに root が無いときは本文 window のバッファで直接判定する。プロ
ジェクトメモは `project-current-directory-override' を持つので、既に
メモが出ていてもそれ自身で正しく自己判定できる (`wamei/project-memo-buffer'
参照)。back を先に見てしまうと、window に別プロジェクトの toggle 連鎖の
残骸や、toggle を経由しない window-buffer の差し替えで古い back が残って
いたときに、表示中のメモとは無関係な判定に化けてしまう。back を見るのは
直接判定が失敗したとき (override を持たない全体メモが出ているとき) だけ
でよい。"
  (if-let* ((root (wamei/project-tabs-current-root)))
      (project-current nil root)
    (let* ((window (wamei/project-tabs-main-window))
           (buffer (window-buffer window)))
      (or (with-current-buffer buffer (project-current nil))
          (when-let* ((back (window-parameter window 'wamei/project-memo-back)))
            (and (buffer-live-p back)
                 (with-current-buffer back (project-current nil))))))))

(defun wamei/project-memo--restore (window)
  "WINDOW をメモを出す前のバッファに戻す。記録が無ければ直前の非メモバッファ。

記録が無いのは、toggle を経由せず WINDOW に最初からメモが出ていた場合
(desktop 復元直後など、メモは普通のファイルバッファなので普通に復元
される)。その状態での `switch-to-prev-buffer' の候補は別のメモのこと
があり、それだと「メモから抜ける」はずの操作がメモに留まってしまう。
非メモのバッファが見つかるまで探し、無ければ *scratch* に逃がす。"
  (let ((back (window-parameter window 'wamei/project-memo-back)))
    (set-window-parameter window 'wamei/project-memo-back nil)
    (if (buffer-live-p back)
        (set-window-buffer window back)
      (switch-to-prev-buffer window)
      (when (wamei/project-memo-buffer-p (window-buffer window))
        (set-window-buffer
         window
         (or (seq-find (lambda (buf) (not (wamei/project-memo-buffer-p buf)))
                        (buffer-list))
             (get-buffer-create "*scratch*")))))
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

;;; 自動保存

(defun wamei/project-memo--auto-save-p ()
  "`auto-save-visited-predicate' 用。メモバッファだけ実ファイルへ保存する。

`auto-save-visited-mode' はグローバルなので、述語を置かないと全部の
ファイルが勝手に保存されるようになる。"
  (wamei/project-memo-buffer-p))

(defun wamei/project-memo-save-all (&rest _)
  "変更のあるメモバッファを全て保存する。

`window-selection-change-functions' (frame を受け取る)、
`after-focus-change-function'、`kill-emacs-hook' から呼ぶので引数は受け流す。
アイドル中の保存は `auto-save-visited-mode' が見るため、ここは
「メモから離れた瞬間」を埋めるためにある。"
  (let ((save-silently t))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (wamei/project-memo-buffer-p) (buffer-modified-p))
          (save-buffer)))))
  nil)

(defun wamei/project-memo-autosave-setup ()
  "メモの自動保存を有効にする。init.el から 1 回呼ぶ。

複数回呼んでも安全 (idempotent)。`add-hook' は同じ関数の重複追加を自分で
弾いてくれる。`add-function' も同じ FUNCTION を渡す限りは内部で古い方を
外してから積み直すだけで二重合成にはならないが、それを暗黙の前提にせず
`advice-function-member-p' で「既に合成済みか」を明示的に見てから合成する
(hook 側の `add-hook' と対称にして、この関数全体が idempotent だと
読み取れるようにする意図)。init.el を対話的に再評価する運用なので、
このガードで安心して再評価できる。"
  (setq auto-save-visited-predicate #'wamei/project-memo--auto-save-p)
  (auto-save-visited-mode 1)
  (add-hook 'window-selection-change-functions #'wamei/project-memo-save-all)
  (unless (advice-function-member-p #'wamei/project-memo-save-all
                                    after-focus-change-function)
    (add-function :after after-focus-change-function #'wamei/project-memo-save-all))
  (add-hook 'kill-emacs-hook #'wamei/project-memo-save-all))

;;; タブの初期画面

(defun wamei/project-memo-switch-setup ()
  "プロジェクトを開いた直後の画面を作る。

`project-switch-commands' に置いて `project-switch-project' から
`call-interactively' で呼ばれる。左に sidebar、本文 window にその
プロジェクトのメモを出し、フォーカスは本文に残す。

対象プロジェクトは `project-current' から取る。呼び出し元バッファに
`project-current-directory-override' がバッファローカルで設定されて
いるため (`default-directory' は変わらない)。`select-window' は選択した
window のバッファをカレントにするので、取得はその前に済ませる。"
  (interactive)
  (let* ((project (project-current nil))
         (root (and project (project-root project)))
         (window (wamei/project-tabs-main-window))
         (buffer (wamei/project-memo-buffer project)))
    (select-window window)
    (delete-other-windows window)
    (set-window-buffer window buffer)
    (when root
      (wamei/project-sidebar-show root))
    (select-window window)))

(provide 'project-memo)
;;; project-memo.el ends here
