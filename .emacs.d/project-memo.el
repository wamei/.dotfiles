;;; project-memo.el --- org のメモ (プロジェクト別 / 全体) -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; プロジェクトごとのメモと、プロジェクトに紐づかない全体メモを org で持つ。
;;
;; - 実体は `wamei/project-memo-directory' (既定 ~/org/) 直下のフラットな
;;   org ファイル。プロジェクトメモは <project-name>.org、全体メモは global.org
;; - 保存は意識しなくてよい。アイドル中も、メモから離れるときも、
;;   `wamei/project-memo-save-all' が実ファイルへ書く。アイドル分は
;;   このモジュール専用の `run-with-idle-timer' で回す
;;   (`auto-save-visited-mode' は使わない。理由は
;;   `wamei/project-memo-autosave-setup' の docstring)
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
project-find-file などの起点もメモのディレクトリになってしまう。

`default-directory' も同じ root に向ける。override はバッファローカルで
`project-current' 越しにしか見えないので、default-directory を生で読む
利用者には届かない。たとえば `wamei/project-sidebar-toggle' の「sidebar が
出ていない」枝は本文 window のバッファの default-directory をそのまま
使うため、メモが本文にいると ~/org/ の dired が開いてしまう
(カレントバッファがメモ自身なら override が効いて隠れるが、端末パネル等に
フォーカスがあるときに露呈する)。1 行で default-directory の利用者を
まとめて正しくする。

全体メモは (プロジェクトに属さないので) どちらも設定しない。"
  (let* ((file (if project
                   (wamei/project-memo-file project)
                 (wamei/project-memo-global-file)))
         (new (not (file-exists-p file)))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (when (and new (zerop (buffer-size)))
        (insert "#+title: " (if project (project-name project) (file-name-base file)) "\n\n"))
      (if project
          (let ((root (file-name-as-directory (expand-file-name (project-root project)))))
            (setq-local project-current-directory-override root)
            (setq-local default-directory root))
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

(defcustom wamei/project-memo-autosave-idle-interval 5
  "アイドル何秒でメモを実ファイルへ書くか。

`auto-save-visited-interval' の既定と同じ 5 秒。アイドルが続く間は
この間隔で繰り返し走る。"
  :type 'number
  :group 'wamei/project-memo)

(defvar wamei/project-memo--autosave-timer nil
  "アイドル保存の繰り返しタイマー。`wamei/project-memo-autosave-setup' が持つ。

この 1 つに限ることで、setup を何度呼んでもタイマーが積み上がらない。")

(defun wamei/project-memo-save-all (&rest _)
  "変更のあるメモバッファを全て保存する。

アイドル時のタイマーと、`window-selection-change-functions' (frame を
受け取る)、`after-focus-change-function'、`kill-emacs-hook' から呼ぶので
引数は受け流す。

`save-buffer' を安全に呼べない場面が 2 つあるので、それぞれ手当てする。

1. modtime がずれているバッファは飛ばす。init.el は desktop を Emacs.app と
   `emacs -nw' で分けているので、同じメモを 2 つのインスタンスが開ける。
   片方が保存するともう片方の記録した modtime は古くなり、`basic-save-buffer'
   が `yes-or-no-p' で「Save anyway?」を聞く。この関数の呼び出し元は
   `window-selection-change-functions' = redisplay 中なので、そこで
   プロンプトを出すわけにはいかない (`save-silently' が抑えるのは
   メッセージだけでプロンプトではない)。飛ばしても編集はバッファに残り、
   ユーザーが `C-x C-s' したときに通常どおり確認できる。
2. エラーは握って `message' に落とす。redisplay hook で飛ばすと表示が壊れ、
   `kill-emacs-hook' で飛ばすと Emacs が終了できなくなる。1 つのメモの
   失敗で残りのメモの保存まで止めない。sidebar の
   `wamei/project-sidebar--follow-soon' と同じ流儀。"
  (let ((save-silently t))
    (dolist (buffer (buffer-list))
      (with-demoted-errors "project-memo: save failed: %S"
        (with-current-buffer buffer
          (when (and (wamei/project-memo-buffer-p)
                     (buffer-modified-p)
                     (verify-visited-file-modtime))
            (save-buffer))))))
  nil)

(defun wamei/project-memo-autosave-setup ()
  "メモの自動保存を有効にする。init.el から 1 回呼ぶ。

アイドル中の保存は専用の繰り返しタイマーで回す。`auto-save-visited-mode'
は使わない。あれは `save-some-buffers' 経由で、`buffer-save-without-query'
が非 nil のバッファを述語より先に無条件で保存してしまう (files.el)。
magit の save-repository-buffers に `Y' で答えるとそのフラグが立つので、
以後そのソースファイルが書きかけのまま 5 秒ごとにディスクへ書かれる。
`save-some-buffers-functions' も走るため abbrev ファイルまで書かれる。
`wamei/project-memo-save-all' は作りからしてメモしか触らないので、
これに置き換えれば「メモ以外は書かない」が述語頼みでなく構造で保証される。

複数回呼んでも安全 (idempotent)。タイマーは張り直す前に古いものを消す。
`add-hook' は同じ関数の重複追加を自分で弾いてくれる。`add-function' も
同じ FUNCTION を渡す限りは内部で古い方を外してから積み直すだけで二重合成に
はならないが、それを暗黙の前提にせず `advice-function-member-p' で
「既に合成済みか」を明示的に見てから合成する (hook 側の `add-hook' と
対称にして、この関数全体が idempotent だと読み取れるようにする意図)。
init.el を対話的に再評価する運用なので、このガードで安心して再評価できる。"
  (when (timerp wamei/project-memo--autosave-timer)
    (cancel-timer wamei/project-memo--autosave-timer))
  (setq wamei/project-memo--autosave-timer
        (run-with-idle-timer wamei/project-memo-autosave-idle-interval t
                             #'wamei/project-memo-save-all))
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
