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
;; - 既定の表示先は画面中央の posframe (`wamei/project-memo-toggle')。
;;   `C-u' を付けると本文 window に出す。posframe が使えない環境
;;   (`posframe-workable-p' が nil) では `C-u' 無しでも本文 window に落とす
;; - posframe はフォーカスが外れたときと、もう一度トグルしたときに閉じる。
;;   いずれも閉じる前に保存する。ESC / C-g では閉じない (どちらも org の
;;   編集中に使う)。`posframe-show' は child frame の root window を強い
;;   dedicated にする (posframe.el) が、`wamei/project-memo-posframe-show' は
;;   show の直後にそれを解除する。メモの小窓にフォーカスがある状態で
;;   find-file や magit-status を実行したら、そのバッファは小窓自身の中に
;;   開く。本文 window は一切触らない。トグル
;;   (`wamei/project-memo--toggle') は小窓に映っているバッファ
;;   (`wamei/project-memo--posframe-buffer-shown') を見て、要求されたメモ
;;   本人ならそのまま閉じ、別のバッファ (別のメモ、または小窓の中で開いた
;;   ファイル) なら閉じずに要求されたメモを出し直す
;;
;; 置き場をフラットにしたので、同名の repo が複数あると同じメモを共有する。
;; 「どこにある repo でも扱えること」を優先した結果として受け入れている。
;;
;; 既知の制限:
;;
;; - `wamei/project-memo-directory' (~/org) 自体を git repo にすると、
;;   `wamei/project-sidebar--follow' がメモを「別プロジェクトのファイル」と
;;   見なして sidebar を ~/org の dired に引っ張る。あちらは
;;   `(file-name-directory file)' からプロジェクトを出すので、メモバッファの
;;   `project-current-directory-override' も `default-directory' も見ない
;;   (`wamei/project-sidebar--follow' の file-root の計算)。このモジュール側は
;;   `wamei/project-memo--usable-project' で自分の判定を守っているが、
;;   sidebar 側には手を入れていない。~/org を repo にするなら project-sidebar.el
;;   にも同じ手当てが要る。
;; - バッファローカルの `project-current-directory-override' と window
;;   パラメータ `wamei/project-memo-back' は再起動をまたがない。desktop は
;;   どちらも保存しないし、`wamei/project-memo-back' は
;;   `window-persistent-parameters' に無いので `set-window-configuration'
;;   (magit の q) でも落ちる。実害は無い。タブの root
;;   (`wamei/project-tabs-root') は残るので対象プロジェクトの判定は効き、
;;   最初の `C-x C-m' が override を張り直す。back が無いときの戻り先は
;;   `wamei/project-memo--restore' のフォールバックが受け持つ。
;;
;;; Code:

(require 'project)
(require 'project-tabs)
(require 'project-sidebar)
(require 'posframe)

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

(defun wamei/project-memo--truename-directory (dir)
  "DIR を末尾 / 付きの実体パスにする。パス同士の比較用。

`file-truename' まで通すのは、片方が symlink 越し (macOS の
/var/folders → /private/var/folders など) でも同じ場所だと分かるように
するため。"
  (file-name-as-directory (file-truename (expand-file-name dir))))

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

中身が空なら #+title: の 1 行だけ入れる。ファイルは最初の保存で生まれる
(自動保存があるので、開いたまま数秒放置すれば実体ができる)。

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
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (when (zerop (buffer-size))
        ;; 判断は「バッファが空か」だけで、ファイルの有無は見ない。
        ;; file-exists-p も条件に入れると、実体が 0 バイトになったメモに
        ;; 二度と title が入らなくなる (自動保存があるので 0 バイトの
        ;; 実体は簡単にできる)。
        ;;
        ;; 挿入中は undo を止める。title が undo スタックに載っていると、
        ;; 新規メモで 1 回 undo しただけで消え、次の自動保存が 0 バイトで
        ;; 書いてしまう。buffer-undo-list を後から潰すのではなく let で
        ;; 抑止するのは、既に編集中のバッファの履歴を巻き添えにしないため。
        (let ((buffer-undo-list t))
          (insert "#+title: "
                  (if project (project-name project) (file-name-base file))
                  "\n\n")))
      (if project
          (let ((root (file-name-as-directory (expand-file-name (project-root project)))))
            (setq-local project-current-directory-override root)
            (setq-local default-directory root))
        (kill-local-variable 'project-current-directory-override)))
    buffer))

;;; 表示

(defun wamei/project-memo--usable-project (project)
  "PROJECT をメモの対象にしてよければそのまま返す。だめなら nil。

root がメモディレクトリ自身のものを弾く。spec §2 の「将来 ~/org 自体を
git repo にしても ~/org のプロジェクトとは判定されない」を、パスから
プロジェクトを計算するこの経路でも守るため。override が守るのはメモ
バッファ自身の `project-current' だけで、ここには届かない。

弾かないと、root の無いタブで全体メモを出しているときに `C-x C-m' が
~/org/org.org を開く。それ自体がメモなので、メモを開くつもりの操作が
別のメモを増やすだけになる。"
  (and project
       (not (equal (wamei/project-memo--truename-directory (project-root project))
                   (wamei/project-memo--truename-directory wamei/project-memo-directory)))
       project))

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
でよい。

どの経路の答えも `wamei/project-memo--usable-project' に通す。メモ
ディレクトリ自身を root とするプロジェクトを返さないため。"
  (if-let* ((root (wamei/project-tabs-current-root)))
      (wamei/project-memo--usable-project (project-current nil root))
    (let* ((window (wamei/project-tabs-main-window))
           (buffer (window-buffer window)))
      (or (wamei/project-memo--usable-project
           (with-current-buffer buffer (project-current nil)))
          (when-let* ((back (window-parameter window 'wamei/project-memo-back)))
            (and (buffer-live-p back)
                 (wamei/project-memo--usable-project
                  (with-current-buffer back (project-current nil)))))))))

(defun wamei/project-memo--restore (window)
  "WINDOW をメモを出す前のバッファに戻す。記録が無ければ直前の非メモバッファ。

記録が無いのは、toggle を経由せず WINDOW に最初からメモが出ていた場合
(desktop 復元直後など、メモは普通のファイルバッファなので普通に復元
される)。その状態での `switch-to-prev-buffer' の候補は別のメモのこと
があり、それだと「メモから抜ける」はずの操作がメモに留まってしまう。
非メモのバッファが見つかるまで探し、無ければ *scratch* に逃がす。

探すのは生の `buffer-list' なので、名前が空白で始まる内部バッファ
(\" *Minibuf-0*\"、\" *sidebar: foo*\"、\" *load*\" など) も候補に混ざる。
これらは `switch-to-prev-buffer' が意図して飛ばすもので、本文 window に
出してよいものではない。`set-window-buffer' は黙って受け付けてしまうので
ここで弾く。"
  (let ((back (window-parameter window 'wamei/project-memo-back)))
    (set-window-parameter window 'wamei/project-memo-back nil)
    (if (buffer-live-p back)
        (set-window-buffer window back)
      (switch-to-prev-buffer window)
      (when (wamei/project-memo-buffer-p (window-buffer window))
        (set-window-buffer
         window
         (or (seq-find (lambda (buf)
                         (and (not (string-prefix-p " " (buffer-name buf)))
                              (not (wamei/project-memo-buffer-p buf))))
                       (buffer-list))
             (get-buffer-create "*scratch*")))))
    (select-window window)))

(defun wamei/project-memo--show-in-main-window (buffer)
  "BUFFER を本文 window に出す。既に出ていれば元のバッファに戻る。

戻り先は window パラメータに退避する。メモから別のメモへ切り替えた
ときは上書きせず、最初にメモを出す前のバッファを保つ。"
  (let ((window (wamei/project-tabs-main-window)))
    (if (eq (window-buffer window) buffer)
        (wamei/project-memo--restore window)
      (unless (wamei/project-memo-buffer-p (window-buffer window))
        (set-window-parameter window 'wamei/project-memo-back (window-buffer window)))
      (set-window-buffer window buffer)
      (select-window window))))

(defun wamei/project-memo--toggle (global main-window)
  "メモを出す。GLOBAL が非 nil なら全体メモ、nil ならプロジェクトメモ。

MAIN-WINDOW が非 nil なら本文 window、nil なら画面中央の posframe に出す。
posframe が使えない環境 (`posframe-workable-p' が nil、batch や child frame
非対応の端末) では MAIN-WINDOW によらず本文 window に落とす。

posframe が既に出ているときの分岐は「いま小窓に映っているバッファ」
(`wamei/project-memo--posframe-buffer-shown') を見る。追跡変数
(`wamei/project-memo-posframe-buffer') ではなく実際に映っているものを見る
のは、`wamei/project-memo-posframe-show' が dedicated を外しているため、
小窓の中で find-file した別のファイルや、それを経由せず映ったままの別の
メモが、追跡変数とは無関係に映っていることがあるため。

- 映っているのが BUFFER 自身 → 閉じる (トグル)
- それ以外 (別のメモ、または小窓の中で開いたファイル) → 閉じずに BUFFER を
  `wamei/project-memo-posframe-show' で出す。小窓の中でファイルを開いた後に
  同じキーを押すとメモに戻り、もう一度押すと閉じる往復になる。BUFFER の
  切り替えは同関数が既に持っている「先に古い方を隠す」処理に任せる

表示先を本文 window に変えるときだけ、ここで posframe を閉じる。"
  (let* ((project (unless global (wamei/project-memo--project)))
         (buffer (wamei/project-memo-buffer project))
         (use-posframe (and (not main-window) (posframe-workable-p))))
    (cond
     (use-posframe
      (if (eq (wamei/project-memo--posframe-buffer-shown) buffer)
          (wamei/project-memo-posframe-hide)
        (wamei/project-memo-posframe-show buffer)))
     (t
      (wamei/project-memo-posframe-hide)
      (wamei/project-memo--show-in-main-window buffer)))))

(defun wamei/project-memo-toggle (&optional main-window)
  "プロジェクトメモを画面中央の posframe に出す。出ていれば閉じる。

MAIN-WINDOW (`C-u') が非 nil なら posframe ではなく本文 window に出す。
タブがプロジェクトに紐づいていないときは全体メモになる。

posframe が使えない環境では `C-u' 無しでも本文 window に出る。"
  (interactive "P")
  (wamei/project-memo--toggle nil main-window))

(defun wamei/project-memo-toggle-global (&optional main-window)
  "全体メモを画面中央の posframe に出す。出ていれば閉じる。

MAIN-WINDOW (`C-u') が非 nil なら posframe ではなく本文 window に出す。
posframe が使えない環境では `C-u' 無しでも本文 window に出る。"
  (interactive "P")
  (wamei/project-memo--toggle t main-window))

;;; posframe

(defcustom wamei/project-memo-posframe-width-ratio 0.6
  "メモの posframe の幅。親フレームの桁数に対する比率。"
  :type 'float
  :group 'wamei/project-memo)

(defcustom wamei/project-memo-posframe-height-ratio 0.6
  "メモの posframe の高さ。親フレームの行数に対する比率。"
  :type 'float
  :group 'wamei/project-memo)

(defcustom wamei/project-memo-posframe-min-width 40
  "メモの posframe の最小の幅 (桁)。"
  :type 'integer
  :group 'wamei/project-memo)

(defcustom wamei/project-memo-posframe-min-height 10
  "メモの posframe の最小の高さ (行)。"
  :type 'integer
  :group 'wamei/project-memo)

(defvar wamei/project-memo--posframe-frame nil
  "メモを出している posframe のフレーム。出ていなければ nil。")

(defvar wamei/project-memo--posframe-buffer nil
  "posframe に出しているメモバッファ。`posframe-hide' はバッファで指定する。")

(defun wamei/project-memo-posframe-frame ()
  "メモの posframe が出ていればそのフレーム。出ていなければ nil。"
  (and wamei/project-memo--posframe-frame
       (frame-live-p wamei/project-memo--posframe-frame)
       wamei/project-memo--posframe-frame))

(defun wamei/project-memo-posframe-buffer ()
  "posframe に対して最後に show を求めたバッファ。出ていなければ nil。

`wamei/project-memo--posframe-buffer' の公開アクセサ。フレームが死んで
いるときに nil を返すのは `wamei/project-memo-posframe-frame' と同じ扱い。

表示先を選ぶ層 (`wamei/project-memo--toggle') はこれではなく「実際に
小窓に映っているバッファ」(`wamei/project-memo--posframe-buffer-shown')
を見て閉じる/出すを判断する。`wamei/project-memo-posframe-show' が
dedicated を外しているため、最後に要求したバッファと実際に映っている
バッファが食い違うことがあり (小窓の中で find-file した場合など)、
トグルの判断にはそちらが要る。このアクセサは「最後に何を要求したか」を
読みたい側 (テストなど) のために残してある。"
  (and (wamei/project-memo-posframe-frame)
       wamei/project-memo--posframe-buffer))

(defun wamei/project-memo--posframe-size (ratio total minimum)
  "RATIO (親フレームの TOTAL に対する比率) から posframe の大きさを出す。
MINIMUM を下回らない。"
  (max minimum (round (* ratio total))))

(defun wamei/project-memo--popup-color (face attribute)
  "FACE の ATTRIBUTE の色。FACE が未定義か未指定なら nil。

枠と背景は init.el の *popup-appearance が定義する `wamei/popup-border' /
`wamei/popup-body' から取る。あちらは init.el 側なので、モジュール単体で
読む batch テストには存在しない。`face-attribute' は未定義の face に対して
エラーを出す (\"Invalid face\") ので、存在するときだけ引く。nil を渡された
posframe はフレーム既定の色を使う。"
  (when (facep face)
    (let ((value (face-attribute face attribute nil t)))
      (unless (eq value 'unspecified) value))))

(defun wamei/project-memo-posframe-show (buffer)
  "BUFFER を画面中央の posframe に出し、フォーカスを移す。フレームを返す。

`:accept-focus' を渡さないと posframe 自身が
`posframe--redirect-posframe-focus' でフォーカスを親フレームへ送り返すので、
編集できない。カーソルも既定では隠されるので明示的に出す。

`:window-point' を渡さないと `posframe-show' は毎回 point 0 に飛ばす
(posframe.el の `posframe--create-posframe' が
`(window-point (or window-point 0))' としている)。閉じて開き直しても
前回いた場所に戻れるよう、BUFFER の現在の point を渡す。

枠と背景は corfu / eldoc-box / vertico-posframe と同じ
`wamei/popup-border' / `wamei/popup-body' から取る (init.el の
*popup-appearance)。tty の罫線枠は同ブロックが display table に入れた
box グリフがそのまま効く。

`:respect-mode-line' を渡すのは見た目の趣味ではない。posframe は
`:respect-mode-line' が nil だと表示するバッファに `mode-line-format' を
nil で setq-local する。これは posframe を隠しても残るので、そのメモを
あとから `C-u' で本文 window に出したときモードラインが消えたままになる。
バッファを壊さないために残す。

`posframe-show' は child frame の root window を強い dedicated にする
(posframe.el)。show の直後にそれを解除する。メモの小窓にフォーカスがある
状態で `C-x C-f' や `magit-status' を実行したら、その小窓の中に開くように
するため (本文 window は一切触らない)。dedicated を保ったままだと
`set-window-buffer' がエラーになり `display-buffer' もこの window を避ける
ので、他のバッファは強制的に親フレームへ出て行ってしまう。

すでに別の BUFFER を出している posframe があれば、先にそれを隠す
(`wamei/project-memo-posframe-hide' 経由で保存も伴う)。`posframe--frame' は
バッファローカル (posframe.el) なので、隠さずに別バッファへ `posframe-show'
すると古いフレームは追跡から外れたまま画面に残ってしまう。トグルや
自動クローズなど `show' の呼び出し元が複数になる後続タスクのために、
「show の前に自分で hide する」という前提を呼び出し側に負わせない。

`posframe-show' は `(selected-window)' の frame を親として使う
(posframe.el、`:parent-frame' を渡す口が無い)。dedicated を外した (このあと)
結果、小窓の中でファイルを開いて作業していることが正常な状態になったので、
`selected-frame' が小窓 (前回の posframe 自身) のままこの関数が呼ばれる経路が
実在する — 小窓の中で別のメモや `C-u' 無しの `C-x C-m' を求めたとき。
その状態のまま `posframe-show' すると、親が小窓自身になった `parent-frame'
チェーンを作ろうとして \"Circular specification of \\='parent-frame\\='\" で
落ちる。`wamei/project-tabs-base-frame' (project-tabs.el) で最上位の実
フレームまで遡ってから show するので、呼び出し時にどこが選択されていても
親は常に本物のトップレベルフレームになる。

`posframe-show' はフレームを使い回すとき (`posframe--create-posframe' が
生死と直前の引数の両方を見て判定する) `set-window-buffer' を呼ばない —
窓が既に強い dedicated で BUFFER 以外を映せない、という前提の上の最適化
らしい。dedicated を外した今、小窓に別のバッファ (小窓の中で開いたファイル
や別のメモ) が映ったままフレームを使い回すと、`posframe-show' に BUFFER を
渡しても窓の中身はそのまま変わらない。`posframe-show' の返り値任せにせず、
このあと自分で `set-window-buffer' と `set-window-point' をやり直す。
バッファが既に映っているときは `set-window-buffer' を呼ばない。無条件に
呼ぶと同じバッファを映しているときも `window-start' がリセットされ、
`window-buffer-change-functions' が child frame で発火してしまう
(project-tabs.el の `wamei/project-tabs--pin-name-soon' がこれに載っている
ので、素通しだと再表示のたびに無駄な `run-at-time' が積まれる)。

`(frame-selected-window frame)' ではなく `(frame-root-window frame)' で
window を取る。posframe.el 自身が同じ window を `posframe--create-posframe'
でこの取り方 (`frame-root-window') をしている (create 時の
`set-window-buffer' / `set-window-dedicated-p' 呼び出し箇所) ので、posframe
が「その child frame の window」として扱っているものに合わせる。child frame
は `unsplittable' なので selected-window と root-window は今のところ一致
するが、指しているものが違うと読めてしまうのは避けたい。"
  (when (and (wamei/project-memo-posframe-frame)
             (not (eq wamei/project-memo--posframe-buffer buffer)))
    (wamei/project-memo-posframe-hide))
  (setq wamei/project-memo--posframe-buffer buffer)
  (with-selected-frame (wamei/project-tabs-base-frame)
    (setq wamei/project-memo--posframe-frame
          (posframe-show
           buffer
           :poshandler #'posframe-poshandler-frame-center
           :width (wamei/project-memo--posframe-size
                   wamei/project-memo-posframe-width-ratio
                   (frame-width) wamei/project-memo-posframe-min-width)
           :height (wamei/project-memo--posframe-size
                    wamei/project-memo-posframe-height-ratio
                    (frame-height) wamei/project-memo-posframe-min-height)
           :border-width 1
           :border-color (wamei/project-memo--popup-color 'wamei/popup-border :background)
           :background-color (wamei/project-memo--popup-color 'wamei/popup-body :background)
           :accept-focus t
           :cursor 'box
           :respect-mode-line t
           :window-point (with-current-buffer buffer (point))))
    (let ((window (frame-root-window wamei/project-memo--posframe-frame)))
      (set-window-dedicated-p window nil)
      (unless (eq (window-buffer window) buffer)
        (set-window-buffer window buffer))
      (set-window-point window (with-current-buffer buffer (point)))))
  (add-hook 'post-command-hook #'wamei/project-memo--posframe-post-command)
  (select-frame-set-input-focus wamei/project-memo--posframe-frame)
  wamei/project-memo--posframe-frame)

(defun wamei/project-memo-posframe-hide ()
  "メモの posframe を保存してから隠す。出ていなければ何もしない。

追跡の後始末 (hook の除去と変数の nil 化) はフレームが生きているかに
関わらず必ず行う。フレームが何らかの外部要因 (`delete-frame' を直接呼ぶ、
`posframe-delete-all' など) で先に死んでいる状態でここへ来る経路がある。
`when' の中に畳むと、その場合に `post-command-hook' のエントリと死んだ
バッファへの参照がセッションの残りの間ずっと残る。

なお dedicated を外した (`wamei/project-memo-posframe-show') ので、メモ
バッファを kill しても posframe.el 自身がこの child frame を道連れに
削除することはもう無い (`replace-buffer-in-windows' は dedicated でない
window を単に `switch-to-prev-buffer' で差し替えるだけで、フレームは
生きたまま残る)。その代わりの後始末は `wamei/project-memo--posframe-action'
の「追跡バッファが死んでいたら \\='hide にする」ガードが受け持つ。

隠したあと、選択がそのフレームの window に残っていたら本文 window へ戻す。
tty には window ごとのフォーカスイベントが無いので、見えなくなった child
frame に選択が残ると以後の入力がその不可視バッファに吸い込まれ、自然には
復帰しない (`C-x C-f' を `C-g' で抜けた直後がまさにこれ)。既に posframe の
外を選択しているときは触らない。トグルや別 window への移動で閉じる経路で
選択を奪ってしまうため。

戻すときは `select-frame-set-input-focus' を先に呼ぶ。`select-window' は
選択された frame の中の選択 window を変えるだけで、ウィンドウシステムの
入力フォーカスまでは動かさない。child frame が OS レベルの入力フォーカス
を持ったまま隠れると、親フレームのカーソルが非アクティブ表示 (見えなく)
になる。"
  (let ((frame (wamei/project-memo-posframe-frame)))
    (when frame
      (wamei/project-memo-save-all)
      (posframe-hide wamei/project-memo--posframe-buffer))
    (remove-hook 'post-command-hook #'wamei/project-memo--posframe-post-command)
    (setq wamei/project-memo--posframe-frame nil)
    (setq wamei/project-memo--posframe-buffer nil)
    (when (and frame (eq (window-frame (selected-window)) frame))
      (let ((window (wamei/project-tabs-main-window)))
        (select-frame-set-input-focus (window-frame window))
        (select-window window))))
  nil)

(defun wamei/project-memo--posframe-buffer-shown ()
  "posframe の window が映しているバッファ。出ていなければ nil。

`(frame-root-window frame)' で window を取る。posframe.el 自身が
`posframe--create-posframe' で同じ window をこの取り方をしている
(`wamei/project-memo-posframe-show' も参照)。"
  (when-let* ((frame (wamei/project-memo-posframe-frame)))
    (window-buffer (frame-root-window frame))))

(defun wamei/project-memo--posframe-action ()
  "posframe に対していま取るべき動作。

- nil    … そのまま (メモにフォーカスがある)
- `hide' … 隠す (フォーカスが Emacs 内の別の場所へ移った)

`wamei/project-memo-posframe-show' が show の直後に dedicated を外して
いるので、小窓の中で `find-file' や `magit-status' を実行してもそのバッファ
は小窓自身に開く。以前あった「メモ以外のバッファが入ったら本文 window へ
引き渡す」経路 (\\='handoff) は、dedicated を保つという前提ごと無くなった
ので削除した。小窓に何が映っていても、フォーカスが小窓に留まっている限り
ここは nil のまま — 閉じるかどうかの判断 (映っているのが要求されたメモ
本人かどうか) はトグル側 (`wamei/project-memo--toggle') が
`wamei/project-memo--posframe-buffer-shown' を見て行う。

追跡しているバッファ (`wamei/project-memo--posframe-buffer') が死んでいたら
無条件に \='hide にする。dedicated を外す前は、メモバッファを kill すると
posframe.el がその dedicated window ごと child frame を道連れに削除していた
(posframe.el の `posframe--create-posframe' 参照、「buffer が消えたら
child frame も消す」という設計になっている)。dedicated を外した今はそうは
ならず、`replace-buffer-in-windows' は dedicated でない window を単に
`switch-to-prev-buffer' で無関係なバッファに差し替えるだけなのでフレームは
生きたまま残る — フォーカスは小窓に留まったままなので、下の \='hide 判定
(selected-frame が変わったか) には引っかからず、何もせずに放置すると小窓が
無関係なバッファを映したまま孤児化する。

ミニバッファが立っている間は「フォーカスはまだ外れていない」と見て nil を
返す。posframe の child frame は自分のミニバッファを持たず親フレームのもの
を使う (posframe.el)。そのためメモにフォーカスがあるまま `C-x C-f' や
`M-x' を始めると、コマンドの途中で `selected-frame' が親に変わってしまう。
これを \='hide と読むと、モジュールは追跡変数も hook も捨てるのに Emacs は
ミニバッファを抜けた後で child frame の window を選択し直すので、閉じ方の
分からないフレームが画面に残る。ミニバッファを抜けたあと本当に別の場所へ
フォーカスが移っていれば、次のコマンド境界で通常どおり \='hide になる。
バッファが死んでいる場合はこのガードより先に \='hide にする — 死んだ
バッファを抱えたまま待つ理由が無いため。

素の `C-g' (`this-command' が `keyboard-quit') は、上のどの判定より先に
無条件で \='hide にする。実機診断 (詳細は
`docs/superpowers/specs/2026-09-10-project-memo-posframe-design.md' の
「posframe を閉じる条件」参照): 実端末 (tmux + `emacs -nw') で小窓に
フォーカスがある状態で素の `C-g' を送ると、`keyboard-quit' が quit を
signal する過程で tty 側が `selected-frame' を端末本体のフレームへ戻し、
それきり戻らない — tty の
child frame は同じ端末画面への重ね描画でしかなく、GUI のような独立した
ウィンドウを持たないため。これは上の「フォーカスが外れた」判定に副作用的
に引っかかって閉じているだけで、GUI (独立した child frame を持つ) では
同じ再選択が起きないので閉じないままだった。この食い違いは設計ではなく
バグであり、ユーザーは「どちらの環境でも閉じる」を選んだ。`selected-frame'
の偶然の変化に環境ごと頼るのではなく `this-command' で直接見ることで、
tty と GUI のどちらでも同じに、かつ確実に閉じるようにする。ミニバッファを
`C-g' で取り消す方は `minibuffer-keyboard-quit' / `abort-minibuffers' に
なり `keyboard-quit' ではないので、この分岐には自然に引っかからない
(下のミニバッファガードに委ねる)。

この分岐を「ミニバッファが活性な間は閉じない」ガードより先に置いても
安全な理由: 実機 (tty) で `M-x' を `C-g' で取り消したときの
`this-command' は `abort-minibuffers' であり `keyboard-quit' には
ならないことを確認済み。逆に本物の `keyboard-quit' がミニバッファ読み取り
中に dispatch された場合は、`keyboard-quit' が無条件に quit を signal
する結果その recursive edit ごと巻き戻ってからでないと
`post-command-hook' は走らないので、この関数が呼ばれる時点では
`active-minibuffer-window' は既に nil になっている — つまりこの2つの
分岐が同時に問題を起こす (ミニバッファ活性中に \\='hide してしまう) 経路は
無い。

tty の実機診断では、この分岐と下の「フォーカスが外れた」分岐
(`(not (eq (selected-frame) frame))`) が同時に真になる (`this-command' が
`keyboard-quit' であることと、quit の副作用で `selected-frame' が
既に変わっていることの両方が同時に成り立つ) ことも確認している。`cond' は
最初に真になった節だけを採るのでこの分岐が先に \\='hide を返し、下の節は
評価すらされない。`wamei/project-memo-posframe-hide' は既に隠れている
状態からもう一度呼ばれても安全 (frame が生きているかの `when' で分岐し、
追跡変数と hook の後始末は無条件に行う設計、上のコメント参照) なので、
両方が同時に真であっても二重に閉じたり後始末が壊れたりはしない。"
  (when-let* ((frame (wamei/project-memo-posframe-frame)))
    (cond
     ((not (buffer-live-p wamei/project-memo--posframe-buffer)) 'hide)
     ((eq this-command 'keyboard-quit) 'hide)
     ((active-minibuffer-window) nil)
     ((not (eq (selected-frame) frame)) 'hide)
     (t nil))))

(defun wamei/project-memo--posframe-post-command ()
  "`post-command-hook' 用。フォーカスが外れていたら posframe を閉じる。

閉じる条件のうち「フォーカスが外れた」をここで見る (トグルで閉じるのは
`wamei/project-memo-toggle' 側)。dedicated を外しているので (
`wamei/project-memo-posframe-show')、小窓の中で `find-file' や
`magit-status' を実行してもそのバッファは小窓自身に開く。以前あった
「メモ以外のバッファが入ったら本文 window へ引き渡す」経路は、その前提
(dedicated を保つ) ごと無くなったので削除した。"
  (when (eq (wamei/project-memo--posframe-action) 'hide)
    (wamei/project-memo-posframe-hide))
  nil)

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
