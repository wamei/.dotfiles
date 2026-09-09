;;; early-init.el --- pre-frame initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; 前回終了時のフレーム位置・サイズ・フルスクリーン状態を復元する。
;;
;; Emacs の起動順は
;;   early-init.el -> frame-initialize (初期フレーム生成) -> init.el
;;                 -> frame-notice-user-settings
;; なので、init.el で `initial-frame-alist' を設定しても効きはするが、
;; デフォルトサイズで表示された後にリサイズされて見える。フレーム生成前に
;; 読まれる early-init.el に置くことで、最初から目的のサイズで表示する。
;;
;; あわせて、ネイティブコンパイラがリンカに渡すオプションもここで決める。
;; init.el で subr に advice を当てると trampoline のコンパイルが走るので、
;; それより前に設定されている必要がある。

;;; Code:

(require 'seq)

(defvar wamei/frame-geometry-file
  (expand-file-name "frame-geometry.eld" user-emacs-directory)
  "フレームジオメトリを保存するファイル。
マシンローカルな状態なので dotfiles リポジトリの外に置く。")

(defconst wamei/frame-geometry--params '(left top width height fullscreen)
  "保存対象のフレームパラメータ。")


;;; 純粋関数 (early-init-test.el でテストする)

(defun wamei/frame-geometry--merge (saved current)
  "保存すべきジオメトリ alist を返す。
SAVED は前回保存した alist (なければ nil)、CURRENT は現在のフレームから
取得した alist。

CURRENT がフルスクリーン/最大化状態のとき、その left/top/width/height は
画面いっぱいの値になっている。これで上書きするとフルスクリーンを解除した
ときに戻るべき通常サイズが失われるため、SAVED 側の値を保持する。"
  (let* ((fullscreen (alist-get 'fullscreen current))
         (geometry (if fullscreen saved current)))
    (delq nil
          (append
           (mapcar (lambda (key)
                     (let ((cell (assq key geometry)))
                       (and cell (cons key (cdr cell)))))
                   '(left top width height))
           (and fullscreen (list (cons 'fullscreen fullscreen)))))))

(defun wamei/frame-geometry--offscreen-p (left top workareas)
  "LEFT TOP がどの WORKAREAS にも含まれないとき non-nil を返す。
WORKAREAS は (X Y WIDTH HEIGHT) のリスト。外部ディスプレイを外した状態で
起動したときにフレームが画面外に出るのを防ぐために使う。

座標が (+ N) / (- N) のような相対指定のとき、および WORKAREAS が空で
判定できないときは nil を返す (Emacs の解釈に任せる)。"
  (and workareas
       (integerp left) (integerp top)
       (not (seq-some
             (lambda (workarea)
               (pcase-let ((`(,x ,y ,width ,height) workarea))
                 (and (<= x left) (< left (+ x width))
                      (<= y top)  (< top  (+ y height)))))
             workareas))))


;;; ネイティブコンパイラのリンカ設定 (early-init-test.el でテストする)

(defconst wamei/native-comp--library-globs
  '("/opt/homebrew/lib/gcc/current"
    "/opt/homebrew/Cellar/gcc/*/lib/gcc/current/gcc/*/*")
  "gcc のランタイムライブラリを探すワイルドカード。
Homebrew の gcc は libemutls_w.a を
`.../lib/gcc/current/gcc/<target>/<version>/' に置く。target
\(aarch64-apple-darwin24 など) と version は gcc の更新で変わるので、
固定せずワイルドカードで拾う。")

(defun wamei/native-comp--library-options (dirs)
  "DIRS のうち実在するものを -L オプションのリストにして返す。
重複は除き、順序は保つ (リンカは先に見つけたものを使う)。"
  (mapcar (lambda (dir) (concat "-L" dir))
          (seq-uniq (seq-filter #'file-directory-p dirs))))

(defun wamei/native-comp--library-dirs ()
  "gcc のランタイムライブラリがあるディレクトリを探す。"
  (seq-mapcat #'file-expand-wildcards wamei/native-comp--library-globs))


;;; 入出力

(defun wamei/frame-geometry--read ()
  "保存済みのジオメトリ alist を読む。読めなければ nil。"
  (when (file-readable-p wamei/frame-geometry-file)
    (with-temp-buffer
      (insert-file-contents wamei/frame-geometry-file)
      (goto-char (point-min))
      (ignore-errors (read (current-buffer))))))

(defun wamei/frame-geometry--write (geometry)
  "GEOMETRY を `wamei/frame-geometry-file' に書き出す。"
  (with-temp-file wamei/frame-geometry-file
    (insert ";; -*- lisp-data -*-\n"
            ";; Written by early-init.el.  Machine-local; do not track in git.\n")
    (prin1 geometry (current-buffer))
    (insert "\n")))

(defun wamei/frame-geometry--workareas ()
  "接続中の各モニタのワークエリアのリストを返す。取得できなければ nil。"
  (delq nil (mapcar (lambda (attributes) (alist-get 'workarea attributes))
                    (ignore-errors (display-monitor-attributes-list)))))

(defun wamei/frame-geometry--target-frame ()
  "保存対象の GUI フレームを返す。"
  (let ((frame (selected-frame)))
    (if (and (frame-live-p frame) (display-graphic-p frame))
        frame
      (seq-find (lambda (f) (and (frame-live-p f) (display-graphic-p f)))
                (frame-list)))))


;;; エントリポイント

(defun wamei/frame-geometry-save ()
  "現在のフレームジオメトリを保存する。`kill-emacs-hook' 用。"
  ;; 終了処理を止めないよう、失敗してもメッセージに留める。
  (with-demoted-errors "frame-geometry: save failed: %S"
    (when-let* ((frame (wamei/frame-geometry--target-frame)))
      (wamei/frame-geometry--write
       (wamei/frame-geometry--merge
        (wamei/frame-geometry--read)
        (mapcar (lambda (param) (cons param (frame-parameter frame param)))
                wamei/frame-geometry--params))))))

(defun wamei/frame-geometry-restore ()
  "保存済みジオメトリを `initial-frame-alist' に反映する。"
  (with-demoted-errors "frame-geometry: restore failed: %S"
    (dolist (cell (wamei/frame-geometry--read))
      (setq initial-frame-alist
            (cons cell (assq-delete-all (car cell) initial-frame-alist))))))

(defun wamei/frame-geometry-ensure-visible (&optional frame)
  "FRAME が画面外にあれば主モニタの左上へ移動する。`window-setup-hook' 用。
モニタ構成が前回終了時から変わっている場合に効く。"
  (with-demoted-errors "frame-geometry: %S"
    (let ((frame (or frame (selected-frame)))
          (workareas (wamei/frame-geometry--workareas)))
      (when (and (display-graphic-p frame)
                 (wamei/frame-geometry--offscreen-p
                  (frame-parameter frame 'left)
                  (frame-parameter frame 'top)
                  workareas))
        (pcase-let ((`(,x ,y ,_width ,_height) (car workareas)))
          (set-frame-position frame x y))))))

(defun wamei/native-comp-setup-driver-options ()
  "ネイティブコンパイラが gcc のランタイムライブラリを見つけられるようにする。

libgccjit は `gcc' という名前でドライバを起動するが、macOS で PATH から引ける
gcc は Apple clang なので、gcc 固有のライブラリ検索パスを持たない。その結果
libemutls_w.a が見つからず

  ld: library \='emutls_w\=' not found

でネイティブコンパイルが失敗する。init.el が subr に advice を当てた時点で
trampoline の生成が走るので、起動そのものが止まる。

環境変数 LIBRARY_PATH が通っていれば動くが、GUI 起動 (Finder/Dock) でも
tty 起動でも確実に効くよう、リンカオプションとして明示的に渡す。
`native-comp-driver-options' は comp が読まれるまで未 bound だが、先に値を
入れておけば後の `defcustom' には上書きされない。"
  (when (eq system-type 'darwin)
    (when-let* ((options (wamei/native-comp--library-options
                          (wamei/native-comp--library-dirs))))
      (setq native-comp-driver-options
            (append options (and (boundp 'native-comp-driver-options)
                                 native-comp-driver-options))))))

(wamei/native-comp-setup-driver-options)

;; batch (テスト実行時など) では副作用を起こさない。
(unless noninteractive
  (wamei/frame-geometry-restore)
  (add-hook 'window-setup-hook #'wamei/frame-geometry-ensure-visible)
  (add-hook 'kill-emacs-hook #'wamei/frame-geometry-save))

(provide 'early-init)
;;; early-init.el ends here
