;;; term-input-test.el --- tests for term-input -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l term-input-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "term-input.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; テスト用の ghostel スタブ

(defvar wamei/term-input-test--sent nil
  "スタブが受け取った送信内容。(KEY-NAME MODS) か `yank'。")

(defmacro wamei/term-input-test--with-ghostel (&rest body)
  "ghostel の送信関数をスタブに差し替えて BODY を実行する。"
  (declare (indent 0))
  `(let ((wamei/term-input-test--sent nil)
         (kill-ring nil)
         (kill-ring-yank-pointer nil)
         (last-command nil)
         (this-command nil))
     (cl-letf (((symbol-function 'ghostel-send-key)
                (lambda (key-name &optional mods)
                  (push (list key-name mods) wamei/term-input-test--sent)))
               ((symbol-function 'ghostel-yank)
                (lambda (&rest _) (push 'yank wamei/term-input-test--sent))))
       ,@body)))

(defun wamei/term-input-test--call-kill-line ()
  "コマンドループを模して `wamei/term-input-kill-line' を 1 回呼ぶ。"
  (setq this-command #'wamei/term-input-kill-line)
  (call-interactively #'wamei/term-input-kill-line)
  (setq last-command this-command))

;;; kill-line

(ert-deftest wamei/term-input-kill-line-saves-rest-of-line ()
  "point から行末までを kill-ring に入れ、C-k を端末へ送る。"
  (wamei/term-input-test--with-ghostel
    (with-temp-buffer
      (insert "$ echo hello world\n")
      (goto-char (point-min))
      (search-forward "hello")
      (backward-word)
      (wamei/term-input-test--call-kill-line)
      (should (equal (car kill-ring) "hello world"))
      (should (equal wamei/term-input-test--sent '(("k" "ctrl")))))))

(ert-deftest wamei/term-input-kill-line-appends-when-repeated ()
  "連続して呼ぶと Emacs の kill-line と同じく 1 つの kill に連結する。"
  (wamei/term-input-test--with-ghostel
    (with-temp-buffer
      (insert "$ foo\n")
      (goto-char (point-min))
      (search-forward "foo")
      (backward-word)
      (wamei/term-input-test--call-kill-line)
      ;; シェル側で行が消えた状態を模す
      (delete-region (point) (line-end-position))
      (insert "bar")
      (backward-word)
      (wamei/term-input-test--call-kill-line)
      (should (equal kill-ring '("foobar"))))))

(ert-deftest wamei/term-input-kill-line-at-eol-leaves-kill-ring ()
  "行末では zsh の kill-line 同様に何も切り取らず、kill-ring も汚さない。"
  (wamei/term-input-test--with-ghostel
    (with-temp-buffer
      (insert "$ foo\n")
      (goto-char (point-min))
      (end-of-line)
      (wamei/term-input-test--call-kill-line)
      (should-not kill-ring)
      (should (equal wamei/term-input-test--sent '(("k" "ctrl")))))))

(ert-deftest wamei/term-input-kill-line-ignores-trailing-spaces ()
  "端末が画面幅まで埋める行末の空白は kill に含めない。"
  (wamei/term-input-test--with-ghostel
    (with-temp-buffer
      (insert "$ foo   \n")
      (goto-char (point-min))
      (search-forward "foo")
      (backward-word)
      (wamei/term-input-test--call-kill-line)
      (should (equal (car kill-ring) "foo")))))

;;; クリップボードの画像判定

(defmacro wamei/term-input-test--with-clipboard (targets &rest body)
  "`gui-get-selection' が TARGETS を返すようにして BODY を実行する。
TARGETS が `error' なら選択が取れない環境 (tty など) を模す。"
  (declare (indent 1))
  `(cl-letf (((symbol-function 'gui-get-selection)
              (lambda (&optional _selection type)
                (if (eq ,targets 'error)
                    (error "No selection")
                  (when (eq type 'TARGETS) ,targets)))))
     ,@body))

(ert-deftest wamei/term-input-clipboard-image-p-detects-image ()
  "画像をコピーすると TARGETS に image/* が並ぶ。"
  (wamei/term-input-test--with-clipboard [TARGETS image/png image/tiff]
    (should (wamei/term-input--clipboard-image-p))))

(ert-deftest wamei/term-input-clipboard-image-p-rejects-text ()
  "テキストだけのときは nil。"
  (wamei/term-input-test--with-clipboard [TARGETS STRING]
    (should-not (wamei/term-input--clipboard-image-p))))

(ert-deftest wamei/term-input-clipboard-image-p-handles-no-selection ()
  "選択が空でも、取得できない環境でも落ちない。"
  (wamei/term-input-test--with-clipboard nil
    (should-not (wamei/term-input--clipboard-image-p)))
  (wamei/term-input-test--with-clipboard 'error
    (should-not (wamei/term-input--clipboard-image-p))))

;;; 貼り付け

(ert-deftest wamei/term-input-paste-sends-ctrl-v-for-image ()
  "画像なら C-v を端末へ送る (Claude が自分でクリップボードを読む)。"
  (wamei/term-input-test--with-ghostel
    (wamei/term-input-test--with-clipboard [TARGETS image/png]
      (call-interactively #'wamei/term-input-paste))
    (should (equal wamei/term-input-test--sent '(("v" "ctrl"))))))

(ert-deftest wamei/term-input-paste-yanks-for-text ()
  "画像でなければ従来どおり kill-ring から貼る。"
  (wamei/term-input-test--with-ghostel
    (wamei/term-input-test--with-clipboard [TARGETS STRING]
      (call-interactively #'wamei/term-input-paste))
    (should (equal wamei/term-input-test--sent '(yank)))))

;;; コピー (M-w)

(ert-deftest wamei/term-input-copy-without-mark-does-not-error ()
  "マークが一度も設定されていないバッファでも error にしない。
Claude Code はドラッグ選択を自分でクリップボードへコピーするので、その直後の
M-w には Emacs 側のリージョンもマークも無い。`kill-ring-save' の標準エラー
\(The mark is not set now) でデバッガに落ちないようにする。"
  (wamei/term-input-test--with-ghostel
    (with-temp-buffer
      (insert "$ echo hello\n")
      (should-not (mark t))
      (should-not (condition-case err
                      (progn (call-interactively #'wamei/term-input-copy) nil)
                    (error err)))
      (should-not kill-ring))))

(ert-deftest wamei/term-input-copy-with-region-saves-it ()
  "リージョンがあれば `kill-ring-save' と同じくコピーする。"
  (wamei/term-input-test--with-ghostel
    (with-temp-buffer
      (insert "$ echo hello world\n")
      (goto-char (point-min))
      (search-forward "hello")
      (push-mark (match-beginning 0) t t)
      (goto-char (match-end 0))
      (call-interactively #'wamei/term-input-copy)
      (should (equal (car kill-ring) "hello")))))

(ert-deftest wamei/term-input-copy-with-inactive-mark-copies-mark-to-point ()
  "マークが非アクティブでも `kill-ring-save' と同じく mark と point の間をコピーする。
copy mode で M-w した後は ghostel がマークを非アクティブにするので、その状態で
もう一度 M-w を押しても従来と同じ結果になることを固定する。"
  (wamei/term-input-test--with-ghostel
    (with-temp-buffer
      (insert "$ echo hello world\n")
      (goto-char (point-min))
      (search-forward "world")
      (push-mark (match-beginning 0) t nil)
      (goto-char (match-end 0))
      (deactivate-mark)
      (let ((mark-even-if-inactive t))
        (call-interactively #'wamei/term-input-copy))
      (should (equal (car kill-ring) "world")))))

;;; IME の変換中 overlay

;; nsterm.m の変数。batch では未定義なので special にしておく。
(defvar ns-working-overlay nil)
;; ghostel の buffer-local 変数 (端末カーソルの位置)。
(defvar-local ghostel--cursor-char-pos nil)

(ert-deftest wamei/term-input-ns-overlay-follows-cursor ()
  "再描画で置き去りになった変換中 overlay を端末カーソルへ張り直す。"
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((cursor (progn (goto-char (point-min)) (forward-line 2) (point)))
          (ov (make-overlay 1 1)))
      (setq-local ghostel--cursor-char-pos cursor)
      (let ((ns-working-overlay ov))
        (wamei/term-input--ns-keep-working-overlay))
      (should (= (overlay-start ov) cursor))
      (should (= (overlay-end ov) cursor)))))

(ert-deftest wamei/term-input-ns-overlay-ignores-other-buffer ()
  "別のバッファに属する overlay は動かさない。"
  (let ((other (generate-new-buffer " *other*")))
    (unwind-protect
        (let ((ov (with-current-buffer other (insert "abc") (make-overlay 1 1))))
          (with-temp-buffer
            (insert "line1\nline2\n")
            (setq-local ghostel--cursor-char-pos (point-max))
            (let ((ns-working-overlay ov))
              (wamei/term-input--ns-keep-working-overlay))
            (should (= (overlay-start ov) 1))))
      (kill-buffer other))))

(ert-deftest wamei/term-input-ns-overlay-without-cursor ()
  "端末カーソルの位置が分からないときは何もしない。"
  (with-temp-buffer
    (insert "line1\nline2\n")
    (let ((ov (make-overlay 1 1)))
      (setq-local ghostel--cursor-char-pos nil)
      (let ((ns-working-overlay ov))
        (wamei/term-input--ns-keep-working-overlay))
      (should (= (overlay-start ov) 1)))))

(ert-deftest wamei/term-input-ns-overlay-absent ()
  "変換中でない (overlay が無い) ときは黙って何もしない。"
  (with-temp-buffer
    (setq-local ghostel--cursor-char-pos 1)
    (let ((ns-working-overlay nil))
      (should-not (wamei/term-input--ns-keep-working-overlay)))))

(provide 'term-input-test)
;;; term-input-test.el ends here
