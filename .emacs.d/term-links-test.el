;;; term-links-test.el --- tests for term-links -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l term-links-test.el -f ert-run-tests-batch-and-exit
;;
;; ghostel のリンク付け (ghostel-links.el) は package-initialize で読む。
;; ネイティブモジュールは使わないので、無い旨の警告が出ても構わない。
;;; Code:

(require 'ert)
(require 'cl-lib)
(package-initialize)
(require 'ghostel)
(load (expand-file-name "term-links.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro wamei/term-links-test--with-project (files &rest body)
  "FILES (相対パスのリスト) を置いた一時ディレクトリを `root' に束縛して BODY を評価する。
プロジェクトの判定は `project-find-functions' を差し替えて ROOT を返す。"
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (make-temp-file "term-links" t)))
          (project-find-functions (list (lambda (_dir) (cons 'transient root))))
          (wamei/term-links--index-cache (make-hash-table :test 'equal))
          (wamei/term-links--refresh-pending (make-hash-table :test 'equal))
          ;; 索引の作り直しはアイドル時に予約されるので、テストでは即座に行う
          (wamei/term-links--schedule-function
           (lambda (_secs _repeat fn &rest args)
             (apply fn args)
             (let ((deadline (+ (float-time) 8)))
               (while (and (> (hash-table-count wamei/term-links--refresh-pending) 0)
                           (< (float-time) deadline))
                 (accept-process-output nil 0.02))))))
     (unwind-protect
         (progn
           (dolist (file ,files)
             (let ((path (expand-file-name file root)))
               (make-directory (file-name-directory path) t)
               (write-region "1\n2\n3\n" nil path nil 'silent)))
           ,@body)
       (delete-directory root t))))

;;; 解決

(ert-deftest wamei/term-links-test-resolve-prefers-default-directory ()
  "default-directory 直下にあればそれを使う (プロジェクトに同名があっても)。"
  (wamei/term-links-test--with-project '("init.el" "sub/init.el")
    (should (equal (wamei/term-links--resolve "init.el" root)
                   (expand-file-name "init.el" root)))))

(ert-deftest wamei/term-links-test-resolve-unique-basename-in-project ()
  "直下に無ければ、プロジェクト内で basename が一意のファイルを使う。"
  (wamei/term-links-test--with-project '(".emacs.d/init.el" ".zshrc")
    (should (equal (wamei/term-links--resolve "init.el" root)
                   (expand-file-name ".emacs.d/init.el" root)))))

(ert-deftest wamei/term-links-test-resolve-ambiguous-is-nil ()
  "同じ basename が複数あれば決められないので nil。"
  (wamei/term-links-test--with-project '("a/foo.el" "b/foo.el")
    (should-not (wamei/term-links--resolve "foo.el" root))))

(ert-deftest wamei/term-links-test-resolve-missing-is-nil ()
  "どこにも無い名前 (バージョン番号など) は nil。"
  (wamei/term-links-test--with-project '("a/foo.el")
    (should-not (wamei/term-links--resolve "2.1.308" root))
    (should-not (wamei/term-links--resolve "e.g" root))))

(ert-deftest wamei/term-links-test-resolve-sees-new-files-after-interval ()
  "一覧に無い名前を引いたとき、作り直しの間隔を過ぎていれば一覧を作り直す。"
  (wamei/term-links-test--with-project '("a/foo.el")
    (let ((wamei/term-links-index-refresh-interval 0))
      (should-not (wamei/term-links--resolve "bar.el" root))
      (write-region "" nil (expand-file-name "a/bar.el" root) nil 'silent)
      (should (equal (wamei/term-links--resolve "bar.el" root)
                     (expand-file-name "a/bar.el" root))))))

(ert-deftest wamei/term-links-test-resolve-defers-index-build ()
  "索引の作り直しは予約するだけで、その場では作らない (描画中に止まらない)。"
  (wamei/term-links-test--with-project '("a/foo.el")
    (let* ((scheduled nil)
           (wamei/term-links--schedule-function
            (lambda (_secs _repeat fn &rest args) (push (cons fn args) scheduled))))
      (should-not (wamei/term-links--resolve "foo.el" root))
      (should-not (wamei/term-links--resolve "foo.el" root))
      ;; 予約は 1 回だけ
      (should (= (length scheduled) 1))
      (apply (caar scheduled) (cdar scheduled))
      (let ((deadline (+ (float-time) 8)))
        (while (and (> (hash-table-count wamei/term-links--refresh-pending) 0)
                    (< (float-time) deadline))
          (accept-process-output nil 0.02)))
      (should (equal (wamei/term-links--resolve "foo.el" root)
                     (expand-file-name "a/foo.el" root))))))

;;; リンク付け

(ert-deftest wamei/term-links-test-index-does-not-read-project-in-ui ()
  (wamei/term-links-test--with-project '("src/example.el")
    (let ((project (cons 'transient root)))
      (unwind-protect
          (cl-letf (((symbol-function 'project-files)
                     (lambda (&rest _) (ert-fail "UI scanned project synchronously"))))
            (puthash root t wamei/term-links--refresh-pending)
            (wamei/term-links--refresh-index project)
            (should-not (gethash root wamei/term-links--index-cache))
            (let ((deadline (+ (float-time) 8)))
              (while (and (gethash root wamei/term-links--refresh-pending)
                          (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should (equal (gethash "example.el" (cdr (gethash root wamei/term-links--index-cache)))
                           (list (expand-file-name "src/example.el" root)))))
        (when (fboundp 'wamei/background-cancel)
          (wamei/background-cancel (cons 'term-links root)))))))

(defun wamei/term-links-test--links ()
  "現在のバッファのリンク (文字列 . help-echo) を先頭から並べる。"
  (let ((pos (point-min)) links)
    (while (setq pos (text-property-not-all pos (point-max) 'help-echo nil))
      (let ((stop (next-single-property-change pos 'help-echo nil (point-max))))
        (push (cons (buffer-substring-no-properties pos stop)
                    (get-text-property pos 'help-echo))
              links)
        (setq pos stop)))
    (nreverse links)))

(defmacro wamei/term-links-test--in-terminal (text &rest body)
  "TEXT を入れた一時バッファで BODY を評価する。カーソル行の除外が効かないよう末尾に空行を置く。"
  (declare (indent 1))
  `(with-temp-buffer
     (setq default-directory root)
     (insert ,text "\n\n")
     (setq-local ghostel--cursor-char-pos (point-max))
     ,@body))

(ert-deftest wamei/term-links-test-detect-bare-names ()
  "/ を含まないファイル名を、行番号の有無を問わずリンクにする。"
  (wamei/term-links-test--with-project '(".emacs.d/init.el" "ai-panel.el" ".zshrc")
    (wamei/term-links-test--in-terminal
        "see init.el:2 and `ai-panel.el` or .zshrc, not 2.1.308."
      (wamei/term-links--detect-bare-names (point-min) (point-max))
      (should (equal (wamei/term-links-test--links)
                     `(("init.el:2" . ,(concat "fileref:" root ".emacs.d/init.el:2"))
                       ("ai-panel.el" . ,(concat "fileref:" root "ai-panel.el"))
                       (".zshrc" . ,(concat "fileref:" root ".zshrc"))))))))

(ert-deftest wamei/term-links-test-detect-leaves-paths-to-ghostel ()
  "/ を含むパスの一部 (ディレクトリ名・末尾のファイル名) は拾わない。"
  (wamei/term-links-test--with-project '("a.d/init.el" "init.el")
    (wamei/term-links-test--in-terminal "a.d/init.el x/init.el"
      (wamei/term-links--detect-bare-names (point-min) (point-max))
      (should-not (wamei/term-links-test--links)))))

(ert-deftest wamei/term-links-test-detect-keeps-existing-links ()
  "OSC 8 などで既にリンクになっている所は触らない。"
  (wamei/term-links-test--with-project '("init.el")
    (wamei/term-links-test--in-terminal "init.el"
      (put-text-property (point-min) (+ (point-min) 7) 'help-echo "file:///elsewhere")
      (wamei/term-links--detect-bare-names (point-min) (point-max))
      (should (equal (wamei/term-links-test--links)
                     '(("init.el" . "file:///elsewhere")))))))

(ert-deftest wamei/term-links-test-detect-skips-cursor-line ()
  "入力中の行 (カーソル行) はリンクにしない (ghostel 本体と同じ)。"
  (wamei/term-links-test--with-project '("init.el")
    (with-temp-buffer
      (setq default-directory root)
      (insert "init.el")
      (setq-local ghostel--cursor-char-pos (point-max))
      (wamei/term-links--detect-bare-names (point-min) (point-max))
      (should-not (wamei/term-links-test--links)))))

(ert-deftest wamei/term-links-test-advice-survives-ghostel-scan ()
  "ghostel の走査の後に足すので、ghostel が自分のリンクとして消しても残る。"
  (wamei/term-links-test--with-project '("init.el" "src/main.el")
    (wamei/term-links-test--in-terminal "init.el src/main.el:1"
      (wamei/term-links-mode 1)
      (unwind-protect
          (progn
            (ghostel--detect-urls)
            (ghostel--detect-urls)
            (should (equal (mapcar #'car (wamei/term-links-test--links))
                           '("init.el" "src/main.el:1"))))
        (wamei/term-links-mode -1)))))

;;; 端末幅で改行されたパス

(defun wamei/term-links-test--split (text cols indent)
  "TEXT を COLS 桁で切り、2 行目以降の頭に INDENT 桁の空白を入れた文字列。
Claude Code が長い行を自分で改行して字下げを揃えるのと同じ形にする。"
  (let ((lines nil)
        (rest text)
        (first t))
    (while (> (string-width rest) 0)
      (let* ((head (if first "" (make-string indent ?\s)))
             (room (- cols (string-width head)))
             (piece (truncate-string-to-width rest room)))
        (push (concat head piece) lines)
        (setq rest (substring rest (length piece))
              first nil)))
    (string-join (nreverse lines) "\n")))

(ert-deftest wamei/term-links-test-detect-hard-wrapped-path ()
  "端末幅ちょうどで切られ、次の行が字下げして続くパスを繋いでリンクにする。"
  (wamei/term-links-test--with-project '("scratchpad/cow-database-phase0-ledger.md")
    (let* ((path (expand-file-name "scratchpad/cow-database-phase0-ledger.md" root))
           (cols (+ 2 (string-width "記録は、") (/ (length path) 2)))
           (text (wamei/term-links-test--split
                  (concat "  記録は、" path " に残しています。") cols 2)))
      (wamei/term-links-test--in-terminal text
        (setq-local ghostel--term-cols cols)
        (should (string-search "\n" (buffer-substring (point-min) (- (point-max) 2))))
        (wamei/term-links--detect-hard-wrapped (point-min) (point-max))
        (let ((links (wamei/term-links-test--links)))
          (should (= (length links) 2))
          (should (equal (apply #'concat (mapcar #'car links))
                         (concat path)))
          (should (equal (delete-dups (mapcar #'cdr links))
                         (list (concat "fileref:" path))))
          ;; 2 つの断片は 1 つのリンクとして扱う
          (let ((ids (let ((pos (point-min)) ids)
                       (while (setq pos (text-property-not-all pos (point-max) 'help-echo nil))
                         (push (get-text-property pos 'ghostel-link-id) ids)
                         (setq pos (next-single-property-change pos 'help-echo nil (point-max))))
                       ids)))
            (should (equal (car ids) (cadr ids)))))))))

(ert-deftest wamei/term-links-test-detect-hard-wrapped-with-line ()
  "続きの行に :LINE が付いていれば行番号付きのリンクにする。"
  (wamei/term-links-test--with-project '("src/main.el")
    (with-temp-buffer
      (setq default-directory root)
      (insert "xyz src/ma\n  in.el:2 です\n\n")
      (setq-local ghostel--cursor-char-pos (point-max))
      (setq-local ghostel--term-cols 10)
      (wamei/term-links--detect-hard-wrapped (point-min) (point-max))
      (should (equal (wamei/term-links-test--links)
                     `(("src/ma" . ,(concat "fileref:" root "src/main.el:2"))
                       ("in.el:2" . ,(concat "fileref:" root "src/main.el:2"))))))))

(ert-deftest wamei/term-links-test-hard-wrap-needs-full-row-and-existing-file ()
  "行が端末幅に届いていない、続きが別の語、繋いでも実在しなければ繋がない。"
  (wamei/term-links-test--with-project '("src/main.el")
    (dolist (text '("xy src/ma\n  in.el"       ; 9 桁 (端末幅 10 に届かない)
                    "xyz src/ma\n  (in.el"     ; 続きの頭が括弧
                    "xyz src/ma\n  ix.el"))    ; src/maix.el は無い
      (with-temp-buffer
        (setq default-directory root)
        (insert text "\n\n")
        (setq-local ghostel--cursor-char-pos (point-max))
        (setq-local ghostel--term-cols 10)
        (wamei/term-links--detect-hard-wrapped (point-min) (point-max))
        (should-not (wamei/term-links-test--links))))))

(ert-deftest wamei/term-links-test-hard-wrap-survives-ghostel-scan ()
  "ghostel の走査を通しても、繋いだパスのリンクが残る。"
  (wamei/term-links-test--with-project '("src/main.el")
    (with-temp-buffer
      (setq default-directory root)
      (insert "xyz src/ma\n  in.el\n\n")
      (setq-local ghostel--cursor-char-pos (point-max))
      (setq-local ghostel--term-cols 10)
      (wamei/term-links-mode 1)
      (unwind-protect
          (progn
            (ghostel--detect-urls)
            (ghostel--detect-urls)
            (should (equal (mapcar #'car (wamei/term-links-test--links))
                           '("src/ma" "in.el"))))
        (wamei/term-links-mode -1)))))

;;; マウスオーバーで断片をまとめて光らせる

(defun wamei/term-links-test--highlighted ()
  "ホバー用 overlay が掛かっている文字列のリスト。"
  (mapcar (lambda (ov) (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
          (sort (copy-sequence wamei/term-links--hover-overlays)
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defmacro wamei/term-links-test--with-wrapped-link (&rest body)
  "2 行に分かれたリンク src/ma + in.el と、1 行のリンク init.el を置いて BODY を評価する。"
  (declare (indent 0))
  `(wamei/term-links-test--with-project '("src/main.el" "init.el")
     (with-temp-buffer
       (setq default-directory root)
       (insert "xyz src/ma\n  in.el と init.el\n\n")
       (setq-local ghostel--cursor-char-pos (point-max))
       (setq-local ghostel--term-cols 10)
       (wamei/term-links--detect-bare-names (point-min) (point-max))
       (wamei/term-links--detect-hard-wrapped (point-min) (point-max))
       (unwind-protect (progn ,@body)
         (wamei/term-links--hover-clear)))))

(ert-deftest wamei/term-links-test-hover-highlights-all-fragments ()
  "どちらの断片に乗っても、同じリンクの断片をすべて光らせる。"
  (wamei/term-links-test--with-wrapped-link
    (wamei/term-links--hover-highlight (current-buffer) 6)   ; src/ma の上
    (should (equal (wamei/term-links-test--highlighted) '("src/ma" "in.el")))
    (wamei/term-links--hover-highlight (current-buffer) 14)  ; in.el の上
    (should (equal (wamei/term-links-test--highlighted) '("src/ma" "in.el")))))

(ert-deftest wamei/term-links-test-hover-ignores-single-fragment-links ()
  "1 つの断片だけのリンクは mouse-face で足りるので何もしない。リンクの外でも消える。"
  (wamei/term-links-test--with-wrapped-link
    (wamei/term-links--hover-highlight (current-buffer) 6)
    (wamei/term-links--hover-highlight (current-buffer) (- (point-max) 4)) ; init.el の上
    (should-not (wamei/term-links-test--highlighted))
    (wamei/term-links--hover-highlight (current-buffer) 6)
    (wamei/term-links--hover-highlight (current-buffer) 1)                 ; リンクの外
    (should-not (wamei/term-links-test--highlighted))))

(ert-deftest wamei/term-links-test-hover-follows-show-help ()
  "ヘルプの表示 (マウスが乗った) で光らせ、nil (離れた) で消す。"
  (wamei/term-links-test--with-wrapped-link
    (let ((buffer (current-buffer)))
      (cl-letf (((symbol-function 'wamei/term-links--mouse-buffer-pos)
                 (lambda () (cons buffer 6))))
        (wamei/term-links--on-show-help "fileref:x")
        (should (equal (wamei/term-links-test--highlighted) '("src/ma" "in.el")))
        (wamei/term-links--on-show-help nil)
        (should-not (wamei/term-links-test--highlighted))))))

(ert-deftest wamei/term-links-test-mouse-outside-frame-is-nil ()
  "マウスが frame の外 (負の座標) にあるときは位置を求めない。"
  ;; `posn-at-x-y' は負の座標で wrong-type-argument wholenump を投げる
  (dolist (xy '((448 . -25) (-3 . 10)))
    (cl-letf (((symbol-function 'mouse-pixel-position)
               (lambda () (cons (selected-frame) xy))))
      (should-not (wamei/term-links--mouse-buffer-pos)))))

;;; 右クリックメニューでリンクのアドレスをコピーする

(defun wamei/term-links-test--context-menu-at (pos)
  "カレントバッファの POS で右クリックしたときのメニュー。"
  (set-window-buffer (selected-window) (current-buffer))
  ;; batch では `posn-at-point' が nil を返すので、位置情報を組み立てる
  ;; (posn の形は (WINDOW POS-OR-AREA (X . Y) TIMESTAMP ...))
  (let ((click (list 'down-mouse-3 (list (selected-window) pos '(0 . 0) 0))))
    (wamei/term-links-context-menu (make-sparse-keymap) click)))

(defun wamei/term-links-test--menu-copy (menu)
  "MENU のコピーの項目を (ラベル . 実行後の kill-ring の先頭) のリストで返す。"
  ;; `lookup-key' は menu-item の中のコマンドしか返さないので項目ごと取る
  (let (items)
    (dolist (entry (cdr menu))
      (when (and (consp entry)
                 (string-prefix-p "wamei-term-links-copy-" (symbol-name (car entry))))
        (let ((kill-ring nil))
          (funcall (nth 3 entry))
          (push (cons (nth 2 entry) (car kill-ring)) items))))
    (nreverse items)))

(ert-deftest wamei/term-links-test-context-menu-copies-url ()
  "Web のリンクでは URL をそのままコピーする。"
  (with-temp-buffer
    (insert "see https://example.com/a?b=1 now")
    (add-text-properties 5 30 `(help-echo "https://example.com/a?b=1" keymap ,ghostel-link-map))
    (should (equal (wamei/term-links-test--menu-copy
                    (wamei/term-links-test--context-menu-at 10))
                   '(("Copy Link Address" . "https://example.com/a?b=1"))))))

(ert-deftest wamei/term-links-test-context-menu-copies-file-paths ()
  "ファイルのリンク (fileref:) は絶対・プロジェクトからの相対・~ 始まりの 3 つを出す。
どれも fileref: を外し、行番号を残す。"
  (wamei/term-links-test--with-project '("src/main.el")
    ;; ROOT をホームディレクトリとみなす
    (let ((abbreviated-home-dir
           (concat "\\`" (regexp-quote (directory-file-name root)) "\\(/\\|\\'\\)"))
          (file (expand-file-name "src/main.el" root)))
      (with-temp-buffer
        (setq default-directory (expand-file-name "src/" root))
        (insert "src/main.el:2")
        (add-text-properties 1 14 `(help-echo ,(concat "fileref:" file ":2")
                                              keymap ,ghostel-link-map))
        (should (equal (wamei/term-links-test--menu-copy
                        (wamei/term-links-test--context-menu-at 3))
                       `(("Copy Absolute Path" . ,(concat file ":2"))
                         ("Copy Relative Path" . "src/main.el:2")
                         ("Copy Home-Relative Path" . "~/src/main.el:2"))))))))

(ert-deftest wamei/term-links-test-context-menu-relative-without-project ()
  "プロジェクトの外では default-directory からの相対にする。"
  (let* ((dir (file-name-as-directory (make-temp-file "term-links" t)))
         (project-find-functions nil))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory dir)
          (insert "a.el")
          (add-text-properties 1 5 `(help-echo ,(concat "fileref:" dir "a.el")
                                               keymap ,ghostel-link-map))
          (should (equal (cdr (assoc "Copy Relative Path"
                                     (wamei/term-links-test--menu-copy
                                      (wamei/term-links-test--context-menu-at 2))))
                         "a.el")))
      (delete-directory dir t))))

(ert-deftest wamei/term-links-test-context-menu-only-on-links ()
  "リンクの外ではメニューに何も足さない。"
  (with-temp-buffer
    (insert "plain text")
    ;; ツールチップの help-echo はリンクではない
    (put-text-property 1 6 'help-echo "mouse-1: visit")
    (should-not (wamei/term-links-test--menu-copy
                 (wamei/term-links-test--context-menu-at 3)))))

(provide 'term-links-test)
;;; term-links-test.el ends here
