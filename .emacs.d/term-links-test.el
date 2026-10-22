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
          (wamei/term-links--index-cache (make-hash-table :test 'equal)))
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

;;; リンク付け

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

(provide 'term-links-test)
;;; term-links-test.el ends here
