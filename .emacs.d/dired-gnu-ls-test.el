;;; dired-gnu-ls-test.el --- tests for dired-gnu-ls -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-gnu-ls-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'dired)
(load (expand-file-name "dired-gnu-ls.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defmacro wamei/dired-gnu-ls-test--with-temp-dir (var &rest body)
  "一時ディレクトリを VAR に束縛して BODY を評価し、後で削除する。"
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "dgls-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun wamei/dired-gnu-ls-test--touch (dir name executable)
  "DIR に NAME を作り、EXECUTABLE が非 nil なら実行権を付けてパスを返す。"
  (let ((path (expand-file-name name dir)))
    (with-temp-file path (insert "#!/bin/sh\n"))
    (set-file-modes path (if executable #o755 #o644))
    path))

;;; wamei/dired-gnu-ls-find

(ert-deftest wamei/dired-gnu-ls-find-picks-first-executable ()
  "候補は並び順に見て、最初の実行可能なものを返す。"
  (wamei/dired-gnu-ls-test--with-temp-dir dir
    (let ((first (wamei/dired-gnu-ls-test--touch dir "a" t))
          (second (wamei/dired-gnu-ls-test--touch dir "b" t)))
      (should (equal first
                     (wamei/dired-gnu-ls-find
                      (list (expand-file-name "missing" dir) first second)))))))

(ert-deftest wamei/dired-gnu-ls-find-skips-non-executable ()
  "存在しても実行権の無いファイルは候補として採らない。"
  (wamei/dired-gnu-ls-test--with-temp-dir dir
    (let ((plain (wamei/dired-gnu-ls-test--touch dir "plain" nil))
          (exec (wamei/dired-gnu-ls-test--touch dir "exec" t)))
      (should (equal exec (wamei/dired-gnu-ls-find (list plain exec)))))))

(ert-deftest wamei/dired-gnu-ls-find-expands-tilde ()
  "候補の ~ は展開して判定する (既定の候補が ~/bin/gls を含むため)。"
  (wamei/dired-gnu-ls-test--with-temp-dir dir
    (wamei/dired-gnu-ls-test--touch dir "gls" t)
    (let ((process-environment (cons (concat "HOME=" (directory-file-name dir))
                                     process-environment)))
      (should (equal (expand-file-name "gls" dir)
                     (wamei/dired-gnu-ls-find '("~/gls")))))))

(ert-deftest wamei/dired-gnu-ls-find-nil-when-nothing-found ()
  "候補も PATH も外れたら nil を返す (BSD ls へ落とす判断は呼び出し側)。"
  (wamei/dired-gnu-ls-test--with-temp-dir dir
    (let ((exec-path nil))
      (should-not (wamei/dired-gnu-ls-find
                   (list (expand-file-name "missing" dir)))))))

(ert-deftest wamei/dired-gnu-ls-find-falls-back-to-exec-path ()
  "候補が全滅したときは PATH の gls も見る (別の入手経路のマシン向け)。"
  (wamei/dired-gnu-ls-test--with-temp-dir dir
    (wamei/dired-gnu-ls-test--touch dir "gls" t)
    (let ((exec-path (list dir)))
      (should (equal (expand-file-name "gls" dir)
                     (wamei/dired-gnu-ls-find
                      (list (expand-file-name "missing" dir))))))))

;;; switches の定数

(ert-deftest wamei/dired-gnu-ls-bsd-switches-have-no-gnu-options ()
  "BSD ls 用の switches に GNU 専用の長オプションを混ぜない。
混ざると /bin/ls が unrecognized option で落ち、dired が開けなくなる。"
  (should-not (string-match-p "--" wamei/dired-bsd-ls-switches)))

(ert-deftest wamei/dired-gnu-ls-switches-keep-group-directories-first ()
  "GNU ls を使えるときはディレクトリ先頭ソートを維持する。"
  (should (string-match-p "--group-directories-first"
                          wamei/dired-gnu-ls-switches)))

;;; wamei/dired-gnu-ls-configure

(ert-deftest wamei/dired-gnu-ls-configure-uses-gnu-when-available ()
  "GNU ls が見つかれば insert-directory-program と switches をそちらに向ける。"
  (wamei/dired-gnu-ls-test--with-temp-dir dir
    (let* ((gls (wamei/dired-gnu-ls-test--touch dir "gls" t))
           (insert-directory-program "ls")
           (dired-listing-switches "-al")
           (result (wamei/dired-gnu-ls-configure (list gls))))
      (should (equal gls result))
      (should (equal gls insert-directory-program))
      (should (equal wamei/dired-gnu-ls-switches dired-listing-switches)))))

(ert-deftest wamei/dired-gnu-ls-configure-falls-back-to-bsd ()
  "GNU ls が無ければ素の ls に戻し、switches も BSD 用に落とす。"
  (wamei/dired-gnu-ls-test--with-temp-dir dir
    (let* ((exec-path nil)
           (insert-directory-program "/opt/homebrew/bin/gls")
           (dired-listing-switches wamei/dired-gnu-ls-switches)
           (result (wamei/dired-gnu-ls-configure
                    (list (expand-file-name "missing" dir)))))
      (should-not result)
      (should (equal "ls" insert-directory-program))
      (should (equal wamei/dired-bsd-ls-switches dired-listing-switches)))))

(ert-deftest wamei/dired-gnu-ls-configure-stops-dired-probe-on-bsd ()
  "BSD ls に落ちたときは `--dired' を試させない。
GNU ls が無いと分かっているので、Emacs に探らせると
\"ls does not support --dired\" を毎回警告される。"
  (wamei/dired-gnu-ls-test--with-temp-dir dir
    (let ((exec-path nil)
          (dired-use-ls-dired 'unspecified))
      (wamei/dired-gnu-ls-configure (list (expand-file-name "missing" dir)))
      (should-not dired-use-ls-dired))))

(ert-deftest wamei/dired-gnu-ls-configure-leaves-dired-probe-on-gnu ()
  "GNU ls を使うときは `--dired' の対応判定を Emacs に委ねる。
候補の gls が本当に GNU 互換かはここでは分からないので t に決め打たない。"
  (wamei/dired-gnu-ls-test--with-temp-dir dir
    (let ((gls (wamei/dired-gnu-ls-test--touch dir "gls" t))
          (dired-use-ls-dired nil))
      (wamei/dired-gnu-ls-configure (list gls))
      (should (eq 'unspecified dired-use-ls-dired)))))

(provide 'dired-gnu-ls-test)
;;; dired-gnu-ls-test.el ends here
