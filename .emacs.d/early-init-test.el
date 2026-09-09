;;; early-init-test.el --- tests for frame geometry restore -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l early-init-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(load (expand-file-name "early-init.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; wamei/frame-geometry--merge

(ert-deftest wamei/frame-geometry--merge-normal ()
  "通常時は current の値をそのまま保存し、fullscreen は含めない。"
  (should (equal '((left . 100) (top . 50) (width . 120) (height . 40))
                 (wamei/frame-geometry--merge
                  nil
                  '((left . 100) (top . 50) (width . 120) (height . 40)
                    (fullscreen . nil))))))

(ert-deftest wamei/frame-geometry--merge-normal-overrides-saved ()
  "通常時は saved より current が優先される。"
  (should (equal '((left . 1) (top . 2) (width . 3) (height . 4))
                 (wamei/frame-geometry--merge
                  '((left . 900) (top . 900) (width . 900) (height . 900))
                  '((left . 1) (top . 2) (width . 3) (height . 4)
                    (fullscreen . nil))))))

(ert-deftest wamei/frame-geometry--merge-fullscreen-keeps-saved-size ()
  "フルスクリーン中は current の巨大な値を捨て、saved の通常時サイズを保持する。"
  (should (equal '((left . 100) (top . 50) (width . 120) (height . 40)
                   (fullscreen . fullboth))
                 (wamei/frame-geometry--merge
                  '((left . 100) (top . 50) (width . 120) (height . 40))
                  '((left . 0) (top . 0) (width . 480) (height . 130)
                    (fullscreen . fullboth))))))

(ert-deftest wamei/frame-geometry--merge-fullscreen-without-saved ()
  "saved がない状態でフルスクリーン終了した場合は fullscreen のみ保存する。"
  (should (equal '((fullscreen . maximized))
                 (wamei/frame-geometry--merge
                  nil
                  '((left . 0) (top . 0) (width . 480) (height . 130)
                    (fullscreen . maximized))))))

;;; wamei/frame-geometry--offscreen-p

(defconst wamei/frame-geometry-test--single '((0 0 1512 944)))
(defconst wamei/frame-geometry-test--dual '((0 0 1512 944) (1512 -300 2560 1440)))

(ert-deftest wamei/frame-geometry--offscreen-p-inside ()
  "ワークエリア内の座標は画面外ではない。"
  (should-not (wamei/frame-geometry--offscreen-p
               100 50 wamei/frame-geometry-test--single)))

(ert-deftest wamei/frame-geometry--offscreen-p-outside ()
  "どのワークエリアにも含まれない座標は画面外。"
  (should (wamei/frame-geometry--offscreen-p
           3000 200 wamei/frame-geometry-test--single)))

(ert-deftest wamei/frame-geometry--offscreen-p-second-monitor ()
  "2枚目のモニタ内なら画面外ではない (負の top を含む)。"
  (should-not (wamei/frame-geometry--offscreen-p
               2000 -100 wamei/frame-geometry-test--dual)))

(ert-deftest wamei/frame-geometry--offscreen-p-no-workareas ()
  "モニタ情報が取れないときは判定不能として nil を返す。"
  (should-not (wamei/frame-geometry--offscreen-p 3000 200 nil)))

(ert-deftest wamei/frame-geometry--offscreen-p-relative-coordinate ()
  "(+ N) / (- N) 形式の相対座標は Emacs に解釈させるため判定しない。"
  (should-not (wamei/frame-geometry--offscreen-p '(- 0) 200
                                                 wamei/frame-geometry-test--single)))

(ert-deftest wamei/frame-geometry--offscreen-p-boundary ()
  "ワークエリアの右下端は含まない (x+w は範囲外)。"
  (should (wamei/frame-geometry--offscreen-p
           1512 0 wamei/frame-geometry-test--single)))

;;; wamei/native-comp--library-options

(ert-deftest wamei/native-comp--library-options-keeps-only-existing-dirs ()
  "実在しないディレクトリは -L に含めない。
存在しないパスを渡すと gcc が警告を出すだけで実害はないが、
どこを見ているのかが分からなくなる。"
  (let ((dir (file-name-as-directory temporary-file-directory)))
    (should (equal (wamei/native-comp--library-options
                    (list dir "/no/such/dir/for/sure"))
                   (list (concat "-L" dir))))))

(ert-deftest wamei/native-comp--library-options-drops-duplicates ()
  "同じディレクトリが 2 つの glob から拾われても 1 つにする。"
  (let ((dir (file-name-as-directory temporary-file-directory)))
    (should (equal (wamei/native-comp--library-options (list dir dir))
                   (list (concat "-L" dir))))))

(ert-deftest wamei/native-comp--library-options-keeps-order ()
  "並び順は渡された順のまま。リンカは先に見つけたものを使うので順序が意味を持つ。"
  (let ((a (file-name-as-directory temporary-file-directory))
        (b (file-name-as-directory (expand-file-name "." temporary-file-directory))))
    (skip-unless (not (equal a b)))
    (should (equal (wamei/native-comp--library-options (list a b))
                   (list (concat "-L" a) (concat "-L" b))))))

(ert-deftest wamei/native-comp--library-options-empty ()
  (should-not (wamei/native-comp--library-options nil))
  (should-not (wamei/native-comp--library-options '("/no/such/dir"))))

(ert-deftest wamei/native-comp--library-dirs-finds-libemutls ()
  "この機能が存在する理由は libemutls_w.a が見つからないことなので、
実際にそれを含むディレクトリを拾えることを確かめる。
gcc が入っていない環境ではスキップする。"
  (let ((dirs (wamei/native-comp--library-dirs)))
    (skip-unless dirs)
    (should (seq-some (lambda (dir)
                        (file-exists-p (expand-file-name "libemutls_w.a" dir)))
                      dirs))))

(provide 'early-init-test)
;;; early-init-test.el ends here
