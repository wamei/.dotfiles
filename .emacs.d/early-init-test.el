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

(provide 'early-init-test)
;;; early-init-test.el ends here
