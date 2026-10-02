;;; header-tabs-test.el --- tests for header-tabs -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l header-tabs-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)

(load (expand-file-name "header-tabs.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defun header-tabs-test--tabs (string)
  "STRING をタブごとの (ID . 文字列) にする。ID は各タブの `header-tabs-test-id'。"
  (let ((pos 0) tabs)
    (while (< pos (length string))
      (let ((next (or (next-single-property-change pos 'header-tabs-test-id string)
                      (length string)))
            (id (get-text-property pos 'header-tabs-test-id string)))
        (when id
          (push (cons id (substring string pos next)) tabs))
        (setq pos next)))
    (nreverse tabs)))

(defun header-tabs-test--tab (id label &rest plist)
  "ID と LABEL のタブ。PLIST はそのまま足す。"
  (append (list :label label :properties (list 'header-tabs-test-id id)) plist))

(defun header-tabs-test--faces-at (string pos)
  "STRING の POS に付いている face の一覧。"
  (let ((face (get-text-property pos 'face string)))
    (if (and (listp face) (not (keywordp (car face)))) face (list face))))

(defun header-tabs-test--align-to (text)
  "タブ TEXT の末尾の詰め物の `:align-to'。"
  (plist-get (cdr (get-text-property (1- (length text)) 'display text)) :align-to))

;;; 等分

(ert-deftest wamei/header-tabs-split-the-width-equally ()
  "タブは window の幅を等分する。各タブの末尾の詰め物が次の境目
\(window の幅の k/n ピクセル) まで伸び、最後のタブは右端まで伸びる。
header-line は window の左端 (フリンジの外側) から描かれるが、`:align-to' は
本文の左端から測るので、左のフリンジのぶん (ここでは 8px) 引いて指定する。"
  (let ((tabs (header-tabs-test--tabs
               (wamei/header-tabs-render
                (list (header-tabs-test--tab 1 "a")
                      (header-tabs-test--tab 2 "b")
                      (header-tabs-test--tab 3 "c"))
                80 1000 8))))
    (should (equal (mapcar (lambda (tab) (header-tabs-test--align-to (cdr tab))) tabs)
                   '((325) (659) (992))))))

(ert-deftest wamei/header-tabs-label-fits-its-share ()
  "ラベルは 1 タブぶんの桁数に収まるよう切り詰める (詰め物は後戻りできない)。"
  (let ((text (cdr (car (header-tabs-test--tabs
                         (wamei/header-tabs-render
                          (list (header-tabs-test--tab 1 (make-string 200 ?x))
                                (header-tabs-test--tab 2 "b"))
                          40 400))))))
    ;; 詰め物 (末尾の 1 文字) を除いて 40 / 2 = 20 桁未満
    (should (< (string-width (substring text 0 -1)) 20))
    (should (string-match-p "…\\'" (substring text 0 -1)))))

(ert-deftest wamei/header-tabs-head-is-kept-and-counted ()
  "ラベルの前に置く HEAD (● など) は切り詰めず、そのぶんラベルを詰める。"
  (let ((text (cdr (car (header-tabs-test--tabs
                         (wamei/header-tabs-render
                          (list (header-tabs-test--tab 1 (make-string 200 ?x)
                                                       :head "● ")
                                (header-tabs-test--tab 2 "b"))
                          40 400))))))
    (should (string-prefix-p " ● x" text))
    (should (< (string-width (substring text 0 -1)) 20))))

(ert-deftest wamei/header-tabs-label-escapes-percent ()
  "header-line は :eval の結果の %-construct も展開するので、ラベルの % は二重にする。"
  (should (string-match-p
           "100%%"
           (wamei/header-tabs-render
            (list (header-tabs-test--tab 1 "100%")) 80 800))))

;;; 見た目

(ert-deftest wamei/header-tabs-current-is-highlighted ()
  "今のタブは `wamei/header-tab-current'、他は `wamei/header-tab'。
タブの face は HEAD の face の後ろに足す (● の色を潰さない)。"
  (let* ((string (wamei/header-tabs-render
                  (list (header-tabs-test--tab 1 "a")
                        (header-tabs-test--tab 2 "b" :current t
                                               :head (propertize "● " 'face 'error)))
                  80 800))
         (second (cdr (nth 1 (header-tabs-test--tabs string))))
         (start (string-search second string))
         (mark (string-search "●" string)))
    (should (memq 'wamei/header-tab (header-tabs-test--faces-at string 0)))
    (should (memq 'wamei/header-tab-current (header-tabs-test--faces-at string start)))
    (should (eq (car (header-tabs-test--faces-at string mark)) 'error))
    (should (memq 'wamei/header-tab-current (header-tabs-test--faces-at string mark)))))

(ert-deftest wamei/header-tabs-faces-inherit-tab-line ()
  "タブの見た目は claude パネルの tab-line に揃える。"
  (should (eq (face-attribute 'wamei/header-tab :inherit nil nil) 'tab-line-tab-inactive))
  (should (eq (face-attribute 'wamei/header-tab-current :inherit nil nil)
              'tab-line-tab-current)))

(ert-deftest wamei/header-tabs-hover-covers-exactly-one-tab ()
  "マウス強調は 1 タブを一続きに光らせ、隣のタブとは地続きにしない
\(`mouse-face' は同じ (eq) 値が続く範囲を光らせる)。"
  (let* ((string (wamei/header-tabs-render
                  (list (header-tabs-test--tab 1 "a") (header-tabs-test--tab 2 "b"))
                  80 800))
         (first-end (next-single-property-change 0 'header-tabs-test-id string)))
    (should (get-text-property 0 'mouse-face string))
    (should (eq (next-single-property-change 0 'mouse-face string) first-end))
    (should (get-text-property first-end 'mouse-face string))))

(ert-deftest wamei/header-tabs-properties-cover-the-whole-tab ()
  "PROPERTIES (キーマップや識別子) はタブ全体 (詰め物まで) に張る。"
  (let* ((map (make-sparse-keymap))
         (text (cdr (car (header-tabs-test--tabs
                          (wamei/header-tabs-render
                           (list (list :label "a"
                                       :properties (list 'header-tabs-test-id 1
                                                         'local-map map)))
                           80 800))))))
    (should (eq (get-text-property 0 'local-map text) map))
    (should (eq (get-text-property (1- (length text)) 'local-map text) map))))

;;; クリック

(ert-deftest wamei/header-tabs-event-property-reads-the-clicked-tab ()
  (let* ((string (wamei/header-tabs-render
                  (list (header-tabs-test--tab 1 "a") (header-tabs-test--tab 2 "b"))
                  80 800))
         (pos (text-property-any 0 (length string) 'header-tabs-test-id 2 string))
         (event (list 'mouse-1 (list (selected-window) 'header-line '(0 . 0) 0
                                     (cons string pos) nil '(0 . 0) nil nil nil))))
    (should (eql (wamei/header-tabs-event-property event 'header-tabs-test-id) 2))))

(provide 'header-tabs-test)
;;; header-tabs-test.el ends here
