;;; header-tabs.el --- header-line に window の幅を等分するタブを描く -*- lexical-binding: t; -*-
;;; Commentary:
;; header-line に「window の幅を等分するタブ」を 1 行で描く部品。
;; 端末パネル (term-panel.el) とメモ (project-memo.el) が使う。
;;
;; 何をタブにするか・クリックで何をするかは呼び出し側が決める。ここは
;; タブの並び (plist のリスト) を受け取って、等分・切り詰め・見た目・
;; マウス強調を付けた文字列にするだけ。
;;
;; 見た目は claude パネルの tab-line (claude-panel.el) に揃える。
;; tab-line そのものを使わないのは、タブの幅を window の等分にできないため。
;;
;; テストは header-tabs-test.el。
;;; Code:

(require 'seq)
;; タブの face は tab-line の face を継ぐ。
(require 'tab-line)

(defgroup wamei/header-tabs nil
  "header-line に描く等分のタブ。"
  :group 'convenience)

(defface wamei/header-tab '((t :inherit tab-line-tab-inactive))
  "タブの face。claude パネルの tab-line に揃えてある。"
  :group 'wamei/header-tabs)

(defface wamei/header-tab-current '((t :inherit tab-line-tab-current))
  "今のタブの face。"
  :group 'wamei/header-tabs)

(defun wamei/header-tabs--escape (string)
  "STRING の % を二重にする。テキストプロパティは保つ。
header-line は `:eval' が返した文字列の中の %-construct も展開するので、
そのままだと \"100%\" の % やタイトルの中の %s が消える。"
  (replace-regexp-in-string "%" "%%" string t t))

(defun wamei/header-tabs-tab-end (index count pixel-width pixel-offset)
  "COUNT 個のタブの INDEX 番目 (0 始まり) が終わる位置の `:align-to'。
PIXEL-WIDTH (header-line の幅) の (INDEX+1)/COUNT をピクセルで指定するので、
フォントによらず等分される。

header-line は window の左端 (フリンジの外側) から右端まで描かれるが、
`:align-to' の位置は本文の左端から測る (実測)。`right' や `text' も本文の
幅なので、それで割ると左右のフリンジのぶん端がずれる。window の幅で割り、
本文の左端までの PIXEL-OFFSET を引いて指定する。"
  (list (- (round (* (1+ index) pixel-width) count) pixel-offset)))

(defun wamei/header-tabs-render (tabs width pixel-width &optional pixel-offset)
  "TABS を header-line 1 行の文字列にする。
WIDTH は header-line の桁数 (ラベルの切り詰めに使う)、PIXEL-WIDTH はピクセル幅、
PIXEL-OFFSET は window の左端から本文の左端まで (どちらもタブの境目に使う)。

TABS の各要素は plist で、
- :label      タブの名前。1 タブぶんの桁数に収まるよう切り詰める
- :head       ラベルの前に置く文字列 (● など)。切り詰めない。face はそのまま
- :current    非 nil なら今のタブとして強調する
- :properties タブ全体に張るテキストプロパティ (識別子や `local-map')

各タブは \" HEAD ラベル\" と、次の境目までの詰め物。詰め物 (`:align-to') は
後戻りできないので、ラベルは 1 タブぶんの桁数に収まるよう切り詰める。
`mouse-face' はタブごとに別のオブジェクトにする。同じ (eq) 値が続くと
隣のタブまで一続きに光る。"
  (let ((count (length tabs)))
    (apply
     #'concat
     (seq-map-indexed
      (lambda (tab index)
        (let* ((head (or (plist-get tab :head) ""))
               (label-width (max 1 (- (/ width count) 1 (string-width head) 1)))
               (text (concat
                      " " head
                      (wamei/header-tabs--escape
                       (truncate-string-to-width
                        (plist-get tab :label) label-width nil nil t))
                      (propertize " " 'display
                                  `(space :align-to
                                          ,(wamei/header-tabs-tab-end
                                            index count pixel-width
                                            (or pixel-offset 0)))))))
          ;; HEAD の色を残すため、タブの face は後ろに足す
          (add-face-text-property 0 (length text)
                                  (if (plist-get tab :current)
                                      'wamei/header-tab-current
                                    'wamei/header-tab)
                                  t text)
          (add-text-properties 0 (length text)
                               (append (list 'mouse-face (list 'tab-line-highlight))
                                       (plist-get tab :properties))
                               text)
          text))
      tabs))))

(defun wamei/header-tabs-format (tabs)
  "選択中の window の header-line に描く TABS の文字列。
header-line の `:eval' から呼ぶ。redisplay の中では描いている window が
選択中になっているので、幅はその window のもの。header-line は右の
区切り線とスクロールバーには掛からない。"
  (wamei/header-tabs-render tabs
                            (window-body-width)
                            (- (window-pixel-width)
                               (window-right-divider-width)
                               (window-scroll-bar-width))
                            (- (car (window-body-pixel-edges))
                               (car (window-pixel-edges)))))

(defun wamei/header-tabs-event-property (event property)
  "EVENT がクリックしたタブの PROPERTY の値。"
  (when-let* ((object (posn-string (event-start event))))
    (get-text-property (cdr object) property (car object))))

(provide 'header-tabs)
;;; header-tabs.el ends here
