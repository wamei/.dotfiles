;;; desktop-side-windows.el --- desktop の保存データから side window を外して復元する -*- lexical-binding: t; -*-

;;; Commentary:

;; treemacs / 端末 / claude-code-ide などの side window は、表示している
;; バッファが desktop に保存されない。そのまま frameset を保存すると復元時に
;; 「存在しないバッファを指す side window」が残り、side window だけのタブでは
;; window--sides-check-failed から split-window が無限再帰して落ちる。
;;
;; 以前は保存の前後で live な side window を削除して開き直していたが、
;; アイドル 10 秒ごとの自動保存のたびに window オブジェクトが無効になり、
;; フォーカスの戻り先が壊れるなど副作用が大きかった。
;;
;; ここでは live window には触らず、`desktop-save-frameset' が作った
;; frameset (純粋なデータ) から side window の leaf を取り除く。取り除いた
;; side window の side / slot / 寸法は `wamei/desktop-side-saved' に記録し、
;; desktop のグローバル変数として一緒に保存する。読み込み後は
;; `desktop-after-read-hook' で記録をもとにタブごとに開き直す。
;;
;; バッファの種類ごとの開き直し方 (treemacs は treemacs-select-window など) は
;; `wamei/desktop-side-restorers' に登録する。登録が無いものは、同名のバッファ
;; が復元されていれば記録どおりの side window に表示する。

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'frameset)

(declare-function tab-bar--current-tab-index "tab-bar")
(declare-function tab-bar-select-tab "tab-bar")
(defvar tab-bar-tabs-function)
(defvar desktop-saved-frameset)
(defvar desktop-globals-to-save)

;;; 記録

(defvar wamei/desktop-side-saved nil
  "前回保存した side window の記録。
形は ((FRAMESET-ID (TAB-INDEX SPEC...) ...) ...)。FRAMESET-ID はフレームの
frameset--id パラメータ、TAB-INDEX は 0 始まりのタブ位置。SPEC は plist で
:buffer (名前) :side :slot :size (左右なら幅、上下なら高さ) :dedicated を持つ。
`desktop-globals-to-save' 経由で desktop ファイルに書かれる。")

(defvar wamei/desktop-side-restorers nil
  "side window を開き直す関数の alist ((REGEXP . FUNCTION) ...)。
spec の :buffer が REGEXP にマッチしたら FUNCTION を spec を引数に呼ぶ。
どれにもマッチしなければ `wamei/desktop-side-display' を使う。")

(defconst wamei/desktop-side--order '(left right top bottom)
  "side window を開き直す順序。
`window-sides-vertical' が t のとき、下部の side window の寸法は左右の
side window の有無で決まる。左右を先に作らないと幅が合わない。")

(defconst wamei/desktop-side--parameters
  '(window-side window-slot no-other-window no-delete-other-windows)
  "side window を通常の window に戻すときに外す window パラメータ。")

(defconst wamei/desktop-side--size-keys
  '(pixel-width pixel-height total-width total-height normal-width normal-height)
  "window 状態のうち寸法を表す属性。")

;;; window 状態 (window-state-get の戻り値) の操作

(defun wamei/desktop-side--child-p (item)
  "ITEM が子 window の状態なら非 nil。"
  (memq (car-safe item) '(leaf vc hc)))

(defun wamei/desktop-side--children (node)
  "NODE の子 window 状態のリスト。"
  (seq-filter #'wamei/desktop-side--child-p (cdr node)))

(defun wamei/desktop-side--attrs (node)
  "NODE の子 window 以外の属性のリスト。"
  (seq-remove #'wamei/desktop-side--child-p (cdr node)))

(defun wamei/desktop-side--attr (node key)
  "NODE の属性 KEY の値。"
  (cdr (assq key (cdr node))))

(defun wamei/desktop-side--parameters (node)
  "NODE (leaf) に保存された window パラメータの alist。"
  (cdr (assq 'parameters (cdr node))))

(defun wamei/desktop-side--side (node)
  "NODE が side window なら その side、そうでなければ nil。"
  (cdr (assq 'window-side (wamei/desktop-side--parameters node))))

(defun wamei/desktop-side--spec (node)
  "side window の leaf NODE から復元用の spec を作る。"
  (let* ((side (wamei/desktop-side--side node))
         (buffer (cdr (assq 'buffer (cdr node))))
         (name (car buffer)))
    (list :buffer (if (bufferp name) (buffer-name name) name)
          :side side
          :slot (or (cdr (assq 'window-slot (wamei/desktop-side--parameters node))) 0)
          :size (wamei/desktop-side--attr
                 node (if (memq side '(left right)) 'total-width 'total-height))
          :dedicated (cdr (assq 'dedicated (cdr buffer))))))

(defun wamei/desktop-side--inherit (child parent)
  "CHILD の寸法と last を PARENT のものに置き換えた新しい状態を返す。
PARENT の子が CHILD だけになり、CHILD を PARENT の位置へ繰り上げるときに使う。"
  (cons (car child)
        (append
         (when (assq 'last (cdr parent)) '((last . t)))
         (mapcar (lambda (key) (cons key (wamei/desktop-side--attr parent key)))
                 (seq-filter (lambda (key) (assq key (cdr parent)))
                             wamei/desktop-side--size-keys))
         (seq-remove (lambda (item)
                       (or (eq (car-safe item) 'last)
                           (memq (car-safe item) wamei/desktop-side--size-keys)))
                     (cdr child)))))

(defun wamei/desktop-side--rescale (children parent)
  "PARENT の子 CHILDREN の寸法を PARENT に合わせた新しい状態のリストを返す。
並び方向は、子の合計が PARENT と一致していればそのまま、違えば比例配分し直す。
直交方向は常に PARENT と同じにする。last は末尾の子だけに付ける。
side window を外して子が親へ繰り上がると、その下の子は元の (小さい) 親を基準
にした寸法のままになる。復元時は非末尾の子に保存値がそのまま使われ、末尾の子が
残りを全部取るため、合わせ直さないと再起動のたびに右 (下) の window が広がる。"
  (let* ((horizontal (eq (car parent) 'hc))
         (total-key (if horizontal 'total-width 'total-height))
         (pixel-key (if horizontal 'pixel-width 'pixel-height))
         (normal-key (if horizontal 'normal-width 'normal-height))
         (other-total-key (if horizontal 'total-height 'total-width))
         (other-pixel-key (if horizontal 'pixel-height 'pixel-width))
         (other-normal-key (if horizontal 'normal-height 'normal-width))
         (parent-total (wamei/desktop-side--attr parent total-key))
         (parent-pixel (wamei/desktop-side--attr parent pixel-key))
         (sum-of (lambda (key)
                   (apply #'+ (mapcar (lambda (child) (wamei/desktop-side--attr child key))
                                      children))))
         (consistent (and (= (funcall sum-of total-key) parent-total)
                          (= (funcall sum-of pixel-key) parent-pixel)))
         (sum (funcall sum-of total-key))
         (last (car (last children)))
         ;; 比例配分した並び方向の寸法。丸めの余りは末尾の子が取る
         (totals (let ((used 0))
                   (mapcar (lambda (child)
                             (if (eq child last)
                                 (- parent-total used)
                               (let ((total (round (* parent-total
                                                      (/ (float (wamei/desktop-side--attr
                                                                 child total-key))
                                                         sum)))))
                                 (setq used (+ used total))
                                 total)))
                           children)))
         (pixels (let ((used 0)
                       (last-index (1- (length totals))))
                   (seq-map-indexed
                    (lambda (total index)
                      (if (= index last-index)
                          (- parent-pixel used)
                        (let ((pixel (round (* parent-pixel (/ (float total) parent-total)))))
                          (setq used (+ used pixel))
                          pixel)))
                    totals))))
    (seq-map-indexed
     (lambda (child index)
       (let* ((total (nth index totals))
              (replacements
               `(,@(unless consistent
                     `((,total-key . ,total)
                       (,pixel-key . ,(nth index pixels))
                       (,normal-key . ,(/ (float total) parent-total))))
                 (,other-total-key . ,(wamei/desktop-side--attr parent other-total-key))
                 (,other-pixel-key . ,(wamei/desktop-side--attr parent other-pixel-key))
                 (,other-normal-key . 1.0))))
         (cons (car child)
               (append
                (when (eq child last) '((last . t)))
                replacements
                (seq-remove (lambda (item)
                              (or (eq (car-safe item) 'last)
                                  (assq (car-safe item) replacements)))
                            (cdr child))))))
     children)))

(defun wamei/desktop-side--fit (node)
  "NODE 以下の全ての子の寸法を、それぞれの親に合わせ直した新しい状態を返す。"
  (if (eq (car node) 'leaf)
      node
    (cons (car node)
          (append (wamei/desktop-side--attrs node)
                  (mapcar #'wamei/desktop-side--fit
                          (wamei/desktop-side--rescale
                           (wamei/desktop-side--children node) node))))))

(defun wamei/desktop-side--strip-node (node)
  "NODE から side window を取り除く。
戻り値は (NEW-NODE . SPECS)。NEW-NODE が nil なら NODE 全体が side window
だった。SPECS は取り除いた side window の spec のリスト。NODE は書き換えない。"
  (if (eq (car node) 'leaf)
      (if (wamei/desktop-side--side node)
          (cons nil (list (wamei/desktop-side--spec node)))
        (cons node nil))
    (let ((children (wamei/desktop-side--children node))
          (kept nil)
          (specs nil))
      (dolist (child children)
        (pcase-let ((`(,new . ,child-specs) (wamei/desktop-side--strip-node child)))
          (when new (push new kept))
          (setq specs (append specs child-specs))))
      (setq kept (nreverse kept))
      ;; 子の寸法の合わせ直しは `wamei/desktop-side--fit' が木全体に対して行う
      (cons (cond
             ((null kept) nil)
             ;; 子が 1 つだけ残ったら親の位置へ繰り上げる
             ((null (cdr kept)) (wamei/desktop-side--inherit (car kept) node))
             (t (cons (car node) (append (wamei/desktop-side--attrs node) kept))))
            specs))))

(defun wamei/desktop-side--first-leaf (node)
  "NODE 以下で最初に見つかる leaf。"
  (if (eq (car node) 'leaf)
      node
    (wamei/desktop-side--first-leaf (car (wamei/desktop-side--children node)))))

(defun wamei/desktop-side--neutralize (tree)
  "side window だけで構成された TREE を、通常の 1 window の状態に置き換える。
side window 系のパラメータと dedicated を外し、*scratch* を表示させる。
side window だけのフレームは window--sides-check を通らず、復元時に
window--sides-check-failed -> split-window が無限再帰して落ちるため。"
  (let ((leaf (wamei/desktop-side--inherit (wamei/desktop-side--first-leaf tree) tree)))
    (cons 'leaf
          (mapcar
           (lambda (item)
             (pcase (car-safe item)
               ('parameters
                (cons 'parameters
                      (seq-remove (lambda (parameter)
                                    (memq (car parameter) wamei/desktop-side--parameters))
                                  (cdr item))))
               ('buffer
                `(buffer "*scratch*"
                         ,@(mapcar (lambda (entry)
                                     (pcase (car entry)
                                       ('selected '(selected . t))
                                       ('dedicated '(dedicated))
                                       ('point '(point . 1))
                                       ('start '(start . 1))
                                       (_ entry)))
                                   (cddr item))))
               (_ item)))
           (seq-remove (lambda (item) (memq (car-safe item) '(prev-buffers next-buffers)))
                       (cdr leaf))))))

(defun wamei/desktop-side-strip-state (state)
  "`window-state-get' の戻り値 STATE から side window を取り除く。
戻り値は (NEW-STATE . SPECS)。STATE は書き換えない。"
  (let ((header (car state))
        (tree (cdr state)))
    (pcase-let ((`(,new . ,specs) (wamei/desktop-side--strip-node tree)))
      (cons (cons header (wamei/desktop-side--fit
                          (or new (wamei/desktop-side--neutralize tree))))
            specs))))

;;; frameset の操作

(defun wamei/desktop-side--strip-frame-state (state)
  "frameset の 1 フレーム分 STATE (PARAMS . ROOT) から side window を取り除く。
戻り値は (NEW-STATE . BY-TAB)。BY-TAB は ((TAB-INDEX . SPECS) ...)。
現在のタブは ROOT、それ以外のタブは tabs パラメータ内の ws を対象にする。"
  (pcase-let* ((`(,params . ,root) state)
               (tabs (cdr (assq 'tabs params)))
               (by-tab nil)
               (index -1)
               (new-root nil)
               (strip (lambda (window-state)
                        (pcase-let ((`(,new . ,specs)
                                     (wamei/desktop-side-strip-state window-state)))
                          (when specs (push (cons index specs) by-tab))
                          new)))
               (new-tabs
                (mapcar
                 (lambda (tab)
                   (setq index (1+ index))
                   (cond
                    ((eq (car tab) 'current-tab)
                     (setq new-root (funcall strip root))
                     tab)
                    ((alist-get 'ws (cdr tab))
                     (let ((new-ws (funcall strip (alist-get 'ws (cdr tab)))))
                       (cons (car tab)
                             (mapcar (lambda (entry)
                                       (if (eq (car entry) 'ws) (cons 'ws new-ws) entry))
                                     (cdr tab)))))
                    (t tab)))
                 tabs)))
    ;; tab-bar を使っていない (tabs が無い) フレームはタブ 0 として扱う
    (unless new-root
      (setq index 0
            new-root (funcall strip root)))
    (cons (cons (if tabs
                    (mapcar (lambda (entry)
                              (if (eq (car entry) 'tabs) (cons 'tabs new-tabs) entry))
                            params)
                  params)
                new-root)
          (nreverse by-tab))))

(defun wamei/desktop-side-strip-frameset (frameset)
  "FRAMESET の全フレームから side window を取り除き、記録を返す。
FRAMESET の states は差し替える (frameset 自体は `frameset-save' が作った
新しいオブジェクトなので構わない) が、中の window 状態は書き換えない。
戻り値は `wamei/desktop-side-saved' と同じ形。"
  (let ((saved nil))
    (setf (frameset-states frameset)
          (mapcar (lambda (state)
                    (pcase-let ((`(,new . ,by-tab)
                                 (wamei/desktop-side--strip-frame-state state)))
                      (when by-tab
                        (push (cons (cdr (assq 'frameset--id (car state))) by-tab) saved))
                      new))
                  (frameset-states frameset)))
    (nreverse saved)))

;;; 復元

(defun wamei/desktop-side-sort-specs (specs)
  "SPECS を開き直す順 (左・右・上・下、同じ side なら slot 順) に並べる。"
  (let ((rank (lambda (spec)
                (or (seq-position wamei/desktop-side--order (plist-get spec :side)) 99))))
    (sort (copy-sequence specs)
          (lambda (a b)
            (let ((ra (funcall rank a))
                  (rb (funcall rank b)))
              (if (= ra rb)
                  (< (plist-get a :slot) (plist-get b :slot))
                (< ra rb)))))))

(defun wamei/desktop-side-restorer-for (spec)
  "SPEC を開き直す関数。`wamei/desktop-side-restorers' から選ぶ。"
  (let ((name (plist-get spec :buffer)))
    (or (cdr (seq-find (lambda (entry) (string-match-p (car entry) name))
                       wamei/desktop-side-restorers))
        #'wamei/desktop-side-display)))

(defun wamei/desktop-side-display (spec)
  "SPEC の :buffer が生きていれば、記録した side / slot / 寸法の side window に出す。
戻り値は表示した window。バッファが無ければ nil。"
  (when-let* ((buffer (get-buffer (plist-get spec :buffer))))
    (let ((side (plist-get spec :side))
          (size (plist-get spec :size)))
      (display-buffer
       buffer
       `(display-buffer-in-side-window
         (side . ,side)
         (slot . ,(plist-get spec :slot))
         (dedicated . ,(or (plist-get spec :dedicated) t))
         ,(if (memq side '(left right))
              (cons 'window-width size)
            (cons 'window-height size))
         (window-parameters . ((no-other-window . t)
                               (no-delete-other-windows . t))))))))

(defun wamei/desktop-side-resize (window size)
  "side window WINDOW を記録した SIZE (左右なら幅、上下なら高さ) に合わせる。
treemacs のように幅を固定しているバッファでも効くよう window-size-fixed を外す。"
  (when (and (window-live-p window) size)
    (let* ((horizontal (memq (window-parameter window 'window-side) '(left right)))
           (delta (- size (if horizontal
                              (window-total-width window)
                            (window-total-height window)))))
      (unless (zerop delta)
        (with-selected-window window
          (let ((window-size-fixed nil))
            (ignore-errors (window-resize window delta horizontal t))))))))

(defun wamei/desktop-side-restore-specs (specs)
  "現在のタブに SPECS の side window を開き直す。選択 window は変えない。"
  (let ((selected (selected-window)))
    (dolist (spec (wamei/desktop-side-sort-specs specs))
      (condition-case err
          (funcall (wamei/desktop-side-restorer-for spec) spec)
        (error (message "desktop-side: %s を開き直せません: %s"
                        (plist-get spec :buffer) (error-message-string err)))))
    (when (window-live-p selected)
      (select-window selected))))

(defun wamei/desktop-side-restore ()
  "`wamei/desktop-side-saved' の記録をもとに、全フレームの各タブへ side window を開き直す。
`desktop-after-read-hook' から呼ぶ。"
  (dolist (frame (frame-list))
    (when-let* ((id (frame-parameter frame 'frameset--id))
                (by-tab (cdr (assoc id wamei/desktop-side-saved))))
      (with-selected-frame frame
        (if (not (bound-and-true-p tab-bar-mode))
            (when-let* ((specs (cdr (assq 0 by-tab))))
              (wamei/desktop-side-restore-specs specs))
          (let ((current (tab-bar--current-tab-index))
                (count (length (funcall tab-bar-tabs-function))))
            (unwind-protect
                (pcase-dolist (`(,index . ,specs) by-tab)
                  (when (< index count)
                    (tab-bar-select-tab (1+ index))
                    (wamei/desktop-side-restore-specs specs)))
              (tab-bar-select-tab (1+ current)))))))))

;;; desktop への組み込み

(defun wamei/desktop-side--after-save-frameset (&rest _)
  "`desktop-save-frameset' の :after。保存用の frameset から side window を外して記録する。"
  (when (frameset-p desktop-saved-frameset)
    (setq wamei/desktop-side-saved
          (wamei/desktop-side-strip-frameset desktop-saved-frameset))))

(defun wamei/desktop-side-setup ()
  "desktop の保存・読み込みに組み込む。"
  (add-to-list 'desktop-globals-to-save 'wamei/desktop-side-saved)
  (advice-add 'desktop-save-frameset :after #'wamei/desktop-side--after-save-frameset)
  (add-hook 'desktop-after-read-hook #'wamei/desktop-side-restore))

(provide 'desktop-side-windows)
;;; desktop-side-windows.el ends here
