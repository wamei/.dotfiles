;;; magit-main-window.el --- magit を閉じたら本体の window だけを戻す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; magit の status は side window (sidebar / claude パネル / 端末パネル) を残して
;; 本体いっぱいに開き (`magit-display-buffer-fullframe-status-v1')、閉じたら開く前の
;; 構成に戻す。magit 既定の保存と復元 (`magit-save-window-configuration' /
;; `magit-restore-window-configuration') はフレーム全体の window 構成なので、
;; magit を開いている間に claude パネルを開いたり、サイドバーの幅を変えたり
;; した分まで閉じたときに巻き戻る。
;;
;; ここでは本体の window (`window-main-window'、side window を除いた部分) の
;; 状態だけを保存し、閉じたときにそこへだけ書き戻す (`window-state-put')。
;; side window には触らない。
;;
;; init.el で `magit-pre-display-buffer-hook' と `magit-bury-buffer-function' を
;; これに差し替える。テストは magit-main-window-test.el。
;;; Code:

(defvar magit-inhibit-save-previous-winconf) ; magit-mode.el

(defvar-local wamei/magit--main-window-state nil
  "このバッファ (magit) を開く前の本体の window の状態 (`window-state-get')。")
;; magit のバッファはモードを入れ直すので、そのとき消えないようにする
;; (magit の `magit-previous-window-configuration' と同じ扱い)。
(put 'wamei/magit--main-window-state 'permanent-local t)

(defun wamei/magit-save-main-window ()
  "本体の window の状態を保存する。`magit-pre-display-buffer-hook' 用。
`magit-save-window-configuration' の置き換えで、保存する条件も同じにする。
`magit-inhibit-save-previous-winconf' が立っていれば保存しない (unset なら
保存を消す)。カレントバッファ (これから出す magit のバッファ) が既に出て
いるときも保存しない。出し直すたびに上書きすると戻る先が magit 自身になる。"
  (cond ((bound-and-true-p magit-inhibit-save-previous-winconf)
         (when (eq magit-inhibit-save-previous-winconf 'unset)
           (setq wamei/magit--main-window-state nil)))
        ((not (get-buffer-window (current-buffer) (selected-frame)))
         (setq wamei/magit--main-window-state
               (window-state-get (window-main-window) t)))))

(defun wamei/magit-restore-main-window (&optional kill-buffer)
  "カレントバッファ (magit) を閉じて、本体の window を開く前の状態に戻す。
`magit-bury-buffer-function' 用で、`magit-restore-window-configuration' の置き換え。
KILL-BUFFER が非 nil ならバッファを消す。

side window はその場のまま残る。保存があるときは `quit-window' を通さない。
`quit-window' は magit の window を閉じるときに隣の side window の幅まで
変える (サイドバーが縮む) うえ、本体はどうせ丸ごと書き戻すので要らない。
バッファは奥に回す (KILL-BUFFER なら消す) だけにして、書き戻す。書き戻す
ときに保存後に消えたバッファがあれば、`window-state-put' の safe でその
window を別のバッファにする。

保存が無いとき (別の magit バッファから開いた場合など) は `quit-window' で
閉じるだけ。"
  (let ((state wamei/magit--main-window-state)
        (buffer (current-buffer)))
    (if (not state)
        (quit-window kill-buffer (selected-window))
      (setq wamei/magit--main-window-state nil)
      (if kill-buffer
          (kill-buffer buffer)
        (bury-buffer-internal buffer))
      (window-state-put state (window-main-window) 'safe))))

(provide 'magit-main-window)
;;; magit-main-window.el ends here
