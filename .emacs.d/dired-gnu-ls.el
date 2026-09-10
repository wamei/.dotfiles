;;; dired-gnu-ls.el --- dired に GNU ls を引き当てる -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; dired の listing switches は GNU 拡張 (`--dired' / `--group-directories-first')
;; に依存する。macOS の /bin/ls は BSD 版でどちらも受け付けないため、GNU 互換の
;; ls を別に用意して `insert-directory-program' に据える必要がある。
;;
;; 以前は "/opt/homebrew/bin/gls" を直書きしていたが、brew の coreutils を
;; mise の `aqua:uutils/coreutils' に移した際に brew 版を uninstall したことで
;; そのパスが消え、dired が file-missing で開けなくなった。uutils が入れるのは
;; multicall バイナリ `coreutils' 1 本だけで、`gls' という名前は PATH に出ない。
;; 橋渡しは dotfiles の bin/gls (~/bin/gls へ symlink) が持つ。
;;
;; ここでの方針:
;;
;; - PATH ではなく絶対パスの候補列を先に見る。この設定は init.el の
;;   `leaf dired' の :preface で評価され、PATH を引き継ぐ
;;   `exec-path-from-shell' より前に走るため、GUI 起動 (Finder / Dock から
;;   立ち上げた Emacs は PATH が /usr/bin:/bin:/usr/sbin:/sbin しかない) では
;;   `executable-find' が当たらない。候補の先頭は dotfiles が自分で管理する
;;   ~/bin/gls なので、環境の推測ではなく契約として扱える
;; - それでも外れたら最後に PATH も引く。gls を別経路で入れたマシン向けの保険
;; - 全滅したら素の ls に戻し、switches からも GNU 専用オプションを落とす。
;;   dired が開けないより表示順を諦める方がまし (`--dired' は Emacs 側が
;;   `dired-use-ls-dired' の自動判定で付け外しするのでここでは触らない)
;;
;;; Code:

(require 'dired)

(defconst wamei/dired-gnu-ls-candidates
  '("~/bin/gls"          ; dotfiles の bin/gls -> mise の uutils coreutils
    "/opt/homebrew/bin/gls"  ; brew の coreutils (Apple Silicon)
    "/usr/local/bin/gls")    ; brew の coreutils (Intel)
  "GNU 互換 ls を探す絶対パスの候補。先頭から順に見る。")

(defconst wamei/dired-gnu-ls-switches
  "--color=auto --group-directories-first -alLv"
  "GNU ls を使えるときの `dired-listing-switches'。")

(defconst wamei/dired-bsd-ls-switches
  "-alLv"
  "BSD ls に落ちたときの `dired-listing-switches'。
GNU 専用の長オプションを含めてはならない。macOS の /bin/ls は
unrecognized option で終了ステータスを返し、dired がバッファを
作れなくなる。")

(defun wamei/dired-gnu-ls-find (&optional candidates)
  "GNU 互換 ls の実行ファイルのパスを返す。見つからなければ nil。
CANDIDATES は絶対パス (~ 展開可) のリストで、既定は
`wamei/dired-gnu-ls-candidates'。候補が全滅したときだけ PATH を引く。"
  (or (seq-find #'file-executable-p
                (mapcar #'expand-file-name
                        (or candidates wamei/dired-gnu-ls-candidates)))
      (executable-find "gls")))

(defun wamei/dired-gnu-ls-configure (&optional candidates)
  "dired が使う ls を決め、`dired-listing-switches' も揃える。
GNU 互換 ls が見つかればそのパスを返し、見つからなければ nil を返して
素の ls と BSD 用 switches に落とす。CANDIDATES は
`wamei/dired-gnu-ls-find' に渡す。"
  (let ((gls (wamei/dired-gnu-ls-find candidates)))
    (if gls
        (setq insert-directory-program gls
              dired-listing-switches wamei/dired-gnu-ls-switches
              ;; 候補の gls が本当に GNU 互換かはここでは分からないので t に
              ;; 決め打たない。`unspecified' は Emacs に 1 回だけ実測させる。
              dired-use-ls-dired 'unspecified)
      (setq insert-directory-program "ls"
            dired-listing-switches wamei/dired-bsd-ls-switches
            ;; BSD ls は `--dired' を持たないと分かっているので探らせない。
            ;; 探らせると "ls does not support --dired" が毎回警告される。
            dired-use-ls-dired nil))
    gls))

(provide 'dired-gnu-ls)
;;; dired-gnu-ls.el ends here
