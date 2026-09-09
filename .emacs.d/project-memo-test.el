;;; project-memo-test.el --- tests for project-memo -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'project)
(package-initialize)
(require 'dired-subtree)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "project-tabs.el" dir) nil t)
  (load (expand-file-name "dired-tree.el" dir) nil t)
  (load (expand-file-name "project-sidebar.el" dir) nil t)
  (load (expand-file-name "project-memo.el" dir) nil t))

(wamei/project-sidebar-setup)

;;; フィクスチャ

(defmacro wamei/project-memo-test--with-project (var &rest body)
  "一時ディレクトリを transient プロジェクトにして VAR に束縛し BODY を評価する。

`wamei/project-memo-directory' も一時ディレクトリに差し替える。実ユーザーの
~/org/ にテストが書き込まないようにするため、この束縛は必須。
VAR は `file-truename' 済み (macOS では make-temp-file の結果が
/var/folders/… → /private/var/… と symlink 越しになる)。"
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "memo-proj-" t))))
          (wamei/project-memo-directory
           (file-name-as-directory (file-truename (make-temp-file "memo-org-" t))))
          (project-find-functions
           (list (lambda (dir)
                   (when (string-prefix-p ,var (file-truename (expand-file-name dir)))
                     (cons 'transient ,var))))))
     (unwind-protect
         (progn ,@body)
       ;; メモバッファを (未保存でも聞かれないように) 片付けてからディレクトリを消す
       (dolist (buf (buffer-list))
         (when-let* ((file (buffer-file-name buf))
                     ((string-prefix-p wamei/project-memo-directory file)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory ,var t)
       (delete-directory wamei/project-memo-directory t))))

(defun wamei/project-memo-test--project (root)
  "ROOT のプロジェクトオブジェクト。"
  (project-current nil root))

;;; パス解決

(ert-deftest wamei/project-memo-file-is-project-name-under-memo-directory ()
  (wamei/project-memo-test--with-project root
    (should (equal (wamei/project-memo-file (wamei/project-memo-test--project root))
                   (expand-file-name
                    (concat (file-name-nondirectory (directory-file-name root)) ".org")
                    wamei/project-memo-directory)))))

(ert-deftest wamei/project-memo-file-replaces-slash-in-name ()
  (wamei/project-memo-test--with-project root
    (cl-letf (((symbol-function 'project-name) (lambda (_project) "group/app")))
      (should (equal (wamei/project-memo-file (wamei/project-memo-test--project root))
                     (expand-file-name "group-app.org" wamei/project-memo-directory))))))

(ert-deftest wamei/project-memo-global-file-is-global-name ()
  (wamei/project-memo-test--with-project root
    (should (equal (wamei/project-memo-global-file)
                   (expand-file-name "global.org" wamei/project-memo-directory)))))

(ert-deftest wamei/project-memo-file-creates-memo-directory ()
  (wamei/project-memo-test--with-project root
    (delete-directory wamei/project-memo-directory t)
    (should-not (file-directory-p wamei/project-memo-directory))
    (wamei/project-memo-global-file)
    (should (file-directory-p wamei/project-memo-directory))))

;;; メモバッファの判定

(ert-deftest wamei/project-memo-buffer-p-matches-org-under-memo-directory ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (find-file-noselect (wamei/project-memo-global-file))))
      (should (wamei/project-memo-buffer-p buffer)))))

(ert-deftest wamei/project-memo-buffer-p-rejects-other-files ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (should-not (wamei/project-memo-buffer-p buffer))
        (kill-buffer buffer)))))

(ert-deftest wamei/project-memo-buffer-p-rejects-non-org-in-memo-directory ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (find-file-noselect
                   (expand-file-name "notes.txt" wamei/project-memo-directory))))
      (should-not (wamei/project-memo-buffer-p buffer)))))

;;; メモバッファ

(ert-deftest wamei/project-memo-buffer-inserts-title-for-new-file ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
      (with-current-buffer buffer
        (should (string-prefix-p
                 (concat "#+title: " (file-name-nondirectory (directory-file-name root)))
                 (buffer-string)))))))

(ert-deftest wamei/project-memo-buffer-inserts-title-for-existing-empty-file ()
  (wamei/project-memo-test--with-project root
    ;; 「新規」を file-exists-p で見ると、0 バイトのファイルが既にあるとき
    ;; 二度と title が入らない。実体が 0 バイトになるのは珍しくない
    ;; (title を undo した直後に自動保存が走った後など)。
    (let ((file (wamei/project-memo-global-file)))
      (with-temp-file file)
      (should (= 0 (file-attribute-size (file-attributes file))))
      (with-current-buffer (wamei/project-memo-buffer nil)
        (should (string-prefix-p "#+title: " (buffer-string)))))))

(ert-deftest wamei/project-memo-buffer-does-not-put-title-on-undo-list ()
  (wamei/project-memo-test--with-project root
    ;; 新規メモで 1 回 undo すると #+title: 行が消え、そのまま放置すると
    ;; 自動保存が 0 バイトで書いてしまう。title は undo の対象にしない。
    (with-current-buffer (wamei/project-memo-buffer nil)
      (let ((before (buffer-string)))
        (should (string-prefix-p "#+title: " before))
        (should (null buffer-undo-list))
        (ignore-errors (undo))
        (should (equal (buffer-string) before))))))

(ert-deftest wamei/project-memo-buffer-keeps-undo-history-of-existing-buffer ()
  (wamei/project-memo-test--with-project root
    ;; title を入れるときの undo 抑止が、既にあるバッファの undo 履歴まで
    ;; 巻き添えにしないこと。C-x C-m のたびにこの関数を通るので、
    ;; buffer-undo-list を無条件に潰すと編集中の履歴が消える。
    (let ((memo (wamei/project-memo-buffer nil)))
      (with-current-buffer memo
        (goto-char (point-max))
        (insert "編集した\n")
        (undo-boundary)
        (should buffer-undo-list))
      ;; 2 回目の呼び出し (もう空ではない)
      (wamei/project-memo-buffer nil)
      (with-current-buffer memo
        (should buffer-undo-list)))))

(ert-deftest wamei/project-memo-buffer-keeps-existing-content ()
  (wamei/project-memo-test--with-project root
    (let ((file (wamei/project-memo-file (wamei/project-memo-test--project root))))
      (with-temp-file file (insert "既存の中身\n"))
      (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
        (with-current-buffer buffer
          (should (equal (buffer-string) "既存の中身\n")))))))

(ert-deftest wamei/project-memo-buffer-overrides-project-for-project-memo ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
      (with-current-buffer buffer
        (should (local-variable-p 'project-current-directory-override))
        (should (equal (project-root (project-current nil)) root))))))

(ert-deftest wamei/project-memo-buffer-sets-default-directory-to-project-root ()
  (wamei/project-memo-test--with-project root
    ;; project-current-directory-override はバッファローカルなので、それを
    ;; 見ない default-directory の利用者 (project-sidebar の toggle、
    ;; dired-jump など) には届かない。それらにも同じ答えを返させる。
    (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
      (with-current-buffer buffer
        (should (equal (file-truename default-directory) root))))))

(ert-deftest wamei/project-memo-buffer-does-not-override-project-for-global ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (wamei/project-memo-buffer nil)))
      (with-current-buffer buffer
        (should-not (local-variable-p 'project-current-directory-override))
        (should (equal (buffer-file-name) (wamei/project-memo-global-file)))
        ;; 全体メモはプロジェクトに属さないので default-directory も触らない
        ;; (メモの実体がある場所のまま)。
        (should (equal (file-truename default-directory)
                       (file-truename wamei/project-memo-directory)))))))

(ert-deftest wamei/project-memo-sidebar-reopens-at-project-root ()
  (wamei/project-memo-test--with-project root
    ;; C-x C-p でプロジェクトを開き、sidebar を q で閉じ、端末パネル等から
    ;; C-x C-n で開き直す経路。`wamei/project-sidebar-toggle' の「非表示」枝は
    ;; 本文 window のバッファの default-directory を生で読むので、そこにメモが
    ;; いるとメモディレクトリの dired (" *sidebar: org*") が出てしまう。
    ;;
    ;; カレントバッファをメモ以外にしておくのが再現の条件。メモ自身が
    ;; カレントだと、そのバッファローカルな
    ;; `project-current-directory-override' が
    ;; `wamei/project-sidebar--root-for' の中の `project-current' に効いて
    ;; しまい、default-directory が間違っていても正しい root に化ける。
    (let ((main (selected-window))
          (memo (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
      (unwind-protect
          (progn
            (set-window-buffer main memo)
            (should-not (local-variable-p 'project-current-directory-override))
            (wamei/project-sidebar-toggle)
            (let ((side (wamei/project-sidebar-window)))
              (should side)
              (should (equal (file-truename
                              (with-current-buffer (window-buffer side) default-directory))
                             root))))
        (when-let* ((side (wamei/project-sidebar-window)))
          (delete-window side))
        (select-window main)))))

(ert-deftest wamei/project-memo-buffer-is-org-mode ()
  (wamei/project-memo-test--with-project root
    (with-current-buffer (wamei/project-memo-buffer nil)
      (should (derived-mode-p 'org-mode)))))

;;; 表示トグル

(ert-deftest wamei/project-memo-toggle-shows-project-memo-in-main-window ()
  ;; batch は posframe-workable-p が nil なので、prefix 無しでも本文 window。
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      (wamei/project-memo-toggle)
      (should (eq (selected-window) main))
      (should (equal (buffer-file-name (window-buffer main))
                     (wamei/project-memo-file (wamei/project-memo-test--project root)))))))

(ert-deftest wamei/project-memo-toggle-returns-to-previous-buffer ()
  (wamei/project-memo-test--with-project root
    (let* ((main (selected-window))
           (work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (set-window-buffer main work)
            (wamei/project-memo-toggle '(4))
            (should (wamei/project-memo-buffer-p (window-buffer main)))
            (wamei/project-memo-toggle '(4))
            (should (eq (window-buffer main) work)))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-toggle-global-shows-the-global-memo ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      (wamei/project-memo-toggle-global '(4))
      (should (equal (buffer-file-name (window-buffer main))
                     (wamei/project-memo-global-file))))))

(ert-deftest wamei/project-memo-toggle-outside-project-shows-global-memo ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window))
          (scratch (get-buffer-create "*memo-test-scratch*")))
      (unwind-protect
          (progn
            (with-current-buffer scratch (setq default-directory temporary-file-directory))
            (set-window-buffer main scratch)
            (wamei/project-memo-toggle '(4))
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-global-file))))
        (kill-buffer scratch)))))

(ert-deftest wamei/project-memo-toggle-from-global-to-project-keeps-back-buffer ()
  (wamei/project-memo-test--with-project root
    (let* ((main (selected-window))
           (work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (set-window-buffer main work)
            (wamei/project-memo-toggle-global '(4))   ; 全体メモ
            (wamei/project-memo-toggle '(4))          ; プロジェクトメモ (メモ → メモ)
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-file (wamei/project-memo-test--project root))))
            (wamei/project-memo-toggle '(4))          ; 戻り先は work のまま
            (should (eq (window-buffer main) work)))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-toggle-follows-displayed-memo-despite-stale-back-buffer ()
  (wamei/project-memo-test--with-project root
    (let* ((root-b (file-name-as-directory (file-truename (make-temp-file "memo-proj-b-" t))))
           (project-find-functions
            (cons (lambda (dir)
                    (when (string-prefix-p root-b (file-truename (expand-file-name dir)))
                      (cons 'transient root-b)))
                  project-find-functions))
           (main (selected-window))
           (memo-a (wamei/project-memo-buffer (wamei/project-memo-test--project root)))
           (work-b (find-file-noselect (expand-file-name "other.el" root-b))))
      (unwind-protect
          (progn
            ;; window に root の project memo を出した状態で、back には別
            ;; プロジェクト (root-b) の普通のバッファが残っている、という
            ;; toggle を経由しない (find-file 等での) 差し替えを模す。
            (set-window-buffer main memo-a)
            (set-window-parameter main 'wamei/project-memo-back work-b)
            (wamei/project-memo-toggle)
            ;; 表示中の memo-a 自身が root のメモだと正しく自己判定され、
            ;; back の別プロジェクトに惑わされず「戻る」動作 (work-b へ)
            ;; になる。back を無条件に信用すると root-b のメモへ誤って
            ;; 切り替わってしまう。
            (should (eq (window-buffer main) work-b)))
        (kill-buffer work-b)
        (delete-directory root-b t)))))

(ert-deftest wamei/project-memo-toggle-skips-other-memo-buffers-when-no-back-recorded ()
  (wamei/project-memo-test--with-project root
    (let* ((main (selected-window))
           (project (wamei/project-memo-test--project root))
           (project-memo (wamei/project-memo-buffer project))
           (global-memo (wamei/project-memo-buffer nil))
           (other (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            ;; toggle を経由せず window に直接メモを出した状態 (desktop 復元
            ;; 直後など) を模す。back は明示的に未記録にする (この window は
            ;; 1 つの batch セッションを全テストで使い回すので、他のテスト
            ;; の toggle が残した値が残っていることがある)。window の履歴
            ;; (window-prev-buffers) を別のメモだけにしておき、
            ;; `switch-to-prev-buffer' 任せだとメモに留まってしまうことを
            ;; 確認する。
            (set-window-buffer main project-memo)
            (set-window-parameter main 'wamei/project-memo-back nil)
            (set-window-prev-buffers main (list (list global-memo (point-min) (point-min))))
            (wamei/project-memo-toggle)
            (should-not (wamei/project-memo-buffer-p (window-buffer main))))
        (kill-buffer other)))))

(ert-deftest wamei/project-memo--project-ignores-project-rooted-at-memo-directory ()
  (wamei/project-memo-test--with-project root
    ;; spec §2 の「将来 ~/org 自体を git repo にしても ~/org のプロジェクト
    ;; とは判定されない」。override はメモバッファ自身の project-current しか
    ;; 守らないので、パスから計算するこの関数にも同じ保証を持たせる。
    ;; さもないと root 無しのタブで全体メモを出しているとき C-x C-m が
    ;; ~/org/org.org (= それ自体がメモ) を開いてしまう。
    (let* ((memo-dir wamei/project-memo-directory)
           (project-find-functions
            (cons (lambda (dir)
                    (when (string-prefix-p memo-dir (file-truename (expand-file-name dir)))
                      (cons 'transient memo-dir)))
                  project-find-functions))
           (main (selected-window))
           (global-memo (wamei/project-memo-buffer nil)))
      ;; タブに root が無く、本文 window に全体メモ (override 無し) が出ている。
      (set-window-buffer main global-memo)
      (set-window-parameter main 'wamei/project-memo-back nil)
      (should-not (wamei/project-memo--project))
      ;; その帰結として、開かれるのは global.org のまま。
      (should (equal (buffer-file-name
                      (wamei/project-memo-buffer (wamei/project-memo--project)))
                     (wamei/project-memo-global-file))))))

(ert-deftest wamei/project-memo-toggle-skips-internal-buffers-when-no-back-recorded ()
  (wamei/project-memo-test--with-project root
    ;; フォールバックの seq-find は生の `buffer-list' を走るので、
    ;; " *Minibuf-0*" や " *sidebar: foo*" のような先頭空白のバッファまで
    ;; 候補になる。これらは `switch-to-prev-buffer' が意図して飛ばすもので、
    ;; 本文 window に出してよいものではない (`set-window-buffer' は黙って
    ;; 受け付けてしまう)。back パラメータは window-persistent-parameters に
    ;; 無いので desktop 復元や magit の q で落ちる = この経路は日常的に通る。
    (let* ((main (selected-window))
           (project-memo (wamei/project-memo-buffer
                          (wamei/project-memo-test--project root)))
           (global-memo (wamei/project-memo-buffer nil))
           (internal (get-buffer-create " *memo-test-internal*"))
           (work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (set-window-buffer main project-memo)
            (set-window-parameter main 'wamei/project-memo-back nil)
            ;; switch-to-prev-buffer が別のメモを返すようにして、
            ;; フォールバックの seq-find を必ず通す。
            (set-window-prev-buffers main (list (list global-memo (point-min) (point-min))))
            ;; 先頭空白のバッファを非メモの先頭に据える。実セッションの
            ;; buffer-list の並びは運任せなので、ここで固定して常に同じ
            ;; 経路を踏ませる。
            (cl-letf (((symbol-function 'buffer-list)
                       (lambda (&rest _) (list global-memo project-memo internal work))))
              (wamei/project-memo-toggle))
            (should-not (wamei/project-memo-buffer-p (window-buffer main)))
            (should-not (string-prefix-p " " (buffer-name (window-buffer main))))
            (should (eq (window-buffer main) work)))
        (with-current-buffer work (set-buffer-modified-p nil))
        (kill-buffer work)
        (kill-buffer internal)))))

(ert-deftest wamei/project-memo-toggle-uses-the-posframe-by-default ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((main (selected-window)))
        (with-current-buffer (window-buffer main)
          (setq default-directory root))
        ;; hide は unwind-protect で必ず走らせる。手前の should が落ちたときに
        ;; hide を素通りすると post-command-hook と posframe 変数が汚れたまま
        ;; 残り、後続テストに漏れる (このプランで既に 2 度踏んだ defect)。
        (unwind-protect
            (progn
              (wamei/project-memo-toggle)
              (should (eq (car (car calls)) 'show))
              (should (equal (buffer-file-name (cadr (car calls)))
                             (wamei/project-memo-file (wamei/project-memo-test--project root))))
              ;; 本文 window は触らない
              (should-not (wamei/project-memo-buffer-p (window-buffer main))))
          (wamei/project-memo-posframe-hide))))))

(ert-deftest wamei/project-memo-toggle-global-uses-the-posframe-by-default ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (unwind-protect
          (progn
            (wamei/project-memo-toggle-global)
            (should (equal (buffer-file-name (cadr (car calls)))
                           (wamei/project-memo-global-file))))
        (wamei/project-memo-posframe-hide)))))

(ert-deftest wamei/project-memo-toggle-closes-the-posframe-when-shown ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (wamei/project-memo-toggle-global)
      (should (wamei/project-memo-posframe-frame))
      (wamei/project-memo-toggle-global)
      (should-not (wamei/project-memo-posframe-frame)))))

(ert-deftest wamei/project-memo-toggle-with-prefix-closes-the-posframe-first ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((main (selected-window)))
        (wamei/project-memo-toggle-global)          ; posframe
        (should (wamei/project-memo-posframe-frame))
        (wamei/project-memo-toggle-global '(4))     ; 本文 window
        (should-not (wamei/project-memo-posframe-frame))
        (should (equal (buffer-file-name (window-buffer main))
                       (wamei/project-memo-global-file)))))))

(ert-deftest wamei/project-memo-toggle-falls-back-to-the-main-window ()
  ;; posframe-workable-p が nil のときは prefix 無しでも本文 window。
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (cl-letf (((symbol-function 'posframe-workable-p) (lambda () nil))
                ((symbol-function 'posframe-show)
                 (lambda (&rest _) (error "posframe should not be shown"))))
        (wamei/project-memo-toggle-global)
        (should (equal (buffer-file-name (window-buffer main))
                       (wamei/project-memo-global-file)))))))

(ert-deftest wamei/project-memo-toggle-switches-the-posframe-to-a-different-target ()
  ;; 全体メモを posframe に出した状態でプロジェクトメモを求めたら、閉じる
  ;; だけで終わらず新しい対象が show される (`wamei/project-memo-posframe-show'
  ;; の「先に古いバッファを隠す」処理に切り替えを任せる)。記録された呼び出し
  ;; 列全体を見て、部分一致で通ってしまわないようにする。
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((main (selected-window)))
        (with-current-buffer (window-buffer main)
          (setq default-directory root))
        (unwind-protect
            (progn
              (wamei/project-memo-toggle-global)   ; 全体メモを posframe に出す
              (wamei/project-memo-toggle)           ; 別の対象 (プロジェクトメモ) を求める
              (should (equal (mapcar #'car calls) '(show hide show)))
              (should (equal (buffer-file-name (cadr (car calls)))
                             (wamei/project-memo-file (wamei/project-memo-test--project root))))
              (should (wamei/project-memo-posframe-frame)))
          (wamei/project-memo-posframe-hide))))))

(ert-deftest wamei/project-memo-toggle-posframe-does-not-disturb-a-main-window-already-showing-it ()
  ;; 本文 window に既にそのメモが出ている状態で posframe を開くのは spec で
  ;; 許されている組み合わせ。特別扱いせず、本文 window はそのまま。
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let* ((main (selected-window))
             (buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
        (with-current-buffer (window-buffer main)
          (setq default-directory root))
        (set-window-buffer main buffer)
        (unwind-protect
            (progn
              (wamei/project-memo-toggle)
              (should (eq (car (car calls)) 'show))
              (should (eq (window-buffer main) buffer)))
          (wamei/project-memo-posframe-hide))))))

;;; posframe

(defmacro wamei/project-memo-test--with-posframe-stub (calls &rest body)
  "`posframe-show' / `posframe-hide' をスタブして BODY を評価する。

CALLS には呼び出しが (show BUFFER . ARGS) / (hide BUFFER) の形で新しい順に
積まれる。`posframe-show' はダミーのシンボル 'memo-posframe-frame を返し、
`frame-live-p' もそれを生きているものとして扱う (batch では child frame を
作れないため)。"
  (declare (indent 1))
  `(let ((,calls nil))
     (cl-letf (((symbol-function 'posframe-show)
                (lambda (buffer &rest args)
                  (push (cons 'show (cons buffer args)) ,calls)
                  'memo-posframe-frame))
               ((symbol-function 'posframe-hide)
                (lambda (buffer) (push (list 'hide buffer) ,calls) nil))
               ((symbol-function 'frame-live-p)
                (lambda (frame) (eq frame 'memo-posframe-frame)))
               ((symbol-function 'select-frame-set-input-focus)
                (lambda (frame &optional _norecord) frame))
               ((symbol-function 'posframe-workable-p) (lambda () t)))
       ,@body)))

(ert-deftest wamei/project-memo-posframe-show-passes-buffer-and-center-poshandler ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((buffer (wamei/project-memo-buffer nil)))
        (wamei/project-memo-posframe-show buffer)
        (let* ((call (car calls))
               (args (cddr call)))
          (should (eq (car call) 'show))
          (should (eq (cadr call) buffer))
          (should (eq (plist-get args :poshandler) #'posframe-poshandler-frame-center))
          ;; 編集するのでフォーカスを受け取れる必要がある
          (should (eq (plist-get args :accept-focus) t))
          ;; カーソルが見えないと編集できない
          (should (plist-get args :cursor))
          ;; mode-line を消させない (下の -does-not-clobber- のテスト参照)
          (should (plist-get args :respect-mode-line))))
      (wamei/project-memo-posframe-hide))))

(ert-deftest wamei/project-memo-popup-color-is-nil-for-an-undefined-face ()
  ;; init.el の *popup-appearance が定義する face はモジュール単体の batch には
  ;; 無い。`face-attribute' は未定義 face でエラーを出すので、引く前に守る。
  (should-not (wamei/project-memo--popup-color 'wamei/project-memo-test--no-such-face
                                               :background)))

(ert-deftest wamei/project-memo-popup-color-reads-a-defined-face ()
  (let ((face 'wamei/project-memo-test--color-face))
    (unwind-protect
        (progn
          (custom-declare-face face '((t (:background "#123456"))) "test")
          (should (equal (wamei/project-memo--popup-color face :background) "#123456")))
      (put face 'face-defface-spec nil))))

(ert-deftest wamei/project-memo-popup-color-is-nil-for-an-unspecified-attribute ()
  ;; face 自体は定義されていても、その ATTRIBUTE を指定していなければ
  ;; `face-attribute' は 'unspecified を返す。posframe にフレーム既定色を
  ;; 使わせるため、その値も nil に変換する (未定義 face と同じ扱い)。
  (let ((face 'wamei/project-memo-test--unspecified-face))
    (unwind-protect
        (progn
          (custom-declare-face face '((t (:weight bold))) "test")
          (should-not (wamei/project-memo--popup-color face :background)))
      (put face 'face-defface-spec nil))))

(ert-deftest wamei/project-memo-posframe-show-sizes-from-the-ratios ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((wamei/project-memo-posframe-width-ratio 0.5)
            (wamei/project-memo-posframe-height-ratio 0.5)
            (wamei/project-memo-posframe-min-width 1)
            (wamei/project-memo-posframe-min-height 1))
        (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
        (let ((args (cddr (car calls))))
          (should (= (plist-get args :width) (round (* 0.5 (frame-width)))))
          (should (= (plist-get args :height) (round (* 0.5 (frame-height)))))))
      (wamei/project-memo-posframe-hide))))

(ert-deftest wamei/project-memo-posframe-show-respects-the-minimums ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((wamei/project-memo-posframe-width-ratio 0.01)
            (wamei/project-memo-posframe-height-ratio 0.01)
            (wamei/project-memo-posframe-min-width 40)
            (wamei/project-memo-posframe-min-height 10))
        (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
        (let ((args (cddr (car calls))))
          (should (= (plist-get args :width) 40))
          (should (= (plist-get args :height) 10))))
      (wamei/project-memo-posframe-hide))))

(ert-deftest wamei/project-memo-posframe-show-hides-the-previous-buffer-first ()
  ;; posframe.el の `posframe--frame' はバッファローカルなキャッシュなので、
  ;; 隠さずに別バッファへ `posframe-show' すると古いフレームは追跡から外れた
  ;; まま画面上に残ってしまう (leak)。2 回目の show の前に 1 回目の
  ;; バッファを hide していることを確認する。
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((buffer-a (wamei/project-memo-buffer nil))
            (buffer-b (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
        (wamei/project-memo-posframe-show buffer-a)
        (wamei/project-memo-posframe-show buffer-b)
        ;; calls は新しい順に積まれるので、古い順に戻して並びを確認する。
        (should (equal (mapcar (lambda (call) (cons (car call) (cadr call)))
                               (reverse calls))
                       (list (cons 'show buffer-a)
                             (cons 'hide buffer-a)
                             (cons 'show buffer-b)))))
      (wamei/project-memo-posframe-hide))))

(ert-deftest wamei/project-memo-posframe-frame-tracks-visibility ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (should-not (wamei/project-memo-posframe-frame))
      (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
      (should (wamei/project-memo-posframe-frame))
      (wamei/project-memo-posframe-hide)
      (should-not (wamei/project-memo-posframe-frame)))))

(ert-deftest wamei/project-memo-posframe-hide-saves-the-memo ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((buffer (wamei/project-memo-buffer nil)))
        (wamei/project-memo-posframe-show buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "posframe から書いた\n"))
        (wamei/project-memo-posframe-hide)
        (should-not (buffer-modified-p buffer))
        (with-temp-buffer
          (insert-file-contents (wamei/project-memo-global-file))
          (should (string-match-p "posframe から書いた" (buffer-string))))))))

(ert-deftest wamei/project-memo-posframe-hide-is-a-no-op-when-not-shown ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (wamei/project-memo-posframe-hide)
      (should-not calls))))

(ert-deftest wamei/project-memo-posframe-hide-returns-the-selection-to-the-parent ()
  ;; 隠したフレームに選択が残ると、tty では見えない window に入力が吸われる
  ;; (フォーカスイベントが無いので自然には戻らない)。隠す側で親の本文
  ;; window に選択を戻す。
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((main (selected-window))
            (selected nil))
        (unwind-protect
            (progn
              (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
              (cl-letf (((symbol-function 'window-frame)
                         (lambda (&optional _window) 'memo-posframe-frame))
                        ((symbol-function 'wamei/project-tabs-main-window)
                         (lambda () main))
                        ((symbol-function 'select-window)
                         (lambda (window &optional _norecord) (setq selected window))))
                (wamei/project-memo-posframe-hide))
              (should (eq selected main)))
          (wamei/project-memo-posframe-hide))))))

(ert-deftest wamei/project-memo-posframe-hide-leaves-a-selection-elsewhere-alone ()
  ;; 選択が既に posframe の外にあるとき (トグルや focus 移動で閉じる経路) は
  ;; 触らない。閉じるたびに本文 window へ選択を飛ばすと、別の window で
  ;; 作業していた人の選択まで奪ってしまう。
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((selected nil))
        (unwind-protect
            (progn
              (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
              (cl-letf (((symbol-function 'select-window)
                         (lambda (window &optional _norecord) (setq selected window))))
                ;; window-frame は素のまま = 選択 window は実フレームにいる
                (wamei/project-memo-posframe-hide))
              (should-not selected))
          (wamei/project-memo-posframe-hide))))))

(ert-deftest wamei/project-memo-posframe-hide-cleans-up-after-the-frame-died ()
  ;; メモバッファを kill すると dedicated window ごと child frame が消える。
  ;; `frame-live-p' が nil になった後で hide しても、post-command-hook と
  ;; 追跡変数を残さない (残すとセッションの残りの間ずっと空振りの hook が
  ;; コマンドのたびに回り、buffer も参照され続ける)。
  (wamei/project-memo-test--with-project root
    (let ((post-command-hook nil))
      (wamei/project-memo-test--with-posframe-stub calls
        (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
        (should (memq #'wamei/project-memo--posframe-post-command post-command-hook))
        (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) nil)))
          (wamei/project-memo-posframe-hide))
        (should-not (memq #'wamei/project-memo--posframe-post-command post-command-hook))
        (should-not wamei/project-memo--posframe-frame)
        (should-not wamei/project-memo--posframe-buffer)))))

;;; posframe の自動クローズ

(ert-deftest wamei/project-memo-posframe-action-is-nil-when-not-shown ()
  (wamei/project-memo-test--with-project root
    (should-not (wamei/project-memo--posframe-action))))

(ert-deftest wamei/project-memo-posframe-action-is-nil-while-focused-on-the-memo ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((buffer (wamei/project-memo-buffer nil)))
        (wamei/project-memo-posframe-show buffer)
        (cl-letf (((symbol-function 'selected-frame) (lambda () 'memo-posframe-frame))
                  ((symbol-function 'frame-selected-window)
                   (lambda (&optional _f) (selected-window)))
                  ((symbol-function 'window-buffer)
                   (lambda (&optional _w) buffer)))
          (should-not (wamei/project-memo--posframe-action))))
      (wamei/project-memo-posframe-hide))))

(ert-deftest wamei/project-memo-posframe-action-is-hide-when-focus-left ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
      ;; selected-frame は素のまま = posframe のフレームではない
      (should (eq (wamei/project-memo--posframe-action) 'hide))
      (wamei/project-memo-posframe-hide))))

(ert-deftest wamei/project-memo-posframe-action-is-nil-while-the-minibuffer-is-active ()
  ;; child frame は自分のミニバッファを持たず親フレームのものを使う
  ;; (posframe.el の `posframe--create-posframe' が :minibuffer nil で作る)。
  ;; メモにフォーカスがあるまま `C-x C-f' や `M-x' を始めると、コマンドの
  ;; 途中で `selected-frame' が親に変わる。そこで 'hide にしてしまうと、
  ;; ミニバッファを抜けた後に Emacs が child frame の window を選択し直す
  ;; 一方でモジュール側は追跡変数も hook も捨てているので、画面にフレームが
  ;; 残ったまま二度と閉じられなくなる。ミニバッファが立っている間は
  ;; 「フォーカスはまだ外れていない」と見る。
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (unwind-protect
          (progn
            (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
            ;; ミニバッファが無ければ 'hide になる状況をまず作る。
            (should (eq (wamei/project-memo--posframe-action) 'hide))
            (cl-letf (((symbol-function 'active-minibuffer-window)
                       (lambda () (selected-window))))
              (should-not (wamei/project-memo--posframe-action))))
        (wamei/project-memo-posframe-hide)))))

(ert-deftest wamei/project-memo-posframe-action-is-handoff-for-a-foreign-buffer ()
  ;; `window-buffer' を丸ごとスタブすると cl-letf が関数セルを差し替えるため、
  ;; 実装側 (`wamei/project-memo--posframe-buffer-shown') の呼び出しまで
  ;; 巻き込んでしまう。切り出した `--posframe-buffer-shown' 自体をスタブして
  ;; 「posframe に映っているバッファ」だけを差し替える。
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((foreign (find-file-noselect (expand-file-name "main.el" root))))
        (unwind-protect
            (progn
              (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
              (cl-letf (((symbol-function 'selected-frame) (lambda () 'memo-posframe-frame))
                        ((symbol-function 'wamei/project-memo--posframe-buffer-shown)
                         (lambda () foreign)))
                (should (eq (wamei/project-memo--posframe-action) 'handoff)))
              (wamei/project-memo-posframe-hide))
          (with-current-buffer foreign (set-buffer-modified-p nil))
          (kill-buffer foreign))))))

(ert-deftest wamei/project-memo-posframe-post-command-hides-when-focus-left ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
      (wamei/project-memo--posframe-post-command)
      (should-not (wamei/project-memo-posframe-frame))
      (should (eq (car (car calls)) 'hide)))))

(ert-deftest wamei/project-memo-posframe-post-command-hands-the-buffer-to-the-main-window ()
  ;; `selected-frame' をダミーの posframe フレーム (シンボル) にすり替えると、
  ;; `wamei/project-tabs-main-window' 経由で本物の `frame-parent' に渡って
  ;; wrong-type-argument になる (posframe を実フレームで持たない batch の
  ;; 制約)。ここで検証したいのは「post-command が handoff をどう処理するか」
  ;; だけなので、判定そのもの (`--posframe-action') は別テストに任せて直接
  ;; スタブし、`--posframe-buffer-shown' だけ差し替える。
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((foreign (find-file-noselect (expand-file-name "main.el" root)))
            (main (selected-window)))
        (unwind-protect
            (progn
              (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
              (cl-letf (((symbol-function 'wamei/project-memo--posframe-action)
                         (lambda () 'handoff))
                        ((symbol-function 'wamei/project-memo--posframe-buffer-shown)
                         (lambda () foreign)))
                (wamei/project-memo--posframe-post-command))
              (should-not (wamei/project-memo-posframe-frame))
              (should (eq (window-buffer main) foreign)))
          (with-current-buffer foreign (set-buffer-modified-p nil))
          (kill-buffer foreign))))))

(ert-deftest wamei/project-memo-posframe-registers-and-removes-the-post-command-hook ()
  (wamei/project-memo-test--with-project root
    (let ((post-command-hook nil))
      (wamei/project-memo-test--with-posframe-stub calls
        (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
        (should (memq #'wamei/project-memo--posframe-post-command post-command-hook))
        (wamei/project-memo-posframe-hide)
        (should-not (memq #'wamei/project-memo--posframe-post-command post-command-hook))))))

;;; 自動保存

(defmacro wamei/project-memo-test--with-autosave-env (&rest body)
  "`wamei/project-memo-autosave-setup' の副作用をテスト内に閉じ込めて BODY を評価する。

hook と advice はレキシカルな束縛で隔離できるが、アイドルタイマーは
`timer-idle-list' というグローバルに載るので明示的に消す必要がある。
消し忘れると batch セッションの残りのテストの最中に
`wamei/project-memo-save-all' が走り、実ファイルへの書き込みが
テストの外へ漏れる。"
  (declare (indent 0))
  `(let ((wamei/project-memo--autosave-timer nil)
         (window-selection-change-functions nil)
         (kill-emacs-hook nil)
         (after-focus-change-function #'ignore))
     (unwind-protect
         (progn ,@body)
       (when (timerp wamei/project-memo--autosave-timer)
         (cancel-timer wamei/project-memo--autosave-timer)))))

(defun wamei/project-memo-test--save-all-timers ()
  "`timer-idle-list' にある `wamei/project-memo-save-all' のタイマー。"
  (seq-filter (lambda (timer)
                (eq (timer--function timer) #'wamei/project-memo-save-all))
              timer-idle-list))

(ert-deftest wamei/project-memo-save-all-writes-modified-memo ()
  (wamei/project-memo-test--with-project root
    (let ((memo (wamei/project-memo-buffer nil)))
      (with-current-buffer memo
        (goto-char (point-max))
        (insert "書きかけ\n")
        (should (buffer-modified-p)))
      (wamei/project-memo-save-all)
      (should-not (buffer-modified-p memo))
      (should (file-exists-p (wamei/project-memo-global-file)))
      (with-temp-buffer
        (insert-file-contents (wamei/project-memo-global-file))
        (should (string-match-p "書きかけ" (buffer-string)))))))

(ert-deftest wamei/project-memo-save-all-leaves-other-buffers-alone ()
  (wamei/project-memo-test--with-project root
    (let ((work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (with-current-buffer work (insert ";; 未保存\n"))
            (wamei/project-memo-save-all)
            (should (buffer-modified-p work))
            (should-not (file-exists-p (expand-file-name "main.el" root))))
        (with-current-buffer work (set-buffer-modified-p nil))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-save-all-accepts-hook-arguments ()
  (wamei/project-memo-test--with-project root
    ;; window-selection-change-functions は frame を渡す。
    (should-not (wamei/project-memo-save-all (selected-frame)))))

(ert-deftest wamei/project-memo-save-all-skips-buffer-with-stale-modtime ()
  (wamei/project-memo-test--with-project root
    ;; init.el は desktop を GUI と -nw で分けているので、同じメモを 2 つの
    ;; インスタンスが開くことがある。片方が保存すると、もう片方の記録した
    ;; modtime は古くなる。その状態で save-buffer を呼ぶと
    ;; 「changed since visited or saved. Save anyway?」を聞かれるが、
    ;; この関数は redisplay hook (window-selection-change-functions) から
    ;; 呼ばれるので、そこでプロンプトを出させてはいけない。
    (let ((memo (wamei/project-memo-buffer nil))
          (prompts 0))
      (wamei/project-memo-save-all)     ; 実ファイルを作る
      (with-current-buffer memo
        (goto-char (point-max))
        (insert "こちらの編集\n")
        ;; 他インスタンスが書いた後を模す (記録した modtime を実体とずらす)
        (set-visited-file-modtime (time-convert 1 'list)))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (setq prompts (1+ prompts)) t)))
        (wamei/project-memo-save-all))
      (should (= prompts 0))
      ;; 保存を見送るので、変更は失われずバッファに残る。
      (should (buffer-modified-p memo)))))

(ert-deftest wamei/project-memo-save-all-contains-errors ()
  (wamei/project-memo-test--with-project root
    ;; redisplay hook と kill-emacs-hook から呼ぶので、1 つのメモの保存が
    ;; 失敗しても外へ飛ばさない (kill-emacs-hook で飛ぶと終了できなくなる)。
    ;; 残りのメモの保存も続ける。
    (let ((global-memo (wamei/project-memo-buffer nil))
          (project-memo (wamei/project-memo-buffer
                         (wamei/project-memo-test--project root))))
      (with-current-buffer global-memo (goto-char (point-max)) (insert "g\n"))
      (with-current-buffer project-memo (goto-char (point-max)) (insert "p\n"))
      (cl-letf* ((real-save (symbol-function 'save-buffer))
                 ((symbol-function 'save-buffer)
                  (lambda (&rest args)
                    (if (eq (current-buffer) global-memo)
                        (error "書き込みに失敗した")
                      (apply real-save args)))))
        ;; エラーが漏れればこの時点でテストが失敗する。
        (wamei/project-memo-save-all))
      (should (buffer-modified-p global-memo))
      (should-not (buffer-modified-p project-memo)))))

(ert-deftest wamei/project-memo-autosave-setup-creates-idle-timer ()
  (wamei/project-memo-test--with-autosave-env
    (wamei/project-memo-autosave-setup)
    (should (timerp wamei/project-memo--autosave-timer))
    (should (memq wamei/project-memo--autosave-timer timer-idle-list))
    (should (eq (timer--function wamei/project-memo--autosave-timer)
                #'wamei/project-memo-save-all))
    (should (equal (wamei/project-memo-test--save-all-timers)
                   (list wamei/project-memo--autosave-timer)))))

(ert-deftest wamei/project-memo-autosave-setup-leaves-exactly-one-idle-timer ()
  (wamei/project-memo-test--with-autosave-env
    ;; init.el を対話的に再評価すると 2 回呼ばれる。タイマーが積み上がって
    ;; アイドルごとに何度も保存が走ることがないようにする。
    (wamei/project-memo-autosave-setup)
    (wamei/project-memo-autosave-setup)
    (should (equal (wamei/project-memo-test--save-all-timers)
                   (list wamei/project-memo--autosave-timer)))))

(ert-deftest wamei/project-memo-autosave-setup-does-not-enable-auto-save-visited-mode ()
  ;; auto-save-visited-mode は save-some-buffers 経由なので、
  ;; buffer-save-without-query が立ったバッファ (magit の Y) を述語より先に
  ;; 保存してしまう。メモ以外を書かないことを構造的に保証するため、この
  ;; モジュールは auto-save-visited-mode を一切触らない。
  (let ((auto-save-visited-predicate 'sentinel)
        (enable-calls 0))
    (wamei/project-memo-test--with-autosave-env
      (cl-letf (((symbol-function 'auto-save-visited-mode)
                 (lambda (&rest _) (setq enable-calls (1+ enable-calls)))))
        (wamei/project-memo-autosave-setup))
      (should (= enable-calls 0))
      (should-not auto-save-visited-mode)
      (should (eq auto-save-visited-predicate 'sentinel)))))

(ert-deftest wamei/project-memo-autosave-setup-installs-hooks ()
  (let ((base-calls 0)
        (save-all-calls 0))
    (wamei/project-memo-test--with-autosave-env
      ;; #'ignore だと「呼ばれたかどうか」を見られないので、素の focus-change
      ;; 処理を模した副作用付きの関数にしておく。
      (setq after-focus-change-function (lambda () (setq base-calls (1+ base-calls))))
      (cl-letf (((symbol-function 'wamei/project-memo-save-all)
                 (lambda (&rest _) (setq save-all-calls (1+ save-all-calls)))))
        (wamei/project-memo-autosave-setup)
        (should (memq #'wamei/project-memo-save-all window-selection-change-functions))
        (should (memq #'wamei/project-memo-save-all kill-emacs-hook))
        ;; after-focus-change-function への合成が :after であることを確認する。
        ;; advice-function-member-p は「含まれているか」しか見ないので、それだけだと
        ;; :override 等への取り違えを見逃す。base (素の focus-change 処理) の副作用と
        ;; wamei/project-memo-save-all の副作用が両方観測できることまで見て、
        ;; base を消してしまう合成方法ではないことを確かめる。
        (should (advice-function-member-p #'wamei/project-memo-save-all
                                          after-focus-change-function))
        (funcall after-focus-change-function)
        (should (= base-calls 1))
        (should (= save-all-calls 1))))))

(ert-deftest wamei/project-memo-autosave-setup-does-not-double-compose-after-focus-change-function ()
  (let ((save-all-calls 0))
    (wamei/project-memo-test--with-autosave-env
      (cl-letf (((symbol-function 'wamei/project-memo-save-all)
                 (lambda (&rest _) (setq save-all-calls (1+ save-all-calls)))))
        ;; init.el を再評価するなどして 2 回呼ばれても、フォーカス変化のたびに
        ;; wamei/project-memo-save-all が 2 回走る (合成が二重になる) ことがない
        ;; ように、実際に 1 回だけ発火することを確認する。
        (wamei/project-memo-autosave-setup)
        (wamei/project-memo-autosave-setup)
        (funcall after-focus-change-function)
        (should (= save-all-calls 1))))))

;;; タブの初期画面

(ert-deftest wamei/project-memo-switch-setup-shows-sidebar-and-memo ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      (unwind-protect
          (progn
            ;; ERT は各テストの本体を `with-temp-buffer' で包むため、ここまでの
            ;; current-buffer は main のバッファではなく ERT の一時バッファになっている。
            ;; 実際の呼び出し (project-switch-project からの call-interactively) では
            ;; コマンドループが current-buffer を選択中 window のバッファに揃えてから
            ;; 呼ぶので、その前提をここで揃える (select-window はバッファもカレントにする)。
            (select-window main)
            (wamei/project-memo-switch-setup)
            ;; 本文 window にプロジェクトメモ
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-file (wamei/project-memo-test--project root))))
            ;; 左に sidebar が出ていて、フォーカスは本文に残る
            (let ((side (wamei/project-sidebar-window)))
              (should side)
              (should (eq (window-parameter side 'window-side) 'left)))
            (should (eq (selected-window) main)))
        (when-let* ((side (wamei/project-sidebar-window)))
          (delete-window side))))))

(ert-deftest wamei/project-memo-switch-setup-uses-directory-override ()
  (wamei/project-memo-test--with-project root
    ;; project-switch-project と同じ状況: default-directory は別で、
    ;; project-current-directory-override だけが対象プロジェクトを指す。
    (let ((main (selected-window))
          (caller (get-buffer-create "*memo-test-caller*")))
      (unwind-protect
          (progn
            (with-current-buffer caller
              (setq default-directory temporary-file-directory)
              (setq-local project-current-directory-override root)
              (set-window-buffer main caller)
              (wamei/project-memo-switch-setup))
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-file (wamei/project-memo-test--project root)))))
        (when-let* ((side (wamei/project-sidebar-window)))
          (delete-window side))
        (kill-buffer caller)))))

(provide 'project-memo-test)
;;; project-memo-test.el ends here
