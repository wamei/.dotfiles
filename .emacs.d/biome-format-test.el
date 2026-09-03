;;; biome-format-test.el --- tests for biome-format -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l biome-format-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)

;; apheleia 本体は読み込まず、参照する変数と関数だけを用意する。
(defvar apheleia-formatters nil)
(defvar-local apheleia-formatter nil)
(defvar wamei/biome-format-test--mode-calls nil
  "スタブの `apheleia-mode' が受け取った引数。")
(defun apheleia-mode (&optional arg)
  (push arg wamei/biome-format-test--mode-calls))

(load (expand-file-name "biome-format.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro wamei/biome-format-test--with-project (config-name &rest body)
  "一時ディレクトリに CONFIG-NAME (nil なら無し) と src/ を作って BODY を実行する。
BODY 中は `root' にプロジェクトルート、`src' に src/ が束縛される。"
  (declare (indent 1))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "biome-format-test" t)))
          (src (file-name-as-directory (expand-file-name "src" root))))
     (unwind-protect
         (progn
           (make-directory src)
           (when ,config-name
             (with-temp-file (expand-file-name ,config-name root) (insert "{}")))
           ,@body)
       (delete-directory root t))))

(defmacro wamei/biome-format-test--with-file-buffer (file &rest body)
  "FILE を訪問しているかのようなバッファで BODY を実行する。"
  (declare (indent 1))
  `(with-temp-buffer
     (setq buffer-file-name ,file
           default-directory (file-name-directory ,file))
     (let ((wamei/biome-format-test--mode-calls nil))
       ,@body)))

;;; 設定ファイルの探索

(ert-deftest wamei/biome-format-config-dir-finds-biome-json-in-same-dir ()
  "同じディレクトリの biome.json を見つける。"
  (wamei/biome-format-test--with-project "biome.json"
    (should (equal (wamei/biome-format-config-dir root) root))))

(ert-deftest wamei/biome-format-config-dir-finds-biome-json-in-parent ()
  "親ディレクトリにある biome.json まで遡って見つける。"
  (wamei/biome-format-test--with-project "biome.json"
    (should (equal (wamei/biome-format-config-dir src) root))))

(ert-deftest wamei/biome-format-config-dir-finds-biome-jsonc ()
  "biome.jsonc も設定ファイルとして扱う。"
  (wamei/biome-format-test--with-project "biome.jsonc"
    (should (equal (wamei/biome-format-config-dir src) root))))

(ert-deftest wamei/biome-format-config-dir-returns-nil-without-config ()
  "設定ファイルが無ければ nil。"
  (wamei/biome-format-test--with-project nil
    (should-not (wamei/biome-format-config-dir src))))

;;; 保存時フォーマットの有効化

(ert-deftest wamei/biome-format-maybe-enable-turns-on-apheleia-with-config ()
  "設定があるプロジェクトのファイルでは biome を選んで apheleia-mode を入れる。"
  (wamei/biome-format-test--with-project "biome.json"
    (wamei/biome-format-test--with-file-buffer (expand-file-name "a.ts" src)
      (should (wamei/biome-format-maybe-enable))
      (should (eq apheleia-formatter 'biome))
      (should (equal wamei/biome-format-test--mode-calls '(1))))))

(ert-deftest wamei/biome-format-maybe-enable-does-nothing-without-config ()
  "設定が無ければ何もしない。"
  (wamei/biome-format-test--with-project nil
    (wamei/biome-format-test--with-file-buffer (expand-file-name "a.ts" src)
      (should-not (wamei/biome-format-maybe-enable))
      (should-not apheleia-formatter)
      (should-not wamei/biome-format-test--mode-calls))))

(ert-deftest wamei/biome-format-maybe-enable-does-nothing-without-file ()
  "ファイルを訪問していないバッファでは何もしない。"
  (wamei/biome-format-test--with-project "biome.json"
    (with-temp-buffer
      (setq default-directory src)
      (let ((wamei/biome-format-test--mode-calls nil))
        (should-not (wamei/biome-format-maybe-enable))
        (should-not wamei/biome-format-test--mode-calls)))))

;;; フォーマッタ定義

(ert-deftest wamei/biome-format-setup-registers-remote-capable-command ()
  "apheleia-npx を使わず、npx シンボルで node_modules/.bin を解決するコマンドに差し替える。"
  (let ((apheleia-formatters '((biome . ("apheleia-npx" "biome" "check"))
                               (prettier . ("prettier")))))
    (wamei/biome-format-setup)
    (should (equal (alist-get 'biome apheleia-formatters)
                   '(npx "biome" "check" "--write" "--linter-enabled=false"
                         "--stdin-file-path" filepath)))
    ;; 他のフォーマッタには触らない
    (should (equal (alist-get 'prettier apheleia-formatters) '("prettier")))))

(ert-deftest wamei/biome-format-setup-adds-entry-when-missing ()
  "biome エントリが無くても追加する。"
  (let ((apheleia-formatters '((prettier . ("prettier")))))
    (wamei/biome-format-setup)
    (should (eq (car (alist-get 'biome apheleia-formatters)) 'npx))))

(provide 'biome-format-test)
;;; biome-format-test.el ends here
