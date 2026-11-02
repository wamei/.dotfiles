;;; magit-pr-section-test.el --- tests for magit-pr-section -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l magit-pr-section-test.el -f ert-run-tests-batch-and-exit
;;
;; magit-section (ELPA) で section を組み立てるところまでを見る。gh は呼ばず、
;; `wamei/magit-pr-command' を JSON を出すだけのシェルに差し替える。
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)
(package-initialize)
(load (expand-file-name "magit-pr-section.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst magit-pr-section-test--json
  "{\"additions\":120,\"baseRefName\":\"main\",\"deletions\":30,
    \"headRefName\":\"feature/x\",\"isDraft\":false,\"number\":42,
    \"reviewDecision\":\"APPROVED\",\"state\":\"OPEN\",
    \"title\":\"Add things\",\"url\":\"https://github.com/o/r/pull/42\",
    \"statusCheckRollup\":[
      {\"__typename\":\"CheckRun\",\"status\":\"COMPLETED\",\"conclusion\":\"SUCCESS\"},
      {\"__typename\":\"CheckRun\",\"status\":\"COMPLETED\",\"conclusion\":\"SUCCESS\"},
      {\"__typename\":\"CheckRun\",\"status\":\"COMPLETED\",\"conclusion\":\"FAILURE\"},
      {\"__typename\":\"CheckRun\",\"status\":\"IN_PROGRESS\",\"conclusion\":null},
      {\"__typename\":\"CheckRun\",\"status\":\"COMPLETED\",\"conclusion\":\"SKIPPED\"},
      {\"__typename\":\"StatusContext\",\"state\":\"PENDING\"},
      {\"__typename\":\"StatusContext\",\"state\":\"ERROR\"}]}"
  "gh pr view --json の出力の例。")

(defun magit-pr-section-test--pr (&rest overrides)
  "例の PR に OVERRIDES (alist のキーと値を交互に) を被せたもの。"
  (let ((pr (wamei/magit-pr--parse magit-pr-section-test--json)))
    (while overrides
      (setf (alist-get (pop overrides) pr) (pop overrides)))
    pr))

(defun magit-pr-section-test--render (pr)
  "PR の section を挿入したバッファの文字列と、その section を返す。"
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (magit-root-section)
        (wamei/magit-pr--insert pr)))
    (cons (buffer-substring-no-properties (point-min) (point-max))
          (car (oref magit-root-section children)))))

(ert-deftest magit-pr-section-checks-counts-by-outcome ()
  "CheckRun と StatusContext を成功・失敗・実行中・スキップに数え分ける。"
  (should (equal (wamei/magit-pr--checks
                  (alist-get 'statusCheckRollup (magit-pr-section-test--pr)))
                 '((success . 2) (failure . 2) (pending . 2) (skipped . 1)))))

(ert-deftest magit-pr-section-state-label ()
  "draft は OPEN より優先して DRAFT と出す。"
  (should (equal (wamei/magit-pr--state-label (magit-pr-section-test--pr)) "OPEN"))
  (should (equal (wamei/magit-pr--state-label
                  (magit-pr-section-test--pr 'isDraft t))
                 "DRAFT"))
  (should (equal (wamei/magit-pr--state-label
                  (magit-pr-section-test--pr 'state "MERGED"))
                 "MERGED")))

(ert-deftest magit-pr-section-renders-all-lines ()
  "見出しに番号・タイトル・状態、本文に URL・ブランチと差分量・チェック・レビュー。"
  (should (equal (car (magit-pr-section-test--render (magit-pr-section-test--pr)))
                 "Pull request #42 Add things [OPEN]
  https://github.com/o/r/pull/42
  main <- feature/x  +120 -30
  Checks: ✓2 ✗2 ●2 -1
  Review: APPROVED

")))

(ert-deftest magit-pr-section-omits-empty-checks-and-review ()
  "チェックが無ければ Checks 行を、レビュー判定が空なら Review 行を出さない。"
  (should (equal (car (magit-pr-section-test--render
                       (magit-pr-section-test--pr 'statusCheckRollup nil
                                                  'reviewDecision "")))
                 "Pull request #42 Add things [OPEN]
  https://github.com/o/r/pull/42
  main <- feature/x  +120 -30

")))

(ert-deftest magit-pr-section-value-is-url-and-ret-browses ()
  "section の値は URL で、RET (magit-visit-thing) でブラウザに開く。"
  (let* ((section (cdr (magit-pr-section-test--render (magit-pr-section-test--pr))))
         (map (symbol-value (oref section keymap))))
    (should (equal (oref section value) "https://github.com/o/r/pull/42"))
    (should (eq (keymap-lookup map "<remap> <magit-visit-thing>")
                #'wamei/magit-pr-browse))))

(ert-deftest magit-pr-section-fetch-parses-output ()
  "コマンドの出力を JSON として読んでコールバックに渡す。"
  (let ((wamei/magit-pr-command
         (list "sh" "-c" (format "printf '%%s' '%s'"
                                 (replace-regexp-in-string
                                  "\n" "" magit-pr-section-test--json))))
        result done)
    (wamei/magit-pr--fetch default-directory
                           (lambda (pr) (setq result pr done t)))
    (with-timeout (5 (error "timeout"))
      (while (not done) (accept-process-output nil 0.05)))
    (should (equal (alist-get 'number result) 42))))

(ert-deftest magit-pr-section-fetch-failure-gives-none ()
  "PR が無い (gh が非 0 で終わる) ときはコールバックに :none を渡す。"
  (let ((wamei/magit-pr-command
         (list "sh" "-c" "echo 'no pull requests found' >&2; exit 1"))
        result done)
    (wamei/magit-pr--fetch default-directory
                           (lambda (pr) (setq result pr done t)))
    (with-timeout (5 (error "timeout"))
      (while (not done) (accept-process-output nil 0.05)))
    (should (eq result :none))))

(ert-deftest magit-pr-section-insert-section-uses-cache-and-fetches-stale ()
  "キャッシュがあればそれを描き、古ければ取り直しを始める。"
  (let ((wamei/magit-pr--cache (make-hash-table :test #'equal))
        (wamei/magit-pr--pending (make-hash-table :test #'equal))
        fetched)
    (cl-letf (((symbol-function 'magit-toplevel) (lambda (&rest _) "/repo/"))
              ((symbol-function 'magit-get-current-branch) (lambda () "feature/x"))
              ((symbol-function 'wamei/magit-pr--fetch)
               (lambda (dir _cb) (push dir fetched))))
      ;; キャッシュ無し: 何も描かず取りに行く
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (magit-root-section)
            (wamei/magit-insert-pull-request)))
        (should (equal (string-trim (buffer-string)) "")))
      (should (equal fetched '("/repo/")))
      ;; 新しいキャッシュ: 描くだけで取りに行かない
      (puthash '("/repo/" . "feature/x")
               (cons (float-time) (magit-pr-section-test--pr))
               wamei/magit-pr--cache)
      (remhash '("/repo/" . "feature/x") wamei/magit-pr--pending)
      (setq fetched nil)
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (magit-root-section)
            (wamei/magit-insert-pull-request)))
        (should (string-prefix-p "Pull request #42" (buffer-string))))
      (should (null fetched))
      ;; 古いキャッシュ: 描いたうえで取りに行く
      (puthash '("/repo/" . "feature/x")
               (cons (- (float-time) wamei/magit-pr-ttl 1)
                     (magit-pr-section-test--pr))
               wamei/magit-pr--cache)
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (magit-root-section)
            (wamei/magit-insert-pull-request)))
        (should (string-prefix-p "Pull request #42" (buffer-string))))
      (should (equal fetched '("/repo/"))))))

;;; magit-pr-section-test.el ends here
