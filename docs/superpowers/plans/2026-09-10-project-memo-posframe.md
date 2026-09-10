# メモを画面中央の posframe で開く 実装計画

> この plan は実行時点の記録であり、その後
> `docs/superpowers/specs/2026-09-10-project-memo-posframe-design.md`
> で改訂された (handoff の削除、`C-x m` への付け替えなど)。内容はここでは
> 直さず、最新の設計は上記 spec を参照すること。

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `C-x C-m` の既定の表示先を画面中央の posframe に変え、本文 window 表示は `C-u` 付きに移し、全体メモを `C-x C-S-m` に分ける。

**Architecture:** `project-memo.el` に `;;; posframe` セクションを足し、posframe の表示・非表示・`post-command-hook` による自動クローズを持たせる。表示先の選択 (posframe / 本文 window) は共有の内部関数が受け取り、対象メモの決定 (`wamei/project-memo--project` / `wamei/project-memo-buffer`) は既存のまま使う。child frame にフォーカスがあるときに `tab-bar` が親のタブを見失う問題は、`project-tabs.el` に `parent-frame` を遡るヘルパを足して塞ぐ。

**Tech Stack:** Emacs 31.1 (macOS NS ビルド), Emacs Lisp (lexical binding), posframe (導入済み), ert (batch), 既存モジュール `project-tabs.el` / `project-sidebar.el`, leaf.el

**Spec:** `docs/superpowers/specs/2026-09-10-project-memo-posframe-design.md`

前提となる先行設計: `docs/superpowers/specs/2026-09-09-project-memo-design.md`

## Global Constraints

- Emacs 31.1 (macOS NS ビルド)。GUI と `emacs -nw` の両方で使う。
- **`posframe-workable-p` は `noninteractive` で必ず nil を返す。batch では posframe は出せない。** テストはこれを前提に組む (スタブ + フォールバックの実地検証)。
- **テストは実ユーザーの `~/org/` を絶対に触らない。** 既存フィクスチャ `wamei/project-memo-test--with-project` が `wamei/project-memo-directory` を一時ディレクトリに let 束縛する。新しいテストも必ずこの中に置く。
- テストは global state を残さない。hook・advice・timer・child frame・一時ディレクトリを後始末する。
- 既存テストは 38 本。壊さないこと。
- コメントと docstring は日本語。既存モジュール (`project-sidebar.el` / `project-tabs.el` / `project-memo.el`) と同じ密度で「なぜそうしたか」を書く。
- テストは `cd <repo>/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit` で走る。
- init.el は `~/.emacs.d/init.el` への symlink。モジュールの load は `(file-name-directory (file-truename user-init-file))` 起点。init.el を実機検証するときは worktree 側の実体を指すこと (`~/.emacs.d/init.el` は master を指す)。
- child frame ポップアップの色は `wamei/popup-body` (背景) と `wamei/popup-border` (枠) から取る。新しい face を作らない。
- TDD (Red → Green → Refactoring)。各タスクの最後にコミットする。
- コミットメッセージは英語。末尾に以下を付ける:

  ```
  Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01P54ynGfJYgZ8dapJev2YBy
  ```

---

## ファイル構成

| ファイル | 変更 |
| --- | --- |
| `.emacs.d/project-tabs.el` | `parent-frame` を遡るヘルパを追加し、`wamei/project-tabs-current-root` / `wamei/project-tabs-main-window` / `wamei/project-tabs-set-root` をそこ経由にする |
| `.emacs.d/project-tabs-test.el` | 上のテスト |
| `.emacs.d/project-memo.el` | `;;; posframe` セクションを追加。`;;; 表示` のトグルを表示先付きに作り替える |
| `.emacs.d/project-memo-test.el` | 上のテスト |
| `.emacs.d/init.el` | `leaf project-memo` に `C-x C-S-m` の bind を追加、コメント更新 |

タスク順は「土台 (tabs) → posframe の描画 → 自動クローズ → コマンド/キー」。各タスクは単体でテストが通る状態で終わる。

---

## Task 1: child frame から親フレームのタブを見る

**Files:**
- Modify: `.emacs.d/project-tabs.el`
- Test: `.emacs.d/project-tabs-test.el`

**Interfaces:**
- Consumes: なし
- Produces: `(wamei/project-tabs-base-frame &optional FRAME)` → frame。FRAME (既定は選択フレーム) から `parent-frame` を遡った最上位のフレームを返す。

**背景:** posframe にフォーカスがあると `selected-frame` は child frame になる。child frame は `tabs` フレームパラメータを持たないので `wamei/project-tabs-current-root` が nil を返し、`wamei/project-tabs-main-window` は child frame の window をそのまま返してしまう (side window ではないため)。後者は `wamei/tab-bar-tab-name-project` 経由でタブ名をメモバッファ名に化けさせる。

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/project-tabs-test.el` の末尾、`(provide ...)` の前に足す。既存のテストと同じ流儀で、フレームは実際には作らず `frame-parameter` をスタブする。

```elisp
;;; child frame からの解決

(ert-deftest wamei/project-tabs-base-frame-returns-frame-itself-without-parent ()
  (should (eq (wamei/project-tabs-base-frame (selected-frame)) (selected-frame))))

(ert-deftest wamei/project-tabs-base-frame-walks-up-parent-frames ()
  ;; 親子関係だけをスタブする。'child2 → 'child1 → selected-frame。
  (let ((parents (list (cons 'child2 'child1)
                       (cons 'child1 (selected-frame)))))
    (cl-letf (((symbol-function 'frame-parent)
               (lambda (frame) (alist-get frame parents))))
      (should (eq (wamei/project-tabs-base-frame 'child2) (selected-frame)))
      (should (eq (wamei/project-tabs-base-frame 'child1) (selected-frame))))))

(ert-deftest wamei/project-tabs-current-root-reads-the-base-frame ()
  ;; child frame には tabs が無い。親の tabs を読めていれば root が返る。
  (let ((tabs '((current-tab (name . "proj") (wamei-project . "/tmp/proj/")))))
    (cl-letf (((symbol-function 'frame-parent)
               (lambda (frame) (when (eq frame 'child) (selected-frame))))
              ((symbol-function 'frame-parameter)
               (lambda (frame param)
                 (cond ((eq frame 'child) nil)
                       ((eq param 'tabs) tabs)
                       (t nil)))))
      (should (equal (wamei/project-tabs-current-root 'child) "/tmp/proj/")))))

(ert-deftest wamei/project-tabs-main-window-ignores-child-frames ()
  ;; child frame の window が選択されていても、親フレームの window を返す。
  (let* ((main (selected-window)))
    (cl-letf (((symbol-function 'selected-window) (lambda () 'child-window))
              ((symbol-function 'window-frame)
               (lambda (&optional _w) 'child-frame))
              ((symbol-function 'frame-parent)
               (lambda (frame) (when (eq frame 'child-frame) (selected-frame))))
              ((symbol-function 'frame-selected-window)
               (lambda (&optional frame)
                 (if (eq frame (selected-frame)) main 'child-window))))
      (should (eq (wamei/project-tabs-main-window) main)))))
```

`cl-lib` が要る。ファイル冒頭に `(require 'cl-lib)` が無ければ足す。

- [ ] **Step 2: テストが失敗することを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-tabs-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL (`wamei/project-tabs-base-frame` が void-function)

- [ ] **Step 3: 最小の実装を書く**

`.emacs.d/project-tabs.el` の「タブに紐づくプロジェクト」節の先頭 (`wamei/project-tabs--normalize-root` の前) に足す。

```elisp
(defun wamei/project-tabs-base-frame (&optional frame)
  "FRAME (既定は選択フレーム) が属する最上位のフレーム。

child frame (posframe など) にフォーカスがあると `selected-frame' は
その child frame になる。child frame は tabs パラメータを持たないので、
そのままタブを引くと「タブが無い」ことになってしまう。タブは常に
最上位のフレームのものを見る。"
  (let ((frame (or frame (selected-frame))))
    (while-let ((parent (frame-parent frame)))
      (setq frame parent))
    frame))
```

`wamei/project-tabs-current-root` の本体を base frame 経由にする。

```elisp
(defun wamei/project-tabs-current-root (&optional frame)
  "FRAME のカレントタブに紐づけたプロジェクトルート。無ければ nil。

tab-bar-tabs (や tab-bar--current-tab-find) を通すとカレントタブ名の
再計算が走り、その中で project-current が呼ばれる。この関数は
project-current の advice から呼ぶので、それでは再帰する。
独自パラメータは frame の tabs にそのまま入っているため直接読む。

child frame にフォーカスがあるときのために `wamei/project-tabs-base-frame'
を通す (child frame に tabs は無い)。"
  (wamei/project-tabs-root
   (assq 'current-tab (frame-parameter (wamei/project-tabs-base-frame frame) 'tabs))))
```

`wamei/project-tabs-main-window` も base frame 基準にする。

```elisp
(defun wamei/project-tabs-main-window ()
  "タブの本文とみなす window。
選択 window が side window (no-other-window 付き) なら直近の通常 window。
タブ名の根拠、サイドバーが従うバッファ、サイドバーからファイルを開く先に使う。

child frame (メモの posframe など) にフォーカスがあるときは、その window を
本文とみなしてはいけない。タブ名がメモバッファ名に化け、サイドバーの追従先も
狂う。最上位フレームの選択 window に読み替えてから判定する。"
  (let* ((base (wamei/project-tabs-base-frame))
         (window (if (eq (window-frame (selected-window)) base)
                     (selected-window)
                   (frame-selected-window base))))
    (if (window-parameter window 'no-other-window)
        (or (get-mru-window base nil t t) window)
      window)))
```

`wamei/project-tabs-set-root` の `tab-bar--current-tab-find` も選択フレーム基準なので、child frame から呼ばれても親に効くように `with-selected-frame` で包む。

```elisp
(defun wamei/project-tabs-set-root (root)
  "カレントタブに ROOT を紐づける。

タブは (current-tab (KEY . VALUE) ...) という構造で先頭がシンボルのため、
setf alist-get だと局所変数へ push されるだけで実体に残らない。
保存されているリストへ直接つなぐ必要がある。

child frame から呼ばれても親フレームのタブに書くよう
`wamei/project-tabs-base-frame' のフレームで実行する。"
  (with-selected-frame (wamei/project-tabs-base-frame)
    (tab-bar-tabs)                      ; frame の tabs パラメータを確実に用意する
    (when-let* ((tab (tab-bar--current-tab-find))
                (root (wamei/project-tabs--normalize-root root)))
      (if-let* ((cell (assq 'wamei-project (cdr tab))))
          (setcdr cell root)
        (setcdr tab (cons (cons 'wamei-project root) (cdr tab))))
      root)))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-tabs-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS (既存 + 4 本)

- [ ] **Step 5: メモ側のテストも壊れていないことを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: 38/38 PASS

- [ ] **Step 6: サイドバーのテストも壊れていないことを確認**

`wamei/project-tabs-main-window` は sidebar が使っている。

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit
```

Expected: 24/24 PASS

- [ ] **Step 7: コミット**

```bash
cd <worktree>
git add .emacs.d/project-tabs.el .emacs.d/project-tabs-test.el
git commit -m "$(cat <<'EOF'
Resolve tabs against the top-level frame, not a child frame

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01P54ynGfJYgZ8dapJev2YBy
EOF
)"
```

---

## Task 2: posframe を出す / 隠す

**Files:**
- Modify: `.emacs.d/project-memo.el` (`;;; 表示` の後、`;;; 自動保存` の前に `;;; posframe` 節を足す。冒頭の require に `posframe` を足す)
- Test: `.emacs.d/project-memo-test.el`

**Interfaces:**
- Consumes: `wamei/project-memo-buffer-p`, `wamei/project-memo-save-all` (既存)
- Produces:
  - `wamei/project-memo-posframe-width-ratio` / `-height-ratio` (defcustom, float, 既定 0.6)
  - `wamei/project-memo-posframe-min-width` / `-min-height` (defcustom, integer, 既定 40 / 10)
  - `(wamei/project-memo--popup-color FACE ATTRIBUTE)` → 色文字列または nil
  - `(wamei/project-memo-posframe-frame)` → frame または nil (生きている posframe のフレーム)
  - `(wamei/project-memo-posframe-show BUFFER)` → frame。BUFFER を中央の posframe に出してフォーカスを移す
  - `(wamei/project-memo-posframe-hide)` → nil。保存してから隠す。出ていなければ何もしない

**注意:** `posframe-workable-p` は `noninteractive` で nil を返すので、batch では `posframe-show` は呼ばない。テストはすべて `posframe-show` / `posframe-hide` を `cl-letf` でスタブして「何を渡したか」を検証する。

- [ ] **Step 1: 失敗するテストを書く**

`project-memo-test.el` の `;;; 自動保存` 節の前に足す。

```elisp
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
          (should (plist-get args :respect-mode-line)))))
    (wamei/project-memo-posframe-hide)))

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
          (should (= (plist-get args :height) (round (* 0.5 (frame-height))))))))
    (wamei/project-memo-posframe-hide)))

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
          (should (= (plist-get args :height) 10)))))
    (wamei/project-memo-posframe-hide)))

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
```

- [ ] **Step 2: テストが失敗することを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL (`wamei/project-memo-posframe-show` が void-function)

- [ ] **Step 3: 最小の実装を書く**

`project-memo.el` 冒頭の require に足す。

```elisp
(require 'posframe)
```

`;;; 表示` 節の後ろ、`;;; 自動保存` の前に足す。

```elisp
;;; posframe

(defcustom wamei/project-memo-posframe-width-ratio 0.6
  "メモの posframe の幅。親フレームの桁数に対する比率。"
  :type 'float
  :group 'wamei/project-memo)

(defcustom wamei/project-memo-posframe-height-ratio 0.6
  "メモの posframe の高さ。親フレームの行数に対する比率。"
  :type 'float
  :group 'wamei/project-memo)

(defcustom wamei/project-memo-posframe-min-width 40
  "メモの posframe の最小の幅 (桁)。"
  :type 'integer
  :group 'wamei/project-memo)

(defcustom wamei/project-memo-posframe-min-height 10
  "メモの posframe の最小の高さ (行)。"
  :type 'integer
  :group 'wamei/project-memo)

(defvar wamei/project-memo--posframe-frame nil
  "メモを出している posframe のフレーム。出ていなければ nil。")

(defvar wamei/project-memo--posframe-buffer nil
  "posframe に出しているメモバッファ。`posframe-hide' はバッファで指定する。")

(defun wamei/project-memo-posframe-frame ()
  "メモの posframe が出ていればそのフレーム。出ていなければ nil。"
  (and wamei/project-memo--posframe-frame
       (frame-live-p wamei/project-memo--posframe-frame)
       wamei/project-memo--posframe-frame))

(defun wamei/project-memo--posframe-size (ratio total minimum)
  "RATIO (親フレームの TOTAL に対する比率) から posframe の大きさを出す。
MINIMUM を下回らない。"
  (max minimum (round (* ratio total))))

(defun wamei/project-memo--popup-color (face attribute)
  "FACE の ATTRIBUTE の色。FACE が未定義か未指定なら nil。

枠と背景は init.el の *popup-appearance が定義する `wamei/popup-border' /
`wamei/popup-body' から取る。あちらは init.el 側なので、モジュール単体で
読む batch テストには存在しない。`face-attribute' は未定義の face に対して
エラーを出す (\"Invalid face\") ので、存在するときだけ引く。nil を渡された
posframe はフレーム既定の色を使う。"
  (when (facep face)
    (let ((value (face-attribute face attribute nil t)))
      (unless (eq value 'unspecified) value))))

(defun wamei/project-memo-posframe-show (buffer)
  "BUFFER を画面中央の posframe に出し、フォーカスを移す。フレームを返す。

`:accept-focus' を渡さないと posframe 自身が
`posframe--redirect-posframe-focus' でフォーカスを親フレームへ送り返すので、
編集できない。カーソルも既定では隠されるので明示的に出す。

枠と背景は corfu / eldoc-box / vertico-posframe と同じ
`wamei/popup-border' / `wamei/popup-body' から取る (init.el の
*popup-appearance)。tty の罫線枠は同ブロックが display table に入れた
box グリフがそのまま効く。

`:respect-mode-line' を渡すのは見た目の趣味ではない。posframe は
`:respect-mode-line' が nil だと表示するバッファに `mode-line-format' を
nil で setq-local する。これは posframe を隠しても残るので、そのメモを
あとから `C-u' で本文 window に出したときモードラインが消えたままになる。
バッファを壊さないために残す。"
  (setq wamei/project-memo--posframe-buffer buffer)
  (setq wamei/project-memo--posframe-frame
        (posframe-show
         buffer
         :poshandler #'posframe-poshandler-frame-center
         :width (wamei/project-memo--posframe-size
                 wamei/project-memo-posframe-width-ratio
                 (frame-width) wamei/project-memo-posframe-min-width)
         :height (wamei/project-memo--posframe-size
                  wamei/project-memo-posframe-height-ratio
                  (frame-height) wamei/project-memo-posframe-min-height)
         :border-width 1
         :border-color (wamei/project-memo--popup-color 'wamei/popup-border :background)
         :background-color (wamei/project-memo--popup-color 'wamei/popup-body :background)
         :accept-focus t
         :cursor 'box
         :respect-mode-line t))
  (select-frame-set-input-focus wamei/project-memo--posframe-frame)
  wamei/project-memo--posframe-frame)

(defun wamei/project-memo-posframe-hide ()
  "メモの posframe を保存してから隠す。出ていなければ何もしない。"
  (when (wamei/project-memo-posframe-frame)
    (wamei/project-memo-save-all)
    (posframe-hide wamei/project-memo--posframe-buffer)
    (setq wamei/project-memo--posframe-frame nil)
    (setq wamei/project-memo--posframe-buffer nil))
  nil)
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS (38 + 8 = 46 本)

`wamei/popup-body` / `wamei/popup-border` は init.el 側の face なので batch には無い。`face-attribute` は未定義の face に対して `(error "Invalid face" ...)` を出す (実測済み) ので、必ず `wamei/project-memo--popup-color` 経由で引くこと。直接 `face-attribute` を呼ぶと batch のテストが落ちる。

- [ ] **Step 5: コミット**

```bash
cd <worktree>
git add .emacs.d/project-memo.el .emacs.d/project-memo-test.el
git commit -m "$(cat <<'EOF'
Show a memo in a centered posframe

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01P54ynGfJYgZ8dapJev2YBy
EOF
)"
```

---

## Task 3: posframe を自動で閉じる

**Files:**
- Modify: `.emacs.d/project-memo.el` (`;;; posframe` 節の続き)
- Test: `.emacs.d/project-memo-test.el`

**Interfaces:**
- Consumes: `wamei/project-memo-posframe-frame`, `wamei/project-memo-posframe-hide` (Task 2), `wamei/project-tabs-main-window` (Task 1 で child frame 対応済み), `wamei/project-memo-buffer-p` (既存)
- Produces:
  - `(wamei/project-memo--posframe-action)` → シンボル。`nil` (何もしない) / `'hide` (フォーカスが外れた) / `'handoff` (メモ以外が入った) のいずれか
  - `(wamei/project-memo--posframe-post-command)` → nil。`post-command-hook` 用
  - `wamei/project-memo-posframe-show` / `-hide` が `post-command-hook` の登録/解除を行う

**背景:** 閉じる条件は 3 つ (spec)。トグルは Task 4。ここでは残り 2 つを `post-command-hook` 1 本で見る。判定を副作用の無い `--posframe-action` に切り出し、batch でテストできるようにする。

- [ ] **Step 1: 失敗するテストを書く**

`project-memo-test.el` の `;;; posframe` 節の末尾に足す。

```elisp
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

(ert-deftest wamei/project-memo-posframe-action-is-handoff-for-a-foreign-buffer ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((foreign (find-file-noselect (expand-file-name "main.el" root))))
        (unwind-protect
            (progn
              (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
              (cl-letf (((symbol-function 'selected-frame) (lambda () 'memo-posframe-frame))
                        ((symbol-function 'frame-selected-window)
                         (lambda (&optional _f) (selected-window)))
                        ((symbol-function 'window-buffer)
                         (lambda (&optional _w) foreign)))
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
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((foreign (find-file-noselect (expand-file-name "main.el" root)))
            (main (selected-window)))
        (unwind-protect
            (progn
              (wamei/project-memo-posframe-show (wamei/project-memo-buffer nil))
              (cl-letf (((symbol-function 'selected-frame) (lambda () 'memo-posframe-frame))
                        ((symbol-function 'frame-selected-window)
                         (lambda (&optional _f) (selected-window)))
                        ((symbol-function 'window-buffer)
                         (lambda (&optional w)
                           (if w (funcall #'window-buffer w) foreign))))
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
```

`window-buffer` を丸ごとスタブすると `cl-letf` が関数セルを置き換えるため、実装側の他の `window-buffer` 呼び出しまで巻き込んで再帰しうる。実装は「posframe に映っているバッファ」を `wamei/project-memo--posframe-buffer-shown` に切り出す (Step 3 で定義する) ので、上の 4 本目と 6 本目のテストはそちらをスタブする形に書き換えてよい。どちらの形にしたかを report に書くこと。

- [ ] **Step 2: テストが失敗することを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL (`wamei/project-memo--posframe-action` が void-function)

- [ ] **Step 3: 最小の実装を書く**

`;;; posframe` 節の `wamei/project-memo-posframe-hide` の後ろに足す。

```elisp
(defun wamei/project-memo--posframe-buffer-shown ()
  "posframe の window が映しているバッファ。出ていなければ nil。"
  (when-let* ((frame (wamei/project-memo-posframe-frame)))
    (window-buffer (frame-selected-window frame))))

(defun wamei/project-memo--posframe-action ()
  "posframe に対していま取るべき動作。

- nil      … そのまま (メモにフォーカスがある)
- `hide'   … 隠す (フォーカスが Emacs 内の別の場所へ移った)
- `handoff'… 隠して中身を本文 window へ渡す (メモ以外のバッファが入った)"
  (when-let* ((frame (wamei/project-memo-posframe-frame)))
    (cond
     ((not (eq (selected-frame) frame)) 'hide)
     ((not (wamei/project-memo-buffer-p (wamei/project-memo--posframe-buffer-shown)))
      'handoff)
     (t nil))))

(defun wamei/project-memo--posframe-post-command ()
  "`post-command-hook' 用。posframe を閉じるべきなら閉じる。

閉じる条件のうち「フォーカスが外れた」と「メモ以外のバッファが入った」を
ここで見る (トグルで閉じるのは `wamei/project-memo-toggle' 側)。

メモ以外が入るのは、posframe にフォーカスがあるまま `find-file' や
`magit-status' を実行した場合。禁止キーの一覧を持つ代わりに、入ってしまった
ものを本文 window へ引き取る。`post-command-hook' は再描画の前に走るので、
別バッファが posframe に見える瞬間は基本的に出ない。"
  (pcase (wamei/project-memo--posframe-action)
    ('hide (wamei/project-memo-posframe-hide))
    ('handoff
     (let ((buffer (wamei/project-memo--posframe-buffer-shown))
           (window (wamei/project-tabs-main-window)))
       (wamei/project-memo-posframe-hide)
       (when (buffer-live-p buffer)
         (set-window-buffer window buffer))
       (select-window window))))
  nil)
```

`wamei/project-memo-posframe-show` の末尾 (`select-frame-set-input-focus` の前) に hook 登録を足す。

```elisp
  (add-hook 'post-command-hook #'wamei/project-memo--posframe-post-command)
```

`wamei/project-memo-posframe-hide` の `when` の中に hook 解除を足す。

```elisp
    (remove-hook 'post-command-hook #'wamei/project-memo--posframe-post-command)
```

`--posframe-action` が `handoff` を返すとき `wamei/project-tabs-main-window` は
Task 1 で child frame を無視するようになっているので、親フレームの本文 window が返る。

- [ ] **Step 4: テストが通ることを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS (46 + 7 = 53 本)

- [ ] **Step 5: グローバル state が残っていないことを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit --eval '(princ (format "\nleft: post-command=%S frame=%S\n" (memq (function wamei/project-memo--posframe-post-command) post-command-hook) wamei/project-memo--posframe-frame))'
```

Expected: `left: post-command=nil frame=nil`

- [ ] **Step 6: コミット**

```bash
cd <worktree>
git add .emacs.d/project-memo.el .emacs.d/project-memo-test.el
git commit -m "$(cat <<'EOF'
Close the memo posframe on focus loss or a foreign buffer

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01P54ynGfJYgZ8dapJev2YBy
EOF
)"
```

---

## Task 4: トグルコマンドを表示先付きにする

**Files:**
- Modify: `.emacs.d/project-memo.el` (`;;; 表示` 節の `wamei/project-memo-toggle`)
- Test: `.emacs.d/project-memo-test.el` (既存のトグルのテストを新しい contract に合わせる)

**Interfaces:**
- Consumes: `wamei/project-memo--project`, `wamei/project-memo-buffer`, `wamei/project-memo--restore` (既存)、Task 2 / Task 3 の posframe 関数
- Produces:
  - `(wamei/project-memo--toggle GLOBAL MAIN-WINDOW)` → nil。共有の内部関数
  - `(wamei/project-memo-toggle &optional MAIN-WINDOW)` (interactive `"P"`) — プロジェクトメモ
  - `(wamei/project-memo-toggle-global &optional MAIN-WINDOW)` (interactive `"P"`) — 全体メモ

**contract (spec より):**

| 呼び方 | 表示先 | 対象 |
| --- | --- | --- |
| `wamei/project-memo-toggle` | posframe | プロジェクトメモ |
| `wamei/project-memo-toggle` + prefix | 本文 window | プロジェクトメモ |
| `wamei/project-memo-toggle-global` | posframe | 全体メモ |
| `wamei/project-memo-toggle-global` + prefix | 本文 window | 全体メモ |

- posframe が使えない環境 (`posframe-workable-p` が nil) では prefix 無しでも本文 window に出す。
- posframe が出ている状態で本文 window 表示を求められたら、posframe を閉じてから本文に出す。
- 本文 window に既にそのメモが出ている状態で posframe を開くのは許す (特別扱いしない)。
- posframe が出ている状態で同じ posframe 表示を求められたら閉じる (トグル)。

**既存テストの扱い:** 現在のトグルのテスト 5 本は prefix の意味が「全体メモ」なので、そのままでは contract に合わない。`wamei/project-memo-toggle-global` を使う形に書き換える (削除ではなく移植)。batch は `posframe-workable-p` が nil なので、prefix 無しの呼び出しも本文 window に落ちる。これは仕様どおりで、フォールバックの実地検証も兼ねる。

- [ ] **Step 1: 既存のトグルテストを新しい contract に移植し、失敗させる**

`project-memo-test.el` の `;;; 表示トグル` 節を次に置き換える。5 本の既存テストの検証内容はそのまま保ち、全体メモを求める呼び出しだけ `-global` に移す。

```elisp
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
```

さらに新しい contract のテストを足す。

```elisp
(ert-deftest wamei/project-memo-toggle-uses-the-posframe-by-default ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (let ((main (selected-window)))
        (with-current-buffer (window-buffer main)
          (setq default-directory root))
        (wamei/project-memo-toggle)
        (should (eq (car (car calls)) 'show))
        (should (equal (buffer-file-name (cadr (car calls)))
                       (wamei/project-memo-file (wamei/project-memo-test--project root))))
        ;; 本文 window は触らない
        (should-not (wamei/project-memo-buffer-p (window-buffer main)))
        (wamei/project-memo-posframe-hide)))))

(ert-deftest wamei/project-memo-toggle-global-uses-the-posframe-by-default ()
  (wamei/project-memo-test--with-project root
    (wamei/project-memo-test--with-posframe-stub calls
      (wamei/project-memo-toggle-global)
      (should (equal (buffer-file-name (cadr (car calls)))
                     (wamei/project-memo-global-file)))
      (wamei/project-memo-posframe-hide))))

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
```

- [ ] **Step 2: テストが失敗することを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL (`wamei/project-memo-toggle-global` が void-function、および prefix の意味が変わったテスト)

- [ ] **Step 3: 実装を書く**

`;;; 表示` 節の `wamei/project-memo-toggle` を次に置き換える。

```elisp
(defun wamei/project-memo--show-in-main-window (buffer)
  "BUFFER を本文 window に出す。既に出ていれば元のバッファに戻る。

戻り先は window パラメータに退避する。メモから別のメモへ切り替えた
ときは上書きせず、最初にメモを出す前のバッファを保つ。"
  (let ((window (wamei/project-tabs-main-window)))
    (if (eq (window-buffer window) buffer)
        (wamei/project-memo--restore window)
      (unless (wamei/project-memo-buffer-p (window-buffer window))
        (set-window-parameter window 'wamei/project-memo-back (window-buffer window)))
      (set-window-buffer window buffer)
      (select-window window))))

(defun wamei/project-memo--toggle (global main-window)
  "メモを出す。GLOBAL が非 nil なら全体メモ、nil ならプロジェクトメモ。

MAIN-WINDOW が非 nil なら本文 window、nil なら画面中央の posframe に出す。
posframe が使えない環境 (`posframe-workable-p' が nil、batch や child frame
非対応の端末) では MAIN-WINDOW によらず本文 window に落とす。

同じ表示先を続けて求められたら閉じる (トグル)。表示先を変えるときは先に
posframe を閉じ、表示先が 2 つに増えないようにする。"
  (let* ((project (unless global (wamei/project-memo--project)))
         (buffer (wamei/project-memo-buffer project))
         (use-posframe (and (not main-window) (posframe-workable-p))))
    (cond
     (use-posframe
      (if (wamei/project-memo-posframe-frame)
          (wamei/project-memo-posframe-hide)
        (wamei/project-memo-posframe-show buffer)))
     (t
      (wamei/project-memo-posframe-hide)
      (wamei/project-memo--show-in-main-window buffer)))))

(defun wamei/project-memo-toggle (&optional main-window)
  "プロジェクトメモを画面中央の posframe に出す。出ていれば閉じる。

MAIN-WINDOW (`C-u') が非 nil なら posframe ではなく本文 window に出す。
タブがプロジェクトに紐づいていないときは全体メモになる。

posframe が使えない環境では `C-u' 無しでも本文 window に出る。"
  (interactive "P")
  (wamei/project-memo--toggle nil main-window))

(defun wamei/project-memo-toggle-global (&optional main-window)
  "全体メモを画面中央の posframe に出す。出ていれば閉じる。

MAIN-WINDOW (`C-u') が非 nil なら posframe ではなく本文 window に出す。
posframe が使えない環境では `C-u' 無しでも本文 window に出る。"
  (interactive "P")
  (wamei/project-memo--toggle t main-window))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS (53 + 5 = 58 本)

- [ ] **Step 5: コミット**

```bash
cd <worktree>
git add .emacs.d/project-memo.el .emacs.d/project-memo-test.el
git commit -m "$(cat <<'EOF'
Make the prefix argument choose the display target, not the memo

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01P54ynGfJYgZ8dapJev2YBy
EOF
)"
```

---

## Task 5: Commentary の更新と init.el への配線、実機確認

**Files:**
- Modify: `.emacs.d/project-memo.el` (Commentary)
- Modify: `.emacs.d/init.el` (`leaf project-memo` の `:bind` とコメント)

**Interfaces:**
- Consumes: `wamei/project-memo-toggle` / `wamei/project-memo-toggle-global` (Task 4)
- Produces: なし (配線のみ)

- [ ] **Step 1: Commentary を更新する**

`project-memo.el` の Commentary の箇条書きに、表示先の話を足す。既存の行は消さない。

```elisp
;; - 既定の表示先は画面中央の posframe (`wamei/project-memo-toggle')。
;;   `C-u' を付けると本文 window に出す。posframe が使えない環境
;;   (`posframe-workable-p' が nil) では `C-u' 無しでも本文 window に落とす
;; - posframe はフォーカスが外れたとき、メモ以外のバッファが入ったとき、
;;   もう一度トグルしたときに閉じる。いずれも閉じる前に保存する。
;;   ESC / C-g では閉じない (どちらも org の編集中に使う)
```

- [ ] **Step 2: init.el に `C-x C-S-m` を足す**

`leaf project-memo` の `:bind` を次にする。既存の `C-x C-m` のコメントはそのまま残す。

```elisp
  :bind (("C-x C-m" . wamei/project-memo-toggle)
         ;; 全体メモ。C-x C-M と書くと C-x C-m と同じキー列になってしまうので
         ;; (kbd の "C-M" は Control 文字の C-m そのもの)、Shift を明示する。
         ;; GUI では確実に届く。端末では modifyOtherKeys で送られる場合だけ届く。
         ("C-x C-S-m" . wamei/project-memo-toggle-global))
```

- [ ] **Step 3: init.el が構文として読めることを確認**

```bash
cd <worktree>/.emacs.d && emacs -Q --batch --eval '(with-temp-buffer (insert-file-contents "init.el") (goto-char (point-min)) (condition-case err (while t (read (current-buffer))) (end-of-file (princ "read ok")) (error (princ (format "READ ERROR: %S" err)))))'
```

Expected: `read ok`

- [ ] **Step 4: 隔離 daemon で配線を確認する**

worktree 側の init.el を指すこと (`~/.emacs.d/init.el` は master の実体を指すので使わない)。

```bash
D=$(mktemp -d)
W=<worktree>/.emacs.d
ln -s ~/.emacs.d/elpa "$D/elpa"
ln -s ~/.emacs.d/.cache "$D/.cache"
ln -s "$W/init.el" "$D/init.el"
ln -s "$W/early-init.el" "$D/early-init.el"
emacs --init-directory="$D" --daemon=memo-posframe-check 2>&1 | tail -20
```

確認する項目:

```bash
# 0. worktree の init.el を読んでいる
emacsclient -s memo-posframe-check -e '(file-truename user-init-file)'
# Expected: <worktree>/.emacs.d/init.el

# 1. キーが両方割り当たっている
emacsclient -s memo-posframe-check -e '(list (key-binding (kbd "C-x C-m")) (key-binding (kbd "C-x C-S-m")))'
# Expected: (wamei/project-memo-toggle wamei/project-memo-toggle-global)

# 2. daemon (フレーム無し) では posframe-workable-p が nil で、フォールバックが効く
emacsclient -s memo-posframe-check -e '(posframe-workable-p)'

# 3. GUI フレームを作って posframe を実際に出す
emacsclient -s memo-posframe-check -c -e '(progn (wamei/project-memo-toggle-global) (sleep-for 1) (list :frame (and (wamei/project-memo-posframe-frame) t) :focused (eq (selected-frame) (wamei/project-memo-posframe-frame)) :size (when (wamei/project-memo-posframe-frame) (cons (frame-width (wamei/project-memo-posframe-frame)) (frame-height (wamei/project-memo-posframe-frame))))))'
# Expected: (:frame t :focused t :size (W . H)) — W/H は親フレームの 0.6 倍前後
```

`sit-for` は即返るので使わない (`sleep-for` を使う)。`screencapture` は権限で落ちるので使わない。

終わったら PID で止める (`pkill -f daemon=` では落ちないことがある)。

```bash
emacsclient -s memo-posframe-check -e '(kill-emacs)' || pkill -f "daemon=memo-posframe-check"
pgrep -fl "daemon=memo-posframe-check" || echo "no stray daemon"
```

- [ ] **Step 5: tty で `C-x C-S-m` が届くか調べる**

Emacs 側が modifyOtherKeys の `C-S-m` を受け取れるかを機械的に確認する。tmux で `emacs -Q -nw` を上げ、`xterm-extra-capabilities` が有効な状態で raw シーケンスを流し込む。

```bash
tmux kill-session -t memokey 2>/dev/null
tmux new-session -d -s memokey -x 100 -y 30 \
  'TERM=xterm-256color emacs -Q -nw --eval "(progn (setq xterm-extra-capabilities (quote (modifyOtherKeys))) (global-set-key (kbd \"C-x C-S-m\") (lambda () (interactive) (message \"GOT C-S-m\"))) (global-set-key (kbd \"C-x C-m\") (lambda () (interactive) (message \"GOT C-m\"))))"'
sleep 3
# C-x のあと modifyOtherKeys 形式の C-S-m (CSI 27;6;109~) を送る
tmux send-keys -t memokey C-x
tmux send-keys -t memokey -H 1b 5b 32 37 3b 36 3b 31 30 39 7e
sleep 1
tmux capture-pane -p -t memokey | tail -3
tmux kill-session -t memokey
```

Expected: `GOT C-S-m`。`GOT C-m` が出た場合は Emacs 側が区別できていない。

**この結果は報告するだけでよい。** 実際に Ghostty がそのシーケンスを送るかはユーザーが `C-h k` で確認する必要があり、それは自動化できない。結果を report に書き、届かない場合の代替キー案 (`C-q m` / `C-q M` など、既存の `C-q` プレフィクスに揃える) を 1 つ添えること。

- [ ] **Step 6: 全テストを通す**

```bash
cd <worktree>/.emacs.d && for f in project-memo-test.el project-tabs-test.el project-sidebar-test.el; do echo "--- $f"; emacs -Q --batch -l $f -f ert-run-tests-batch-and-exit 2>&1 | tail -2; done
```

Expected: 3 つとも 0 unexpected

- [ ] **Step 7: コミット**

```bash
cd <worktree>
git add .emacs.d/init.el .emacs.d/project-memo.el
git commit -m "$(cat <<'EOF'
Bind the global memo to C-x C-S-m

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01P54ynGfJYgZ8dapJev2YBy
EOF
)"
```

---

## 完了条件

- `project-memo-test.el` / `project-tabs-test.el` / `project-sidebar-test.el` が 3 つとも 0 unexpected
- テストが post-command-hook・posframe フレーム・一時ディレクトリを残さない
- 隔離 daemon の GUI フレームで posframe が中央に出てフォーカスが移る
- `C-x C-m` / `C-x C-S-m` が両方 bind されている
- tty の `C-S-m` 到達性の調査結果が report にある (ユーザー確認が残ることを明記)
