# tty で画像ファイルを開く 実装計画 (Phase 1)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** tty の Emacs で画像ファイルを開いたとき、kitty graphics protocol の Unicode placeholder で画像を表示し、window の大きさに追従させる。

**Architecture:** 既存 `dired-image-preview-kitty.el` から端末描画層を `kitty-graphics.el` に切り出し、その上に新しい major mode `wamei/tty-image-mode` を載せる。入口は `image-mode` への `:override` advice 1 箇所。画像はバッファのテキストを壊さず `display` テキストプロパティとして被せる。

**Tech Stack:** Emacs Lisp (Emacs 31.1)、ERT (batch)、leaf.el、sips (macOS)、kitty graphics protocol、Ghostty 1.3.1、tmux 3.7

**Spec:** `docs/superpowers/specs/2026-09-09-tty-image-mode-design.md`

## Global Constraints

- 作業ディレクトリは `/Users/wamei/.dotfiles`。Emacs Lisp は `.emacs.d/` 直下に置く
- テストの実行は `.emacs.d` で `emacs -Q --batch -l <name>-test.el -f ert-run-tests-batch-and-exit`
- すべての関数・変数の prefix は `wamei/`。内部用は `--` を挟む (`wamei/kitty-graphics--send`)
- docstring とコメントは日本語 (このリポジトリの既存コードに合わせる)
- 画像 ID は 1〜255 の巡回 (256 色端末では前景色 `color-N` で表すため)
- `error` を投げない。失敗はメッセージ + `image-mode-as-text` へのフォールバック
- init.el は symlink なので、モジュールの `load` は `(file-name-directory (file-truename user-init-file))` を基準にする
- 対象は tty で起動した Emacs のみ。GUI 起動の Emacs に `emacsclient -nw` を足す使い方は対象外

**spec からの意図的な差分:** spec は再描画のきっかけとして `window-size-change-functions` と `window-configuration-change-hook` の 2 系統を挙げているが、`window-configuration-change-hook` は window のリサイズでも走るため、buffer-local な後者 1 本だけにする (グローバルフックを増やさない)。frame のリサイズで追従することは Task 5 の実端末確認で担保する。

---

## File Structure

| ファイル | 責務 |
|---|---|
| `.emacs.d/kitty-graphics.el` (新規) | 端末に画像を置き placeholder 文字列を返す。バッファも frame も dired も知らない |
| `.emacs.d/kitty-graphics-test.el` (新規) | 上の batch テスト |
| `.emacs.d/tty-image-mode.el` (新規) | 画像ファイルを開くバッファ (major mode + advice) |
| `.emacs.d/tty-image-mode-test.el` (新規) | 上の batch テスト |
| `.emacs.d/dired-image-preview-kitty.el` (改) | child frame 固有部分だけ残す |
| `.emacs.d/dired-image-preview-kitty-test.el` (改) | 移設したテストを削る |
| `.emacs.d/init.el` (改) | leaf を 2 つ追加、行番号の hook に `image-mode-hook` |

---

### Task 1: kitty-graphics.el を切り出す

**Files:**
- Create: `.emacs.d/kitty-graphics.el`
- Create: `.emacs.d/kitty-graphics-test.el`
- Modify: `.emacs.d/dired-image-preview-kitty.el` (全面)
- Modify: `.emacs.d/dired-image-preview-kitty-test.el` (移設分を削除)
- Modify: `.emacs.d/init.el` (leaf 追加)

**Interfaces:**
- Consumes: なし (既存コードの移動)
- Produces:
  - `(wamei/kitty-graphics-available-p)` → boolean
  - `(wamei/kitty-graphics-cell-size)` → `(幅 . 高さ)` ピクセル
  - `(wamei/kitty-graphics-image-size FILE)` → `(幅 . 高さ)` ピクセル or nil
  - `(wamei/kitty-graphics-cell-count IMAGE-PX CELL-PX MAX-CELLS)` → `(桁 . 行)`
  - `(wamei/kitty-graphics-put FILE COLS ROWS)` → 画像 ID (integer) or nil
  - `(wamei/kitty-graphics-delete ID)` → 副作用のみ
  - `(wamei/kitty-graphics-placeholder-line ID COLS ROW &optional COLOR)` → string
  - `(wamei/kitty-graphics-placeholder-string ID COLS ROWS)` → string (行は `\n` 区切り)
  - defcustom `wamei/kitty-graphics-max-pixels` (1200)、`wamei/kitty-graphics-response-timeout` (0.5)

- [ ] **Step 1: テストを新ファイルへ移す (Red)**

`.emacs.d/kitty-graphics-test.el` を新規作成する。`.emacs.d/dired-image-preview-kitty-test.el` から次の 18 個の `ert-deftest` を**そのまま切り取って**貼り、テスト名と呼び出している関数名の `wamei/dired-image-preview-kitty` を `wamei/kitty-graphics` に一括置換する (内部関数の `--` は維持、下の対応表で公開になるものは `--` を取る)。

移すテスト: `wrap-passes-through-outside-tmux` / `wrap-uses-tmux-passthrough` / `chunks-splits-by-size` / `transmit-sequences-first-carries-control-keys` / `transmit-sequences-single-chunk` / `delete-sequence-frees-data` / `query-sequence` / `query-ok-p` / `parse-cell-size-reads-height-then-width` / `cell-count-fits-max-keeping-aspect` / `cell-count-does-not-upscale` / `cell-count-is-at-least-one` / `diacritic-table` / `color-name-depends-on-color-depth` / `placeholder-line-is-composed-cells` / `next-id-cycles-in-8-bits` / `resize-args-only-when-larger-than-limit` / `parse-sips-size`

公開になるもの (テスト側で `--` を取る): `--cell-count` → `-cell-count`、`--placeholder-line` → `-placeholder-line`。
内部のまま (`--` 維持): `--wrap` `--chunks` `--transmit-sequences` `--delete-sequence` `--query-sequence` `--query-ok-p` `--parse-cell-size` `--diacritic` `--color` `--next-id` `--last-id` `--resize-args` `--parse-sips-size`。

ファイルの頭は次のとおり:

```elisp
;;; kitty-graphics-test.el --- tests for kitty-graphics -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l kitty-graphics-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "kitty-graphics.el" dir) nil t))
```

末尾は:

```elisp
(provide 'kitty-graphics-test)
;;; kitty-graphics-test.el ends here
```

さらに新規テストを 1 つ足す (`placeholder-string` は新設の関数なので必ず落ちる):

```elisp
(ert-deftest wamei/kitty-graphics-placeholder-string-joins-rows-with-newline ()
  "ROWS 行ぶんの placeholder を改行で連ねる。各行は COLS 桁。"
  (cl-letf (((symbol-function 'display-color-cells) (lambda (&optional _) 256)))
    (let* ((s (wamei/kitty-graphics-placeholder-string 7 2 3))
           (lines (split-string s "\n")))
      (should (= (length lines) 3))
      (dolist (line lines)
        (should (= (string-width line) 2)))
      ;; 行番号の結合文字が行ごとに違う
      (should (= (aref (nth 0 lines) 1) (wamei/kitty-graphics--diacritic 0)))
      (should (= (aref (nth 1 lines) 1) (wamei/kitty-graphics--diacritic 1)))
      (should (= (aref (nth 2 lines) 1) (wamei/kitty-graphics--diacritic 2)))
      ;; 256 色モードでは前景色が color-N
      (should (equal (get-text-property 0 'face (nth 0 lines)) '(:foreground "color-7"))))))
```

テスト冒頭の `(require 'ert)` の下に `(require 'cl-lib)` を足しておく。

- [ ] **Step 2: 落ちることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l kitty-graphics-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL。`kitty-graphics.el` が無いので `load` の時点で "Cannot open load file"。

- [ ] **Step 3: kitty-graphics.el を作る (Green)**

`.emacs.d/dired-image-preview-kitty.el` の 1〜275 行あたりから次を**中身を変えずに**移し、prefix を `wamei/dired-image-preview-kitty` → `wamei/kitty-graphics` に置換する。

| 移す元 | 移した先 |
|---|---|
| `--chunk-size` `--placeholder` `--diacritics` (defconst) | 同名 (`--` 維持) |
| `--last-id` (defvar) | 同名 |
| `-max-pixels` `-response-timeout` (defcustom) | `wamei/kitty-graphics-max-pixels` / `-response-timeout` |
| `--wrap` `--chunks` `--transmit-sequences` `--delete-sequence` `--query-sequence` `--query-ok-p` `--parse-cell-size` `--diacritic` `--color` `--next-id` `--parse-sips-size` `--resize-args` `--prepare-png` `--send` `--read-response` `--query-terminal` | 同名 (`--` 維持) |
| `--cell-count` | `wamei/kitty-graphics-cell-count` (公開) |
| `--image-size` | `wamei/kitty-graphics-image-size` (公開) |
| `--cell-size` | `wamei/kitty-graphics-cell-size` (公開) |
| `-available-p` | `wamei/kitty-graphics-available-p` (公開) — **`(featurep 'tty-child-frames)` の条件は移さない** |
| `--placeholder-line` | `wamei/kitty-graphics-placeholder-line` (公開、COLOR を optional に) |

defgroup は次に置き換える:

```elisp
(defgroup wamei/kitty-graphics nil
  "kitty graphics protocol で端末に画像を出す。"
  :group 'multimedia)
```

`available-p` は child frame の条件を落とす:

```elisp
(defun wamei/kitty-graphics-available-p ()
  "この端末が kitty graphics の Unicode placeholder を使えるなら非 nil。
結果は端末ごとに記憶する。"
  (and (not (display-graphic-p))
       (wamei/kitty-graphics--query-terminal
        'wamei/kitty-graphics-supported
        (wamei/kitty-graphics--query-sequence)
        (lambda (response)
          (and (wamei/kitty-graphics--query-ok-p response) t)))))
```

`placeholder-line` は COLOR を optional にし、省略時は ID から作る:

```elisp
(defun wamei/kitty-graphics-placeholder-line (id cols row &optional color)
  "画像 ID の ROW 行目を COLS 桁ぶん描く placeholder の文字列。前景色は COLOR。
COLOR を省くと ID と端末の色数から決める。
1 セル (基底 + 行 + 桁の結合文字) ずつ合成して 1 桁のグリフにする。"
  (let ((color (or color (wamei/kitty-graphics--color id (display-color-cells))))
        ;; make-string で作る文字列は unibyte になり多バイト文字を aset できないので、
        ;; 文字のリストから multibyte 文字列を組む
        (line (apply #'string
                     (cl-loop for col below cols
                              collect wamei/kitty-graphics--placeholder
                              collect (wamei/kitty-graphics--diacritic row)
                              collect (wamei/kitty-graphics--diacritic col)))))
    (dotimes (col cols)
      (compose-string line (* col 3) (+ (* col 3) 3)))
    (add-face-text-property 0 (length line) `(:foreground ,color) nil line)
    line))
```

新設する関数 3 つ:

```elisp
(defun wamei/kitty-graphics-placeholder-string (id cols rows)
  "画像 ID を COLS x ROWS のセルに描く placeholder の文字列。行は改行で連ねる。"
  (let ((color (wamei/kitty-graphics--color id (display-color-cells))))
    (mapconcat (lambda (row) (wamei/kitty-graphics-placeholder-line id cols row color))
               (number-sequence 0 (1- rows))
               "\n")))

(defun wamei/kitty-graphics-put (file cols rows)
  "FILE を端末へ送り COLS x ROWS のセルに描く配置を作る。画像 ID を返す。
送れなければ nil。ID の解放は呼び手の責任 (`wamei/kitty-graphics-delete')。
端末は画像を COLS x ROWS の矩形に合わせて拡縮するので、転送するピクセル数は
`wamei/kitty-graphics-max-pixels' まで落としてよい。"
  (when-let* ((prepared (wamei/kitty-graphics--prepare-png file)))
    (unwind-protect
        (let* ((id (wamei/kitty-graphics--next-id))
               (b64 (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally (car prepared))
                      (base64-encode-string (buffer-string) t))))
          (dolist (seq (wamei/kitty-graphics--transmit-sequences b64 id cols rows))
            (wamei/kitty-graphics--send seq))
          id)
      (delete-file (car prepared)))))

(defun wamei/kitty-graphics-delete (id)
  "画像 ID の配置とデータを端末から解放する。ID が nil なら何もしない。"
  (when id
    (wamei/kitty-graphics--send (wamei/kitty-graphics--delete-sequence id))))
```

ファイルの頭は `(require 'cl-lib)` と defgroup、末尾は `(provide 'kitty-graphics)`。Commentary には spec の「前提と制約」から、Unicode placeholder の仕組み・`compose-string` が要る理由・256 色で ID が 1〜255 に限られること・応答を `read-event` で回収することを書く (元ファイルの Commentary をほぼそのまま流用してよい)。

- [ ] **Step 4: テストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l kitty-graphics-test.el -f ert-run-tests-batch-and-exit
```

Expected: 19 tests PASS。

- [ ] **Step 5: dired-image-preview-kitty.el を薄くする**

`.emacs.d/dired-image-preview-kitty.el` を書き直す。残すのは child frame 固有部分だけ。

先頭:

```elisp
(require 'cl-lib)
(require 'dired-image-preview)
(require 'kitty-graphics)
```

`available-p` は child frame の条件をここで持つ:

```elisp
(defun wamei/dired-image-preview-kitty-available-p ()
  "この端末で kitty graphics のプレビューを出せるなら非 nil。
tty child frame が使え、端末が kitty graphics に対応していること。"
  (and (featurep 'tty-child-frames)
       (wamei/kitty-graphics-available-p)))
```

`--fill-buffer` は placeholder の組み立てを委譲する:

```elisp
(defun wamei/dired-image-preview-kitty--fill-buffer (id cols rows)
  "placeholder バッファに画像 ID の COLS x ROWS を並べて返す。"
  (with-current-buffer (get-buffer-create wamei/dired-image-preview-kitty--buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (wamei/kitty-graphics-placeholder-string id cols rows))
      (goto-char (point-min)))
    (setq-local mode-line-format nil
                header-line-format nil
                cursor-type nil
                truncate-lines t
                show-trailing-whitespace nil
                cursor-in-non-selected-windows nil)
    (current-buffer)))
```

`show` は転送を `wamei/kitty-graphics-put` に委譲する。画像の大きさは元ファイルから測る (端末が矩形に合わせて拡縮するので、縮小後のピクセル数を待つ必要はない):

```elisp
(defun wamei/dired-image-preview-kitty-show (target)
  "TARGET の画像を kitty graphics で child frame に表示する。"
  (wamei/dired-image-preview-kitty-hide)
  (let* ((window (wamei/dired-image-preview--target-window target))
         (frame (window-frame window))
         (file (wamei/dired-image-preview--target-file target))
         (image-px (wamei/kitty-graphics-image-size file)))
    (when image-px
      (let* ((cells (wamei/kitty-graphics-cell-count
                     image-px (wamei/kitty-graphics-cell-size)
                     (wamei/dired-image-preview-kitty--max-cells frame)))
             (id (wamei/kitty-graphics-put file (car cells) (cdr cells))))
        (when id
          (let* ((anchor (window-absolute-pixel-position
                          (wamei/dired-image-preview--target-anchor target) window))
                 (position (wamei/dired-image-preview-kitty--frame-position
                            (or anchor '(0 . 0)) cells
                            (cons (frame-width frame) (frame-height frame))
                            wamei/dired-image-preview-gap)))
            (setq wamei/dired-image-preview-kitty--shown-id id)
            (setq wamei/dired-image-preview-kitty--frame
                  (wamei/dired-image-preview-kitty--make-frame
                   frame (wamei/dired-image-preview-kitty--fill-buffer id (car cells) (cdr cells))
                   position cells))))))))
```

`hide` は解放を委譲する:

```elisp
(defun wamei/dired-image-preview-kitty-hide ()
  "表示中のプレビューを消し、端末側の画像データも解放する。"
  (when (frame-live-p wamei/dired-image-preview-kitty--frame)
    (delete-frame wamei/dired-image-preview-kitty--frame t))
  (setq wamei/dired-image-preview-kitty--frame nil)
  (when wamei/dired-image-preview-kitty--shown-id
    (wamei/kitty-graphics-delete wamei/dired-image-preview-kitty--shown-id)
    (setq wamei/dired-image-preview-kitty--shown-id nil)))
```

`--frame-position` `--make-frame` `--max-cells` `-setup` と defvar 3 つ (`--buffer-name` `--frame` `--shown-id`) はそのまま残す。defgroup は `:group 'wamei/dired-image-preview` のまま残してよいが、中身の defcustom は無くなるので削除する。移した関数・定数・defcustom はすべて消す。

- [ ] **Step 6: 移設済みのテストを消す**

`.emacs.d/dired-image-preview-kitty-test.el` から Step 1 で移した 18 個の `ert-deftest` を削除する。残すのは `frame-position-below-right-of-anchor` / `frame-position-flips-above-when-no-room-below` / `frame-position-clamps-to-right-edge` / `make-frame-keeps-selected-frame` / `make-frame-does-not-reselect-when-unchanged` の 5 個。ファイル冒頭の `load` に `kitty-graphics.el` を足す:

```elisp
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "kitty-graphics.el" dir) nil t)
  (load (expand-file-name "dired-image-preview.el" dir) nil t)
  (load (expand-file-name "dired-image-preview-kitty.el" dir) nil t))
```

- [ ] **Step 7: 両方のテストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && \
  emacs -Q --batch -l kitty-graphics-test.el -f ert-run-tests-batch-and-exit && \
  emacs -Q --batch -l dired-image-preview-kitty-test.el -f ert-run-tests-batch-and-exit && \
  emacs -Q --batch -l dired-image-preview-test.el -f ert-run-tests-batch-and-exit
```

Expected: 3 本とも PASS (19 / 5 / 既存件数)。

- [ ] **Step 8: init.el に leaf を足す**

`.emacs.d/init.el` の `(leaf dired-image-preview-kitty ...)` (1091 行あたり) の**直前**に次を挿入する:

```elisp
(leaf kitty-graphics
  :doc "kitty graphics protocol (Unicode placeholder) で端末に画像を出す"
  :ensure nil
  ;; tty のときだけ読む。dired-image-preview-kitty と tty-image-mode の土台。
  :if (not (display-graphic-p))
  :preface
  (load (expand-file-name "kitty-graphics"
                          (file-name-directory (file-truename user-init-file)))
        nil t))
```

- [ ] **Step 9: 起動が壊れていないことを確認**

```bash
cd /Users/wamei/.dotfiles && emacs -Q --batch --eval '(progn (load "/Users/wamei/.dotfiles/.emacs.d/kitty-graphics.el") (message "loaded: %s" (featurep (quote kitty-graphics))))'
```

Expected: `loaded: t`、警告なし。

- [ ] **Step 10: commit**

```bash
cd /Users/wamei/.dotfiles && git add .emacs.d/kitty-graphics.el .emacs.d/kitty-graphics-test.el .emacs.d/dired-image-preview-kitty.el .emacs.d/dired-image-preview-kitty-test.el .emacs.d/init.el && git commit -m "$(cat <<'EOM'
kitty graphics の描画層を切り出す

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
EOM
)"
```

---

### Task 2: tty-image-mode の描画コア

**Files:**
- Create: `.emacs.d/tty-image-mode.el`
- Create: `.emacs.d/tty-image-mode-test.el`

**Interfaces:**
- Consumes: `wamei/kitty-graphics-image-size` / `-cell-size` / `-cell-count` / `-put` / `-delete` / `-placeholder-string` (Task 1)
- Produces:
  - `wamei/tty-image--id` (buffer-local, integer or nil)
  - `wamei/tty-image--cells` (buffer-local, `(桁 . 行)` or nil)
  - `(wamei/tty-image--window-cells WINDOW)` → `(桁 . 行)`
  - `(wamei/tty-image--target-cells FILE WINDOW)` → `(桁 . 行)` or nil
  - `(wamei/tty-image--show ID CELLS)` → 副作用のみ
  - `(wamei/tty-image--forget)` → 副作用のみ
  - `(wamei/tty-image--render)` → 副作用のみ

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/tty-image-mode-test.el` を新規作成:

```elisp
;;; tty-image-mode-test.el --- tests for tty-image-mode -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l tty-image-mode-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "kitty-graphics.el" dir) nil t)
  (load (expand-file-name "tty-image-mode.el" dir) nil t))

;;; 大きさ

(ert-deftest wamei/tty-image-window-cells-uses-body-size ()
  "上限は window の本文の桁数・行数。"
  (cl-letf (((symbol-function 'window-body-width) (lambda (&optional _w) 80))
            ((symbol-function 'window-body-height) (lambda (&optional _w) 24)))
    (should (equal (wamei/tty-image--window-cells 'window) '(80 . 24)))))

(ert-deftest wamei/tty-image-window-cells-is-at-least-one ()
  "0 桁・0 行の window でも 1 を下回らない (0 を渡すと転送が壊れる)。"
  (cl-letf (((symbol-function 'window-body-width) (lambda (&optional _w) 0))
            ((symbol-function 'window-body-height) (lambda (&optional _w) 0)))
    (should (equal (wamei/tty-image--window-cells 'window) '(1 . 1)))))

(ert-deftest wamei/tty-image-target-cells-fits-image-into-window ()
  "画像のピクセル数とセルのピクセル数から、window に収まる (桁 . 行) を出す。"
  (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(800 . 400)))
            ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
            ((symbol-function 'window-body-width) (lambda (&optional _w) 40))
            ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
    ;; 800/8 = 100 桁、400/16 = 25 行。40 桁に収めるので 0.4 倍 → 40 x 10
    (should (equal (wamei/tty-image--target-cells "a.png" 'window) '(40 . 10)))))

(ert-deftest wamei/tty-image-target-cells-nil-without-window ()
  "window に出ていなければ nil。"
  (should-not (wamei/tty-image--target-cells "a.png" nil)))

(ert-deftest wamei/tty-image-target-cells-nil-when-size-unknown ()
  "大きさが測れない (画像でない) なら nil。"
  (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) nil))
            ((symbol-function 'window-body-width) (lambda (&optional _w) 40))
            ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
    (should-not (wamei/tty-image--target-cells "a.txt" 'window))))

;;; バッファへの被せ方

(ert-deftest wamei/tty-image-show-covers-buffer-without-modifying-it ()
  "バッファのテキストは残したまま display プロパティで placeholder を被せ、
変更フラグを立てない (ファイル訪問バッファを壊さないため)。"
  (cl-letf (((symbol-function 'wamei/kitty-graphics-placeholder-string)
             (lambda (_id _cols _rows) "PLACEHOLDER")))
    (with-temp-buffer
      (insert "\x89PNG\r\n\x1a\n")
      (set-buffer-modified-p nil)
      (let ((text (buffer-string)))
        (wamei/tty-image--show 7 '(4 . 2))
        (should (equal (buffer-string) text))
        (should-not (buffer-modified-p))
        (should (equal (get-text-property (point-min) 'display) "PLACEHOLDER"))
        (should (equal (get-text-property (1- (point-max)) 'display) "PLACEHOLDER"))))))

;;; 再描画

(ert-deftest wamei/tty-image-render-transmits-once-when-size-unchanged ()
  "大きさが変わらなければ 2 回目は転送しない。"
  (let ((puts 0) (deletes 0))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(80 . 80)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-string) (lambda (&rest _) "P"))
              ((symbol-function 'wamei/kitty-graphics-put)
               (lambda (&rest _) (setq puts (1+ puts)) 7))
              ((symbol-function 'wamei/kitty-graphics-delete)
               (lambda (&rest _) (setq deletes (1+ deletes))))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&optional _w) 40))
              ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
      (with-temp-buffer
        (insert "data")
        (setq buffer-file-name "/tmp/a.png")
        (wamei/tty-image--render)
        (wamei/tty-image--render)
        (should (= puts 1))
        (should (= deletes 0))
        (should (= wamei/tty-image--id 7))
        (setq buffer-file-name nil)))))

(ert-deftest wamei/tty-image-render-retransmits-when-size-changes ()
  "大きさが変わったら古い画像を解放してから送り直す。"
  (let ((puts 0) (deleted nil) (width 40))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(80 . 80)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-string) (lambda (&rest _) "P"))
              ((symbol-function 'wamei/kitty-graphics-put)
               (lambda (&rest _) (setq puts (1+ puts)) puts))
              ((symbol-function 'wamei/kitty-graphics-delete)
               (lambda (id) (push id deleted)))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&optional _w) width))
              ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
      (with-temp-buffer
        (insert "data")
        (setq buffer-file-name "/tmp/a.png")
        (wamei/tty-image--render)
        (setq width 20)
        (wamei/tty-image--render)
        (should (= puts 2))
        (should (equal deleted '(1)))
        (should (= wamei/tty-image--id 2))
        (setq buffer-file-name nil)))))

(ert-deftest wamei/tty-image-forget-releases-the-image ()
  "解放すると ID と大きさの記憶を捨てる。"
  (let ((deleted nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-delete)
               (lambda (id) (push id deleted))))
      (with-temp-buffer
        (setq wamei/tty-image--id 9
              wamei/tty-image--cells '(4 . 2))
        (wamei/tty-image--forget)
        (should (equal deleted '(9)))
        (should-not wamei/tty-image--id)
        (should-not wamei/tty-image--cells)))))

(provide 'tty-image-mode-test)
;;; tty-image-mode-test.el ends here
```

- [ ] **Step 2: 落ちることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-mode-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL。`tty-image-mode.el` が無いので "Cannot open load file"。

- [ ] **Step 3: 最小の実装を書く**

`.emacs.d/tty-image-mode.el` を新規作成:

```elisp
;;; tty-image-mode.el --- tty の Emacs で画像ファイルを開く -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; tty の Emacs では `image-mode' が入口で失敗する (image-mode.el の
;; `(unless (display-images-p) (error "Display does not support images"))')。
;; ここでは kitty graphics protocol の Unicode placeholder で画像を描く major mode を
;; 用意し、tty のときだけ `image-mode' の代わりに使う。
;;
;; バッファのテキスト (ファイルの生データ) は壊さない。image-mode と同じく
;; `display' テキストプロパティとして placeholder を被せるだけなので、保存や
;; revert が壊れない。
;;
;;; Code:

(require 'kitty-graphics)

(defvar-local wamei/tty-image--id nil
  "端末に置いている画像の ID。無ければ nil。")

(defvar-local wamei/tty-image--cells nil
  "最後に描いた大きさ (桁 . 行)。無ければ nil。")

;;;; 大きさ

(defun wamei/tty-image--window-cells (window)
  "WINDOW に収まる上限の (桁 . 行)。0 は転送が壊れるので 1 を下回らない。"
  (cons (max 1 (window-body-width window))
        (max 1 (window-body-height window))))

(defun wamei/tty-image--target-cells (file window)
  "FILE を WINDOW いっぱいに出すときの (桁 . 行)。
WINDOW が nil か、FILE の大きさを測れなければ nil。"
  (when window
    (let ((size (wamei/kitty-graphics-image-size file)))
      (when size
        (wamei/kitty-graphics-cell-count
         size (wamei/kitty-graphics-cell-size)
         (wamei/tty-image--window-cells window))))))

;;;; 表示

(defun wamei/tty-image--show (id cells)
  "今のバッファ全体に、画像 ID を CELLS (桁 . 行) で描く placeholder を被せる。
テキストは書き換えず `display' プロパティを載せるだけ。変更フラグは元に戻す。"
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (modified (buffer-modified-p)))
    (put-text-property (point-min) (point-max) 'display
                       (wamei/kitty-graphics-placeholder-string
                        id (car cells) (cdr cells)))
    (set-buffer-modified-p modified)))

(defun wamei/tty-image--forget ()
  "端末に置いた画像を解放し、記憶を捨てる。"
  (wamei/kitty-graphics-delete wamei/tty-image--id)
  (setq wamei/tty-image--id nil
        wamei/tty-image--cells nil))

(defun wamei/tty-image--render ()
  "今の window の大きさに合わせて画像を描き直す。
大きさが前と同じなら何もしない (redisplay のたびに転送しないため)。"
  (let* ((file buffer-file-name)
         (window (get-buffer-window (current-buffer)))
         (cells (and file (wamei/tty-image--target-cells file window))))
    (when (and cells (not (equal cells wamei/tty-image--cells)))
      (wamei/tty-image--forget)
      (let ((id (wamei/kitty-graphics-put file (car cells) (cdr cells))))
        (if id
            (progn
              (setq wamei/tty-image--id id
                    wamei/tty-image--cells cells)
              (wamei/tty-image--show id cells))
          ;; `error' は投げない。tty で debug-on-error t だとデバッガに入って操作不能になる。
          (message "画像を端末へ送れませんでした: %s" (file-name-nondirectory file)))))))

(provide 'tty-image-mode)
;;; tty-image-mode.el ends here
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-mode-test.el -f ert-run-tests-batch-and-exit
```

Expected: 8 tests PASS。

- [ ] **Step 5: commit**

```bash
cd /Users/wamei/.dotfiles && git add .emacs.d/tty-image-mode.el .emacs.d/tty-image-mode-test.el && git commit -m "$(cat <<'EOM'
tty の画像バッファの描画コアを足す

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
EOM
)"
```

---

### Task 3: major mode とキー操作

**Files:**
- Modify: `.emacs.d/tty-image-mode.el`
- Modify: `.emacs.d/tty-image-mode-test.el`

**Interfaces:**
- Consumes: `wamei/tty-image--render` / `--forget` (Task 2)
- Produces:
  - `(wamei/tty-image--image-files DIR)` → 画像ファイルの絶対パスのリスト (名前順)
  - `(wamei/tty-image--sibling FILE FILES N)` → FILE から N 個ずれた要素 or nil
  - `(wamei/tty-image-next-file &optional N)` / `(wamei/tty-image-previous-file &optional N)` — command
  - `(wamei/tty-image-refresh)` — command
  - `wamei/tty-image-mode` — major mode、`wamei/tty-image-mode-map`

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/tty-image-mode-test.el` の `(provide ...)` の直前に足す:

```elisp
;;; 次/前のファイル

(ert-deftest wamei/tty-image-sibling-moves-within-the-list ()
  (let ((files '("/d/a.png" "/d/b.png" "/d/c.png")))
    (should (equal (wamei/tty-image--sibling "/d/a.png" files 1) "/d/b.png"))
    (should (equal (wamei/tty-image--sibling "/d/c.png" files -1) "/d/b.png"))))

(ert-deftest wamei/tty-image-sibling-stops-at-the-ends ()
  "端では nil を返す (巡回しない)。"
  (let ((files '("/d/a.png" "/d/b.png")))
    (should-not (wamei/tty-image--sibling "/d/b.png" files 1))
    (should-not (wamei/tty-image--sibling "/d/a.png" files -1))))

(ert-deftest wamei/tty-image-sibling-nil-when-file-not-listed ()
  (should-not (wamei/tty-image--sibling "/d/z.png" '("/d/a.png") 1)))

(ert-deftest wamei/tty-image-sibling-nil-for-single-file ()
  (should-not (wamei/tty-image--sibling "/d/a.png" '("/d/a.png") 1)))

(ert-deftest wamei/tty-image-image-files-filters-and-sorts ()
  "ディレクトリの中の画像だけを名前順に返す。"
  (let ((dir (make-temp-file "tty-image-test-" t)))
    (unwind-protect
        (progn
          (dolist (name '("b.png" "a.jpg" "notes.txt" "c.gif"))
            (write-region "" nil (expand-file-name name dir)))
          (should (equal (mapcar #'file-name-nondirectory
                                 (wamei/tty-image--image-files dir))
                         '("a.jpg" "b.png" "c.gif"))))
      (delete-directory dir t))))

;;; モード

(ert-deftest wamei/tty-image-mode-turns-off-line-numbers ()
  "行番号が桁を食うと placeholder の桁数が合わなくなるので必ず切る。"
  (cl-letf (((symbol-function 'wamei/tty-image--render) #'ignore))
    (with-temp-buffer
      (display-line-numbers-mode 1)
      (wamei/tty-image-mode)
      (should-not display-line-numbers-mode)
      (should truncate-lines)
      (should-not cursor-type))))

(ert-deftest wamei/tty-image-mode-renders-and-hooks-window-changes ()
  "モードに入ると描画し、window の変化で描き直すようにする。"
  (let ((renders 0))
    (cl-letf (((symbol-function 'wamei/tty-image--render)
               (lambda () (setq renders (1+ renders)))))
      (with-temp-buffer
        (wamei/tty-image-mode)
        (should (= renders 1))
        (should (memq #'wamei/tty-image--render
                      (buffer-local-value 'window-configuration-change-hook
                                          (current-buffer))))
        (should (memq #'wamei/tty-image--forget
                      (buffer-local-value 'kill-buffer-hook (current-buffer))))))))
```

- [ ] **Step 2: 落ちることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-mode-test.el -f ert-run-tests-batch-and-exit
```

Expected: 新しい 7 件が FAIL (`void-function wamei/tty-image--sibling` 等)。既存 8 件は PASS。

- [ ] **Step 3: 実装を足す**

`.emacs.d/tty-image-mode.el` の `(provide 'tty-image-mode)` の直前に足す。先頭の `require` に `(require 'image-file)` (`image-file-name-regexp` のため) と `(require 'seq)` (`seq-filter` / `seq-position` のため) を追加する。

```elisp
;;;; 次/前のファイル

(defun wamei/tty-image--image-files (dir)
  "DIR の中の画像ファイルの絶対パスを名前順に返す。"
  (let ((re (image-file-name-regexp)))
    (seq-filter (lambda (file)
                  (and (not (file-directory-p file))
                       (string-match-p re file)))
                (directory-files dir t))))

(defun wamei/tty-image--sibling (file files n)
  "FILES の中で FILE から N 個ずれた要素。端をはみ出すか FILE が無ければ nil。"
  (let ((i (seq-position files file #'equal)))
    (when i
      (let ((j (+ i n)))
        (when (and (>= j 0) (< j (length files)))
          (nth j files))))))

(defun wamei/tty-image-next-file (&optional n)
  "同じディレクトリの N 個あと (既定 1) の画像を開く。端なら何もしない。"
  (interactive "p" wamei/tty-image-mode)
  (let* ((file buffer-file-name)
         (next (and file
                    (wamei/tty-image--sibling
                     file (wamei/tty-image--image-files (file-name-directory file))
                     (or n 1)))))
    (if next
        (find-alternate-file next)
      (message "これ以上画像がありません"))))

(defun wamei/tty-image-previous-file (&optional n)
  "同じディレクトリの N 個まえ (既定 1) の画像を開く。端なら何もしない。"
  (interactive "p" wamei/tty-image-mode)
  (wamei/tty-image-next-file (- (or n 1))))

(defun wamei/tty-image-refresh ()
  "画像を送り直して描き直す。"
  (interactive nil wamei/tty-image-mode)
  (wamei/tty-image--forget)
  (wamei/tty-image--render))

;;;; モード

(defvar-keymap wamei/tty-image-mode-map
  :doc "`wamei/tty-image-mode' のキーマップ。"
  :parent special-mode-map
  "n" #'wamei/tty-image-next-file
  "p" #'wamei/tty-image-previous-file
  "g" #'wamei/tty-image-refresh)

(define-derived-mode wamei/tty-image-mode special-mode "TtyImage"
  "tty の Emacs で画像ファイルを見るためのモード。
kitty graphics protocol の Unicode placeholder で端末に描く。"
  (setq-local truncate-lines t
              cursor-type nil)
  ;; 行番号は見た目の問題ではない。桁を食われると placeholder の桁数が合わなくなる。
  (display-line-numbers-mode 0)
  (add-hook 'window-configuration-change-hook #'wamei/tty-image--render nil t)
  (add-hook 'kill-buffer-hook #'wamei/tty-image--forget nil t)
  (wamei/tty-image--render))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-mode-test.el -f ert-run-tests-batch-and-exit
```

Expected: 15 tests PASS。

- [ ] **Step 5: commit**

```bash
cd /Users/wamei/.dotfiles && git add .emacs.d/tty-image-mode.el .emacs.d/tty-image-mode-test.el && git commit -m "$(cat <<'EOM'
tty の画像バッファにモードとキー操作を足す

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
EOM
)"
```

---

### Task 4: image-mode の乗っ取りと init.el の配線

**Files:**
- Modify: `.emacs.d/tty-image-mode.el`
- Modify: `.emacs.d/tty-image-mode-test.el`
- Modify: `.emacs.d/init.el` (343 行あたりの hook リスト、Task 1 で足した leaf の直後)

**Interfaces:**
- Consumes: `wamei/tty-image-mode` (Task 3)、`wamei/kitty-graphics-available-p` (Task 1)
- Produces:
  - `(wamei/tty-image--image-mode-override &rest _)` — `image-mode` の `:override` advice
  - `(wamei/tty-image--as-text)` — テキスト表示へのフォールバック
  - `(wamei/tty-image-setup)` — advice を掛ける

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/tty-image-mode-test.el` の `(provide ...)` の直前に足す:

```elisp
;;; image-mode の乗っ取り

(ert-deftest wamei/tty-image-override-uses-tty-mode-when-available ()
  "端末が対応していて大きさも測れれば自分のモードで開く。"
  (let ((called nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () t))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(80 . 40)))
              ((symbol-function 'wamei/tty-image-mode) (lambda () (setq called 'tty)))
              ((symbol-function 'wamei/tty-image--as-text) (lambda () (setq called 'text))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/a.png")
        (wamei/tty-image--image-mode-override)
        (setq buffer-file-name nil))
      (should (eq called 'tty)))))

(ert-deftest wamei/tty-image-override-falls-back-to-text-when-unavailable ()
  "非対応端末では error を投げずテキスト表示に落ちる
\(tty で debug-on-error t だとデバッガに入って操作不能になるため)。"
  (let ((called nil) (messages nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () nil))
              ((symbol-function 'wamei/tty-image-mode) (lambda () (setq called 'tty)))
              ((symbol-function 'wamei/tty-image--as-text) (lambda () (setq called 'text)))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                            (push (apply #'format fmt args) messages))))
      (wamei/tty-image--image-mode-override)
      (should (eq called 'text))
      (should messages))))

(ert-deftest wamei/tty-image-override-falls-back-to-text-when-not-an-image ()
  "空ファイルや画像でないファイル (大きさを測れない) もテキスト表示に落ちる。"
  (let ((called nil) (messages nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () t))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) nil))
              ((symbol-function 'wamei/tty-image-mode) (lambda () (setq called 'tty)))
              ((symbol-function 'wamei/tty-image--as-text) (lambda () (setq called 'text)))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                            (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/empty.png")
        (wamei/tty-image--image-mode-override)
        (setq buffer-file-name nil))
      (should (eq called 'text))
      (should messages))))

(ert-deftest wamei/tty-image-setup-overrides-image-mode ()
  "setup で image-mode に override の advice が掛かる。"
  (unwind-protect
      (progn
        (wamei/tty-image-setup)
        (should (advice-member-p #'wamei/tty-image--image-mode-override 'image-mode)))
    (advice-remove 'image-mode #'wamei/tty-image--image-mode-override)))
```

- [ ] **Step 2: 落ちることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-mode-test.el -f ert-run-tests-batch-and-exit
```

Expected: 新しい 4 件が FAIL (`void-function wamei/tty-image--image-mode-override`)。既存 15 件は PASS。

- [ ] **Step 3: 実装を足す**

`.emacs.d/tty-image-mode.el` の `(provide 'tty-image-mode)` の直前に足す。先頭の `require` に `(require 'image-mode)` を追加する (`image-mode-as-text` のため)。

```elisp
;;;; image-mode の乗っ取り

(defun wamei/tty-image--image-mode-override (&rest _)
  "tty で `image-mode' の代わりに呼ばれる。
端末が kitty graphics に対応していて、かつファイルの大きさを測れれば
`wamei/tty-image-mode'。どちらかが駄目ならテキスト表示に落ちる。`error' は投げない。
空ファイルや画像でないファイルは「大きさを測れない」に含まれる。"
  (cond
   ((not (wamei/kitty-graphics-available-p))
    (message "この端末は kitty graphics に対応していないのでテキストとして開きます")
    (wamei/tty-image--as-text))
   ((not (and buffer-file-name (wamei/kitty-graphics-image-size buffer-file-name)))
    (message "画像として読めないのでテキストとして開きます")
    (wamei/tty-image--as-text))
   (t (wamei/tty-image-mode))))

(defun wamei/tty-image--as-text ()
  "画像を出せないときにテキスト表示へ落ちる。
`image-mode-as-text' は `major-mode-restore' に \='(image-mode image-mode-as-text)
を渡すので、auto-mode-alist から image-mode を外した状態で normal-mode を呼ぶ。
advice は image-mode に掛かっているため、ここから戻ってくる再帰は起きない。"
  (image-mode-as-text))

(defun wamei/tty-image-setup ()
  "tty で画像ファイルを開いたときに `wamei/tty-image-mode' が使われるようにする。
`auto-mode-alist' の拡張子エントリ、`M-x image-mode'、dired の RET、bookmark 復元は
すべて `image-mode' を通るので、ここ 1 箇所で足りる。"
  (advice-add 'image-mode :override #'wamei/tty-image--image-mode-override))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-mode-test.el -f ert-run-tests-batch-and-exit
```

Expected: 19 tests PASS。

- [ ] **Step 5: init.el に leaf を足す**

`.emacs.d/init.el` の `(leaf dired-image-preview-kitty ...)` の**直後**に挿入する:

```elisp
(leaf tty-image-mode
  :doc "tty では kitty graphics protocol で画像ファイルを表示する"
  :ensure nil
  ;; image-mode は tty では入口で error になるので、tty のときだけ横取りする。
  ;; 端末が対応しているかは最初に画像を開いたときに a=q で訊いて端末ごとに記憶する。
  :if (not (display-graphic-p))
  :preface
  (load (expand-file-name "tty-image-mode"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :config
  (wamei/tty-image-setup))
```

- [ ] **Step 6: GUI の image-mode で行番号を消す**

`.emacs.d/init.el` の 343 行あたり、`hide-mode-line` leaf の hook リストに `image-mode-hook` を足す。

変更前:

```elisp
  ((dired-mode-hook ghostel-mode-hook wamei/term-list-mode-hook)
   . (lambda() (display-line-numbers-mode 0))))
```

変更後:

```elisp
  ((dired-mode-hook ghostel-mode-hook wamei/term-list-mode-hook image-mode-hook)
   . (lambda() (display-line-numbers-mode 0))))
```

- [ ] **Step 7: GUI で行番号が消えることを確認**

```bash
cd /Users/wamei/.dotfiles && emacs -Q --batch --eval '(progn
  (setq global-display-line-numbers-mode t)
  (add-hook (quote image-mode-hook) (lambda () (display-line-numbers-mode 0)))
  (with-temp-buffer
    (run-hooks (quote image-mode-hook))
    (message "line-numbers: %s" display-line-numbers-mode)))'
```

Expected: `line-numbers: nil`。

- [ ] **Step 8: commit**

```bash
cd /Users/wamei/.dotfiles && git add .emacs.d/tty-image-mode.el .emacs.d/tty-image-mode-test.el .emacs.d/init.el && git commit -m "$(cat <<'EOM'
tty では image-mode の代わりに kitty graphics で画像を開く

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
EOM
)"
```

---

### Task 5: 実端末での受け入れ確認

**Files:**
- Create: `/private/tmp/claude-501/-Users-wamei--dotfiles/5a54e24c-3fb0-4210-9d83-80bd7d6bbc5b/scratchpad/tty-image-check.sh` (使い捨て、commit しない)

**Interfaces:**
- Consumes: Task 1〜4 のすべて
- Produces: 受け入れ条件 1〜6 の確認結果

**注意 (memory `kitty-graphics-placeholder-emacs-tty` より):**
- 生成したスクリプトは**起動前に必ず `cat` して**、実行ファイルへ書き込む形になっていないか目で確認する。`script(1)` は使わず `tmux` の `capture-pane` か Emacs 側のログにする
- 実 init を tty で起動する前に、隔離 dir の `.emacs.desktop-nw` を消す。GUI で開いた画像バッファが残っていると tty で image-mode がエラーになる
- tmux の中では `allow-passthrough on` が要る (`.tmux.conf` に設定済み)

- [ ] **Step 1: 確認用の画像を用意する**

外部の素材に頼らず、8x8 の PNG を base64 から起こして sips で引き伸ばす (`sips -Z` は小さい画像を拡大する)。

```bash
SCRATCH=/private/tmp/claude-501/-Users-wamei--dotfiles/5a54e24c-3fb0-4210-9d83-80bd7d6bbc5b/scratchpad
mkdir -p "$SCRATCH/images"
emacs -Q --batch --eval "(with-temp-file \"$SCRATCH/images/base.png\"
  (set-buffer-multibyte nil)
  (insert (base64-decode-string \"iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\")))"
# 400x200 (縦横比が 1 でないものを 1 枚は用意して、桁と行の比を確認できるようにする)
sips -s format png --resampleHeightWidth 200 400 "$SCRATCH/images/base.png" --out "$SCRATCH/images/a.png"
sips -s format jpeg "$SCRATCH/images/a.png" --out "$SCRATCH/images/b.jpg"
sips -s format png -Z 150 "$SCRATCH/images/a.png" --out "$SCRATCH/images/c.png"
rm -f "$SCRATCH/images/base.png"
sips -g pixelWidth -g pixelHeight "$SCRATCH/images"/*.png "$SCRATCH/images"/*.jpg
```

Expected: `a.png` (400x200)、`b.jpg` (400x200)、`c.png` (150x75) の 3 つができる。

- [ ] **Step 2: tmux の中で tty Emacs を起動して画像を開く**

```bash
SCRATCH=/private/tmp/claude-501/-Users-wamei--dotfiles/5a54e24c-3fb0-4210-9d83-80bd7d6bbc5b/scratchpad
rm -f ~/.emacs.d/.emacs.desktop-nw
tmux -f ~/.dotfiles/.tmux.conf new-session -d -s ttyimage -x 120 -y 40 \
  "emacs -nw --eval '(progn (setq inhibit-startup-screen t) (find-file \"$SCRATCH/images/a.png\"))'"
sleep 8
tmux capture-pane -p -t ttyimage | head -20
```

Expected: major mode が `TtyImage` になり、画面に placeholder のセルが並ぶ。`Display does not support images` が出ないこと。

- [ ] **Step 3: 1 セル = 1 桁に合成されていることを確認 (受け入れ条件 1)**

```bash
tmux send-keys -t ttyimage 'M-:' \
  '(message "mode=%s cells=%s width=%s" major-mode wamei/tty-image--cells (save-excursion (goto-char (point-min)) (- (line-end-position) (line-beginning-position))))' Enter
sleep 2
tmux capture-pane -p -t ttyimage | tail -3
```

Expected: `mode=wamei/tty-image-mode`、`cells` は `(N . M)` で **N > M**（400x200 の画像なので桁の方が多い）。`width` は buffer のテキストの文字数なので画面上の桁数と食い違ってよい。

**画面側の確認**:

```bash
tmux capture-pane -p -t ttyimage | sed -n 1p | wc -m
```

Expected: 1 行目の文字数が placeholder の桁数 N + 1 (改行) 程度に収まること。3 倍 (3N) になっていたら `compose-string` が効いていない。

- [ ] **Step 4: リサイズに追従することを確認 (受け入れ条件 2)**

```bash
tmux resize-window -t ttyimage -x 60 -y 20
sleep 2
tmux send-keys -t ttyimage 'M-:' '(message "cells=%s" wamei/tty-image--cells)' Enter
sleep 2
tmux capture-pane -p -t ttyimage | tail -3
```

Expected: `cells` が Step 3 より小さい値に変わっている。変わっていなければ `window-configuration-change-hook` が frame のリサイズで走っていないので、`window-size-change-functions` にも `wamei/tty-image--render` を掛ける (spec の 2 系統に戻す)。

- [ ] **Step 5: n / p が動くことを確認 (受け入れ条件 3)**

```bash
tmux send-keys -t ttyimage 'n'
sleep 2
tmux send-keys -t ttyimage 'M-:' '(message "file=%s" (file-name-nondirectory buffer-file-name))' Enter
sleep 2
tmux capture-pane -p -t ttyimage | tail -3
tmux send-keys -t ttyimage 'p'
sleep 2
tmux send-keys -t ttyimage 'M-:' '(message "file=%s" (file-name-nondirectory buffer-file-name))' Enter
sleep 2
tmux capture-pane -p -t ttyimage | tail -3
```

Expected: `file=b.jpg` → `file=a.png`。

- [ ] **Step 6: 閉じると画像が残らないことを確認 (受け入れ条件 4)**

```bash
tmux send-keys -t ttyimage 'M-:' '(progn (kill-buffer) (message "id=%s" (bound-and-true-p wamei/tty-image--id)))' Enter
sleep 2
tmux capture-pane -p -t ttyimage | tail -5
```

Expected: 画面に画像の残骸が残らない (placeholder の文字が消え、下のバッファがそのまま見える)。

- [ ] **Step 7: dired のプレビューが壊れていないことを確認 (受け入れ条件 6)**

```bash
SCRATCH=/private/tmp/claude-501/-Users-wamei--dotfiles/5a54e24c-3fb0-4210-9d83-80bd7d6bbc5b/scratchpad
tmux send-keys -t ttyimage 'M-:' "(dired \"$SCRATCH/images\")" Enter
sleep 3
tmux send-keys -t ttyimage 'n'
sleep 3
tmux capture-pane -p -t ttyimage | head -20
```

Expected: 画像ファイルの行にカーソルを置くと child frame のプレビューが出る。

- [ ] **Step 8: 後始末**

```bash
tmux kill-session -t ttyimage
```

- [ ] **Step 9: GUI が壊れていないことを確認 (受け入れ条件 5)**

GUI の Emacs を新しく起動し、`C-x C-f` で `$SCRATCH/images/a.png` を開く。画像が従来どおり表示され、**行番号が出ない**ことを目視で確認する。既に起動している Emacs があれば、そちらでは init を読み直さないと反映されない点に注意。

- [ ] **Step 10: 全テストをもう一度回して commit するものが無いことを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && for t in kitty-graphics tty-image-mode dired-image-preview dired-image-preview-kitty; do
  echo "--- $t"
  emacs -Q --batch -l "$t-test.el" -f ert-run-tests-batch-and-exit || echo "FAILED: $t"
done
cd /Users/wamei/.dotfiles && git status --short
```

Expected: 4 本とも PASS、`git status` が clean (scratchpad の生成物はリポジトリ外)。
