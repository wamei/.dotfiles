# tty で image-dired のサムネイル一覧を出す 実装計画 (Phase 2)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** tty の Emacs で `M-x image-dired` したときに、kitty graphics protocol の Unicode placeholder でサムネイルをグリッド表示し、スクロールに追従させる。

**Architecture:** `image-dired-thumbnail-mode` から派生した major mode を作り、バッファを段単位で組み立てる。点ベースの組み込みコマンド (マーク・dired 連動) は派生によってそのまま使い、走査するコマンド (移動・`x`・`RET`) だけを添字ベースで置き換える。画像 ID は 1〜255 しかないので、window に見えているサムネイルだけを端末へ送る。

**Tech Stack:** Emacs Lisp (Emacs 31.1)、ERT (batch)、leaf.el、sips (macOS)、kitty graphics protocol、Ghostty 1.3.1、tmux 3.7

**Spec:** `docs/superpowers/specs/2026-09-09-tty-image-dired-design.md`

## Global Constraints

- 作業ディレクトリは `/Users/wamei/.dotfiles`。Emacs Lisp は `.emacs.d/` 直下
- テスト実行: `.emacs.d` で `emacs -Q --batch -l <name>-test.el -f ert-run-tests-batch-and-exit`
- すべての関数・変数の prefix は `wamei/`。内部用は `--` を挟む
- docstring とコメントは日本語 (このリポジトリの既存コードに合わせる)
- **`error` を投げない**。失敗はメッセージか nil (tty で `debug-on-error t` だとデバッガに入って操作不能になる)
- 画像 ID は 1〜255 の巡回。プールは `dired-image-preview` / `tty-image-mode` と共有
- 行番号 (`display-line-numbers-mode`) を切るのは機能要件。桁を食われると placeholder の桁数が合わなくなる
- tty には fringe が無いので、行が `window-body-width` ちょうどでも最終桁が truncation glyph に取られる。桁の計算では必ず 1 桁引く
- init.el は symlink なので、`load` は `(file-name-directory (file-truename user-init-file))` を基準にする
- 対象は tty で起動した Emacs のみ
- git commit のメッセージは日本語。末尾に次の 2 行を含める:
  ```
  Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
  ```

## Phase 1 で完成している、このプランが使うインターフェース

```elisp
(wamei/kitty-graphics-available-p)                            ; tty かつ端末が対応なら非 nil
(wamei/kitty-graphics-cell-size)                              ; セルの (幅 . 高さ) ピクセル
(wamei/kitty-graphics-image-size FILE)                        ; (幅 . 高さ) ピクセル or nil
(wamei/kitty-graphics-cell-count IMAGE-PX CELL-PX MAX-CELLS)  ; (桁 . 行)
(wamei/kitty-graphics-put FILE COLS ROWS &optional SIZE)      ; 画像 ID or nil
(wamei/kitty-graphics-delete ID)                              ; ID が nil なら何もしない
(wamei/kitty-graphics-placeholder-line ID COLS ROW &optional COLOR)
(wamei/kitty-graphics-placeholder-string ID COLS ROWS)        ; 行は \n 区切り
(wamei/tty-image-mode)                                        ; Phase 1 の画像バッファ
```

## 組み込み image-dired の、このプランが使うもの

```elisp
(image-dired-thumb-name FILE)          ; サムネイルのキャッシュパス
(image-dired-original-file-name)       ; point の original-file-name プロパティ
(image-dired-associated-dired-buffer)  ; point の associated-dired-buffer プロパティ
(image-dired-list-tags FILE)
(image-dired-get-comment FILE)
(image-dired-dired-file-marked-p &optional MARKER)
image-dired-thumb-size                 ; サムネイルの最大ピクセル (既定 128)
image-dired-thumbnail-buffer           ; バッファ名
(image-dired-display-thumbs &optional ARG APPEND DO-NOT-POP)  ; 乗っ取る入口
image-dired-thumbnail-mode             ; 派生元
(image-dired--thumb-update-marks)      ; 差し替える
```

---

## File Structure

| ファイル | 責務 |
|---|---|
| `.emacs.d/tty-image-dired.el` (新規) | グリッドの幾何・組み立て・可視範囲の送受・モード・入口 |
| `.emacs.d/tty-image-dired-test.el` (新規) | batch テスト |
| `.emacs.d/init.el` (改) | leaf を 1 つ追加 |

**グリッドの座標は保持せず、添字から計算する。** 箱は桁・行とも固定なので、添字 `i` から次のように出せる。位置を持たないぶん、バッファを組み替えても壊れない。

```
段 (band) r = i / columns
段の中の桁 c = i mod columns
段の先頭行 = r * (箱の行数 + 1)          ; +1 はキャプション行
箱の桁の起点 = c * (箱の桁数 + 1)        ; +1 は箱の間の空き
キャプション行 = 段の先頭行 + 箱の行数
```

---

### Task 1: グリッドの幾何 (純粋関数)

**Files:**
- Create: `.emacs.d/tty-image-dired.el`
- Create: `.emacs.d/tty-image-dired-test.el`

**Interfaces:**
- Consumes: なし (純粋関数のみ)
- Produces:
  - `(wamei/tty-image-dired--box-size THUMB-PX CELL-PX)` → `(桁 . 行)`
  - `(wamei/tty-image-dired--columns WIDTH BOX-COLS)` → integer
  - `(wamei/tty-image-dired--move-index INDEX TOTAL COLUMNS DIRECTION)` → integer or nil
  - `(wamei/tty-image-dired--visible-range FIRST-LINE WINDOW-LINES BAND-LINES COLUMNS TOTAL)` → `(最初 . 最後)` or nil
  - defcustom `wamei/tty-image-dired-box-size` (既定 nil)

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/tty-image-dired-test.el` を新規作成:

```elisp
;;; tty-image-dired-test.el --- tests for tty-image-dired -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "kitty-graphics.el" dir) nil t)
  (load (expand-file-name "tty-image-mode.el" dir) nil t)
  (load (expand-file-name "tty-image-dired.el" dir) nil t))

;;; 箱の大きさ

(ert-deftest wamei/tty-image-dired-box-size-rounds-up ()
  "サムネイルの最大ピクセルを、セルの大きさで割り上げた桁・行。"
  ;; 128px / 幅 8px = 16 桁、128px / 高さ 19px = 6.7 → 7 行
  (should (equal (wamei/tty-image-dired--box-size 128 '(8 . 19)) '(16 . 7))))

(ert-deftest wamei/tty-image-dired-box-size-is-at-least-one ()
  "セルが画像より大きくても 1 桁 1 行は確保する。"
  (should (equal (wamei/tty-image-dired--box-size 4 '(8 . 19)) '(1 . 1))))

;;; 段あたりの枚数

(ert-deftest wamei/tty-image-dired-columns-reserves-a-column-for-the-truncation-glyph ()
  "tty には fringe が無いので最終桁が truncation glyph に取られる。その 1 桁を引く。
箱と箱の間は 1 桁空ける。"
  ;; (80 - 1) / (16 + 1) = 4.6 → 4 枚
  (should (= (wamei/tty-image-dired--columns 80 16) 4))
  ;; (35 - 1) / (16 + 1) = 2 → 2 枚
  (should (= (wamei/tty-image-dired--columns 35 16) 2)))

(ert-deftest wamei/tty-image-dired-columns-is-at-least-one ()
  "箱が入りきらない細い window でも 1 枚は並べる (0 だと段が作れない)。"
  (should (= (wamei/tty-image-dired--columns 10 16) 1))
  (should (= (wamei/tty-image-dired--columns 1 16) 1)))

;;; 移動

(ert-deftest wamei/tty-image-dired-move-index-forward-and-backward ()
  (should (= (wamei/tty-image-dired--move-index 0 5 3 'forward) 1))
  (should (= (wamei/tty-image-dired--move-index 4 5 3 'backward) 3)))

(ert-deftest wamei/tty-image-dired-move-index-stops-at-the-ends ()
  "端では巡回せず nil を返す。"
  (should-not (wamei/tty-image-dired--move-index 4 5 3 'forward))
  (should-not (wamei/tty-image-dired--move-index 0 5 3 'backward)))

(ert-deftest wamei/tty-image-dired-move-index-down-and-up ()
  "段をまたぐ移動は添字 ± 段あたりの枚数。"
  ;; 7 枚 3 列: 0 1 2 / 3 4 5 / 6
  (should (= (wamei/tty-image-dired--move-index 0 7 3 'down) 3))
  (should (= (wamei/tty-image-dired--move-index 3 7 3 'up) 0))
  ;; 5 + 3 = 8 は範囲外
  (should-not (wamei/tty-image-dired--move-index 5 7 3 'down))
  (should-not (wamei/tty-image-dired--move-index 1 7 3 'up)))

(ert-deftest wamei/tty-image-dired-move-index-line-beginning-and-end ()
  "段の端へ。最終段が半端でも溢れない。"
  ;; 7 枚 3 列: 0 1 2 / 3 4 5 / 6
  (should (= (wamei/tty-image-dired--move-index 4 7 3 'line-beginning) 3))
  (should (= (wamei/tty-image-dired--move-index 3 7 3 'line-end) 5))
  ;; 最終段は 6 の 1 枚だけ
  (should (= (wamei/tty-image-dired--move-index 6 7 3 'line-end) 6))
  (should (= (wamei/tty-image-dired--move-index 6 7 3 'line-beginning) 6)))

;;; 可視範囲

(ert-deftest wamei/tty-image-dired-visible-range-covers-the-shown-bands ()
  "window の先頭行と行数から、見えている段に載っている添字の範囲を出す。"
  ;; 段の高さ 8 行 (箱 7 行 + キャプション 1 行)、3 列、20 枚 (段は 0..6)
  ;; 先頭行 0、24 行見える → 段 0..2 (行 0..23) → 添字 0..8
  (should (equal (wamei/tty-image-dired--visible-range 0 24 8 3 20) '(0 . 8)))
  ;; 先頭行 8 (段 1 から)、16 行 → 段 1..2 → 添字 3..8
  (should (equal (wamei/tty-image-dired--visible-range 8 16 8 3 20) '(3 . 8))))

(ert-deftest wamei/tty-image-dired-visible-range-clamps-to-the-last-image ()
  "最終段が半端でも、存在しない添字を返さない。"
  ;; 7 枚 3 列 (段 0..2、最終段は 6 のみ)、先頭行 8、24 行 → 段 1..3 だが 7 枚しかない
  (should (equal (wamei/tty-image-dired--visible-range 8 24 8 3 7) '(3 . 6))))

(ert-deftest wamei/tty-image-dired-visible-range-nil-when-nothing-shown ()
  "1 枚も無ければ nil。"
  (should-not (wamei/tty-image-dired--visible-range 0 24 8 3 0)))

(provide 'tty-image-dired-test)
;;; tty-image-dired-test.el ends here
```

- [ ] **Step 2: 落ちることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL。`tty-image-dired.el` が無いので `load` の時点で "Cannot open load file"。

- [ ] **Step 3: 最小の実装を書く**

`.emacs.d/tty-image-dired.el` を新規作成:

```elisp
;;; tty-image-dired.el --- tty の Emacs で image-dired のサムネイルを出す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; 組み込みの `image-dired' には `display-images-p' のガードが無く、tty では
;; `insert-image' がそのまま呼ばれて空白が並ぶだけになる。ここでは kitty graphics
;; protocol の Unicode placeholder でサムネイルをグリッド表示する。
;;
;; `image-dired-thumbnail-mode' から派生させるのが要点。image-dired の多くの
;; コマンドは `(unless (derived-mode-p 'image-dired-thumbnail-mode) (user-error ...))'
;; のガードを持つので、派生していれば点ベースのコマンド (マーク、dired 連動) が
;; そのまま動く。走査するコマンド (移動、x、RET) だけを添字ベースで置き換える。
;;
;; グリッドの座標は保持せず、添字から計算する:
;;
;;   段 r = i / columns、段の中の桁 c = i mod columns
;;   段の先頭行   = r * (箱の行数 + 1)   ; +1 はキャプション行
;;   箱の桁の起点 = c * (箱の桁数 + 1)   ; +1 は箱の間の空き
;;
;;; Code:

(require 'cl-lib)
(require 'image-dired)
(require 'kitty-graphics)

(defgroup wamei/tty-image-dired nil
  "tty の image-dired サムネイル一覧。"
  :group 'image-dired)

(defcustom wamei/tty-image-dired-box-size nil
  "サムネイル 1 枚を置く箱の (桁 . 行)。nil なら
`image-dired-thumb-size' と端末のセルの大きさから決める。"
  :type '(choice (const :tag "自動" nil) (cons integer integer)))

;;;; 幾何

(defun wamei/tty-image-dired--box-size (thumb-px cell-px)
  "THUMB-PX (サムネイルの最大ピクセル) を CELL-PX (幅 . 高さ) で表す箱の (桁 . 行)。
割り上げる。最低 1 桁 1 行。"
  (cons (max 1 (ceiling thumb-px (car cell-px)))
        (max 1 (ceiling thumb-px (cdr cell-px)))))

(defun wamei/tty-image-dired--columns (width box-cols)
  "本文 WIDTH 桁に BOX-COLS 桁の箱を何枚並べられるか。最低 1 枚。
箱と箱の間は 1 桁空ける。tty には fringe が無いので、行が `window-body-width'
ちょうどでも最終桁が truncation glyph に取られる。その 1 桁を引く。"
  (max 1 (/ (1- width) (1+ box-cols))))

(defun wamei/tty-image-dired--move-index (index total columns direction)
  "INDEX から DIRECTION へ動いた添字。動けなければ nil (巡回しない)。
DIRECTION は forward / backward / down / up / line-beginning / line-end。
TOTAL は枚数、COLUMNS は段あたりの枚数。"
  (let ((next (pcase direction
                ('forward (1+ index))
                ('backward (1- index))
                ('down (+ index columns))
                ('up (- index columns))
                ('line-beginning (* columns (/ index columns)))
                ('line-end (min (1- total) (+ (* columns (/ index columns)) columns -1))))))
    (and next (>= next 0) (< next total) next)))

(defun wamei/tty-image-dired--visible-range (first-line window-lines band-lines columns total)
  "見えている添字の範囲 (最初 . 最後)。1 枚も無ければ nil。
FIRST-LINE は window の先頭がバッファの何行目か (0 起点)、WINDOW-LINES は
見えている行数、BAND-LINES は段の高さ (箱の行数 + キャプション 1 行)。"
  (when (> total 0)
    (let* ((first-band (/ first-line band-lines))
           (last-band (/ (+ first-line (max 0 (1- window-lines))) band-lines))
           (first-index (* first-band columns))
           (last-index (min (1- total) (+ (* last-band columns) columns -1))))
      (when (< first-index total)
        (cons first-index last-index)))))

(provide 'tty-image-dired)
;;; tty-image-dired.el ends here
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: 11 tests PASS。

- [ ] **Step 5: commit**

```bash
cd /Users/wamei/.dotfiles && git add .emacs.d/tty-image-dired.el .emacs.d/tty-image-dired-test.el && git commit -m "$(cat <<'EOM'
tty の image-dired グリッドの幾何を足す

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
EOM
)"
```

---

### Task 2: バッファの組み立て

**Files:**
- Modify: `.emacs.d/tty-image-dired.el`
- Modify: `.emacs.d/tty-image-dired-test.el`

**Interfaces:**
- Consumes: Task 1 の `--box-size` / `--columns`
- Produces:
  - buffer-local `wamei/tty-image-dired--files` (vector)、`--ids` (vector)、`--columns` (integer)、`--box` (`(桁 . 行)`)、`--selected` (integer)、`--dired-buffer` (buffer)
  - `(wamei/tty-image-dired--caption FILE WIDTH SELECTED MARKED)` → propertized string
  - `(wamei/tty-image-dired--band-lines)` → integer
  - `(wamei/tty-image-dired--box-line-region INDEX ROW)` → `(開始 . 終了)`
  - `(wamei/tty-image-dired--caption-region INDEX)` → `(開始 . 終了)`
  - `(wamei/tty-image-dired--goto-index INDEX)` → 副作用
  - `(wamei/tty-image-dired--build FILES DIRED-BUFFER COLUMNS BOX)` → 副作用 (現在のバッファに組み立てる)

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/tty-image-dired-test.el` の `(provide ...)` の直前に足す:

```elisp
;;; キャプション

(ert-deftest wamei/tty-image-dired-caption-truncates-to-the-box-width ()
  "ファイル名は箱の幅に切り詰める。"
  (let ((s (wamei/tty-image-dired--caption "/d/very-long-name.png" 8 nil nil)))
    (should (= (length s) 8))))

(ert-deftest wamei/tty-image-dired-caption-pads-to-the-box-width ()
  "短くても箱の幅ぶんの桁を占める (グリッドがずれないように)。"
  (let ((s (wamei/tty-image-dired--caption "/d/a.png" 12 nil nil)))
    (should (= (length s) 12))
    (should (string-prefix-p "a.png" s))))

(ert-deftest wamei/tty-image-dired-caption-marks-with-an-asterisk ()
  "dired でマークされていれば頭に * を付ける。"
  (let ((s (wamei/tty-image-dired--caption "/d/a.png" 12 nil t)))
    (should (string-prefix-p "*a.png" s))
    (should (= (length s) 12))))

(ert-deftest wamei/tty-image-dired-caption-highlights-when-selected ()
  "選択中は face を付ける。placeholder のセルには face を当てられないので、
選択の表示はキャプションが担う。"
  (let ((plain (wamei/tty-image-dired--caption "/d/a.png" 12 nil nil))
        (sel (wamei/tty-image-dired--caption "/d/a.png" 12 t nil)))
    (should-not (get-text-property 0 'face plain))
    (should (get-text-property 0 'face sel))))

;;; 組み立て

(defvar wamei/tty-image-dired-test--tags-db
  (make-temp-file "tty-image-dired-test-tags-")
  "テスト用のタグ DB。ユーザの `image-dired-tags-db-file' を読み書きしないため。")

(defun wamei/tty-image-dired-test--build (files columns box)
  "テスト用に FILES を COLUMNS 列・BOX の箱で組み立てたバッファを作る。"
  (let ((buf (generate-new-buffer " *tty-image-dired-test*"))
        (image-dired-tags-db-file wamei/tty-image-dired-test--tags-db))
    (with-current-buffer buf
      (wamei/tty-image-dired--build (vconcat files) nil columns box))
    buf))

(ert-deftest wamei/tty-image-dired-build-lays-out-bands ()
  "段は 箱の行数 + キャプション 1 行。5 枚 3 列なら 2 段。"
  (let ((buf (wamei/tty-image-dired-test--build
              '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png" "/d/e.png") 3 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (should (= wamei/tty-image-dired--columns 3))
          (should (equal wamei/tty-image-dired--box '(4 . 2)))
          (should (= (wamei/tty-image-dired--band-lines) 3))
          ;; 2 段 x 3 行 = 6 行
          (should (= (count-lines (point-min) (point-max)) 6)))
      (kill-buffer buf))))

(ert-deftest wamei/tty-image-dired-build-puts-image-dired-properties ()
  "点ベースの組み込みコマンドが動くよう、箱とキャプションの全体に
image-dired と同じテキストプロパティを載せる。"
  (let ((buf (wamei/tty-image-dired-test--build '("/d/a.png" "/d/b.png") 2 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (dolist (row '(0 1))
            (let ((region (wamei/tty-image-dired--box-line-region 0 row)))
              (should (get-text-property (car region) 'image-dired-thumbnail))
              (should (equal (get-text-property (car region) 'original-file-name) "/d/a.png"))))
          (let ((region (wamei/tty-image-dired--caption-region 0)))
            (should (equal (get-text-property (car region) 'original-file-name) "/d/a.png")))
          ;; 2 枚目は別のファイル
          (let ((region (wamei/tty-image-dired--box-line-region 1 0)))
            (should (equal (get-text-property (car region) 'original-file-name) "/d/b.png"))))
      (kill-buffer buf))))

(ert-deftest wamei/tty-image-dired-box-line-region-is-the-box-width ()
  "箱の 1 行ぶんの領域は、箱の桁数と同じ長さ。"
  (let ((buf (wamei/tty-image-dired-test--build '("/d/a.png" "/d/b.png") 2 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (let ((r0 (wamei/tty-image-dired--box-line-region 0 0))
                (r1 (wamei/tty-image-dired--box-line-region 1 0)))
            (should (= (- (cdr r0) (car r0)) 4))
            (should (= (- (cdr r1) (car r1)) 4))
            ;; 2 枚目は 1 枚目の右、間に 1 桁空く
            (should (= (car r1) (+ (cdr r0) 1)))))
      (kill-buffer buf))))

(ert-deftest wamei/tty-image-dired-goto-index-moves-point-into-the-thumbnail ()
  "移動先の point には original-file-name が載っている (組み込みコマンドの前提)。"
  (let ((buf (wamei/tty-image-dired-test--build '("/d/a.png" "/d/b.png" "/d/c.png") 2 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (wamei/tty-image-dired--goto-index 2)
          (should (equal (get-text-property (point) 'original-file-name) "/d/c.png"))
          (should (= wamei/tty-image-dired--selected 2)))
      (kill-buffer buf))))
```

- [ ] **Step 2: 落ちることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: 新しい 8 件が FAIL (`void-function wamei/tty-image-dired--caption` 等)。既存 11 件は PASS。

- [ ] **Step 3: 実装を足す**

`.emacs.d/tty-image-dired.el` の `(provide ...)` の直前に足す:

```elisp
;;;; バッファの状態

(defvar-local wamei/tty-image-dired--files nil
  "グリッド順に並べたファイルのベクタ。")

(defvar-local wamei/tty-image-dired--ids nil
  "各サムネイルの画像 ID のベクタ。端末に置いていなければその要素は nil。")

(defvar-local wamei/tty-image-dired--columns 1
  "段あたりの枚数。")

(defvar-local wamei/tty-image-dired--box '(1 . 1)
  "箱の (桁 . 行)。")

(defvar-local wamei/tty-image-dired--selected 0
  "選択中のサムネイルの添字。")

(defvar-local wamei/tty-image-dired--dired-buffer nil
  "元の dired バッファ。")

(defun wamei/tty-image-dired--band-lines ()
  "段の高さ (箱の行数 + キャプション 1 行)。"
  (1+ (cdr wamei/tty-image-dired--box)))

;;;; キャプション

(defun wamei/tty-image-dired--caption (file width selected marked)
  "FILE のキャプションを WIDTH 桁で返す。
SELECTED なら `highlight' face、MARKED なら頭に `*'。
placeholder のセルは前景色が画像 ID なので face を当てられない。選択と
マークの表示はこの行が担う。"
  (let* ((name (concat (if marked "*" "") (file-name-nondirectory file)))
         (text (if (> (length name) width)
                   (substring name 0 width)
                 (concat name (make-string (- width (length name)) ?\s)))))
    (when selected
      (add-face-text-property 0 (length text) 'highlight nil text))
    text))

;;;; 位置

(defun wamei/tty-image-dired--goto-line (line)
  "バッファの LINE 行目 (0 起点) の先頭へ。"
  (goto-char (point-min))
  (forward-line line))

(defun wamei/tty-image-dired--box-line-region (index row)
  "サムネイル INDEX の箱の ROW 行目の領域 (開始 . 終了)。"
  (let* ((columns wamei/tty-image-dired--columns)
         (box-cols (car wamei/tty-image-dired--box))
         (band (/ index columns))
         (col (mod index columns)))
    (save-excursion
      (wamei/tty-image-dired--goto-line (+ (* band (wamei/tty-image-dired--band-lines)) row))
      (move-to-column (* col (1+ box-cols)))
      (let ((start (point)))
        (move-to-column (+ (* col (1+ box-cols)) box-cols))
        (cons start (point))))))

(defun wamei/tty-image-dired--caption-region (index)
  "サムネイル INDEX のキャプション行の領域 (開始 . 終了)。"
  (wamei/tty-image-dired--box-line-region index (cdr wamei/tty-image-dired--box)))

(defun wamei/tty-image-dired--goto-index (index)
  "サムネイル INDEX の箱の左上へ point を移し、選択を更新する。"
  (let ((old wamei/tty-image-dired--selected))
    (setq wamei/tty-image-dired--selected index)
    (wamei/tty-image-dired--redraw-caption old)
    (wamei/tty-image-dired--redraw-caption index))
  (let ((region (wamei/tty-image-dired--box-line-region index 0)))
    (goto-char (car region))))

;;;; 組み立て

(defun wamei/tty-image-dired--marked-p (file)
  "FILE が元の dired バッファでマークされていれば非 nil。"
  (let ((buffer wamei/tty-image-dired--dired-buffer))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (save-excursion
             (and (dired-goto-file file)
                  (image-dired-dired-file-marked-p)))))))

(defun wamei/tty-image-dired--put-properties (index)
  "サムネイル INDEX の箱とキャプションに image-dired と同じプロパティを載せる。"
  (let* ((file (aref wamei/tty-image-dired--files index))
         (props (list 'image-dired-thumbnail t
                      ;; 組み込みは 1 サムネ = 1 文字を前提にしたキーマップを
                      ;; 無効にしている。こちらも同じにする。
                      'keymap nil
                      'original-file-name file
                      'associated-dired-buffer wamei/tty-image-dired--dired-buffer
                      'tags (image-dired-list-tags file)
                      'mouse-face 'highlight
                      'comment (image-dired-get-comment file))))
    (dotimes (row (1+ (cdr wamei/tty-image-dired--box)))
      (let ((region (wamei/tty-image-dired--box-line-region index row)))
        (add-text-properties (car region) (cdr region) props)))))

(defun wamei/tty-image-dired--redraw-caption (index)
  "サムネイル INDEX のキャプションを描き直す。"
  (when (and wamei/tty-image-dired--files
             (< index (length wamei/tty-image-dired--files)))
    (let* ((file (aref wamei/tty-image-dired--files index))
           (region (wamei/tty-image-dired--caption-region index))
           (inhibit-read-only t)
           (text (wamei/tty-image-dired--caption
                  file (car wamei/tty-image-dired--box)
                  (= index wamei/tty-image-dired--selected)
                  (wamei/tty-image-dired--marked-p file))))
      (save-excursion
        (delete-region (car region) (cdr region))
        (goto-char (car region))
        (insert text))
      (wamei/tty-image-dired--put-properties index))))

(defun wamei/tty-image-dired--build (files dired-buffer columns box)
  "現在のバッファに FILES のグリッドを組み立てる。
COLUMNS は段あたりの枚数、BOX は箱の (桁 . 行)。箱は空白のままにし、
画像は可視範囲だけ後から送る (画像 ID は 1〜255 しかないため)。"
  (setq wamei/tty-image-dired--files files
        wamei/tty-image-dired--ids (make-vector (length files) nil)
        wamei/tty-image-dired--columns columns
        wamei/tty-image-dired--box box
        wamei/tty-image-dired--selected 0
        wamei/tty-image-dired--dired-buffer dired-buffer)
  (let* ((inhibit-read-only t)
         (box-cols (car box))
         (box-rows (cdr box))
         (total (length files))
         (bands (ceiling total columns))
         ;; 段の 1 行ぶんの幅。箱の間の 1 桁を含める
         (line-width (* columns (1+ box-cols))))
    (erase-buffer)
    (dotimes (_band bands)
      (dotimes (_row box-rows)
        (insert (make-string line-width ?\s) "\n"))
      (insert (make-string line-width ?\s) "\n"))
    (dotimes (index total)
      (wamei/tty-image-dired--redraw-caption index))
    (goto-char (point-min))))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: 19 tests PASS。

- [ ] **Step 5: commit**

```bash
cd /Users/wamei/.dotfiles && git add .emacs.d/tty-image-dired.el .emacs.d/tty-image-dired-test.el && git commit -m "$(cat <<'EOM'
tty の image-dired グリッドを組み立てる

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
EOM
)"
```

---

### Task 3: 可視範囲の送受

**Files:**
- Modify: `.emacs.d/tty-image-dired.el`
- Modify: `.emacs.d/tty-image-dired-test.el`

**Interfaces:**
- Consumes: Task 1 の `--visible-range`、Task 2 のバッファ状態、Phase 1 の `kitty-graphics-*`
- Produces:
  - `(wamei/tty-image-dired--thumb-file FILE)` → サムネイルのパス or nil
  - `(wamei/tty-image-dired--show-box INDEX)` → 副作用
  - `(wamei/tty-image-dired--hide-box INDEX)` → 副作用
  - `(wamei/tty-image-dired--sync-visible)` → 副作用
  - `(wamei/tty-image-dired--release-all)` → 副作用

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/tty-image-dired-test.el` の `(provide ...)` の直前に足す:

```elisp
;;; サムネイルの実体

(ert-deftest wamei/tty-image-dired-thumb-file-reuses-a-fresh-cache ()
  "キャッシュが元ファイルより新しければ sips を起こさない。"
  (let ((dir (make-temp-file "tty-image-dired-test-" t)) (sips 0))
    (unwind-protect
        (let ((orig (expand-file-name "a.png" dir))
              (thumb (expand-file-name "thumb.png" dir)))
          (write-region "orig" nil orig)
          (write-region "thumb" nil thumb)
          (cl-letf (((symbol-function 'image-dired-thumb-name) (lambda (_f) thumb))
                    ((symbol-function 'wamei/tty-image-dired--make-thumb)
                     (lambda (&rest _) (setq sips (1+ sips)) t)))
            (should (equal (wamei/tty-image-dired--thumb-file orig) thumb))
            (should (= sips 0))))
      (delete-directory dir t))))

(ert-deftest wamei/tty-image-dired-thumb-file-creates-a-missing-cache ()
  "キャッシュが無ければ作る。作れなければ nil。"
  (let ((dir (make-temp-file "tty-image-dired-test-" t)))
    (unwind-protect
        (let ((orig (expand-file-name "a.png" dir))
              (thumb (expand-file-name "thumb.png" dir))
              (made nil))
          (write-region "orig" nil orig)
          (cl-letf (((symbol-function 'image-dired-thumb-name) (lambda (_f) thumb))
                    ((symbol-function 'wamei/tty-image-dired--make-thumb)
                     (lambda (_src dst) (setq made t) (write-region "t" nil dst) t)))
            (should (equal (wamei/tty-image-dired--thumb-file orig) thumb))
            (should made))
          (delete-file thumb)
          (cl-letf (((symbol-function 'image-dired-thumb-name) (lambda (_f) thumb))
                    ((symbol-function 'wamei/tty-image-dired--make-thumb)
                     (lambda (&rest _) nil)))
            (should-not (wamei/tty-image-dired--thumb-file orig))))
      (delete-directory dir t))))

;;; 送受

(defmacro wamei/tty-image-dired-test--with-grid (files columns box &rest body)
  "FILES を COLUMNS 列・BOX の箱で組み立てたバッファで BODY を評価する。"
  (declare (indent 3))
  `(let ((buf (wamei/tty-image-dired-test--build ,files ,columns ,box)))
     (unwind-protect (with-current-buffer buf ,@body)
       (kill-buffer buf))))

(ert-deftest wamei/tty-image-dired-show-box-writes-the-placeholder-and-keeps-properties ()
  "placeholder を書き込んでも、点ベースのコマンドが使うプロパティは残る。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
            ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
            ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
            ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) 7))
            ((symbol-function 'wamei/kitty-graphics-placeholder-line)
             (lambda (_id cols _row &optional _color) (make-string cols ?P))))
    (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
      (wamei/tty-image-dired--show-box 0)
      (should (= (aref wamei/tty-image-dired--ids 0) 7))
      (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
        (should (equal (buffer-substring-no-properties (car region) (cdr region)) "PPPP"))
        (should (equal (get-text-property (car region) 'original-file-name) "/d/a.png"))))))

(ert-deftest wamei/tty-image-dired-hide-box-releases-the-id-and-blanks-the-box ()
  (let ((deleted nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) 7))
              ((symbol-function 'wamei/kitty-graphics-delete) (lambda (id) (push id deleted)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-line)
               (lambda (_id cols _row &optional _color) (make-string cols ?P))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
        (wamei/tty-image-dired--show-box 0)
        (wamei/tty-image-dired--hide-box 0)
        (should (equal deleted '(7)))
        (should-not (aref wamei/tty-image-dired--ids 0))
        (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
          (should (equal (buffer-substring-no-properties (car region) (cdr region)) "    "))
          ;; プロパティは残す (点ベースのコマンドが動かなくなるため)
          (should (equal (get-text-property (car region) 'original-file-name) "/d/a.png")))))))

(ert-deftest wamei/tty-image-dired-sync-visible-sends-only-what-is-shown ()
  "可視範囲の外は送らない。範囲が変わらなければ送り直さない。"
  (let ((puts nil) (deleted nil) (range '(0 . 1)))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-put)
               (lambda (file &rest _) (push file puts) (length puts)))
              ((symbol-function 'wamei/kitty-graphics-delete) (lambda (id) (push id deleted)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-line)
               (lambda (_id cols _row &optional _color) (make-string cols ?P)))
              ((symbol-function 'wamei/tty-image-dired--window-visible-range)
               (lambda () range)))
      (wamei/tty-image-dired-test--with-grid
          '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png") 2 '(4 . 2)
        (wamei/tty-image-dired--sync-visible)
        (should (equal (nreverse (copy-sequence puts)) '("/d/a.png" "/d/b.png")))
        ;; 2 回目は範囲が同じなので何も起きない
        (wamei/tty-image-dired--sync-visible)
        (should (= (length puts) 2))
        (should-not deleted)
        ;; 範囲が動いたら、外れたものを解放して新しいものを送る
        (setq range '(2 . 3))
        (wamei/tty-image-dired--sync-visible)
        (should (= (length puts) 4))
        (should (= (length deleted) 2))))))

(ert-deftest wamei/tty-image-dired-release-all-frees-every-id ()
  (let ((deleted nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) 7))
              ((symbol-function 'wamei/kitty-graphics-delete) (lambda (id) (push id deleted)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-line)
               (lambda (_id cols _row &optional _color) (make-string cols ?P))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
        (wamei/tty-image-dired--show-box 0)
        (wamei/tty-image-dired--show-box 1)
        (wamei/tty-image-dired--release-all)
        (should (= (length deleted) 2))
        (should-not (aref wamei/tty-image-dired--ids 0))
        (should-not (aref wamei/tty-image-dired--ids 1))))))

(ert-deftest wamei/tty-image-dired-show-box-survives-a-failed-transfer ()
  "端末への転送に失敗しても、その箱が空白のままになるだけで error にしない。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
            ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
            ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
            ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) nil)))
    (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
      (wamei/tty-image-dired--show-box 0)
      (should-not (aref wamei/tty-image-dired--ids 0))
      (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
        (should (equal (buffer-substring-no-properties (car region) (cdr region)) "    "))))))

(ert-deftest wamei/tty-image-dired-show-box-survives-a-failed-thumbnail ()
  "1 枚のサムネイルが作れなくても、その箱が空白のままになるだけで error にしない。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (_f) nil)))
    (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
      (wamei/tty-image-dired--show-box 0)
      (should-not (aref wamei/tty-image-dired--ids 0))
      (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
        (should (equal (buffer-substring-no-properties (car region) (cdr region)) "    "))))))
```

- [ ] **Step 2: 落ちることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: 新しい 8 件が FAIL (`void-function wamei/tty-image-dired--thumb-file` 等)。既存 19 件は PASS。

- [ ] **Step 3: 実装を足す**

`.emacs.d/tty-image-dired.el` の `(provide ...)` の直前に足す:

```elisp
;;;; サムネイルの実体

(defun wamei/tty-image-dired--make-thumb (src dst)
  "SRC のサムネイルを DST に sips で作る。作れたら非 nil。
組み込みの `image-dired-create-thumb' は非同期なので使わない。ファイルが
できるまで箱を空白にして後から描き直す、という面倒を避ける。"
  (make-directory (file-name-directory dst) t)
  (and (zerop (call-process "sips" nil nil nil
                            "-s" "format" "png"
                            "-Z" (number-to-string image-dired-thumb-size)
                            (expand-file-name src) "--out" dst))
       (file-exists-p dst)
       (> (file-attribute-size (file-attributes dst)) 0)))

(defun wamei/tty-image-dired--thumb-file (file)
  "FILE のサムネイルのパス。無いか古ければ作る。作れなければ nil。
置き場所は `image-dired-thumb-name' が返すパスで、GUI の image-dired と共有する。"
  (let* ((thumb (image-dired-thumb-name file))
         (thumb-attr (file-attributes thumb)))
    (if (and thumb-attr
             (not (time-less-p (file-attribute-modification-time thumb-attr)
                               (file-attribute-modification-time (file-attributes file)))))
        thumb
      (and (wamei/tty-image-dired--make-thumb file thumb) thumb))))

;;;; 送受

(defun wamei/tty-image-dired--fill-box (index lines)
  "サムネイル INDEX の箱に LINES (行ごとの文字列のリスト) を書き込む。
書き換えたあとプロパティを載せ直す (消えると点ベースのコマンドが動かなくなる)。"
  (let ((inhibit-read-only t))
    (save-excursion
      (cl-loop for row from 0 below (cdr wamei/tty-image-dired--box)
               for line in lines
               do (let ((region (wamei/tty-image-dired--box-line-region index row)))
                    (delete-region (car region) (cdr region))
                    (goto-char (car region))
                    (insert line))))
    (wamei/tty-image-dired--put-properties index)))

(defun wamei/tty-image-dired--show-box (index)
  "サムネイル INDEX を端末へ送り、箱に placeholder を書き込む。
サムネイルが作れない / 送れないときは箱を空白のままにする。`error' は投げない。"
  (unless (aref wamei/tty-image-dired--ids index)
    (let* ((file (aref wamei/tty-image-dired--files index))
           (thumb (wamei/tty-image-dired--thumb-file file))
           (px (and thumb (wamei/kitty-graphics-image-size thumb))))
      (when px
        (let* ((cells (wamei/kitty-graphics-cell-count
                       px (wamei/kitty-graphics-cell-size) wamei/tty-image-dired--box))
               (id (wamei/kitty-graphics-put thumb (car cells) (cdr cells) px)))
          (when id
            (aset wamei/tty-image-dired--ids index id)
            (wamei/tty-image-dired--fill-box
             index
             (cl-loop for row from 0 below (cdr wamei/tty-image-dired--box)
                      collect (let ((line (if (< row (cdr cells))
                                              (wamei/kitty-graphics-placeholder-line
                                               id (car cells) row)
                                            "")))
                                (concat line
                                        (make-string (- (car wamei/tty-image-dired--box)
                                                        (length line))
                                                     ?\s)))))))))))

(defun wamei/tty-image-dired--hide-box (index)
  "サムネイル INDEX を端末から解放し、箱を空白に戻す。"
  (when-let* ((id (aref wamei/tty-image-dired--ids index)))
    (wamei/kitty-graphics-delete id)
    (aset wamei/tty-image-dired--ids index nil)
    (wamei/tty-image-dired--fill-box
     index
     (make-list (cdr wamei/tty-image-dired--box)
                (make-string (car wamei/tty-image-dired--box) ?\s)))))

(defun wamei/tty-image-dired--window-visible-range ()
  "この window で見えているサムネイルの添字の範囲 (最初 . 最後)。無ければ nil。"
  (when-let* ((window (get-buffer-window (current-buffer))))
    (wamei/tty-image-dired--visible-range
     (save-excursion (goto-char (window-start window))
                     (count-lines (point-min) (line-beginning-position)))
     (window-body-height window)
     (wamei/tty-image-dired--band-lines)
     wamei/tty-image-dired--columns
     (length wamei/tty-image-dired--files))))

(defvar-local wamei/tty-image-dired--shown-range nil
  "最後に送った可視範囲 (最初 . 最後)。")

(defun wamei/tty-image-dired--sync-visible ()
  "見えているサムネイルだけが端末に置かれている状態にする。
範囲が前と同じなら何もしない。画像 ID は 1〜255 しかないので、画面外のぶんは解放する。"
  (let ((range (wamei/tty-image-dired--window-visible-range)))
    (unless (equal range wamei/tty-image-dired--shown-range)
      (setq wamei/tty-image-dired--shown-range range)
      (dotimes (index (length wamei/tty-image-dired--files))
        (if (and range (>= index (car range)) (<= index (cdr range)))
            (wamei/tty-image-dired--show-box index)
          (wamei/tty-image-dired--hide-box index))))))

(defun wamei/tty-image-dired--release-all ()
  "端末に置いたサムネイルをすべて解放する。"
  (when wamei/tty-image-dired--ids
    (dotimes (index (length wamei/tty-image-dired--ids))
      (wamei/tty-image-dired--hide-box index)))
  (setq wamei/tty-image-dired--shown-range nil))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: 27 tests PASS。

- [ ] **Step 5: commit**

```bash
cd /Users/wamei/.dotfiles && git add .emacs.d/tty-image-dired.el .emacs.d/tty-image-dired-test.el && git commit -m "$(cat <<'EOM'
tty の image-dired で可視範囲だけ端末へ送る

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
EOM
)"
```

---

### Task 4: モードと移動・上書きコマンド

**Files:**
- Modify: `.emacs.d/tty-image-dired.el`
- Modify: `.emacs.d/tty-image-dired-test.el`

**Interfaces:**
- Consumes: Task 1〜3 のすべて
- Produces:
  - `wamei/tty-image-dired-mode` (major mode、`image-dired-thumbnail-mode` から派生)、`wamei/tty-image-dired-mode-map`
  - `(wamei/tty-image-dired-forward-image &optional N)` / `-backward-image` / `-next-line` / `-previous-line` / `-move-beginning-of-line` / `-move-end-of-line`
  - `(wamei/tty-image-dired-display-this)` — `RET`
  - `(wamei/tty-image-dired-do-flagged-delete)` — `x`
  - `(wamei/tty-image-dired--update-marks)` — `image-dired--thumb-update-marks` の差し替え先

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/tty-image-dired-test.el` の `(provide ...)` の直前に足す:

```elisp
;;; モード

(ert-deftest wamei/tty-image-dired-mode-derives-from-image-dired-thumbnail-mode ()
  "組み込みのコマンドは derived-mode-p のガードを持つので、派生していないと使えない。"
  (with-temp-buffer
    (wamei/tty-image-dired-mode)
    (should (derived-mode-p 'image-dired-thumbnail-mode))))

(ert-deftest wamei/tty-image-dired-mode-turns-off-line-numbers ()
  "行番号が桁を食うと placeholder の桁数が合わなくなる。
`global-display-line-numbers-mode' が有効だと major mode 変更後に t になるので、
それを打ち消せているかを見る。"
  (global-display-line-numbers-mode 1)
  (unwind-protect
      (with-temp-buffer
        (wamei/tty-image-dired-mode)
        (should-not display-line-numbers-mode)
        (should truncate-lines))
    (global-display-line-numbers-mode -1)))

(ert-deftest wamei/tty-image-dired-mode-hooks-window-changes-and-cleanup ()
  (with-temp-buffer
    (wamei/tty-image-dired-mode)
    (should (memq #'wamei/tty-image-dired--sync-visible
                  (buffer-local-value 'window-configuration-change-hook (current-buffer))))
    (should (memq #'wamei/tty-image-dired--scroll-sync
                  (buffer-local-value 'window-scroll-functions (current-buffer))))
    (should (memq #'wamei/tty-image-dired--release-all
                  (buffer-local-value 'kill-buffer-hook (current-buffer))))))

;;; 移動コマンド

(ert-deftest wamei/tty-image-dired-forward-image-moves-the-selection ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 0)
      (wamei/tty-image-dired-forward-image)
      (should (= wamei/tty-image-dired--selected 1))
      (should (equal (get-text-property (point) 'original-file-name) "/d/b.png")))))

(ert-deftest wamei/tty-image-dired-forward-image-messages-at-the-end ()
  "端では動かず、error も投げない。"
  (let ((messages nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
        (wamei/tty-image-dired--goto-index 1)
        (wamei/tty-image-dired-forward-image)
        (should (= wamei/tty-image-dired--selected 1))
        (should messages)))))

(ert-deftest wamei/tty-image-dired-next-line-moves-a-band-down ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 0)
      (wamei/tty-image-dired-next-line)
      (should (= wamei/tty-image-dired--selected 2))
      (wamei/tty-image-dired-previous-line)
      (should (= wamei/tty-image-dired--selected 0)))))

(ert-deftest wamei/tty-image-dired-move-to-line-ends ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 2)
      (wamei/tty-image-dired-move-end-of-line)
      (should (= wamei/tty-image-dired--selected 3))
      (wamei/tty-image-dired-move-beginning-of-line)
      (should (= wamei/tty-image-dired--selected 2)))))

;;; RET

(ert-deftest wamei/tty-image-dired-display-this-opens-the-original-file ()
  "組み込みの image-dired-image-mode ではなく find-file で開き、
auto-mode-alist → image-mode → Phase 1 の advice の経路に載せる。"
  (let ((opened nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
              ((symbol-function 'find-file) (lambda (f) (setq opened f))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
        (wamei/tty-image-dired--goto-index 1)
        (wamei/tty-image-dired-display-this)
        (should (equal opened "/d/b.png"))))))
```

- [ ] **Step 2: 落ちることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: 新しい 8 件が FAIL。既存 27 件は PASS。

- [ ] **Step 3: 実装を足す**

`.emacs.d/tty-image-dired.el` の `(provide ...)` の直前に足す:

```elisp
;;;; 移動

(defun wamei/tty-image-dired--move (direction)
  "DIRECTION へ選択を動かす。端なら動かさずメッセージを出す。"
  (let ((next (wamei/tty-image-dired--move-index
               wamei/tty-image-dired--selected
               (length wamei/tty-image-dired--files)
               wamei/tty-image-dired--columns
               direction)))
    (if next
        (progn (wamei/tty-image-dired--goto-index next)
               (wamei/tty-image-dired--sync-visible))
      (message "これ以上サムネイルがありません"))))

(defun wamei/tty-image-dired-forward-image (&optional _n)
  "次のサムネイルへ。"
  (interactive "p" wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'forward))

(defun wamei/tty-image-dired-backward-image (&optional _n)
  "前のサムネイルへ。"
  (interactive "p" wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'backward))

(defun wamei/tty-image-dired-next-line ()
  "1 段下のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'down))

(defun wamei/tty-image-dired-previous-line ()
  "1 段上のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'up))

(defun wamei/tty-image-dired-move-beginning-of-line ()
  "段の先頭のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'line-beginning))

(defun wamei/tty-image-dired-move-end-of-line ()
  "段の末尾のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'line-end))

(defun wamei/tty-image-dired--scroll-sync (_window _start)
  "スクロールしたときに可視範囲を送り直す。`window-scroll-functions' 用。
匿名関数にすると `remove-hook' できず、モードに入り直すたびに溜まる。"
  (wamei/tty-image-dired--sync-visible))

;;;; 上書きするコマンド

(defun wamei/tty-image-dired-display-this ()
  "選択中の画像を開く。
組み込みの `image-dired-display-this' は `image-dired-image-mode'
\(`image-mode' 派生) を起こすが、それは Phase 1 の `image-mode' への advice と
噛み合わない。`find-file' で auto-mode-alist → image-mode → advice の経路に載せる。"
  (interactive nil wamei/tty-image-dired-mode)
  (let ((file (aref wamei/tty-image-dired--files wamei/tty-image-dired--selected)))
    (find-file file)))

(defun wamei/tty-image-dired-do-flagged-delete ()
  "dired 側で削除フラグの付いたファイルを消し、一覧を組み直す。
組み込みの `image-dired-do-flagged-delete' は 1 サムネ = 1 文字を前提に
バッファを走査するので使えない。"
  (interactive nil wamei/tty-image-dired-mode)
  (let ((dired-buffer wamei/tty-image-dired--dired-buffer))
    (when (buffer-live-p dired-buffer)
      (with-current-buffer dired-buffer (dired-do-flagged-delete))
      (wamei/tty-image-dired--release-all)
      (let ((files (cl-remove-if-not #'file-exists-p wamei/tty-image-dired--files)))
        (wamei/tty-image-dired--build files dired-buffer
                                      wamei/tty-image-dired--columns
                                      wamei/tty-image-dired--box))
      (wamei/tty-image-dired--goto-index 0)
      (wamei/tty-image-dired--sync-visible))))

(defun wamei/tty-image-dired--update-marks ()
  "マークの表示を更新する。`image-dired--thumb-update-marks' の差し替え先。
組み込みはサムネイルの枠の見た目で表すが、placeholder のセルには face を
当てられないのでキャプションで表す。"
  (when (derived-mode-p 'wamei/tty-image-dired-mode)
    (dotimes (index (length wamei/tty-image-dired--files))
      (wamei/tty-image-dired--redraw-caption index))))

;;;; モード

(defvar-keymap wamei/tty-image-dired-mode-map
  :doc "`wamei/tty-image-dired-mode' のキーマップ。
組み込みの移動コマンドは 1 文字ずつ走査するので矩形の中で止まる。差し替える。"
  :parent image-dired-thumbnail-mode-map
  "f" #'wamei/tty-image-dired-forward-image
  "b" #'wamei/tty-image-dired-backward-image
  "n" #'wamei/tty-image-dired-next-line
  "p" #'wamei/tty-image-dired-previous-line
  "a" #'wamei/tty-image-dired-move-beginning-of-line
  "e" #'wamei/tty-image-dired-move-end-of-line
  "x" #'wamei/tty-image-dired-do-flagged-delete
  "RET" #'wamei/tty-image-dired-display-this)

(define-derived-mode wamei/tty-image-dired-mode image-dired-thumbnail-mode "TtyImageDired"
  "tty の Emacs で image-dired のサムネイルをグリッド表示するモード。
`image-dired-thumbnail-mode' から派生させることで、点ベースの組み込みコマンド
\(マーク、dired 連動) をそのまま使う。"
  (setq-local truncate-lines t
              cursor-type nil)
  ;; 行番号は見た目の問題ではない。桁を食われると placeholder の桁数が合わなくなる。
  (display-line-numbers-mode 0)
  (add-hook 'window-configuration-change-hook #'wamei/tty-image-dired--sync-visible nil t)
  (add-hook 'window-scroll-functions #'wamei/tty-image-dired--scroll-sync nil t)
  (add-hook 'kill-buffer-hook #'wamei/tty-image-dired--release-all nil t))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: 35 tests PASS。

- [ ] **Step 5: commit**

```bash
cd /Users/wamei/.dotfiles && git add .emacs.d/tty-image-dired.el .emacs.d/tty-image-dired-test.el && git commit -m "$(cat <<'EOM'
tty の image-dired にモードと移動を足す

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
EOM
)"
```

---

### Task 5: 入口の advice と init.el の配線

**Files:**
- Modify: `.emacs.d/tty-image-dired.el`
- Modify: `.emacs.d/tty-image-dired-test.el`
- Modify: `.emacs.d/init.el`

**Interfaces:**
- Consumes: Task 4 の `wamei/tty-image-dired-mode`、Task 2 の `--build`、Phase 1 の `wamei/kitty-graphics-available-p`
- Produces:
  - `(wamei/tty-image-dired--display-thumbs-around ORIG &rest ARGS)`
  - `(wamei/tty-image-dired-setup)`

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/tty-image-dired-test.el` の `(provide ...)` の直前に足す:

```elisp
;;; 入口

(ert-deftest wamei/tty-image-dired-around-builds-the-grid-when-available ()
  "端末が対応していれば自分のグリッドを組む。"
  (let ((built nil) (orig-called nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () t))
              ((symbol-function 'wamei/tty-image-dired--show-thumbs)
               (lambda (&rest _) (setq built t))))
      (wamei/tty-image-dired--display-thumbs-around
       (lambda (&rest _) (setq orig-called t)))
      (should built)
      (should-not orig-called))))

(ert-deftest wamei/tty-image-dired-around-falls-back-to-the-original ()
  "非対応端末では error を投げず、元の実装をそのまま呼ぶ (今日と同じ挙動)。"
  (let ((orig-args nil) (messages nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () nil))
              ((symbol-function 'wamei/tty-image-dired--show-thumbs)
               (lambda (&rest _) (error "呼ばれてはいけない")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired--display-thumbs-around
       (lambda (&rest args) (setq orig-args args)) 'arg 'append 'do-not-pop)
      (should (equal orig-args '(arg append do-not-pop)))
      (should messages))))

(ert-deftest wamei/tty-image-dired-show-thumbs-messages-when-there-are-no-images ()
  "画像が 1 枚も無ければメッセージだけ。バッファは作らず error も投げない。"
  (let ((messages nil) (built nil))
    (cl-letf (((symbol-function 'dired-get-marked-files) (lambda (&rest _) nil))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/tty-image-dired--build)
               (lambda (&rest _) (setq built t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (wamei/tty-image-dired--show-thumbs))
      (should messages)
      (should-not built))))

(ert-deftest wamei/tty-image-dired-setup-adds-the-advice ()
  (unwind-protect
      (progn
        (wamei/tty-image-dired-setup)
        (should (advice-member-p #'wamei/tty-image-dired--display-thumbs-around
                                 'image-dired-display-thumbs))
        (should (advice-member-p #'wamei/tty-image-dired--update-marks
                                 'image-dired--thumb-update-marks)))
    (advice-remove 'image-dired-display-thumbs
                   #'wamei/tty-image-dired--display-thumbs-around)
    (advice-remove 'image-dired--thumb-update-marks
                   #'wamei/tty-image-dired--update-marks)))
```

- [ ] **Step 2: 落ちることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: 新しい 4 件が FAIL。既存 35 件は PASS。

- [ ] **Step 3: 実装を足す**

`.emacs.d/tty-image-dired.el` の `(provide ...)` の直前に足す:

```elisp
;;;; 入口

(defun wamei/tty-image-dired--show-thumbs (&optional arg _append _do-not-pop)
  "dired でマークされているファイルのサムネイルをグリッド表示する。
ARG があれば point のファイル 1 枚だけ。"
  (let* ((dired-buffer (current-buffer))
         (files (vconcat (dired-get-marked-files nil (and arg 1))))
         (cell (wamei/kitty-graphics-cell-size))
         (box (or wamei/tty-image-dired-box-size
                  (wamei/tty-image-dired--box-size image-dired-thumb-size cell)))
         (buffer (get-buffer-create image-dired-thumbnail-buffer)))
    (if (zerop (length files))
        (message "画像ファイルがありません")
      (with-current-buffer buffer
        (unless (derived-mode-p 'wamei/tty-image-dired-mode)
          (wamei/tty-image-dired-mode))
        (wamei/tty-image-dired--release-all)
        (wamei/tty-image-dired--build
         files dired-buffer
         (wamei/tty-image-dired--columns
          (window-body-width (or (get-buffer-window buffer) (selected-window)))
          (car box))
         box)
        (wamei/tty-image-dired--goto-index 0))
      (display-buffer buffer)
      (with-current-buffer buffer (wamei/tty-image-dired--sync-visible)))))

(defun wamei/tty-image-dired--display-thumbs-around (orig &rest args)
  "tty で `image-dired-display-thumbs' の代わりに呼ばれる。
端末が kitty graphics に対応していれば自分のグリッド、していなければ
元の実装をそのまま呼ぶ (空白が並ぶだけで害はない)。`error' は投げない。"
  (if (wamei/kitty-graphics-available-p)
      (apply #'wamei/tty-image-dired--show-thumbs args)
    (message "この端末は kitty graphics に対応していないのでサムネイルは出ません")
    (apply orig args)))

(defun wamei/tty-image-dired-setup ()
  "tty で image-dired のサムネイルがグリッド表示されるようにする。
`M-x image-dired' も dired からの呼び出しも `image-dired-display-thumbs' を通る。"
  (advice-add 'image-dired-display-thumbs :around
              #'wamei/tty-image-dired--display-thumbs-around)
  (advice-add 'image-dired--thumb-update-marks :after
              #'wamei/tty-image-dired--update-marks))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
```

Expected: 39 tests PASS。

- [ ] **Step 5: init.el に leaf を足す**

`.emacs.d/init.el` の `(leaf tty-image-mode ...)` の**直後**に挿入する:

```elisp
(leaf tty-image-dired
  :doc "tty では kitty graphics protocol で image-dired のサムネイルを出す"
  :ensure nil
  ;; 組み込みの image-dired は tty でも error にならず空白が並ぶだけなので、
  ;; tty のときだけ入口を横取りする。
  :if (not (display-graphic-p))
  :preface
  (load (expand-file-name "tty-image-dired"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :config
  (wamei/tty-image-dired-setup))
```

- [ ] **Step 6: init.el の diff が自分のハンクだけか確認**

```bash
cd /Users/wamei/.dotfiles && git diff .emacs.d/init.el
```

Expected: leaf 1 つの追加だけ。他セッションのハンクが混ざっていたらコミットせず報告する。

- [ ] **Step 7: 4 スイートを回す**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && for t in kitty-graphics tty-image-mode tty-image-dired dired-image-preview dired-image-preview-kitty; do
  printf "%-28s" "$t"
  emacs -Q --batch -l "$t-test.el" -f ert-run-tests-batch-and-exit 2>&1 | grep -E 'Ran [0-9]+ tests|FAILED'
done
```

Expected: 5 本とも PASS。

- [ ] **Step 8: commit**

```bash
cd /Users/wamei/.dotfiles && git add .emacs.d/tty-image-dired.el .emacs.d/tty-image-dired-test.el .emacs.d/init.el && git commit -m "$(cat <<'EOM'
tty では image-dired のサムネイルを kitty graphics で出す

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016z2EhqguW3GKLw5nzP2N2G
EOM
)"
```

---

### Task 6: 実端末での受け入れ確認

**Files:**
- Create: scratchpad の使い捨てスクリプト (commit しない)

**Interfaces:**
- Consumes: Task 1〜5 のすべて
- Produces: 受け入れ条件 1〜6 の判定

**注意 (過去の事故と、Phase 1 で分かったこと):**

- **`script(1)` を絶対に使わない。** 以前この検証で `script` の引数編集ミスにより
  `/Applications/Emacs.app/Contents/MacOS/Emacs` を上書きした。出力の取得は
  `tmux capture-pane` か Emacs 側の `write-region` だけを使う
- **生成したスクリプトは実行前に必ず `cat` して内容を report に貼る**
- **detached tmux では検証にならない。** kitty graphics の対応判定は端末に `a=q` を送って
  応答を読む方式なので、実端末に繋がっていない pty では応答が来ず、フォールバック経路が
  走ってしまう。`open -na Ghostty --args -e <script>` で実 Ghostty ウィンドウを開く
- **素の `emacs -nw` では変更が読まれない。** `~/.emacs.d/init.el` は main checkout への
  symlink。`emacs -Q -nw` に module を明示 load させる
- **合成の判定に `capture-pane` の文字数は使えない。** kitty の placeholder は
  capture-pane の出力上で合成の有無を区別できない。Emacs 側で `find-composition` を使う

- [ ] **Step 1: 確認用の画像を 30 枚用意する**

```bash
SCRATCH=/private/tmp/claude-501/-Users-wamei--dotfiles/5a54e24c-3fb0-4210-9d83-80bd7d6bbc5b/scratchpad
mkdir -p "$SCRATCH/grid"
emacs -Q --batch --eval "(with-temp-file \"$SCRATCH/grid/base.png\"
  (set-buffer-multibyte nil)
  (insert (base64-decode-string \"iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\")))"
for i in $(seq -w 1 30); do
  sips -s format png --resampleHeightWidth 200 400 "$SCRATCH/grid/base.png" --out "$SCRATCH/grid/img$i.png" >/dev/null
done
rm -f "$SCRATCH/grid/base.png"
ls "$SCRATCH/grid" | wc -l
```

Expected: 30。

- [ ] **Step 2: Ghostty で起動して一覧を出す**

次の 2 つを scratchpad に作り、**実行前に両方 `cat` して内容を report に貼る**
(過去に生成スクリプトの引数ミスで Emacs.app を壊した事故があるため)。

`$SCRATCH/grid-probe.el`:

```elisp
(setq inhibit-startup-screen t)
(let ((d "/Users/wamei/.dotfiles/.emacs.d/"))
  (load (concat d "kitty-graphics.el") nil t)
  (load (concat d "tty-image-mode.el") nil t)
  (load (concat d "tty-image-dired.el") nil t))
(wamei/tty-image-setup)
(wamei/tty-image-dired-setup)

(defun probe-write (tag)
  "今の状態を scratchpad の probe-TAG.txt に書く。"
  (let ((out (format "/private/tmp/claude-501/-Users-wamei--dotfiles/5a54e24c-3fb0-4210-9d83-80bd7d6bbc5b/scratchpad/probe-%s.txt" tag))
        (info (with-current-buffer (get-buffer image-dired-thumbnail-buffer)
                (list 'mode major-mode
                      'columns wamei/tty-image-dired--columns
                      'box wamei/tty-image-dired--box
                      'total (length wamei/tty-image-dired--files)
                      'live (length (delq nil (append wamei/tty-image-dired--ids nil)))
                      'range wamei/tty-image-dired--shown-range
                      'selected wamei/tty-image-dired--selected
                      'pool (length wamei/kitty-graphics--live-ids)
                      'composed (let ((n 0))
                                  (save-excursion
                                    (goto-char (point-min))
                                    (while (not (eobp))
                                      (when (find-composition (point)) (setq n (1+ n)))
                                      (forward-char 1)))
                                  n)))))
    (write-region (format "%S\n" info) nil out)))

(run-with-timer
 2 nil
 (lambda ()
   (image-dired "/private/tmp/claude-501/-Users-wamei--dotfiles/5a54e24c-3fb0-4210-9d83-80bd7d6bbc5b/scratchpad/grid")
   (run-with-timer 3 nil (lambda () (probe-write "initial")))))
```

`$SCRATCH/grid-run.sh`:

```bash
#!/bin/zsh
tmux -f ~/.dotfiles/.tmux.conf new-session -s ttyimagedired -x 120 -y 40   "emacs -Q -nw -l /private/tmp/claude-501/-Users-wamei--dotfiles/5a54e24c-3fb0-4210-9d83-80bd7d6bbc5b/scratchpad/grid-probe.el"
```

`cat` して確認したら実行する:

```bash
chmod +x "$SCRATCH/grid-run.sh"
open -na Ghostty --args -e "$SCRATCH/grid-run.sh"
sleep 12
cat "$SCRATCH/probe-initial.txt"
```

- [ ] **Step 3: グリッドが出ていることを確認 (受け入れ条件 1)**

Step 2 で出力した `probe-initial.txt` を読む。

Expected: `mode` が `wamei/tty-image-dired-mode`、`columns` が 1 より大きい、
`total` が 30、`composed` が 0 より大きい (placeholder が 1 セル = 1 グリフに合成されている)。
`composed` が 0 なら `compose-string` が効いておらず、画像が縦縞になっている。

- [ ] **Step 4: 可視ぶんだけ端末に置かれていることを確認 (受け入れ条件 3)**

`probe-initial.txt` の `live` と `pool` を見る。

Expected: `total` が 30 なのに `live` は可視ぶん (40x120 の画面で 20 前後) で頭打ちになり、
`pool` (`wamei/kitty-graphics--live-ids` の長さ) が 255 に達していない。
`live` が 30 なら可視範囲の絞り込みが効いていない。

- [ ] **Step 5: スクロールで送受が入れ替わることを確認 (受け入れ条件 3)**

```bash
tmux send-keys -t ttyimagedired 'M-:' '(progn (scroll-up-command) (probe-write "scrolled"))' Enter
sleep 3
cat "$SCRATCH/probe-scrolled.txt"
```

Expected: `range` が `probe-initial.txt` と変わっている。`live` はほぼ同じ数のまま
(増え続けていない)。増えていれば画面外のぶんを解放できていない。

- [ ] **Step 6: 移動とマークを確認 (受け入れ条件 2、4)**

```bash
tmux send-keys -t ttyimagedired 'f' 'f' 'n' 'm'
sleep 2
tmux send-keys -t ttyimagedired 'M-:' '(probe-write "moved")' Enter
sleep 2
cat "$SCRATCH/probe-moved.txt"
tmux capture-pane -p -t ttyimagedired | grep -n '\*img' | head -3
```

Expected: `selected` が `2 + columns` になっている (`f` 2 回 + `n` 1 回)。
capture-pane にマーク付きのキャプション (`*img...`) が 1 行見える。

- [ ] **Step 7: RET で Phase 1 のモードに入ることを確認 (受け入れ条件 5)**

```bash
tmux send-keys -t ttyimagedired 'Enter'
sleep 3
tmux send-keys -t ttyimagedired 'M-:' '(message "mode=%s file=%s" major-mode (file-name-nondirectory (or buffer-file-name "")))' Enter
sleep 2
tmux capture-pane -p -t ttyimagedired | tail -3
```

Expected: `mode=wamei/tty-image-mode` と、選択していた画像のファイル名。

- [ ] **Step 8: 後始末**

```bash
tmux kill-session -t ttyimagedired
pgrep -af 'MacOS/Emacs|bin/emacs' | grep -v grep
```

Ghostty ウィンドウが閉じていること、検証用の Emacs が残っていないことを確認する
(ユーザの GUI Emacs は残っていてよい)。

- [ ] **Step 9: GUI 側 (受け入れ条件 6)**

GUI は目視できないので、`emacs -Q --batch` で
`(leaf tty-image-dired ...)` の `:if (not (display-graphic-p))` により GUI では
advice が掛からないことだけを確認し、**目視は未実施**と正直に報告する。

- [ ] **Step 10: 全スイートを回して commit するものが無いことを確認**

```bash
cd /Users/wamei/.dotfiles/.emacs.d && for t in kitty-graphics tty-image-mode tty-image-dired dired-image-preview dired-image-preview-kitty; do
  printf "%-28s" "$t"
  emacs -Q --batch -l "$t-test.el" -f ert-run-tests-batch-and-exit 2>&1 | grep -E 'Ran [0-9]+ tests|FAILED'
done
cd /Users/wamei/.dotfiles && git status --short
```

Expected: 5 本とも PASS、`git status` に自分の変更が残っていない。
