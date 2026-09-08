;;; sql-connections.el --- client 設定ファイルから SQL 接続先を拾う -*- lexical-binding: t; -*-

;;; Commentary:
;; 接続先を Emacs 用に書き直さず、mysql / psql が元々読む設定ファイルだけを情報源にする。
;;
;;   ~/.my.cnf          [client<名前>] グループ         → mysql --defaults-group-suffix=<名前>
;;   ~/.pg_service.conf [<名前>] セクション             → psql "service=<名前>"
;;
;; これに加えて、カレント project の .wrangler にある wrangler dev のローカル D1
;; (実体は SQLite ファイル) も接続先として拾う。こちらは設定ファイルではなく
;; ディスク上のファイルそのものが情報源。
;;
;; どちらも [client] や共通キーを継承する仕組みを持っているので、ホスト・ユーザ・
;; パスワードはそちらに 1 度書けばよい。Emacs は名前だけを渡し、認証情報は
;; コマンドラインに出さない (ps で見えてしまうため)。
;;
;; 用途は 2 つ:
;;   - `sql-connections-refresh' で `sql-connection-alist' を作る (C-u M-x sql-connect の選択肢)
;;   - `sql-connections-sqls' で言語サーバ sqls の :connections を作る
;;     (sqls は自分では my.cnf / pg_service.conf を読めないので、ここで値を展開して渡す)

;;; Code:

(require 'sql)
(require 'subr-x)
(require 'project)

(defvar sql-connections-my-cnf "~/.my.cnf"
  "MySQL クライアントの設定ファイル。[client<名前>] を接続先として拾う。")

(defvar sql-connections-pg-service-file
  (or (getenv "PGSERVICEFILE") "~/.pg_service.conf")
  "libpq の service ファイル。セクション名をそのまま接続先として拾う。")

(defconst sql-connections--mysql-group-prefix "client"
  "接続先として扱う my.cnf のグループ名の接頭辞。
mysql は --defaults-group-suffix=X を付けると [client] に加えて [clientX] を読む。")

(defvar sql-connections-wrangler-root nil
  "ローカル D1 を探す repo。nil なら `project-current' の root を使う。")

(defvar sql-connections-wrangler-d1-dir
  ".wrangler/state/v3/d1/miniflare-D1DatabaseObject"
  "repo からの相対で、wrangler dev がローカル D1 の SQLite を置くディレクトリ。")

(defconst sql-connections--wrangler-name-length 8
  "接続名に使うファイル名の先頭の桁数。")

;;; INI パーサ

(defun sql-connections--unquote (value)
  "VALUE を囲う引用符を外す。"
  (if (and (>= (length value) 2)
           (memq (aref value 0) '(?\" ?'))
           (eq (aref value 0) (aref value (1- (length value)))))
      (substring value 1 -1)
    value))

(defun sql-connections--parse-ini (file)
  "FILE を INI として読み、((セクション . ((キー . 値) ...)) ...) を返す。
FILE が無ければ nil。# と ; はコメント、! で始まる行 (my.cnf の !include) は無視する。"
  (let ((path (and file (expand-file-name file))))
    (when (and path (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (let (sections)
          (while (not (eobp))
            (let ((line (string-trim (buffer-substring-no-properties
                                      (line-beginning-position) (line-end-position)))))
              (cond
               ((or (string-empty-p line)
                    (memq (aref line 0) '(?# ?\; ?!)))
                nil)
               ((string-match "\\`\\[\\(.*\\)\\]\\'" line)
                (push (cons (string-trim (match-string 1 line)) nil) sections))
               ;; 値の無いキー (my.cnf の quick など) は空文字列として持つ
               ((and sections (string-match "\\`\\([^=]+?\\)[ \t]*\\(?:=[ \t]*\\(.*\\)\\)?\\'" line))
                (let ((key (match-string 1 line))
                      (value (sql-connections--unquote (string-trim (or (match-string 2 line) "")))))
                  (setcdr (car sections)
                          (append (cdr (car sections)) (list (cons key value))))))))
            (forward-line 1))
          (nreverse sections))))))

(defun sql-connections--merge (base override)
  "BASE のキーを OVERRIDE で上書きした alist を返す。"
  (let ((result (copy-alist base)))
    (dolist (cell override result)
      (setf (alist-get (car cell) result nil nil #'equal) (cdr cell)))))

(defun sql-connections--get (params key &optional default)
  "PARAMS から KEY を引く。空なら DEFAULT。"
  (let ((value (cdr (assoc key params))))
    (if (or (null value) (string-empty-p value)) default value)))

;;; 接続先の一覧

(defun sql-connections-mysql-list ()
  "`sql-connections-my-cnf' の [client<名前>] を ((名前 . パラメータ) ...) で返す。
パラメータは [client] を継承した後の値。"
  (let* ((parsed (sql-connections--parse-ini sql-connections-my-cnf))
         (base (cdr (assoc sql-connections--mysql-group-prefix parsed)))
         (prefix-length (length sql-connections--mysql-group-prefix))
         result)
    (dolist (section parsed)
      (let ((group (car section)))
        (when (and (string-prefix-p sql-connections--mysql-group-prefix group)
                   (> (length group) prefix-length))
          (push (cons (substring group prefix-length)
                      (sql-connections--merge base (cdr section)))
                result))))
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun sql-connections-postgres-list ()
  "`sql-connections-pg-service-file' のセクションを ((名前 . パラメータ) ...) で返す。"
  (sort (sql-connections--parse-ini sql-connections-pg-service-file)
        (lambda (a b) (string< (car a) (car b)))))

;;; wrangler のローカル D1

(defun sql-connections--wrangler-root ()
  "ローカル D1 を探す repo。無ければ nil。"
  (or sql-connections-wrangler-root
      (when-let* ((project (project-current)))
        (project-root project))))

(defun sql-connections--wrangler-used-p (file)
  "FILE にユーザのテーブルがあれば非 nil。
sqlite_ は SQLite、_cf_ は miniflare の内部テーブルなので数えない (LIKE の _ は
1 文字のワイルドカードなので escape する)。wrangler.jsonc に書いただけで一度も
使っていない binding のファイルもできるため、それを接続先の一覧から外す。
組み込み sqlite の無い Emacs では判定できないので、その場合は全て採用する。"
  (if (not (sqlite-available-p))
      t
    ;; sqlite-open は読み書きで開くので -wal / -shm ができるが、miniflare が
    ;; 同じものを既に作っている (D1 は WAL) ので実害はない。
    (let ((db (sqlite-open file)))
      (unwind-protect
          (> (caar (sqlite-select
                    db (concat "select count(*) from sqlite_master"
                               " where type = 'table'"
                               " and name not like 'sqlite\\_%' escape '\\'"
                               " and name not like '\\_cf\\_%' escape '\\'")))
             0)
        (sqlite-close db)))))

(defun sql-connections-wrangler-list ()
  "wrangler dev のローカル D1 を ((名前 . ファイル) ...) で返す。
名前は <repo 名>/<ファイル名の先頭 8 桁>。ファイル名は miniflare が Durable Object
の id から作るもので、binding 名や database_id からは復元できないため、
ファイル名をそのまま識別子にする (どの DB かは中身を見て判断する)。"
  (when-let* ((root (sql-connections--wrangler-root))
              (dir (expand-file-name sql-connections-wrangler-d1-dir root))
              ((file-directory-p dir))
              (repo (file-name-nondirectory (directory-file-name root))))
    (delq nil
          (mapcar
           (lambda (file)
             (let ((base (file-name-base file)))
               ;; metadata.sqlite は miniflare 自身の台帳で、D1 の中身ではない
               (and (not (equal base "metadata"))
                    (sql-connections--wrangler-used-p file)
                    (cons (format "%s/%s" repo
                                  (substring base 0
                                             (min sql-connections--wrangler-name-length
                                                  (length base))))
                          file))))
           (directory-files dir t "\\.sqlite\\'")))))

;;; sql-connection-alist (SQLi)

(defun sql-connections-alist ()
  "設定ファイルから `sql-connection-alist' の値を組み立てる。
接続情報は渡さず、mysql には --defaults-group-suffix、psql には service= だけを渡す。
`sql-get-login' に追加のプロンプトを出させないため、残りの login-params は空で束縛する。"
  (append
   (mapcar
    (lambda (conn)
      `(,(intern (concat "mysql:" (car conn)))
        (sql-product 'mysql)
        ;; --defaults-group-suffix は mysql の最初の引数でなければならない。
        ;; sql-comint-mysql は sql-mysql-options を先頭に置くのでここに入れる。
        (sql-mysql-options (cons ,(format "--defaults-group-suffix=%s" (car conn))
                                 sql-mysql-options))
        (sql-user "") (sql-password "") (sql-server "") (sql-port 0) (sql-database "")))
    (sql-connections-mysql-list))
   (mapcar
    (lambda (conn)
      `(,(intern (concat "postgres:" (car conn)))
        (sql-product 'postgres)
        (sql-user "") (sql-password "") (sql-server "") (sql-port 0)
        ;; psql は最後の引数を conninfo 文字列として解釈する
        (sql-database ,(format "service=%s" (car conn)))))
    (sql-connections-postgres-list))
   (mapcar
    (lambda (conn)
      `(,(intern (concat "sqlite:" (car conn)))
        (sql-product 'sqlite)
        (sql-user "") (sql-password "") (sql-server "") (sql-port 0)
        ;; sqlite3 は引数がファイルパスそのもの
        (sql-database ,(cdr conn))))
    (sql-connections-wrangler-list))))

;;;###autoload
(defun sql-connections-refresh ()
  "`sql-connection-alist' を client 設定ファイルから作り直す。
設定ファイルを編集した後に呼ぶ。"
  (interactive)
  (setq sql-connection-alist (sql-connections-alist))
  (when (called-interactively-p 'interactive)
    (message "sql-connections: %s"
             (mapconcat #'symbol-name (mapcar #'car sql-connection-alist) " ")))
  sql-connection-alist)

;;; sqls (言語サーバ)

(defun sql-connections--mysql-dsn (params)
  "PARAMS から go-sql-driver の DSN を作る。"
  (let ((user (sql-connections--get params "user" ""))
        (password (sql-connections--get params "password"))
        (host (sql-connections--get params "host" "127.0.0.1"))
        (port (sql-connections--get params "port" "3306"))
        (database (sql-connections--get params "database" "")))
    (format "%s@tcp(%s:%s)/%s"
            (if password (concat user ":" password) user)
            host port database)))

(defun sql-connections--pg-dsn (params)
  "PARAMS から libpq 形式の DSN を作る。
service ファイルの options=-csearch_path=... は sqls が解釈できないので展開する。"
  (let* ((options (sql-connections--get params "options"))
         (search-path (and options
                           (string-match "-c[ \t]*search_path=\\([^ \t]+\\)" options)
                           (match-string 1 options)))
         (pairs (delq nil
                      (mapcar (lambda (key)
                                (let ((value (sql-connections--get params key)))
                                  (and value (format "%s=%s" key value))))
                              '("host" "port" "dbname" "user" "password")))))
    (string-join
     (append pairs
             ;; ローカルの開発 DB が大半なので既定は disable。
             ;; 本番など TLS が要るものは service ファイルに sslmode を書けばそちらを使う。
             (list (format "sslmode=%s" (sql-connections--get params "sslmode" "disable")))
             (and search-path (list (format "search_path=%s" search-path))))
     " ")))

(defun sql-connections-sqls ()
  "sqls に渡す設定 (:connections [...]) を返す。渡せる接続先が無ければ nil。
DB 名 (my.cnf の database= / service ファイルの dbname=) の無い接続は外す。
sqls は起動時に DATABASE() を読むので、無いと \"no database connection\" と
Scan error を出し続ける。"
  (let ((connections
         (append
          (mapcar (lambda (conn)
                    (list :driver "mysql"
                          :dataSourceName (sql-connections--mysql-dsn (cdr conn))))
                  (seq-filter (lambda (conn)
                                (sql-connections--get (cdr conn) "database"))
                              (sql-connections-mysql-list)))
          (mapcar (lambda (conn)
                    (list :driver "postgresql"
                          :dataSourceName (sql-connections--pg-dsn (cdr conn))))
                  (seq-filter (lambda (conn)
                                (sql-connections--get (cdr conn) "dbname"))
                              (sql-connections-postgres-list)))
          (mapcar (lambda (conn)
                    (list :driver "sqlite3" :dataSourceName (cdr conn)))
                  (sql-connections-wrangler-list)))))
    (when connections
      (list :connections (vconcat connections)))))

(provide 'sql-connections)
;;; sql-connections.el ends here
