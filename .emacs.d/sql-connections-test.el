;;; sql-connections-test.el --- tests for sql-connections -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l sql-connections-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'sql)

(load (expand-file-name "sql-connections.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro sql-connections-test--with-files (my-cnf pg-service &rest body)
  "MY-CNF と PG-SERVICE (nil なら作らない) を一時ファイルに書いて BODY を実行する。"
  (declare (indent 2))
  `(let* ((dir (file-name-as-directory (make-temp-file "sql-connections-test" t)))
          (sql-connections-my-cnf (expand-file-name "my.cnf" dir))
          (sql-connections-pg-service-file (expand-file-name "pg_service.conf" dir)))
     (unwind-protect
         (progn
           (when ,my-cnf
             (with-temp-file sql-connections-my-cnf (insert ,my-cnf)))
           (when ,pg-service
             (with-temp-file sql-connections-pg-service-file (insert ,pg-service)))
           ,@body)
       (delete-directory dir t))))

(defun sql-connections-test--bindings (entry)
  "接続定義 ENTRY の変数束縛を評価し、(product user password server port database options) を返す。"
  (eval `(let ,(cdr entry)
           (list sql-product sql-user sql-password sql-server sql-port sql-database
                 sql-mysql-options))
        t))

(defconst sql-connections-test--my-cnf "\
# コメント
[client]
host=example.invalid
user   =   base
password = \"basepw\"
port=1

[mysqldump]
quick

[clientbeeco]
; セミコロンのコメント
host=127.0.0.1
port=13306
user=dev
password=dev
database=platform

[clientsocket]
user=root
database=app
")

(defconst sql-connections-test--pg-service "\
# コメント
[beeco]
host=127.0.0.1
port=15432
dbname=redshift_dev
user=redshift_user
password=redshift_local_pass
options=-csearch_path=platform

[bare]
dbname=onlydb
")

;;; ini パーサ

(ert-deftest sql-connections-test-parse-ini ()
  "セクション・コメント・空白・引用符を扱える。"
  (sql-connections-test--with-files sql-connections-test--my-cnf nil
    (let ((parsed (sql-connections--parse-ini sql-connections-my-cnf)))
      (should (equal (mapcar #'car parsed)
                     '("client" "mysqldump" "clientbeeco" "clientsocket")))
      (let ((client (cdr (assoc "client" parsed))))
        (should (equal (cdr (assoc "host" client)) "example.invalid"))
        ;; = の周りの空白は落とす
        (should (equal (cdr (assoc "user" client)) "base"))
        ;; 値を囲む引用符は外す
        (should (equal (cdr (assoc "password" client)) "basepw")))
      ;; 値のないキーは空文字列
      (should (equal (cdr (assoc "quick" (cdr (assoc "mysqldump" parsed)))) "")))))

(ert-deftest sql-connections-test-parse-ini-missing-file ()
  "ファイルが無ければ nil を返し、エラーにしない。"
  (sql-connections-test--with-files nil nil
    (should (null (sql-connections--parse-ini sql-connections-my-cnf)))))

;;; 一覧

(ert-deftest sql-connections-test-mysql-list ()
  "[client<名前>] だけを接続として拾い、[client] をベースにマージする。"
  (sql-connections-test--with-files sql-connections-test--my-cnf nil
    (let ((conns (sql-connections-mysql-list)))
      (should (equal (mapcar #'car conns) '("beeco" "socket")))
      (let ((beeco (cdr (assoc "beeco" conns))))
        ;; グループ側の値が [client] を上書きする
        (should (equal (cdr (assoc "host" beeco)) "127.0.0.1"))
        (should (equal (cdr (assoc "port" beeco)) "13306"))
        (should (equal (cdr (assoc "user" beeco)) "dev")))
      ;; グループに無いキーは [client] から引き継ぐ
      (should (equal (cdr (assoc "host" (cdr (assoc "socket" conns))))
                     "example.invalid")))))

(ert-deftest sql-connections-test-postgres-list ()
  "pg_service.conf のセクションをそのまま拾う。"
  (sql-connections-test--with-files nil sql-connections-test--pg-service
    (let ((conns (sql-connections-postgres-list)))
      (should (equal (mapcar #'car conns) '("bare" "beeco")))
      (should (equal (cdr (assoc "dbname" (cdr (assoc "beeco" conns))))
                     "redshift_dev")))))

;;; sql-connection-alist

(ert-deftest sql-connections-test-alist-mysql ()
  "mysql は --defaults-group-suffix だけを渡し、他は空にして my.cnf に任せる。"
  (sql-connections-test--with-files sql-connections-test--my-cnf nil
    (let* ((sql-mysql-options '("-t" "-A"))
           (alist (sql-connections-alist))
           (entry (assq 'mysql:beeco alist)))
      (should entry)
      (pcase-let ((`(,product ,user ,password ,server ,port ,database ,options)
                   (sql-connections-test--bindings entry)))
        (should (eq product 'mysql))
        ;; コマンドラインに認証情報を出さない (ps で見えてしまう)
        (should (equal user ""))
        (should (equal password ""))
        (should (equal server ""))
        (should (equal port 0))
        (should (equal database ""))
        ;; --defaults-group-suffix は mysql の最初の引数でなければならず、
        ;; 既定のオプション (-t / -A) はその後ろに残す
        (should (equal options '("--defaults-group-suffix=beeco" "-t" "-A")))))))

(ert-deftest sql-connections-test-alist-postgres ()
  "postgres は service= だけを渡し、他は空にして pg_service.conf に任せる。"
  (sql-connections-test--with-files nil sql-connections-test--pg-service
    (let* ((alist (sql-connections-alist))
           (entry (assq 'postgres:beeco alist)))
      (should entry)
      (pcase-let ((`(,product ,user ,password ,server ,port ,database ,_options)
                   (sql-connections-test--bindings entry)))
        (should (eq product 'postgres))
        (should (equal user ""))
        (should (equal password ""))
        (should (equal server ""))
        (should (equal port 0))
        (should (equal database "service=beeco"))))))

(ert-deftest sql-connections-test-alist-covers-login-params ()
  "sql-connect が追加のプロンプトを出さないよう、login-params を全て束縛する。"
  (sql-connections-test--with-files sql-connections-test--my-cnf
      sql-connections-test--pg-service
    (dolist (entry (sql-connections-alist))
      (let ((bound (mapcar #'car (cdr entry)))
            (product (if (string-prefix-p "mysql" (symbol-name (car entry)))
                         'mysql 'postgres)))
        (dolist (param (sql-get-product-feature product :sqli-login))
          ;; login-params の要素は名前か (名前 :default ...) のどちらか
          (let ((name (if (consp param) (car param) param)))
            (should (memq (intern (format "sql-%s" name)) bound))))))))

;;; sqls (言語サーバ) の設定

(ert-deftest sql-connections-test-sqls ()
  "同じファイルから sqls の :connections を組み立てる。"
  (sql-connections-test--with-files sql-connections-test--my-cnf
      sql-connections-test--pg-service
    (let* ((conns (plist-get (sql-connections-sqls) :connections)))
      (should (vectorp conns))
      (should (equal (mapcar (lambda (c) (plist-get c :driver)) (append conns nil))
                     '("mysql" "mysql" "postgresql" "postgresql")))
      (let ((dsns (mapcar (lambda (c) (plist-get c :dataSourceName)) (append conns nil))))
        ;; mysql: user:password@tcp(host:port)/database
        (should (member "dev:dev@tcp(127.0.0.1:13306)/platform" dsns))
        ;; グループに無いキーは [client] から継承する (host / port / password)
        (should (member "root:basepw@tcp(example.invalid:1)/app" dsns))
        ;; postgres: libpq のキーを並べ、options の search_path は sqls 用に展開する
        (should (member (concat "host=127.0.0.1 port=15432 dbname=redshift_dev "
                                "user=redshift_user password=redshift_local_pass "
                                "sslmode=disable search_path=platform")
                        dsns))
        ;; 書かれていないキーは出さない
        (should (member "dbname=onlydb sslmode=disable" dsns))))))

(ert-deftest sql-connections-test-sqls-mysql-defaults ()
  "host / port がどこにも無ければ go-sql-driver 向けに既定値で埋める。"
  (sql-connections-test--with-files "[clientlocal]\nuser=root\ndatabase=app\n" nil
    (let ((dsns (mapcar (lambda (c) (plist-get c :dataSourceName))
                        (append (plist-get (sql-connections-sqls) :connections) nil))))
      ;; パスワードが無いときは ":" を付けない
      (should (equal dsns '("root@tcp(127.0.0.1:3306)/app"))))))

(ert-deftest sql-connections-test-sqls-requires-database ()
  "DB 名の無い接続は sqls に渡さない。
sqls は起動時に DATABASE() を読むので、DB 名が無いと \"no database connection\" と
Scan error を出し続ける。SQLi (sql-connect) 側では DB 名が無くても使えるので、
一覧から外すのは sqls の分だけ。"
  (sql-connections-test--with-files
      "[clientnodb]\nuser=root\n[clientwithdb]\nuser=root\ndatabase=app\n"
      "[nodb]\nhost=h\n[withdb]\nhost=h\ndbname=app\n"
    (let ((dsns (mapcar (lambda (c) (plist-get c :dataSourceName))
                        (append (plist-get (sql-connections-sqls) :connections) nil))))
      (should (equal (length dsns) 2))
      (should (member "root@tcp(127.0.0.1:3306)/app" dsns))
      (should (member "host=h dbname=app sslmode=disable" dsns)))
    ;; SQLi の一覧は DB 名の有無に関係なく全部出す
    (should (equal (mapcar #'car (sql-connections-alist))
                   '(mysql:nodb mysql:withdb postgres:nodb postgres:withdb)))))

(ert-deftest sql-connections-test-sqls-empty ()
  "設定ファイルが無ければ nil (eglot に何も渡さない)。"
  (sql-connections-test--with-files nil nil
    (should (null (sql-connections-sqls)))))

;;; refresh

(ert-deftest sql-connections-test-refresh ()
  "`sql-connection-alist' を設定ファイルから作り直す。"
  (sql-connections-test--with-files sql-connections-test--my-cnf
      sql-connections-test--pg-service
    (let ((sql-connection-alist '((stale (sql-product 'ansi)))))
      (sql-connections-refresh)
      (should (equal (mapcar #'car sql-connection-alist)
                     '(mysql:beeco mysql:socket postgres:bare postgres:beeco))))))

(provide 'sql-connections-test)
;;; sql-connections-test.el ends here
