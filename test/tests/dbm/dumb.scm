(import (rnrs) (dbm) (srfi :64) (clos user))

(test-begin "DBM - DUMB test")

(define-constant +dumb-db-file+ "dumb.db")
(when (file-exists? +dumb-db-file+)
  (delete-file +dumb-db-file+))

(define dumb-class (dbm-type->class 'dumb))
(test-assert "dbm meta class" (is-a? dumb-class <dbm-meta>))
(test-assert "meta operation" (not (dbm-db-exists? dumb-class +dumb-db-file+)))

(let ((dumb-dbm (dbm-open dumb-class :path +dumb-db-file+)))
  (test-assert "dbm" (is-a? dumb-dbm <dbm>))
  (test-assert "open?" (not (dbm-closed? dumb-dbm)))

  (test-assert "put!" (dbm-put! dumb-dbm 'key1 #t))
  (test-assert "get" (dbm-get dumb-dbm 'key1))
  (test-assert "get" (boolean? (dbm-get dumb-dbm 'key1)))
  (test-assert "exists" (dbm-exists? dumb-dbm 'key1))

  (test-error "unbound" condition? (dbm-get dumb-dbm 'no-value))
  (test-equal "fallback" "fallback" (dbm-get dumb-dbm 'no-value "fallback"))

  (test-equal "dbm-for-each" "#t" 
	      (call-with-string-output-port
	       (lambda (out)
		 (dbm-for-each dumb-dbm (lambda (k v) (display v out))))))

  (test-equal "dbm-map" '((key1 . #t)) (dbm-map dumb-dbm cons))

  (test-assert "dbm-close" (dbm-close dumb-dbm))
  (test-assert "check db file close" (file-exists? +dumb-db-file+))
)

;; read existing db test
(let ((dumb-dbm (dbm-open dumb-class :path +dumb-db-file+)))
  (test-assert "get" (dbm-get dumb-dbm 'key1))
  (test-assert "get" (boolean? (dbm-get dumb-dbm 'key1)))
  (test-assert "exists" (dbm-exists? dumb-dbm 'key1))
)

;; meta
(test-assert "dbm-db-exists?" (dbm-db-exists? dumb-class +dumb-db-file+))
(test-assert "dbm-db-copy" 
	     (dbm-db-copy dumb-class +dumb-db-file+ "copied.db"
			  :overwrite #t))
(test-assert "dbm-db-exists? (copied.db) (1)"
	     (dbm-db-exists? dumb-class "copied.db"))
(test-assert "dbm-db-move" (dbm-db-move dumb-class "copied.db" "moved.db"
					:overwrite #t))
(test-assert "dbm-db-exists? (copied.db) (2)" 
	     (not (dbm-db-exists? dumb-class "copied.db")))
(test-assert "dbm-db-exists? (moved.db) (1)" 
	     (dbm-db-exists? dumb-class "moved.db"))
(test-assert "dbm-db-remove (moved.db)" 
	     (dbm-db-remove dumb-class "moved.db"))
(test-assert "dbm-db-exists? (moved.db) (2)" 
	     (not (dbm-db-exists? dumb-class "moved.db")))

(test-assert "dbm-db-remove" (dbm-db-remove dumb-class +dumb-db-file+))
(test-assert "dbm-db-exists?" 
	     (not (dbm-db-exists? dumb-class +dumb-db-file+)))
;; TODO more tests...

(test-end)