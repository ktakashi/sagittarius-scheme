(import (rnrs)
	(util file)
	(sagittarius)
	(sagittarius control)
	(srfi :1)
	(srfi :64 testing))

(test-begin "File operations tests")

;; creates test directory
(define top-test-dir "test-util-file")
(define test-directory (build-path top-test-dir
				   (build-path "file" "test")))
(create-directory* test-directory)
;; creates test files
(dotimes (i 10)
  (let1 path (build-path test-directory (number->string i))
    (when (file-exists? path) (delete-file path))
    (when (file-symbolic-link? path) (delete-file path))
    (if (= i 9)
	(create-symbolic-link (build-path test-directory "0")
			      (build-path test-directory "9"))
	(call-with-output-file path
	  (lambda (p)
	    (dotimes (j (+ i 1))
	      (put-string p "test\n")))))))

(test-equal "file->string" "test\n" 
	    (file->string (build-path test-directory "0")))
(test-equal "file->string-list" '("test" "test")
	    (file->string-list (build-path test-directory "1")))
(test-equal "file->sexp-list" '(test test)
	    (file->sexp-list (build-path test-directory "1")))
(test-equal "file->list" '(1 1)
	    (file->list (lambda (p)
			  (if (eof-object? (read p))
			      (eof-object)
			      1))
			(build-path test-directory "1")))

;; find files
(let ((files (do ((i 0 (+ i 1))
		  (r '() (cons (build-path test-directory (number->string i))
			       r)))
		 ((= i 10) (reverse! r)))))
  (test-equal "find-files" (take files 9) (find-files test-directory))
  (test-equal "find-files with physical #f"
	      files
	      (find-files test-directory :physical #f))
  (test-equal "find-files reverse sort" 
	      (reverse files)
	      (find-files test-directory :sort string>=? :physical #f))
  (test-equal "find-files recursive #f" 
	      '()
	      (find-files top-test-dir :recursive #f))
  (test-equal "find-files recursive #t from top" 
	      (take files 9)
	      (find-files top-test-dir)))

;; path-for-each
(test-assert "path-for-each" (path-for-each top-test-dir (lambda (p t) 'ok)))
(test-assert "path-for-each file-only (inclusing symbolic-link)"
	     (path-for-each top-test-dir
			    (lambda (p t)
			      (unless (or (eq? t 'file) (eq? t 'symbolic-link))
				(error 'ng "non-file")))
			    :file-only #t
			    :physical #f))
(test-assert "path-for-each file-only"
	     (path-for-each top-test-dir
			    (lambda (p t)
			      (when (eq? t 'directory) (error 'ng "non-file")))
			    :file-only #t))
(test-assert "path-for-each file-only"
	     (path-for-each top-test-dir
			    (lambda (p t)
			      (when (eq? t 'directory) (error 'ng "non-file")))
			    :file-only #t))

;; path-map
(let ((files (do ((i 0 (+ i 1))
		  (r '() (cons (build-path test-directory (number->string i))
			       r)))
		 ((= i 10) (reverse! r)))))
  (test-equal "path-map"
	      (take files 9) ;; remove symbolic-link
	      (list-sort string<=? (path-map test-directory (lambda (v t) v))))
  (test-equal "path-map physical"
	      files
	      (list-sort string<=? (path-map test-directory (lambda (v t) v)
					     :physical #f)))
  )

(test-assert "delete-directory*" (delete-directory* top-test-dir))
(test-assert "dir is removed(1)" (not (file-exists? top-test-dir)))

;; build-path*
(test-equal "build-path*" 
	    (cond-expand
	     (windows "a\\b\\c\\d")
	     (else "a/b/c/d"))
	    (build-path* "a" "b" "c" "d"))
;; trivials
(test-equal "build-path*(0)" "" (build-path*))
(test-equal "build-path*(1)" "a" (build-path* "a"))
(test-equal "build-path*(2)" (cond-expand (windows "a\\b") (else "a/b"))
	    (build-path* "a" "b"))

;; call #81
(import (srfi :1) (srfi :18))
(let ()
  (define ls '(1 2 3))

  (let ((ts (map (lambda (a) 
		   (make-thread (lambda ()
				  (let-values (((out file) 
						(make-temporary-file)))
				    (close-output-port out)
				    file)))) ls)))
    (map thread-start! ts)
    (let ((files (map thread-join! ts)))
      (delete-duplicates! files)
      (test-equal "file count" 3 (length files))
      (for-each delete-file files))))

;; call #140
(let ()
  (define hidden-dir (build-path* top-test-dir ".hidden"))
  (define normal-dir (build-path* top-test-dir "normal"))
  (define (fake-touch file) (call-with-output-file file (lambda (out) #t)))
  (create-directory* hidden-dir)
  (create-directory* normal-dir)
  (create-directory* (build-path* normal-dir ".hidden2"))
  (fake-touch (build-path* normal-dir "foo"))
  (fake-touch (build-path* hidden-dir "boo"))
  (test-assert "delete-directory* with hidden" (delete-directory* top-test-dir))
  (test-assert "dir is removed(2)" (not (file-exists? top-test-dir))))
  
;; related to call #140
(let ()
  (define hidden-dir (build-path* top-test-dir ".hidden"))
  (define normal-dir (build-path* top-test-dir "normal"))
  (define (fake-touch file) (call-with-output-file file (lambda (out) #t)))
  ;; implement with path-map
  (define ($delete-directory path)
    ;; delete-directory can handle file as well
    (define (remove path type) (delete-directory path))
    (path-map path remove)
    ;; delete the top most
    (delete-directory path))
  (create-directory* hidden-dir)
  (create-directory* normal-dir)
  (create-directory* (build-path* normal-dir ".hidden2"))
  (fake-touch (build-path* normal-dir "foo"))
  (fake-touch (build-path* hidden-dir "boo"))
  (test-assert "$delete-directory" ($delete-directory top-test-dir))
  (test-assert "dir is removed(3)" (not (file-exists? top-test-dir))))

(test-end)
