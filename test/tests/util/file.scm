(import (rnrs)
	(util file)
	(sagittarius)
	(sagittarius control)
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

(test-end)
