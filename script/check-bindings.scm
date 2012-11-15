(import (rnrs) (util file)
        (sagittarius vm) (match)
        (srfi :26)
	(getopt))

(define-constant +default-path+ "./{lib,sitelib,ext}/**/*.scm")
 
(define (check-exports file)
  (define (do-check name exports)
    (guard (e (#t
               (when (message-condition? e)
                 (print (condition-message e)))
               (print "failed to find library: " name)))
      ;; resolve library first otherwise compiler compiles library
      ;; incollectly.
      (eval `(import ,name) #f)
      (let ((lib (find-library name #f)))
        (define (check orig export)
          (unless (keyword? export)
	    (let ((gloc (find-binding lib export #f)))
	      (or (and gloc (gloc-bound? gloc))
		  (format (current-error-port)
			  "Not binded ~a#~a~%" export name)))))
        (for-each
         (lambda (export)
           (match export
             (('rename renames ...)
              (for-each (lambda (rename)
                          (check rename (car rename))) renames))
             (_ (check export export))))
         exports))))
  (guard (e (#t (print "failed to read: " file)))
    (when (file-regular? file)
      (let ((expr (file->sexp-list file)))
        (match expr
          ((('library name ('export exports ...) rest ...))
           (do-check name exports))
          (_ #f))))))

(define (check files)
  (for-each (cut check-exports <>) files))

(define (main args)
  (with-args args
      ((path (#\p "path") #f +default-path+))
    (check (glob path))
    (print 'done!)))
