;; -*- scheme -*-
(library (sagittarius cgen util)
    (export for-each1-with-index string-split alpha alpha-num
	    c-identifier register-number-compare
	    register-and/or set-renderer! generate-renderer renderer
	    renderer-indent-incl! renderer-indent-decl!
	    renderer-no-indent
	    acons
	    gen-temporary)
    (import (rnrs (6))
	    (only (srfi :13) string-tokenize)
	    (only (srfi :14) string->char-set)
	    (sagittarius format))
  ;; TODO remove
  (define-syntax acons
    (syntax-rules ()
      ((_ a b alist)
       (cons (cons a b) alist))))

  (define *renderer* #f)
  (define *renderer-indent* "")
  (define *renderer-no-indent* #f)
  
  (define (set-renderer! renderer)
    (set! *renderer* renderer))

  (define (renderer) *renderer*)
  (define (renderer-indent-incl!)
    (set! *renderer-indent* (string-append *renderer-indent* "  ")))

  (define (renderer-indent-decl!)
    (set! *renderer-indent* (substring *renderer-indent* 2 (string-length *renderer-indent*))))

  (define (renderer-no-indent . flag)
    (if (null? flag)
	*renderer-no-indent*
	(set! *renderer-no-indent* (car flag))))

  (define (generate-renderer . user-renderer)
    (lambda (x)
      (if (null? user-renderer)
	  (begin
	    (unless *renderer-no-indent*
	      (display *renderer-indent*))
	    (display x))
	  (begin
	    (unless *renderer-no-indent*
	      ((car user-renderer) *renderer-indent*))
	    ((car user-renderer) x)))))

  (define for-each1-with-index
    (lambda (proc lst)
      (let loop ((i 0)
		 (lst lst))
	(cond ((null? lst) '())
	      (else
	       (proc i (car lst))
	       (loop (+ i 1) (cdr lst)))))))

  ;; number compare
  (define-syntax register-number-compare
    (syntax-rules ()
      ((_ name op)
       (define (name body dispatch k)
	 (or (= (length body) 3)
	     (error 'break (format "3 argument required but got ~s" (length body)) body))
	 (renderer-no-indent #t)
	 (dispatch (cadr body) dispatch k)
	 (renderer-no-indent #t)
	 ((renderer) (format " ~s " 'op))
	 (renderer-no-indent #t)
	 (dispatch (caddr body) dispatch k)
	 (renderer-no-indent #f)))))

  (define-syntax register-and/or
    (syntax-rules ()
      ((_ name op)
       (define (name body dispatch k)
	 (renderer-no-indent #t)
	 ((renderer) "(")
	 (let loop ((first #t)
		    (args (cdr body)))
	   (renderer-no-indent #t)
	   (unless (null? args)
	     (unless first
	       ((renderer) (format " ~a " op)))
	     (dispatch (car args) dispatch k)
	     (loop #f (cdr args))))
	 (renderer-no-indent #t)
	 ((renderer) ")")
	 (renderer-no-indent #f)))))


  (define alpha        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (define alpha-num    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")
  (define c-identifier "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890*_")

  (define (string-split s . seed)
    (if (null? seed)
	(string-tokenize s (string->char-set alpha-num))
	(string-tokenize s (string->char-set (car seed)))))
  
  (define gen-temporary
    (let ((count 0))
      (lambda ()
	(set! count (+ count 1))
	(string->symbol (format "~s~a" 'cgen_ count))))))
