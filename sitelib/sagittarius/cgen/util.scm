;; -*- scheme -*-
(library (sagittarius cgen util)
    (export for-each1-with-index string-split alpha alpha-num
	    c-identifier register-number-compare
	    register-and/or set-renderer! generate-renderer renderer
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
  
  (define (set-renderer! renderer)
    (set! *renderer* renderer))

  (define (renderer) *renderer*)

  (define (generate-renderer . user-renderer)
    (lambda (x)
      (if (null? user-renderer)
	  (display x)
	  ((car user-renderer) x))))

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
	 (dispatch (cadr body) dispatch k)
	 ((renderer) (format " ~s " 'op))
	 (dispatch (caddr body) dispatch k)))))

  (define-syntax register-and/or
    (syntax-rules ()
      ((_ name op)
       (define (name body dispatch k)
	 ((renderer) "(")
	 (let loop ((first #t)
		    (args (cdr body)))
	   (unless (null? args)
	     (unless first
	       ((renderer) (format " ~a " op)))
	     (dispatch (car args) dispatch k)
	     (loop #f (cdr args))))
	 ((renderer) ")")))))


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
