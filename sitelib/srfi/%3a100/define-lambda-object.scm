;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a100.scm/define-lambda-object.scm - define-lambda-object
;;;  
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(library (srfi :100 define-lambda-object)
    (export define-lambda-object)
    (import (rnrs)
	    ;; we make applicable <lambda-object> instead of procedure
	    (clos user))

(define-class <lambda-object> () ((procedure :init-keyword :procedure)))
(define-method object-apply ((o <lambda-object>) . args)
  (apply (slot-ref o 'procedure) args))

;;; define-lambda-object --- define-syntax

(define-syntax unquote-get
  (syntax-rules ()
    ((unquote-get symbol ((n0 d0) (n1 d1) ...))
     (if (eq? symbol 'n0)
	 d0
	 (unquote-get symbol ((n1 d1) ...))))
    ((unquote-get symbol ())
     (error 'define-lambda-object "absent field" symbol))))

(define-syntax unquote-get*
  (syntax-rules ()
    ((unquote-get* symbol (n0 n1 ...))
     (if (eq? symbol 'n0)
	 n0
	 (unquote-get* symbol (n1 ...))))
    ((unquote-get* symbol ())
     (error 'define-lambda-object "not available inspection" symbol))))

(define-syntax unquote-set!
  (syntax-rules ()
    ((unquote-set! symbol new-val (n0 n1 ...) fi)
     (if (eq? symbol 'n0)
	 (set! n0 new-val)
	 (unquote-set! symbol new-val (n1 ...) fi)))
    ((unquote-set! symbol new-val () fi)
     (if (memq symbol 'fi)
	 (error 'define-lambda-object "read-only field" symbol)
	 (error 'define-lambda-object "absent field" symbol)))))

(define-syntax seq-lambda
  (syntax-rules ()
    ((seq-lambda () (r ...) () body)
     (lambda (r ...) body))
    ((seq-lambda () (r ...) (o oo ...) body)
     (lambda (r ... . z)
       (seq-lambda (z) () (o oo ...) body)))
    ((seq-lambda (z) () ((n d) . e) body)
     (let ((y (if (null? z) z (cdr z)))
	   (n (if (null? z) d (car z))))
       (seq-lambda (y) () e body)))
    ((seq-lambda (z) () () body)
     (if (null? z)
	 body
	 (error 'define-lambda-object "too many arguments" z)))))

;; Choose either procedure type or macro type according to your implementation.
;; 1. procedure opt-key
(define (opt-key z k d)
  (let ((x (car z)) (y (cdr z)))
    (if (null? y)
	(cons d z)
	(if (eq? k x)
	    y
	    (let lp ((head (list x (car y))) (tail (cdr y)))
	      (if (null? tail)
		  (cons d z)
		  (let ((x (car tail)) (y (cdr tail)))
		    (if (null? y)
			(cons d z)
			(if (eq? k x)
			    (cons (car y) (append head (cdr y)))
			    (lp (cons x (cons (car y) head)) (cdr y)))))))))))
;; 2. macro opt-key!
(define-syntax opt-key!
  (syntax-rules ()
    ((opt-key! z n d)
     (let ((x (car z)) (y (cdr z)))
       (if (null? y)
	   d
	   (if (eq? 'n x)
	       (begin (set! z (cdr y)) (car y))
	       (let lp ((head (list x (car y)))
			(tail (cdr y)))
		 (if (null? tail)
		     d
		     (let ((x (car tail)) (y (cdr tail)))
		       (if (null? y)
			   d
			   (if (eq? 'n x)
			       (begin (set! z (append head (cdr y))) (car y))
			       (lp (cons x (cons (car y) head)) (cdr y)))))))))))))

(define-syntax key-lambda
  (syntax-rules ()
    ((key-lambda () (r ...) () body)
     (lambda (r ...) body))
    ((key-lambda () (r ...) (o oo ...) body)
     (lambda (r ... . z)
       (key-lambda (z) () (o oo ...) body)))
    ((key-lambda (z) () ((n d) . e) body)
     ;; 1. procedure opt-key
     (let* ((y (if (null? z) (cons d z) (opt-key z 'n d)))
	    (n (car y))
	    (y (cdr y)))
       (key-lambda (y) () e body)))
    ;; 2. macro opt-key!
    ;; (let ((n (if (null? z) d (opt-key! z n d))))
    ;;   (key-lambda (z) () e body)))
    ((key-lambda (z) () () body)
     (if (null? z)
	 body
	 (error 'define-lambda-object "too many arguments" z)))))

(define (check-duplicate ls err-str)
  (cond ((null? ls) #f)
	((memq (car ls) (cdr ls)) (error 'define-lambda-object err-str (car ls)))
	(else (check-duplicate (cdr ls) err-str))))

(define (check-field part-list main-list cmp name err-str)
  (let lp ((part part-list) (main main-list))
    (if (null? part)
	main
	(if (null? main)
	    (error 'define-lambda-object err-str name (car part))
	    (let ((field (car part)))
	      (if (cmp field (car main))
		  (lp (cdr part) (cdr main))
		  (let loop ((head (list (car main))) (tail (cdr main)))
		    (if (null? tail)
			(error 'define-lambda-object err-str name field)
			(if (cmp field (car tail))
			    (lp (cdr part) (append head (cdr tail)))
			    (loop (cons (car tail) head) (cdr tail)))))))))))

(define-syntax define-object
  (syntax-rules ()
    ((define-object name make-object make-object-by-name pred-object (gr ...) (gi ...) (fm ...) ((fi id) ...) (r ...) (o ...) (a ...) ((c cd) ...) ((v vd) ...) ((h hd) ...))
     (begin
       (define safe-parent
	 (begin
	   ;; check duplication
	   (check-duplicate '(name gi ... gr ...) "duplicated group")
	   (check-duplicate '(fm ... fi ... h ...) "duplicated field")
	   ;; check field
	   (check-field (gi 'read-write-field) '(fm ...) eq? 'gi "incompatible read-write field") ...
	   (check-field (gi 'read-only-field) '(fi ...) eq? 'gi "incompatible read-only field") ...
	   (check-field (gi 'required-field) '(r ...) eq? 'gi "incompatible required field") ...
	   (check-field (gi 'optional-field) '(o ...) equal? 'gi "incompatible optional field") ...
	   (check-field (gi 'automatic-field) '((c cd) ... (v vd) ... a ...) equal? 'gi "incompatible automatic field") ...
	   (check-field (map car (gi 'common-field)) '(c ...) eq? 'gi "incompatible common field") ...
	   (check-field (map car (gi 'virtual-field)) '(v ...) eq? 'gi "incompatible virtual field") ...
	   (check-field (map car (gi 'hidden-field)) '(h ...) eq? 'gi "incompatible hidden field") ...
	   (check-field (append (gr 'read-write-field) (gr 'read-only-field) (map car (gr 'hidden-field))) '(fm ... fi ... h ...) eq? 'gr "incompatible whole field") ...
	   (list gi ... gr ...)))
       (define safe-name 'tmp)
       ;; Alist, vector/enum, vector/alist or hashtable can be used instead of
       ;; unquote-get & unquote-set! according to your implementation.
       ;; cf. (eval-variant expression implementation-specific-namespace)
       ;; An example of vector/enum:
       ;; (define enum-a (make-enumeration '(fm ... fi ...)))
       ;; (define enum-m (make-enumeration '(fm ...)))
       ;; (define enum-index-a (enum-set-indexer enum-a))
       ;; (define enum-index-m (enum-set-indexer enum-m))
       ;; (define makers
       ;;  (let* ((c cd) ...)
       ;;    (cons (seq-lambda () (r ...) (o ...)
       ;;      (let* (a ... (array (vector (lambda (x) (if (eq? enum-index-a x) fm (set! fm x))) ... (lambda (x) id) ...)))
       ;;        (define *%lambda-object%*
       ;;  (lambda (arg . args)
       ;;    (if (null? args)
       ;;        (let ((n (enum-index-a arg)))
       ;;  (if n
       ;;      ((vector-ref array n) enum-index-a)
       ;;      (error 'define-lambda-object "absent field" arg)))
       ;;        (if (null? (cdr args))
       ;;    (let ((n (enum-index-m arg)))
       ;;      (if n
       ;;  ((vector-ref array n) (car args))
       ;;  (if (enum-set-member? arg enum-a)
       ;;      (error 'define-lambda-object "read-only field" arg)
       ;;      (error 'define-lambda-object "absent field" arg))))
       ;;    safe-name))))
       ;;        *%lambda-object%*))
       ;;  (key-lambda () (r ...) (o ...)
       ;;      (let* (a ... (array (vector (lambda (x) (if (eq? enum-index-a x) fm (set! fm x))) ... (lambda (x) id) ...)))
       ;;        (define *%lambda-object%*
       ;;  (lambda (arg . args)
       ;;    (if (null? args)
       ;;        (let ((n (enum-index-a arg)))
       ;;  (if n
       ;;      ((vector-ref array n) enum-index-a)
       ;;      (error 'define-lambda-object "absent field" arg)))
       ;;        (if (null? (cdr args))
       ;;    (let ((n (enum-index-m arg)))
       ;;      (if n
       ;;  ((vector-ref array n) (car args))
       ;;  (if (enum-set-member? arg enum-a)
       ;;      (error 'define-lambda-object "read-only field" arg)
       ;;      (error 'define-lambda-object "absent field" arg))))
       ;;    safe-name))))
       ;;        *%lambda-object%*)))))
       (define makers
	 (let* ((c cd) ...)
	   ;; ++ Example requires this as a list
	   (list (seq-lambda () (r ...) (o ...)
			     (let* (a ...)
			       (make <lambda-object>
				 :procedure
				 (lambda (arg . args)
				   (if (null? args)
				       (unquote-get arg ((fm fm) ... (fi id) ...))
				       (if (null? (cdr args))
					   (unquote-set! arg (car args) (fm ...) (fi ...))
					   safe-name))))
			       #;*%lambda-object%*))
		 (key-lambda () (r ...) (o ...)
			     (let* (a ...)
			       (make <lambda-object>
				 :procedure
				 (lambda (arg . args)
				   (if (null? args)
				       (unquote-get arg ((fm fm) ... (fi id) ...))
				       (if (null? (cdr args))
					   (unquote-set! arg (car args) (fm ...) (fi ...))
					   safe-name))))
			       #;*%lambda-object%*)))))
       (define make-object (car makers))
       (define make-object-by-name (cadr makers))
       ;; The predicate procedure is implementation dependant.
       (define (pred-object object)
	 (and ;;(eq? '*%lambda-object%* (object-name object)) ;mzscheme
	  (is-a? object <lambda-object>)
	  (let ((group (object #f #f #f)))
	    (or (eq? safe-name group)
		(let lp ((group-list (group 'parent)))
		  (if (null? group-list)
		      #f
		      (or (eq? safe-name (car group-list))
			  (lp ((car group-list) 'parent))
			  (lp (cdr group-list)))))))))
       (define name
	 (let ((parent safe-parent)
	       (constructor makers)
	       (predicate pred-object)
	       (read-write-field '(fm ...))
	       (read-only-field '(fi ...))
	       (required-field '(r ...))
	       (optional-field '(o ...))
	       (automatic-field '((c cd) ... (v vd) ... a ...))
	       (common-field '((c cd) ...))
	       (virtual-field '((v vd) ...))
	       (hidden-field '((h hd) ...)))
	   (lambda (symbol)
	     (unquote-get* symbol (parent constructor predicate
					  read-write-field read-only-field
					  required-field optional-field
					  automatic-field common-field
					  virtual-field hidden-field)))))
       (define tmp (set! safe-name name))))))

(define-syntax define-make-object
  (lambda (x)
    (syntax-case x ()
      ((_ nm gr gi fm fi r o a c v h)
       (let ((name (syntax->datum #'nm)))
	 (let ((make-obj (string->symbol (string-append "make-" (symbol->string name))))
	       (make-obj-by-name (string->symbol (string-append "make-" (symbol->string name) "-by-name")))
	       (pred-obj (string->symbol (string-append (symbol->string name) "?"))))
	   (with-syntax
	       ((make-object (datum->syntax #'nm make-obj))
		(make-object-by-name (datum->syntax #'nm make-obj-by-name))
		(pred-object (datum->syntax #'nm pred-obj)))
	     #'(define-object nm make-object make-object-by-name pred-object gr gi fm fi r o a c v h))))))))

(define-syntax field-sort
  (syntax-rules (quote unquote quasiquote)
    ((field-sort gr gi (fm ...) fi r o a (c ...) v h (((,,n) d) . e))
     (field-sort gr gi (fm ... n) fi r o a (c ... (n d)) v h e))
    ((field-sort gr gi fm (fi ...) r o a (c ...) v h ((,,n d) . e))
     (field-sort gr gi fm (fi ... (n n)) r o a (c ... (n d)) v h e))
    ((field-sort gr gi fm fi r o (a ...) c v (h ...) ((',n d) . e))
     (field-sort gr gi fm fi r o (a ... (n d)) c v (h ... (n d)) e))
    ((field-sort gr gi fm (fi ...) r o a c (v ...) h ((`,n d) . e))
     (field-sort gr gi fm (fi ... (n d)) r o a c (v ... (n d)) h e))
    ((field-sort gr gi (fm ...) fi r o (a ...) c v h (((,n) d) . e))
     (field-sort gr gi (fm ... n) fi r o (a ... (n d)) c v h e))
    ((field-sort gr gi fm (fi ...) r o (a ...) c v h ((,n d) . e))
     (field-sort gr gi fm (fi ... (n n)) r o (a ... (n d)) c v h e))
    ((field-sort gr gi fm fi r (o ...) () () () (h ...) (('n d) . e))
     (field-sort gr gi fm fi r (o ... (n d)) () () () (h ... (n d)) e))
    ((field-sort gr gi (fm ...) fi r (o ...) () () () h (((n) d) . e))
     (field-sort gr gi (fm ... n) fi r (o ... (n d)) () () () h e))
    ((field-sort gr gi fm (fi ...) r (o ...) () () () h ((n d) . e))
     (field-sort gr gi fm (fi ... (n n)) r (o ... (n d)) () () () h e))
    ((field-sort gr gi (fm ...) fi (r ...) () () () () () ((n) . e))
     (field-sort gr gi (fm ... n) fi (r ... n) () () () () () e))
    ((field-sort gr gi fm (fi ...) (r ...) () () () () () (n . e))
     (field-sort gr gi fm (fi ... (n n)) (r ... n) () () () () () e))
    ((field-sort gr (name gi ...) fm fi r o a c v h ())
     (define-make-object name gr (gi ...) fm fi r o a c v h))))

(define-syntax group-sort
  (syntax-rules ()
    ((group-sort (gr ...) (gi ...) ((g) gg ...) f)
     (group-sort (gr ... g) (gi ...) (gg ...) f))
    ((group-sort (gr ...) (gi ...)  (g gg ...) f)
     (group-sort (gr ...) (gi ... g) (gg ...) f))
    ((group-sort () () g f)
     (group-sort () (g) () f))
    ((group-sort gr gi () f)
     (field-sort gr gi () () () () () () () () f))))

(define-syntax define-lambda-object
  (syntax-rules ()
    ((define-lambda-object g . f)
     (group-sort () () g f))))

)
