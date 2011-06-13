;; -*- scheme -*-
(library (rnrs enums (6))
    (export make-enumeration
	    enum-set-universe
	    enum-set-indexer
	    enum-set-constructor
	    enum-set->list
	    enum-set-member?
	    enum-set-subset?
	    enum-set=?
	    enum-set-union
	    enum-set-intersection
	    enum-set-difference
	    enum-set-complement
	    enum-set-projection
	    define-enumeration)
    (import (core)
	    (core enums)
	    (core base)
	    (core syntax)
	    (sagittarius))

    ;; we use er-macro-transformer to implement enumeration
  (define-syntax defset
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((name (cadr form))
	     (endname (gensym))
	     (endsname (caddr form))
	     (symbols (cdddr form))
	     (_begin (rename 'begin)) (_define (rename 'define))
	     (_define-syntax (rename 'define-syntax))
	     (_syntax-rules (rename 'syntax-rules))
	     (_make-enumeration (rename 'make-enumeration))
	     (_enum-set-constructor (rename 'enum-set-constructor)))
	 `(,_begin
	    (,_define ,endname (,_make-enumeration ',@symbols))
	    (,_define-syntax ,endsname
	      (,_syntax-rules ()
		((_ sym1 ...)
		 (,_begin
		   ((,_enum-set-constructor ,endname)
		    (list (,name sym1) ...)))))))))))

  (define-syntax define-enumeration
    (syntax-rules ()
      ((_ name symbols ctr)
       (begin
	 (define-syntax name
	   (lambda (x)
	     (define (err)
	       (syntax-violation 'name "illigal symbol" (unwrap-syntax (car x))))
	     (syntax-case x ()
	       ((_ y)
		(let ((sym1 (syntax->datum #'y)))
		  (if (memq sym1 'symbols)
		      #''y
		      (err)))))))
	 (defset name ctr symbols)))))
  

) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
