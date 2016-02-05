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
	    (core syntax))

  (define-syntax define-enumeration
    (syntax-rules ()
      ((_ name symbols ctr)
       (begin
	 (define-syntax name
	   (lambda (x)
	     (syntax-case x ()
	       ((_ y)
		(let ((sym1 (syntax->datum #'y)))
		  (if (memq sym1 'symbols)
		      #''y
		      (syntax-violation 'name "illigal symbol" 
					(syntax->datum (car x)))))))))
	 ;; procedural constructor (invisible)
	 (define endname (make-enumeration 'symbols))
	 ;; actual constructor
	 (define-syntax ctr
	   (syntax-rules ()
	     ((_ sym1 (... ...))
	      ((enum-set-constructor endname) 
	       (list (name sym1) (... ...))))))))))
  

) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
