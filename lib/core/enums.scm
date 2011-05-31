;; -*- scheme -*-
#!core
(library (core enums helper)
    (export :all)
    (import (core)
	    (core struct)
	    (sagittarius))

  (define-struct <enum-type>
    (make-enum-type universe indexer)
    enum-type?
    (lambda (i p)
      (format p "#<enum-type ~a>" (enum-type-members i)))
    (universe enum-type-universe)
    (indexer enum-type-indexer))

  (define-struct <enum-set>
    (make-enum-set type members)
    enum-set?
    (lambda (i p)
      (format p "#<enum-set ~a>" (enum-set-members i)))
    (type enum-set-type)
    (members enum-set-members))
)

(library (core enums)
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
	    (core base)
	    (core struct)
	    (core syntax)
	    (core syntax-rules)
	    (core enums helper))

  ;; from mosh
  (define (make-enumeration-type symbol-list)
    (let ([ht (make-eq-hashtable)])
      (let loop ([symbol-list symbol-list]
		 [i 0])
	(if (null? symbol-list)
	    '()
	    (begin (hashtable-set! ht (car symbol-list) i)
		   (loop (cdr symbol-list) (+ i 1)))))
      (make-enum-type symbol-list
		      (lambda (symbol)
			(hashtable-ref ht symbol #f)))))


  (define (make-enumeration symbol-list)
    (cond
     [(and (list? symbol-list) (for-all symbol? symbol-list))
      (make-enum-set (make-enumeration-type symbol-list) symbol-list)]
     [else
      (assertion-violation 'make-enumeration "argument 1 must be a list of symbols")]))


  (define (enum-set-universe enum-set)
    (make-enum-set (enum-set-type enum-set)
		   (enum-type-universe (enum-set-type enum-set))))

  (define (enum-set-indexer enum-set)
    (enum-type-indexer (enum-set-type enum-set)))

  (define (enum-set-constructor enum-set)
    (lambda (symbol-list)
      (let ([universe (enum-type-universe (enum-set-type enum-set))])
	(if (for-all (lambda (x) (memq x universe)) symbol-list)
	    (make-enum-set (enum-set-type enum-set) symbol-list)
	    (assertion-violation 'enum-set-constructor "the symbol list must all belong to the universe." universe symbol-list)))))

  (define (enum-set->list enum-set)
    (let ([universe (enum-type-universe (enum-set-type enum-set))]
	  [members (enum-set-members enum-set)])
      (let loop ([universe universe])
	(cond
	 [(null? universe) '()]
	 [(memq (car universe) members)
	  (cons (car universe) (loop (cdr universe)))]
	 [else
	  (loop (cdr universe))]))))

  (define (enum-set-member? symbol enum-set)
    (and (memq symbol (enum-set-members enum-set)) #t))

  (define (enum-set-subset? enum-set1 enum-set2)
    (and
     (let ([enum-set2-univese (enum-set->list (enum-set-universe enum-set2))])
       (for-all
	(lambda (symbol) (memq symbol enum-set2-univese))
	(enum-set->list (enum-set-universe enum-set1))))
     (for-all
      (lambda (symbol) (enum-set-member? symbol enum-set2))
      (enum-set-members enum-set1))))

  (define (enum-set=? enum-set1 enum-set2)
    (and (enum-set-subset? enum-set1 enum-set2)
	 (enum-set-subset? enum-set2 enum-set1)))

  (define (enum-set-union enum-set1 enum-set2)
    (define (union lst1 lst2)
      (let loop ([ret lst1]
		 [lst lst2])
	(cond
	 [(null? lst) ret]
	 [(memq (car lst) ret)
	  (loop ret (cdr lst))]
	 [else
	  (loop (cons (car lst) ret) (cdr lst))])))
    (if (eq? (enum-set-type enum-set1) (enum-set-type enum-set2))
	(make-enum-set (enum-set-type enum-set1)
		       (union (enum-set-members enum-set1) (enum-set-members enum-set2)))
	(assertion-violation 'enum-set-union "enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.")))

  (define (enum-set-intersection enum-set1 enum-set2)
    (define (intersection lst1 lst2)
      (let loop ([ret '()]
		 [lst lst1])
	(if (null? lst)
	    ret
	    (cond
	     [(memq (car lst) lst2)
	      (loop (cons (car lst) ret) (cdr lst))]
	     [else
	      (loop ret (cdr lst))]))))
    (if (eq? (enum-set-type enum-set1) (enum-set-type enum-set2))
	(make-enum-set (enum-set-type enum-set1)
		       (intersection (enum-set-members enum-set1) (enum-set-members enum-set2)))
	(assertion-violation 'enum-set-intersection "enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.")))

  (define (enum-set-difference enum-set1 enum-set2)
    (define (difference lst1 lst2)
      (let loop ([ret '()]
		 [lst lst1])
	(if (null? lst)
	    ret
	    (cond
	     [(memq (car lst) lst2)
	      (loop ret (cdr lst))]
	     [else
	      (loop (cons (car lst) ret) (cdr lst))]))))
    (if (eq? (enum-set-type enum-set1) (enum-set-type enum-set2))
	(make-enum-set (enum-set-type enum-set1)
		       (difference (enum-set-members enum-set1) (enum-set-members enum-set2)))
	(assertion-violation 'enum-set-difference "enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.")))

  (define (enum-set-complement enum-set)
    (let ([members (enum-set-members enum-set)])
      (make-enum-set (enum-set-type enum-set)
		     (filter (lambda (symbol) (not (memq symbol members))) (enum-type-universe (enum-set-type enum-set))))))

  (define (enum-set-projection enum-set1 enum-set2)
    (if (enum-set-subset? enum-set1 enum-set2)
	enum-set1
	(let ([universe2 (enum-type-universe (enum-set-type enum-set2))]
	      [members1 (enum-set-members enum-set1)])
	  (make-enum-set (enum-set-type enum-set2)
			 (filter (lambda (symbol) (memq symbol universe2)) members1)))))

  (define-syntax defset
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((name (cadr form))
	     (endname (gensym))
	     (endsname (caddr form))
	     (symbols (cdddr form)))
	 `(begin
	    (define ,endname (make-enumeration ',@symbols))
	    (define-syntax ,endsname
	      (syntax-rules ()
		((_ sym1 ...)
		 (begin
		   ((enum-set-constructor ,endname)
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
	       
  #;(define-syntax define-enumeration
    (syntax-rules ()
      ((_ type-name (symbol1 ...) constructor-syntax)
       (begin
         (define constructor (enum-set-constructor (make-enumeration '(symbol1 ...))))
         (define-syntax type-name
           (lambda (x)
             (syntax-case x ()
               ((_ symbol2)
                (or (memq (syntax->datum (syntax symbol2)) '(symbol1 ...))
                    (syntax-violation 'type-name "excpectd symbols which belong to the universe" x))
                (syntax 'symbol2)))))
         (define-syntax constructor-syntax
           (lambda (x)
             (syntax-case x ()
               ((_ symbol3 (... ...))
                (or (for-all (lambda (e) (memq e '(symbol1 ...)))
                             (syntax->datum (syntax (symbol3 (... ...)))))
                    (syntax-violation 'constructor-syntax "excpectd symbols which belong to the universe" x))
                (syntax (constructor '(symbol3 (... ...))))))))))))
) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
