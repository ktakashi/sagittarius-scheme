;; -*- scheme -*-
#!core
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
	    ;;(for (core struct) expand)
	    ;;(for (core syntax-rules) expand)
	    (sagittarius))

  ;; use record api directory
  (define <enum-type>
    (let* ((rtd (make-record-type-descriptor '<enum-type> #f #f #f #f
					     (vector '(immutable universe)
						     '(immutable indexer))))
	   (rcd (make-record-constructor-descriptor rtd #f #f)))
      (make-record-type '<enum-type> rtd rcd)))
  (define make-enum-type (record-constructor (record-type-rcd <enum-type>)))
  (define enum-type? (record-predicate (record-type-rtd <enum-type>)))
  (define enum-type-universe (record-accessor (record-type-rtd <enum-type>) 0))
  (define enum-type-indexer (record-accessor (record-type-rtd <enum-type>) 1))

  (define <enum-set>
    (let* ((rtd (make-record-type-descriptor '<enum-set> #f #f #f #f
					     (vector '(immutable type)
						     '(immutable members))))
	   (rcd (make-record-constructor-descriptor rtd #f #f)))
      (make-record-type '<enum-set> rtd rcd)))
  (define make-enum-set (record-constructor (record-type-rcd <enum-set>)))
  (define enum-set? (record-predicate (record-type-rtd <enum-set>)))
  (define enum-set-type (record-accessor (record-type-rtd <enum-set>) 0))
  (define enum-set-members (record-accessor (record-type-rtd <enum-set>) 1))

  #;(define-struct <enum-type>
    (make-enum-type universe indexer)
    enum-type?
    (lambda (i p)
      (format p "#<enum-type ~a>" (enum-type-members i)))
    (universe enum-type-universe)
    (indexer enum-type-indexer))

  #;(define-struct <enum-set>
    (make-enum-set type members)
    enum-set?
    (lambda (i p)
      (format p "#<enum-set ~a>" (enum-set-members i)))
    (type enum-set-type)
    (members enum-set-members))

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
) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
