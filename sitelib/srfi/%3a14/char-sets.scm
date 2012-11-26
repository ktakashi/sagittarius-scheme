;;; -*- coding:utf-8; -*-
;;;
;;; char-set.scm - SRFI-14 character set library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

#!compatible
#!nobacktrace
(library (srfi :14 char-sets)
  (export
   ;; predicates & comparison
   char-set? char-set= char-set<= char-set-hash
   ;; iterating over character sets
   char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
   char-set-fold char-set-unfold char-set-unfold!
   char-set-for-each char-set-map
   ;; creating character sets
   char-set-copy char-set
   list->char-set string->char-set
   list->char-set! string->char-set!
   char-set-filter ucs-range->char-set
   char-set-filter! ucs-range->char-set!
   ->char-set
   ;; querying character sets
   char-set->list char-set->string
   char-set-size char-set-count char-set-contains?
   char-set-every char-set-any
   ;; character-set algebra
   char-set-adjoin char-set-delete
   char-set-adjoin! char-set-delete!
   char-set-complement char-set-union char-set-intersection
   char-set-complement! char-set-union! char-set-intersection!
   char-set-difference char-set-xor char-set-diff+intersection
   char-set-difference! char-set-xor! char-set-diff+intersection!
   ;; standard character sets
   char-set:lower-case char-set:upper-case char-set:title-case
   char-set:letter char-set:digit char-set:letter+digit
   char-set:graphic char-set:printing char-set:whitespace
   char-set:iso-control char-set:punctuation char-set:symbol
   char-set:hex-digit char-set:blank char-set:ascii
   char-set:empty char-set:full)
  (import (rnrs)
	  (rnrs r5rs)
	  (sagittarius)
	  (sagittarius control))

;;; Exports:
;;; char-set? char-set= char-set<=
;;; char-set-hash 
;;; char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
;;; char-set-fold char-set-unfold char-set-unfold!
;;; char-set-for-each char-set-map
;;; char-set-copy char-set
;;;
;;; list->char-set  string->char-set 
;;; list->char-set! string->char-set! 
;;;
;;; filterchar-set  ucs-range->char-set  ->char-set
;;; filterchar-set! ucs-range->char-set!
;;;
;;; char-set->list char-set->string
;;;
;;; char-set-size char-set-count char-set-contains?
;;; char-set-every char-set-any
;;;
;;; char-set-adjoin  char-set-delete 
;;; char-set-adjoin! char-set-delete!
;;; 

;;; char-set-complement  char-set-union  char-set-intersection  
;;; char-set-complement! char-set-union! char-set-intersection! 
;;;
;;; char-set-difference  char-set-xor  char-set-diff+intersection
;;; char-set-difference! char-set-xor! char-set-diff+intersection!
;;;
;;; char-set:lower-case		char-set:upper-case	char-set:title-case
;;; char-set:letter		char-set:digit		char-set:letter+digit
;;; char-set:graphic		char-set:printing	char-set:whitespace
;;; char-set:iso-control	char-set:punctuation	char-set:symbol
;;; char-set:hex-digit		char-set:blank		char-set:ascii
;;; char-set:empty		char-set:full

  ;; from Gauche start

  ;; compariosn
  (define (char-set= . args)
    (cond ((null? args) #t)
	  ((null? (cdr args)) #t)
	  (else (let loop ((cs (car args))
			   (rest (cdr args)))
		  (cond ((null? rest) #t)
			((%char-set-equal? cs (car rest))
			 (loop cs (cdr rest)))
			(else #f))))))

  (define (char-set<= . args)
    (cond ((null? args) #t)
	  ((null? (cdr args)) #t)
	  (else (let loop ((cs (car args))
			   (rest (cdr args)))
		  (cond ((null? rest) #t)
			((%char-set<=? cs (car rest))
			 (loop cs (cdr rest)))
			(else #f))))))

  (define (char-set-hash cs . args)
    (let ((bound (if (pair? args) (car args) #x1fffffff)))
      (let loop ((ranges (%char-set-ranges cs))
		 (value 0))
	(if (null? ranges)
	    value
	    (loop (cdr ranges)
		  (modulo (+ value (caar ranges) (cdar ranges)) bound))))))
  
  ;; iteration
  (define (char-set-cursor cs)
    (let ((ranges (%char-set-ranges cs)))
      (if (null? ranges) #f) (cons (caar ranges) ranges)))

  (define (char-set-ref cs cursor)
    (if (and (pair? cursor) (integer? (car cursor)))
	(integer->char (car cursor))
	(assertion-violation 'char-set-ref
			     "bad character-set cursor" cursor)))

  (define (char-set-cursor-next cs cursor)
    (if (pair? cursor)
	(let ((code (car cursor))
	      (range (cadr cursor)))
	  (cond ((< code (cdr range)) (cons (+ code 1) (cdr cursor)))
		((null? (cddr cursor)) #f)
		(else (cons (caaddr cursor) (cddr cursor)))))
	(assertion-violation 'char-set-ref
			     "bad character-set cursor" cursor)))

  (define (end-of-char-set? cursor) (not cursor))

  ;; functional ops
  (define (char-set-fold kons knil cs)
    (let loop ((cursor (char-set-cursor cs))
	       (result knil))
      (if (end-of-char-set? cursor)
	  result
	  (loop (char-set-cursor-next cs cursor)
		(kons (char-set-ref cs cursor) result)))))
  
  (define (char-set-unfold! pred fun gen seed base)
    (let loop ((seed seed))
      (if (pred seed)
	  base
	  (let ((c (fun seed)))
	    (%char-set-add-range! base c c)
	    (loop (gen seed))))))

  (define (char-set-unfold pred fun gen seed . args)
    (let ((base (if (pair? args) (char-set-copy (car args)) (char-set))))
      (char-set-unfold! pred fun gen seed base)))

  (define (char-set-for-each proc cs)
    (let loop ((cursor (char-set-cursor cs)))
      (unless (end-of-char-set? cursor)
	(proc (char-set-ref cs cursor))
	(loop (char-set-cursor-next cs cursor)))))

  (define (char-set-map proc cs)
    (let ((new (char-set)))
      (let loop ((cursor (char-set-cursor cs)))
	(if (end-of-char-set? cursor)
	    new
	    (let ((c (proc (char-set-ref cs cursor))))
	      (%char-set-add-range! new c c)
	      (loop (char-set-cursor-next cs cursor)))))))

  ;; construction

  ;; char-set-copy native
  ;; char-set native

  (define (list->char-set chars . args)
    (let ((base (if (null? args) (char-set) (char-set-copy (car args)))))
      (%char-set-add-chars! base chars)))

  (define (list->char-set! chars base)
      (%char-set-add-chars! base chars))

  (define (string->char-set str . args)
    (let ((base (if (null? args) (char-set) (char-set-copy (car args)))))
      (%char-set-add-chars! base (string->list str))))

  (define (string->char-set! str base)
    (%char-set-add-chars! base (string->list str)))

  (define (char-set-filter pred cs . args)
    (let ((base (if (null? args) (char-set) (char-set-copy (car args)))))
      (char-set-filter! pred cs base)))

  (define (char-set-filter! pred cs base)
    (let loop ((cursor (char-set-cursor cs)))
      (if (end-of-char-set? cursor)
	  base
	  (let ((c (char-set-ref cs cursor)))
	    (when (pred c) (%char-set-add-range! base c c))
	    (loop (char-set-cursor-next cs cursor))))))

  (define (ucs-range->char-set low upper . args)
    (let ((error? (and (pair? args) (car args)))
	  (base (if (and (pair? args) (pair? (cdr args)))
		    (char-set-copy (cadr args))
		    (char-set))))
      (ucs-range->char-set! low upper error? base)))

  (define (ucs-range->char-set! low upper error? base)
    (when (< low 0)
      (if error?
	  (assertion-violation 'ucs-range->char-set!
			       "argument out of range" low)
	  (set! low 0)))
    (when (> upper (+ *char-code-max* 1))
      (if error?
	  (assertion-violation 'ucs-range->char-set!
			       "argument out of range" upper)
	  (set! upper (+ *char-code-max* 1))))
    (%char-set-add-range! base low (- upper 1)))
  
  (define (->char-set obj)
    (cond ((list? obj) (list->char-set obj))
	  ((string? obj) (string->char-set obj))
	  ((char-set? obj) obj)
	  ((char? obj) (char-set obj))
	  (else
	   (assertion-violation '->char-set
				(format "cannot coerse ~s into a char-set"
					obj)
				obj))))

  ;; query
  (define (char-set-size cs)
    (let ((count 0))
      (for-each (lambda (range)
		  (set! count (+ count (- (cdr range) (car range)) 1)))
		(%char-set-ranges cs))
      count))

  (define (char-set-count pred cs)
    (char-set-fold (lambda (c r) (if (pred c) (+ r 1) r)) 0 cs))

  (define (char-set->list cs)
    (char-set-fold cons '() cs))

  (define (char-set->string cs)
    (list->string (char-set-fold cons '() cs)))

  ;; char-set-contains? native
  (define (char-set-every pred cs)
    (let loop ((cursor (char-set-cursor cs)))
      (cond ((end-of-char-set? cursor))
	    ((pred (char-set-ref cs cursor))
	     (loop (char-set-cursor-next cs cursor)))
	    (else #f))))

  (define (char-set-any pred cs)
    (let loop ((cursor (char-set-cursor cs)))
      (cond ((end-of-char-set? cursor) #f)
	    ((pred (char-set-ref cs cursor)) #t)
	    (else (loop (char-set-cursor-next cs cursor))))))

  ;; sets
  ;; auxiliary functions for range operation other than adjoin and union.
  (define (%char-set-sub-range ranges from to)
    (let loop ((ranges ranges)
	       (result '()))
      (if (null? ranges)
	  (reverse result)
	  (let ((lo (caar ranges))
		(hi (cdar ranges))
		(next (cdr ranges)))
	    (if (<= lo from hi)
		(if (<= lo to hi)
		    (if (= lo from)
			(if (= to hi)
			    (loop next result)
			    (loop next (acons (+ to 1) hi result)))
			(if (= to hi)
			    (loop next (acons lo (- from 1) result))
			    (loop next (cons* (cons (+ to 1) hi)
					      (cons lo (- from 1))
					      result))))
		    (if (= lo from)
			(loop next result)
			(loop next (acons lo (- from 1) result))))
		(if (<= lo to hi)
		    (if (= to hi)
			(loop next result)
			(loop next (acons (+ to 1) hi result)))
		    (if (and (< from lo) (< hi to))
			(loop next result)
			(loop next (acons lo hi result)))))
	    ))
      ))

  (define (%char-set-sub-chars range chars)
    (let loop ((range range)
	       (chars chars))
      (if (null? chars)
	  range
	  (let ((code (char->integer (car chars))))
	    (loop (%char-set-sub-range range code code) (cdr chars))))))

  (define (%ranges->char-set ranges)
    (let ((base (char-set)))
      (for-each (lambda (range)
		  (%char-set-add-range! base (car range) (cdr range)))
		ranges)
      base))
  
  (define (char-set-adjoin base . chars)
    (if (null? chars) base (%char-set-add-chars! (char-set-copy base) chars)))

  (define (char-set-adjoin! base . chars)
    (%char-set-add-chars! base chars))

  (define (char-set-delete base . chars)
    (%ranges->char-set (%char-set-sub-chars (%char-set-ranges base) chars)))

  (define char-set-delete! char-set-delete)

  (define (char-set-complement cs)
    (%char-set-complement! (char-set-copy cs)))

  (define (char-set-complement! cs) (%char-set-complement! cs))

  (define (char-set-union . charsets)
    (if (null? charsets)
	char-set:empty
	(let loop ((base (char-set-copy (car charsets)))
		   (rest (cdr charsets)))
	  (if (null? rest)
	      base
	      (loop (%char-set-add! base (car rest)) (cdr rest))))))

  (define (char-set-union! base . charsets)
    (if (null? charsets)
	base
	(let loop ((base base) (rest charsets))
	  (if (null? rest)
	      base
	      (loop (%char-set-add! base (car rest)) (cdr rest))))))

  (define (%char-set-intersection2 x y)
    (char-set-difference x (char-set-complement y)))

  (define (char-set-intersection . charsets)
    (cond ((null? charsets) char-set:full)
	  ((null? (cdr charsets)) (car charsets))
	  (else (apply char-set-intersection
		       (%char-set-intersection2 (car charsets) (cadr charsets))
		       (cddr charsets)))))

  (define (char-set-intersection! base . charsets)
    (if (null? charsets)
	base
	(apply char-set-intersection base charsets)))

  (define (char-set-difference base . charsets)
    (define (char-range-sub2 ranges sub)
      (let loop ((ranges ranges) (sub sub))
	(if (null? sub)
	    ranges
	    (loop (%char-set-sub-range ranges (caar sub) (cdar sub))
		  (cdr sub)))))

    (let loop ((ranges (%char-set-ranges base))
	       (sets charsets))
      (if (null? sets)
	  (%ranges->char-set ranges)
	  (loop (char-range-sub2 ranges (%char-set-ranges (car sets)))
		(cdr sets))))
    )
  (define char-set-difference! char-set-difference)

  (define (%char-set-xor2 x y)
    (char-set-difference (char-set-union x y)
			 (%char-set-intersection2 x y)))

  (define (char-set-xor . charsets)
    (cond ((null? charsets) char-set:empty)
	  ((null? (cdr charsets)) (car charsets))
	  (else (apply char-set-xor
		       (%char-set-xor2 (car charsets) (cadr charsets))
		       (cddr charsets)))))

  (define (char-set-xor! base . charsets)
    (if (null? charsets)
	base
	(apply char-set-xor base charsets)))

  (define (char-set-diff+intersection base . charsets)
    (values (apply char-set-difference base charsets)
	    (char-set-intersection base (apply char-set-union charsets))))

  (define (char-set-diff+intersection! cs1 cs2 . charsets)
    (values (apply char-set-difference! cs1 cs2 charsets)
	    (char-set-intersection! cs1 (apply char-set-union! cs2 charsets))))
  ;; from Gauche end.

) ;; [end]