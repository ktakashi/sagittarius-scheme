;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; bytevector.scm - bytevector utility
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

(library (util bytevector)
    (export bytevector-xor
	    bytevector-xor!
	    bytevector-ior
	    bytevector-ior!
	    bytevector-and
	    bytevector-and!
	    bytevector-slices
	    bytevector-split-at*
	    ;; parity stuff
	    ->odd-parity
	    ->odd-parity!

	    ;; extra comparison
	    bytevector<?
	    bytevector>?
	    bytevector<=?
	    bytevector>=?

	    bytevector->hex-string
	    hex-string->bytevector
	    bytevector-reverse!
	    bytevector-reverse
	    ;; inspired by srfi 13
	    ;; utility
	    u8? u8-set? u8-set-contains?
	    string->u8-set char-set->u8-set

	    ;; do you want map and for-each as well?
	    bytevector-fold bytevector-fold-right

	    ;; cutting & pasting bytevectors
	    bytevector-take bytevector-take-right
	    bytevector-drop bytevector-drop-right
	    bytevector-trim bytevector-trim-right bytevector-trim-both
	    bytevector-pad bytevector-pad-right

	    ;; prefixes & suffixes
	    bytevector-prefix-length bytevector-suffix-length
	    bytevector-prefix? bytevector-suffix?
	    ;; searching
	    bytevector-index bytevector-index-right
	    bytevector-skip  bytevector-skip-right
	    bytevector-contains
	    ;; miscellaneous: insertion, parsing
	    bytevector-replace bytevector-tokenize
	    ;; filtering & deleting
	    bytevector-filter bytevector-delete

	    ;; others
	    align-bytevectors
	    align-bytevectors!
	    )
    (import (rnrs)
	    (rnrs mutable-strings)
	    (sagittarius)
	    (sagittarius control)
	    (srfi :1 lists)
	    (only (srfi :13 strings) make-kmp-restart-vector)
	    (srfi :14 char-sets)
	    (srfi :26 cut))
(define (process-bytevector! op out . bvs)
  (let ((len (apply min (map bytevector-length bvs))))
    (dotimes (i len)
      (bytevector-u8-set! out i
			  (apply op
				 (map (^(bv) (bytevector-u8-ref bv i))
				      bvs))))
    out))

(define (bytevector-xor! out . bvs)
  (apply process-bytevector! bitwise-xor out bvs))

(define (bytevector-xor . bvs)
  (let* ((len (apply min (map bytevector-length bvs)))
	 (out (make-bytevector len 0)))
    (apply bytevector-xor! out bvs)))

(define (bytevector-ior! out . bvs)
  (apply process-bytevector! bitwise-ior out bvs))

(define (bytevector-ior . bvs)
  (let* ((len (apply min (map bytevector-length bvs)))
	 (out (make-bytevector len 0)))
    (apply bytevector-ior! out bvs)))

(define (bytevector-and! out . bvs)
  (apply process-bytevector! bitwise-and out bvs))

(define (bytevector-and . bvs)
  (let* ((len (apply min (map bytevector-length bvs)))
	 (out (make-bytevector len 0)))
    (apply bytevector-and! out bvs)))

(define (->odd-parity bv . args)
  (apply ->odd-parity! (bytevector-copy bv) args))

(define (->odd-parity! bv :optional (start 0) (end (bytevector-length bv)))
  (define (fixup b)
    (let ((parity (bitwise-bit-count b)))
      (if (even? parity)
	  (if (even? b)
	      (bitwise-ior b #x01)
	      (bitwise-and b #xFE))
	  b)))
  (do ((i start (+ i 1)))
      ((= i end) bv)
    (bytevector-u8-set! bv i (fixup (bytevector-u8-ref bv i)))))

;; analogy of slices and split-at* in (util list)
(define (bytevector-slices bv k . args)
  (unless (and (integer? k) (positive? k))
    (assertion-violation 'bytevector-slices "index must be positive integer" k))
  (let1 len (bytevector-length bv)
    (let loop ((bv bv) (r '()) (i 0))
      (if (< i len)
	  (let-values (((h t) (apply bytevector-split-at* bv k args)))
	    (loop t (cons h r) (+ i k)))
	  (reverse! r)))))

(define (bytevector-split-at* bv k :key (padding #f))
  (unless (and (integer? k) (not (negative? k)))
    (assertion-violation 'bytevector-split-at* 
			 "index must be non-negative integer" k))
  (let1 len (bytevector-length bv)
    (if (< k len)
	(let ((r1 (bytevector-copy bv 0 k))
	      (r2 (bytevector-copy bv k)))
	  (values r1 r2))
	(values (if padding (padding bv) bv) #vu8()))))

(define (bytevector->hex-string bv :key (upper? #t))
;; this is not efficient...
;;   (define (fixup x)
;;     (if (= (string-length x) 1)
;; 	(string-append "0" x)
;; 	x))
;;   (string-concatenate (map (lambda (u8) (fixup (number->string u8 16)))
;; 			   (bytevector->u8-list bv)))
;;   (let-values (((out extract) (open-string-output-port)))
;;     (let ((fmt (if capital? "~2,'0X" "~2,'0x")))
;;       (dotimes (i (bytevector-length bv))
;; 	(format out fmt (bytevector-u8-ref bv i))))
;;     (extract))
  ;; this is the fastest
  (define (hex->char i)
    (cond ((< i 10) (integer->char (+ i 48)))   ;; + #\0
	  (upper?   (integer->char (+ i 55)))   ;; + #\A - 10
	  (else     (integer->char (+ i 87))))) ;; + #\a - 10
  (let* ((len (bytevector-length bv))
	 (str (make-string (* len 2))))
    (dotimes (i len str)
      (let* ((b (bytevector-u8-ref bv i))
	     (hi (bitwise-arithmetic-shift (bitwise-and b #xF0) -4))
	     (lo (bitwise-and b #x0F)))
	(string-set! str (* i 2) (hex->char hi))
	(string-set! str (+ (* i 2) 1) (hex->char lo))))))
(define (hex-string->bytevector str)
  ;; make the same as following
  ;; (integer->bytevector (string->number str 16))
  ;; so it needs to handle odd length string as well
  ;; thus "123" would be #vu8(#x01 #x23)
  (define (safe-ref s i)
    (if (< i 0) #\0 (string-ref s i)))
  (define (->hex c)
    (if (char-set-contains? char-set:hex-digit c)
	(or (digit-value c) ;; easy
	    (let ((c (char-upcase c)))
	      ;; must be A-F
	      (- (char->integer c) #x37)))
	(assertion-violation 'hex-string->bytevector "non hex character" c str)))
  (let* ((len (string-length str))
	 (bv (make-bytevector (ceiling (/ len 2)))))
    (let loop ((i (- (bytevector-length bv) 1)) (j (- len 1)))
      (if (< i 0)
	  bv
	  (let ((h (->hex (safe-ref str (- j 1))))
		(l (->hex (safe-ref str j))))
	    (bytevector-u8-set! bv i 
	      (bitwise-ior (bitwise-arithmetic-shift h 4) l))
	    (loop (- i 1) (- j 2)))))))
  
;; srfi 13 things
;; helper
(define (u8? n) (and (integer? n) (<= 0 n #xFF)))
(define (u8-set? o) 
  (and (pair? o)
       (let loop ((l o))
	 (or (null? l)
	     (and (u8? (car l)) (loop (cdr l)))))))
(define (string->u8-set s) (map char->integer (string->list s)))
(define (char-set->u8-set cset)
  (map char->integer
       (char-set->list (char-set-intersection cset char-set:ascii))))

(define (bytevector-take bv n) (bytevector-copy bv 0 n))
(define (bytevector-take-right bv n)
  (let ((len (bytevector-length bv)))
    (bytevector-copy bv (- len n) len)))
(define (bytevector-drop bv n)
  (let ((len (bytevector-length bv)))
    (bytevector-copy bv n len)))
(define (bytevector-drop-right bv n)
  (let ((len (bytevector-length bv)))
    (bytevector-copy bv 0 (- len n))))

(define u8-set:whitespace (string->u8-set " \r\f\v\n\t"))
(define (bytevector-trim bv
	 :optional (criterion u8-set:whitespace)
		   (start 0) (end (bytevector-length bv)))
  (cond ((bytevector-skip bv criterion start end) =>
	 (lambda (i) (bytevector-copy bv i end)))
	(else #vu8())))
(define (bytevector-trim-right bv
	 :optional (criterion u8-set:whitespace)
		   (start 0) (end (bytevector-length bv)))
  (cond ((bytevector-skip-right bv criterion start end) =>
	 (lambda (i) (bytevector-copy bv start (+ i 1))))
	(else #vu8())))
(define (bytevector-trim-both bv
	 :optional (criterion u8-set:whitespace)
		   (start 0) (end (bytevector-length bv)))
  (cond ((bytevector-skip bv criterion start end) => 
	 (lambda (i) 
	   (bytevector-copy bv i 
			    (+ (bytevector-skip-right bv criterion i end) 1))))
	(else #vu8())))

(define (bytevector-pad bv n
	  :optional (u8 0) (start 0) (end (bytevector-length bv)))
  (let ((len (- end start)))
    (if (<= n len)
	(bytevector-copy bv (- end n) end)
	(let ((ans (make-bytevector n u8)))
	  (bytevector-copy! bv start ans (- n len) len)
	  ans))))
(define (bytevector-pad-right bv n
	  :optional (u8 0) (start 0) (end (bytevector-length bv)))
  (let ((len (- end start)))
    (if (<= n len)
	(bytevector-copy bv start (+ start n))
	(let ((ans (make-bytevector n u8)))
	  (bytevector-copy! bv start ans 0 len)
	  ans))))

(define (bytevector-prefix-length bv1 bv2
	   :optional (start1 0) (end1 (bytevector-length bv1))
		     (start2 0) (end2 (bytevector-length bv2)))
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))
    (if (and (eq? bv1 bv2) (= start1 start2))	; EQ fast path
	delta
	(let lp ((i start1) (j start2))		; Regular path
	  (if (or (>= i end1)
		  (not (= (bytevector-u8-ref bv1 i)
			  (bytevector-u8-ref bv2 j))))
	      (- i start1)
	      (lp (+ i 1) (+ j 1)))))))

(define (bytevector-prefix? bv1 bv2
	   :optional (start1 0) (end1 (bytevector-length bv1))
		     (start2 0) (end2 (bytevector-length bv2)))
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))
	 (= (bytevector-prefix-length bv1 bv2 start1 end1 start2 end2) len1))))

(define (bytevector-suffix-length bv1 bv2
	   :optional (start1 0) (end1 (bytevector-length bv1))
		     (start2 0) (end2 (bytevector-length bv2)))
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))
    (if (and (eq? bv1 bv2) (= start1 start2))	; EQ fast path
	delta
	(let lp ((i (- end1 1)) (j (- end2 1)))	; Regular path
	  (if (or (< i start1)
		  (not (= (bytevector-u8-ref bv1 i)
			  (bytevector-u8-ref bv2 j))))
	      (- (- end1 i) 1)
	      (lp (- i 1) (- j 1)))))))

(define (bytevector-suffix? bv1 bv2
	   :optional (start1 0) (end1 (bytevector-length bv1))
		     (start2 0) (end2 (bytevector-length bv2)))
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))
	 (= (bytevector-suffix-length bv1 bv2 start1 end1 start2 end2) len1))))

;; search

;; sort of set operation
(define (u8-set-contains? set u8) (memv u8 set))

(define (bytevector-index bv criterion
	  :optional (start 0) (end (bytevector-length bv)))
  (cond ((u8? criterion)
	 (let lp ((i start))
	   (and (< i end)
		(if (= criterion (bytevector-u8-ref bv i)) i
		    (lp (+ i 1))))))
	((u8-set? criterion)
	 (let lp ((i start))
	   (and (< i end)
		(if (u8-set-contains? criterion (bytevector-u8-ref bv i)) i
		    (lp (+ i 1))))))
	((procedure? criterion)
	 (let lp ((i start))
	   (and (< i end)
		(if (criterion (bytevector-u8-ref bv i)) i
		    (lp (+ i 1))))))
	(else 
	 (error 'bytevector-index
		"Second param is neither u8, u8-set or predicate procedure."
		criterion))))

(define (bytevector-index-right bv criterion
	  :optional (start 0) (end (bytevector-length bv)))
  (cond ((u8? criterion)
	 (let lp ((i (- end 1)))
	   (and (>= i start)
		(if (= criterion (bytevector-u8-ref bv i)) i
		    (lp (- i 1))))))
	((u8-set? criterion)
	 (let lp ((i (- end 1)))
	   (and (>= i start)
		(if (u8-set-contains? criterion (bytevector-u8-ref bv i)) i
		    (lp (- i 1))))))
	((procedure? criterion)
	 (let lp ((i (- end 1)))
	   (and (>= i start)
		(if (criterion (bytevector-u8-ref bv i)) i
		    (lp (- i 1))))))
	(else 
	 (error 'bytevector-index-right
		"Second param is neither u8, u8-set or predicate procedure."
		criterion))))

(define (bytevector-skip bv criterion
	  :optional (start 0) (end (bytevector-length bv)))
  (cond ((u8? criterion)
	 (let lp ((i start))
	   (and (< i end)
		(if (= criterion (bytevector-u8-ref bv i))
		    (lp (+ i 1))
		    i))))
	((u8-set? criterion)
	 (let lp ((i start))
	   (and (< i end)
		(if (u8-set-contains? criterion (bytevector-u8-ref bv i))
		    (lp (+ i 1))
		    i))))
	((procedure? criterion)
	 (let lp ((i start))
	   (and (< i end)
		(if (criterion (bytevector-u8-ref bv i))
		    (lp (+ i 1))
		    i))))
	(else 
	 (error 'bytevector-index
		"Second param is neither u8, u8-set or predicate procedure."
		criterion))))

(define (bytevector-skip-right bv criterion
	  :optional (start 0) (end (bytevector-length bv)))
  (cond ((u8? criterion)
	 (let lp ((i (- end 1)))
	   (and (>= i start)
		(if (= criterion (bytevector-u8-ref bv i))
		    (lp (- i 1))
		    i))))
	((u8-set? criterion)
	 (let lp ((i (- end 1)))
	   (and (>= i start)
		(if (u8-set-contains? criterion (bytevector-u8-ref bv i)) 
		    (lp (- i 1))
		    i))))
	((procedure? criterion)
	 (let lp ((i (- end 1)))
	   (and (>= i start)
		(if (criterion (bytevector-u8-ref bv i)) 
		    (lp (- i 1))
		    i))))
	(else 
	 (error 'bytevector-index-right
		"Second param is neither u8, u8-set or predicate procedure."
		criterion))))

;; contains
(define (bytevector-contains bv pattern
	  :optional (b-start 0) (b-end (bytevector-length bv))
		    (p-start 0) (p-end (bytevector-length pattern)))
  (let ((plen (- p-end p-start))
	(rv   (make-kmp-restart-vector pattern = 
				       p-start p-end bytevector-u8-ref)))

    ;; The search loop. TJ & PJ are redundant state.
    (let lp ((ti b-start) (pi 0)
	     (tj (- b-end b-start)) ; (- tlen ti) -- how many chars left.
	     (pj plen))		 ; (- plen pi) -- how many chars left.

      (if (= pi plen)
	  (- ti plen)			; Win.
	  (and (<= pj tj)		; Lose.
	       (if (= (bytevector-u8-ref bv ti) ; Search.
		      (bytevector-u8-ref pattern (+ p-start pi)))
		   (lp (+ 1 ti) (+ 1 pi) (- tj 1) (- pj 1)) ; Advance.
		   
		   (let ((pi (vector-ref rv pi))) ; Retreat.
		     (if (= pi -1)
			 (lp (+ ti 1) 0  (- tj 1) plen) ; Punt.
			 (lp ti       pi tj       (- plen pi))))))))))

;; replace
(define (bytevector-replace bv1 bv2 start1 end1
	  :optional (start2 0) (end2 (bytevector-length bv2)))
  (let* ((bv-len1 (bytevector-length bv1))
	 (sublen (- end2 start2))
	 (alen (+ (- bv-len1 (- end1 start1)) sublen))
	 (ans  (make-bytevector alen)))
    (bytevector-copy! bv1 0 ans 0 start1)
    (bytevector-copy! bv2 start2 ans start1 sublen)
    (bytevector-copy! bv1 end1 ans (+ start1 sublen) (- bv-len1 end1))
    ans))

;; tokenize
(define u8-set:graphics
  (string->u8-set "~}|{zyxwvutsrqponmlkjihgfedcba`_^]\\[ZYXWVUTSRQPONMLKJIHGFEDCBA@?>=<;:9876543210/.-,+*)('&%$#\"!"))
(define (bytevector-tokenize bv
	  :optional (token-set u8-set:graphics)
		    (start 0) (end (bytevector-length bv)))
  (let lp ((i end) (ans '()))
    (cond ((and (< start i) (bytevector-index-right bv token-set start i)) =>
	   (lambda (tend-1)
	     (let ((tend (+ 1 tend-1)))
	       (cond ((bytevector-skip-right bv token-set start tend-1) =>
		      (lambda (tstart-1)
			(lp tstart-1
			    (cons (bytevector-copy bv (+ 1 tstart-1) tend)
				  ans))))
		     (else (cons (bytevector-copy bv start tend) ans))))))
	  (else ans))))

;; fold... for what!
(define (bytevector-fold kons knil bv
	  :optional (start 0) (end (bytevector-length bv)))
  (let lp ((v knil) (i start))
    (if (< i end)
	(lp (kons (bytevector-u8-ref bv i) v) (+ i 1))
	v)))
(define (bytevector-fold-right kons knil bv
	  :optional (start 0) (end (bytevector-length bv)))
  (let lp ((v knil) (i (- end 1)))
    (if (>= i start)
	(lp (kons (bytevector-u8-ref bv i) v) (- i 1))
	v)))

;; filter & delete
(define (bytevector-filter criterion bv
	  :optional (start 0) (end (bytevector-length bv)))
  (if (procedure? criterion)
      (let* ((slen (- end start))
	     (temp (make-bytevector slen))
	     (ans-len (bytevector-fold (lambda (c i)
					 (if (criterion c)
					     (begin (bytevector-u8-set! temp i c)
						    (+ i 1))
					     i))
				       0 bv start end)))
	(if (= ans-len slen) temp (bytevector-copy temp 0 ans-len)))

      (let* ((cset (cond ((u8-set? criterion) criterion)
			 ((u8? criterion) (list criterion))
			 (else 
			  (error 'bytevector-filter
				 "criterion not predicate, char or char-set"
				 criterion))))
	     
	     (len (bytevector-fold (lambda (c i) (if (u8-set-contains? cset c)
						     (+ i 1)
						     i))
				   0 bv start end))
	     (ans (make-bytevector len)))
	(bytevector-fold (lambda (c i) (if (u8-set-contains? cset c)
					   (begin (bytevector-u8-set! ans i c)
						  (+ i 1))
					   i))
			 0 bv start end)
	ans)))

(define (bytevector-delete criterion bv
	  :optional (start 0) (end (bytevector-length bv)))
  (if (procedure? criterion)
      (let* ((slen (- end start))
	     (temp (make-bytevector slen))
	     (ans-len (bytevector-fold (lambda (c i)
					 (if (criterion c)
					     i
					     (begin (bytevector-u8-set! temp i c)
						    (+ i 1))))
				       0 bv start end)))
	(if (= ans-len slen) temp (bytevector-copy temp 0 ans-len)))

      (let* ((cset (cond ((u8-set? criterion) criterion)
			 ((u8? criterion) (list criterion))
			 (else 
			  (error 'bytevector-delete
				 "criterion not predicate, char or char-set"
				 criterion))))
	     
	     (len (bytevector-fold (lambda (c i) (if (u8-set-contains? cset c)
						     i
						     (+ i 1)))
				   0 bv start end))
	     (ans (make-bytevector len)))
	(bytevector-fold (lambda (c i) (if (u8-set-contains? cset c)
					   i
					   (begin (bytevector-u8-set! ans i c)
						  (+ i 1))))
			 0 bv start end)
	ans)))


  
;; helper
(define (align-bytevectors bvs size)
  (align-bytevectors! (list-copy bvs) size))
(define (align-bytevectors! bvs size)
  (define (length-pred bv) (= (bytevector-length bv) size))
  (define (align1 bvs offset)
    (let ((buf (make-bytevector size)))
      (let loop ((bvs bvs) (i 0) (offset offset))
	(cond ((null? bvs) (values (bytevector-copy buf 0 i) '() 0))
	      ((= i size) (values buf bvs offset))
	      (else
	       (let* ((bv (car bvs))
		      (len (bytevector-length bv))
		      (buf-rest (- size i))
		      (src-len (- len offset)))
		 (cond ((>= buf-rest src-len)
			;; buffer has enough space, just copy
			(bytevector-copy! bv offset buf i src-len)
			(loop (cdr bvs) (+ i src-len) 0))
		       (else
			(bytevector-copy! bv offset buf i buf-rest)
			(values buf bvs (+ offset buf-rest))))))))))

  (let-values (((aligned need) (span! length-pred bvs)))
    (let loop ((need need) (r '()) (offset 0))
      (if (null? need)
	  (append aligned (reverse! r))
	  (let-values (((bv rest offset) (align1 need offset)))
	    (loop rest (cons bv r) offset))))))

)
