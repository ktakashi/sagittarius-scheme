;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; vectors.scm - implementation of SRFI-43
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


(library (srfi :43 vectors)
    (export 
     ;; constructors
     make-vector vector vector-unfold vector-unfold-right
     vector-copy vector-reverse-copy vector-append vector-concatenate
     ;; predicates
     vector? vector-empty? vector=
     ;; selectors
     vector-ref vector-length
     ;; iteration
     vector-fold vector-fold-right vector-map vector-map!
     vector-for-each vector-count
     ;; searching
     vector-index vector-index-right vector-skip vector-skip-right
     vector-binary-search vector-any vector-every
     ;; mutators
     vector-set! vector-swap! vector-fill! vector-reverse!
     vector-copy! vector-reverse-copy!
     ;; conversion
     vector->list reverse-vector->list
     list->vector reverse-list->vector)
    (import (except (rnrs) vector-map vector-for-each)
	    (sagittarius))

  (define-syntax check-type
    (lambda (stx)
      (syntax-case stx ()
        ((_ pred? value callee)
	 (if (identifier? #'value)
	     #'(if (pred? value)
		   value
		   (assertion-violation 'callee "type check failed" value))
	     #'(let ([v value])
		 (if (pred? v)
		     v
		     (assertion-violation 'callee
					  "type check failed" value))))))))

  (define (between? x y z) (and (< x y) (<= y z)))
  (define (vectors-ref vectors i) (map (lambda (v) (vector-ref v i)) vectors))
  (define (smallest-length vec-list len)
    (let loop ((vec-list vec-list) (len len))
      (if (null? vec-list)
	  len
	  (loop (cdr vec-list)
		(min (vector-length (car vec-list)) len)))))

  (define (vector-fold1 kons knil len vec)
    (let loop ((i 0) (knil knil))
      (if (= i len)
	  knil
	  (loop (+ i 1) (kons i knil (vector-ref vec i))))))
  (define (vector-fold2+ kons knil len vectors)
    (let loop ((i 0) (knil knil))
      (if (= i len)
	  knil
	  (loop (+ i 1) (apply kons i knil (vectors-ref vectors i))))))

  (define (vector-map1! f target vec len)
    (let loop ((i len))
      (if (zero? i)
	  target
	  (let ((j (- i 1)))
	    (vector-set! target j (f j (vector-ref vec j)))
	    (loop j)))))
  (define (vector-map2+! f target vectors len)
    (let loop ((i len))
      (if (zero? i)
	  target
	  (let ((j (- i 1)))
	    (vector-set! target j (apply f j (vectors-ref vectors j)))
	    (loop j)))))

  (define (vector-unfold f len . seed)
    (define (tabulate! f vec i len)
      (when (< i len)
	(vector-set! vec i (f i))
	(tabulate! f vec (+ i 1) len)))
    (define (unfold1! f vec i len seed)
      (if (< i len)
	  (let-values (((elt seed) (f i seed)))
	    (vector-set! vec i elt)
	    (unfold1! f vec (+ i 1) len seed))))
    (define (unfold2+! f vec i len seeds)
      (if (< i len)
	  (let-values (((elt . seeds) (apply f i seeds)))
	    (vector-set! vec i elt)
	    (unfold2+! f vec (+ i 1) len seeds))))
    (let ((vec (make-vector len))
	  (i (- len 1)))
      (cond ((null? seed)
	     (tabulate! f vec 0 len))
	    ((null? (cdr seed))
	     (unfold1!  f vec 0 len (car seed)))
	    ((null? (cdr seed))
	     (unfold2+! f vec 0 len seed)))
      vec))

  (define (vector-unfold-right f len . seed)
    (define (tabulate! f vec i)
      (when (>= i 0)
	(vector-set! vec i (f i))
	(tabulate! f vec (- i 1))))
    (define (unfold1! f vec i seed)
      (if (>= i 0)
	  (let-values (((elt seed) (f i seed)))
	    (vector-set! vec i elt)
	    (unfold1! f vec (- i 1) seed))))
    (define (unfold2+! f vec i seeds)
      (if (>= i 0)
	  (let-values (((elt . seeds) (apply f i seeds)))
	    (vector-set! vec i elt)
	    (unfold2+! f vec (- i 1) seeds))))
    (let ((vec (make-vector len))
	  (i (- len 1)))
      (cond ((null? seed)
	     (tabulate! f vec i))
	    ((null? (cdr seed))
	     (unfold1!  f vec i (car seed)))
	    ((null? (cdr seed))
	     (unfold2+! f vec i seed)))
      vec))

  (define (vector-empty? vec) (zero? (vector-length vec)))

  (define (vector= elt=? . vectors)
    (define (binary-vector= elt=? vector-a vector-b)
      (or (eq? vector-a vector-b)           ;+++
	  (let ((length-a (vector-length vector-a))
		(length-b (vector-length vector-b)))
	    (define (loop i)
	      (or (= i length-a)
		  (and (< i length-b)
		       (test (vector-ref vector-a i)
			     (vector-ref vector-b i)
			     i))))
	    (define (test elt-a elt-b i)
	      (and (or (eq? elt-a elt-b) ;+++
		       (elt=? elt-a elt-b))
		   (loop (+ i 1))))
	    (and (= length-a length-b)
		 (loop 0)))))
    (cond ((null? vectors) #t)
	  ((null? (cdr vectors)) 
	   (check-type vector? (car vectors) vector=)
	   #t)
	  (else
	   (let loop ((vecs vectors))
	     (let ((vec1 (check-type vector? (car vecs) vector=))
		   (vec2+ (cdr vecs)))
	       (or (null? vec2+)
		   (and (binary-vector= elt=? vec1 (car vec2+))
			(loop vec2+))))))))


  (define (vector-fold kons knil vec . vectors)
    (if (null? vectors)
	(vector-fold1  kons knil (vector-length vec) vec)
	(vector-fold2+ kons knil (smallest-length vectors (vector-length vec))
		       (cons vec vectors))))

  (define (vector-fold-right kons knil vec . vectors)
    (if (null? vectors)
	(let loop ((i (- (vector-length vec) 1)) (knil knil))
	  (if (negative? i)
	      knil
	      (loop (- i 1) (kons i knil (vector-ref vec i)))))
	(let ((vecs (cons vec vectors)))
	  (let loop ((i (- (smallest-length vectors (vector-length vec)) 1))
		     (knil knil))
	    (if (negative? i)
		knil
		(loop (- i 1) (apply kons i knil (vectors-ref vecs i))))))))

  (define (vector-map f vec . vectors)
    (if (null? vectors)
	(let ((len (vector-length vec)))
	  (vector-map1!  f (make-vector len) vec len))
	(let ((len (smallest-length vectors (vector-length vec))))
	  (vector-map2+! f (make-vector len) (cons vec vectors) len))))

  (define (vector-map! f vec . vectors)
    (if (null? vectors)
	(vector-map1!  f vec vec (vector-length vec))
	(vector-map2+! f vec (cons vec vectors)
		      (smallest-length vectors (vector-length vec))))
    (undefined))

  (define (vector-for-each f vec . vectors)
    (if (null? vectors)
	(let ((len (vector-length vec)))
	  (let loop ((i 0))
	    (when (< i len)
	      (f i (vector-ref vec i))
	      (loop (+ i 1)))))
	(let ((len (smallest-length vectors (vector-length vec)))
	      (vecs (cons vec vectors)))
	  (let loop ((i 0))
	    (when (< i len)
	      (apply f i (vectors-ref vecs i))
	      (loop (+ i 1)))))))

  (define (vector-count pred? vec . vectors)
    (if (null? vectors)
	(vector-fold1 (lambda (index count elt)
			(if (pred? index elt) (+ count 1) count))
		      0
		      (vector-length vec)
		      vec)
	(vector-fold2+ (lambda (index count . elts)
			 (if (apply pred? index elts) (+ count 1) count))
		       0
		       (smallest-length vectors (vector-length vec))
		       (cons vec vectors))))

  (define (vector-index pred? vec . vectors)
    (vector-index/skip pred? vec vectors))
  (define (vector-skip pred? vec . vectors)
    (vector-index/skip (lambda elts (not (apply pred? elts))) vec vectors))

  (define (vector-index/skip pred? vec vectors)
    (if (null? vectors)
	(let ((len (vector-length vec)))
	  (let loop ((i 0))
	    (cond ((= i len) #f)
		  ((pred? (vector-ref vec i)) i)
		  (else (loop (+ i 1))))))
	(let ((vecs (cons vec vectors))
	      (len (smallest-length vectors (vector-length vec))))
	  (let loop ((i 0))
	    (cond ((= len i) #f)
		  ((apply pred? (vectors-ref vecs i)) i)
		  (else (loop (+ i 1))))))))

  (define (vector-index-right pred? vec . vectors)
    (vector-index/skip-right pred? vec vectors))
  (define (vector-skip-right pred? vec . vectors)
    (vector-index/skip-right (lambda elts (not (apply pred? elts)))
			     vec vectors))
  (define (vector-index/skip-right pred? vec vectors)
    (if (null? vectors)
	(let loop ((i (- (vector-length vec) 1)))
	  (cond ((negative? i) #f)
		((pred? (vector-ref vec i)) i)
		(else (loop (- i 1)))))
	(let ((vecs (cons vec vectors)))
	  (let loop ((i (- (smallest-length vectors (vector-length vec)) 1)))
	    (cond ((negative? i) #f)
		  ((apply pred? (vectors-ref vecs i)) i)
		  (else (loop (- i 1))))))))

  (define (vector-binary-search vec value cmp
				:optional (start 0) (end (vector-length vec)))
    (let loop ((start start) (end end) (j #f))
      (let ((i (div (+ start end) 2)))
	(if (or (= start end) (and j (= i j)))
	    #f
	    (let ((comparison (cmp (vector-ref vec i) value)))
	      (cond ((not (integer? comparison))
		     (assertion-violation 
		      'vector-binary-search
		      "comparator returned non integer value" comparison))
		    ((zero? comparison) i)
		    ((positive? comparison) (loop start i i))
		    (else (loop i end i))))))))

  (define (vector-any pred? vec . vectors)
    (if (null? vectors)
	(let* ((len (vector-length vec))
	       (len-1 (- len 1)))
	  (let loop ((i 0))
	    (and (not (= i len))
		 (if (= i len-1)
		     (pred? (vector-ref vec i))
		     (or (pred? (vector-ref vec i))
			 (loop (+ i 1)))))))
	(let* ((len (smallest-length vectors (vector-length vec)))
	       (len-1 (- len 1))
	       (vectors (cons vec vectors)))
	  (let loop ((i 0))
	    (and (not (= i len))
		(if (= i len-1)
		    (apply pred? (vectors-ref vectors i))
		    (or (apply pred? (vectors-ref vectors i))
			 (loop (+ i 1)))))))))

  (define (vector-every pred? vec . vectors)
    (if (null? vectors)
	(let* ((len (vector-length vec))
	       (len-1 (- len 1)))
	  (let loop ((i 0))
	    (or (= i len)
		(if (= i len-1)
		    (pred? (vector-ref vec i))
		    (and (pred? (vector-ref vec i))
			 (loop (+ i 1)))))))
	(let* ((len (smallest-length vectors (vector-length vec)))
	       (len-1 (- len 1))
	       (vectors (cons vec vectors)))
	  (let loop ((i 0))
	    (or (= i len)
		(if (= i len-1)
		    (apply pred? (vectors-ref vectors i))
		    (and (apply pred? (vectors-ref vectors i))
			 (loop (+ i 1)))))))))

  (define (vector-swap! vec i j)
    (let ((x (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j x)))

  (define (vector-copy! target tstart src
			:optional (sstart 0) (send (vector-length src)))
    (let ((src-len (vector-length src))
	  (dst-len (vector-length target)))
      (when (or (< dst-len (+ tstart (- send sstart)))
		(< sstart 0) (< send 0) (< tstart 0)
		(> sstart send) (> send src-len) (>= sstart src-len)
		(> tstart dst-len))
	(assertion-violation 'vector-copy!
			     "vector range out of bound"
			     `((target ,target) (start ,tstart))
			     `((source ,src) (start ,sstart) (end ,send)))))
    (if (> sstart tstart)
	(let loop ((i sstart) (j tstart))
	  (when (< i send)
	    (vector-set! target j (vector-ref src i))
	    (loop (+ i 1) (+ j 1))))
	(let loop ((i (- send 1)) (j (+ -1 tstart send (- sstart))))
	  (when (>= i sstart)
	    (vector-set! target j (vector-ref src i))
	    (loop (- i 1) (- j 1))))))

  (define (%vector-reverse-copy! dst ds src ss se)
    (let loop ((i (- se 1)) (j ds))
      (unless (< i ss)
	(vector-set! dst j (vector-ref src i))
	(loop (- i 1) (+ j 1)))))

  (define (vector-reverse-copy vec
			       :optional (start 0) (end (vector-length vec)))
    (let ((new (make-vector (- end start))))
      (%vector-reverse-copy! new 0 vec start end)
      new))

  (define (vector-reverse-copy! target tstart src
				:optional (sstart 0) (send (vector-length src)))
    (let ((src-len (vector-length src)))
      (when (or (>= sstart src-len) (> send src-len)
		(< sstart 0) (< send 0) (> sstart send))
	(assertion-violation 'vector-reverse-copy!
			     "source vector range out of bound" sstart)))
    (cond ((and (eq? target src) (= sstart tstart))
	   (vector-reverse! target tstart send))
	  ((and (eq? tstart src)
		(or (between? sstart tstart send)
		    (between? tstart sstart (+ tstart (- send sstart)))))
	   (assertion-violation 'vector-reverse-copy!
				"vector range for self-copying overlaps"
				target tstart sstart send))
	  (else (%vector-reverse-copy! target tstart src sstart send))))

  (define (reverse-vector->list vec
				:optional (start 0) (end (vector-length vec)))
    (reverse! (vector->list vec start end)))

  (define (reverse-list->vector lis
				:optional (start 0) (end (length lis)))
    (vector-reverse! (list->vector lis start end)))
)