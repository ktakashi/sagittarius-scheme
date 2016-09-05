;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a133/vectors - Vector Library (R7RS-compatible)
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

(library (srfi :133 vectors)
    (export ;;Constructors
	    make-vector vector
	    vector-unfold vector-unfold-right
	    vector-copy vector-reverse-copy
	    vector-append vector-concatenate vector-append-subvectors

	    ;; Predicates
	    vector?
	    vector-empty?
	    vector=

	    ;; Selectors
	    vector-ref
	    vector-length

	    ;; Iteration
	    vector-fold vector-fold-right
	    vector-map vector-map!
	    vector-for-each vector-count
	    vector-cumulate

	    ;; Searching
	    vector-index vector-index-right
	    vector-skip vector-skip-right
	    vector-binary-search
	    vector-any vector-every
	    vector-partition

	    ;; Mutators
	    vector-set! vector-swap!
	    vector-fill! vector-reverse!
	    vector-copy! vector-reverse-copy!
	    vector-unfold! vector-unfold-right!

	    ;; Conversion
	    vector->list reverse-vector->list
	    list->vector reverse-list->vector
	    vector->string string->vector)
    (import (rnrs)
	    (sagittarius)
	    (match)
	    (rename (srfi :43) 
		    (vector-map s:vector-map)
		    (vector-map! s:vector-map!)
		    (vector-for-each s:vector-for-each)
		    (vector-fold s:vector-fold)
		    (vector-fold-right s:vector-fold-right)
		    (vector-binary-search s:vector-binary-search)
		    (vector-count s:vector-count)))

(define (vector-append-subvectors . vse*)
  ;; vse* = [vector start end] ...
  ;; the SRFI doesn't specify if the list of [vector start end] must 
  ;; always be like this or the last element can have only [vector].
  ;; as long as it's not specified, then we go strict way. 
  ;; (raises an error)
  (define (compute-size ovse*)
    (let loop ((vse* ovse*) (r 0))
      (match vse*
	(() r)
	(((? vector? vec) start end next ...)
	 (let ((n (- end start)))
	   (when (or (< start 0) (< end 0) (< n 0) (< end start))
	     (assertion-violation 'vector-append-subvectors
				  "start and end must be positive integer"
				  start end))
	   (loop next (+ r n))))
	(_ (assertion-violation 'vector-append-subvectors
				"argument must be list of [vector start end]"
				vse* ovse*)))))
  (let* ((size (compute-size vse*))
	 (r (make-vector size)))
    (let loop ((vse* vse*) (i 0))
      (match vse*
	(() r)
	((vec start end next ...)
	 (vector-copy! r i vec start end)
	 (loop next (+ i (- end start))))))))

;; TODO performance
(define (vector-fold kons knil vec . vecs)
  (apply s:vector-fold (lambda (i . args) (apply kons args)) knil vec vecs))
(define (vector-fold-right kons knil vec . vecs)
  (apply s:vector-fold-right (lambda (i . args) (apply kons args))
	 knil vec vecs))

(define (vector-map! f vec . vecs)
  (apply s:vector-map! (lambda (i . args) (apply f args)) vec vecs))

(define (vector-count pred? vec . vecs)
  (if (null? vecs)
      (s:vector-count (lambda (i e) (pred? e)) vec)
      (apply s:vector-count (lambda (i . e*) (apply pred? e*)) vec vecs)))

(define (vector-cumulate f knil vec)
  (define len (vector-length vec))
  (let loop ((i 0) (knil knil) (r '()))
    (if (= i len)
	(list->vector (reverse! r))
	(let ((knil (f knil (vector-ref vec i))))
	  (loop (+ i 1) knil (cons knil r))))))

;; only restrict the optional argument
;; TODO should we?
(define (vector-binary-search vec value cmp) 
  (s:vector-binary-search vec value cmp))

(define (vector-partition pred? vec)
  (define len (vector-length vec))
  (let loop ((i 0) (c 0) (f '()) (r '()))
    (cond ((= i len)
	   (values (list->vector (append! (reverse! f) (reverse! r))) c))
	  ((pred? (vector-ref vec i))
	   (loop (+ i 1) (+ c 1) (cons (vector-ref vec i) f) r))
	  (else
	   (loop (+ i 1) c f (cons (vector-ref vec i) r))))))

(define (vector-unfold! f vec start end . seed)
  (define (tabulate! f vec i len)
    (when (< i len)
      (vector-set! vec i (f i))
      (tabulate! f vec (+ i 1) len)))
  (define (unfold1! f vec i len seed)
    (when (< i len)
      (let-values (((elt seed) (f i seed)))
	(vector-set! vec i elt)
	(unfold1! f vec (+ i 1) len seed))))
  (define (unfold2+! f vec i len seeds)
    (when (< i len)
      (let-values (((elt . seeds) (apply f i seeds)))
	(vector-set! vec i elt)
	(unfold2+! f vec (+ i 1) len seeds))))
  (cond ((null? seed)       (tabulate! f vec start end))
	((null? (cdr seed)) (unfold1!  f vec start end (car seed)))
	(else               (unfold2+! f vec start end seed)))
  (undefined))

(define (vector-unfold-right! f vec start end . seed)
  (define (tabulate! f vec i)
    (when (>= i start)
      (vector-set! vec i (f i))
      (tabulate! f vec (- i 1))))
  (define (unfold1! f vec i seed)
    (when (>= i start)
      (let-values (((elt seed) (f i seed)))
	(vector-set! vec i elt)
	(unfold1! f vec (- i 1) seed))))
  (define (unfold2+! f vec i seeds)
    (when (>= i start)
      (let-values (((elt . seeds) (apply f i seeds)))
	(vector-set! vec i elt)
	(unfold2+! f vec (- i 1) seeds))))
  ;; exclusive
  (let ((end (- end 1)))
    (cond ((null? seed)       (tabulate! f vec end))
	  ((null? (cdr seed)) (unfold1!  f vec end (car seed)))
	  (else               (unfold2+! f vec end seed)))
    (undefined)))

(define (string->vector s . opts) (list->vector (apply string->list s opts)))
(define (vector->string v . opts) (apply string (apply vector->list v opts)))

)

