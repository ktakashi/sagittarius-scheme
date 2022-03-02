;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; util/flexible-vector - Flexible vector
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

;; at this moment, we implemenet what we need
#!nounbound
(library (util flexible-vector)
    (export flexible-vector?
	    
	    make-flexible-vector
	    flexible-vector-size
	    (rename (flexible-vector-size flexible-vector-length)
		    (%flexible-vector flexible-vector))
	    flexible-vector-append flexible-vector-concatenate
	    vector->flexible-vector list->flexible-vector
	    flexible-vector->vector flexible-vector->list

	    flexible-vector-ref flexible-vector-front flexible-vector-back

	    flexible-vector-set!
	    flexible-vector-insert! flexible-vector-delete!
	    flexible-vector-copy! flexible-vector-append!

	    flexible-vector-index flexible-vector-index-right
	    flexible-vector-any flexible-vector-every
	    )
    (import (rnrs)
	    (srfi :1 lists) ;; for reverse!
	    (srfi :133 vectors))
(define-record-type flexible-vector
  (fields (mutable elements flexible-vector-elements %fv-elements-set!)
	  (mutable size flexible-vector-size %fv-size-set!)
	  default)
  (protocol (lambda (p)
	      (lambda (capacity . maybe-default)
		(define default
		  (if (not (null? maybe-default)) (car maybe-default)))
		(unless (fixnum? capacity)
		  (assertion-violation 'make-flexible-vector
		    "Initial capacity must be a fixnum" capacity))
		(p (make-vector capacity default) 0 default)))))

;;; constructors
(define (%flexible-vector . args)
  (let ((fv (make-flexible-vector (length args))))
    (apply flexible-vector-insert! fv 0 args)))

(define (flexible-vector-append . fv*) (flexible-vector-concatenate fv*))

(define (flexible-vector-concatenate fv*)
  (let ((e (apply vector-append (map flexible-vector-elements fv*))))
    (vector->flexible-vector e)))

(define (vector->flexible-vector vec . maybe-default)
  (let ((fv (apply make-flexible-vector 0 maybe-default)))
    (%fv-elements-set! fv (vector-copy vec))
    (%fv-size-set! fv (vector-length vec))
    fv))

(define (list->flexible-vector l . maybe-default)
  (let* ((len (length l))
	 (fv (apply make-flexible-vector len maybe-default))
	 (e* (flexible-vector-elements fv)))
    (%fv-size-set! fv len)
    (do ((i 0 (+ i 1)) (l l (cdr l)))
	((= i len) fv)
      (vector-set! e* i (car l)))))

(define (flexible-vector->vector fv)
  (vector-copy (flexible-vector-elements fv) 0 (flexible-vector-size fv)))

(define (flexible-vector->list fv)
  (do ((i 0 (+ i 1)) (r '() (cons (vector-ref e* i) r))
       (len (flexible-vector-size fv)) (e* (flexible-vector-elements fv)))
      ((= i len) (reverse! r))))

(define (flexible-vector-ensure-size! fv size)
  (define current-size (vector-length (flexible-vector-elements fv)))
  (define (expand fv new-size)
    (define elements (flexible-vector-elements fv))
    (define new-elements (make-vector new-size (flexible-vector-default fv)))
    (do ((i 0 (+ i 1)) (len (vector-length elements)))
	((= i len) (%fv-elements-set! fv new-elements))
      (vector-set! new-elements i (vector-ref elements i))))
  (when (< current-size size) (expand fv size))
  fv)

;;; accessors
(define (flexible-vector-ref fv index)
  (when (>= index (flexible-vector-size fv))
    (assertion-violation 'flexible-vector-ref "Index out of bound" fv index))
  (vector-ref (flexible-vector-elements fv) index))

(define (flexible-vector-front fv) (flexible-vector-ref fv 0))
(define (flexible-vector-back fv)
  (flexible-vector-ref fv (- (flexible-vector-size fv) 1)))

(define (flexible-vector-set! fv index value)
  (define current-size (flexible-vector-size fv))
  (flexible-vector-ensure-size! fv (+ index 1))
  (let* ((elements (flexible-vector-elements fv))
	 (r (if (not (= current-size index)) (vector-ref elements index))))
    (vector-set! elements index value)
    (let ((size (flexible-vector-size fv)))
      (when (< size (+ index 1))
	(%fv-size-set! fv (+ index 1))))
    r))

(define (flexible-vector-insert! fv index value . value*)
  (define current-size (flexible-vector-size fv))
  (define count (+ 1 (length value*)))
  (flexible-vector-ensure-size! fv (if (< index current-size)
				       (+ current-size count)
				       (+ index count)))
  (let ((elements (flexible-vector-elements fv)))
    (when (< index current-size)
      (vector-copy! elements (+ index count) elements index current-size))
    (vector-set! elements index value)
    (unless (null? value*)
      (do ((i (+ index 1) (+ i 1)) (value* value* (cdr value*)))
	  ((null? value*))
	(vector-set! elements i (car value*))))
    (%fv-size-set! fv (+ (if (< index current-size) current-size index) count))
    fv))

;; SRFI-43 order (not bytevector-copy!)
(define flexible-vector-copy!
  (case-lambda
   ((dst dstart src) (flexible-vector-copy! dst dstart src 0))
   ((dst dstart src sstart)
    (flexible-vector-copy! dst dstart src sstart (flexible-vector-size src)))
   ((dst dstart src sstart send)
    (flexible-vector-ensure-size! dst (+ dstart send))
    (vector-copy! (flexible-vector-elements dst) dstart
		  (flexible-vector-elements src) sstart send)
    (%fv-size-set! dst (+ (flexible-vector-size dst) send))
    dst)))

(define (flexible-vector-append! fv . fv*)
  (if (null? fv*)
      fv
      (let* ((size (fold-left (lambda (acc fv)
				(+ (flexible-vector-size fv) acc))
			      (flexible-vector-size fv) fv*))
	     (fv (flexible-vector-ensure-size! fv size)))
	(do ((index (flexible-vector-size fv) (flexible-vector-size fv))
	     (fv* fv* (cdr fv*)))
	    ((null? fv*) fv)
	  (flexible-vector-copy! fv index (car fv*))))))

(define (flexible-vector-delete! fv index)
  (define current-size (flexible-vector-size fv))
  (when (or (< index 0) (> index current-size) (zero? current-size))
    (assertion-violation 'flexible-vector-delete!
			 "Index out of bound" fv index))
  (let* ((elements (flexible-vector-elements fv))
	 (r (vector-ref elements index)))
    (unless (= current-size (+ index 1))
      (vector-copy! elements index elements (+ index 1) current-size))
    (%fv-size-set! fv (- current-size 1))
    r))

(define (flexible-vector-clear! fv) (%fv-size-set! fv 0) fv)

;;; filter
(define (flexible-vector-index pred fx . fx*)
  ;; lazy...
  (apply vector-index pred (map flexible-vector-elements (cons fx fx*))))
(define (flexible-vector-index-right pred fx . fx*)
  ;; lazy...
  (apply vector-index-right pred (map flexible-vector-elements (cons fx fx*))))
(define (flexible-vector-any pred fx . fx*)
  ;; lazy...
  (apply vector-any pred (map flexible-vector-elements (cons fx fx*))))

(define (flexible-vector-every pred fx . fx*)
  ;; lazy...
  (apply vector-every pred (map flexible-vector-elements (cons fx fx*))))
)
