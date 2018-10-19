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
	    flexible-vector-size
	    make-flexible-vector 
	    vector->flexible-vector list->flexible-vector
	    flexible-vector->vector flexible-vector->list

	    flexible-vector-ref flexible-vector-set!
	    flexible-vector-insert! flexible-vector-delete!)
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

(define (flexible-vector-ref fv index)
  (when (>= index (flexible-vector-size fv))
    (assertion-violation 'flexible-vector-ref "Index out of bound" fv index))
  (vector-ref (flexible-vector-elements fv) index))
(define (flexible-vector-set! fv index value)
  (flexible-vector-ensure-size! fv (+ index 1))
  (vector-set! (flexible-vector-elements fv) index value)
  (let ((size (flexible-vector-size fv)))
    (when (< size (+ index 1))
      (%fv-size-set! fv (+ index 1)))))
(define (flexible-vector-insert! fv index value)
  (define current-size (flexible-vector-size fv))
  (flexible-vector-ensure-size! fv (if (< index current-size)
				       (+ current-size 1)
				       (+ index 1)))
  (let ((elements (flexible-vector-elements fv)))
    (when (< index current-size)
      (vector-copy! elements (+ index 1) elements index current-size))
    (vector-set! elements index value)
    (%fv-size-set! fv (+ (if (< index current-size) current-size index) 1))))
(define (flexible-vector-delete! fv index)
  (define current-size (flexible-vector-size fv))
  (when (or (< index 0) (> index current-size))
    (assertion-violation 'flexible-vector-delete!
			 "Index out of bound" fv index))
  (let ((elements (flexible-vector-elements fv)))
    (vector-copy! elements index elements (+ index 1) current-size)
    (%fv-size-set! fv (- current-size 1))))
)
