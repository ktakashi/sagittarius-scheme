;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a230/atomic.scm - Atomic Operations
;;;
;;;  Copyright (c) 2025  Takashi Kato <ktakashi@ymail.com>
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#!nounbound
(library (srfi :230 atomic)
    (export memory-order
	    memory-order?
	    make-atomic-flag
	    atomic-flag?
	    atomic-flag-test-and-set!
	    atomic-flag-clear!
	    make-atomic-box
	    atomic-box?
	    atomic-box-ref
	    atomic-box-set!
	    atomic-box-swap!
	    atomic-box-compare-and-swap!
	    make-atomic-fxbox
	    atomic-fxbox?
	    atomic-fxbox-ref
	    atomic-fxbox-set!
	    atomic-fxbox-swap!
	    atomic-fxbox-compare-and-swap!
	    atomic-fxbox+/fetch!
	    atomic-fxbox-/fetch!
	    atomic-fxbox-and/fetch!
	    atomic-fxbox-ior/fetch!
	    atomic-fxbox-xor/fetch!
	    make-atomic-pair
	    atomic-pair?
	    atomic-pair-ref
	    atomic-pair-set!
	    atomic-pair-swap!
	    atomic-pair-compare-and-swap!
	    atomic-fence)
    (import (rnrs)
	    (rename (sagittarius atomic)
		    (memory-order? a:memory-order?)))
;; memory order
(define-enumeration memory-order
  (relaxed acquire release acquire-release sequentially-consistent)
  memory-orders)
(define *memory-orders* (enum-set-universe (memory-orders)))
(define (memory-order? sym) (enum-set-member? sym *memory-orders*))
(define *default-order* (memory-order sequentially-consistent))

;; atomic flag
(define (make-atomic-flag) (make-atomic #f))
(define (atomic-flag? a) (and (atomic? a) (boolean? (atomic-load a))))
(define (atomic-flag-test-and-set! (atomic atomic-flag?)
				   :optional (order *default-order*))
  (atomic-exchange! atomic #t (memory-order->value order)))
(define (atomic-flag-clear! (atomic atomic-flag?)
			    :optional (order *default-order*))
  (atomic-exchange! atomic #f (memory-order->value order)))

;; atomic box
(define (make-atomic-box obj) (make-atomic obj))
(define atomic-box? atomic?)
(define (atomic-box-ref (box atomic-box?)
			:optional (order *default-order*))
  (atomic-load box (memory-order->value order)))
(define (atomic-box-set! (box atomic-box?) obj 
			 :optional (order *default-order*))
  (atomic-store! box obj (memory-order->value order)))
(define (atomic-box-swap! (box atomic-box?) obj 
			  :optional (order *default-order*))
  (atomic-exchange! box obj (memory-order->value order)))
(define (atomic-box-compare-and-swap! (box atomic-box?) old new 
				      :optional (order *default-order*))
  (if (atomic-compare-and-swap! box old new (memory-order->value order))
      old
      (atomic-box-ref box order)))

;; atomic fixnum
(define (make-atomic-fxbox (fx fixnum?)) (make-atomic-fixnum fx))
(define (atomic-fxbox? obj) (atomic-fixnum? obj))
(define (atomic-fxbox-ref (afx atomic-fxbox?) 
			  :optional (order *default-order*))
  (atomic-fixnum-load afx (memory-order->value order)))
(define (atomic-fxbox-set! (afx atomic-fxbox?) (fx fixnum?) 
			   :optional (order *default-order*))
  (atomic-fixnum-store! afx fx (memory-order->value order)))
(define (atomic-fxbox-swap! (afx atomic-fxbox?) (fx fixnum?) 
			    :optional (order *default-order*))
  (atomic-fixnum-exchange! afx fx (memory-order->value order)))
(define (atomic-fxbox-compare-and-swap! (afx atomic-fxbox?)
					(old fixnum?) (new fixnum?)
					:optional (order *default-order*))
  (if (atomic-compare-and-swap! afx old new (memory-order->value order))
      old
      (atomic-fxbox-ref afx order)))
(define (atomic-fxbox+/fetch! (afx atomic-fxbox?) (fx fixnum?) 
			      :optional (order *default-order*))
  (atomic-fixnum-add! afx fx (memory-order->value order)))
(define (atomic-fxbox-/fetch! (afx atomic-fxbox?) (fx fixnum?) 
			      :optional (order *default-order*))
  (atomic-fixnum-sub! afx fx (memory-order->value order)))
(define (atomic-fxbox-and/fetch! (afx atomic-fxbox?) (fx fixnum?) 
				 :optional (order *default-order*))
  (atomic-fixnum-and! afx fx (memory-order->value order)))
(define (atomic-fxbox-ior/fetch! (afx atomic-fxbox?) (fx fixnum?) 
				 :optional (order *default-order*))
  (atomic-fixnum-ior! afx fx (memory-order->value order)))
(define (atomic-fxbox-xor/fetch! (afx atomic-fxbox?) (fx fixnum?) 
				 :optional (order *default-order*))
  (atomic-fixnum-xor! afx fx (memory-order->value order)))

;; atomic pair
;; make-atomic-pair and atomic-pair? are the same
(define (atomic-pair-ref (ap atomic-pair?) 
			 :optional (order *default-order*))
  (let ((p (atomic-load ap (memory-order->value order))))
    (values (car p) (cdr p))))
(define (atomic-pair-set! (ap atomic-pair?) a d 
			  :optional (order *default-order*))
  (atomic-store! ap (cons a d) (memory-order->value order)))
(define (atomic-pair-swap! (ap atomic-pair?) a d 
			   :optional (order *default-order*))
  (let ((p (atomic-exchange! ap (cons a d) (memory-order->value order))))
    (values (car p) (cdr p))))
(define (atomic-pair-compare-and-swap! (ap atomic-pair?) oa od na nd 
				       :optional (order *default-order*))
  (define morder (memory-order->value order))
  (if (atomic-compare-and-swap! ap (cons oa od) (cons na nd) morder)
      (values oa od)
      (atomic-pair-ref ap order)))

(define (atomic-fence (order memory-order?))
  (atomic-thread-fence (memory-order->value order)))

;; utilities
(define (memory-order->value (order memory-order?))
  (case order
    ((relaxed) *memory-order:relaxed*)
    ((acquire) *memory-order:acquire*)
    ((relaxed) *memory-order:release*)
    ((acquire-release) *memory-order:acq-rel*)
    ((sequentially-consistent) *memory-order:seq-cst*)))

)
