;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keys/types.scm - Key types
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

;; Cipher parameters, we share as much as possible
#!nounbound
(library (sagittarius crypto keys types)
    (export crypto-key? <crypto-key>
	    symmetric-key? <symmetric-key> make-symmetric-key
	    symmetric-key-value
	    asymmetric-key? <asymmetric-key>
	    public-key? <public-key>
	    private-key? <private-key>

	    (rename (key-pair <key-pair>))
	    key-pair? key-pair-private key-pair-public
	    make-key-pair
	    
	    destroyable-key? destroy-key!
	    
	    exportable? <exportable> exportable->bytevector
	    destroyable? <destroyable> destroy-destroyable!
	    )
    (import (rnrs)
	    (clos user))

(define-class <crypto-key> () ()) ;; interface :D
(define (crypto-key? o) (is-a? o <crypto-key>))

(define-class <destroyable> ()
  ((destroyed? :init-value #f :reader destroyable-destroyed?)))
(define destroyable-key-destroyed? destroyable-destroyed?)
(define (destroyable? o) (is-a? o <destroyable>))
(define (destroyable-key? o) (and (crypto-key? o) (destroyable? o)))

(define-generic destroy-destroyable!)
(define-generic destroy-key!)
(define-method destroy-key! (o) (destroy-destroyable! o))

;; symmetric keys are basically mere bytevector
(define-class <symmetric-key> (<crypto-key> <destroyable>)
  ((value :init-keyword :value)))
(define (make-symmetric-key key)
  (make <symmetric-key> :value (bytevector-copy key)))
(define (symmetric-key? o) (is-a? o <symmetric-key>))
(define (symmetric-key-value key) (bytevector-copy (slot-ref key 'value)))
(define-method destroy-destroyable! (key <symmetric-key>)
  (let ((v (slot-ref key 'value)))
    (bytevector-fill! v 0)
    (slot-set! key 'value #f)
    (slot-set! key 'destroyed? #t)))

(define-class <asymmetric-key> (<crypto-key>) ()) ;; interface
(define (asymmetric-key? o) (is-a? o <asymmetric-key>))

;; unfortunately, these are algorithm specific
(define-class <public-key> (<asymmetric-key>) ())
(define (public-key? o) (is-a? o <public-key>))
(define-class <private-key> (<asymmetric-key>) ())
(define (private-key? o) (is-a? o <private-key>))

(define-record-type key-pair
  (fields private public))

(define-class <exportable> () ())
(define (exportable? o) (is-a? o <exportable>))
(define-generic exportable->bytevector)
)
