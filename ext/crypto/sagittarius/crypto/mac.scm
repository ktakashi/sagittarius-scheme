;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/mac.scm - MAC
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

#!nounbound
(library (sagittarius crypto mac)
    (export mac? <mac> make-mac
	    mac-type
	    mac-mac-size
	    mac-oid
	    
	    *mac:hmac* *mac:cmac*

	    generate-mac generate-mac!
	    
	    mac-init!
	    mac-process!
	    mac-done!)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius combinators)
	    (sagittarius mop immutable)
	    (sagittarius crypto mac types)
	    (sagittarius crypto mac hmac)
	    (sagittarius crypto mac cmac))

(define (mac-state mac) (slot-ref mac 'state))
(define-class <mac> (<immutable>)
  ((type :init-keyword :type :reader mac-type)
   (state :init-keyword :state :init-value #f :mutable #t
	  :reader mac-state :writer mac-state-set!)
   (mac-size :init-keyword :mac-size :reader mac-mac-size)
   (oid :init-keyword :oid :reader mac-oid)
   (initializer :init-keyword :initializer :reader mac-initializer)
   (processor :init-keyword :processor :reader mac-processor)
   (finalizer :init-keyword :finalizer :reader mac-finalizer)))
(define-method write-object ((o <mac>) p) (format p "#<mac ~a>" (mac-type o)))
(define (mac? o) (is-a? o <mac>))

(define (make-mac type key . opts)
  (let-values (((initializer mac-size oid)
		(apply mac-state-initializer type key opts)))
    (make <mac>
      :type type
      :mac-size mac-size
      :oid oid
      :initializer initializer
      :processor (mac-state-processor type)
      :finalizer (mac-state-finalizer type))))

(define (generate-mac mac msg :optional (length (mac-mac-size mac)))
  (let ((out (make-bytevector length 0)))
    (generate-mac! mac msg out 0)
    out))

(define (generate-mac! mac msg out . opts)
  (apply mac-done! (mac-process! (mac-init! mac) msg) out opts))

(define (mac-init! (mac mac?))
  (mac-state-set! mac ((mac-initializer mac)))
  mac)
(define (mac-process! (mac mac?) (msg bytevector?) . opts)
  (apply (mac-processor mac) (mac-state mac) msg opts)
  mac)
(define (mac-done! (mac mac?) (out bytevector?) . opts)
  (apply (mac-finalizer mac) (mac-state mac) out opts))
)
