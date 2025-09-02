;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/crypto/keys.scm - SSH2 cryptography
;;;  
;;;   Copyright (c) 2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc ssh crypto keys)
    (export make-ssh-public-key
	    ssh-read-public-key
	    ssh-message-bytevector->public-key
	    read-ssh-public-key
	    ssh-ecdsa-identifier->ec-parameter
	    ec-parameter->ssh-ecdsa-identifier)
    (import (rnrs)
	    (clos user)
	    (rfc ssh types)
	    (binary io)
	    (srfi :13 strings)
	    (sagittarius)
	    (sagittarius crypto keys)
	    (sagittarius crypto math ec))

(define-generic make-ssh-public-key)
(define-method make-ssh-public-key ((pk <dsa-public-key>))
  (let ((param (dsa-key-parameter pk)))
    (make <ssh-dss-public-key> :name "ssh-dss"
	  :p (dsa-key-parameter-p param)
	  :q (dsa-key-parameter-q param)
	  :g (dsa-key-parameter-g param)
	  :y (dsa-public-key-Y pk))))

(define-method make-ssh-public-key ((pk <rsa-public-key>))
  (make <ssh-rsa-public-key> :name "ssh-rsa"
	:e (rsa-public-key-exponent pk)
	:n (rsa-public-key-modulus pk)))

(define-method make-ssh-public-key ((pk <eddsa-public-key>))
  (make <ssh-eddsa-public-key>
    :name (if (ed25519-key? pk) "ssh-ed25519" "ssh-ed448")
    :key (eddsa-public-key-data pk)))

(define-method make-ssh-public-key ((pk <ecdsa-public-key>))
  (define curve (ecdsa-key-parameter pk))
  (define id (ec-parameter->ssh-ecdsa-identifier curve))
  (define Q (encode-ec-point (ec-parameter-curve curve)
			     (ecdsa-public-key-Q pk)))
  (make <ssh-ecdsa-public-key>
    :name (string-append "ecdsa-sha2-" id)
    :blob (make <ssh-ecdsa-public-key-blob> :identifier id :Q Q)))


(define (ssh-read-public-key in)
  (let* ((size (get-u32 in (endianness big)))
	 (name (utf8->string (get-bytevector-n in size))))
    (read-ssh-public-key name in)))

(define (ssh-message-bytevector->public-key bv)
  (ssh-read-public-key (open-bytevector-input-port bv)))

(define-generic read-ssh-public-key :class <predicate-specializable-generic>)
(define-method read-ssh-public-key (m in)
  (error 'ssh-read-public-key "unknown public key type" m))

(define-method read-ssh-public-key ((m (equal "ssh-dss")) in)
  (let* ((p (read-message :mpint in #f))
	 (q (read-message :mpint in #f))
	 (g (read-message :mpint in #f))
	 (y (read-message :mpint in #f))
	 (kp (make <dsa-key-parameter> :p p :q q :g g)))
    (generate-public-key *key:dsa* kp y)))

(define-method read-ssh-public-key ((m (equal "ssh-rsa")) in)
  (let* ((e (read-message :mpint in #f))
	 (n (read-message :mpint in #f)))
    (generate-public-key *key:rsa* n e)))

(define-method read-ssh-public-key ((m (equal "ssh-ed25519")) in)
  (generate-public-key *key:ed25519* (read-message :string in #f)))

(define-method read-ssh-public-key ((m (equal "ssh-ed448")) in)
  (generate-public-key *key:ed448* (read-message :string in #f)))

(define (ecdsa-sha2? n) (string-prefix? "ecdsa-sha2" n))
(define-method read-ssh-public-key ((m (?? ecdsa-sha2?)) in)
  (let* ((blob (read-message <ssh-ecdsa-public-key-blob> in))
	 (identifier (slot-ref blob 'identifier))
	 (curve (ssh-ecdsa-identifier->ec-parameter
		 (string->keyword identifier))))
    (import-public-key *key:ecdsa* (slot-ref blob 'Q)
		       (public-key-format raw)
		       curve)))

(define-generic ssh-ecdsa-identifier->ec-parameter)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistp256)))
  *ec-parameter:p256*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistp384)))
  *ec-parameter:p384*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistp521)))
  *ec-parameter:p521*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistk163)))
  *ec-parameter:k163*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistp192)))
  *ec-parameter:p192*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistp224)))
  *ec-parameter:p224*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistk233)))
  *ec-parameter:k233*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistb233)))
  *ec-parameter:b233*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistk283)))
  *ec-parameter:k283*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistk409)))
  *ec-parameter:k409*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistb409)))
  *ec-parameter:b409*)
(define-method ssh-ecdsa-identifier->ec-parameter ((m (eql :nistt571)))
  *ec-parameter:sect571k1*)

(define-generic ec-parameter->ssh-ecdsa-identifier
  :class <predicate-specializable-generic>)
(define ((curve?? src) target)
  (elliptic-curve=? (ec-parameter-curve src) (ec-parameter-curve target)))
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:p256*))     )) "nistp256")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:p384*))     )) "nistp384")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:p521*))     )) "nistp521")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:k163*))     )) "nistk163")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:p192*))     )) "nistp192")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:p224*))     )) "nistp224")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:k233*))     )) "nistk233")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:b233*))     )) "nistb233")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:k283*))     )) "nistk283")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:k409*))     )) "nistk409")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:b409*))     )) "nistb409")
(define-method ec-parameter->ssh-ecdsa-identifier ((m (?? (curve?? *ec-parameter:sect571k1*)))) "nistt571")

)
