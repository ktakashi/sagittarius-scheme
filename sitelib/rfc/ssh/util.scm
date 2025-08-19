;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/utill.scm - SSH2 utilities
;;;  
;;;   Copyright (c) 2010-2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc ssh util)
    (export ssh-read-public-key
	    bytevector->ssh-public-key
	    read-ssh-public-key)
    (import (rnrs)
	    (clos user)
	    (rfc ssh types)
	    (sagittarius)
	    (sagittarius crypto keys)
	    (binary pack))

(define (ssh-read-public-key in)
  (let* ((size (get-unpack in "!L"))
	 (name (utf8->string (get-bytevector-n in size))))
    (read-ssh-public-key (string->keyword name) in)))

(define (bytevector->ssh-public-key bv)
  (ssh-read-public-key (open-bytevector-input-port bv)))

(define-generic read-ssh-public-key)
(define-method read-ssh-public-key (m in)
  (error 'ssh-read-public-key "unknown public key type" m))

(define-method read-ssh-public-key ((m (eql :ssh-dss)) in)
  (let* ((p (read-message :mpint in #f))
	 (q (read-message :mpint in #f))
	 (g (read-message :mpint in #f))
	 (y (read-message :mpint in #f))
	 (kp (make <dsa-key-parameter> :p p :q q :g g)))
    (generate-public-key *key:dsa* kp y)))

(define-method read-ssh-public-key ((m (eql :ssh-rsa)) in)
  (let* ((e (read-message :mpint in #f))
	 (n (read-message :mpint in #f)))
    (generate-public-key *key:rsa* n e)))

(define-method read-ssh-public-key ((m (eql :ssh-ed25519)) in)
  (generate-public-key *key:ed25519* (read-message :string in #f)))

)
