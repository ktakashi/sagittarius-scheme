;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport/kex/dh.scm - SSH2 protocol DH key exchange
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

#!read-macro=sagittarius/regex
#!nounbound
(library (rfc ssh transport kex dh)
    (export ssh-client-exchange-kex-message)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius crypto digests) 
	    (sagittarius crypto random)
	    (srfi :13 strings)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport io)
	    (rfc ssh transport kex api))

(define-constant +dh-group1-p+
  #xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF)

(define-constant +dh-group1-g+ 2)

(define-constant +dh-group14-p+
  #xffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f14374fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7edee386bfb5a899fa5ae9f24117c4b1fe649286651ece45b3dc2007cb8a163bf0598da48361c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552bb9ed529077096966d670c354e4abc9804f1746c08ca18217c32905e462e36ce3be39e772c180e86039b2783a2ec07a28fb5c55df06f4c52c9de2bcbf6955817183995497cea956ae515d2261898fa051015728e5a8aacaa68ffffffffffffffff)

(define-constant +dh-group14-g+ 2)


(define group-exchanges (list +kex-diffie-hellman-group-exchange-sha256+
			      +kex-diffie-hellman-group-exchange-sha1+))

(define-method ssh-client-exchange-kex-message
  ((m (member group-exchanges)) transport client-packet server-packet)
  (define (generate-e&x transport)
    (let ((gex-req (make <ssh-msg-kex-dh-gex-request>)))
      (ssh-write-ssh-message transport gex-req)
      (let* ((reply (ssh-read-packet transport))
	     (gex-group (read-message <ssh-msg-kex-dh-gex-group>
				      (open-bytevector-input-port reply)))
	     (p (~ gex-group 'p))
	     (g (~ gex-group 'g)))
	(let ((x (generate-x (~ transport 'prng) (div (- p 1) 2))))
	  (values p g x (mod-expt g x p) gex-req)))))
  (let-values (((p g x e req) (generate-e&x transport)))
    (ssh-kex-send/receive transport (make-init-class <ssh-msg-kex-dh-gex-init> e)
			  <ssh-msg-kex-dh-gex-reply>
			  (compute-k p x
			   (lambda (K-S f K)
			     (make <GEX-H> 
			       :V-C (~ transport 'client-version)
			       :V-S (~ transport 'server-version)
			       :I-C client-packet
			       :I-S server-packet
			       :K-S K-S
			       :min (~ req 'min) :n (~ req 'n) :max (~ req 'max)
			       :p p :g g :e e :f f :K K))))))

(define dh-group (list +kex-diffie-hellman-group14-sha256+
		       +kex-diffie-hellman-group14-sha1+
		       +kex-diffie-hellman-group1-sha1+))

(define-method ssh-client-exchange-kex-message
  ((m (member dh-group)) transport client-packet server-packet)
  (define (group-n kex)
    (cond ((#/group(\d+)/ kex) =>
	   (lambda (m) (string->number (m 1))))
	  (else (error 'ssh-exchange-kex-message "must not happen"))))
  (define (generate-e&x transport)
    (let-values (((p g) (case (group-n (~ transport 'kex))
			  ((1)  (values +dh-group1-p+ +dh-group1-g+))
			  ((14) (values +dh-group14-p+ +dh-group14-g+))
			  (else (error 'ssh-exchange-kex-message "unknown group"
				       (~ transport 'kex))))))
      (let ((x (generate-x (~ transport 'prng) p)))
	(values p g x (mod-expt g x p) #f))))
  (let-values (((p g x e req) (generate-e&x transport)))
    (ssh-kex-send/receive transport (make-init-class <ssh-msg-kexdh-init> e)
			  <ssh-msg-kexdh-reply>
			  (compute-k p x
			   (lambda (K-S f K)
			     (make <DH-H> 
			       :V-C (~ transport 'client-version)
			       :V-S (~ transport 'server-version)
			       :I-C client-packet
			       :I-S server-packet
			       :K-S K-S
			       :e e :f f :K K))))))

(define (dh? n) (string-prefix? "diffie-hellman" n))
(define-method ssh-kex-digest ((n (?? dh?)))
  (cond ((#/sha(\d+)/ n)
	 => (lambda (m)
	      (make-message-digest (case (string->number (m 1))
				     ((1) *digest:sha-1*)
				     ((256) *digest:sha-256*)
				     ((384) *digest:sha-384*)
				     ((512) *digest:sha-512*)))))))

;; private
(define ((make-init-class init-class e))
  (make init-class :e e))

(define ((compute-k p x make-verify-message) transport kex-reply)
  (let* ((K-S (~ kex-reply 'K-S))
	 (f   (~ kex-reply 'f))
	 (K   (mod-expt f x p)))
    ;; verify signature
    (let* ((msg (make-verify-message K-S f K))
	   (h (ssh-verify-signature transport msg K-S (~ kex-reply 'signature))))
      (values K h))))

(define (generate-x prng r)
  (define size (div (bitwise-length r) 8))
  (let loop ()
    (let ((v (bytevector->integer
	      (random-generator-read-random-bytes prng size))))
      (or (and (< 1 v) v)
	  (loop)))))

(define-ssh-message <DH-H> ()
  ((V-C :utf8-string)
   (V-S :utf8-string)
   (I-C :string)
   (I-S :string)
   (K-S :string)
   (e   :mpint)
   (f   :mpint)
   (K   :mpint)))

(define-ssh-message <GEX-H> ()
  ((V-C :utf8-string)
   (V-S :utf8-string)
   (I-C :string)
   (I-S :string)
   (K-S :string)
   (min :uint32)
   (n   :uint32)
   (max :uint32)
   (p   :mpint)
   (g   :mpint)
   (e   :mpint)
   (f   :mpint)
   (K   :mpint)))

)
