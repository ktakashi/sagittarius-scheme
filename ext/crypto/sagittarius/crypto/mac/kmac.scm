;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/mac/kmac.scm - KMAC
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
#!read-macro=sagittarius/bv-string
(library (sagittarius crypto mac kmac)
    (export *mac:kmac*
	    *mac:kmac-128*
	    *mac:kmac-256*
	    ;; OIDs
	    *oid-kmac-128*
	    *oid-kmac-256*
	    *oid-kmac-xof-128*
	    *oid-kmac-xof-256*)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto mac types)
	    (sagittarius crypto digests)
	    (sagittarius crypto digests cshake))

(define *mac:kmac* :kmac)
(define *mac:kmac-128* :kmac-128)
(define *mac:kmac-256* :kmac-256)

(define-record-type kmac-state
  (fields md mac-size xof?))

(define-method mac-state-initializer ((m (eql *mac:kmac*)) (key <bytevector>)
	      :key ((cshake cshake-descriptor?) #f)
	      :allow-other-keys opts)
  (apply kmac-state-initializer key cshake opts))
(define-method mac-state-initializer ((m (eql *mac:kmac-128*))
				      (key <bytevector>)
				      . opts)
  (apply kmac-state-initializer key *digest:cshake-128* opts))
(define-method mac-state-initializer ((m (eql *mac:kmac-256*))
				      (key <bytevector>)
				      . opts)
  (apply kmac-state-initializer key *digest:cshake-256* opts))

#|
KMAC128(K, X, L, S):
Validity Conditions: len(K) < 2^2040 and 0 ≤ L < 2^2040 and len(S) < 2^2040
1. newX = bytepad(encode_string(K), 168) || X || right_encode(L).
2. return cSHAKE128(newX, L, “KMAC”, S).

KMAC256(K, X, L, S):
Validity Conditions: len(K) <2^2040 and 0 ≤ L < 2^2040 and len(S) < 2^2040
1. newX = bytepad(encode_string(K), 136) || X || right_encode(L).
2. return cSHAKE256(newX, L, “KMAC”, S).

KMACXOF128(K, X, L, S):
Validity Conditions: len(K) < 2^2040 and 0 ≤ L and len(S) < 2^2040
1. newX = bytepad(encode_string(K), 168) || X || right_encode(0).
2. return cSHAKE128(newX, L, “KMAC”, S).

KMACXOF256(K, X, L, S):
Validity Conditions: len(K) <2^2040 and 0 ≤ L and len(S) < 2^2040
1. newX = bytepad(encode_string(K), 136) || X || right_encode(0).
2. return cSHAKE256(newX, L, “KMAC”, S).
|#
(define (kmac-state-initializer key cshake
	 :key (custom (or #f bytevector?))
	      (mac-size 64) ;; The same as HMAC-SHA512, larger is better though
	      (xof #f))
  (define block-size (digest-descriptor-block-size cshake))
  ;; NIST SP 800-185  8.4.2
  ;; > When used as a MAC, applications of this Recommendation shall not
  ;; > select an output length L that is less than 32 bits, and shall
  ;; > only select an output length less than 64 bits after a careful
  ;; > risk analysis is performed.
  ;; we simply disable output length less than 64 bits
  (when (< mac-size 8)
    (assertion-violation 'kmac-state-initializer
			 "MAC size must be greater than or equal to 8"
			 mac-size))
  (let ((init-val (bytepad (encode-string key) block-size)))
    (values (lambda ()
	      (let ((md (make-digest-descriptor cshake)))
		(message-digest-init! md :name #*"KMAC" :custom custom)
		(message-digest-process! md init-val)
		(make-kmac-state md mac-size xof)))
	    mac-size
	    (case block-size
	      ((168) (if xof *oid-kmac-xof-128* *oid-kmac-128*))
	      ((136) (if xof *oid-kmac-xof-256* *oid-kmac-256*))
	      (else (assertion-violation 'kmac-state-initializer
					 "Unknown KMAC, maybe wrong cSHAKE?"
					 cshake))))))

(define-method mac-state-processor ((s (eql *mac:kmac*))) kmac-process!)
(define-method mac-state-finalizer ((s (eql *mac:kmac*))) kmac-done!)
(define-method mac-state-processor ((s (eql *mac:kmac-128*))) kmac-process!)
(define-method mac-state-finalizer ((s (eql *mac:kmac-128*))) kmac-done!)
(define-method mac-state-processor ((s (eql *mac:kmac-256*))) kmac-process!)
(define-method mac-state-finalizer ((s (eql *mac:kmac-256*))) kmac-done!)

(define (kmac-process! (s kmac-state?) (bv bytevector?) . opts)
  (apply message-digest-process! (kmac-state-md s) bv opts))

(define (kmac-done! (s kmac-state?) (bv bytevector?) . opts)
  (if (kmac-state-xof? s)
      (message-digest-process! (kmac-state-md s) (right-encode 0))
      (let ((mac-size (kmac-state-mac-size s)))
	(message-digest-process! (kmac-state-md s) (right-encode mac-size))))
  (apply message-digest-done! (kmac-state-md s) bv opts))

(define *oid-kmac-128*     "2.16.840.1.101.3.4.2.19")
(define *oid-kmac-256*     "2.16.840.1.101.3.4.2.20")
(define *oid-kmac-xof-128* "2.16.840.1.101.3.4.2.21")
(define *oid-kmac-xof-256* "2.16.840.1.101.3.4.2.22")

)
