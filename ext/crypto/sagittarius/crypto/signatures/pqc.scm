;;; -*- mode:scheme; coding:utf-8; -*-
;;; Internal PQC signer/verifier integration.
;;;
;;; Copyright (c) 2026 Sagittarius contributors.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 1. Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;;    this list of conditions and the following disclaimer in the documentation
;;;    and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.
#!nounbound
(library (sagittarius crypto signatures pqc)
    (export define-pqc-signature)
    (import (rnrs) (clos user) (sagittarius)
            (sagittarius crypto asn1)
            (sagittarius crypto signatures types)
            (sagittarius crypto keys operations asymmetric pqc)
            (sagittarius crypto logic pqc)
            (sagittarius crypto digests)
            (sagittarius crypto random)
            (util bytevector))

(define-class <pqc-signature-state> (<buffered-signature-state>)
  ((context :init-keyword :context :reader pqc-state-context)
   (digest :init-keyword :digest :reader pqc-state-digest)
   (prng :init-keyword :prng :init-value #f :reader pqc-state-prng)
   (deterministic? :init-keyword :deterministic? :init-value #f
                   :reader pqc-state-deterministic?)))
(define-class <pqc-signer-state> (<pqc-signature-state> <signer-state>) ())
(define-class <pqc-verifier-state> (<pqc-signature-state> <verifier-state>) ())

(define approved-prehashes
  (list *digest:sha-224* *digest:sha-256* *digest:sha-384* *digest:sha-512*
        *digest:sha-512/224* *digest:sha-512/256*
        *digest:sha3-224* *digest:sha3-256* *digest:sha3-384* *digest:sha3-512*
        *digest:shake-128* *digest:shake-256*))

(define (check-options scheme key context digest fixed-digest der-encode)
  (when der-encode
    (assertion-violation 'make-signature-state "PQC signatures use raw encoding"))
  (unless (eq? scheme
               (pqc-parameter-identifier (pqc-key-parameter key)))
    (assertion-violation 'make-signature-state
                         "PQC key parameter set does not match signature scheme"))
  (unless (<= (bytevector-length context) 255)
    (assertion-violation 'make-signature-state "PQC context exceeds 255 bytes"))
  (when (and fixed-digest (not (eq? digest fixed-digest)))
    (assertion-violation 'make-signature-state "Signature scheme fixes the digest"))
  (when (and digest
             (or (not (= (pqc-parameter-family (pqc-key-parameter key)) 1))
                 (not (memq digest approved-prehashes))))
    (assertion-violation 'make-signature-state
                         "Unsupported PQC prehash; SLH-DSA modes use distinct keys")))

(define (prepare-message state)
  (define key (signature-state-key state))
  (define context (pqc-state-context state))
  (define digest (pqc-state-digest state))
  (define message (buffered-signature-state-signing-message! state))
  (if digest
      (let* ((pk (if (is-a? key <pqc-private-key>)
                     (pqc-private-key-public-key key) key))
             (tr (digest-message (make-message-digest *digest:shake-256*)
                                 (pqc-public-key-bytes pk) 64))
             (ph (digest-message (make-message-digest digest) message
                                 (if (eq? digest *digest:shake-128*) 32 64)))
             (oid (asn1-encodable->bytevector
                    (oid-string->der-object-identifier
                      (digest-descriptor-oid digest))))
             (mu (digest-message (make-message-digest *digest:shake-256*)
                   (bytevector-append tr
                     (u8-list->bytevector (list 1 (bytevector-length context)))
                     context oid ph) 64)))
        (values mu #vu8() #t))
      (values message context #f)))

(define-method signer-state->signature ((state <pqc-signer-state>))
  (define key (signature-state-key state))
  (define p (pqc-key-parameter key))
  (define family (pqc-parameter-family p))
  (define n (pqc-parameter-randomness-size p))
  (define (sign randomness)
    (let-values (((message context representative?) (prepare-message state)))
      (pqc-sign family (pqc-parameter-number p) (pqc-private-key-bytes key)
                 message context randomness representative?)))
  (if (pqc-state-deterministic? state)
      (let ((randomness (if (= family 1)
                            (make-bytevector n 0)
                            (bytevector-copy
                              (pqc-public-key-bytes
                                (pqc-private-key-public-key key)) 0 n))))
        (dynamic-wind (lambda () #f) (lambda () (sign randomness))
                      (lambda () (pqc-clear! randomness))))
      (pqc-call-with-randomness (pqc-state-prng state) n sign)))

(define-method verifier-state-verify-message ((state <pqc-verifier-state>)
                                               (signature <bytevector>))
  (let* ((key (signature-state-key state))
         (p (pqc-key-parameter key)))
    (let-values (((message context representative?) (prepare-message state)))
      (pqc-verify (pqc-parameter-family p) (pqc-parameter-number p)
                   (pqc-public-key-bytes key) message context signature
                   representative?))))

(define-syntax define-pqc-signature
  (syntax-rules ()
    ((_ name value key-scheme fixed-digest)
     (begin
       (define name value)
       (define-method make-signer-state
           ((m (eql name)) (key <pqc-private-key>)
            :key ((context bytevector?) #vu8())
                 (digest fixed-digest)
                 ((der-encode boolean?) #f)
                 (prng (secure-random-generator *prng:chacha20* 32))
                 ((deterministic? boolean?) #f))
         (check-options key-scheme key context digest fixed-digest der-encode)
         (pqc-private-key-bytes key)
         (make <pqc-signer-state> :key key :context (bytevector-copy context)
               :digest digest :prng prng :deterministic? deterministic?))
       (define-method make-verifier-state
           ((m (eql name)) (key <pqc-public-key>)
            :key ((context bytevector?) #vu8()) (digest fixed-digest)
                 ((der-encode boolean?) #f))
         (check-options key-scheme key context digest fixed-digest der-encode)
         (make <pqc-verifier-state> :key key :context (bytevector-copy context)
               :digest digest))))))
)
