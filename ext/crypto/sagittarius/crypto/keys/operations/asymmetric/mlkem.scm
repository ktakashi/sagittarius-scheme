;;; -*- mode:scheme; coding:utf-8; -*-
;;; ML-KEM key operations (FIPS 203, RFC 9935).
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
(library (sagittarius crypto keys operations asymmetric mlkem)
    (export *key:ml-kem-512* *key:ml-kem-768* *key:ml-kem-1024*
            <ml-kem-key> ml-kem-key?
            <ml-kem-public-key> ml-kem-public-key?
            <ml-kem-private-key> ml-kem-private-key?
            ml-kem-private-key-public-key
            encapsulate decapsulate)
    (import (rnrs) (clos user)
            (sagittarius crypto keys operations asymmetric apis)
            (sagittarius crypto keys operations asymmetric pqc)
            (sagittarius crypto logic pqc)
            (sagittarius crypto random))

(define-class <ml-kem-key> (<pqc-key>) ())
(define-class <ml-kem-public-key> (<pqc-public-key> <ml-kem-key>) ())
(define-class <ml-kem-private-key> (<pqc-private-key> <ml-kem-key>) ())
(define (ml-kem-key? o) (is-a? o <ml-kem-key>))
(define (ml-kem-public-key? o) (is-a? o <ml-kem-public-key>))
(define (ml-kem-private-key? o) (is-a? o <ml-kem-private-key>))
(define ml-kem-private-key-public-key pqc-private-key-public-key)

(define-pqc-parameter *key:ml-kem-512* :ml-kem-512 ml-kem-512
  0 0 "2.16.840.1.101.3.4.4.1" <ml-kem-public-key> <ml-kem-private-key>)
(define-pqc-parameter *key:ml-kem-768* :ml-kem-768 ml-kem-768
  0 1 "2.16.840.1.101.3.4.4.2" <ml-kem-public-key> <ml-kem-private-key>)
(define-pqc-parameter *key:ml-kem-1024* :ml-kem-1024 ml-kem-1024
  0 2 "2.16.840.1.101.3.4.4.3" <ml-kem-public-key> <ml-kem-private-key>)

(define-syntax define-kem
  (syntax-rules ()
    ((_ name parameter)
     (begin
       (define-method encapsulate ((m (eql name)) (key <ml-kem-public-key>)
                                    :key
                                    (prng (secure-random-generator
                                            *prng:chacha20* 32)))
         (pqc-check-parameter parameter key)
         (pqc-call-with-randomness prng 32
           (lambda (randomness)
             (pqc-encapsulate (pqc-parameter-number parameter)
                               (pqc-public-key-bytes key) randomness))))
       (define-method decapsulate ((m (eql name)) (key <ml-kem-private-key>)
                                    (ciphertext <bytevector>))
         (pqc-check-parameter parameter key)
         (pqc-decapsulate (pqc-parameter-number parameter)
                           (pqc-private-key-bytes key) ciphertext))))))
(define-kem *key:ml-kem-512* ml-kem-512)
(define-kem *key:ml-kem-768* ml-kem-768)
(define-kem *key:ml-kem-1024* ml-kem-1024)
)
