;;; -*- mode:scheme; coding:utf-8; -*-
;;; SLH-DSA key operations (FIPS 205, RFC 9909).
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
(library (sagittarius crypto keys operations asymmetric slhdsa)
    (export *key:slh-dsa-sha2-128s* *key:slh-dsa-sha2-128f*
            *key:slh-dsa-sha2-192s* *key:slh-dsa-sha2-192f*
            *key:slh-dsa-sha2-256s* *key:slh-dsa-sha2-256f*
            *key:slh-dsa-shake-128s* *key:slh-dsa-shake-128f*
            *key:slh-dsa-shake-192s* *key:slh-dsa-shake-192f*
            *key:slh-dsa-shake-256s* *key:slh-dsa-shake-256f*
            *key:hash-slh-dsa-sha2-128s-with-sha256*
            *key:hash-slh-dsa-sha2-128f-with-sha256*
            *key:hash-slh-dsa-sha2-192s-with-sha512*
            *key:hash-slh-dsa-sha2-192f-with-sha512*
            *key:hash-slh-dsa-sha2-256s-with-sha512*
            *key:hash-slh-dsa-sha2-256f-with-sha512*
            *key:hash-slh-dsa-shake-128s-with-shake128*
            *key:hash-slh-dsa-shake-128f-with-shake128*
            *key:hash-slh-dsa-shake-192s-with-shake256*
            *key:hash-slh-dsa-shake-192f-with-shake256*
            *key:hash-slh-dsa-shake-256s-with-shake256*
            *key:hash-slh-dsa-shake-256f-with-shake256*
            <slh-dsa-key> slh-dsa-key?
            <slh-dsa-public-key> slh-dsa-public-key?
            <slh-dsa-private-key> slh-dsa-private-key?
            slh-dsa-private-key-public-key)
    (import (rnrs) (clos user)
            (sagittarius crypto keys operations asymmetric pqc))

(define-class <slh-dsa-key> (<pqc-key>) ())
(define-class <slh-dsa-public-key> (<pqc-public-key> <slh-dsa-key>) ())
(define-class <slh-dsa-private-key> (<pqc-private-key> <slh-dsa-key>) ())
(define (slh-dsa-key? o) (is-a? o <slh-dsa-key>))
(define (slh-dsa-public-key? o) (is-a? o <slh-dsa-public-key>))
(define (slh-dsa-private-key? o) (is-a? o <slh-dsa-private-key>))
(define slh-dsa-private-key-public-key pqc-private-key-public-key)

(define-syntax define-slh
  (syntax-rules ()
    ((_ name keyword descriptor number oid)
     (define-pqc-parameter name keyword descriptor 2 number oid
       <slh-dsa-public-key> <slh-dsa-private-key>))))
(define-slh *key:slh-dsa-sha2-128s* :slh-dsa-sha2-128s sha2-128s
  0 "2.16.840.1.101.3.4.3.20")
(define-slh *key:slh-dsa-sha2-128f* :slh-dsa-sha2-128f sha2-128f
  1 "2.16.840.1.101.3.4.3.21")
(define-slh *key:slh-dsa-sha2-192s* :slh-dsa-sha2-192s sha2-192s
  2 "2.16.840.1.101.3.4.3.22")
(define-slh *key:slh-dsa-sha2-192f* :slh-dsa-sha2-192f sha2-192f
  3 "2.16.840.1.101.3.4.3.23")
(define-slh *key:slh-dsa-sha2-256s* :slh-dsa-sha2-256s sha2-256s
  4 "2.16.840.1.101.3.4.3.24")
(define-slh *key:slh-dsa-sha2-256f* :slh-dsa-sha2-256f sha2-256f
  5 "2.16.840.1.101.3.4.3.25")
(define-slh *key:slh-dsa-shake-128s* :slh-dsa-shake-128s shake-128s
  6 "2.16.840.1.101.3.4.3.26")
(define-slh *key:slh-dsa-shake-128f* :slh-dsa-shake-128f shake-128f
  7 "2.16.840.1.101.3.4.3.27")
(define-slh *key:slh-dsa-shake-192s* :slh-dsa-shake-192s shake-192s
  8 "2.16.840.1.101.3.4.3.28")
(define-slh *key:slh-dsa-shake-192f* :slh-dsa-shake-192f shake-192f
  9 "2.16.840.1.101.3.4.3.29")
(define-slh *key:slh-dsa-shake-256s* :slh-dsa-shake-256s shake-256s
  10 "2.16.840.1.101.3.4.3.30")
(define-slh *key:slh-dsa-shake-256f* :slh-dsa-shake-256f shake-256f
  11 "2.16.840.1.101.3.4.3.31")
(define-slh *key:hash-slh-dsa-sha2-128s-with-sha256*
  :hash-slh-dsa-sha2-128s-with-sha256 hash-sha2-128s
  12 "2.16.840.1.101.3.4.3.35")
(define-slh *key:hash-slh-dsa-sha2-128f-with-sha256*
  :hash-slh-dsa-sha2-128f-with-sha256 hash-sha2-128f
  13 "2.16.840.1.101.3.4.3.36")
(define-slh *key:hash-slh-dsa-sha2-192s-with-sha512*
  :hash-slh-dsa-sha2-192s-with-sha512 hash-sha2-192s
  14 "2.16.840.1.101.3.4.3.37")
(define-slh *key:hash-slh-dsa-sha2-192f-with-sha512*
  :hash-slh-dsa-sha2-192f-with-sha512 hash-sha2-192f
  15 "2.16.840.1.101.3.4.3.38")
(define-slh *key:hash-slh-dsa-sha2-256s-with-sha512*
  :hash-slh-dsa-sha2-256s-with-sha512 hash-sha2-256s
  16 "2.16.840.1.101.3.4.3.39")
(define-slh *key:hash-slh-dsa-sha2-256f-with-sha512*
  :hash-slh-dsa-sha2-256f-with-sha512 hash-sha2-256f
  17 "2.16.840.1.101.3.4.3.40")
(define-slh *key:hash-slh-dsa-shake-128s-with-shake128*
  :hash-slh-dsa-shake-128s-with-shake128 hash-shake-128s
  18 "2.16.840.1.101.3.4.3.41")
(define-slh *key:hash-slh-dsa-shake-128f-with-shake128*
  :hash-slh-dsa-shake-128f-with-shake128 hash-shake-128f
  19 "2.16.840.1.101.3.4.3.42")
(define-slh *key:hash-slh-dsa-shake-192s-with-shake256*
  :hash-slh-dsa-shake-192s-with-shake256 hash-shake-192s
  20 "2.16.840.1.101.3.4.3.43")
(define-slh *key:hash-slh-dsa-shake-192f-with-shake256*
  :hash-slh-dsa-shake-192f-with-shake256 hash-shake-192f
  21 "2.16.840.1.101.3.4.3.44")
(define-slh *key:hash-slh-dsa-shake-256s-with-shake256*
  :hash-slh-dsa-shake-256s-with-shake256 hash-shake-256s
  22 "2.16.840.1.101.3.4.3.45")
(define-slh *key:hash-slh-dsa-shake-256f-with-shake256*
  :hash-slh-dsa-shake-256f-with-shake256 hash-shake-256f
  23 "2.16.840.1.101.3.4.3.46")
)
