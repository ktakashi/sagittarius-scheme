;;; -*- mode:scheme; coding:utf-8; -*-
;;; SLH-DSA and HashSLH-DSA signatures (FIPS 205, RFC 9909).
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
(library (sagittarius crypto signatures slhdsa)
    (export *signature:slh-dsa-sha2-128s* *signature:slh-dsa-sha2-128f*
            *signature:slh-dsa-sha2-192s* *signature:slh-dsa-sha2-192f*
            *signature:slh-dsa-sha2-256s* *signature:slh-dsa-sha2-256f*
            *signature:slh-dsa-shake-128s* *signature:slh-dsa-shake-128f*
            *signature:slh-dsa-shake-192s* *signature:slh-dsa-shake-192f*
            *signature:slh-dsa-shake-256s* *signature:slh-dsa-shake-256f*
            *signature:hash-slh-dsa-sha2-128s-with-sha256*
            *signature:hash-slh-dsa-sha2-128f-with-sha256*
            *signature:hash-slh-dsa-sha2-192s-with-sha512*
            *signature:hash-slh-dsa-sha2-192f-with-sha512*
            *signature:hash-slh-dsa-sha2-256s-with-sha512*
            *signature:hash-slh-dsa-sha2-256f-with-sha512*
            *signature:hash-slh-dsa-shake-128s-with-shake128*
            *signature:hash-slh-dsa-shake-128f-with-shake128*
            *signature:hash-slh-dsa-shake-192s-with-shake256*
            *signature:hash-slh-dsa-shake-192f-with-shake256*
            *signature:hash-slh-dsa-shake-256s-with-shake256*
            *signature:hash-slh-dsa-shake-256f-with-shake256*)
    (import (rnrs)
            (sagittarius crypto keys operations asymmetric slhdsa)
            (sagittarius crypto signatures pqc))

(define-syntax define-slh-signature
  (syntax-rules ()
    ((_ signature key) (define-pqc-signature signature key key #f))))
(define-slh-signature *signature:slh-dsa-sha2-128s* *key:slh-dsa-sha2-128s*)
(define-slh-signature *signature:slh-dsa-sha2-128f* *key:slh-dsa-sha2-128f*)
(define-slh-signature *signature:slh-dsa-sha2-192s* *key:slh-dsa-sha2-192s*)
(define-slh-signature *signature:slh-dsa-sha2-192f* *key:slh-dsa-sha2-192f*)
(define-slh-signature *signature:slh-dsa-sha2-256s* *key:slh-dsa-sha2-256s*)
(define-slh-signature *signature:slh-dsa-sha2-256f* *key:slh-dsa-sha2-256f*)
(define-slh-signature *signature:slh-dsa-shake-128s* *key:slh-dsa-shake-128s*)
(define-slh-signature *signature:slh-dsa-shake-128f* *key:slh-dsa-shake-128f*)
(define-slh-signature *signature:slh-dsa-shake-192s* *key:slh-dsa-shake-192s*)
(define-slh-signature *signature:slh-dsa-shake-192f* *key:slh-dsa-shake-192f*)
(define-slh-signature *signature:slh-dsa-shake-256s* *key:slh-dsa-shake-256s*)
(define-slh-signature *signature:slh-dsa-shake-256f* *key:slh-dsa-shake-256f*)
(define-slh-signature *signature:hash-slh-dsa-sha2-128s-with-sha256*
  *key:hash-slh-dsa-sha2-128s-with-sha256*)
(define-slh-signature *signature:hash-slh-dsa-sha2-128f-with-sha256*
  *key:hash-slh-dsa-sha2-128f-with-sha256*)
(define-slh-signature *signature:hash-slh-dsa-sha2-192s-with-sha512*
  *key:hash-slh-dsa-sha2-192s-with-sha512*)
(define-slh-signature *signature:hash-slh-dsa-sha2-192f-with-sha512*
  *key:hash-slh-dsa-sha2-192f-with-sha512*)
(define-slh-signature *signature:hash-slh-dsa-sha2-256s-with-sha512*
  *key:hash-slh-dsa-sha2-256s-with-sha512*)
(define-slh-signature *signature:hash-slh-dsa-sha2-256f-with-sha512*
  *key:hash-slh-dsa-sha2-256f-with-sha512*)
(define-slh-signature *signature:hash-slh-dsa-shake-128s-with-shake128*
  *key:hash-slh-dsa-shake-128s-with-shake128*)
(define-slh-signature *signature:hash-slh-dsa-shake-128f-with-shake128*
  *key:hash-slh-dsa-shake-128f-with-shake128*)
(define-slh-signature *signature:hash-slh-dsa-shake-192s-with-shake256*
  *key:hash-slh-dsa-shake-192s-with-shake256*)
(define-slh-signature *signature:hash-slh-dsa-shake-192f-with-shake256*
  *key:hash-slh-dsa-shake-192f-with-shake256*)
(define-slh-signature *signature:hash-slh-dsa-shake-256s-with-shake256*
  *key:hash-slh-dsa-shake-256s-with-shake256*)
(define-slh-signature *signature:hash-slh-dsa-shake-256f-with-shake256*
  *key:hash-slh-dsa-shake-256f-with-shake256*)
)
