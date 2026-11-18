;;; -*- mode:scheme; coding:utf-8; -*-
;;; ML-DSA and HashML-DSA signatures (FIPS 204).
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
(library (sagittarius crypto signatures mldsa)
    (export *signature:ml-dsa-44* *signature:ml-dsa-65* *signature:ml-dsa-87*
            *signature:hash-ml-dsa-44-with-sha512*
            *signature:hash-ml-dsa-65-with-sha512*
            *signature:hash-ml-dsa-87-with-sha512*)
    (import (rnrs)
            (sagittarius crypto keys operations asymmetric mldsa)
            (sagittarius crypto signatures pqc)
            (sagittarius crypto digests))

(define-pqc-signature *signature:ml-dsa-44* *key:ml-dsa-44*
  *key:ml-dsa-44* #f)
(define-pqc-signature *signature:ml-dsa-65* *key:ml-dsa-65*
  *key:ml-dsa-65* #f)
(define-pqc-signature *signature:ml-dsa-87* *key:ml-dsa-87*
  *key:ml-dsa-87* #f)
(define-pqc-signature *signature:hash-ml-dsa-44-with-sha512*
  :hash-ml-dsa-44-with-sha512 *key:ml-dsa-44* *digest:sha-512*)
(define-pqc-signature *signature:hash-ml-dsa-65-with-sha512*
  :hash-ml-dsa-65-with-sha512 *key:ml-dsa-65* *digest:sha-512*)
(define-pqc-signature *signature:hash-ml-dsa-87-with-sha512*
  :hash-ml-dsa-87-with-sha512 *key:ml-dsa-87* *digest:sha-512*)
)
