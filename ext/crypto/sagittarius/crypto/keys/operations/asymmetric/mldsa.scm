;;; -*- mode:scheme; coding:utf-8; -*-
;;; ML-DSA key operations (FIPS 204, RFC 9881).
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
(library (sagittarius crypto keys operations asymmetric mldsa)
    (export *key:ml-dsa-44* *key:ml-dsa-65* *key:ml-dsa-87*
            <ml-dsa-key> ml-dsa-key?
            <ml-dsa-public-key> ml-dsa-public-key?
            <ml-dsa-private-key> ml-dsa-private-key?
            ml-dsa-private-key-public-key)
    (import (rnrs) (clos user)
            (sagittarius crypto keys operations asymmetric pqc))

(define-class <ml-dsa-key> (<pqc-key>) ())
(define-class <ml-dsa-public-key> (<pqc-public-key> <ml-dsa-key>) ())
(define-class <ml-dsa-private-key> (<pqc-private-key> <ml-dsa-key>) ())
(define (ml-dsa-key? o) (is-a? o <ml-dsa-key>))
(define (ml-dsa-public-key? o) (is-a? o <ml-dsa-public-key>))
(define (ml-dsa-private-key? o) (is-a? o <ml-dsa-private-key>))
(define ml-dsa-private-key-public-key pqc-private-key-public-key)

(define-pqc-parameter *key:ml-dsa-44* :ml-dsa-44 ml-dsa-44
  1 0 "2.16.840.1.101.3.4.3.17" <ml-dsa-public-key> <ml-dsa-private-key>)
(define-pqc-parameter *key:ml-dsa-65* :ml-dsa-65 ml-dsa-65
  1 1 "2.16.840.1.101.3.4.3.18" <ml-dsa-public-key> <ml-dsa-private-key>)
(define-pqc-parameter *key:ml-dsa-87* :ml-dsa-87 ml-dsa-87
  1 2 "2.16.840.1.101.3.4.3.19" <ml-dsa-public-key> <ml-dsa-private-key>)
)
