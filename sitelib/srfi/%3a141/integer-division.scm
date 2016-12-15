;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a141/integer-division.scm - Integer division
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :141 integer-division)
  (export floor/ floor-quotient floor-remainder
	  ceiling/ ceiling-quotient ceiling-remainder
	  truncate/ truncate-quotient truncate-remainder
	  round/ round-quotient round-remainder
	  euclidean/ euclidean-quotient euclidean-remainder
	  balanced/ balanced-quotient balanced-remainder)
  (import (rnrs))

(define-syntax check-integer
  (syntax-rules ()
    ((_ who v)
     (unless (integer? v) (assertion-violation 'who "integer required" v)))))

(define-syntax check-zero
  (syntax-rules ()
    ((_ who d)
     (when (zero? d)
       (assertion-violation 'who "denominator must not be zero")))))

(define-syntax define/nd
  (syntax-rules ()
    ((_ (name n d) body ...)
     (define (name n d)
       (check-integer name n)
       (check-integer name d)
       (check-zero name d)
       body ...))))
(define-syntax define-quotient&remainder
  (syntax-rules ()
    ((_ quotient remainder /)
     (begin
       (define (quotient n d) (let-values (((q r) (/ n d))) q))
       (define (remainder n d) (let-values (((q r) (/ n d))) r))))))
       
(define-syntax define/
  (syntax-rules ()
    ((_ name op)
     (define (name n d)
       (let ((q (op (/ n d))))
	 (values q (- n (* d q))))))))

(define/ floor/ floor)
(define-quotient&remainder floor-quotient floor-remainder floor/)

(define/ ceiling/ ceiling)
(define-quotient&remainder ceiling-quotient ceiling-remainder ceiling/)

(define/ truncate/ truncate)
(define-quotient&remainder truncate-quotient truncate-remainder truncate/)

(define/ round/ round)
(define-quotient&remainder round-quotient round-remainder round/)

(define/nd (euclidean/ n d) (div-and-mod n d))
(define/nd (euclidean-quotient n d) (div n d))
(define/nd (euclidean-remainder n d) (mod n d))

(define/nd (balanced/ n d) (div0-and-mod0 n d))
(define/nd (balanced-quotient n d) (div0 n d))
(define/nd (balanced-remainder n d) (mod0 n d))

)
	  
