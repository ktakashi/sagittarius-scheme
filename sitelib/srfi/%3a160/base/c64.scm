;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a160/base/c64.scm - Homogeneous numeric vector datatypes (base)
;;;  
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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
(library (srfi :160 base c64)
    (export make-c64vector c64vector c64vector? <c64vector>

	    c64vector-length c64vector-ref c64vector-set!
	    c64vector->list list->c64vector)
    (import (rnrs)
	    (srfi :4 numeric-vectors))

(define-tagged-vector "c64" 2
  make-f32vector f32vector-length equal?
  f32vector-ref-f32-as-complex
  f32vector-set-complex-as-f32!)

(define (f32vector-ref-f32-as-complex vec index)
  (let ((real (f32vector-ref vec index))
	(imag (f32vector-ref vec (+ index 1))))
    (if (zero? imag)
	real
	(make-rectangular real imag))))

(define (f32vector-set-complex-as-f32! vec index value)
  (let ((real (real-part value))
	(imag (imag-part value)))
    (f32vector-set! vec index real)
    (f32vector-set! vec (+ index 1) imag)))

)
