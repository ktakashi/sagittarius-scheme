;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; util/vector.scm - Vector utility
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

;; This library defines procedures which are not supported by SRFI-133
;; (or SRFI-43) but very common case that I use.
(library (util vector)
    (export vector-filter vector-remove vector-find)
    (import (rnrs)
	    (sagittarius) ;; for reverse!
	    )

;; vector-filter : (a -> bool, vector) -> vector
(define (vector-filter pred vec)
  (do ((len (vector-length vec))
       (i 0 (+ i 1))
       (r '() (let ((e (vector-ref vec i))) (if (pred e) (cons e r) r))))
      ((= i len) (list->vector (reverse! r)))))

;; vector-remove : (a -> bool, vector) -> vector
(define (vector-remove pred vec)
  (vector-filter (lambda (e) (not (pred e))) vec))

(define (vector-find pred vec)
  (define len (vector-length vec))
  (let loop ((i 0))
    (and (not (= i len))
	 (let ((e (vector-ref vec i)))
	   (or (and (pred e) e)
	       (loop (+ i 1)))))))
)
