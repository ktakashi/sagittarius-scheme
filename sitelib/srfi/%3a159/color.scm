;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a159/color.scm - Combinator Formatting (color)
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

(library (srfi :159 color)
    (export as-red as-blue as-green as-cyan as-yellow
	    as-magenta as-white as-black
	    as-bold as-underline)
    (import (rnrs)
	    (srfi :159 internal base))
;;;; color.scm
;; color.scm -- colored output
;; Copyright (c) 2006-2017 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (color->ansi x)
  (case x
    ((bold) "1")
    ((dark) "2")
    ((underline) "4")
    ((black) "30")
    ((red) "31")
    ((green) "32")
    ((yellow) "33")
    ((blue) "34")
    ((magenta) "35")
    ((cyan) "36")
    ((white) "37")
    (else "0")))

(define (ansi-escape color)
  (string-append (string (integer->char 27)) "[" (color->ansi color) "m"))

(define (colored new-color . args)
  (fn (color)
    (with ((color new-color))
      (each (ansi-escape new-color)
            (each-in-list args)
            (if (or (memq new-color '(bold underline))
                    (memq color '(bold underline)))
                (ansi-escape 'reset)
                nothing)
            (ansi-escape color)))))

(define (as-red . args) (colored 'red (each-in-list args)))
(define (as-blue . args) (colored 'blue (each-in-list args)))
(define (as-green . args) (colored 'green (each-in-list args)))
(define (as-cyan . args) (colored 'cyan (each-in-list args)))
(define (as-yellow . args) (colored 'yellow (each-in-list args)))
(define (as-magenta . args) (colored 'magenta (each-in-list args)))
(define (as-white . args) (colored 'white (each-in-list args)))
(define (as-black . args) (colored 'black (each-in-list args)))
(define (as-bold . args) (colored 'bold (each-in-list args)))
(define (as-underline . args) (colored 'underline (each-in-list args)))


)
