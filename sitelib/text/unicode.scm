;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/unicode.scm - Unicode related
;;;  
;;;   Copyright (c) 2020-2023  Takashi Kato  <ktakashi@ymail.com>
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
(library (text unicode)
    (export unicode-char-width unicode-terminal-width

	    string->unicode-break-generator
	    port->unicode-break-generator
	    char-generator->unicode-break-generator

	    grapheme-strategy
	    )
    (import (rnrs)
	    (peg)
	    (sagittarius generators)
	    (text unicode grapheme))

(define (string->unicode-break-generator string break-strategy)
  (char-generator->unicode-break-generator
   (string->generator string) break-strategy))
(define (port->unicode-break-generator port break-strategy)
  (char-generator->unicode-break-generator
   (port->char-generator port) break-strategy))

(define (char-generator->unicode-break-generator generator break-strategy)
  (break-strategy generator))

;;;;; unicode.scm
;; unicode.scm -- Unicode character width and ANSI escape support
;; Copyright (c) 2006-2017 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; a condensed non-spacing mark range from UnicodeData.txt (chars with
;; the Mn property) - generated partially by hand, should automate
;; this better

(define low-non-spacing-chars
  #vu8(
#xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#x78    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0 #xfe #xff #xff #xff #xff #xff #x1f    0    0    0    0    0    0    0
   0    0 #x3f    0    0    0    0    0    0 #xf8 #xff #x01    0    0 #x01    0
   0    0    0    0    0    0    0    0    0    0 #xc0 #xff #xff #x3f    0    0
   0    0 #x02    0    0    0 #xff #xff #xff #x07    0    0    0    0    0    0
   0    0    0    0 #xc0 #xff #x01    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#x06    0    0    0    0    0    0 #x10 #xfe #x21 #x1e    0 #x0c    0    0    0
#x02    0    0    0    0    0    0 #x10 #x1e #x20    0    0 #x0c    0    0    0
#x06    0    0    0    0    0    0 #x10 #xfe #x3f    0    0    0    0 #x03    0
#x06    0    0    0    0    0    0 #x30 #xfe #x21    0    0 #x0c    0    0    0
#x02    0    0    0    0    0    0 #x90 #x0e #x20 #x40    0    0    0    0    0
#x04    0    0    0    0    0    0    0    0 #x20    0    0    0    0    0    0
   0    0    0    0    0    0    0 #xc0 #xc1 #xff #x7f    0    0    0    0    0
   0    0    0    0    0    0    0 #x10 #x40 #x30    0    0    0    0    0    0
   0    0    0    0    0    0    0    0 #x0e #x20    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0 #x04 #x7c    0    0    0    0    0
   0    0    0    0    0    0 #xf2 #x07 #x80 #x7f    0    0    0    0    0    0
   0    0    0    0    0    0 #xf2 #x1f    0 #x3f    0    0    0    0    0    0
   0    0    0 #x03    0    0 #xa0 #x02    0    0    0    0    0    0 #xfe #x7f
#xdf    0 #xff #xff #xff #xff #xff #x1f #x40    0    0    0    0    0    0    0
   0    0    0    0    0 #xe0 #xfd #x02    0    0    0 #x03    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0 #x1c    0    0    0 #x1c    0    0    0 #x0c    0    0    0 #x0c    0
   0    0    0    0    0    0 #x80 #x3f #x40 #xfe #x0f #x20    0    0    0    0
   0 #x38    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0 #x02    0    0    0    0    0    0    0    0    0    0
   0    0    0    0 #x87 #x01 #x04 #x0e    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0 #xff #x1f #xe2 #x07
       ))

(define (unicode-char-width c)
  (let ((ci (char->integer c)))
    (cond
      ;; hand-checked ranges from EastAsianWidth.txt
      ((<= #x1100 ci #x115F) 2) ; Hangul
      ((<= #x2E80 ci #x4DB5) 2) ; CJK
      ((<= #x4E00 ci #xA4C6) 2)
      ((<= #xAC00 ci #xD7A3) 2) ; Hangul
      ((<= #xF900 ci #xFAD9) 2) ; CJK compat
      ((<= #xFE10 ci #xFE6B) 2)
      ((<= #xFF01 ci #xFF60) 2)
      ((<= #xFFE0 ci #xFFE6) 2)
      ((<= #x20000 ci #x30000) 2)
      ;; non-spacing mark (Mn) ranges from UnicodeData.txt
      ((<= #x0300 ci #x3029)
       ;; inlined bit-vector-ref for portability
       (let* ((i (- ci #x0300))
              (byte (div i 8))
              (off (mod i 8)))
         (if (zero? (bitwise-and (bytevector-u8-ref low-non-spacing-chars byte)
                                 (bitwise-arithmetic-shift 1 off)))
             1
             0)))
      ((<= #x302A ci #x302F) 0)
      ((<= #x3099 ci #x309A) 0)
      ((= #xFB1E ci) 0)
      ((<= #xFE00 ci #xFE23) 0)
      ((<= #x1D167 ci #x1D169) 0)
      ((<= #x1D17B ci #x1D182) 0)
      ((<= #x1D185 ci #x1D18B) 0)
      ((<= #x1D1AA ci #x1D1AD) 0)
      ((<= #xE0100 ci #xE01EF) 0)
      (else 1))))

(define (unicode-terminal-width str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (string-length str))))
    (let lp1 ((i start) (width 0))
      (if (>= i end)
          width
          (let ((c (string-ref str i)))
            (cond
              ;; ANSI escapes
              ((and (= 27 (char->integer c)) ; esc
                    (< (+ i 1) end)
                    (eqv? #\[ (string-ref str (+ i 1))))
               (let lp2 ((i (+ i 2)))
                 (cond ((>= i end) width)
                       ((memv (string-ref str i) '(#\m #\newline))
                        (lp1 (+ i 1) width))
                       (else (lp2 (+ i 1))))))
              ;; unicode characters
              ((>= (char->integer c) #x80)
               (lp1 (+ i 1) (+ width (unicode-char-width c))))
              ;; normal ASCII
              (else (lp1 (+ i 1) (+ width 1)))))))))

)
