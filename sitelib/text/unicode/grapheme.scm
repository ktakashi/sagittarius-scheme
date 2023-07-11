;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/unicode/grapheme.scm - Unicode grapheme generator
;;;  
;;;   Copyright (c) 2023  Takashi Kato  <ktakashi@ymail.com>
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
(library (text unicode grapheme)
    (export grapheme-strategy)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (sagittarius generators)
	    (sagittarius char-sets grapheme)
	    (sagittarius char-sets emojis)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :127 lseqs))

(define $cr ($eqv? #\return))
(define $lf ($eqv? #\newline))
(define $control    ($char-set-contains? char-set:control))
(define $hangul-l   ($char-set-contains? char-set:hangul-l))
(define $hangul-v   ($char-set-contains? char-set:hangul-v))
(define $hangul-t   ($char-set-contains? char-set:hangul-t))
(define $hangul-lv  ($char-set-contains? char-set:hangul-lv))
(define $hangul-lvt ($char-set-contains? char-set:hangul-lvt))
(define $extend     ($char-set-contains? char-set:extend))
(define $zwj        ($char-set-contains? char-set:zwj))
(define $ri         ($char-set-contains? char-set:regional-indicator))
(define $spacing-mark ($char-set-contains? char-set:spacing-mark))
(define $prepend    ($char-set-contains? char-set:prepend))
(define $ext-pict   ($char-set-contains? char-set:extended-pictographic))

;; From Table 1b
;; extended grapheme cluster := crlf
;;                            | Control
;;                            | precore* core postcore*

(define $crlf ($seq $cr $lf ($return "\r\n")))
;; CR and LF is not Control, so need to explicitly added
(define $control ($do (c ($or $control $cr $lf)) ($return (string c))))

(define $precore $prepend)

;; postcore := [Extend ZWJ SpacingMark]
(define $postcore
  ($do (c ($or $extend $zwj $spacing-mark)) ($return (string c))))

;; hangul-syllable := L* (V+ | LV V* | LVT) T*
;;                  | L+
;;                  | T+
(define $hangul-syllable
  ($or ($let ((c0* ($many $hangul-l))
	      (c1* ($or ($many $hangul-v 1)
			($do (lv $hangul-lv) (v* ($many $hangul-v))
			     ($return (cons lv v*)))
			($do (lvt $hangul-lvt) ($return (list lvt)))))
	      (c2* ($many $hangul-t)))
	 ($return (string-append (list->string c0*)
				 (list->string c1*)
				 (list->string c2*))))
       ($do (c* ($many $hangul-l 1)) ($return (list->string c*)))
       ($do (c* ($many $hangul-t 1)) ($return (list->string c*)))))

;; RI-Sequence := RI RI
(define $ri-sequence ($do (c+ ($many $ri 1)) ($return (list->string c+))))

;; xpicto-sequence := \p{Extended_Pictographic} (Extend* ZWJ \p{Extended_Pictographic})*
(define $xpicto-sequence
  ($let ((e1 $ext-pict)
	 (e* ($many ($do (e* ($many $extend)) (zwj $zwj) (ext $ext-pict)
			 ($return (list->string `(,@e* ,zwj ,ext)))))))
    ($return (apply string-append (string e1) e*))))


;; core := hangul-syllable
;;       | ri-sequence
;;       | xpicto-sequence
;;       | [^Control CR LF]
(define $core
  ($or $hangul-syllable
       $ri-sequence
       $xpicto-sequence
       ($do (($not ($or $control $cr $lf))) (c $any) ($return (string c)))))
(define $grapheme
  ;; ignore GB1 and GB2...
  ($or $crlf
       $control
       ($let ((pre* ($many $precore))
	      (core $core)
	      (post* ($many $postcore)))
	 ($return (string-append (list->string pre*)
				 core
				 (string-concatenate post*))))
       ;; default break
       ($do (c $any) ($return (string c)))))

;; [Generator] -> [Generator]
;; extended grapheme strategy :)
(define (grapheme-strategy generator)
  (let ((lseq (generator->lseq generator)))
    (lambda ()
      (if (null? lseq)
	  (eof-object)
	  (let-values (((s v nl) ($grapheme lseq)))
	    (cond ((parse-success? s)
		   (set! lseq nl)
		   v)
		  (else
		   (lseq-realize lseq)
		   (error 'grapheme-strategy
			  "Invalid character" (list->string lseq)))))))))
)
	     
