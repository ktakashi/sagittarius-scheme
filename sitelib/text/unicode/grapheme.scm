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
	    (sagittarius char-sets incb)
	    (sagittarius char-sets emojis)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :127 lseqs))

(define $cr ($eqv? #\return))
(define $lf ($eqv? #\newline))
(define $control0   ($char-set-contains? char-set:control))
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
(define $incb:c     ($char-set-contains? char-set:incb-consonant))
(define $incb:e     ($char-set-contains? char-set:incb-extend))
(define $incb:l     ($char-set-contains? char-set:incb-linker))

;; From Table 1b
;; extended grapheme cluster := crlf
;;                            | Control
;;                            | precore* core postcore*

(define $crlf ($seq $cr $lf ($return "\r\n")))
;; CR and LF is not Control, so need to explicitly added
(define $control ($or $control0 $cr $lf))

(define $precore $prepend)

;; postcore := [Extend ZWJ SpacingMark]
(define $postcore ($or $extend $zwj $spacing-mark))

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
	 ($return `(,@c0* ,@c1* ,@c2*)))
       ($many $hangul-l 1)
       ($many $hangul-t 1)))

;; RI-Sequence := RI RI
(define $ri-sequence ($do (ri0 $ri) (ri1 $ri) ($return (list ri0 ri1))))
;; The below is incorrect, so GB12 and GB13 is a bit misleading to me.
#;(define $ri-sequence
  ($do (2ri* ($many $riri 1)) ($return (string-concatenate 2ri*))))

;; xpicto-sequence := \p{Extended_Pictographic} (Extend* ZWJ \p{Extended_Pictographic})*
(define $xpicto-sequence
  ($let ((e1 $ext-pict)
	 (e* ($many ($do (e* ($many $extend)) (zwj $zwj) (ext $ext-pict)
			 ($return `(,@e* ,zwj ,ext))))))
    ($return (cons e1 (append-map values e*)))))

;; conjunctCluster := \p{InCB=Consonant}
;;                    ([\p{InCB=Extend} \p{InCB=Linker}]*
;;                     \p{InCB=Linker}
;;                     [\p{InCB=Extend} \p{InCB=Linker}]*
;;                     \p{InCB=Consonant})+
(define $el ($or $incb:e $incb:l))
(define ($conjunct-cluster ol)
  (define (check-el l state)
    (let-values (((s v nl) ($el l)))
      (if (parse-success? s)
	  (values state v nl)
	  (return-expect $el l)))) 
  (define (check l state)
    (let-values (((s v nl) ($incb:c l)))
      (cond ((and (parse-success? s) (eq? state 'l))
	     (values 'end v nl))
	    ((not state)
	     (let-values (((s v nl) ($incb:l l)))
	       (if (parse-success? s)
		   (values 'l v nl)
		   (check-el l state))))
	    (else (check-el l state)))))
  (define (parse-one nl vs)
    (let loop ((vs vs) (l nl) (state #f))
      (let-values (((s v nl) (check l state)))
	(cond ((parse-expect? s) (return-expect $el nl))
	      ((eq? s 'end) (return-result (cons v vs) nl))
	      (else (loop (cons v vs) nl s))))))
  (let-values (((s v nl) ($incb:c ol)))
    (if (parse-success? s)
	(let loop ((vs (list v)) (l nl) (first? #t))
	  (let-values (((s v nl) (parse-one l vs)))
	    (cond ((parse-success? s) (loop v nl #f))
		  (first? (return-expect $conjunct-cluster ol))
		  (else (return-result (reverse! vs) l)))))
	(return-expect $incb:c ol))))

;; core := hangul-syllable
;;       | ri-sequence
;;       | xpicto-sequence
;;       | [^Control CR LF]
(define $core
  ($or $hangul-syllable
       $ri-sequence
       $xpicto-sequence
       $conjunct-cluster
       ($do (($not ($or $control $cr $lf))) (c $any) ($return (list c)))))
(define $grapheme
  ($or $crlf
       ($do (c $control) ($return (string c)))
       ($let ((pre* ($many $precore))
	      (core $core)
	      (post* ($many $postcore)))
	 ($return (list->string `(,@pre* ,@core ,@post*))))
       ;; GB9a (this can't be handled by the regexp definition of Unicode)
       ($do (p* ($many $precore)) ($return (list->string p*)))
       ;; GB999 (default break)
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
	     
