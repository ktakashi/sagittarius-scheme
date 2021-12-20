;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/format/markdown/parser.scm - Markdown parser
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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
;; we return SXML based on CommonMark.dtd
(library (sagittarius document format markdown parser)
    (export parse-markdown)
    (import (rnrs)
	    (peg)
	    (srfi :14 char-sets)
	    (sagittarius document input))

(define *namespace* '(xmlns "http://commonmark.org/xml/1.0"))

(define $nl ($or ($input-eqv? #\newline)
		 ($input-eqv? #\return) ($optional ($input-eqv? #\newline))))
(define $eol ($or $eof $nl))
(define $ws ($input-eqv? #\space))
(define $ws+ ($many ($input-eqv? #\space) 1))
(define $non-indent-space ($or ($many $ws 0 3)))

(define *title-char-set*
  (char-set-difference char-set:full char-set:iso-control))
(define $title-char ($input-char-set-contains *title-char-set*))
(define *id-char-set*
  (char-set-difference *title-char-set* (string->char-set "\"'")))
(define $id-char ($input-char-set-contains *id-char-set*))
(define ($string p at-least)
  ($let ((c* ($many p at-least))) ($return (list->string (map car c*)))))

(define $thematic-break
  ($let ((loc $location)
	 ( $non-indent-space )
	 ( ($or ($many ($input-eqv? #\*) 3)
		($many ($input-eqv? #\-) 3)
		($many ($input-eqv? #\_) 3)) )
	 ( $eol ))
    ($return `(thematic_break (@ ,@loc)))))

(define $atx-heading
  ($let ((loc $location)
	 ( $non-indent-space )
	 (level ($or ($seq ($input-eqv? #\#) $ws+ ($return 1))
		     ($seq ($input-token "##") $ws+ ($return 2))
		     ($seq ($input-token "###") $ws+ ($return 3))
		     ($seq ($input-token "####") $ws+ ($return 4))
		     ($seq ($input-token "#####") $ws+ ($return 5))
		     ($seq ($input-token "######") $ws+ ($return 6))))
	 (title ($string $title-char 1))
	 (id ($optional ($let (( $ws+ )
			       ( ($input-token "{#") )
			       (s ($string $id-char 1))
			       ( ($input-eqv? #\}) ))
			  ($return s))))
	 ( $eol ))
   ($return `(heading (@ ,@loc (level ,level) ,@(if id `((id ,id)) '()))
	       ,title))))
			      

(define $heading
  ($or $atx-heading
       ;; $setext-heading
       ))

(define $leaf-block
  ($or $thematic-break
       $heading))

(define $container-block
  ($or ;; $block-quote
       ;; $list
       ;; $code-block
       ;; $paragraph
       ;; $html-block
       ;; $custom-block
   ))

(define $block
  ($or $leaf-block
       $container-block))

(define $markdown
  ($let ((loc $location)
	 (block* ($many $block)))
    ($return `(document (@ ,*namespace* ,@loc) ,@block*))))
  
(define (parse-markdown input)
  (define lseq (document-input->lseq input document:simple-lexer))
  (let-values (((s v nl) ($markdown lseq)))
    (if (and (parse-success? s) (null? nl))
	v
	(document-input-error 'parse-markdown "Failed to parse Markdown"
			      (document-input-filename input)))))

)
