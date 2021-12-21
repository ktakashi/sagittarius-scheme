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
	    (srfi :1 lists)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (sagittarius document input))

(define *namespace* '(xmlns "http://commonmark.org/xml/1.0"))
(define *markdown:tab-size* (make-parameter 4)) ;; 4 but maybe ppl want 8 or 2?

(define $nl ($or ($input-eqv? #\newline)
		 ($seq ($input-eqv? #\return)
		       ($optional ($input-eqv? #\newline)))))
(define $eol ($or $eof $nl))
(define $ws ($input-eqv? #\space))
(define $ws* ($many ($input-eqv? #\space) 0))
(define $ws+ ($many ($input-eqv? #\space) 1))
(define $non-indent-space ($many $ws 0 3))
(define $indent-space ($repeat $ws 4))

(define *title-char-set*
  (char-set-difference char-set:full char-set:iso-control))
(define $title-char ($input-char-set-contains *title-char-set*))
(define *id-char-set*
  (char-set-difference *title-char-set* (string->char-set "\"'")))
(define $id-char ($input-char-set-contains *id-char-set*))
(define ($string p at-least)
  ($let ((c* ($many p at-least))) ($return (list->string (map car c*)))))

(define $line
  ($let ((c* ($many ($seq ($peek ($not $eol)) $any) 1))
	 ( $eol ))
    ($return (list->string (map car c*)))))

;; 4.1 Thematic breaks
(define $thematic-break
  ($let ((loc $location)
	 ( $non-indent-space )
	 ( ($or ($many ($input-eqv? #\*) 3)
		($many ($input-eqv? #\-) 3)
		($many ($input-eqv? #\_) 3)) )
	 ( $eol ))
    ($return `(thematic_break (@ ,@loc)))))

;; 4.2 ATX headings
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

;; 4.3 Setext headings
(define $setext-heading-mark
  ($or ($many ($input-eqv? #\=) 1)
       ($many ($input-eqv? #\-) 1)))
(define $setext-heading
  ($let ((loc $location)
	 (lines ($many ($seq ($peek $non-indent-space)
			     ($not $setext-heading-mark)
			     $line) 1))
	 (sep ($seq $non-indent-space $setext-heading-mark)))
    ($return `(heading (@ ,@loc (level ,(if (eqv? #\= (caar sep)) 1 2)))
		;; will be resolved later
		,@lines))))
			 
(define $heading
  ($or $atx-heading
       $setext-heading))

;; 4.9 Blank lines
(define $blank-line
  ($or ($seq $ws* $nl ($return #f))
       ($seq $ws+ $eof ($return #f))))

;; 4.4 Indented code blocks
(define $indented-code-block
  ($let ((loc $location)
	 ( ($many $blank-line) )
	 (lines ($many ($seq $indent-space $line) 1))
	 ( ($many $blank-line) ))
    ($return `(code_block (@ ,@loc) ,@lines))))

;; 4.5 Fenced code blocks
(define $fence
  ($or ($many ($input-eqv? #\~) 3)
       ($many ($input-eqv? #\`) 3)))
(define ($close-fence fence)
  (write fence) (newline)
  ($many ($input-eqv? (caar fence)) (length fence)))
(define $fenced-code-block
  ($let* ((loc $location)
	  (maybe-space $non-indent-space)
	  (fence $fence)
	  (info ($debug ($optional ($many ($seq ($not $eol) $any) 1))))
	  ( $eol )
	  (lines ($many ($seq ($peek ($not ($close-fence fence))) $line)))
	  ( $non-indent-space )
	  ( ($close-fence fence) )
	  ( $eol ))
    ($return `(code-block (@ ,@loc 
			     ,@(if info
				   `((style ,(list->string (map car info))))
				   '()))
		,@lines))))

(define $code-block
  ($or $indented-code-block
       $fenced-code-block))

(define $leaf-block
  ($or $thematic-break
       $heading
       $code-block
       ;; $html-block
       ;; $link-reference-definition
       ;; $paragraph
       $blank-line
       ))

(define $container-block
  ($or ;; $block-quote
       ;; $lists
       ;; $list-items (must be in $list)
       ;; $custom-block
   ))

(define $block
  ($seq ($not $eof) ;; To avoid infinite loop on $blank-line
	($or $leaf-block $container-block)))

(define $markdown
  ($let ((loc $location)
	 (block* ($many $block)))
    ($return `(document (@ ,*namespace* ,@loc) ,@(filter values block*)))))
  
(define (parse-markdown input)
  (define lseq (document-input->lseq input (markdown:make-lexer)))
  (let-values (((s v nl) ($markdown lseq)))
    (if (and (parse-success? s) (null? nl))
	v
	(document-input-error 'parse-markdown "Failed to parse Markdown"
			      (document-input-filename input)))))

(define (markdown:make-lexer)
  (let ((buffer-count 0)
	(tab-count (*markdown:tab-size*)))
    (lambda (port)
      (cond ((zero? buffer-count)
	     (let ((c (get-char port)))
	       (cond ((eof-object? c) (values c 0 #f))
		     ((eqv? c #\tab)
		      (set! buffer-count (- tab-count 1))
		      (values #\space 1 #f))
		     ((eqv? c #\newline) (values c 0 1))
		     (else (values c 1 #f)))))
	    (else
	     (set! buffer-count (- buffer-count 1))
	     (values #\space 1 #f))))))

)
