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
    (export parse-markdown
	    ;; maybe useful?
	    *markdown:tab-size*)
    (import (rnrs)
	    (peg)
	    (srfi :1 lists)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (srfi :127 lseqs)
	    (sagittarius document input))

(define *namespace* '(xmlns "http://commonmark.org/xml/1.0"))
(define *markdown:tab-size* (make-parameter 4)) ;; 4 but maybe ppl want 8 or 2?
(define *link-definitions* (make-parameter #f))

(define $nl ($or ($input-eqv? #\newline)
		 ($seq ($input-eqv? #\return)
		       ($optional ($input-eqv? #\newline)))))
(define $eol ($or $eof $nl))
(define $ws ($input-eqv? #\space))
(define $ws* ($many $ws 0))
(define $ws+ ($many $ws 1))
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

(define ($char . delimiters)
  (define (check cs c)
    (if (char-set? cs)
	(char-set-contains? cs c)
	(eqv? cs c)))
  (lambda (l)
    (if (null? l)
	(return-expect "EOF" l)
	(let ((c (input-char l))
	      (nl (lseq-cdr l)))
	  (cond ((memp (lambda (c2) (check c2 c)) delimiters)
		 (return-expect "Non delimiter" l))
		((eqv? c #\\) (return-result (input-char nl) (lseq-cdr nl)))
		(else (return-result c nl)))))))
  
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
(define $atx-marker
  ($let ((m ($many ($input-eqv? #\#) 1 6))
	 ( $ws+ ))
    ($return (length m))))
(define $atx-heading
  ($let ((loc $location)
	 ( $non-indent-space )
	 (level $atx-marker)
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
  ($many ($input-eqv? (caar fence)) (length fence)))
(define $fenced-code-block
  ($let* ((loc $location)
	  (maybe-space $non-indent-space)
	  (fence $fence)
	  (info ($optional ($many ($seq ($not $eol) $any) 1)))
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

;; 4.6 HTML blocks
(define $< ($input-eqv? #\<))
(define $> ($input-eqv? #\>))
(define $dquote ($input-eqv? #\"))
(define $squote ($input-eqv? #\'))
(define $html-attr-value
  ($or ($let (( $dquote )
	      (v ($many ($char #\" #\< #\&)))
	      ( $dquote ))
	  ($return (list->string v)))
       ($let (( $squote )
	      (v ($many ($char #\' #\< #\&)))
	      ( $squote ))
	  ($return (list->string v)))))
(define $html-attr
  ($let ((n ($many ($char #\=) 1))
	 ( ($input-eqv? #\=) )
	 (v $html-attr-value))
    ($return (cons (list->string n) v))))
(define $html-attrs ($many ($seq $ws+ $html-attr)))
(define ($html-block-closed-tag . tags)
  (define $tag* (map $input-token-ci tags))
  (define ($end-tag t)
    ($input-token-ci (string-append "</" t ">")))
  (define parser
    ($let* (( $< )
	    (tag (apply $or $tag*))
	    (attr ($optional $html-attrs '()))
	    ( $>)
	    (c* ($many ($seq ($not ($end-tag tag)) ($char))))
	    ( ($end-tag tag) ))
      ($return #t)))
  ($let ((r ($raw parser))) ($return (list->string (map car r)))))

(define ($html-start-end start end)
  (define $end ($input-token end))
  (define $inner
    ($seq ($input-token start)
	  ($many ($seq ($peek ($not $end)) $any))
	  $end))
  ($let ((c ($raw $inner))) ($return (list->string (map car c)))))
(define $html-comment ($html-start-end "<!--" "-->"))
(define $html-cdata ($html-start-end "<![CDATA[" "]]>"))
(define $html-pi ($html-start-end "<?" "?>"))
;; e.g. DOCTYPE, ELEMENT etc
(define $html-decl ($html-start-end "<!" ">"))

(define $html-block
  ($let ((b ($or ($html-block-closed-tag "pre" "script" "style" "textarea")
		 $html-comment
		 $html-cdata
		 $html-pi
		 $html-decl ;; this must be below comment and cdata
		 )))
    ($return `(html_block ,b))))

;; 6.3 Links
(define *link-definition-char-set*
  (char-set-union char-set:iso-control (char-set #\space)))
(define $link-destination
  ($or ($let (( $< )
	      (n ($many ($char #\>)))
	      ( $> ))
	 ($return (list->string n)))
       ($let (( $ws* )
	      (n ($many ($char *link-definition-char-set*) 1)))
	 ($return (list->string n)))))

(define $link-title ($or )) ;; TODO

;; 4.7 Link reference definitions
;; we just need to store the definition into the storage,
;; then return #f
(define $link-reference-definition
  ($let (( ($input-eqv? #\[) )
	 (c* ($many ($char #\]) 1))
	 ( ($input-token "]:") )
	 (url $link-destination)
	 (title ($optional $link-title)))
    (let ((n (list->string c*)))
      ;; we only use the first one
      (hashtable-update! (*link-definitions*) n (lambda (v) v) `(,url ,title))
      ($return #f))))

;; 4.8 Paragraphs
(define $paragraph
  ($let ((c* ($many ($seq ($not $blank-line) $line) 1)))
    ($return `(paragraph ,@c*))))

(define $leaf-block
  ($or $thematic-break
       $heading
       $code-block
       $html-block
       $link-reference-definition
       $paragraph
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
  (parameterize ((*link-definitions*
		  (make-hashtable string-ci-hash string-ci=?)))
    (let-values (((s v nl) ($markdown lseq)))
      (if (and (parse-success? s) (null? nl))
	  v
	  (document-input-error 'parse-markdown "Failed to parse Markdown"
				(document-input-filename input))))))

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
