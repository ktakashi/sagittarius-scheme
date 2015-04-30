;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser.scm - Parser for Markdown text
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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


;; For now only needed part is implemented.

(library (text markdown parser)
    (export markdown-parser
	    parse-markdown
	    markdown-parser-error?
	    markdown-parser-position
	    markdown-parser-expected)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (packrat)
	    (sagittarius)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :26 cut)
	    (srfi :39 parameters))

;; lexical generator
(define (generator p)
  (let ((ateof #f)
	(pos (top-parse-position (cond ((car (port-info p)))
				       (else "<?>")))))
    (lambda ()	
      (if ateof
	  (values pos #f)
	  (let ((x (read-char p)))
	    (if (eof-object? x)
		(begin
		  (set! ateof #t)
		  (values pos #f))
		(let ((old-pos pos))
		  (set! pos (update-parse-position pos x))
		  (values old-pos (cons x x)))))))))

;; helper for tokens
(define (token str)
  (lambda (starting-results)
    (let loop ((pos 0) (results starting-results))
      (if (= pos (string-length str))
	  (make-result str results)
	  (let ((ch (parse-results-token-value results)))
	    (if (and ch (char=? ch (string-ref str pos)))
		(loop (+ pos 1) (parse-results-next results))
		(make-expected-result
		 (parse-results-position starting-results) str)))))))

(define (min-max char/pred/charset :optional (min 0) (max #f))
  (let ((pred (cond ((char? char/pred/charset)
		     (lambda (c) (char=? char/pred/charset c)))
		    ((procedure? char/pred/charset) char/pred/charset)
		    ((char-set? char/pred/charset)
		     (lambda (c) (char-set-contains? char/pred/charset c)))
		    (else
		     (assertion-violation 
		      'min-max
		      (format "character or procedure required but got ~s"
			      char/pred) char/pred))))
	(required (format "~a{~a,~a}" char/pred/charset min (if max max ""))))
    (lambda (starting-results)
      (let loop ((count 0) (results starting-results) (acc '()))
	(let ((c (parse-results-token-value results)))
	  (if (and c (pred c))
	      (loop (+ count 1) (parse-results-next results) (cons c acc))
	      (cond ((or (and max (<= min count max)) ;; when max is given
			 (and (not max) (<= min count)))
		     (make-result (list->string (reverse! acc)) results))
		    (else
		     (make-expected-result
		      (parse-results-position starting-results)
		      required)))))))))

(define (one-of-token tokens)
  (lambda (starting-results)
    (let outer ((tokens tokens))
      (if (null? tokens)
	  (make-expected-result
	   (parse-results-position starting-results) tokens)
	  (let ((str (symbol->string (car tokens))))
	    (let loop ((pos 0) (results starting-results))
	      (if (= pos (string-length str))
		  (make-result str results)
		  (let ((ch (parse-results-token-value results)))
		    (if (and ch (char=? ch (string-ref str pos)))
			(loop (+ pos 1) (parse-results-next results))
			(outer (cdr tokens)))))))))))

(define (combine-pred . preds)
  (lambda (ch)
    (exists (cut <> ch) preds)))

;; TODO union with extended special char
(define *special-char-set* (string->char-set "~*_`&[]()<!#\\'\""))

(define *space-char-set* (string->char-set " \t"))
(define *newline-char-set* (string->char-set "\n\r"))

(define *nonspace-char-set* (char-set-difference char-set:full
			     *special-char-set* *newline-char-set*))
(define *normal-char-set*
  (char-set-difference char-set:full *special-char-set* *space-char-set*
		       *newline-char-set*))
;; parameters
(define inline-tags (make-parameter '()))
(define block-tags (make-parameter '()))

(define *known-inline-tags* 
  ;; TODO add
  ;; we need not a literal list
  '(a img span code b strong strike i abbr address br hr sub sup))

(define *known-block-tags* 
  ;; TODO add
  '(div p table blockquote pre dl))

#|
Parser of Markdown
Compatible with peg-markdown: https://github.com/jgm/peg-markdown
|#
(define markdown-parser
  (packrat-parser
   (begin
     (define (token-with-charset sets results)
       (let loop ((acc '()) (results results))
	 (let ((ch (parse-results-token-value results)))
	   (cond ((and ch (char-set-contains? sets ch))
		  (loop (cons ch acc) (parse-results-next results)))
		 ((zero? (length acc))
		  (make-expected-result
		   (parse-results-position results) "<?>"))
		 (else
		  (make-result (list->string (reverse! acc)) results))))))
     (define (char-with-charset sets results)
       (let ((ch (parse-results-token-value results)))
	 (if (and ch (char-set-contains? sets ch))
	     (make-result ch (parse-results-next results))
	     (make-expected-result
		 (parse-results-position results) "<?>"))))
     
     (define (normal-char results) 
       (char-with-charset *normal-char-set* results))
     (define (symbol results) 
       (char-with-charset *special-char-set* results))
     (define (alphanumeric results) 
       (char-with-charset char-set:letter+digit results))
     (define (number results) 
       (char-with-charset char-set:digit results))

     (define (any-char results) 
       ;; correct?
       (char-with-charset char-set:full results))
     (define (nonspace-char results)
       (char-with-charset *nonspace-char-set*
	 results))
     (define (one-line results)
       (token-with-charset (char-set-difference char-set:full
						*newline-char-set*)
			   results))

     (define (token-with-charset-parser sets)
       (lambda (results)
	 (let ((result (token-with-charset sets results)))
	   (if (parse-result-successful? result)
	       (merge-result-errors result (parse-result-error result))
	       result))))

     (define url
       (packrat-check (token-with-charset-parser
		       (char-set-intersection char-set:letter char-set:ascii))
	 (lambda (result1)
	   (packrat-check (token "://")
	      (lambda (result2)
		(packrat-check (token-with-charset-parser
				(char-set-difference char-set:full
				  (string->char-set "\r\n>")))
		   (lambda (result3)
		     (lambda (results)
		       (make-result
			(string-append result1 "://" result3)
			results)))))))))
     
     (define email
       (packrat-check (token-with-charset-parser
		       (char-set-union
			(char-set-intersection char-set:letter char-set:ascii)
			(string->char-set "-+_./!%~$")))
	 (lambda (result1)
	   (packrat-check (token "@")
	      (lambda (result2)
		(packrat-check (token-with-charset-parser
				(char-set-difference char-set:full
				     (string->char-set "\r\n>")))
		   (lambda (result3)
		     (lambda (results)
		       (make-result
			(string-append result1 "@" result3)
			results)))))))))
     ;; code is better to do it here
     (define (code starting-results)
       ;; we also do until ticks5
       (define (get-ticks results)
	 (let loop ((results results) (count 0))
	   (if (= count 5)
	       (values count results)
	       (let ((ch (parse-results-token-value results)))
		 (if (eqv? #\` ch)
		     (loop (parse-results-next results) (+ count 1))
		     (values count results))))))
       (define (read-code ticks results)
	 (let loop ((acc '()) (results results))
	   (let ((ch (parse-results-token-value results)))
	     (cond ((not ch)
		    (make-expected-result 
		     (parse-results-position starting-results)
		     "Unexpected EOF"))
		   ((and (not (eqv? ch #\`))
			 (char-set-contains? *nonspace-char-set* ch))
		    (loop (cons ch acc) (parse-results-next results)))
		   ((eqv? #\` ch)
		    ;; check tick
		    (let-values (((ticks2 next-results) (get-ticks results)))
		      (if (= ticks ticks2)
			  ;; ok ends
			  (make-result 
			   (list :code (list->string (reverse! acc)))
			   next-results)
			  (loop (cons ch acc) (parse-results-next results)))))
		   (else 
		    (loop (cons ch acc) (parse-results-next results)))))))
	 
       (let-values (((ticks results) (get-ticks starting-results)))
	 (cond ((zero? ticks)
		(make-expected-result (parse-results-position starting-results) 
				      "inline code requires `"))
	       ((eqv? (parse-results-token-value results) #\`)
		(make-expected-result (parse-results-position starting-results)
				      "more than max inline code `"))
	       (else
		(read-code ticks results)))))

;; this goes to infinite loop ...
;;      (define (any+eof results)
;;        (let loop ((acc '()) (results results))
;; 	 (let ((ch (parse-results-token-value results)))
;; 	   (if ch
;; 	       (loop (cons ch acc) (parse-results-next results))
;; 	       (make-result (list->string (reverse! acc))
;; 			    results)))))

     doc)
   (doc ((b* <- (* block)) (cons :doc b*)))
   (block (((* blankline) 
	    b <- (/ block-quote 
		    verbatim 
		    note 
		    reference 
		    horizontal-rule 
		    heading
		    ordered-list
		    bullet-list
		    ;;html-block
		    ;;style-block
		    para
		    plain ;; when this will be?
		    )
	    ) b))

   (block-quote ((b <- block-quote-raw) (cons :blockquote (apply append b))))
   (block-quote-raw ((b <- (+ block-quote-content)) b))
   (block-quote-content ((b1 <- block-quote-one
			  b2 <- (* block-quote-next)
			  bl <- (* blankline))
			 (if (null? bl)
			     (cons b1 b2)
			     `(,b1 ,@b2 "\n"))))
   (block-quote-one (('#\> (? '#\space) l <- line) l))
   (block-quote-next (((! '#\>) (! blankline) l <- line) l))

   ;; verbatim
   (verbatim ((v <- (+ verbatim-chunk)) (cons :verbatim v)))
   (verbatim-chunk ((b <- (* blankline) l <- (+ non-blank-indented-line))
		    (if (null? b)
			(string-join l "\n")
			(string-append "\n" (string-join l "\n")))))
   ;; TODO add ``` ... ``` type of verbatim

   ;; horizontal-rule
   (horizontal-rule ((non-indent-space rule* sp nl (+ blankline)) :line)
		    ((non-indent-space rule- sp nl (+ blankline)) :line)
		    ((non-indent-space rule_ sp nl (+ blankline)) :line))
   (rule* (('#\* sp '#\* sp '#\* (* sp*)) :***))
   (sp* ((sp '#\*) :*))
   (rule- (('#\- sp '#\- sp '#\- (* sp-)) :---))
   (sp- ((sp '#\-) :-))
   (rule_ (('#\_ sp '#\_ sp '#\_ (* sp_)) :___))
   (sp_ ((sp '#\_) :_))

   ;; list
   (bullet (((! horizontal-rule) non-indent-space 
	     (/ ('#\+) ('#\*) ('#\-)) (+ space-char)) :bullet))
   (bullet-list (((& bullet) l <- (/ (list-tight) (list-loose)))
		 (cons :bullet-list l)))

   (list-tight ((i <- (+ list-item-tight) (* blankline) 
		   (! (/ (bullet) (enumerator)))) i))
   (list-loose ((i <- (+ list-loose*)) i))
   (list-loose* ((i <- list-item (* blankline)) (string-append i "\n\n")))

   (list-item (((/ (bullet) (enumerator))
		b <- list-block 
		b* <- (* list-continuation-block))
	       (apply string-append b b*)))
   (list-item-tight (((/ (bullet) (enumerator))
		      b <- list-block 
		      b* <- (* list-item-tight*))
		     (apply string-append b b*)))
   (list-item-tight* (((! blankline) c <- list-continuation-block) c))

   (list-block (((! blankline) l <- line l* <- (* list-block-line))
		(string-append l "\n" (string-join l* "\n"))))
   (list-continuation-block (((* blankline) l* <- (+ list-continuation-block*))
			     (string-concatenate l*)))
   (list-continuation-block* ((indent b <- list-block) b))

   (list-block-line (((! blankline) (! (? indent) (/ (bullet) (enumerator)))
		      (! horizontal-rule) l <- optionally-indented-line) l))

   (enumerator ((non-indent-space (+ number) '#\. (+ space-char)) :enum))
   (ordered-list (((& enumerator) l <- (/ (list-tight) (list-loose)))
		  (cons :ordered-list l)))

   (para ((non-indent-space i* <- inlines (+ blankline)) (cons :paragraph i*)))
   (plain ((i* <- inlines) (cons :plain i*)))
   (atx-inline (((! nl) (! sp (* '#\#) sp nl) i <- inline) i))
   (atx-start (((token "######")) :h6)
	      (((token "#####"))  :h5)
	      (((token "####"))   :h4)
	      (((token "###"))    :h3)
	      (((token "##"))     :h2)
	      (((token "#"))      :h1))
   (atx-heading ((t <- atx-start sp i* <- (+ atx-inline) (? sp (* '#\#) sp) nl)
		 (cons t i*)))

   (heading ((h <- atx-heading) h))

   (inlines ((i <- (+ inlines*)) i))
   (inlines* (((! endline) i <- inline (? endline)) i))
   (inline ((e <- endline) :eol)
	   ((v <- ul-or-star-line) v)
	   ((s <- space) s)
	   ((s <- strong) s)
	   ((e <- emph) e)
	   ((i <- image) i)
	   ((l <- link) l)
	   ((n <- note-reference) n)
	   ((i <- inline-note) i)
	   ((c <- code) c)
	   ;; ((r <- raw-html) r)
	   ;; ((e <- entity) e)
	   ;; ((e <- escaped-char) e)
	   ;; ((s <- smart) s)
	   ((s <- str) s) ;; should be lower, otherwise lots of things fail
	   ((s <- symbol) (string s))
	   )

   (str ((c* <- (+ normal-char) sc* <- (* str-chunk))
	 (apply string-append (list->string c*) sc*)))
   
   (str-chunk ((c <- (/ normal-char str-chunk-helper))
	       (if (char? c)
		   (string c)
		   c))
	      (('#\' (& alphanumeric)) "'"))
   (str-chunk-helper ((c* <- (+ '#\_) (& alphanumeric)) (list->string c*)))

   (ul-or-star-line ((v <- (/ ul-line start-line)) v))
   (start-line (((token "****") v <- (* '#\*)) 
		(string-append "****" (list->string v)))
	       ((space-char v <- (+ '#\*) (& space-char)) (list->string v)))
   (ul-line (((token "____") v <- (* '#\_)) 
	     (string-append "____" (list->string v)))
	    ((space-char v <- (+ '#\_) (& space-char)) (list->string v)))

   (emph ((v <- emph-star) v)
	 ((v <- emph-ul) v))
   (emph-star (('#\* (! white-space) c <- (+ emph-star-helper) '#\*)
	       (cons* :emph c)))
   (emph-star-helper (((! '#\*) i <- inline) i)
		     ((s <- strong-star) s))
   (emph-ul (('#\_ (! white-space) c <- (+ emph-ul-helper) '#\_)
	     (cons* :emph c)))
   (emph-ul-helper (((! '#\_) i <- inline) i)
		     ((s <- strong-ul) s))
   
   (strong ((v <- strong-star) v)
	   ((v <- strong-ul) v))
   (strong-star (((token "**") 
		  (! white-space) c <- (+ strong-star-helper)
		  (token "**"))
		 (cons* :strong c)))
   (strong-star-helper (((! (token "**")) i <- inline) i))
   (strong-ul (((token "__")
		(! white-space) c <- (+ strong-ul-helper)
		(token "__"))
	       (cons* :strong c)))
   (strong-ul-helper (((! (token "__")) i <- inline) i))

   ;; image
   (image (('#\! l <- (/ explicit-link reference-link))
	   (if (and (pair? l) (eq? (car l) :link))
	       (begin (set-car! l :image) l)
	       ;; TODO how should we handle
	       (cons* :string "!" (cdr l)))))
   ;; link
   (link ((l <- (/ explicit-link reference-link auto-link)) l))
   (reference-link ((l <- reference-link-dobule) l)
		   ((l <- reference-link-single) l))
   (reference-link-dobule ((a <- label spnl (! (token "[]")) b <- label)
			   ;; TODO find reference
			   (list :link a "" "")))
   (reference-link-single ((a <- label (? refls-hlp))
			   ;; TODO find reference
			   (list :link a "" "")))
   (refls-hlp ((spnl (token "[]")) :ignore))

   (explicit-link ((l <- label '#\( sp s <- source spnl t <- title sp '#\))
		   (list :link l s t)))
   (source (('#\< s <- source-contents '#\>) s)
	   ((s <- source-contents) s))
   (source-contents ((v <- (* source-contents-helper))
		     (string-concatenate v)))
   (source-contents-helper ((c* <- (+ schh))
			    (list->string c*))
			   (('#\( s <- source-contents '#\))
			    (string-concatenate s)))
   ;; source contents helper helper...
   (schh (((! '#\() (! '#\)) (! '#\>) c <- nonspace-char) c))
   (title ((v <- title-single) v)
	  ((v <- title-double) v)
	  (((token "")) ""))
   (title-single (('#\' v* <- (* title-helper1) '#\') (list->string v*)))
   (title-double (('#\" v* <- (* title-helper2) '#\") (list->string v*)))
   (title-helper1 (((! title-helper1*) c <- any-char) c))
   (title-helper1* (('#\' sp close-or-nl) :ignore))
   (title-helper2 (((! title-helper2*) c <- any-char) c))
   (title-helper2* (('#\" sp close-or-nl) :ignore))
   (close-or-nl (('#\)) :ignore)
		((nl) :ignore))
   
   (auto-link ((u <- auto-link-url) (list :link (list :label u) u ""))
	      ((e <- auto-link-email) `(:link ,@e "")))
   (auto-link-url (('#\< u <- url '#\>) u))
   (auto-link-email (('#\< (token "mailto:") e <- email '#\>) 
		     (list (list :label (string-append "mailto:" e)) e))
		    (('#\<  e <- email '#\>)
		     (list (list :label (string-append "mailto:" e)) e)))

   ;; reference
   (reference ((non-indent-space (! (token "[]")) l <- label '#\: spnl
				 s <- ref-src t <- ref-title (+ blankline))
	       (list :reference l s t)))
   (ref-src ((c <- (+ nonspace-char)) (list->string c)))
   (ref-title (('#\" '#\") "")
	      ((spnl t <- title-single) t)
	      ((spnl t <- title-double) t)
	      ((spnl '#\( c <- (* title-helper3) '#\)) (list->string c))
	      (() ""))
   (title-helper3 (((! title-helper3*) c <- any-char) c))
   (title-helper3* (('#\) sp nl) :ignore))

   ;; TODO find reference
   (note-reference ((ref <- raw-note-reference) (list :note-ref ref)))
   (raw-note-reference (((token "[^") c <- (+ note-char) '#\]) 
			(list->string c)))
   (note-char (((! nl) (! '#\]) c <- any-char) c))
   (note ((non-indent-space ref <- raw-note-reference '#\: sp
			    b1 <- raw-note-block
			    b2 <- (* raw-note-block*))
	  (if (null? b2)
	      `(:note (:ref ,ref) ,@b1)
	      (apply append `(:note (:ref ,ref) ,@b1) b2))))
   (raw-note-block ((b <- (+ raw-note-block-content) l* <- (* blankline)) b))
   (raw-note-block* ((indent b <- raw-note-block) b))
   (raw-note-block-content (((! blankline) l <- optionally-indented-line) l))

   (inline-note (((token "^[") n <- (+ inline-note*) '#\]) (cons* :note n)))
   (inline-note* (((! '#\]) i <- inline) i))

   ;; indent
   (non-indent-space (((token "   ")) :non-indent-space)
		     (((token "  ")) :non-indent-space)
		     (((token " ")) :non-indent-space)
		     (((token "")) :non-indent-space))
   (indent (('#\tab) :indent)
	   (((token "    ")) :indent))
   (indented-line ((indent l <- line) l))
   (non-blank-indented-line (((! blankline) l <- indented-line) l))
   (optionally-indented-line (((? indent) l <- line) l))

   ;; line
   (line ((l <- one-line nl) l)
	 ;; ((l <- any+eof) l) ;; how to handle EOF?
	 )

   ;; label
   (label (('#\[ (! '#\^ ) i* <- (* label-helper) '#\]) (cons :label i*)))
   (label-helper (((! '#\]) i <- inline) i))

   ;; misc
   (blankline ((sp nl) :blankline))
   (sp (((* space-char)) :sp))
   (space (((+ space-char)) " "))
   (spnl ((sp (? nl sp)) :spnl))
   (space-char (('#\space) " ")
	       (('#\tab)   "\t"))
   (nl   (('#\newline) "\n")
	 (('#\return (? '#\newline)) "\n"))
   (white-space ((space-char) " ")
		((nl) "\n"))
   ;; endline
   (endline ((linebreak) :eol)
	    ((terminal-endline) :eol)
	    ((normal-endline) :eol))

   (normal-endline ((sp nl (! blankline) (! '#\>) (! atx-start)) 
		    :normal-endline))
   (terminal-endline ((sp nl sp) :terminal-endline))
   (linebreak (((token "  ") normal-endline) :linebreak))
   ))
  ;; condition
  (define-condition-type &markdown-parser-error &error
    make-parser-error markdown-parser-error?
    (position markdown-parser-position)
    (expected markdown-parser-expected))

  (define (raise-markdown-perser-error who msg position expected)
    (raise
     (condition (make-parser-error position expected)
		(make-who-condition who)
		(make-message-condition msg))))

  #;(define (remove-separator lst)
    ;; :separator must be only toplevel
    (let loop ((acc '())
	       (lst lst))
      (cond ((null? lst) (reverse! acc))
	    ((eq? (car lst) :separator)
	     (loop acc (cdr lst)))
	    (else (loop (cons (car lst) acc) (cdr lst))))))

  ;; entry point
  (define (parse-markdown p :key (parser markdown-parser)
			         ;; additional tags
			         (inline-tag '())
				 (block-tag '())
			    ;; we don't use but need this
			    :allow-other-keys)
    (parameterize ((inline-tags *known-inline-tags*)
		   (block-tags *known-block-tags*))
      (unless (or (null? inline-tag)
		  (list? inline-tag))
	(assertion-violation 'parse-markdown
			     (format "list required but got ~s" inline-tag)
			     inline-tag))
      (unless (or (null? block-tag)
		  (list? block-tag))
	(assertion-violation 'parse-markdown
			     (format "list required but got ~s" block-tag)
			     block-tag))
      (inline-tags (append (inline-tags) inline-tag))
      (block-tags (append (block-tags) block-tag))
      (let ((result (parser (base-generator->results (generator p)))))
	(if (parse-result-successful? result)
	    (parse-result-semantic-value result)
	    (let ((e (parse-result-error result)))
	      (raise-markdown-perser-error 'parse-markdown
					   (parse-error-messages e)
					   (parse-error-position e)
					   (parse-error-expected e)))))))
	     
)
