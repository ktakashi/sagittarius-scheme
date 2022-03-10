;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/inlines.scm - Inline parsers
;;;
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (text markdown parser inlines)
    (export inline-parser:parse!

	    make-inline-parser-context inline-parser-context?
	    make-inline-parser inline-parser?

	    (rename (delimiter-processor <delimiter-processor>))
	    delimiter-processor?

	    delimiter?
	    delimiter-characters delimiter-char delimiter-original-length
	    delimiter-can-open? delimiter-can-close?
	    delimiter:length
	    delimiter:opener delimiter:opener*
	    delimiter:closer delimiter:closer*
	    )
    (import (rnrs)
	    (core misc)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :115 regexp)
	    (srfi :117 list-queues)
	    (srfi :158 generators-and-accumulators)
	    (text markdown parser escaping)
	    (text markdown parser link-reference)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser scanner)
	    (text markdown parser source)
	    (util flexible-vector))

(define-vector-type delimiter
  (%make-delimiter characters
		   char
		   original-length
		   can-open?
		   can-close?
		   previous
		   next)
  delimiter?
  (characters delimiter-characters)
  (char delimiter-char)
  (original-length delimiter-original-length)
  (can-open? delimiter-can-open?)
  (can-close? delimiter-can-close?)
  (previous delimiter-previous delimiter-previous-set!)
  (next delimiter-next delimiter-next-set!))

(define (make-delimiter characters char can-open? can-close? previous)
  (%make-delimiter characters char (flexible-vector-size characters)
		   can-open? can-close? previous #f))
(define (delimiter:length delimiter)
  (flexible-vector-size (delimiter-characters delimiter)))
(define (delimiter:opener delimiter)
  (flexible-vector-back (delimiter-characters delimiter)))
(define (delimiter:opener* delimiter length)
  (define len (delimiter:length delimiter))
  (unless (<= 1 length len)
    (assertion-violation 'delimiter:opener*
			 (string-append "Length must be between 1 and "
					(number->string len)
					", was " (number->string len))))
  (let ((chars (flexible-vector->list (delimiter-characters delimiter))))
    (drop chars (- len length))))
(define (delimiter:closer delimiter)
  (flexible-vector-front (delimiter-characters delimiter)))
(define (delimiter:closer* delimiter length)
  (define len (delimiter:length delimiter))
  (unless (<= 1 length len)
    (assertion-violation 'delimiter:opener*
			 (string-append "Length must be between 1 and "
					(number->string len)
					", was " (number->string len))))
  (let ((chars (flexible-vector->list (delimiter-characters delimiter))))
    (take chars length)))

(define-record-type delimiter-processor
  (fields opening-character
	  closing-character
	  (mutable min-length)
	  process))
(define-record-type staggered-delimiter-processor
  (parent delimiter-processor)
  (fields processors)
  (protocol (lambda (n)
	      (lambda (delim)
		((n delim delim 0 staggered-delimiter-processor:process)
		 (flexible-vector))))))

(define (delimiter-processor:process dp opening-run closing-run)
  (define process (delimiter-processor-process dp))
  (process dp opening-run closing-run))

(define (staggered-delimiter-processor:process dp opening-run closing-run)
  (define processors (staggered-delimiter-processor-processors dp))
  (define len (delimiter:length opening-run))
  (define (find-processor len)
    (or (flexible-vector-any (lambda (p) (delimiter-processor-min-length p) len)
			     processors)
	(flexible-vector-front processors)))
  (delimiter-processor:process (find-processor (delimiter:length opening-run))
			       opening-run closing-run))
(define (staggered-delimiter-processor:add! sdp dp)
  (define processors (staggered-delimiter-processor-processors sdp))
  (define len (delimiter-processor-min-length dp))
  (define (add fv i)
    (and (not (= i len))
	 (let* ((p (flexible-vector-ref fv i))
		(plen (delimiter-processor-min-length p)))
	   (cond ((> len plen) (flexible-vector-insert! fv i dp))
		 ((= len plen)
		  (assertion-violation 'staggered-delimiter-processor:add!
		   "Cannot add delimiter processors for the char and the minimum length"
		   `((char . ,(delimiter-processor-opening-character sdp))
		     (min-length . ,len))))
		 (else (add fv (+ i 1)))))))
  (unless (add processors 0)
    (flexible-vector-insert-back! processors dp)
    (delimiter-processor-min-length-set! sdp len))
  sdp)

(define-vector-type inline-parser-state
  (make-inline-parser-state block
			    scanner
			    include-source-locations?
			    trailing-spaces
			    last-delimiter
			    last-bracket)
  inline-parser-state
  (block inline-parser-state-block)
  (scanner inline-parser-state-scanner)
  ;; FIXME
  (include-source-locations? parsing-state-include-source-locations?)
  (trailing-spaces parsing-state-trailing-spaces
		   parsing-state-trailing-spaces-set!)
  (last-delimiter parsing-state-last-delimiter
		  parsing-state-last-delimiter-set!)
  (last-bracket parsing-state-last-bracket parsing-state-last-bracket-set!))

(define-vector-type inline-parser-context
  (make-inline-parser-context delimiter-processors definitions)
  inline-parser-context?
  (delimiter-processors inline-parser-context-processors)
  (definitions inline-parser-context-definitions))

(define (inline-parser-context:get-deffinition context label)
  (link-reference-definitions:get (inline-parser-context-definitions context)
				  label))

(define-record-type inline-parser
  (fields (mutable parsing-state)
	  context
	  processors ;; delimiter processors
	  )
  (protocol (lambda (p)
	      (lambda (context)
		(p #f
		   context
		   (calculate-delimiter-processors
		    (inline-parser-context-processors context)))))))

(define (calculate-delimiter-processors processors)
  (define (add-processor-for-char char p ht)
    (define (check v)
      (when v
	(assertion-violation 'make-inline-parser
	 "Delimiter processor conflict with the delimiter char" char))
      p)
    (hashtable-update! ht char check #f))
  (define (add-processors ht processors)
    (define (add-processor p)
      (define opening (delimiter-processor-opening-character p))
      (define closing (delimiter-processor-closing-character p))
      (define (staggered old)
	(if (staggered-delimiter-processor? old)
	    old
	    (let ((s (make-staggered-delimiter-processor opening)))
	      (staggered-delimiter-processor:add! s old))))
      (if (eqv? opening closing)
	  (let ((old (hashtable-ref ht opening)))
	    (if (and old (eqv? (delimiter-processor-opening-character old)
			       (delimiter-processor-closing-character p)))
		(let ((s (staggered old)))
		  (hashtable-set! ht opening
				  (staggered-delimiter-processor:add! s p)))
		(add-processor-for-char opening p ht)))
	  (begin
	    (add-processor-for-char closing p ht)
	    (add-processor-for-char closing p ht))))
    (for-each add-processor processors))
  (let ((ht (make-eqv-hashtable (length processors))))
    (add-processors ht (list (make-asterisk-delimiter-processor)
     			     (make-underscore-delimiter-processor)))
    (add-processors ht processors)
    ht))

(define (inline-parser:parse! inline-parser source-lines block)
  (define scanner (scanner:of source-lines))
  (define locs (source-lines:source-loactions source-lines))
  (define state (make-inline-parser-state block scanner (not (null? locs))
					  0 #f #f))
  (inline-parser-parsing-state-set! inline-parser state)
  (let loop ()
    (cond ((inline-parser:parse-inline! inline-parser) =>
	   (lambda (nodes)
	     (for-each (lambda (node)
			 (markdown-node:append-child! block node))
		       nodes)
	     (loop)))))
  (inline-parser:process-delimiters! inline-parser #f)
  (inline-parser:merge-text-nodes! inline-parser block))

(define (inline-parser:parse-inline! inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define processors (inline-parser-processors inline-parser))
  (define (single-char inline-parser scanner)
    (let ((s (scanner:position scanner)))
      (scanner:next! scanner)
      (let ((e (scanner:position scanner)))
	(inline-parser:text inline-parser (scanner:source scanner s e)))))

  (define (parse-delimiters processor c)
    (let-values (((chars can-open? can-close?)
		  (inline-parser:scan-delimiters inline-parser processor c)))
      (and chars
	   (let ((d (make-delimiter chars c can-open? can-close?
				    (parsing-state-last-delimiter state))))
	     (parsing-state-last-delimiter-set! state d)
	     (when (delimiter-previous d)
	       (delimiter-next-set! (delimiter-previous d) d))
	     (flexible-vector->list chars)))))
  
  (let ((c (scanner:peek scanner)))
    (define (processor-match processor) (parse-delimiters processor c))
    ;; make delimiter processor higher prio
    (and c
	 (or
	  (cond ((hashtable-ref processors c #f) => processor-match)
		(else #f))
	  (case c
	    ((#\[) (list (inline-parser:parse-open-blacket inline-parser)))
	    ((#\!) (list (inline-parser:parse-bang inline-parser)))
	    ((#\]) (list (inline-parser:parse-close-blacket inline-parser)))
	    ((#\newline) (list (inline-parser:parse-line-break inline-parser)))
	    ((#\\) (list (inline-parser:parse-backslash inline-parser)))
	    ((#\`) (list (inline-parser:parse-backticks inline-parser)))
	    ((#\&) (list (inline-parser:parse-entity inline-parser)))
	    ((#\<)
	     (cond ((hashtable-ref processors c #f) => processor-match)
		   (else
		    (list (or (inline-parser:parse-auto-link inline-parser)
			      (inline-parser:parse-html-inline inline-parser)
			      (single-char inline-parser scanner))))))
	    (else (list (inline-parser:parse-text inline-parser))))))))
  
(define (inline-parser:text inline-parser source-lines)
  (define state (inline-parser-parsing-state inline-parser))
  (define block (inline-parser-state-block state))
  (let ((t (make-text-node block (source-lines:content source-lines))))
    (markdown-node:source-locations-set! t
     (source-lines:source-loactions source-lines))
    t))
(define (inline-parser:add-bracket! inline-parser braket)
  (define state (inline-parser-parsing-state inline-parser))
  (define last-braket (parsing-state-last-bracket state))
  (when last-braket
    (bracket-bracket-after?-set! last-braket #t))
  (parsing-state-last-bracket-set! state braket))

(define (inline-parser:remove-last-bracket! inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define last-braket (parsing-state-last-bracket state))
  (parsing-state-last-bracket-set! state (bracket-previous last-braket)))

(define (inline-parser:remove-delimiter! inline-parser delim)
  (define state (inline-parser-parsing-state inline-parser))
  (when (delimiter-previous delim)
    (delimiter-next-set! (delimiter-previous delim) (delimiter-next delim)))
  (if (not (delimiter-next delim))
      (parsing-state-last-delimiter-set! state (delimiter-previous delim))
      (delimiter-previous-set! (delimiter-next delim)
			       (delimiter-previous delim))))

(define (inline-parser:parse-open-blacket inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))
  (scanner:next! scanner)
  (let* ((p (scanner:position scanner))
	 (source (scanner:source scanner start p))
	 (t (inline-parser:text inline-parser source)))
    (inline-parser:add-bracket! inline-parser
     (bracket:link t start p
		   (parsing-state-last-bracket state)
		   (parsing-state-last-delimiter state)))
    t))

(define (inline-parser:parse-bang inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))
  (scanner:next! scanner)
  (if (scanner:next-char? scanner  #\[)
      (let* ((p (scanner:position scanner))
	     (source (scanner:source scanner start p))
	     (t (inline-parser:text inline-parser source)))
	(inline-parser:add-bracket! inline-parser
	 (bracket:link t start p
		       (parsing-state-last-bracket state)
		       (parsing-state-last-delimiter state)))
	t)
      (let* ((p (scanner:position scanner))
	     (source (scanner:source scanner start p)))
	(inline-parser:text inline-parser source))))

(define (inline-parser:parse-close-blacket inline-parser)
  (define (check-inline-link scanner after-close)
    (define (parse-link-destination scanner)
      (define delim (scanner:peek scanner))
      (define start (scanner:position scanner))
      (define (parse-destination scanner delim start)
	(let ((dest (source-lines:content
		     (scanner:source scanner start
				     (scanner:position scanner)))))
	  (if (eqv? delim #\<)
	      (substring dest 1 (- (string-length dest) 1))
	      dest)))
      (and (link-scanner:scan-link-destination! scanner)
	   (let ((dest (parse-destination scanner delim start)))
	     (escaping:unescape dest))))
    (define (parse-link-title scanner)
      (define start (scanner:position scanner))
      (and (link-scanner:scan-link-title! scanner)
	   (let ((title (source-lines:content
			 (scanner:source scanner start
					 (scanner:position scanner)))))
	     (escaping:unescape (substring title 1
					   (- (string-length title) 1))))))
    (define (finish scanner dest title)
      (cond ((not (scanner:next-char? scanner #\)))
	     (scanner:position! scanner after-close)
	     (values #f #f))
	    (else (values dest title))))
    (cond ((scanner:next-char? scanner #\()
	   (scanner:whitespace scanner)
	   (cond ((parse-link-destination scanner) =>
		  (lambda (dest)
		    (if (>= (scanner:whitespace scanner) 1)
			(let ((title (parse-link-title scanner)))
			  (scanner:whitespace scanner)
			  (finish scanner dest title))
			(finish scanner dest #f))))
		 (else
		  (scanner:position! scanner after-close)
		  (values #f #f))))
	  (else (values #f #f))))

  (define (check-label scanner opener after-close dest title)
    (define context (inline-parser-context inline-parser))
    (define (parse-link-label scanner)
      (and-let* (( (scanner:next-char? scanner #\[) )
		 (start (scanner:position scanner))
		 ( (link-scanner:scan-link-label-content! scanner) )
		 (end (scanner:position scanner))
		 ( (scanner:next-char? scanner #\]) )
		 (content (source-lines:content
			   (scanner:source scanner start end)))
		 ;; at most 999 characters, so less than 1000
		 ( (< (string-length content) 1000) ))
	content))
    (if (not dest)
	(let ((ref (parse-link-label scanner)))
	  (unless ref (scanner:position! scanner after-close))
	  (let ((ref (if (and (or (not ref) (zero? (string-length ref)))
			      (not (bracket-bracket-after? opener)))
			 (source-lines:content
			  (scanner:source scanner
					  (bracket-content-position opener)
					  before-close))
			 ref)))
	    (cond ((and ref
			(inline-parser-context:get-deffinition context ref)) =>
		   (lambda (destination)
		     (values (link-reference-definition-destination destination)
			     (link-reference-definition-title destination))))
		  (else (values dest title)))))
	(values dest title)))
  
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define before-close (scanner:position scanner))

  (scanner:next! scanner)
  (let ((after-close (scanner:position scanner))
	(opener (parsing-state-last-bracket state)))
    (cond ((not opener)
	   (inline-parser:text inline-parser
	     (scanner:source scanner before-close after-close)))
	  ((not (bracket-allowed? opener))
	   (inline-parser:remove-last-bracket! inline-parser)
	   (inline-parser:text inline-parser
	     (scanner:source scanner before-close after-close)))
	  (else
	   (let*-values (((dest title) (check-inline-link scanner after-close))
			 ((dest title)
			  (check-label scanner opener after-close dest title)))
	     (if dest
		 (let ((link/image ((if (bracket-image? opener)
					make-image-node
					make-link-node)
				    (inline-parser-state-block state)
				    dest title)))
		   (do ((node (markdown-node-next (bracket-node opener))
			      (markdown-node-next node)))
		       ((not node))
		     (markdown-node:append-child! link/image node))
		   (markdown-node:source-locations-set! link/image
		    (source-lines:source-loactions
		     (scanner:source scanner 
				     (bracket-mark-position opener)
				     (scanner:position scanner))))
		   (inline-parser:process-delimiters! inline-parser
		    (bracket-previous-delimiter opener))
		   (inline-parser:merge-text-nodes! inline-parser link/image)
		   (markdown-node:unlink! (bracket-node opener))
		   (inline-parser:remove-last-bracket! inline-parser)
		   (unless (bracket-image? opener)
		     (do ((bracket (parsing-state-last-bracket state)
				   (bracket-previous bracket)))
			 ((not bracket))
		       (unless (bracket-image? bracket)
			 (bracket-allowed?-set! bracket #f))))
		   link/image)
		 (begin
		   (inline-parser:remove-last-bracket! inline-parser)
		   (scanner:position! scanner after-close)
		   (inline-parser:text inline-parser
		    (scanner:source scanner before-close after-close)))))))))

(define (inline-parser:parse-line-break inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (scanner:next! scanner)
  (if (>= (parsing-state-trailing-spaces state) 2)
      (make-linebreak-node (inline-parser-state-block state))
      (make-softbreak-node (inline-parser-state-block state))))

(define (inline-parser:parse-backslash inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define block (inline-parser-state-block state))
  (scanner:next! scanner) ;; #\\
  (let ((c (scanner:peek scanner)))
    (cond ((eqv? c #\newline)
	   (scanner:next! scanner)
	   (make-linebreak-node block))
	  ((or (eqv? c #\-) (parsing:escapable? c))
	   (scanner:next! scanner)
	   (make-text-node block (string c)))
	  (else
	   (make-text-node block "\\")))))

(define (inline-parser:parse-backticks inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define block (inline-parser-state-block state))
  (define start (scanner:position scanner))
  (define open-ticks (scanner:match-char scanner #\`))

  (define (strip content)
    (define len (string-length content))
    (if (and (>= len 3)
	     (eqv? (string-ref content 0) #\space)
	     (eqv? (string-ref content (- len 1)) #\space)
	     (string-any (lambda (c) (not (eqv? c #\space))) content))
	(substring content 1 (- len 1))
	content))
  (let ((after-opening (scanner:position scanner)))
    (let loop ()
      (cond ((positive? (scanner:find-char scanner #\`))
	     (let* ((before-closing (scanner:position scanner))
		    (count (scanner:match-char scanner #\`)))
	       (if (= open-ticks count)
		   (let* ((source (scanner:source scanner
						  after-opening
						  before-closing))
			  (content (string-map
				    (lambda (c) (if (eqv? c #\newline)
						    #\space
						    c))
				    (source-lines:content source))))
		     (make-code-node block (strip content)))
		   (loop))))
	    (else
	     (let ((source (scanner:source scanner start after-opening)))
	       (scanner:position! scanner after-opening)
	       (inline-parser:text inline-parser source)))))))

(define ascii:hex-digit
  (char-set-intersection char-set:hex-digit char-set:ascii))
(define ascii:digit (char-set-intersection char-set:digit char-set:ascii))
(define ascii:letter (char-set-intersection char-set:letter char-set:ascii))
(define ascii:letter+digit
  (char-set-intersection char-set:letter+digit char-set:ascii))

(define (inline-parser:parse-entity inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))

  (define (entity scanner start)
    (define block (inline-parser-state-block state))
    (define position (scanner:position scanner))
    (define source (scanner:source scanner start position))
    (let ((text (source-lines:content source)))
      (make-text-node block (escaping:resolve-entity text))))

  (scanner:next! scanner) ;; skip #\&
  (let ((c (scanner:peek scanner))
	(pos (scanner:position scanner)))
    (define (not-entity)
      (scanner:position! scanner pos)
      (inline-parser:text inline-parser (scanner:source scanner start pos)))
    (cond ((eqv? c #\#)
	   (scanner:next! scanner)
	   (let-values (((cset bound)
			 (if (or (scanner:next-char? scanner #\x)
				 (scanner:next-char? scanner #\X))
			     (values char-set:hex-digit 6)
			     (values char-set:digit 7))))
	     (or (and-let* ((n (scanner:match-charset scanner cset))
			    ( (<= 1 n bound) )
			    ( (scanner:next-char? scanner #\;) ))
		   (entity scanner start))
		 (not-entity))))
	  ((char-set-contains? ascii:letter c)
	   (scanner:match-charset scanner ascii:letter+digit)
	   (if (scanner:next-char? scanner #\;)
	       (entity scanner start)
	       (not-entity)))
	  (else (not-entity)))))

(define *uri*
  (rx (: (/ "az") (** 1 31 (or (/ "azAZ09") ".+-")) #\:
	 (* (~ #\< #\> space control)))))
(define *email*
  (rx (: (+ (or (/ "azAZ09") ".!#$%&'*+/=?^_`{|}~-")) #\@
	 (/ "azAZ09")
	 (? (** 0 61 (or (/ "azAZ09") #\-) (/ "azAZ09"))
	    (* (: #\. (/ "azAZ09")
		  (? (** 0 61 (or (/ "azAZ09") #\-) (/ "azAZ09")))))))))
(define (inline-parser:parse-auto-link inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))
  (define (uri/email c)
    (cond ((regexp-matches *uri* c) c)
	  ((regexp-matches *email* c) (string-append "mailto:" c))
	  (else #f)))
  (define (rollback) (scanner:position! scanner start) #f)
  (scanner:next! scanner)
  (let ((s (scanner:position scanner)))
    (or (and (scanner:find-char scanner #\>)
	     (let* ((e (scanner:position scanner))
		    (source (scanner:source scanner s e))
		    (c (source-lines:content source)))
	       (scanner:next! scanner)
	       (cond ((uri/email c) =>
		      (lambda (dest)
			(let* ((block (inline-parser-state-block state))
			       (link (make-link-node block dest #f))
			       (text (make-text-node link c)))
			  (markdown-node:source-locations-set!
			   text (source-lines:source-loactions source))
			  (markdown-node:append-child! link text)
			  link)))
		     (else #f))))
	(rollback))))

(define (open-close-pattern open close)
  (rx ,open (+ (: (neg-look-ahead ,close) any)) ,close))
(define *html-comment-pattern*
  (open-close-pattern *parsing:html-comment-open-pattern*
		      *parsing:html-comment-close-pattern*))
(define *html-cdata-pattern*
  (open-close-pattern *parsing:html-cdata-open-pattern*
		      *parsing:html-cdata-close-pattern*))
(define *html-pi-pattern*
  (open-close-pattern *parsing:html-pi-open-pattern*
		      *parsing:html-pi-close-pattern*))
(define *html-declaration-pattern*
  (open-close-pattern *parsing:html-declaration-open-pattern*
		      *parsing:html-declaration-close-pattern*))

(define *html-tag-pattern*
  (rx ($ (or ,*html-comment-pattern*
	     ,*html-cdata-pattern*
	     ,*html-pi-pattern*
	     ,*html-declaration-pattern*
	     ,*parsing:html-open-tag-pattern*
	     ,*parsing:html-close-tag-pattern*))))

(define (inline-parser:parse-html-inline inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))
  (define (open-tag? scanner)
    (let ((c (scanner:content scanner start)))
      (cond ((regexp-search *html-tag-pattern* c) =>
	     (lambda (m)
	       (let ((v (regexp-match-submatch m 1)))
		 ;; ugly...
		 (do ((i 0 (+ i 1)) (len (string-length v)))
		     ((= i len) v)
		   (scanner:next! scanner)))))
	    (else #f))))
  (cond ((open-tag? scanner) =>
	 (lambda (c)
	   (make-html-inline-node (inline-parser-state-block state) c)))
	(else #f)))

(define (inline-parser:parse-text inline-parser)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define start (scanner:position scanner))
  (define (scan-until-special-char inline-parser scanner)
    (let loop ((c (scanner:peek scanner)))
      (if (or (not c) (inline-parser:special-char? inline-parser c))
	  c
	  (begin
	    (scanner:next! scanner)
	    (loop (scanner:peek scanner))))))
  (define (get-content c source)
    (let ((content (source-lines:content source)))
      (cond ((eqv? c #\newline)
	     ;; only space
	     (let ((r (string-trim-right content #\space)))
	       (parsing-state-trailing-spaces-set! state
						   (- (string-length content)
						      (string-length r)))
	       r))
	    ((not c)
	     ;; for the last line, both tabs and spaces are trimmed
	     (string-trim-right content parsing:space/tab?))
	    (else content))))
  (scanner:next! scanner)
  (let* ((c (scan-until-special-char inline-parser scanner))
	 (source (scanner:source scanner start (scanner:position scanner)))
	 (text (make-text-node (inline-parser-state-block state)
			       (get-content c source))))
    (markdown-node:source-locations-set! text
     (source-lines:source-loactions source))))

(define (inline-parser:scan-delimiters inline-parser processor c)
  (define state (inline-parser-parsing-state inline-parser))
  (define scanner (inline-parser-state-scanner state))
  (define before (scanner:peek-previous scanner))
  (define start (scanner:position scanner))
  (define (check scanner c before after dp)
    (let ((punc-b (or (not before)
		      (char-set-contains? char-set:punctuation before)))
	  (ws-b (or (not before)
		    (char-set-contains? char-set:whitespace before)))
	  (punc-a (or (not after)
		      (char-set-contains? char-set:punctuation after)))
	  (ws-a (or (not after)
		    (char-set-contains? char-set:whitespace after))))
      (let ((left (and (not ws-a) (or (not punc-a) ws-b punc-b)))
	    (right (and (not ws-b) (or (not punc-b) ws-a punc-a))))
	(if (eqv? c #\_)
	    (values (and left (or (not right) punc-b))
		    (and right (or (not left) punc-a)))
	    (values
	     (and left (eqv? c (delimiter-processor-opening-character dp)))
	     (and right (eqv? c (delimiter-processor-closing-character dp))))
	    ))))
  (let ((count (scanner:match-char scanner c)))
    (cond ((< count (delimiter-processor-min-length processor))
	   (scanner:position! scanner start)
	   (values #f #f #f))
	  (else
	   (let ((delimiters (flexible-vector)))
	     (scanner:position! scanner start)
	     (do ((pos start (scanner:position scanner)))
		 ((not (scanner:next-char? scanner c)))
	       (let ((s (scanner:source scanner pos
					(scanner:position scanner))))
		 (flexible-vector-insert-back! delimiters
		  (inline-parser:text inline-parser s))))
	     (let ((after (scanner:peek scanner)))
	       (let-values (((can-open? can-close?)
			     (check scanner c before after processor)))
		 (values delimiters can-open? can-close?))))))))

(define (inline-parser:process-delimiters! inline-parser stack-bottom)
  (define state (inline-parser-parsing-state inline-parser))
  (define processors (inline-parser-processors inline-parser))
  (define opener-bottom (make-eqv-hashtable))

  (define (get-closer state)
    (define last-delimiter (parsing-state-last-delimiter state))
    (do ((closer last-delimiter (delimiter-previous closer)))
	((or (not closer) (eq? (delimiter-previous closer) stack-bottom))
	 closer)))

  (define (find-opener dp closer open-char)
    (define delim-char (delimiter-char closer))
    (let loop ((opener (delimiter-previous closer))
	       (potential-found? #f)
	       (used-delims 0))
      (if (or (not opener) (eq? opener stack-bottom)
	      (eq? opener (hashtable-ref opener-bottom delim-char #f)))
	  (values #f opener potential-found? used-delims)
	  (cond ((and (delimiter-can-open? opener)
		      (eqv? (delimiter-char opener) open-char))
		 (let ((used-delims
			(delimiter-processor:process dp opener closer)))
		   (if (> used-delims 0)
		       (values #t opener #t used-delims)
		       (loop (delimiter-previous opener) #t used-delims))))
		(else (loop (delimiter-previous opener)
			    potential-found?
			    used-delims))))))

  (let loop ((closer (get-closer state)))
    (cond ((not closer)
	   (let loop ()
	     (let ((last (parsing-state-last-delimiter state)))
	       (unless (or (not last) (eq? last stack-bottom))
		 (inline-parser:remove-delimiter! inline-parser last)))))
	  ((and (delimiter-can-close? closer)
		(hashtable-ref processors (delimiter-char closer) #f)) =>
	   (lambda (dp)
	     (let ((open-char (delimiter-processor-opening-character dp)))
	       (let-values (((found? opener potential-found? used-delim)
			     (find-opener dp closer open-char)))
		 (cond ((not found?)
			(unless potential-found?
			  (hashtable-set! opener-bottom
					  (delimiter-char closer)
					  (delimiter-previous closer))
			  (unless (delimiter-can-open? closer)
			    (inline-parser:remove-delimiter!
			     inline-parser closer)))
			  (loop (delimiter-next closer)))
		       (else
			(do ((character (delimiter-characters opener))
			     (i 0 (+ i 1)))
			    ((= i used-delim))
			  (let ((size (flexible-vector-size character)))
			    (markdown-node:unlink!
			     (flexible-vector-delete! character (- size 1)))))
			(do ((character (delimiter-characters closer))
			     (i 0 (+ i 1)))
			    ((= i used-delim))
			  (let ((size (flexible-vector-size character)))
			    (markdown-node:unlink!
			     (flexible-vector-delete! character 0))))
			(let loop ((d (delimiter-previous closer)))
			  (unless (or (not d) (eq? d opener))
			    (let ((save (delimiter-previous d)))
			      (inline-parser:remove-delimiter! inline-parser d)
			      (loop save))))
			(when (zero? (delimiter:length opener))
			  (inline-parser:remove-delimiter!
			   inline-parser opener))
			(if (zero? (delimiter:length closer))
			    (let ((save (delimiter-next closer)))
			      (inline-parser:remove-delimiter! inline-parser
							      closer)
			      (loop save))
			    (loop closer))))))))
	  (else (loop (delimiter-next closer))))))

(define (inline-parser:merge-text-nodes! inline-parser node)
  (define from (markdown-node:first-child node))
  (define (merge! first last len)
    (when (and first last (not (eq? first last)))
      (let-values (((out e) (open-string-output-port)))
	(put-string out (text-node:content first))
	(let ((loc* (source-locations:empty))
	      (stop (markdown-node-next last)))
	  (source-locations:add! loc* (markdown-node:source-locations first))
	  (let loop ((node (markdown-node-next first)))
	    (unless (eq? node stop)
	      (put-string out (text-node:content node))
	      (source-locations:add! loc* (markdown-node:source-locations node))
	      (let ((unlink node)
		    (next (markdown-node-next node)))
		(markdown-node:unlink! unlink)
		(loop next))))
	  (text-node:content-set! first (e))
	  (markdown-node:source-locations-set! first
	   (source-locations:locations loc*))))))
  (when from
    (let ((to (markdown-node:last-child node)))
      (let loop ((node from)
		 (first #f)
		 (last #f)
		 (len 0))
	(let-values (((first last len)
		      (if (text-node? node)
			  (values (or first node)
				  node
				  (+ len (string-length
					  (text-node:content node))))
			  (begin
			    (merge! first last len)
			    (inline-parser:merge-text-nodes! inline-parser node)
			    (values #f #f 0)))))
	  (if (or (not node) (eq? node to))
	      (merge! first last len)
	      (loop (markdown-node-next node) first last len)))))))

(define (inline-parser:special-char? inline-parser c)
  (define processors (inline-parser-processors inline-parser))
  (or (memv c '(#\[ #\] #\! #\newline #\` #\\ #\& #\<))
      (hashtable-ref processors c #f)))

;; bracket
(define-vector-type bracket
  (make-bracket node mark-position content-position previous
		previous-delimiter image? allowed? barcket-after?)
  bracket?
  (node bracket-node)
  (mark-position bracket-mark-position)
  (content-position bracket-content-position)
  (previous bracket-previous)
  (previous-delimiter bracket-previous-delimiter)
  (image? bracket-image?)
  (allowed? bracket-allowed? bracket-allowed?-set!)
  (barcket-after? bracket-bracket-after? bracket-bracket-after?-set!))

(define (bracket:link node mark-position content-position previous
		      previous-delimiter)
  (make-bracket node mark-position content-position
		previous previous-delimiter #f #t #f))
(define (bracket:image node mark-position content-position previous
		       previous-delimiter)
  (make-bracket node mark-position content-position
		previous previous-delimiter #t #t #f))

;;; default delimiter processors
(define-record-type emphasis-delimiter-processor
  (parent delimiter-processor)
  (protocol (lambda (n)
	      (lambda (delimiter-char)
		((n delimiter-char delimiter-char 1
		    emphasis-delimiter-processor:process))))))
(define (emphasis-delimiter-processor:process dp opening-run closing-run)
  (define opening-can-close? (delimiter-can-close? opening-run))
  (define closing-can-open? (delimiter-can-open? closing-run))
  (define opening-original-length (delimiter-original-length opening-run))
  (define closing-original-length (delimiter-original-length closing-run))
  (define delimiter-char (delimiter-processor-opening-character dp))

  (if (and (or opening-can-close? closing-can-open?)
	   (not (zero? (mod closing-original-length 3)))
	   (zero? (+ opening-original-length
		     (mod closing-original-length 3))))
      0
      (let ((opener (delimiter:opener opening-run))
	    (closer (delimiter:closer closing-run))
	    (locations (source-locations:empty)))
	(let-values (((emphasis used-delimiters)
		      (if (and (>= (delimiter:length opening-run) 2)
			       (>= (delimiter:length closing-run) 2))
			  (values (make-strong-emphasis-node opener
				   (string delimiter-char delimiter-char))
				  2)
			  (values (make-emphasis-node
				   opener (string delimiter-char)) 1))))
	  (define (add! n)
	    (source-locations:add! locations
				   (markdown-node:source-locations n)))
	  (for-each add! (delimiter:opener* opening-run used-delimiters))
	  (generator-for-each (lambda (n)
				(markdown-node:append-child! emphasis n)
				(add! n))
			      (markdown-node-between opener closer))
	  (for-each add! (delimiter:closer* closing-run used-delimiters))
	  (markdown-node:source-locations-set! emphasis
	   (source-locations:locations locations))
	  (markdown-node:insert-after! opener emphasis)
	  used-delimiters))))

(define-record-type asterisk-delimiter-processor
  (parent emphasis-delimiter-processor)
  (protocol (lambda (n) (lambda () ((n #\*))))))
(define-record-type underscore-delimiter-processor
  (parent emphasis-delimiter-processor)
  (protocol (lambda (n) (lambda () ((n #\_))))))

)
