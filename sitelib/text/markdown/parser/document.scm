;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/document.scm - Document parser
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
(library (text markdown parser document)
    (export make-document-parser document-parser?
	    document-parser:parse)
    (import (rnrs)
	    (core misc)
	    (srfi :117 list-queues)
	    (text markdown parser blocks)
	    (text markdown parser factories)
	    (text markdown parser inlines)
	    (text markdown parser nodes)
	    (text markdown parser parsing)
	    (text markdown parser source)
	    (util port))

(define-vector-type open-block-parser 
  (make-open-block-parser block-parser source-index)
  open-block-parser?
  (block-parser open-block-parser-block-parser)
  (source-index open-block-parser-source-index
		open-block-parser-source-index-set!))

(define-record-type document-parser
  (fields block-parser-factories
	  inline-parser-factories
	  delimiter-processors
	  document-block-parser
	  open-block-parsers
	  state
	  (mutable column-in-tab?)
	  (mutable next-non-space-column)
	  block-parsers ;; for inline parser
	  )
  (protocol
   (lambda (p)
     (lambda (block-parser-factories
	      inline-parser-factories
	      delimiter-processors)
       (let ((document-block-parser (make-document-block-parser)))
	 (p block-parser-factories inline-parser-factories
	    delimiter-processors document-block-parser
	    (list-queue (make-open-block-parser document-block-parser 0))
	    (make-parser-state
	     (block-parser-block document-block-parser)
	     #f ;; line
	     -1 ;; line-index
	     0  ;; index
	     0	;; column
	     0  ;; next-non-space-index
	     0	;; indent
	     #f	;; blank
	     )
	    #f
	    0
	    (list-queue)))))))

(define (document-parser:parse document-parser input-port)
  (port-for-each (lambda (line)
		   (document-parser:parse-line document-parser line))
		 (lambda () (get-line input-port)))
  (document-parser:finalize document-parser))

;; private APIs
(define (document-parser:parse-line document-parser line)
  (define open-block-parsers
    (document-parser-open-block-parsers document-parser))
  (define (check-open-block-parser document-parser open-block-parsers)
    (if (= (list-queue-length open-block-parsers) 1)
	1 ;; document-block-parser would always match so skip ;)
	(let loop ((matches 1)
		   (i 1)
		   (obp* (cdr (list-queue-list open-block-parsers))))
	  (if (null? obp*)
	      matches
	      (let ((bp (open-block-parser-block-parser (car obp*)))
		    (state (document-parser-state document-parser)))
		(document-parser:find-next-non-space! document-parser)
		(cond ((block-parser:try-continue bp state) =>
		       (lambda (bc)
			 (open-block-parser-source-index-set!
			  (car obp*)
			  (parser-state-index state))
			 (cond ((block-continue-finalize? bc)
				(document-parser:add-source-location!
				 document-parser)
				(document-parser:close-block-parsers!
				 document-parser
				 (- (list-queue-length open-block-parsers) i))
				#f)
			       (else
				(cond ((not (= (block-continue-index bc) -1))
				       (document-parser:set-new-index!
					document-parser
					(block-continue-index bc)))
				      ((not (= (block-continue-column bc) -1))
				       (document-parser:set-new-column!
					document-parser
					(block-continue-column bc))))
				(loop (+ matches 1) (+ i 1) (cdr obp*))))))
		      (else matches)))))))

  (define (check-new-blocks document-parser block-parser matches)
    (define state (document-parser-state document-parser))
    (define (prepare-active-block-parser! document-parser)
      (let* ((obp (document-parser:deactivate-block-parser! document-parser))
	     (old (open-block-parser-block-parser obp)))
	(when (paragraph-parser? old)
	  (document-parser:add-definition-form! document-parser old))
	(block-parser:close-block! old)
	(let ((block (block-parser-block old)))
	  (markdown-node:unlink! block)
	  block)))
    (define (get-replaced-source-locs document-parser block-start)
      (and (block-start-replace-active-block-parser? block-start)
	   (let ((replaced-block
		  (prepare-active-block-parser! document-parser)))
	     (markdown-node:source-locations replaced-block))))
    (define (find-block-start document-parser block-parser)
      (let ((mbp (make-matched-block-parser block-parser)))
	(let loop ((factories
		    (document-parser-block-parser-factories document-parser)))
	  (cond ((null? factories) #f)
		(((car factories) (document-parser-state document-parser) mbp))
		(else (loop (cdr factories)))))))
    
    
    (let loop ((unmatched (- (list-queue-length open-block-parsers) matches))
	       (block-parser block-parser)
	       (last-index (parser-state-index state))
	       (started-new-block? #f)
	       (try-block-starts?
		(or (paragraph-node? (block-parser-block block-parser))
		    (block-parser-container? block-parser))))
      (document-parser:find-next-non-space! document-parser)
      (cond ((or (parser-state-blank? state)
		 (and (< (parser-state-index state) +parsing-code-block-indent+)
		      (source-line:letter?
		       (parser-state-line state)
		       (parser-state-next-non-space-index state))))
	     (document-parser:set-new-index! document-parser
	      (parser-state-next-non-space-index state))
	     (values last-index started-new-block? unmatched block-parser))
	    ((find-block-start document-parser block-parser) =>
	     (lambda (block-start)
	       (let ((source-index (parser-state-index state)))
		 ;; okay, from now on always new block started
		 (when (positive? unmatched)
		   ;; close open block here as we are handling a new block
		   (document-parser:close-block-parsers!
		    document-parser unmatched))
		 (cond ((not (= (block-start-new-index block-start) -1))
			(document-parser:set-new-index! document-parser
			 (block-start-new-index block-start)))
		       ((not (= (block-start-new-column block-start) -1))
			(document-parser:set-new-column! document-parser
			 (block-start-new-column block-start))))
		 (let ((replaced-source-locs
			(get-replaced-source-locs document-parser block-start)))
		   (let lp2 ((new-bp* (block-start-parsers block-start))
			     (block-parser block-parser)
			     (try-block-starts? try-block-starts?))
		     (if (null? new-bp*)
			 (loop 0
			       block-parser
			       (parser-state-index state)
			       #t
			       try-block-starts?)
			 (let ((new-block-parser (car new-bp*)))
			   (document-parser:add-child! document-parser
			    (make-open-block-parser new-block-parser 
						    source-index))
			   (markdown-node:source-locations-set!
			    (block-parser-block new-block-parser)
			    replaced-source-locs)
			   (lp2 (cdr new-bp*)
				new-block-parser
				(block-parser-container? new-block-parser))))))
		 )))
	    (else
	     (document-parser:set-new-index! document-parser
	      (parser-state-next-non-space-index state))
	     (values last-index started-new-block? unmatched block-parser)))))
  (define state (document-parser-state document-parser))
  
  (document-parser:set-line! document-parser line)
  (let ((matches (check-open-block-parser document-parser open-block-parsers)))
    (when matches
      (let-values (((last-index started-new-block? unmatched block-parser)
		    (check-new-blocks document-parser
		     (open-block-parser-block-parser
		      (list-ref (list-queue-list open-block-parsers)
				(- matches 1)))
		     matches)))
	(cond ((and (not started-new-block?)
		    (not (parser-state-blank? state))
		    (block-parser-allow-lazy-continuation-line?
		     (document-parser:active-block-parser document-parser)))
	       (let ((obp (list-ref (list-queue-list open-block-parsers)
			    (- (list-queue-length open-block-parsers) 1))))
		 (open-block-parser-source-index-set! obp last-index)
		 (document-parser:add-line! document-parser)))
	      (else
	       (when (> unmatched 0)
		 (document-parser:close-block-parsers!
		  document-parser unmatched))
	       (cond ((not (block-parser-container? block-parser))
		      (document-parser:add-line! document-parser))
		     ((not (parser-state-blank? state))
		      (let ((pb (make-paragraph-parser
				 (parser-state-document state))))
			(document-parser:add-child! document-parser
			 (make-open-block-parser pb last-index))
			(document-parser:add-line! document-parser)))
		     (else
		      (document-parser:add-source-location! document-parser)))
	       ))))))

(define (document-parser:finalize document-parser)
  (define open-block-parsers
    (document-parser-open-block-parsers document-parser))
  (define (process-inlines document-parser)
    ;; TODO
    (define context (make-inline-parser-context '() '()))
    (define inline-parser (make-inline-parser context))
    (list-queue-for-each
     (lambda (bp) (block-parser:parse-inlines! bp inline-parser))
     (document-parser-block-parsers document-parser)))

  (document-parser:close-block-parsers! document-parser
					(list-queue-length open-block-parsers))
  (process-inlines document-parser)
  (block-parser-block (document-parser-document-block-parser document-parser)))

;; private
(define (document-parser:find-next-non-space! document-parser)
  (define state (document-parser-state document-parser))
  (define line (parser-state-line state))
  (define len (source-line:length line))
  (define (finish document-parser state i cols)
    (parser-state-next-non-space-index-set! state i)
    (document-parser-next-non-space-column-set! document-parser cols)
    (parser-state-indent-set! state (- cols (parser-state-column state))))
  
  (parser-state-blank?-set! state #t)
  (let loop ((i (parser-state-index state)) (cols (parser-state-column state)))
    (if (< i len)
	(let ((c (source-line:char-at line i)))
	  (case c
	    ((#\space) (loop (+ i 1) (+ cols 1)))
	    ((#\tab)
	     (loop (+ i 1) (+ cols (parsing:columns->next-tab-stop cols))))
	    (else (parser-state-blank?-set! state #f)
		  (finish document-parser state i cols))))
	(finish document-parser state i cols))))

(define (document-parser:set-line! document-parser line)
  (define state (document-parser-state document-parser))
  (let ((line-index (+ (parser-state-line-index state) 1)))
    (parser-state-line-index-set! state line-index)
    (parser-state-index-set! state 0)
    (parser-state-column-set! state 0)
    (document-parser-column-in-tab?-set! document-parser #f)
    ;; TODO replace \x0; to \xFFFD;?
    (let ((loc (source-location:of line-index 0 (string-length line))))
      (parser-state-line-set! state (source-line:of line loc)))))

(define (document-parser:add-line! document-parser)
  (define state (document-parser-state document-parser))
  (define index (parser-state-index state))
  (define line (parser-state-line state))
  (define column-in-tab?
    (document-parser-column-in-tab? document-parser))
  (define (get-content)
    (cond (column-in-tab?
	   (let* ((after-tab (+  1))
		  (rest (source-line:substring line after-tab))
		  (space (parsing:columns->next-tab-stop
			  (parser-state-column state))))
	     (string-append (make-string space #\space)
			    (source-line-content rest))))
	  ((zero? index) (source-line-content line))
	  (else (source-line-content (source-line:substring line index)))))
  (let* ((content (get-content))
	 (loc (source-location:of (parser-state-line-index state) index
				  (string-length content))))
    (block-parser:add-line!
     (document-parser:active-block-parser document-parser)
     (source-line:of content loc))
    (document-parser:add-source-location! document-parser)))

(define (document-parser:add-child! document-parser open-block-parser)
  (define block-parser (open-block-parser-block-parser open-block-parser))
  (define block (block-parser-block block-parser))
  (do ()
      ((block-parser:can-contain?
	(document-parser:active-block-parser document-parser) block))
    (document-parser:close-block-parsers! document-parser 1))
  (markdown-node:append-child!
   (block-parser-block
    (document-parser:active-block-parser document-parser))
   block)
  (document-parser:activate-block-parser! document-parser open-block-parser))

(define (document-parser:set-new-index! document-parser index)
  (define state (document-parser-state document-parser))
  (define next-non-space (parser-state-next-non-space-index state))
  (when (>= index next-non-space)
    (parser-state-index-set! state next-non-space)
    (let ((col (document-parser-next-non-space-column document-parser)))
      (parser-state-column-set! state col)))
  (let ((len (source-line:length (parser-state-line state))))
    (let loop ()
      (when (and (< (parser-state-index state) index)
		 (not (= (parser-state-index state) len)))
	(document-parser:advance! document-parser)))
    (document-parser-column-in-tab?-set! document-parser #f)))

(define (document-parser:set-new-column! document-parser column)
  (define state (document-parser-state document-parser))
  (define next-non-space-column
    (document-parser-next-non-space-column document-parser))
  (when (>= column next-non-space-column)
    (parser-state-index-set! state (parser-state-next-non-space-index state))
    (parser-state-column-set! state next-non-space-column))
  (let ((len (source-line:length (parser-state-line state))))
    (let loop ()
      (when (and (< (parser-state-column state) column)
		 (not (= (parser-state-index state) len)))
	(document-parser:advance! document-parser)))
    (cond ((> (parser-state-column state) column)
	   (parser-state-index-set! state (- (parser-state-index state) 1))
	   (parser-state-column-set! state column)
	   (document-parser-column-in-tab?-set! document-parser #t))
	  (else
	   (document-parser-column-in-tab?-set! document-parser #f)))))

(define (document-parser:advance! document-parser)
  (define state (document-parser-state document-parser))
  (define line (parser-state-line state))
  (define index (parser-state-index state))
  (define column (parser-state-column state))
  (let ((c (source-line:char-at line index)))
    (parser-state-index-set! state (+ index 1))
    (if (eqv? c #\tab)
	(parser-state-column-set! state (parsing:columns->next-tab-stop column))
	(parser-state-column-set! state (+ column 1)))))

(define (document-parser:active-block-parser document-parser)
  (define open-block-parsers
    (document-parser-open-block-parsers document-parser))
  (let-values (((first last) (list-queue-first-last open-block-parsers)))
    (open-block-parser-block-parser (if (null? last) first (car last)))))

(define (document-parser:activate-block-parser! document-parser
						open-block-parser)
  (define open-block-parsers
    (document-parser-open-block-parsers document-parser))
  (list-queue-add-back! open-block-parsers open-block-parser))

(define (document-parser:deactivate-block-parser! document-parser)
  (define open-block-parsers
    (document-parser-open-block-parsers document-parser))
  (list-queue-remove-back! open-block-parsers))

(define (document-parser:add-source-location! document-parser)
  )
(define (document-parser:close-block-parsers! document-parser size)
  (define (finalize document-parser bp)
    (when (paragraph-parser? bp)
      (document-parser:add-definition-form! document-parser bp))
    (block-parser:close-block! bp))
  (define all-block-parsers (document-parser-block-parsers document-parser))
  (do ((i 0 (+ i 1)))
      ((= i size))
    (let* ((obp (document-parser:deactivate-block-parser! document-parser))
	   (bp (open-block-parser-block-parser obp)))
      (finalize document-parser bp)
      (list-queue-add-back! all-block-parsers bp))))

(define (document-parser:add-definition-form! document-parser old)
  )
)
