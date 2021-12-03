;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/format/scribble.scm - Scribble to document parser
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

;; Based on https://docs.racket-lang.org/scribble/reader.html
;; we basically don't support contextual indentation, at least for now
#!nounbound
(library (sagittarius document format scribble)
    (export scribble->document

	    ;; for backward compatibility
	    scribble-parse
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius document input)
	    (match)
	    (peg)
	    (srfi :1 lists)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (srfi :127 lseqs))


(define $whitespace ($input-pred char-whitespace?))
(define $vertical-bar ($input-eqv? #\|))

(define *in-escape?* (make-parameter #f))
(define *open-puncture* (make-parameter ""))
(define *close-puncture* (make-parameter ""))
(define $@
  ($let* (( ($if (*in-escape?*) $vertical-bar ($empty "")) )
	  ( ($input-eqv? #\@) ))
    ($return '@))) ;; won't be used anyway...

(define $open-brace
  ($let* (( ($if (*in-escape?*) $vertical-bar ($empty "")) )
	  ( ($input-token *open-puncture*) )
	  ( ($input-eqv? #\{) ))
    ($return 'open-brace)))
(define $close-brace
  ($let* (( ($input-eqv? #\}) )
	  ( ($input-token *close-puncture*) )
	  ( ($if (*in-escape?*) $vertical-bar ($empty "")) ))
    ($return 'close-brace)))

(define (string->datum loc s)
  (define in (open-string-input-port s))
  (guard (e (else (document-input-error '$datum
					"Invalid S-exp datum"
					s loc)))
    (let loop ((r '()))
      (let ((d (get-datum in)))
	(if (eof-object? d)
	    (reverse r)
	    (loop (cons d r)))))))

(define ($notp parser) ($seq ($peek ($not parser)) $any))

(define (mirror-char ch)
  ;; TODO do we need to do this, ')(', kind of mirro? 
  (case ch ((#\() #\)) ((#\[) #\]) ((#\{) #\}) ((#\<) #\>) (else ch)))
;; FIXME better not to have this but at this moment
;;       I don't have better idea
(define ($text-chars open)
  (define close (mirror-char open))
  (lambda (l)
    (define escape (if (*in-escape?*) '(#\|) '()))
    (define punc `(,@(string->list (*close-puncture*)) . ,escape))
    (define (return r nl)
      (if (null? r)
	  (return-expect "Text char" l)
	  (return-result r nl)))
    (define (check-escape c nl)
      (let lp ((nl2 nl) (escape escape))
	(cond ((null? escape)
	       (and (not (null? nl2)) (eqv? (input-char nl2) #\@)))
	      ((null? nl2) #f)
	      ((eqv? (car escape) (input-char nl2))
	       (lp (lseq-cdr nl2) (cdr escape)))
	      (else #f))))
    (define (merge-buf buf r)
      (let ((s (list->string (reverse! buf))))
	(if (zero? (string-length s))
	    r
	    (cons s r))))
    (let loop ((nl l) (depth 0) (buf '()) (r '()))
      (if (null? nl)
	  (return (reverse! (merge-buf buf r)) nl)
	  (let ((c (input-char nl)))
	    (cond ((check-escape c nl)
		   (return (reverse! (merge-buf buf r)) nl))
		  ((eqv? c open)
		   (loop (lseq-cdr nl) (+ depth 1) (cons c buf) r))
		  ((eqv? c close)
		   (if (zero? depth)
		       (let lp ((p punc) (nl2 (lseq-cdr nl)))
			 (cond ((null? p)
				(return (reverse! (merge-buf buf r)) nl))
			       ((null? nl2)
				(return (reverse! (merge-buf buf r)) nl))
			       ((eqv? (car p) (input-char nl2))
				(lp (cdr p) (lseq-cdr nl2)))
			       (else
				(loop (lseq-cdr nl) (- depth 1)
				      (cons c buf) r))))
		       (loop (lseq-cdr nl) (- depth 1) (cons c buf) r)))
		  ((eqv? c #\newline)
		   (let ((r (merge-buf buf r)))
		     (if (zero? depth)
			 (return (reverse! r) nl)
			 (loop (lseq-cdr nl) depth '() (cons "\n" r)))))
		  (else (loop (lseq-cdr nl) depth (cons c buf) r))))))))

(define $text
  ($let ((loc $location) (c* ($text-chars #\{)))
    ($return `(text (@ ,@loc) ,@c*))))

;; not a good way of handling this, but the same as $text-chars...
(define ($parentheses l)
  (let loop ((nl l) (depth 0) (r '()))
    (if (null? nl)
	(return-expect "Non closed ()" l)
	(let ((c (input-char nl)))
	  (case c
	    ((#\)) (if (zero? depth)
		       (return-result (list->string (reverse! r)) nl)
		       (loop (lseq-cdr nl) (- depth 1) (cons c r))))
	    ((#\() (loop (lseq-cdr nl) (+ depth 1) (cons c r)))
	    (else (loop (lseq-cdr nl) depth (cons c r))))))))

(define command-char-set
  (char-set-complement (string->char-set "[]{}()|\"")))
(define (command-char? c)
  (and (not (char-whitespace? c))
       (char-set-contains? command-char-set c)))
  
(define $command-chars ($input-pred command-char?))
(define $cmd-body
  ($or ($let ((loc $location)
	      ( ($input-eqv? #\() )
	      (text $parentheses)
	      ( ($input-eqv? #\)) ))
	  ($return (string->datum loc text)))
       ($let ((cmd ($many $command-chars 1)))
	  ($return (string->symbol (list->string (map car cmd)))))))
(define $cmd ($seq $@ $cmd-body))

(define $sexp-text ($let ((c* ($text-chars #\[))) ($return (list->string c*))))

(define $datum 
  ($let ((loc $location)
	 ( ($input-eqv? #\[) )
	 ;;(datum ($many ($or ($lazy $command))))
	 (datum ($many ($notp ($input-eqv? #\]))))
	 ( ($input-eqv? #\]) ))
    ($return (string->datum loc (list->string (map car datum))))))

(define $text-body
  ($let* (( $open-brace )
	  (datum ($many $scribble-token))
	  ( $close-brace ))
   ($return datum)))

(define $puncture
  ($let (( $vertical-bar )
	 (c* ($many ($notp $open-brace))))
    ($return (map car c*))))
(define (mirror punc)
  (list->string (map mirror-char punc)))
(define $escaped-body
  ($let* ((punc ($peek $puncture))
	  ;; $text-body contains $open and $close brace
	  ;;( $open-brace)
	  (body ($parameterize ((*open-puncture* (list->string punc))
				(*close-puncture* (mirror punc))
				(*in-escape?* #t))
		  $text-body))
	  ;;( $close-brace )
	  )
   ($return body)))

(define $command-body
  ;; After |@, these 2 parameters must be reset...
  ($parameterize ((*in-escape?* #f))
    ($or $escaped-body $text-body)))

(define $quote ($seq ($input-eqv? #\') ($return 'quote)))
(define $quasiquote ($seq ($input-eqv? #\`) ($return 'quasiquote)))
(define $unquote ($seq ($input-eqv? #\,) ($return 'unquote)))
(define $unquote-splicing
  ($seq ($input-token ",@") ($return 'unquote-splicing)))

(define $quote+
  ($many ($or $quote $quasiquote $unquote-splicing $unquote) 1))

(define (merge-quote quote* cmd datum text)
  (let loop ((quote* quote*))
    (if (null? quote*)
	(list cmd datum text)
	(list (car quote*) (loop (cdr quote*))))))
  

(define $quoted-command
  ($let (( $@ )
	 (quote+ $quote+))
    ($or ($let ((cmd $cmd-body) (d $datum) (t $command-body))
	   ($return (merge-quote quote+ cmd d t)))
	 ($let ((cmd $cmd-body) (d $datum))
	   ($return (merge-quote quote+ cmd d '())))
	 ($let ((cmd $cmd-body) (t $command-body))
	   ($return (merge-quote quote+ cmd '() t)))
	 ($let ((d $datum))
	   ($return (merge-quote quote+ #f d '())))
	 ($let ((t $command-body))
	   ($return (merge-quote quote+ #f '() t)))
	 ($let ((cmd $cmd-body)) ($return (merge-quote quote+ cmd #f #f))))))

(define $normal-command
  ($or ($let ((cmd $cmd) (datum $datum) (text $command-body))
	 ($return `(,cmd ,datum ,text)))
       ($let ((cmd $cmd) (datum $datum)) ($return `(,cmd ,datum ())))
       ($let ((cmd $cmd) (text $command-body)) ($return `(,cmd () ,text)))
       ($let (( $@ ) (text $text-body)) ($return `(#f () ,text)))
       ($let (( $@ ) (datum $datum)) ($return `(#f ,datum ())))
       ($let ((cmd $cmd)) ($return `(,cmd #f #f)))))
;; Not supported, the document says
;; 'no special rules for using @ in the command itself'
;; so, we don't do
(define $command-in-command
  ($or ($let (( $@ ) (c ($lazy $command)) (datum $datum) (text $command-body))
	 ($return `(,c ,datum ,text)))
       ($let (( $@ ) (c ($lazy $command)) (datum $datum)) 
	 ($return `(,c ,datum ())))
       ($let (( $@ ) (c ($lazy $command)) (text $command-body)) 
	 ($return `(,c () ,text)))))

(define $command
  ($let ((loc $location)
	 (command ($or $quoted-command
		       $normal-command)))
    ($return `(command (@ ,@loc) ,command))))

(define $escape
  ($or ($let ((loc $location)
	      ( $@ )
	      ( $vertical-bar )
	      (s* ($many ($notp $vertical-bar)))
	      ( $vertical-bar ))
	 ($return (string->datum loc (list->string (map car s*)))))
       ($let (( $@ )
	      ( ($input-eqv? #\") )
	      (c* ($many ($notp ($input-eqv? #\"))))
	      ( ($input-eqv? #\") ))
	 ($return (list->string (map car c*))))))

(define $nl ($seq ($input-eqv? #\newline) ($return '(text (@) "\n"))))
(define $comment
  ($let ((loc $location)
	 ( ($input-token "@;") ))
     ($or ($let (( ($input-eqv? #\{) )
		 (c* ($many ($notp ($input-token ";}"))))
		 ( ($input-token ";}") ))
	    ($return `(comment (@ ,@loc) ,(list->string (map car c*)))))
	  ($let ((c* ($many ($notp $nl)))
		 ( $nl ))
	    ($return `(comment (@ ,@loc) ,(list->string (map car c*))))))))

(define $scribble-token
  ($let* ((loc $location))
    ($or ($let* ((escape $escape)) 
	   ($return `(token (@ ,@loc) (escape (@ ,@loc) ,escape))))
	 ($let* ((token ($or $comment
			     $command
			     $text
			     ;; This has to be an indivisual token...
			     $nl)))
	   ($return `(token (@ ,@loc) ,token))))))

(define $scribble-token* ($many $scribble-token))

(define $scribble-document
  ($let ((loc $location)
	 (tokens $scribble-token*))
    ($return `(document (@ ,@loc) (content ,@tokens)))))

(define (scribble-parse input)
  (define (quotes? v)
    (or (eq? v 'quote) (eq? v 'unquote) (eq? v 'unquote-splicing)
	(eq? v 'quasiquote)))
  (define (merge-string command)
    (define (do-merge cmd datum texts)
      (define (merge s*)
	(define (finish r)
	  (if (for-all (lambda (v) (and (string? v) (string=? "\n" v))) r)
	      '()
	      r))
	(define (concat-buf buf)
	  (if (null? buf)
	      #f
	      (string-concatenate (reverse! buf))))
	;; (write s*) (newline)
	(let loop ((s* s*) (buf '()) (r '()))
	  (if (null? s*)
	      (let ((t (concat-buf buf)))
		(finish (reverse! (if t (cons t r) r))))
	      (let ((d (car s*)))
		(if (string? d)
		    (if (string=? "\n" d)
			(let ((s (concat-buf buf)))
			  (loop (cdr s*) '() (cons "\n" (if s (cons s r) r))))
			(loop (cdr s*) (cons d buf) r))
		    (let ((s (concat-buf buf)))
		      (loop (cdr s*) '() (cons d (if s (cons s r) r)))))))))
      ;; (write texts) (newline)
      (let loop ((texts texts) (r '()))
	(match texts
	  (() (if cmd `(,cmd ,@datum ,@(merge r)) `(,@datum ,@(merge r))))
	  ((((? string? s) ...) rest ...)
	   (loop rest `(,@r . ,s)))
	  ((('escape text ...) rest ...) (loop rest `(,@r . ,text)))
	  ;; escaped datum
	  ((((datum ...)) rest ...) (loop rest `(,@r . ,datum)))
	  ((cmd rest ...)
	   (loop rest `(,@r ,(merge-string cmd))))
	  (else (error 'merge-string texts)))))
    ;; (write command) (newline)
    (match command
      (((? quotes? x) datum) `(,x ,(merge-string datum)))
      (((? string? s)) s) ;; top level text token
      (('escape datum ...) datum)
      (((? symbol? cmd) datum ... ('text texts ...)) (do-merge cmd datum texts))
      ((datum ... ('text texts ...)) (do-merge #f datum texts))
      (else command)))
  
  (define (strip-command v)
    ;;(write v) (newline)
    (match v
      (('command ('@ loc ...) ((? quotes? x) rest))
       `(,x ,(strip-command `(command (@ ,@loc) ,rest))))
      (('command ('@ loc ...) (cmd datum text))
       (if (and datum text)
	   (let ((str* (filter-map strip-token text)))
	     (if cmd
		 `(,cmd ,@datum (text ,@str*))
		 `(,@datum (text ,@str*))))
	   cmd))
      (('command ('@ loc ...) (? symbol? cmd)) cmd)
      (('comment ('@ loc ...) s) #f)
      (('text ('@ loc ...) s* ...) s*)
      (('escape ('@ loc ...) s* ...) s*)
      (else (error 'strip-command "unknown" v))))
  (define (strip-token v)
    (match v
      (('token ('@ loc ...) cmd) (strip-command cmd))
      (else (error 'strip-token "unknown" v))))
  
  (let-values (((s v n) ($scribble-token* (input document:simple-lexer))))
    (if (parse-success? s)
	(map merge-string (filter-map strip-token v))
	(document-input-error 'scribble-parse
			      "Failed to parse scribble file"
			      n))))

(define (scribble->document input)
  (let-values (((s v n) ($scribble-document (input document:simple-lexer))))
    (if (parse-success? s)
	v
	(document-input-error 'scribble->document
			      "Failed to parse scribble file"
			      ;; TODO proper location
			      ))))

)
