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

(define command-char-set
  (char-set-complement (string->char-set "[]{}()|\"")))
(define (command-char? c)
  (and (not (char-whitespace? c))
       (char-set-contains? command-char-set c)))
  
(define $command-chars ($input-pred command-char?))

;; FIXME better not to have this but at this moment
;;       I don't have better idea
(define (list-prefix? prefix ls)
  (cond ((null? prefix) #t)
        ((null? ls) #f)
        ((equal? (car prefix) (car ls)) (list-prefix? (cdr prefix) (cdr ls)))
        (else #f)))
(define ($text-chars l)
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
	
  (let loop ((nl l) (depth 0) (r '()))
    (if (null? nl)
	(return (reverse! r) nl)
	(let ((c (input-char nl)))
	  (if (check-escape c nl)
	      (return (reverse! r) nl)
	      (case c
		((#\{) (loop (lseq-cdr nl) (+ depth 1) (cons c r)))
		((#\})
		 (if (zero? depth)
		     (let lp ((p punc) (nl2 (lseq-cdr nl)))
		       (cond ((null? p) (return (reverse! r) nl))
			     ((null? nl2) (return (reverse! r) nl))
			     ((eqv? (car p) (input-char nl2))
			      (lp (cdr p) (lseq-cdr nl2)))
			     (else
			      (loop (lseq-cdr nl) (- depth 1) (cons c r)))))
		     (loop (lseq-cdr nl) (- depth 1) (cons c r))))
		((#\newline) (return (reverse! r) nl))
		(else (loop (lseq-cdr nl) depth (cons c r)))))))))

(define $text ($let ((c* $text-chars)) ($return (list->string c*))))

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

(define $cmd-body
  ($or ($let ((loc $location)
	      ( ($input-eqv? #\() )
	      (text $parentheses)
	      ( ($input-eqv? #\)) ))
	  ($return (string->datum loc text)))
       ($let ((cmd ($many $command-chars 1)))
	  ($return (string->symbol (list->string (map car cmd)))))))
(define $cmd ($seq $@ $cmd-body))

(define $datum ($let ((loc $location)
		      ( ($input-eqv? #\[) )
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
  (define (reflect ch)
    ;; TODO do we need to do this, ')(', kind of mirro? 
    (case ch ((#\() #\)) ((#\[) #\]) ((#\{) #\}) ((#\<) #\>) (else ch)))
  (list->string (map reflect punc)))
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

(define $command
  ($or $quoted-command
       ($let ((cmd $cmd) (datum $datum) (text $command-body))
	 ($return `(,cmd ,datum ,text)))
       ($let ((cmd $cmd) (datum $datum)) ($return `(,cmd ,datum ())))
       ($let ((cmd $cmd) (text $command-body)) ($return `(,cmd () ,text)))
       ($let (( $@ ) (text $text-body)) ($return `(#f () ,text)))
       ($let (( $@ ) (datum $datum)) ($return `(#f ,datum ())))
       ($let ((cmd $cmd)) ($return `(,cmd #f #f)))))

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
	 ($return (list (list->string (map car c*)))))))

(define $scribble-token
  ($let* ((loc $location))
    ($or ($let* ((escape $escape)) ($return `(token (@ ,@loc) ,@escape)))
	 ($let* ((token ($or $command
			     $text
			     ($seq ($input-eqv? #\newline) ($return "\n")))))
	   ($return `(token (@ ,@loc) ,token))))))

(define $scribble-token* ($many $scribble-token))

(define $scribble-document
  ($let ((loc $location)
	 (tokens $scribble-token*))
    ($return `(document (@ ,@loc) (content ,@tokens)))))

(define (scribble-parse input)
  (define (merge-string texts)
    (let loop ((texts texts) (r '()))
      (cond ((null? texts) (reverse! r))
	    ((null? (cdr texts)) (reverse! (cons (car texts) r)))
	    ((and (string? (car texts)) (string? (cadr texts)))
	     (if (and (not (string=? "\n" (car texts)))
		      (not (string=? "\n" (cadr texts))))
		 (loop (cons (string-append (car texts) (cadr texts))
			     (cddr texts))
		       r)
		 (loop (cdr texts) (cons (car texts) r))))
	    (else (loop (cdr texts) (cons (car texts) r))))))
  (define (strip-info v)
    (define (quotes? v)
      (or (eq? v 'quote) (eq? v 'unquote) (eq? v 'unquote-splicing)
	  (eq? v 'quasiquote)))
    (match v
      (('token ('@ loc ...) ((? quotes? x) rest))
       `(,x ,(strip-info `(token (@ ,@loc) ,rest))))
      (('token ('@ loc ...) (cmd datum text))
       (if (and datum text)
	   (let ((str* (merge-string (map strip-info text))))
	     (if cmd
		 `(,cmd ,@datum ,@str*)
		 `(,@datum ,@str*)))
	   ;; (cmd #f #f) must be just `cmd`
	   cmd))
      ;; text case
      (('token ('@ loc ...) cmd) cmd)))
  
  (let-values (((s v n) ($scribble-token* (input document:simple-lexer))))
    (if (parse-success? s)
	(map strip-info v)
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
