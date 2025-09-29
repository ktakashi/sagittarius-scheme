;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/input.scm - Parser input
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

;; A library provide lazy sequence which can be used
;; by PEG parser.
;; The lazy seuqnce returns a list (lexer value source-location source-file)
#!nounbound
(library (sagittarius document input)
    (export file->document-input
	    port->document-input

	    parse-document-input

	    document-input?
	    document-input-port
	    document-input-filename
	    document-input->lseq

	    (rename (document-input-options <document-input-options>))
	    document-input-options?
	    document-input-options-builder

	    input-char input-loc input-file

	    $location $location-file
	    $input-eqv? $input-pred $input-token $input-token-ci
	    $input-char-set-contains
	    document:simple-lexer ;; for convenence
	    
	    document-input-error
	    &document-input make-document-input-error document-input-error?
	    )
    (import (rnrs)
	    (record builder)
	    (sagittarius)
	    (sagittarius document conditions)
	    (sagittarius document loader)
	    (sagittarius document tools)
	    (text sxml tools)
	    (peg)
	    (srfi :14 char-sets)
	    (srfi :127 lseqs))

(define-condition-type &document-input &document
  make-document-input-error document-input-error?)
(define (document-input-error who msg . irr)
  (if (null? irr)
      (raise (condition (make-document-input-error)
			(make-who-condition who)
			(make-message-condition msg)))
      (raise (condition (make-document-input-error)
			(make-who-condition who)
			(make-message-condition msg)
			(make-irritants-condition irr)))))

(define-record-type document-input-options
  (fields include-expander))
(define-syntax document-input-options-builder
  (make-record-builder document-input-options))

(define-record-type document-input
  (fields port proc filename))

(define (parse-document-input parser input . maybe-options)
  (define options (if (null? maybe-options) #f (car maybe-options)))
  (define (expand depth expander doc options)
    (define (call-include-expander include-expander source parser options)
      (include-expander depth
       (lambda (source options)
	 (let ((next (parser source options)))
	   (expand (+ depth 1) include-expander next options)))
       source options))
    (cond ((null? doc) '())
	  ((pair? doc)
	   (if (pair? (car doc))
	       (cons (expand depth expander (car doc) options)
		     (cdr (expand depth expander doc options)))
	       (let ((content (sxml:content doc)))
		 (or (and-let* (( (eq? (sxml:name doc) 'include) )
				( (not (null? content)) )
				( (null? (cdr content)) )
				(link (car content))
				(source (sxml:attr link 'source))
				(format (sxml:attr link 'format))
				(new-parser
				 (load-reader-procedure
				  (string->symbol format)))
				(expanded (call-include-expander
					   expander source new-parser options)))
		       (cond ((document:content expanded) =>
			      (lambda (c)
				(let ((expand? (sxml:attr doc 'expand)))
				  (if (and expand? (string=? expand? "true"))
				      `(included
					(expansion (@ (source ,source)
						      (title
						       ,(sxml:attr doc 'title)))
					 ,@(sxml:content c)))
				      `(included (@ (source ,source))
						 ,@(sxml:content c))))))
			     (else #f)))
		     (let ((c (map (lambda (c)
				     (expand depth expander c options))
				   content)))
		       (sxml:change-content doc c))))))
	  (else doc)))
  
  (let ((r (apply parser input maybe-options)))
    (cond ((and options (document-input-options-include-expander options)) =>
	   (lambda (expander)
	     (document:adjust-sections (expand 0 expander r options))))
	  (else r))))


(define (document-input->lseq document-input lexer)
  ((document-input-proc document-input) lexer))

(define file->document-input
  (case-lambda
   ((file) (file->document-input file (native-transcoder)))
   ((file transcoder)
    (let ((in (open-file-input-port file #f (buffer-mode block) transcoder)))
      (port->document-input in file #t)))))

(define port->document-input
  (case-lambda
   ((port) (port->document-input port (or (port-filename port) "<unknown>")))
   ((port filename) (port->document-input port filename #f))
   ((port filename owner?)
    (make-document-input
     port
     (lambda (lexer)
       (let ((column 0)
	     (line 1))
	 (define (return-value c) (list c (cons line column) filename))
	 (generator->lseq
	  (lambda ()
	    (let-values (((c nc nl) (lexer port)))
	      (cond ((eof-object? c) (when owner? (close-port port)) c)
		    (else
		     (let ((r (return-value c)))
		       (cond (nl (set! line (+ line nl)) (set! column 0))
			     (else (set! column (+ column nc))))
		       r))))))))
     filename))))

(define (document:simple-lexer port)
  (let ((c (get-char port)))
    (cond ((eof-object? c) (values c 0 #f))
	  ((eqv? c #\newline) (values c 0 1))
	  (else (values c 1 #f)))))

(define ($location l)
  (if (null? l)
      (values +parse-expect+ "Unexpected EOF" l)
      (let* ((v (lseq-car l))
	     (loc (cdr v)))
	(values +parse-success+ `((file ,(cadr loc))
				  (line ,(caar loc))
				  (column ,(cdar loc)))
		l))))
(define ($location-file l)
  (if (null? l)
      (values +parse-expect+ "Unexpected EOF" l)
      (let* ((v (lseq-car l))
	     (loc (cdr v)))
	(values +parse-success+ (cadr loc) l))))

(define (input-char l) (car (lseq-car l)))
(define (input-loc l) (cadr (lseq-car l)))
(define (input-file l) (caddr (lseq-car l)))

(define ($input-eqv? v) ($satisfy (lambda (c) (eqv? (car c) v)) v))
(define ($input-pred pred) ($satisfy (lambda (c) (pred (car c)))))
(define ($input-char-set-contains set)
  ($input-pred (lambda (c) (char-set-contains? set c))))
(define ($input-token token)
  (let ((c* (map $input-eqv? (string->list token))))
    ($seq (apply $seq c*) ($return token))))

(define ($input-token-ci token)
  (let ((c* (map (lambda (t) ($input-pred (lambda (c) (char-ci=? c t))))
		 (string->list token))))
    ;; TODO we need to return the actual value
    ($seq (apply $seq c*) ($return token))))

)
