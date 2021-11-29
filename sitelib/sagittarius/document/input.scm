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

	    $location
	    document:simple-lexer ;; for convenence
	    
	    document-input-error
	    &document-input make-document-input-error document-input-error?
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius document conditions)
	    (peg)
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
		      r)))))))))))

(define (document:simple-lexer port)
  (let ((c (get-char port)))
    (cond ((eof-object? c) (values c 0 #f))
	  ((eqv? c #\newline) (values c 0 1))
	  (else (values c 1 #f)))))

(define ($location l)
  (let* ((v (lseq-car l))
	 (loc (cdr v)))
    (values +parse-success+ `((file ,(cadr loc))
			      (line ,(caar loc))
			      (column ,(cdar loc)))
	    l)))

)
