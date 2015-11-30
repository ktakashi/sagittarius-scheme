;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sql/scanner.scm - SQL scanner
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=char-set
(library (text sql scanner)
    (export make-sql-scanner)
    (import (rnrs) 
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (packrat)
	    (sagittarius))

(define-record-type (<scanner-context> make-scanner-context scanner-context?)
  (fields (immutable input-port scanner-input)
	  (immutable buffer     scanner-unget-buffer)
	  (mutable   position   scanner-position scanner-position-set!))
  (protocol (lambda (p)
	      (lambda (in)
		(p in 
		   #f ;; TODO list queue
		   (top-parse-position (cond ((car (port-info in)))
					     (else "<?>"))))))))

(define (scanner-get-char ctx) (get-char (scanner-input ctx)))
(define (scanner-peek-char ctx) (lookahead-char (scanner-input ctx)))
;; TODO implement it
(define (scanner-unget-char ctx ch) #f)
(define (update-scanner-position! ctx)
  (let ((pos (scanner-position ctx)))
    (scanner-position-set! ctx (update-parse-position pos #\newline))))

(define (make-sql-scanner p)
  (let ((eof #f)
	(ctx (make-scanner-context p)))
    (lambda ()
      (if eof
	  (values (scanner-position ctx) #f)
	  (let ((c (scanner-get-char ctx)))
	    (if (eof-object? c)
		(begin
		  (set! eof #t)
		  (values (scanner-position ctx) #f))
		;; TODO this doesn't reflect position of after comment
		(let* ((old-pos (scanner-position ctx))
		       (token (scanner-dispatch c ctx)))
		  (values old-pos token))))))))

(define specials #[\"%&'*+,-:\;<=>?/^.()\[\]_\|{}])

(define (read-comment port)
  (let loop ((ch (scanner-get-char port)))
    (cond ((eof-object? ch) (error 'sql-scanner "unexpected EOF"))
	  ((char=? #\* ch)
	   (let ((nc (scanner-get-char port)))
	     (case nc
	       ((#\/) (scanner-dispatch (scanner-get-char port) port))
	       (else (loop nc)))))
	  ((char=? #\newline ch)
	   (update-scanner-position! port)
	   (loop (scanner-get-char port)))
	  (else (loop (scanner-get-char port))))))

;; b'' | B''
(define (read-bit-string port) (error 'read-bit-string "not yet"))

;; n'' | N''
(define (read-national-character port) 
  (error 'read-national-character "not yet"))

;; normal identifier
(define (read-identifier ch port) (error 'read-identifier "not yet"))

;; unicode escape can be determined by UESCAPE clause which is after
;; the string. Thus we can't construct unicode character here since
;; we don't know which character would be unicode escape character
;; so if unicode? is #t then we just return (unicode "string")
;; and let parser handle

;; delimited identifier
(define (read-delimited-identifier port unicode?)
  ;; "" is escape
  (let-values (((out extract) (open-string-output-port)))
    (let loop ((ch (scanner-get-char port)))
      (when (eof-object? ch)
	(error 'sql-scanner
	       "unexpected EOF during reading delimited identifier"))
      (case ch
	((#\")
	 (let ((nc (scanner-peek-char port)))
	   (case nc
	     ((#\") 
	      (put-char out (scanner-get-char port))
	      (loop (scanner-get-char port)))
	     ;; end
	     (else (let ((r (extract))) (if unicode? (list 'unicode r) r))))))
	(else (put-char out ch) (loop (scanner-get-char port)))))))

(define (read-string port unicode?)
  (let-values (((out extract) (open-string-output-port)))
    (let loop ((ch (scanner-get-char port)))
      (when (eof-object? ch)
	(error 'sql-scanner "unexpected EOF during reading string"))
      (case ch
	((#\')
	 (let ((nc (scanner-peek-char port)))
	   (case nc
	     ((#\') 
	      (put-char out (scanner-get-char port))
	      (loop (scanner-get-char port)))
	     ;; end
	     (else (let ((r (extract))) (if unicode? (list 'unicode r) r))))))
	(else (put-char out ch) (loop (scanner-get-char port)))))))

(define (scanner-dispatch ch port)
  ;; TODO maybe we should make ASCII table to dispatch
  ;;      the process for performance.
  (define (next ch port)
    (cond ((eof-object? ch) #f)
	  ;; handle comment first
	  ;; NB: / and - are self standing chars
	  ((char=? #\/ ch)
	   (case (scanner-peek-char port)
	     ((#\*) (scanner-get-char port) (read-comment port))
	     (else (cons ch ch))))
	  ((char=? #\- ch)
	   (case (scanner-peek-char port)
	     ((#\-) 
	      (get-line (scanner-input port))
	      (update-scanner-position! port)
	      (scanner-dispatch (scanner-get-char port) port))
	     (else (cons ch ch))))
	  ;; bit string?
	  ((char-ci=? #\b ch)
	   (case (scanner-peek-char port)
	     ((#\') (scanner-get-char port) (read-bit-string port))
	     (else (read-identifier ch port))))
	  ;; National character
	  ((char-ci=? #\n ch)
	   (case (scanner-peek-char port)
	     ((#\') (scanner-get-char port) (read-national-character port))
	     (else (read-identifier ch port))))
	  ;; quote
	  ((char=? #\' ch) (read-string port #f))
	  ((char=? #\" ch) (read-delimited-identifier port #f))
	  ;; unicode escape
	  ((char-ci=? #\u ch)
	   (case (scanner-peek-char port)
	     ((#\&) (scanner-get-char port)
	      (let ((nc (scanner-get-char port)))
		(case nc
		  ((#\') (read-string port #t))
		  ((#\") (read-delimited-identifier port #t))
		  (else 
		   (scanner-unget-char port nc)
		   (error 'sql-scanner "invalid unicode escpape")))))
	     (else (read-identifier ch port))))
	  ;; <delimiter token>
	  ;; returns token value as string
	  ;; token kind either symbol (more than one letter) or character.
	  ((char=? #\< ch)
	   (case (scanner-peek-char port)
	     ((#\=) (scanner-get-char port) (cons '<= "<="))
	     ((#\>) (scanner-get-char port) (cons '<> "<>"))
	     (else  (cons ch "<"))))
	  ((char=? #\> ch)
	   (case (scanner-peek-char port)
	     ((#\=) (scanner-get-char port) (cons '>= ">="))
	     (else  (cons ch ">"))))
	  ((char=? #\: ch)
	   ;; TODO should we make sure :: is symbols?
	   (case (scanner-peek-char port)
	     ((#\:) (scanner-get-char port) (cons ':: "::"))
	     (else  (cons ch ":"))))
	  ((char=? #\. ch)
	   (case (scanner-peek-char port)
	     ((#\.) (scanner-get-char port) (cons '.. ".."))
	     (else  (cons ch (string ch)))))
	  ((char=? #\- ch)
	   (case (scanner-peek-char port)
	     ((#\>) (scanner-get-char port) (cons '-> "->"))
	     (else  (cons ch "-"))))
	  ((char=? #\| ch)
	   (case (scanner-peek-char port)
	     ((#\|) (scanner-get-char port) (cons 'concat "||"))
	     (else  (cons ch ch))))
	  ((char-set-contains? specials ch) (string ch))
	  
	  ))
  ;; skip continuous white spaces.
  (define (skip-whitespace ch port)
    (let loop ((ch ch))
      (if (and (char? ch) (char-whitespace? ch))
	  (case ch 
	    ((#\newline) 
	     (update-scanner-position! port)
	     (loop (scanner-get-char port)))
	    (else (loop (scanner-get-char port))))
	  ch)))
  (let ((ch (skip-whitespace ch port)))
    (next ch port)))
	
)
