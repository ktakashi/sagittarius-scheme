;;; -*- Scheme -*-
;;;
;;; 5322.scm - RFC5322 Internet Message Format
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; Main reference:
;; RFC5322 URI Generic Syntax
;; <http://www.ietf.org/rfc/rfc5322.txt>

(library (rfc :5322)
    (export &rfc5322-parse-error
	    rfc5322-read-headers
	    rfc5322-next-token
	    rfc5322-header-ref
	    rfc5322-field->tokens
	    rfc5322-quoted-string
	    ;; misc
	    rfc5322-line-reader)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius io)
	    (sagittarius regex)
	    (sagittarius control)
	    (text parse)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-set)
	    (srfi :26 cut))

  (define-condition-type &rfc5322-parse-error &assertion
    make-rfc5322-parse-error
    rfc5322-parse-error?
    (name header-field-name)
    (body header-field-body))

  (define (rfc5322-parse-error who msg name body . irritants)
    (raise
     (apply condition
	    (filter values
		    (list (make-rfc5322-parse-error name body)
			  (and who (make-who-condition who))
			  (make-message-condition msg)
			  (make-irritants-condition irritants))))))

  (define wsp '(#\space #\tab))

  (define rfc5322-char-set (char-set-difference char-set:printing (string->char-set ":")))

  (define (rfc5322-line-reader port)
    (let1 r (get-line port)
      (if (eof-object? r)
	  r
	  (let1 len (string-length r)
	    (cond ((zero? len) r)
		  ((char=? #\x0d (string-ref r (- len 1))) ;; check CR
		   ;; TODO memory waste
		   (substring r 0 (- len 1)))
		  (else r))))))
      

  (define-optional (rfc5322-read-headers in (optional (strict? #f)
						      (reader (cut rfc5322-line-reader <>))))
    (define (accum name bodies r)
      (cons (list name (string-concatenate-reverse bodies)) r))
    (define drop-leading-fws string-trim)

    (let loop ((r '())
	       (line (reader in)))
      (cond ((eof-object? line) (reverse! r))
	    ((string-null? line) (reverse! r))
	    (else
	     (receive (n body) (string-scan line ":" 'both)
	       (let ((name (and-let* (( (string? n) )
				      (name (string-trim-both n))
				      ( (string-every rfc5322-char-set name) ))
			     (string-downcase name))))
		 (cond (name
			(let loop2 ((nline (reader in))
				    (bodies (list (drop-leading-fws body))))
			  (cond ((eof-object? nline)
				 ;; maybe premature end of the message
				 (if strict?
				     (rfc5322-parse-error 'rfc5322-read-headers
							  "premature end of message header"
							  #f #f)
				     (reverse! (accum name bodies r))))
				((string-null? nline)
				 (reverse! (accum name bodies r)))
				;; treats folding.
				((memv (string-ref nline 0) wsp)
				 (loop2 (reader in) (cons nline bodies)))
				(else
				 (loop (accum name bodies r) nline)))))
		       (strict?
			(rfc5322-parse-error 'rfc5322-read-headers
					     (format "bad header line: ~s" line)
					     #f #f))
		       (else (loop r (reader in))))))))))

  (define (rfc5322-header-ref header field-name . maybe-default)
    (cond ((assoc field-name header) => cadr)
	  (else (get-optional maybe-default #f))))

  (define (rfc5322-field->tokens field . opts)
    (call-with-input-string field
      (lambda (port)
	(let ((proc (cut apply rfc5322-next-token <> opts)))
	  (let loop ((l (proc port))
		     (r '()))
	    (if (eof-object? l)
		(reverse! r)
		(loop (proc port) (cons l r))))))))

  (define *rfc5322-atext-chars*
    (string->char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&'*+/=?^_`{|}~-"))
  (define (rfc5322-dot-atom input)
    (next-token-of `(,*rfc5322-atext-chars* #\.) input))
  (define *quote-set* (string->char-set "\""))

  (define (rfc5322-quoted-string input)
    (call-with-string-output-port
     (lambda (port)
       (let loop ((c (peek-next-char input)))
	 (cond ((eof-object? c) #t);; tolerate missing closing DQUOTE
	       ((char=? c #\") (get-char input) #t) ;; discard DQUOTE
	       ((char=? c #\\)
		(let ((c (peek-next-char input)))
		  (cond ((eof-object? c) #t) ;; tolerate stray backslash
			(else (put-char port c) (loop (peek-next-char input))))))
	       (else (put-char port c) (loop (peek-next-char input))))))))

  (define *rfc5322-standard-tokenizers*
    `((,*quote-set* . ,rfc5322-quoted-string)
      (,*rfc5322-atext-chars* . ,rfc5322-dot-atom)))
  
  (define (rfc5322-next-token input . opts)
    (let ((tokenab (map (lambda (e)
			  (cond ((char-set? e) (cons e (cut next-token-of e <>)))
				(else e)))
			(get-optional opts *rfc5322-standard-tokenizers*)))
	  (c (rfc5322-skip-cfws input)))
      (cond ((eof-object? c) c)
	    ((find (lambda (e) (char-set-contains? (car e) c)) tokenab)
	     => (lambda (e) ((cdr e) input)))
	    (else (get-char input)))))

  (define (rfc5322-skip-cfws input)
    (define (scan c)
      (cond ((eof-object? c) c)
	    ((char=? c #\( ) (in-comment (peek-next-char input)))
	    ((char-whitespace? c) (scan (peek-next-char input)))
	    (else c)))
    (define (in-comment c)
      (cond ((eof-object? c) c)
	    ((char=? c #\) ) (scan (peek-next-char input)))
	    ((char=? c #\\ ) (read-char input) (in-comment (peek-next-char input)))
	    ((char=? c #\( ) (in-comment (in-comment (peek-next-char input))))
	    (else (in-comment (peek-next-char input)))))
    (scan (peek-char input)))  
)
