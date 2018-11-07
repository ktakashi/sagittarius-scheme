;;; -*- Scheme -*-
;;;
;;; rfc/%3a5322.scm - RFC5322 Internet Message Format
;;;  
;;;   Copyright (c) 2009-2016  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/regex
(library (rfc :5322)
    (export &rfc5322-parse-error rfc5322-parse-error?
	    rfc5322-read-headers
	    rfc5322-next-token
	    rfc5322-header-ref
	    rfc5322-field->tokens
	    rfc5322-quoted-string
	    rfc5322-write-headers
	    rfc5322-parse-date
	    rfc5322-invalid-header-field
	    ;; misc
	    rfc5322-line-reader
	    rfc5322-dot-atom
	    ;; charsets
	    *rfc5322-atext-chars*
	    date->rfc5322-date
	    )
    (import (except (rnrs) get-line)
	    (rnrs r5rs)
	    (sagittarius)
	    (sagittarius io)
	    (sagittarius regex)
	    (sagittarius control)
	    (text parse)
	    (shorten)
	    (match)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :26 cut)
	    (prefix (binary io) binary:)
	    (util bytevector))

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

  (define rfc5322-char-set
    (char-set-difference char-set:printing (string->char-set ":")))

  (define (rfc5322-line-reader port)
    (define (get-line port)
      (define (strip-carrige bv)
	(define len (bytevector-length bv))
	(cond ((zero? len) bv)
	      ((= #x0d (bytevector-u8-ref bv (- len 1)))
	       (bytevector-copy bv 0 (- len 1)))
	      (else bv)))
      (if (textual-port? port)
	  (read-line port) ;; this can handle all known eol
	  (let ((bv (binary:get-line port)))
	    (if (eof-object? bv)
		bv
		(utf8->string (strip-carrige bv))))))
    (get-line port))

  (define (rfc5322-read-headers in :optional (strict? #f)
				     (reader (cut rfc5322-line-reader <>)))
    (define (accum name bodies r)
      ;; simple optimisation, check length (20% performance improvement)
      (if (null? (cdr bodies))
	  (cons (list name (car bodies)) r)
	  (cons (list name (string-concatenate-reverse bodies)) r)))
    
    ;; Couple of simplified SRFI-13 procedures for performance.
    ;; (50% performace improvement in total)
    (define (string-every char-set s)
      (define len (string-length s))
      (let loop ((i 0))
	(or (= i 0)
	    (and (char-set-contains? char-set (string-ref s i))
		 (loop (+ i 1))))))
    (define (string-trim-both str)
      (define len (string-length str))
      (define (find-start s)
	(let loop ((i 0))
	  (cond ((= i len) i) ;; all white space
		((char-whitespace? (string-ref s i)) (loop (+ i 1)))
		(else i))))
      (define (find-end s)
	(let loop ((i (- len 1)))
	  (cond ((< i 0) -1) ;; all white space (never reach here)
		((char-whitespace? (string-ref s i)) (loop (- i 1)))
		(else (+ i 1)))))
      (let* ((start (find-start str))
	     (end   (if (= start len) len (find-end str))))
	(if (and (zero? start) (= end len)) 
	    str
	    (substring str start end))))
    (define (string-trim str)
      (define len (string-length str))
      (define (find-start s)
	(let loop ((i 0))
	  (cond ((= i len) i) ;; all white space
		((char-whitespace? (string-ref s i)) (loop (+ i 1)))
		(else i))))
      (let* ((start (find-start str)))
	(if (zero? start)
	    str
	    (substring str start len))))

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
    ;; passing headers may not only be read by the reader
    ;; so we need to compare with string-ci=?
    (cond ((assoc field-name header string-ci=?) => cadr)
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
    (string->char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&'*+/=?^_`{|}~-0123456789"))
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

  ;; write
  (define (rfc5322-write-headers headers :key
				 (output (current-output-port))
				 (check :error)
				 (continue #f))
    (define (process headers)
      (dolist (field headers)
	(display (car field) output)
	(display ": " output)
	(display (cadr field) output)
	(unless (string-suffix? "\r\n" (cadr field))
	  (display "\r\n" output)))
      (unless continue (display "\r\n" output)))

    (define (bad name body reason)
      (assertion-violation 'rfc5322-write-headers
			   (format "Illegal RFC5322 header field data (~a)"
				   reason)
			   (format " ~a: ~,,,,80:a" name body)))
    (if (memv check '(#f :ignore))
	(process headers)
	(let loop ((hs headers)
		   (hs2 '()))
	  (match hs
	    (() (process (reverse hs2)))
	    (((name body) . rest)
	     (cond ((rfc5322-invalid-header-field
		     (string-append name ": " body))
		    => (lambda (reason)
			 (if (eq? check :error)
			     (bad name body reason)
			     (receive (name2 body2) (check name body reason)
			       (if (and (equal? name name2) (equal? body body2))
				   (bad name body reason)
				   (loop `((,name2 ,body2) . ,rest) hs2))))))
		   (else (loop rest `((,name ,body) . ,hs2)))))
	    (else
	     (assertion-violation 'rfc5322-write-headers
				  "Invalid header data" headers))))))

  (define *crlf* (string->char-set "\r\n"))
  (define *invalid-char* (char-set-union
			  (char-set-difference char-set:full char-set:ascii)
			  (string->char-set "\x0;")))
  (define (rfc5322-invalid-header-field body)
    (if (string-index body *invalid-char*)
	'bad-character
	(let1 lines (string-split body "\r\n ")
	  (cond ((exists (lambda (s) (> (string-length s) 998)) lines)
		 'line-too-long)
		((exists (lambda (s) (string-index s *crlf*)) lines)
		 'stray-crlf)
		(else #f)))))

  ;; date section 3.3
  (define (rfc5322-parse-date s)
    (define (dow->number dow)
      (list-index (cut string=? <> dow)
		  '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))
    (define (month->number mon)
      (+ 1 (list-index (cut string=? <> mon)
		       '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
			 "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))))
    (define (year->number year)
      (let1 y (string->number year)
	(and y
	     (cond ((< y 50)  (+ y 2000))
		   ((< y 100) (+ y 1900))
		   (else y)))))
    (define (tz->number tz)
      (cond ((equal? tz "-0000") #f)
	    ((string->number tz))
	    ((assoc tz '(("UT" . 0) ("GMT" . 0) ("EDT" . -400) ("EST" . -500)
			 ("CDT" . -500) ("CST" . -600) ("MDT" . -600)
			 ("MST" . -700) ("PDT" . -700) ("PST" . -800)))
	     => cdr)
	    (else #f)))
    (cond ((#/
	    # [day-of-week ","]
	    (?:(Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s*,)?\s*
	    # date = day month year 
	    # day = {[FWS] 1*2DIGIT FWS} \/ obs-day
	    (\d+)\s*
	    # month
	    (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s* # for emacs|)
	    # year = {FWS 4*DIGIT FWS} \/ obs-year
	    (\d{2}(?:\d{2})?)\s+
	    # time = time-of-day zone
	    (?# time-of-day = hour ":" minute [":" second])
	    (\d{2})\s*:\s*(\d{2})(?:\s*:\s*(\d{2}))?
	    # zone = {FWS ["+" \/ "-"] 4DIGIT} \/ obs-zone
	    (?:\s+([+-]\d{4}|[A-Z][A-Z][A-Z]?))? # we ignore military zone|))
	    /x s)
	   => (^m (values (year->number (m 4))    ; year
			  (month->number (m 3))	  ; month
			  (string->number (m 2))  ; dom
			  (string->number (m 5))  ; hour
			  (string->number (m 6))  ; min
			  (cond ((m 7) => string->number) (else #f)) ; sec
			  (cond ((m 8) => tz->number) (else #f)) ; tz
			  (cond ((m 1) => dow->number) (else #f)) ; dow
			  )))
	  (else (values #f #f #f #f #f #f #f #f))))

  (define (date->rfc5322-date date)
    ;; we can not use this because of ~z formatter...
    ;;(date->string date "~a, ~e ~b ~Y ~3 ~z")
    (let1 tz (date-zone-offset date)
      (format "~a, ~2d ~a ~4d ~2,'0d:~2,'0d:~2,'0d ~a~2,'0d~2,'0d"
	      (vector-ref '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
			  (date-week-day date))
	      (date-day date)
	      (vector-ref '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
			     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
			  (date-month date))
	      (date-year date)
	      (date-hour date) (date-minute date) (date-second date)
	      (if (>= tz 0) "+" "-")
	      (quotient (abs tz) 3600)
	      (modulo (quotient (abs tz) 60) 60)))
    )
)
