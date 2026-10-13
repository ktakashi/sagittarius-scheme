;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/format.scm - JSON schema format validator
;;;
;;;   Copyright (c) 2023  Takashi Kato  <ktakashi@ymail.com>
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

;; reference:
;; Draft-7, 2019-09 and 2020-12: https://json-schema.org/
#!nounbound
(library (text json schema validators format)
    (export json-schema:format
	    *json-schema:validate-format?*)
    (import (rnrs)
	    (sagittarius regex)
	    (sagittarius generators)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (srfi :115 regexp)
	    (srfi :127 lseqs)
	    (peg)
	    (peg chars)
	    (rfc smtp format)
	    (rfc timestamps)
	    (rfc uri parser)
	    (rfc uri-template)
	    (rfc uuid)
	    (text json pointer)
	    (only (text json schema validators primitives)
		  json-schema:pattern)
	    (text xml schema parsers))

(define *json-schema:validate-format?* (make-parameter #t))

(define (json-schema:format v)
  (cond ((assoc v +json-schema-defined-formats+) =>
	 (lambda (slot)
	   (let ((validator (cdr slot)))
	     (lambda (e)
	       (if (*json-schema:validate-format?*)
		   (or (not (string? e)) (validator e))
		   #t)))))
	;; not supported, so ignore
	(else (lambda (e) #t))))

(define (parser->validator p)
  (lambda (e)
    (or (not (string? e))
	(date? (p e)))))

(define json-schema:format-date (parser->validator parse-date))
(define json-schema:format-time (parser->validator parse-time))
(define json-schema:format-date-time (parser->validator parse-date-time))

(define (ascii-string? s)
  (string-every (lambda (c) (char-set-contains? char-set:ascii c)) s))
(define (ascii-digit? c)
  (and (char-set-contains? char-set:ascii c)
       (char-set-contains? char-set:digit c)))
(define (json-schema:format-email e)
  (and (ascii-string? e) (smtp-valid-address? e)))
(define (json-schema:format-idn-email e) (smtp-valid-address? e))

;; hostname
;; label must be max 63
;; see https://datatracker.ietf.org/doc/html/rfc1034#section-3.1
(define (ascii-ldh-char? c)
  (and (char-set-contains? char-set:ascii c)
       (or (char-alphabetic? c) (char-numeric? c) (char=? c #\-))))
(define (ascii-alnum? c)
  (and (char-set-contains? char-set:ascii c)
       (or (char-alphabetic? c) (char-numeric? c))))

(define (ascii-label? s)
  (let ((n (string-length s)))
    (and (<= 1 n 63)
	 (ascii-alnum? (string-ref s 0))
	 (ascii-alnum? (string-ref s (- n 1)))
	 (string-every ascii-ldh-char? s)
	 (or (< n 4)
	     (not (and (char=? (string-ref s 2) #\-)
		       (char=? (string-ref s 3) #\-)))))))

(define +punycode-base+ 36)
(define +punycode-t-min+ 1)
(define +punycode-t-max+ 26)
(define +punycode-skew+ 38)
(define +punycode-damp+ 700)
(define +punycode-initial-bias+ 72)
(define +punycode-initial-n+ 128)

(define (punycode-digit c)
  (cond ((char<=? #\a c #\z) (+ 0 (- (char->integer c) (char->integer #\a))))
	((char<=? #\A c #\Z) (+ 0 (- (char->integer c) (char->integer #\A))))
	((char<=? #\0 c #\9) (+ 26 (- (char->integer c) (char->integer #\0))))
	(else #f)))

(define (punycode-adapt delta num-points first?)
  (let* ((delta (if first?
		    (div delta +punycode-damp+)
		    (div delta 2)))
	 (delta (+ delta (div delta num-points))))
    (let loop ((delta delta) (k 0))
      (if (> delta (div (* (- +punycode-base+ +punycode-t-min+)
			   +punycode-t-max+)
			2))
	  (loop (div delta (- +punycode-base+ +punycode-t-min+))
		(+ k +punycode-base+))
	  (+ k (div (* (+ (- +punycode-base+ +punycode-t-min+) 1) delta)
		     (+ delta +punycode-skew+)))))))

(define (insert-at lst i v)
  (if (zero? i)
      (cons v lst)
      (cons (car lst) (insert-at (cdr lst) (- i 1) v))))

(define (decode-punycode payload)
  (define n +punycode-initial-n+)
  (define i 0)
  (define bias +punycode-initial-bias+)
  (define delim (string-index-right payload #\-))
  (define basic (if delim (substring payload 0 delim) ""))
  (if (not (string-every (lambda (c) (and (ascii-ldh-char? c)
					  (not (char=? c #\-))))
			 basic))
      #f
      (let ((output (string->list basic))
	    (index (if delim (+ delim 1) 0)))
	(guard (e (else #f))
	  (let loop ((index index) (n n) (i i) (bias bias)
		     (output output) (first? #t))
	    (if (>= index (string-length payload))
		(list->string output)
		(let* ((old-i i))
		  (let digit-loop ((index index) (i i) (w 1)
				   (k +punycode-base+))
		    (if (>= index (string-length payload))
			#f
			(let ((digit (punycode-digit (string-ref payload index))))
			  (if (not digit)
			      #f
			      (let* ((i (+ i (* digit w)))
				     (t (cond ((<= k bias) +punycode-t-min+)
					      ((>= k (+ bias +punycode-t-max+))
					       +punycode-t-max+)
					      (else (- k bias)))))
				(if (< digit t)
				    (let* ((out-len (+ (length output) 1))
					   (bias (punycode-adapt (- i old-i)
								 out-len
								 first?))
					   (n (+ n (div i out-len)))
					   (i (mod i out-len))
					   (output (insert-at output i
							      (integer->char n))))
				      (loop (+ index 1) n (+ i 1) bias output #f))
				    (digit-loop (+ index 1)
						i
						(* w (- +punycode-base+ t))
						(+ k +punycode-base+)))))))))))))))

(define +idna-disallowed-exceptions+
  '(#x0640 #x07FA #x302E #x302F))

(define +virama-codepoints+
  '(#x094D #x09CD #x0A4D #x0ACD #x0B4D #x0BCD #x0C4D #x0CCD #x0D4D #x0DCA
    #x0E3A #x0F84 #x1039 #x103A #x1714 #x1734 #x17D2 #x1A60 #x1B44 #x1BAA
    #x1BAB #x1BF2 #x1BF3 #x2D7F #xA806 #xA8C4 #xA953 #xA9C0 #xAAF6 #xABED
    #x10A3F #x11046 #x110B9 #x11133 #x111C0 #x11235 #x112EA #x1134D #x11442
    #x114C2 #x115BF #x1163F #x116B6 #x1172B #x11839 #x1193D #x119E0 #x11A34
    #x11A47 #x11A99 #x11C3F #x11D44 #x11D45 #x11D97))

(define (greek-char? c)
  (let ((cp (char->integer c)))
    (or (<= #x0370 cp #x03FF)
	(<= #x1F00 cp #x1FFF))))
(define (hebrew-char? c)
  (<= #x0590 (char->integer c) #x05FF))
(define (hira-kata-han-char? c)
  (let ((cp (char->integer c)))
    (or (<= #x3040 cp #x309F)
	(<= #x30A0 cp #x30FF)
	(<= #x4E00 cp #x9FFF))))

(define (label-context-valid? label)
  (define chars (string->list label))
  (define n (length chars))
  (define (char-at i) (list-ref chars i))
  (and (or (= n 0)
	   (let ((gc (char-general-category (car chars))))
	     (not (memq gc '(Mn Mc Me)))))
       (let loop ((i 0))
	 (if (= i n)
	     #t
	     (let* ((c (char-at i))
		    (cp (char->integer c)))
	       (cond
		((memv cp +idna-disallowed-exceptions+) #f)
		((= cp #x00B7)
		 (and (> i 0) (< (+ i 1) n)
		      (char=? (char-at (- i 1)) #\l)
		      (char=? (char-at (+ i 1)) #\l)
		      (loop (+ i 1))))
		((= cp #x0375)
		 (and (< (+ i 1) n)
		      (greek-char? (char-at (+ i 1)))
		      (loop (+ i 1))))
		((or (= cp #x05F3) (= cp #x05F4))
		 (and (> i 0)
		      (hebrew-char? (char-at (- i 1)))
		      (loop (+ i 1))))
		((= cp #x30FB)
		 (and (exists (lambda (x)
				(and (not (= (char->integer x) #x30FB))
				     (hira-kata-han-char? x)))
			      chars)
		      (loop (+ i 1))))
		((= cp #x200D)
		 (and (> i 0)
		      (memv (char->integer (char-at (- i 1)))
			    +virama-codepoints+)
		      (loop (+ i 1))))
		(else (loop (+ i 1)))))))
       (let ((has-arabic-indic
	      (exists (lambda (c) (<= #x0660 (char->integer c) #x0669)) chars))
	     (has-ext-arabic-indic
	      (exists (lambda (c) (<= #x06F0 (char->integer c) #x06F9)) chars)))
	 (not (and has-arabic-indic has-ext-arabic-indic)))
       (or (< n 4)
	   (not (and (char=? (char-at 2) #\-)
		     (char=? (char-at 3) #\-))))))

(define (a-label? label)
  (and (string-prefix? "xn--" label)
       (> (string-length label) 4)
       (let ((decoded (decode-punycode (substring label 4 (string-length label)))))
	 (and decoded
	      (> (string-length (substring label 4 (string-length label))) 1)
	      (label-context-valid? decoded)))))

(define (split-on-char s sep)
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (r '()))
      (cond ((= i n) (reverse (cons (substring s start i) r)))
	    ((char=? (string-ref s i) sep)
	     (loop (+ i 1) (+ i 1) (cons (substring s start i) r)))
	    (else (loop (+ i 1) start r))))))
(define (split-labels s) (split-on-char s #\.))

(define (json-schema:format-hostname e)
  (and (<= (string-length e) 255)
       (not (string-null? e))
       (not (string-suffix? "." e))
       (ascii-string? e)
       (string-every (lambda (c) (not (char-whitespace? c))) e)
       (let ((labels (split-labels e)))
	 (and (pair? labels)
	      (for-all (lambda (label) (not (string-null? label))) labels)
	      (for-all (lambda (label)
			 (or (ascii-label? label)
			     (a-label? label)))
		       labels)))))
(define json-schema:format-idn-hostname
  (json-schema:pattern 
   "^(?u:([^\\.-]|[^\\.-][^\\.]*[^\\.\\-])\\.)*([^\\.\\-]|[^\\.\\-][^\\.]*[^\\.\\-])$"))

;; IP address
(define (json-schema:format-ipv4 e) (ipv4-address? e))
(define (compat-ipv6v4? e)
  (if (string-prefix? "::" e)
      (let* ((rest (substring e 2 (string-length e)))
	     (parts (split-on-char rest #\:)))
	(and (> (length parts) 1)
	     (let* ((last (car (reverse parts)))
		    (head (reverse (cdr (reverse parts)))))
	       (and (<= 5 (length head) 6)
		    (for-all (lambda (h)
			       (and (not (string-null? h))
				    (<= (string-length h) 4)
				    (string-every
				     (lambda (c)
				       (char-set-contains? char-set:hex-digit c))
				     h)))
			     head)
		    (ipv4-address? last)))))
      #f))
(define (json-schema:format-ipv6 e)
  (or (ipv6-address? e) (compat-ipv6v4? e)))

;; we don't check if the scheme requires authority or not
(define (valid-percent-encoding? s)
  (let loop ((i 0))
    (if (>= i (string-length s))
	#t
	(let ((c (string-ref s i)))
	  (if (char=? c #\%)
	      (and (< (+ i 2) (string-length s))
		   (char-set-contains? char-set:hex-digit (string-ref s (+ i 1)))
		   (char-set-contains? char-set:hex-digit (string-ref s (+ i 2)))
		   (loop (+ i 3)))
	      (loop (+ i 1)))))))
(define (parse-valid? parser e)
  (let-values (((s v nl) (parser (generator->lseq (string->generator e)))))
    (and (parse-success? s) (null? nl))))

(define (json-schema:format-uri e)
  (and (ascii-string? e)
       (valid-percent-encoding? e)
       (parse-valid? uri:uri-parser e)))
(define (json-schema:format-uri-reference e)
  (and (ascii-string? e)
       (valid-percent-encoding? e)
       (parse-valid? uri:uri-reference-parser e)))
(define (json-schema:format-iri e)
  (and (valid-percent-encoding? e)
       (parse-valid? uri:uri-parser e)))
(define (json-schema:format-iri-reference e)
  (and (valid-percent-encoding? e)
       (parse-valid? uri:uri-reference-parser e)))
(define (json-schema:format-uri-template e)
  (guard (e (else #f))
    (begin (parse-uri-template e) #t)))

(define (json-schema:format-json-pointer v)
  (guard (e (else #f)) (and (json-pointer v) #t)))

(define $non-negative-integer
  ($or ($eqv? #\0)
       ($seq ($char-set-contains? (ucs-range->char-set #x31 #x40))
	     ($many ($char-set-contains? (ucs-range->char-set #x30 #x40))))))
(define $index-manipulation
  ($seq ($or ($eqv? #\+) ($eqv? #\-)) $non-negative-integer))
(define $relative-json-pointer
  ($or ($seq $non-negative-integer ($eqv? #\#))
       ($seq $non-negative-integer
	     ($optional $index-manipulation)
	     $json-pointer)))

(define (json-schema:format-relative-json-pointer v)
  (let-values (((s v nl) ($relative-json-pointer (string->list v))))
    (and (parse-success? s) (null? nl))))
(define (contains-inline-flag-group? v)
  (let ((n (string-length v)))
    (let loop ((i 0))
      (cond ((>= (+ i 3) n) #f)
	    ((and (char=? (string-ref v i) #\()
		  (char=? (string-ref v (+ i 1)) #\?))
	     (let ((c (string-ref v (+ i 2))))
	       (if (char-alphabetic? c)
		   (let flag-loop ((j (+ i 3)))
		     (cond ((>= j n) #f)
			   ((char-alphabetic? (string-ref v j))
			    (flag-loop (+ j 1)))
			   ((char=? (string-ref v j) #\)) #t)
			   (else (loop (+ i 1)))))
		   (loop (+ i 1)))))
	    (else (loop (+ i 1)))))))

(define (trailing-backslash? v)
  (let ((n (string-length v)))
    (and (> n 0)
	 (let loop ((i (- n 1)) (c 0))
	   (if (and (>= i 0) (char=? (string-ref v i) #\\))
	       (loop (- i 1) (+ c 1))
	       (odd? c))))))

(define (invalid-range? v)
  (define n (string-length v))
  (let loop ((i 0) (in-class? #f) (escaped? #f) (prev #f))
    (cond ((>= i n) #f)
	  (escaped? (loop (+ i 1) in-class? #f (string-ref v i)))
	  ((char=? (string-ref v i) #\\) (loop (+ i 1) in-class? #t prev))
	  ((and (not in-class?) (char=? (string-ref v i) #\[))
	   (loop (+ i 1) #t #f #f))
	  ((and in-class? (char=? (string-ref v i) #\]))
	   (loop (+ i 1) #f #f #f))
	  ((and in-class? (char=? (string-ref v i) #\-)
		(> i 0) (< (+ i 1) n)
		prev
		(not (char=? (string-ref v (+ i 1)) #\])))
	   (let ((next (string-ref v (+ i 1))))
	     (if (> (char->integer prev) (char->integer next))
		 #t
		 (loop (+ i 1) #t #f #\-))))
	  (else (loop (+ i 1) in-class? #f (string-ref v i))))))

(define (json-schema:format-regex v)
  (cond ((or (string=? v "[]") (string=? v "[^]")) #t)
	((or (string-contains v "(?#")
	     (string-contains v "(?P<")
	     (string-contains v "(?P=")
	     (string-contains v "\\a")
	     (contains-inline-flag-group? v)
	     (trailing-backslash? v)
	     (invalid-range? v))
	 #f)
	(else
	 (guard (e (else #f)) (and (regex v 0 #t) #t)))))

(define (json-schema:format-uuid v)
  (guard (e (else #f)) (uuid? (string->uuid v))))

(define (parse-duration-components s tokens)
  (define (token-index t)
    (let loop ((tokens tokens) (i 0))
      (cond ((null? tokens) #f)
	    ((char=? t (car tokens)) i)
	    (else (loop (cdr tokens) (+ i 1))))))
  (let ((n (string-length s)))
    (let loop ((i 0) (pos -1) (seen '()))
      (cond ((>= i n) (values #t seen))
	    ((not (ascii-digit? (string-ref s i)))
	     (values #f seen))
	    (else
	     (let num-loop ((j i))
	       (if (and (< j n) (ascii-digit? (string-ref s j)))
		   (num-loop (+ j 1))
		   (if (>= j n)
		       (values #f seen)
		       (let ((t (string-ref s j)))
			 (cond ((memv t tokens)
				(let ((idx (token-index t)))
				  (if (or (<= idx pos) (assv t seen))
				      (values #f seen)
				      (loop (+ j 1) idx (cons (cons t #t) seen)))))
			       (else (values #f seen))))))))))))

(define (json-schema:format-duration v)
  (guard (e (else #f))
    (let-values (((m s) (xs:parse-duration v *rfc:duration-parser*)))
      #t)))

(define +json-schema-defined-formats+
  `(
    ("date" . ,json-schema:format-date)
    ("time" . ,json-schema:format-time)
    ("date-time" . ,json-schema:format-date-time)
    ("email" . ,json-schema:format-email)
    ("idn-email" . ,json-schema:format-idn-email)
    ("hostname" . ,json-schema:format-hostname)
    ("idn-hostname" . ,json-schema:format-idn-hostname)
    ("ipv4" . ,json-schema:format-ipv4)
    ("ipv6" . ,json-schema:format-ipv6)
    ("uri" . ,json-schema:format-uri)
    ("uri-reference" . ,json-schema:format-uri-reference)
    ("iri" . ,json-schema:format-iri)
    ("iri-reference" . ,json-schema:format-iri-reference)
    ("uri-template" . ,json-schema:format-uri-template)
    ("json-pointer" . ,json-schema:format-json-pointer)
    ("relative-json-pointer" . ,json-schema:format-relative-json-pointer)
    ("regex" . ,json-schema:format-regex)
    ;; from draft 2019-09, but make it common
    ("uuid" . ,json-schema:format-uuid)
    ("duration" . ,json-schema:format-duration)
    ))

)
