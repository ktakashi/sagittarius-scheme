;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/cookie.scm - HTTP State Management Mechanism
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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


;; reference
;;  RFC 6265: https://tools.ietf.org/html/rfc6265
;; NOTE:
;;  RFC 2965 (https://tools.ietf.org/html/rfc2965), aka Cookie2, is not widely
;;  and obsolated by RFC 6265 so we don't support it for now.
;;  It seems not so difficult to support once we implement RFC 6265 properly,
;;  so we might support it if there's enough demand.

#!read-macro=sagittarius/regex
(library (rfc cookie)
    (export <cookie> make-cookie cookie?
	    parse-cookie
	    parse-cookie-string
	    parse-cookies
	    parse-cookies-string
	    cookie->string
	    cookies->string
	    ;; accessors
	    cookie-name
	    cookie-value cookie-value-set!
	    cookie-expires cookie-expires-set!
	    cookie-max-age cookie-max-age-set!
	    cookie-domain cookie-domain-set!
	    cookie-path cookie-path-set!
	    cookie-secure? cookie-secure-set!
	    cookie-http-only? cookie-http-only-set!
	    cookie-extension cookie-extension-add!

	    (rename (cookie-jar <cookie-jar>))
	    make-cookie-jar cookie-jar?
	    cookie-jar-size
	    cookie-jar-add-cookie!
	    cookie-jar-delete-cookie!
	    cookie-jar->cookies
	    )
    (import (rnrs)
	    (sagittarius) ;; for format
	    (sagittarius regex)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :113 sets)
	    (srfi :128 comparators)
	    (text parse)
	    (util hashtables)
	    (clos user))

(define-class <cookie> ()
  (;; required, we don't  allow users to change name. or should we?
   (name :init-keyword :name :reader cookie-name)
   ;; required
   (value :init-keyword :value :reader cookie-value :writer cookie-value-set!)
   ;; SRFI-19 time object
   (expires :init-keyword :expires :init-value #f
	    :reader cookie-expires :writer cookie-expires-set!)
   (max-age :init-keyword :max-age :init-value #f
	    :reader cookie-max-age :writer cookie-max-age-set!)
   (domain :init-keyword :domain :init-value #f
	   :reader cookie-domain :writer cookie-domain-set!)
   (path :init-keyword :path :init-value #f
	 :reader cookie-path :writer cookie-path-set!)
   (secure :init-keyword :secure :init-value #f
	   :reader cookie-secure? :writer cookie-secure-set!)
   (http-only :init-keyword :http-only :init-value #f
	      :reader cookie-http-only? :writer cookie-http-only-set!)
   ;; TODO can extension-av appear multiple times?
   ;; (I think so)
   (extension :init-keyword :extension
	      :init-form (make-hashtable string-ci-hash string-ci=?)
	      :reader cookie-extension)))

(define-method write-object ((o <cookie>) out)
  (format out "#<cookie ~a:~a expires=~a domain=~a max-age=~a path=~a~a~a>"
	  (cookie-name o)
	  (cookie-value o)
	  (cookie-expires o)
	  (cookie-domain o)
	  (cookie-max-age o)
	  (cookie-path o)
	  (if (cookie-secure? o) " secure" "")
	  (if (cookie-http-only? o) " httpOnly" "")))

(define (make-cookie name value . opt)
  (apply make <cookie> :name name :value value opt))
(define (cookie? o) (is-a? o <cookie>))

(define (cookie-extension-add! cookie name value)
  (let ((ht (cookie-extension cookie)))
    (hashtable-set! ht name value)))

#|
  The following BNF is the syntax of cookie string.

  set-cookie-header = "Set-Cookie:" SP set-cookie-string
  set-cookie-string = cookie-pair *( ";" SP cookie-av )
  cookie-pair       = cookie-name "=" cookie-value
  cookie-name       = token
  cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
  cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
                       ; US-ASCII characters excluding CTLs,
                       ; whitespace DQUOTE, comma, semicolon,
                       ; and backslash
  token             = <token, defined in [RFC2616], Section 2.2>
  
  cookie-av         = expires-av / max-age-av / domain-av /
                      path-av / secure-av / httponly-av /
                      extension-av
  expires-av        = "Expires=" sane-cookie-date
  sane-cookie-date  = <rfc1123-date, defined in [RFC2616], Section 3.3.1>
  max-age-av        = "Max-Age=" non-zero-digit *DIGIT
                       ; In practice, both expires-av and max-age-av
                       ; are limited to dates representable by the
                       ; user agent.
  non-zero-digit    = %x31-39
                       ; digits 1 through 9
  domain-av         = "Domain=" domain-value
  domain-value      = <subdomain>
                       ; defined in [RFC1034], Section 3.5, as
                       ; enhanced by [RFC1123], Section 2.1
  path-av           = "Path=" path-value
  path-value        = <any CHAR except CTLs or ";">
  secure-av         = "Secure"
  httponly-av       = "HttpOnly"
  extension-av      = <any CHAR except CTLs or ";">

  the parse-cookie-string parses 'set-cookie-string' part. the above is
  the string from server the following is the user agent string:

  cookie-header = "Cookie:" OWS cookie-string OWS
  cookie-string = cookie-pair *( ";" SP cookie-pair )
  
  it's basically the same.

  NOTE: for some reason server side cookie doesn't allow to have OWS
  (optional white space) but we accept it for my sake.
|#
(define (parse-cookie input)
  (let-values (((name value) (parse-cookie-pair input)))
    (let ((cookie (make-cookie name value)))
      (do () ((not (next-av? input)) cookie)
	(let ((token (parse-token input)))
	  (cond ((assoc token *parsers* string-ci=?)
		 => (lambda (spec)
		      (let ((slot (cadr spec))
			    (parser (caddr spec)))
			(if parser
			    (if (eqv? (get-char input) #\=)
				(let ((value (parser input)))
				  (slot-set! cookie slot value))
				(assertion-violation 'parse-cookie
						     "unexpected token format"
						     token))
			    ;; TODO check tailing string
			    (slot-set! cookie slot #t)))))
		(else
		 ;; extension-av
		 (case (lookahead-char input)
		   ((#\=)
		    (get-char input)
		    (let ((v (parse-it extension-char? input)))
		      (cookie-extension-add! cookie token v)))
		   ((#\;)
		    (cookie-extension-add! cookie token #t))
		   (else
		    ;; not a key=value thing
		    (cookie-extension-add! cookie 
		      (string-append token (parse-it extension-char? input))
		      #t))))))))))

(define (parse-cookie-string cookie)
  (parse-cookie (open-string-input-port cookie)))

;; this parses Cookie: header
;; a bit ugly but I don't want to break backward compatibility
(define (parse-cookies input)
  (let loop ((r '()))
    ;; ignore separator
    (parse-it (lambda (c) (char-set-contains? char-set:separators c)) input)
    (if (eof-object? (lookahead-char input))
	(reverse! r)
	(or (and-let* ((name (parse-token input))
		       ( (eqv? (get-char input) #\=) )
		       (value (parse-value input)))
	      
	      (loop (cons (make-cookie name value) r)))
	    (assertion-violation 'parse-cookies
				 "cookie pair must be $name=$value format")))))
(define (parse-cookies-string cookie)
  (parse-cookies (open-string-input-port cookie)))

;;; parsing utilities
(define (space? c) (and (not (eof-object? c)) (eqv? c #\space))) ;; SP
(define (semicolon/eof? c) (or (eof-object? c) (char=? c #\;)))
(define (parse-it pred input) 
  (skip-while space? input)
  (next-token-of pred input))

;; from RFC2616 section 2-2 https://tools.ietf.org/html/rfc2616#section-2.2
(define char-set:separators (string->char-set "()<>@,;:\\\"/[]?={} \t"))
(define char-set:token (char-set-difference char-set:ascii char-set:iso-control
					    char-set:separators))
(define char-set:ascii-digit (char-set-intersection char-set:ascii 
						    char-set:digit))
;; %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
;; NB: upper range must be boundary +1 otherwise it won't be there
(define char-set:cookie-octet 
  (ucs-range->char-set! #x5D #x7F #f
   (ucs-range->char-set! #x3C #x5C #f
    (ucs-range->char-set! #x2D #x3B #f
     (ucs-range->char-set! #x23 #x2C #f (ucs-range->char-set #x21 #x22))))))

;; char predicates
(define (token-char? c) (char-set-contains? char-set:token c))
(define (value-char? c) (char-set-contains? char-set:cookie-octet c))

;; parsers
(define (parse-token input) (parse-it token-char? input))
(define (parse-value input) (parse-it value-char? input))

(define (parse-cookie-pair input)
  (let ((name (parse-token input)))
    (if (eqv? (get-char input) #\=)
	(values name (parse-value input))
	(assertion-violation 'parse-cookie-pair
			     "cookie pair must be $name=$value format"))))

(define (parse-non-zero input)
  (define (digit? c) (char-set-contains? char-set:ascii-digit c))
  (let ((n (string->number (parse-it digit? input))))
    (if (or (not n) (zero? n))
	(assertion-violation 'parse-non-zero
			     "Max-Age requires non zero number" n)
	n)))

;; we only need <subdomain>
;; <domain> ::= <subdomain> | " "
;; 
;; <subdomain> ::= <label> | <subdomain> "." <label>
;; 
;; <label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
;; 
;; <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
;; 
;; <let-dig-hyp> ::= <let-dig> | "-"
;; 
;; <let-dig> ::= <letter> | <digit>
;; 
;; <letter> ::= any one of the 52 alphabetic characters A through Z in
;; upper case and a through z in lower case
;; 
;; <digit> ::= any one of the ten digits 0 through 9
;;
;; for now we don't check that strict.
(define char-set:subdomain
  (char-set-union! (string->char-set ".-")
   char-set:ascii-digit
   (char-set-intersection char-set:ascii char-set:letter)))
(define (subdomain-char? c) (char-set-contains? char-set:subdomain c))
(define (parse-domain input) (parse-it subdomain-char? input))

(define char-set:path
  (char-set-difference char-set:ascii char-set:iso-control 
		       (list->char-set '(#\;))))
(define (path-char? c) (char-set-contains? char-set:path c))
(define (parse-path input) (parse-it path-char? input))

(define extension-char? path-char?)

;; parsing date is pain in the ass. following is the specification of
;; date format (sane-cookie-date):
;;   Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
;;   Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
;;   Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
;; this is defined in RFC 2616 section 3.3.1. 
;; however, user agent *MUST* use an algorithm defined in RFC 6265 section
;; 5.1.1 or equivalent. and the algorithm seems much more flexible than
;; reading above 3 format. parsing date uses the algorithm.
;;
;; BNF definition of cookie-date
;;    cookie-date     = *delimiter date-token-list *delimiter
;;    date-token-list = date-token *( 1*delimiter date-token )
;;    date-token      = 1*non-delimiter
;; 
;;    delimiter       = %x09 / %x20-2F / %x3B-40 / %x5B-60 / %x7B-7E
;;    non-delimiter   = %x00-08 / %x0A-1F / DIGIT / ":" / ALPHA / %x7F-FF
;;    non-digit       = %x00-2F / %x3A-FF
;; 
;;    day-of-month    = 1*2DIGIT ( non-digit *OCTET )
;;    month           = ( "jan" / "feb" / "mar" / "apr" /
;;                        "may" / "jun" / "jul" / "aug" /
;;                        "sep" / "oct" / "nov" / "dec" ) *OCTET
;;    year            = 2*4DIGIT ( non-digit *OCTET )
;;    time            = hms-time ( non-digit *OCTET )
;;    hms-time        = time-field ":" time-field ":" time-field
;;    time-field      = 1*2DIGIT
(define char-set:delimiter
  (ucs-range->char-set! #x7B #x7F #f
   (ucs-range->char-set! #x5B #x61 #f
    (ucs-range->char-set! #x3C #x41 #f ;; trick to detect #\;
     (ucs-range->char-set! #x20 #x30 #f (ucs-range->char-set #x09 #x09))))))
(define char-set:non-delimiter
  (ucs-range->char-set! #x00 #x09 #f
   (ucs-range->char-set! #x0A #x20 #f
    (ucs-range->char-set! #x7F #x100 #f 
     (char-set-union char-set:ascii-digit
		     (list->char-set '(#\:))
		     (char-set-intersection char-set:ascii char-set:letter))))))
(define (delimiter? c) 
  (and (not (eof-object? c))
       (char-set-contains? char-set:delimiter c)))
(define (non-delimiter? c) (char-set-contains? char-set:non-delimiter c))
(define-constant +month-prefix+
  '((jan . 1) (feb . 2) (mar . 3) (apr . 4) (may . 5) (jun . 6) 
    (jul . 7) (aug . 8) (sep . 9) (oct . 10) (nov . 11) (dec . 12)))
(define (parse-date input)
  (define (->year token)
    (cond ((#/^(\d{2,4})\W*/ token) =>
	   (lambda (m)
	     (let ((n (string->number (m 1))))
	       (cond ((< n 100)
		      (cond ((<= 70 n 99) (+ 1900 n))
			    ((<= 0 n  69) (+ 2000 n))))
		     ((< 1600 n) n) ;; 4 digit year
		     (else (assertion-violation 'parse-date
						"invalid year" n))))))
	  (else #f)))
  (define (->month token)
    (and (>= (string-length token) 3)
	 (cond ((assq (string->symbol (string-downcase (substring token 0 3)))
		      +month-prefix+) => cdr)
	       (else #f))))
  (define (->time token)
    (cond ((#/^(\d{1,2}):(\d{1,2}):(\d{1,2})/ token) =>
	   (lambda (m)
	     (let ((hour (string->number (m 1)))
		   (min  (string->number (m 2)))
		   (sec  (string->number (m 3))))
	       (unless (<= 1 hour 23)
		 (assertion-violation 'parse-date "invalid hour" hour))
	       (unless (<= 0 min 59)
		 (assertion-violation 'parse-date "invalid minute" min))
	       (unless (<= 0 sec 59)
		 (assertion-violation 'parse-date "invalid second" sec))
	       (list hour min sec))))
	  (else #f)))
  (define (->day token)
    (cond ((#/^(\d{1,2})/ token) =>
	   (lambda (m)
	     (let ((d (string->number (m 1))))
	       (unless (<= 1 d 31)
		 (assertion-violation 'parse-date "invalid day" d))
	       d)))
	  (else #f)))

  (let loop ((year #f) (month #f) (day #f) (hour #f) (minute #f) (second #f))
    (skip-while delimiter? input)
    (let ((token (parse-it non-delimiter? input)))
      ;; look like?
      (cond ((string-null? token)
	     ;; must always be GMT (= offset 0)
	     ;; TODO Should make-date validate the input?
	     (make-date 0 (or second 0) (or minute 0) (or hour 0)
			day month year 0))
	    ((and (not hour) (->time token)) =>
	     (lambda (h&m&s)
	       (loop year month day (car h&m&s) (cadr h&m&s) (caddr h&m&s))))
	    ;; if there are 2 2*digit tokens, then we don't have any way
	    ;; to detect which is which
	    ((and (not day) (->day token)) =>
	     (lambda (d)
	       (loop year month d hour minute second)))
	    ((and (not month) (->month token)) =>
	     (lambda (m)
	       (loop year m day hour minute second)))
	    ((and (not year) (->year token)) =>
	     (lambda (y)
	       (loop y month day hour minute second)))
	    ;; ignore them
	    (else (loop year month day hour minute second))))))

(define (next-av? input)
  (let ((c (skip-until semicolon/eof? input)))
    (not (eof-object? c))))

(define (date->cookie-date date)
  ;; TODO ignore locale specific
  ;; for now we don't have any locale management so ~b returns
  ;; english name
  ;; Use RFC5322 (old RFC822) style
  (date->string date "~a, ~d ~b ~Y ~H:~M:~S GMT"))

(define *parsers*
  ;; name        slot     value?          to string
  `(("Expires"  expires   ,parse-date     ,date->cookie-date)
    ("Max-Age"  max-age   ,parse-non-zero #f)
    ("Domain"   domain    ,parse-domain   #f)
    ("Path"     path      ,parse-path     #f)
    ("Secure"   secure    #f              #f)
    ("HttpOnly" http-only #f              #f)))

;;; string conversion
(define (write-cookie out cookie)
  (format out "~a=~a" (cookie-name cookie) (cookie-value cookie)))
(define (cookie->string cookie) 
  (let-values (((out extract) (open-string-output-port)))
    (write-cookie out cookie)
    (for-each (lambda (spec)
		(and-let* ((v (slot-ref cookie (cadr spec))))
		  (if (caddr spec)
		      (let ((conv (cadddr spec)))
			(format out "; ~a=~a" (car spec)
				(if conv (conv v) v)))
		      (format out "; ~a" (car spec))))) *parsers*)
    ;; extension
    (let ((ht (cookie-extension cookie)))
      (hashtable-for-each
       (lambda (key value)
	 (when value 
	   (if (boolean? value)
	       (format out "; ~a" key)
	       (format out "; ~a=~a" key value)))) ht))
    (extract)))

(define (cookies->string cookies)
  (when (null? cookies)
    (assertion-violation 'cookies->string "at least one cookie is required"))
  (let-values (((out extract) (open-string-output-port)))
    (write-cookie out (car cookies))
    (for-each (lambda (cookie) (display "; " out) (write-cookie out cookie))
	      (cdr cookies))
    (extract)))

(define cookie-comparator
  (make-comparator cookie?
		   (lambda (c1 c2)
		     (and (equal? (cookie-name c1) (cookie-name c2))
			  (equal? (cookie-path c1) (cookie-path c2))))
		   #f
		   (lambda (c)
		     (let* ((hash (* 17 37))
			    (name-hash (+ hash (string-hash (cookie-name c)))))
		       (cond ((cookie-path c) =>
			      (lambda (path)
				(+ (* name-hash 37) (string-hash path))))
			     (else name-hash))))))
		     
(define-record-type cookie-jar
  (fields cookies)
  (protocol (lambda (p) (lambda () (p (set cookie-comparator))))))

(define (cookie-jar-size cookie-jar) (set-size (cookie-jar-cookies cookie-jar)))

(define (cookie-jar-add-cookie! cookie-jar cookie . cookie*)
  (apply set-adjoin! (cookie-jar-cookies cookie-jar) cookie cookie*)
  cookie-jar)
(define (cookie-jar-delete-cookie! cookie-jar cookie . cookie*)
  (apply set-delete! (cookie-jar-cookies cookie-jar) cookie cookie*)
  cookie-jar)
(define (cookie-jar->cookies cookie-jar . maybe-predicate)
  (define (true o) #t)
  (let ((predicate (if (null? maybe-predicate) true (car maybe-predicate))))
    (set-fold (lambda (cookie acc)
		(if (predicate cookie) (cons cookie acc) acc))
	      '() (cookie-jar-cookies cookie-jar))))

)
