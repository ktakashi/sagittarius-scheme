;;; -*- Scheme -*-
;;;
;;; mime.scm - RFC2045 Multipurpose Internet Mail Extensions utilities
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

;; Ref RFC2045 Multipurpose Internet Mail Extensions Part One
;; <http://www.ietf.org/rfc/rfc2045.txt>
;; Ref RFC2045 Multipurpose Internet Mail Extensions Part Two
;; <http://www.ietf.org/rfc/rfc2046.txt>
;; Ref RFC2045 Multipurpose Internet Mail Extensions Part Three
;; <http://www.ietf.org/rfc/rfc2047.txt>

(library (rfc mime)
    (export mime-parse-version
	    mime-parse-content-type
	    mime-compose-parameters
	    mime-decode-word
	    mime-decode-text
	    mime-encode-word)
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-set)
	    (match)
	    (sagittarius define-optional)
	    (sagittarius regex)
	    (sagittarius)
	    (sagittarius io)
	    (encoding decoder)
	    (rfc :5322)
	    (rfc quoted-printable)
	    (rfc base64))

  (define *version-regex* (regex "^(\\d+)\\.(\\d+)$"))

  ;; returns list of major and minor versions in integers
  (define (mime-parse-version field)
    (and-let* (( field )
	       (s (string-concatenate
		   (map (lambda (f) (format "~a" f)) (rfc5322-field->tokens field))))
	       (m (looking-at *version-regex* s)))
      (map (lambda (d) (string->number (m d))) '(1 2))))


  (define *ct-token-chars*
    (char-set-difference (ucs-range->char-set #x21 #x7e) (string->char-set "()<>@,;:\\\"/[]?=")))

  ;; RFC2045 Content-Type header field
  ;; returns (<type> <subtype (attribute . <value>) ...)
  (define (mime-parse-content-type field)
    (and field
	 (call-with-input-string field
	   (lambda (input)
	     (and-let* ((type (rfc5322-next-token input `(,*ct-token-chars*)))
			( (string? type) )
			( (eqv? #\/ (rfc5322-next-token input '())) )
			(subtype (rfc5322-next-token input `(,*ct-token-chars*)))
			( (string? subtype) ))
	       (cons* (string-downcase type)
		      (string-downcase subtype)
		      (mime-parse-parameters input)))))))

  ;; RFC2183 Content-Disposition header field
  ;; returns (<token> (<attribute> . <value>) ...)
  (define (mime-parse-content-disposition field)
    (and field
	 (call-with-input-string field
	   (lambda (input)
	     (and-let* ((token (rfc5322-next-token input `(,*ct-token-chars*)))
			( (string? token) ))
	       (cons (string-downcase token)
		     (mime-parse-parameters input)))))))

  ;; parse a parameter-values type header field
  ;;  ;paremter=value;parameter-value
  ;; => ((parameter . value) ...)
  (define-optional (mime-parse-parameters (optional (input (current-input-port))))
    (let loop ((r '()))
      (cond ((and-let* (( (eqv? #\; (rfc5322-next-token input '())) )
			(attr (rfc5322-next-token input `(,*ct-token-chars*)))
			( (string? attr) )
			( (eqv? #\= (rfc5322-next-token input '())) )
			(val (rfc5322-next-token input `(,*ct-token-chars*
							 (,(string->char-set "\"")
							  . ,rfc5322-quoted-string))))
			( (string? val) ))
	       (cons attr val))
	     => (lambda (p) (loop (cons p r))))
	    (else (reverse! r)))))

  ;; Inverse of mime-parse-parameters
  ;; ((paramter . value) ...) => ;parameter=value;parameter=value ...
  (define-optional (mime-compose-parameters pvs (optional (port (current-output-port))
							  (start-column 0)))
    (define quote-re (regex "[\"\\\\]"))
    (define (quote-value v)
      (if (string-every *ct-token-chars* v)
	  v
	  (string-append "\"" (regex-replace-all quote-re v "\\\\\\0") "\"")))
    (define (valid-name p)
      (let ((z (format "~a" p)))
	(if (string-every *ct-token-chars* z)
	    z
	    (assertion-violation 'mime-compose-parameters
				 "invalid parameter valur for rfc2822 header"
				 p))))
    (define (gen)
      (fold (lambda (pv column)
	      (match pv
		((p . v)
		 (let* ((z (format "~a=~a" (valid-name p) (quote-value (format "~a" v))))
			(len (+ (string-length z) column)))
		   (cond ((> len 78)
			  (display ";\r\n") (display z) (string-length z))
			 (else
			  (display ";") (display z) (+ len column 2)))))
		(_ (assertion-violation 'mime-compose-parameters
					"bad parameter-value entry"
					pv))))
	    start-column pvs))
    (match port
      (#f (with-output-to-string gen))
      (#t (gen))
      ((? port?) (with-output-to-port port gen))))

  ;; RFC2047 header field encoding

  (define *mime-encoded-header-re* 
    (regex "^=\\?([-!#-'*+\\w\\^-~]+)\\?([-!#-'*+\\w\\^-~]+)\\?([!->@-~]+)\\?="))

  (define (%mime-decode-word word charset encoding body)
    (let ((decoder (lookup-decoder charset)))
      (if decoder
	  (cond ((string-ci=? encoding "q")
		 (decode decoder (quoted-printable-decode (string->utf8 body))))
		((string-ci=? encoding "b")
		 (decode decoder (base64-decode (string->utf8 body))))
		(else word))
	  word)))

  ;; decode rfc2047-encoded word, i.e. "=?...?="
  (define (mime-decode-word word)
    (cond ((looking-at *mime-encoded-header-re* word)
	   => (lambda (m)
		(if (equal? (m 'after) "")
		    (%mime-decode-word word (m 1) (m 2) (m 3))
		    word)))
	  (else word)))

  ;; decode the entire header field body, possibly a mixture of
  ;; encoded-words and orginary words.
  (define (mime-decode-text body)
    (let loop ((s body))
      (receive (pre rest) (string-scan s "=?" 'before*)
	(cond ((not pre) s)
	      ((looking-at *mime-encoded-header-re* rest)
	       => (lambda (m)
		    (string-append pre
				   (%mime-decode-word (m 0) (m 1) (m 2) (m 3))
				   (loop (m 'after)))))
	      (else s)))))

  (define (%canonical-encoding encoding)
    (case encoding
      ((B b base64) 'B)
      ((Q q quoted-printable) 'Q)
      (else
       (assertion-violation 'canonical-encoding
			    "unsupported MIME header encoding specifier"
			    encoding))))

  (define-optional (mime-encode-word word (optional (charset 'utf-8)
						    (transfer-encoding 'base64)))
    ;; decoder is just a codec.
    (let ((decoder (lookup-decoder charset))
	  (enc (%canonical-encoding transfer-encoding)))
      (format "=?~a?~a?~a?=" charset enc
	      ((if (eq? enc 'B)
		   base64-encode-string
		   quoted-printable-encode-string)
	       word (make-transcoder decoder (eol-style crlf))))))
)

