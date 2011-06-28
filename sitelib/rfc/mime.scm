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
	    mime-parse-content-type)
    (import (rnrs)
	    (rfc :5322)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-set)
	    (sagittarius define-optional)
	    (sagittarius regex)
	    (sagittarius)
	    (sagittarius io))

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
	       (list* (string-downcase type)
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

)

