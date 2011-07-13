;;; -*- Scheme -*-
;;;
;;; uri.scm - parse and construct URIs 
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
;; RFC3986 URI Generic Syntax
;; <http://www.ietf.org/rfc/rfc3986.txt>

#!compatible
(library (rfc uri)
    (export uri-parse
	    uri-scheme&specific
	    uri-decompose-hierarchical
	    uri-decompose-authority

	    uri-decode
	    uri-decode-string
	    uri-encode
	    uri-encode-string

	    uri-compose
	    *rfc3986-unreserved-char-set*
	    *rfc2396-unreserved-char-set*)
    (import (rnrs)
	    (srfi :13 strings)
	    (srfi :14 char-set)
	    (sagittarius)
	    (sagittarius io)
	    (encoding decoder)
	    (sagittarius control)
	    (sagittarius regex))

  ;; from RFC3986 Appendix B. Parsing a URI Reference with a Regular Expression
  (define scheme (regex "^([a-zA-Z][a-zA-Z0-9+.-]*):"))
  (define hierarchical (regex "(?://([^/?#]*))?([^?#]*)?(?:\\?([^#]*))?(?:#(.*))?$"))
  (define authority (regex "(?:(.*?)@)?([^:]*)(?::(\\d*))"))

  (define (uri-scheme&specific uri)
    (cond ((looking-at scheme uri)
	   => (lambda (m) (values (string-downcase (m 1)) (m 'after))))
	  (else (values #f uri))))

  ;; returns (values authority path query fragments)
  (define (uri-decompose-hierarchical specific)
    (cond ((looking-at hierarchical specific)
	   => (lambda (m) (values (m 1) (m 2) (m 3) (m 4))))
	  (else (values #f #f #f #f))))

  ;; returns (values userinfo host port)
  (define (uri-decompose-authority auth)
    (cond ((looking-at authority auth)
	   => (lambda (m) (values (m 1) (m 2) (m 3))))
	  (else (values #f #f #f))))

  ;; returns (scheme user-info host port path query fragments)
  (define (uri-parse uri)
    (define (filter-non-empty-string str)
      (and (string? str)
	   (not (string-null? str))
	   str))
    (receive (scheme specific) (uri-scheme&specific uri)
      (receive (auth path query frag) (uri-decompose-hierarchical specific)
	(receive (user-info host port) (uri-decompose-authority auth)
	  (values scheme
		  user-info
		  (filter-non-empty-string host)
		  (and port (string->number port))
		  (filter-non-empty-string path)
		  query
		  frag)))))

  ;; compose
  (define-with-key (uri-compose :key (scheme #f) (userinfo #f) (host #f) (port #f)
				(authority #f) (path  #f) (path* #f) (query #f)
				(fragment #f) (specific #f))
      (with-output-to-string
	(lambda ()
	  (when scheme (display scheme) (display ":"))
	  (if specific
	      (display specific)
	      (begin
		(display "//")
		(if authority
		    (begin (display authority))
		    (begin
		      (when userinfo (display userinfo) (display "@"))
		      (when host     (display host))
		      (when port     (display ":") (display port))))
		(if path*
		    (begin
		      (unless (string-prefix? "/" path*) (display "/"))
		      (display path*))
		    (begin
		      (if path
			  (begin (unless (string-prefix? "/" path) (display "/"))
				 (display path))
			  (display "/"))
		      (when query (display "?") (display query))
		      (when fragment (display "#") (display fragment))))
		))
	  )))


  ;; encoding & decoding
  ;; This is for internal.
  ;; in and out must be binary-port
  (define-optional (uri-decode in out (optional (cgi-decode #f)))
    (define (hex-char? n)
      (cond ((<= #x30 n #x39) ;; #\0 - #\9
	     (- n #x30))
	    ((<= #x61 n #x66) ;; #\a - #\f
	     (- n #x57))
	    ((<= #x41 n #x46) ;; #\A - #\F
	     (- n #x37))
	    (else #f)))
    (check-arg binary-port? in 'uri-decode)
    (let loop ((c (get-u8 in)))
      (cond ((eof-object? c))
	    ((= c #x25) ;; %
	     (let1 c1 (get-u8 in)
	       (cond ((eof-object? c1) (put-u8 out c))
		     ((hex-char? c1)
		      => (lambda (i1)
			   (let1 c2 (get-u8 in)
			     (cond ((eof-object? c2) (put-u8 out c) (put-u8 out c1))
				   ((hex-char? c2)
				    => (lambda (i2)
					 (put-u8 out (+ (* i1 16) i2))
					 (loop (get-u8 in))))
				   (else (put-u8 out c) (put-u8 out c1) (loop c2))))))
		     (else (put-u8 out c) (loop c1)))))
	    ((= c #x2b) ;; +
	     (if cgi-decode
		 (put-u8 out #x20) ;; #\space
		 (put-u8 out #x2b))
	     (loop (get-u8 in)))
	    (else (put-u8 out c) (loop (get-u8 in))))))

  (define-optional (uri-decode-string string (optional (encoding 'utf-8)
						       (cgi-decode #f)))
    ;; decoder is mere codec.
    (let ((decoder (lookup-decoder encoding)))
      (if decoder
	  (let ((bv (string->bytevector string (make-transcoder decoder))))
	    (bytevector->string
	     (call-with-bytevector-output-port
	      (lambda (out)
		(uri-decode (open-bytevector-input-port bv) out cgi-decode)))
	     (make-transcoder decoder)))
	  string)))

  ;; 2396 -_.!~*'() + [0-9a-zA-Z]
  (define *rfc2396-unreserved-char-set* (char-set-union (string->char-set "-_.!~*'()")
							(char-set-difference char-set:letter+digit
									     char-set:ascii)))
  ;; 3986 -_.~ + [0-9a-zA-Z]
  (define *rfc3986-unreserved-char-set* (char-set-union (string->char-set "-_.~")
							(char-set-difference char-set:letter+digit
									     char-set:ascii)))

  (define-optional (uri-encode in out (optional (echars *rfc3986-unreserved-char-set*)))
    (define (hex->char-integer h)
      (cond ((<= 0 h 9) (+ h #x30))
	    ((<= #xa h #xf) (+ h (- #x61 10)))
	    (else (assertion-violation 'hex->char-integer
				       "invalid hex number"
				       h))))
    (let loop ((b (get-u8 in)))
      (unless (eof-object? b)
	(if (and (< b #x80)
		 (char-set-contains? echars (integer->char b)))
	    (put-u8 b)
	    (begin
	      (put-u8 out #x25) ;; %
	      (let ((hi (fxand (fxarithmetic-shift-right b 4) #xf))
		    (lo (fxand b #xf)))
		(put-u8 out (hex->char-integer hi))
		(put-u8 out (hex->char-integer lo)))))
	(loop (get-u8 in)))))

  (define-optional (uri-encode-string string (optional (encoding 'utf-8)
						       (echars *rfc3986-unreserved-char-set*)))
    (let ((decoder (lookup-decoder encoding)))
      (if decoder
	  (let ((bv (string->bytevector string (make-transcoder decoder))))
	    (bytevector->string
	     (call-with-bytevector-output-port
	      (lambda (out)
		(uri-encode (open-bytevector-input-port bv)
			    out
			    echars)))
	     (make-transcoder decoder)))
	  string)))
)


;; Local Variables:
;; coding: utf-8-unix
;; End:
