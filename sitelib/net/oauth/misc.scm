;;; -*- Scheme -*-
;;;
;;; misc.scm - OAuth 1.0 library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

#< (sagittarius regex) >
(library (net oauth misc)
    (export oauth-compose-query
	    oauth-uri-encode
	    build-auth-string
	    ;; for now
	    normalize-uri
	    random-source
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius regex)
	    (rfc http)
	    (rfc uri)
	    (srfi :13 strings)
	    (srfi :27 random-bits))

  ;; OAuth expects upper case somehow...
  (define (%-fix str)
    (regex-replace-all #/%[\da-fA-F][\da-fA-F]/ str
		       (lambda (m p) 
			 (put-string p (string-upcase (regex-group m 0))))))

  (define (oauth-uri-encode str)
    (%-fix (uri-encode-string str)))
  (define (oauth-compose-query params)
    (%-fix (http-compose-query #f params)))

  (define (build-auth-string parameters)
    (format "OAuth ~a"
	    (%-fix
	     (string-join (map (lambda (p)
				 (string-append (oauth-uri-encode (car p))
						"="
						(oauth-uri-encode (cadr p))))
			       parameters) ", "))))

  ;; 9.1.2
  (define (normalize-uri uri :optional (need-query? #f))
    (receive (scheme user-info host port path query frag) (uri-parse uri)
      (let ((uri (string-append
		  (string-downcase scheme)
		  "://"
		  (string-downcase host)
		  (cond ((not port) "")
			((and (string=? scheme "http") (= port 80)) "")
			((and (string=? scheme "https") (= port 443)) "")
			(else (string-append ":" (number->string port))))
		  path)))
	(if need-query?
	    (values uri (if query (query-string->alist query) '()))
	    uri))))

  ;; try not to create the same key
  (define random-source 
    (let ((s (make-random-source)))
      (random-source-randomize! s)
      (random-source-make-integers s)))

  )