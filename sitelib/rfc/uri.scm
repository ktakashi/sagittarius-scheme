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

(library (rfc uri)
    (export uri-parse
	    uri-scheme&specific
	    uri-decompose-hierarchical
	    uri-decompose-authority)
    (import (srfi :13 strings)
	    (core)
	    (sagittarius)
	    (sagittarius regex))

  ;; unreserved ::= ALPHA / DIGIT / "-" / "." / "_" / "~" 

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


)


;; Local Variables:
;; coding: utf-8-unix
;; End:
