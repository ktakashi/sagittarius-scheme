;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; uri/regex.scm - Regular expression URI parser
;;;  
;;;   Copyright (c) 2019  Takashi Kato  <ktakashi@ymail.com>
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
;; https://tools.ietf.org/html/rfc3986#appendix-B
#!nounbound
#!read-macro=sagittarius/regex
(library (rfc uri regex)
    (export uri-scheme&specific
	    uri-decompose-hierarchical
	    uri-decompose-authority)
    (import (rnrs)
	    (shorten)
	    (sagittarius regex))

;; from RFC3986 Appendix B. Parsing a URI Reference with a Regular Expression
(define scheme #/^([a-zA-Z][a-zA-Z0-9+.-]*):/)
(define hierarchical #/(?:\/\/([^\/?#]*))?([^?#]+)?(?:\?([^#]*))?(?:#(.*))?$/)
(define authority #/(?:(.*?)@)?([^:]*)(?::(\d*))?/)

(define (uri-scheme&specific uri)
  (cond ((looking-at scheme uri)
	 => (lambda (m) (values (string-downcase (m 1)) (m 'after))))
	(else (values #f uri))))

;; returns (values authority path query fragments)
(define (uri-decompose-hierarchical specific)
  (cond ((looking-at hierarchical specific)
	 => (^m (values (m 1) (m 2) (m 3) (m 4))))
	(else (values #f #f #f #f))))

;; returns (values userinfo host port)
(define (uri-decompose-authority auth)
  (cond ((and (string? auth) (looking-at authority auth))
	 => (^m (values (m 1) (m 2) (m 3))))
	(else (values #f #f #f))))

)
