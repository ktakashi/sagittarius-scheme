;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/uri.scm - URI object
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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

;; URI container
#!nounbound
(library (net uri)
    (export string->uri
	    uri->string

	    uri?
	    uri-scheme uri-specific uri-authority
	    uri-user-info uri-host uri-port uri-path uri-fragment
	    )
    (import (rnrs)
	    (rfc uri))

(define-record-type uri
  (fields scheme
	  specific
	  authority
	  user-info
	  host
	  port
	  path
	  query
	  fragment
	  >string ;; for laziness ;)
	  ))

(define (string->uri s)
  (let*-values (((scheme specific) (uri-scheme&specific s))
		((authority path query fragment)
		 (uri-decompose-hierarchical specific))
		((user-info host port) (uri-decompose-authority authority)))
    (make-uri scheme specific authority
	      user-info host port
	      path query fragment
	      s)))

)
