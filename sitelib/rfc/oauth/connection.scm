;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/oauth/connection.scm - OAuth1 connection
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (rfc oauth connection)
    (export make-oauth-connection oauth-connection?
	    open-oauth-connection! close-oauth-connection!
	    oauth-connection-secure?
	    oauth-connection-consumer-key
	    oauth-connection-access-token
	    oauth-connection-signer
	    oauth-connection-http-connection)
    (import (rnrs)
	    (rfc oauth signature)
	    (rfc http-connections))

  (define-record-type oauth-connection
    (fields http-connection consumer-key access-token signer)
    (protocol
     (lambda (p)
       (define (construct conn key token signer)
	 (when (and (eq? 'PLAINTEXT (oauth-signer-method signer))
		    (not (http-connection-secure? conn)))
	   (assertion-violation 'make-oauth-connection
				"PLAINTEXT must be under TLS connection"))
	 (p conn key token signer))
       (case-lambda
	((conn key signer) (construct conn key #f signer))
	((conn key token signer) (construct conn key token signer))))))

  (define (open-oauth-connection! conn)
    (open-http-connection! (oauth-connection-http-connection conn)))
  (define (close-oauth-connection! conn)
    (close-http-connection! (oauth-connection-http-connection conn)))
  (define (oauth-connection-secure? conn)
    (http-connection-secure? (oauth-connection-http-connection conn)))
)
