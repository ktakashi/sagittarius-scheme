;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rpc/transport/http - message independent http tranport framework for RPC
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

;; TODO need better way to handle transport more generic
(library (rpc transport http)
    (export rpc-http-request
	    ;; for hook
	    rpc-http-method
	    rpc-http-content-type
	    rpc-http-sender
	    rpc-http-receiver
	    rpc-http-response-type
	    rpc-http-unmarshall-message
	    )
    (import (rnrs)
	    (rfc http)
	    (clos user)
	    (srfi :13 strings)
	    (rpc message))

  ;; TODO should we handle the response unmarshalling?
  (define (rpc-http-request url message :key (unmarshall? #t)
			    :allow-other-keys opts)
    (let*-values (((server path) (url-server&path url))
		  ((status header body)
		   (apply http-request (rpc-http-method message) server path
			  :sender (rpc-http-sender message)
			  :receiver (rpc-http-receiver message)
			  :content-type (rpc-http-content-type message)
			  :secure (string-prefix? "https" url)
			  opts)))
      (if (and (char=? (string-ref status 0) #\2) unmarshall?)
	  ;; need header?
	  (values status header 
		  (rpc-http-unmarshall-message 
		   (rpc-http-response-type message) header body))
	  (values status header body))))

  (define-generic rpc-http-method)
  (define-generic rpc-http-content-type)
  (define-generic rpc-http-sender)
  (define-generic rpc-http-receiver)
  (define-generic rpc-http-response-type)
  
  ;; default configuration for message
  (define-method rpc-http-method (o) 'POST)
  (define-method rpc-http-content-type (o) "application/octet-stream")
  ;; default binary
  (define-method rpc-http-sender (m) 
    (http-blob-sender (rpc-marshall-message m)))
  (define-method rpc-http-receiver (o) (http-binary-receiver))
  ;; by default the request
  (define-method rpc-http-response-type (req) req)
  ;; by default header will be ignored
  (define-method rpc-http-unmarshall-message (type header body)
    (rpc-unmarshall-message type body))

)
