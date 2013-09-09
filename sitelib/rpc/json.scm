;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rpc/json - JSON-RPC library.
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

;; JSON-RPC version 2.0
;;  reference: http://www.jsonrpc.org/specification

(library (rpc json)
    (export <json-request>
	    make-json-request
	    json-request-method
	    json-request-params
	    json-request-id
	    json-request->json-string
	    json-string->json-request

	    json-response->json-string
	    json-string->json-response
	    json-response-id
	    json-response-jsonrpc
	    json-response-result)
    (import (rnrs) 
	    (clos user)
	    (srfi :2 and-let*)
	    (json)
	    (rfc uuid)
	    (rpc message)
	    (rpc transport http))

  (define-class <json-request> ()
    (;; fixed version. we don't support 1.0
     (jsonrpc :init-value "2.0"     :reader json-request-jsonrpc)
     (method  :init-keyword :method :reader json-request-method)
     (params  :init-keyword :params :reader json-request-params :init-value '())
     (id      :init-keyword :id     :reader json-request-id)))

  (define-class <json-response> ()
    (;; fixed version. we don't support 1.0
     (jsonrpc :init-value "2.0"     :reader json-response-jsonrpc)
     (result  :init-keyword :result :reader json-response-result)
     (id      :init-keyword :id     :reader json-response-id)))

  (define (make-json-request method
			     :key (params '())
				  (id (uuid->string (make-v4-uuid))))
    (make <json-request>
      :method method
      :params params
      :id id))

  (define (json-request->json-string request)
    (call-with-string-output-port 
     (lambda (out) 
       (let ((sjosn `#((jsonrpc . ,(json-request-jsonrpc request))
		       (method  . ,(json-request-method request))
		       (id      . ,(json-request-id request))
		       (params  . ,(json-request-params request)))))
	 (json-write sjosn out)))))

  (define (json-string->json-request json)
    (let ((sjosn (vector->list (json-read (open-string-input-port json)))))
      (or (and-let* ((id      (assoc "id" sjosn))
		     (jsonrpc (assoc "jsonrpc" sjosn))
		     ( (string=? (cdr jsonrpc) "2.0") )
		     (method  (assoc "method" sjosn)))
	    (let ((params (assoc "params" sjosn)))
	      (apply make-json-request method :id id
		     (if params `(:params ,(cdr params)) '()))))
	  (error 'json-string->json-request "invalid JSON-RPC request" json))))

  ;; response object doesn't have error means we don't create error
  ;; with the procedure.
  (define (json-response->json-string response)
    (call-with-string-output-port 
     (lambda (out) 
       (let ((sjosn `#((jsonrpc . ,(json-response-jsonrpc response))
		       (id      . ,(json-response-id response))
		       (result  . ,(json-response-result response)))))
	 (json-write sjosn out)))))

  (define (json-string->json-response json)
    ;; the result must be vector
    (let ((sjosn (vector->list (json-read (open-string-input-port json)))))
      (or (and-let* ((id      (assoc "id" sjosn))
		     (jsonrpc (assoc "jsonrpc" sjosn))
		     ( (string=? (cdr jsonrpc) "2.0") ))
	    (cond ((assoc "error" sjosn)
		   => (lambda (slot)
			(let ((ejson (vector->list (cdr slot))))
			  (error 'json->json-response
				 (cdr (assoc "message" ejson))
				 `(code ,(cdr (assoc "code" ejson)))
				 `(data ,(or (cond ((assoc "data" ejson) => cdr)
						   (else '()))))))))
		  ((assoc "result" sjosn)
		   => (lambda (slot)
			(make <json-response> :result (cdr slot) :id (cdr id))))
		  (else
		   (error 'json-string->json-response
			  "invalid JSON-RPC response" josn))))
	  (error 'json-string->json-response 
		 "invalid JSON-RPC response" json))))

  (define-method rpc-http-content-type ((m <json-request>))
    "application/json")

  ;; default rpc-http-sender is binary so make it bytevector
  (define-method rpc-marshall-message ((m <json-request>))
    (string->utf8 (json-request->json-string m)))

  ;; http transport gives 3 argument request header and response
  ;; default response is binary
  (define-method rpc-unmarshall-message ((request <json-request>) header body)
    ;; TODO lookup encoding and decide which transcoder
    (json-string->json-response (utf8->string body)))
)