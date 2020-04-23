;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/xpath/fn.scm - XPath Functions and Operators
;;;
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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

;; ref:
;;  XPath and XQuery Functions and Operators 3.1
;;  https://www.w3.org/TR/xpath-functions-31/

#!nounbound
(library (text xml xpath fn)
    (export xpath-fn:node-name
	    xpath-fn:nilled
	    xpath-fn:string
	    xpath-fn:data
	    xpath-fn:base-uri
	    xpath-fn:document-uri)
    (import (rnrs)
	    (text xml errors)
	    (text xml dom)
	    (text xml schema)
	    (text xml xpath dm))

;;; 2 Accessors
;;; All accessor requires the $arg argument, the XPath evaluator
;;; must handle the context item.
(define (xpty0004-error who arg)
  (xqt-error 'XPTY0004 who "Invalid argument" arg))
(define-syntax dm:delegate
  (syntax-rules ()
    ((_ who delegate)
     (let ((proc delegate))
       (lambda (arg)
	 (cond ((null? arg) '())
	       ((not (node? arg))
		(xqt-error 'XPTY0004 'who "Not a node" arg))
	       (else (proc arg))))))))

;;;; 2.1 fn:node-name
;;;; fn:node-name($arg as node()?) as xs:QName?
(define xpath-fn:node-name (dm:delegate xpath-fn:node-name xpath-dm:node-name))

;;;; 2.2 fn:nilled
(define xpath-fn:nilled (dm:delegate xpath-fn:nilled xpath-dm:nilled))

;;;; 2.3 fn:string
(define xpath-fn:string
  (let ((delegate (dm:delegate xpath-fn:string xpath-dm:string-value)))
    (lambda (arg)
      (cond ((null? arg) "")
	    ((xs:any-atomic-type? arg) (atomic->string 'xpath-fn:string arg))
	    (else (delegate arg))))))

;;;; 2.4 fn:data
(define xpath-fn:data
  (let ((delegate (dm:delegate xpath-fn:data xpath-dm:typed-value)))
    (lambda (arg)
      (cond ((pair? arg) (map xpath-fn:data arg))
	    ;; ((array? args) ...)
	    ((xs:any-atomic-type? arg) arg) ;; correct?
	    (else (delegate arg))))))

;;;; 2.5 fn:base-uri
(define xpath-fn:base-uri (dm:delegate xpath-fn:base-uri xpath-dm:base-uri))

;;;; 2.6 fn:document-uri
(define xpath-fn:document-uri
  (dm:delegate xpath-fn:document-uri xpath-dm:document-uri))

;;;; 19 Casting
(define (atomic->string who atomic)
  (cond ((string? atomic) atomic)
	((or (integer? atomic) (flonum? atomic)) (number->string atomic))
	;; this may loose the original information when the value is
	;; either 0 or 1...
	((boolean? atomic) (if atomic "true" "false"))
	(else (xpty0004-error who atomic))))

)
