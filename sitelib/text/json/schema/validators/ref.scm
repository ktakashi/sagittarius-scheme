;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/validators/ref.scm - JSON schema $ref handling
;;;
;;;   Copyright (c) 2023  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (text json schema validators ref)
    (export json-schema:$ref json-schema:$recursive-ref
	    json-schema:draft-7-$ref

	    *json-schema:resolve-external-schema?*
	    *json-schema:external-schema-resolver*)
    (import (rnrs)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (srfi :45 lazy)
	    (text json parse)
	    (text json pointer)
	    (text json schema validators api)
	    (rfc uri)
	    (util uri))
(define *json-schema:resolve-external-schema?* (make-parameter #f))

(define (json-schema:default-external-schema-resolver uri)
  (call-with-port (transcoded-port (open-uri uri) (native-transcoder))
    json-read))
(define *json-schema:external-schema-resolver* (make-parameter #f))


;; probably better to check URI and only fragment but for now
(define (mere-json-pointer? s) (string-prefix? "#/" s))

(define (json-schema:draft-7-$ref value context schema-path)
  (display value) (newline)
  (lambda (e ctx) #t))

(define (json-schema:$ref value context schema-path)
  (lambda (e ctx) #t))


(define (json-schema:$recursive-ref value context schema-path)
  (lambda (e ctx) #t))

;; utilities

)
