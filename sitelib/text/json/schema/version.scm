;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/version.scm - JSON schema versions
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

;; reference:
;; Draft-7, 2019-09 and 2020-12: https://json-schema.org/
#!nounbound
(library (text json schema version)
    (export json-schema:version
	    json-schema->version)
    (import (rnrs)
	    (rfc uri))

(define-enumeration json-schema:version
  (draft-7 2019-09 2020-12)
  json-schema-versions)

(define (json-schema->version schema)
  (define (rec schema original?)
    (cond ((assoc schema *json-schema-versions*) => cdr)
	  (original?
	   (let-values (((scheme ui host port path query frag)
			 (uri-parse schema)))
	     (rec (uri-compose :scheme "https" :host host :path path) #f)))
	  (else #f)))
  (rec schema #t))

(define *json-schema-versions*
  '(("https://json-schema.org/draft-07/schema"      . draft-7)
    ("https://json-schema.org/draft/2019-09/schema" . 2019-09)
    ("https://json-schema.org/draft/2020-12/schema" . 2020-12)))

)
