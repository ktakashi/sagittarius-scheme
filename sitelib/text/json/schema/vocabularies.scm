;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/schema/vocabularies.scm - JSON schema vocabularies
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
(library (text json schema vocabularies)
    (export retrieve-json-schema-vocabulary
	    *json-schema:vocabulary-handler*)
    (import (rnrs)
	    (srfi :39 parameters)
	    (text json parse)
	    (util uri))
;; Parameter
(define *json-schema:vocabulary-handler* 
  ;; default do nothing
  (make-parameter (lambda (uri) #f)))

(define (retrieve-json-schema-vocabulary uri)
  (cond ((assoc uri +json-schema-known-vocabulary-schema-mapping+) =>
	 (lambda (s)
	   (let ((in (transcoded-port (open-uri (cadr s)) (native-transcoder))))
	     (call-with-port in json-read))))
	(else ((*json-schema:vocabulary-handler*) uri))))

;; $vocabulary
(define +json-schema-known-vocabulary-schema-mapping+
  ;; (id uri)
  '(;; 2019-09
    ("https://json-schema.org/draft/2019-09/vocab/core"
     "https://json-schema.org/draft/2019-09/meta/core")
    ("https://json-schema.org/draft/2019-09/vocab/applicator"
     "https://json-schema.org/draft/2019-09/meta/applicator")
    ("https://json-schema.org/draft/2019-09/vocab/validation"
     "https://json-schema.org/draft/2019-09/meta/validation")
    ("https://json-schema.org/draft/2019-09/vocab/meta-data"
     "https://json-schema.org/draft/2019-09/meta/meta-data")
    ("https://json-schema.org/draft/2019-09/vocab/format"
     "https://json-schema.org/draft/2019-09/meta/format")
    ("https://json-schema.org/draft/2019-09/vocab/content"
     "https://json-schema.org/draft/2019-09/meta/content")
    ;; 2020-12
    ("https://json-schema.org/draft/2020-12/vocab/core"
     "https://json-schema.org/draft/2020-12/meta/core")
    ("https://json-schema.org/draft/2020-12/vocab/applicator"
     "https://json-schema.org/draft/2020-12/meta/applicator")
    ("https://json-schema.org/draft/2020-12/vocab/unevaluated"
     "https://json-schema.org/draft/2020-12/meta/unevaluated")
    ("https://json-schema.org/draft/2020-12/vocab/validation"
     "https://json-schema.org/draft/2020-12/meta/validation")
    ("https://json-schema.org/draft/2020-12/vocab/meta-data"
     "https://json-schema.org/draft/2020-12/meta/meta-data")
    ("https://json-schema.org/draft/2020-12/vocab/format-annotation"
     "https://json-schema.org/draft/2020-12/meta/format-annotation")
    ("https://json-schema.org/draft/2020-12/vocab/content"
     "https://json-schema.org/draft/2020-12/meta/content")
    ))

)
