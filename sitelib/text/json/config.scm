;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/json/config.scm - JSON parser config
;;;  
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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
(library (text json config)
    (export json-parser-config? make-json-parser-config
	    json-parser-config-strict?
	    json-parser-config-allow-comment?
	    json-parser-config-raise-error?
	    json-parser-config-max-nesting-depth
	    json-parser-config-array-handler
	    json-parser-config-object-handler
	    json-parser-config-null-handler
	    json-parser-config-boolean-handler
	    json-parser-config-number-handler
	    json-parser-config-string-handler
	    (rename (json-parser-config-builder json-parser-config)))
    (import (rnrs)
	    (record builder))
(define-record-type json-parser-config
  (fields strict?
	  allow-comment?
	  raise-error?
	  max-nesting-depth
	  array-handler
	  object-handler
	  null-handler
	  boolean-handler
	  number-handler
	  string-handler
	  ))

;; default non-strict, allow comment
(define-syntax json-parser-config-builder
  (make-record-builder json-parser-config
    ((allow-comment? #t)
     (raise-error? #t))))

)
