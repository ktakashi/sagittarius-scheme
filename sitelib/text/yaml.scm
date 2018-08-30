;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml.scm - YAML
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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
(library (text yaml)
    (export yaml-read
	    +default-yaml-builders+
	    +json-compat-yaml-builders+
	    +scheme-object-yaml-builders+

	    yaml-write
	    +default-yaml-serializers+
	    +json-compat-yaml-serializers+
	    +scheme-object-yaml-serializers+
	    )
    (import (rnrs)
	    (text yaml parser)
	    (text yaml builder)
	    (text yaml writer))

(define (yaml-read in . builder)
  (let ((yaml (parse-yaml in)))
    (map (lambda (y) (apply yaml->sexp y builder)) yaml)))

(define yaml-write
  (case-lambda
   ((yaml*) (yaml-write yaml* (current-output-port)))
   ((yaml* out) (yaml-write yaml* out +default-yaml-serializers+))
   ((yaml* out serializers)
    (for-each (lambda (yaml) (emit-yaml out yaml serializers)) yaml*))))
)
