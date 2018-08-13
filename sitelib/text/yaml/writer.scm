;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/writer.scm - YAML writer
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
(library (text yaml writer)
    (export emit-yaml)
    (import (rnrs)
	    (srfi :13 strings)
	    (srfi :14 char-sets))

(define emit-yaml 
  (case-lambda 
   ((out yaml) (emit-yaml out yaml +default-yaml-serializers+))
   ((out yaml serializers)
    (put-string out "---\n")
    (serialize-yaml out yaml 0 serializers))))

(define (serialize-yaml out yaml indent serializers)
  (let ((serializer (search-serializer serializers yaml)))
    ((serializer-emitter serializer) out yaml indent serializers)))

(define (search-serializer serializers yaml)
  (let ((r (find (lambda (s) ((serializer-predicate s) yaml)) serializers)))
    (unless r (error 'emit-yaml "Unknown yaml type" yaml))
    r))
(define (emit-indent out indent)
  (do ((i 0 (+ i 1))) ((= i indent)) (put-char out #\space)))

;; ((key . value) ...)
;; -> key: value (simple scalar)
;; -> key:
;;      key2: value ... (nested mapping, indent + 2)
;; -> key:
;;    - a ... (sequence, indent + 0)
(define (mapping-emitter out mapping indent serializers)
  (define (emit-k&v k&v)
    (define key (car k&v))
    (define value (cdr k&v))
    (let ((key-s (search-serializer serializers key)))
      (when (container-serializer? key-s)
	(error 'emit-yaml "Mapping key must be a simple value" key))
      (emit-indent out indent)
      ((serializer-emitter key-s) out key indent serializers))
    (put-string out ": ")
    (let ((val-s (search-serializer serializers value)))
      (when (container-serializer? val-s)
	(put-char out #\newline)
	(emit-indent out indent))
      ((serializer-emitter val-s) out value (+ indent 2) serializers))
    (put-char out #\newline))
  (for-each emit-k&v (vector->list mapping)))

(define (sequence-emitter out sequence indent serializers)
  (define (emit-entry e)
    (emit-indent out indent)
    (put-string out "- ")
    (let ((val-s (search-serializer serializers e)))
      ((serializer-emitter val-s) out e indent serializers))
    (put-char out #\newline))
  (for-each emit-entry sequence))

(define (string-emitter out yaml)
  (if (string-index yaml char-set:iso-control)
      ;; FIXME probably this isn't enough
      (write yaml out)
      (put-string out yaml)))

(define (number-emitter out yaml) (display yaml out))
  
(define (simple-emitter emitter)
  (lambda (out yaml indent serializers) (emitter out yaml)))

;; (container? predicate . emitter)
(define serializer-entry cons*)
(define container-serializer? car)
(define serializer-predicate cadr)
(define serializer-emitter cddr)

(define (simple-entry pred emitter)
  (serializer-entry #f pred (simple-emitter emitter)))
(define +json-yaml-serializers+
  `(
    ,(serializer-entry #t vector? mapping-emitter)
    ,(serializer-entry #t list? sequence-emitter)
    ,(simple-entry string? string-emitter)
    ,(simple-entry number? number-emitter)
    ))
(define +default-yaml-serializers+ +json-yaml-serializers+)

)
