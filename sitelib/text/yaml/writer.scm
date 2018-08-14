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
    (export emit-yaml
	    +default-yaml-serializers+
	    +json-compat-yaml-serializers+
	    +scheme-object-yaml-serializers+
	    )
    (import (rnrs)
	    (text yaml builder)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :113 sets))

(define emit-yaml 
  (case-lambda 
   ((out yaml) (emit-yaml out yaml +default-yaml-serializers+))
   ((out yaml serializers)
    (define (dump-structure l)
      (for-each (lambda (v) (put-char out #\newline) (put-string out v)) l))
    (put-string out "---")
    (dump-structure (serialize-yaml yaml 0 serializers))
    (put-string out "\n...\n"))))

(define (serialize-yaml yaml indent serializers)
  (let ((serializer (search-serializer serializers yaml)))
    ((serializer-emitter serializer) yaml indent serializers)))

(define (search-serializer serializers yaml)
  (let ((r (find (lambda (s) ((serializer-predicate (cdr s)) yaml))
		 serializers)))
    (unless r (error 'emit-yaml "Unknown yaml type" yaml))
    (cdr r)))

(define (emit-indent indent) (make-string indent #\space))

;; ((key . value) ...)
;; -> key: value (simple scalar)
;; -> key:
;;      key2: value ... (nested mapping, indent + 2)
;; -> key:
;;    - a ... (sequence, indent + 0)
(define (mapping-emitter  mapping indent serializers)
  (define (convert-key key)
    (let ((key-s (search-serializer serializers key)))
      (when (container-serializer? key-s)
	(error 'emit-yaml "Mapping key must be a simple value" key))
      ((serializer-emitter key-s) key indent serializers)))
  (define (convert-value value)
    (let ((value-s (search-serializer serializers value)))
      (values ((serializer-emitter value-s) value indent serializers)
	      (container-serializer? value-s))))
  (if (zero? (vector-length mapping))
      '("{}")
      (append-map
       (lambda (k&v)
	 (define key (convert-key (car k&v)))
	 (if (eq? 'null (cdr k&v)) ;; a bit of cheat
	     (list (string-append "? " (car key)))
	     (let-values (((value container?) (convert-value (cdr k&v))))
	       (if container?
		   (cons (string-append (car key) ": ")
			 (map (lambda (e)
				(string-append (emit-indent (+ indent 2)) e))
			      value))
		   (list (string-append (car key) ": " (car value)))))))
       (vector->list mapping))))
(define (hashtable-mapping-emitter mapping indent serializers)
  (let-values (((keys values) (hashtable-entries mapping)))
    (mapping-emitter (vector-map cons keys values) indent serializers)))

(define (sequence-emitter sequence indent serializers)
  (if (null? sequence)
      '("[]")
      (append-map (lambda (e)
		    (let* ((rec (serialize-yaml e indent serializers))
			   (first (car rec)))
		      (map (lambda (e)
			     (string-append
			      (if (eq? e first)
				  "- "
				  ;; add length of "- " (= 2)
				  (emit-indent (+ indent 2))) e))
			   rec))) sequence)))
(define (vector-sequence-emitter sequence indent serializers)
  (sequence-emitter (vector->list sequence) indent serializers))

(define (set-emitter set indent serializers)
  (define (set->flow-string set)
    (let-values (((out extract) (open-string-output-port)))
      (put-char out #\{)
      (set-fold (lambda (e first?)
		  (let ((s (search-serializer serializers e)))
		    (when (container-serializer? s)
		      (error 'emit-yaml "Set element must be a simple value"
			     e))
		    (let ((v ((serializer-emitter s) e indent serializers)))
		      (when first? (put-char out #\space))
		      (put-string out (car v))
		      (put-char out #\,)
		      (put-char out #\space)))
		  #f) #t set)
      (put-char out #\})
      (extract)))
  ;; using flow here for convenience
  (list (string-append "!!set " (set->flow-string set))))

(define (string-emitter yaml)
  (list (if (string-index yaml char-set:iso-control)
	    ;; FIXME probably this isn't enough
	    (let-values (((out extract) (open-string-output-port)))
	      (write yaml out)
	      (extract))
	    yaml)))

(define (number-emitter yaml) (list (number->string yaml)))
(define (boolean-emitter yaml)
  (if yaml '("true") '("false")))
(define (null-emitter yaml) '("~"))

(define (date-emitter yaml) (date->yaml-canonical-date yaml))
   
(define (simple-emitter emitter)
  (lambda (yaml indent serializers) (emitter yaml)))

;; (id container? predicate . emitter)
(define serializer-entry cons*)
(define container-serializer? car)
(define serializer-predicate cadr)
(define serializer-emitter cddr)

(define (simple-entry pred emitter)
  (serializer-entry #f pred (simple-emitter emitter)))

(define (add-yaml-serializer base id serializer)
  (cons (cons id serializer) base))
(define (replace-yaml-serializer base id serializer)
  (let ((r (remp (lambda (s) (eq? (car s) id)) base)))
    (add-yaml-serializer r id serializer)))

(define +json-compat-yaml-serializers+
  `(
    (mapping . ,(serializer-entry #t vector? mapping-emitter))
    (sequence . ,(serializer-entry #t list? sequence-emitter))
    (string . ,(simple-entry string? string-emitter))
    (number . ,(simple-entry number? number-emitter))
    (boolean . ,(simple-entry boolean? boolean-emitter))
    (null . ,(simple-entry (lambda (v) (eq? 'null v)) null-emitter))
    ))
(define +default-yaml-serializers+ +json-compat-yaml-serializers+)

(define +scheme-object-yaml-serializers+
  (add-yaml-serializer
   (add-yaml-serializer
    (replace-yaml-serializer
     (replace-yaml-serializer
      +default-yaml-serializers+
      'mapping (serializer-entry #t hashtable? hashtable-mapping-emitter))
     'sequence (serializer-entry #t vector? vector-sequence-emitter))
    'set (serializer-entry #t set? set-emitter))
  'timestamp (simple-entry date? date-emitter)))

)
