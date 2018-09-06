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

	    add-yaml-serializer
	    replace-yaml-serializer

	    mapping-serializer
	    sequence-serializer
	    string-serializer
	    int-serializer
	    float-serializer
	    boolean-serializer
	    null-serializer
	    binary-serializer

	    *yaml:share-string?*
	    ;; for developer
	    yaml-writer-get-label
	    )
    (import (rnrs)
	    (text yaml builder)
	    (text yaml tags)
	    (rfc base64)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (srfi :113 sets))

;; external parameter
(define *yaml:share-string?* (make-parameter #f))
    
(define *labels* (make-parameter #f))
(define *label-generator* (make-parameter #f))
(define emit-yaml 
  (case-lambda 
   ((out yaml) (emit-yaml out yaml +default-yaml-serializers+))
   ((out yaml serializers)
    (define (dump-structure l)
      (for-each (lambda (v) (put-char out #\newline) (put-string out v)) l))
    (define label-generator
      (let ((i 0))
	(lambda ()
	  (set! i (+ i 1))
	  (string-append "label" (number->string i)))))
    (put-string out "%YAML 1.2\n")
    (put-string out "---")
    (dump-structure
     (parameterize ((*labels* (walk-yaml yaml serializers))
		    (*label-generator* label-generator))
       (serialize-yaml yaml 0 serializers)))
    (put-string out "\n...\n"))))

(define (yaml-writer-get-label yaml)
  (let ((labels (*labels*)))
    (and labels
	 (let ((label (hashtable-ref labels yaml #f)))
	   (cond ((eqv? #t label)
		  (let ((l ((*label-generator*))))
		    (hashtable-set! labels yaml l)
		    (values l #f)))
		 (label (values label #t))
		 (else (values #f #f)))))))

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
  (define (convert value)
    (let ((value-s (search-serializer serializers value)))
      (values ((serializer-emitter value-s) value indent serializers)
	      (container-serializer? value-s))))
  (define (emit klabel kref? key key-container?
		vlabel vref? value value-container?)
    (define (multi-lines v prefix indent)
      (let ((len (string-length prefix)))
	(pair-fold-right
	 (lambda (p acc)
	   (cons (string-append (if (eq? p v)
				    prefix
				    (emit-indent (+ indent len)))
				(car p)) acc)) '() v)))
    (define (->vlabel vlabel vref?)
      (cond ((and vref? vlabel) (string-append ": *" vlabel))
	    (vlabel (string-append ": &" vlabel))
	    (else ": ")))
    (define (->key kref? klabel key vlabel)
      (cond ((and kref? klabel) (string-append "*" klabel " " vlabel))
	    (klabel (string-append "&" klabel " " (car key) vlabel))
	    (else (string-append (car key) vlabel))))
    ;; we don't support key with &label here
    (cond (key-container?
	   `(,@(multi-lines key "? " indent)
	     ,@(multi-lines value (->vlabel vlabel vref?) indent)))
	  (value-container?
	   `(,(->key kref? klabel key (->vlabel vlabel vref?))
	     ,@(multi-lines value (emit-indent (+ indent 2)) indent)))
	  ;; FIXME better way of handling ...
	  ((not (null? (cdr value)))
	   `(,(string-append (->key kref? klabel key (->vlabel vlabel vref?))
			     (case (car value)
			       ((block) "|-")
			       (else    ">-")))
	     ,@(multi-lines (cdr value) (emit-indent (+ indent 2)) indent)))
	  (else
	   (list (string-append
		  (->key kref? klabel key (->vlabel vlabel vref?))
		  (car value))))))

  (define (->label label ref?)
    (if label (string-append (if ref? "*" "&") label) ""))
  
  (if (zero? (vector-length mapping))
      '("{}")
      (append-map
       (lambda (k&v)
	 (define key-entry (car k&v))
	 (define value-entry (cdr k&v))
	 (let-values (((klabel kref?) (yaml-writer-get-label key-entry))
		      ((vlabel vref?) (yaml-writer-get-label value-entry)))
	   ;; handle special case here
	   (if (and kref? vref?)
	       (list (string-append "*" klabel " : *" vlabel))
	       (let-values (((key key-container?)
			     (if kref? (values '("") #f) (convert key-entry)))
			    ((value value-container?)
			     (if vref?
				 (values '("") #f)
				 (convert value-entry))))
		 ;; FIXME value conversion might be useless here
		 (if (eq? 'null (cdr k&v))
		     (emit klabel kref? key key-container? #f #f #f #f)
		     (emit klabel kref? key key-container?
			   vlabel vref? value value-container?))))))
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

(define (string-emitter yaml indent)
  (define (write/escape yaml out)
    (define (escape c)
      (case c
	((#\nul) "\\0")
	((#\alarm) "\\a")
	((#\backspace) "\\b")
	((#\tab) "\t")
	((#\newline) "\\n")
	((#\vtab) "\\v")
	((#\page) "\\f")
	((#\return) "\\r")
	((#\esc) "\\e")
	((#\space) "\\ ")
	((#\") "\\\"")
	((#\\) "\\\\")
	((#\/) "\\/")
	((#\x85) "\\N")
	((#\xA0) "\\_")
	((#\x2028)  "\\L")
	((#\x2029)  "\\P")
	(else (error 'emit-yaml "Unknown control character" c))))
    (string-for-each
     (lambda (c)
       (if (char-set-contains? char-set:iso-control c)
	   (put-string out (escape c))
	   (put-char out c))) yaml))
  
  (define (handle-multi-line yaml indent)
    (let-values (((out extract) (open-string-output-port)))
      (define in (open-string-input-port yaml))
      (let loop ((r '()) (line (get-line in)))
	(if (eof-object? line)
	    (cons 'block (reverse r))
	    (begin
	      (write/escape line out)
	      (loop (cons (extract) r) (get-line in)))))))
  (define (handle-write yaml indent)
    (if (string-index yaml #\newline)
	;; multiline
	(handle-multi-line yaml (+ indent 2))
	(let-values (((out extract) (open-string-output-port)))
	  (put-char out #\")
	  (write/escape yaml out)
	  (put-char out #\")
	  (list (extract)))))
  (if (string-index yaml char-set:iso-control)
      ;; FIXME probably this isn't enough
      (handle-write yaml indent)
      (list yaml)))

(define (number-emitter yaml) (list (number->string yaml)))
(define (boolean-emitter yaml) (if yaml '("true") '("false")))
(define (null-emitter yaml) '("~"))
(define (date-emitter yaml) (date->yaml-canonical-date yaml))
(define (binary-emitter yaml)
  (list (string-append "!!binary "
		       (utf8->string (base64-encode yaml :line-width #f)))))

(define (simple-emitter emitter)
  (lambda (yaml indent serializers) (emitter yaml)))
(define (indent-emitter emitter)
  (lambda (yaml indent serializers) (emitter yaml indent)))

;; (id container? predicate . emitter)
(define serializer-entry cons*)
(define serializer-walker car)
(define (container-serializer? s) (not (not (serializer-walker s))))
(define serializer-predicate cadr)
(define serializer-emitter cddr)

(define (simple-entry pred emitter)
  (serializer-entry #f pred (simple-emitter emitter)))
(define (indent-entry pred emitter)
  (serializer-entry #f pred (indent-emitter emitter)))

(define (add-yaml-serializer base id serializer)
  (cons (cons id serializer) base))
(define (replace-yaml-serializer base id serializer)
  (let ((r (remp (lambda (s) (eq? (car s) id)) base)))
    (add-yaml-serializer r id serializer)))

(define (walk-yaml yaml serializers)
  (define store (make-eq-hashtable))
  (define (rec yaml serializers)
    (define (store-it yaml)
      (cond ((hashtable-contains? store yaml)
	     ;; okay more than once
	     (hashtable-set! store yaml #t))
	    ;; first we need to create an entry
	    (else (hashtable-set! store yaml #f))))
    (cond ((and (string? yaml) (*yaml:share-string?*)) (store-it yaml))
	  ((or (string? yaml) (symbol? yaml) (number? yaml) (boolean? yaml)))
	  (else (store-it yaml)))
    (let* ((s (search-serializer serializers yaml))
	   (walker (serializer-walker s)))
      (when walker
	(walker yaml
		(lambda (next-yaml) (rec next-yaml serializers))))))
  (rec yaml serializers)
  store)

(define (mapping-walker mapping next-walk)
  (vector-for-each (lambda (k&v)
		     (next-walk (car k&v))
		     (next-walk (cdr k&v))) mapping))
(define (sequence-walker sequence next-walk)
  (for-each next-walk sequence))

(define mapping-serializer
  (serializer-entry mapping-walker vector? mapping-emitter))
(define sequence-serializer
  (serializer-entry sequence-walker list? sequence-emitter))
(define string-serializer (indent-entry string? string-emitter))
(define int-serializer (simple-entry integer? number-emitter))
(define float-serializer (simple-entry real? number-emitter))
(define boolean-serializer (simple-entry boolean? boolean-emitter))
(define null-serializer (simple-entry (lambda (v) (eq? 'null v)) null-emitter))
(define binary-serializer (simple-entry bytevector? binary-emitter))

(define +json-compat-yaml-serializers+
  `(
    (,+yaml-tag:map+ . ,mapping-serializer)
    (,+yaml-tag:seq+ . ,sequence-serializer)
    (,+yaml-tag:str+ . ,string-serializer)
    (,+yaml-tag:int+ . ,int-serializer)
    (,+yaml-tag:float+ . ,float-serializer)
    (,+yaml-tag:bool+ . ,boolean-serializer)
    (,+yaml-tag:null+ . ,null-serializer)
    (,+yaml-tag:binary+ . ,binary-serializer)
    ))
(define +default-yaml-serializers+ +json-compat-yaml-serializers+)

(define +scheme-object-yaml-serializers+
  (add-yaml-serializer
   (add-yaml-serializer
    (replace-yaml-serializer
     (replace-yaml-serializer
      +default-yaml-serializers+
      +yaml-tag:map+ (serializer-entry #t hashtable? hashtable-mapping-emitter))
     +yaml-tag:seq+ (serializer-entry #t vector? vector-sequence-emitter))
    +yaml-tag:set+ (serializer-entry #t set? set-emitter))
   +yaml-tag:timestamp+ (simple-entry date? date-emitter)))

)
