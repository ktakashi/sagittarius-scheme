;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/object-builder.scm - JSON to Scheme object builder
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (text json object-builder)
    (export json-string->object json-port->object
	    json:builder? @ ?
	    json-object-builder)
    (import (rnrs)
	    (text json parse))

 (define-record-type json:builder
   (fields build-object))
 
 (define-record-type json:mapping
   (fields order key builder optional? default)
   (protocol (lambda (p)
	       (lambda (order key builder optional? default)
		 (unless (and (fixnum? order) (>= order 0))
		   (assertion-violation 'make-json:mapping 
		     "order must be a non negative fixnum" order))
		 (unless (string? key)
		   (assertion-violation 'make-json:mapping 
		     "key must be a string" key))
		 (unless (json:builder? builder)
		   (assertion-violation 'make-json:mapping 
		     "builder must be a json:builder" builder))
		 (p order key builder optional? default)))))

 (define-record-type json:object-builder
   (fields constructor mappings)
   (parent json:builder)
   (protocol (lambda (p)
	       (lambda (ctr mappings)
		 (unless (procedure? ctr)
		   (assertion-violation 'make-json:object-builder
		     "constructor must be a procedure" ctr))
		 (unless (for-all json:mapping? mappings)
		   (assertion-violation 'make-json:object-builder
		     "mappings must be a list of json:mapping" mappings))
		 ;; TODO check order duplication
		 ((p build-json-object) ctr
		  (list-sort
		   (lambda (a b)
		     (< (json:mapping-order a) (json:mapping-order b)))
		   mappings))))))
 
 (define-record-type json:array-builder
   (fields >array builder)
   (parent json:builder)
   (protocol (lambda (p)
	       (lambda (->array builder)
		 (unless (json:builder? builder)
		   (assertion-violation 'make-json:array-builder
		     "builder must be a json:builder" builder))
		 (unless (procedure? ->array)
		   (assertion-violation 'make-json:array-builder
		     "->array must be a procedure" ->array))
		 ((p build-json-array) ->array builder)))))

 (define (build-json-object builder json)
   (define mappings (json:object-builder-mappings builder))
   (define mapping-length (length mappings))
   (define ctr (json:object-builder-constructor builder))
   (define len (vector-length json))

   (define (find-mapping key mappings)
     (find (lambda (m) (string=? key (json:mapping-key m))) mappings))
   (define (check-existence v mappings)
     (do ((i 0 (+ i 1)) (mappings mappings (cdr mappings)))
	 ((null? mappings))
       (let ((m (car mappings))
	     (val (vector-ref v i)))
	 (when (eq? val ctr)
	   (if (json:mapping-optional? m)
	       (vector-set! v i (json:mapping-default m))
	       (error 'json-string->object "missing key"
		      (json:mapping-key m)))))))

   (do ((i 0 (+ i 1)) (v (make-vector mapping-length ctr)))
       ((= i len)
	(check-existence v mappings)
	(apply ctr (vector->list v)))
     (let* ((kv (vector-ref json i))
	    (mapping (find-mapping (car kv) mappings)))
       (unless mapping
	 (assertion-violation 'json-string->object "no mapping found for key"
			      (car kv)))

       (vector-set! v (json:mapping-order mapping)
		    (json->object (cdr kv) (json:mapping-builder mapping))))))

 ;; array must contain the same object in sense of builder creates
 (define (build-json-array builder json)
   (define ->array (json:array-builder->array builder))
   (define element-builder (json:array-builder-builder builder))
   (apply ->array (map (lambda (j) (json->object j element-builder)) json)))
 
 ;; internal use only since we don't have THE representation for JSON.
 (define (json->object json builder)
   ((json:builder-build-object builder) builder json))

 (define (json-port->object port builder)
   (json->object (json-read port) builder))
 
 (define (json-string->object json-string builder)
   (json-port->object (open-string-input-port json-string) builder))

 (define (simple-build builder json) json)
 (define simple-json-builder (make-json:builder simple-build))

 (define-syntax @ (syntax-rules ()))
 (define-syntax ? (syntax-rules ()))
 
 (define-syntax json-object-object-builder
   (syntax-rules (?)
     ((_ "parse" ctr n (mapping ...) ((? key default spec) rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
        (mapping ... (n key (json-object-builder spec) #t default))
	(rest ...)))
     ((_ "parse" ctr n (mapping ...) ((? key default) rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
        (mapping ... (n key simple-json-builder #t default))
	(rest ...)))
     ((_ "parse" ctr n (mapping ...) ((key spec) rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
        (mapping ... (n key (json-object-builder spec) #f #f))
	(rest ...)))
     ((_ "parse" ctr n (mapping ...) (key rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
	(mapping ... (n key simple-json-builder #f #f))
	(rest ...)))
     ((_ "parse" ctr n ((order key builder optional? default) ...) ())
      (make-json:object-builder ctr
	(list (make-json:mapping order key builder optional? default) ...)))
     ((_ ctr spec ...)
      (json-object-object-builder "parse" ctr 0 () (spec ...)))))
 
 (define-syntax json-object-builder
   (syntax-rules (@)
     ;; top level array
     ((_ (@ ->array spec))
      (make-json:array-builder ->array (json-object-builder spec)))
     ((_ (@ ->array))
      (make-json:array-builder ->array simple-json-builder))
     ;; kv
     ((_ (ctr kb* ...))
      (json-object-object-builder ctr kb* ...))
     ;; top level string or number?
     ((_ ctr) (make-json:builder (lambda (builder json) (ctr json))))))

;;; Serializer
 (define-record-type json:serializer
   (fields serialize-object))
 ;; container
 (define-record-type json:array-serializer
   (parent json:serializer)
   (fields serializer))
 (define-record-type json:sequential-access-array-serializer
   (parent json:array-serializer)
   (fields car cdr null?)
   (protocol (lambda (p)
	       (lambda (next serializer)
		 ((p serialize-list-like-object serializer) next)))))
 (define-record-type json:random-access-array-serializer
   (parent json:array-serializer)
   (fields ref length)
   (protocol (lambda (p)
	       (lambda (next builder)
		 ((p serialize-vector-like-object serializer) next)))))

 (define-record-type json:serializer-mapping
   (fields name ref serializer optional? absent))
 (define-record-type json:object-serializer
   (parent json:serializer)
   (fields mappings)
   (protocol (lambda (p)
	       (lambda (mappings)
		 ((p serialze-object) mappings)))))

 (define (serialize-object serializer obj)
   (define mappings (json:object-serializer-mappings serializer))
   (define (serialize-element mapping obj)
     (define absent-value (json:serializer-mapping mapping))
     (let ((val ((json:serializer-mapping-ref mapping) obj)))
       (if (or (and (procedure? absent-value) (absent-value val))
	       (equal? absent-value val))
	   (values #t #f)
	   (let* ((serializer (json:serializer-mapping-serializer mapping))
		  (json (object->json val serializer)))
	     (cons (json:serializer-mapping-name mapping) json)))))
   (let loop ((mappings mappings) (r '()))
     (if (null? mappings)
	 (list->vector (reverse r))
	 (let-values (((absent? json) (serialize-element (car mapping) obj)))
	   (if absent?
	       (loop (cdr mappings) r)
	       (loop (cdr mappings) (cons json r)))))))

 (define (serialize-list-like-object serializer obj)
   (define car (json:sequential-access-array-serializer-car serializer))
   (define cdr (json:sequential-access-array-serializer-cdr serializer))
   (define null? (json:sequential-access-array-serializer-null? serializer))
   (define element-serializer (json:array-serializer-serializer serializer))
   (let loop ((objs obj) (r '()))
     (if (null? objs)
	 (reverse r)
	 (let ((o (car objs)))
	   (loop (cdr objs) (cons (object->json o element-serializer) r))))))
 (define (serialize-vector-like-object serializer obj)
   (define ref (json:random-access-array-serializer-ref serializer))
   (define length (json:random-access-array-serializer-length serializer))
   (define element-serializer (json:array-serializer-serializer serializer))
   (define len (length obj))
   (let loop ((i 0) (r '()))
     (if (= i len)
	 (reverse r)
	 (let ((obj (ref obj i)))
	   (loop (+ i 1) (cons (object->json obj element-serializer) r))))))
 
 (define (object->json obj serializer)
   ((json:serializer-serialize-object serializer) serializer obj))
 
 (define (object->json-string obj serializer)
   (let-values (((out extract) (open-string-output-port)))
     (json-write (json:serialize-object obj serializer) out)
     (extract)))

 (define simple-json-serializer (make-json:serializer (lambda (obj _) obj)))
 
 (define-syntax json-object-object-serializer
   (syntax-rules (?)
     ((_ "parse" (mapping ...) ((? name absent ref spec) rest ...))
      (json-object-object-serializer "parse"
	(mapping ... (name ref (json-object-serializer spec) #t absent))
	(rest ...)))
     ((_ "parse" (mapping ...) ((? name absent ref) rest ...))
      (json-object-object-serializer "parse"
	(mapping ... (name ref simple-json-serializer #t absent)) (rest ...)))
     ((_ "parse" (mapping ...) ((name ref spec) rest ...))
      (json-object-object-serializer "parse"
	(mapping ... (name ref (json-object-serializer spec) #f #f))
	(rest ...)))
     ((_ "parse" (mapping ...) ((name ref) rest ...))
      (json-object-object-serializer "parse"
	(mapping ... (name ref simple-json-serializer #f #f)) (rest ...)))
     ((_ "parse" ((name ref serializer optional? absent) ...) ())
      (make-json:object-serializer
       (list (make-json:serializer-mapping name ref serializer optional? absent)
	     ...)))
     ((_ mapping mapping* ...)
      (json-object-object-serializer "parse" () (mapping mapping* ...)))))
 
 (define-syntax json-object-serializer
   (syntax-rules (-> @)
     ;; sequential access container (e.g. list)
     ((_ (-> car cdr null? spec))
      (make-json:sequential-access-array-serializer
       car cdr null? (json-object-serializer spec)))
     ((_ (-> car cdr null?))
      (make-json:sequential-access-array-serializer
       car cdr null? simple-json-serializer))
     ;; for convenience
     ((_ (-> spec))
      (make-json:sequential-access-array-serializer
       car cdr null? (json-object-serializer spec)))
     ((_ (->))
      (make-json:sequential-access-array-serializer
       car cdr null? simple-json-serializer))
     ;; random access container (e.g. vector)
     ((_ (@ ref len spec))
      (make-json:random-access-serializer
       ref len (json-object-serializer spec)))
     ((_ (@ ref len))
      (make-json:random-access-array-serializer
       ref len simple-json-serializer))
     ;; for convenience
     ((_ (@ spec))
      (make-json:random-access-array-serializer
       vector-ref vector-length (json-object-serializer spec)))
     ((_ (@))
      (make-json:random-access-array-serializer
       vector-ref vector-length simple-json-serializer))
     ((_ (mapping mapping* ...))
      (json-object-object-serializer mapping mapping*...))
     ((_ serializer)
      (make-json:serializer (lambda (o _) (serializer o))))))
 
 )
