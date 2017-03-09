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
    (export json-string->object json:builder?
	    json-object-builder)
    (import (rnrs)
	    (text json parse))

 (define-record-type json:builder
   (fields build-object))
 
 (define-record-type json:mapping
   (fields order key builder optional?)
   (protocol (lambda (p)
	       (lambda (order key builder optional?)
		 (unless (and (fixnum? order) (>= order 0))
		   (assertion-violation 'make-json:mapping 
		     "order must be a non negative fixnum" order))
		 (unless (string? key)
		   (assertion-violation 'make-json:mapping 
		     "key must be a string" key))
		 (unless (json:builder? builder)
		   (assertion-violation 'make-json:mapping 
		     "builder must be a json:builder" builder))
		 (p order key builder optional?)))))

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
   (define (check-existance v mappings)
     (do ((i 0 (+ i 1)) (mappings mappings (cdr mappings)))
	 ((null? mappings))
       (let ((m (car mappings))
	     (val (vector-ref v i)))
	 (when (eq? val ctr)
	   (if (json:mapping-optional? m)
	       (vector-set! v i #f)
	       (error 'json-string->object "missing key"
		      (json:mapping-key m)))))))

   (do ((i 0 (+ i 1)) (v (make-vector mapping-length ctr)))
       ((= i len)
	(check-existance v mappings)
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
 
 (define (json-string->object json-string builder)
   (let ((json (json-read (open-string-input-port json-string))))
     (json->object json builder)))

 (define (simple-build builder json) json)
 (define simple-json-builder (make-json:builder simple-build))

 (define-syntax json-object-object-builder
   (syntax-rules (?)
     ((_ "parse" ctr n (mapping ...) ((? key (spec ...)) rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
        (mapping ... (n key (json-object-builder spec ...) #t))
	(rest ...)))
     ((_ "parse" ctr n (mapping ...) ((? key) rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
        (mapping ... (n key simple-json-builder #t))
	(rest ...)))
     ((_ "parse" ctr n (mapping ...) ((key (spec ...)) rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
        (mapping ... (n key (json-object-builder spec ...) #f))
	(rest ...)))
     ((_ "parse" ctr n (mapping ...) (key rest ...))
      (json-object-object-builder "parse" ctr (+ n 1)
	(mapping ... (n key simple-json-builder #f))
	(rest ...)))
     ((_ "parse" ctr n ((order key builder optional?) ...) ())
      (make-json:object-builder ctr
	(list (make-json:mapping order key builder optional?) ...)))
     ((_ ctr spec ...)
      (json-object-object-builder "parse" ctr 0 () (spec ...)))))
 
 (define-syntax json-object-builder
   (syntax-rules (@)
     ;; top level array
     ((_ (@ ->array (spec ...)))
      (make-json:array-builder ->array (json-object-builder spec ...)))
     ((_ (@ ->array))
      (make-json:array-builder ->array simple-json-builder))
     ;; kv
     ((_ ctr kb* ...)
      (json-object-object-builder ctr kb* ...))
     ;; top level string or number?
     ((_ ctr) (make-json:builder ctr))))
 #|
e.g. JRD
(json-object-builder make-jrd
  "subject"
  (? "aliases" (@ list))
  (? "properties" (@ list make-jrd:property))
  (? "links"
     (@ list
	("rel"
	 (? "type")
	 (? "href")
	 (? "titles" (@ list make-jrd:title))
	 (? "properties" (@ list make-jrd:property))))))

e.g. location (from RFC7159)
(json-object-builder 
 (@ list
    (make-location
     "precision"
     "Latitude"
     "Longitude"
     "Address"
     "City"
     "State"
     "Zip"
     "Country")))

     
 |#
 )
