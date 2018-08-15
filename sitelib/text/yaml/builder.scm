;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/builder.scm - YAML - SEXP builder
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
(library (text yaml builder)
    (export yaml->sexp
	    +default-yaml-builders+
	    +json-compat-yaml-builders+
	    +scheme-object-yaml-builders+
	    ;; for testing
	    date->yaml-canonical-date)
    (import (rnrs)
	    (text yaml conditions)
	    (text yaml nodes)
	    (text yaml tags)
	    (text yaml tokens)
	    (rfc base64)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (srfi :113 sets)
	    (srfi :114 comparators)
	    (srfi :115 regexp))

(define-condition-type &yaml-builder &yaml
  make-yaml-builder-error yaml-builder-error?
  (when yaml-builder-error-when)
  (node yaml-builder-error-node))
(define (yaml-builder-error node when message . irr)
  (raise (apply condition
		(filter-map values
			    (list
			     (make-yaml-builder-error when node)
			     (and (yaml-node-start-mark node)
				  (yaml-scanner-mark->condition
				   (yaml-node-start-mark node)))
			     (make-who-condition 'yaml->sexp)
			     (make-message-condition message)
			     (and (not (null? irr))
				  (make-irritants-condition irr)))))))
(define yaml->sexp
  (case-lambda
   ((node) (yaml->sexp node +default-yaml-builders+))
   ((node builders)
    (cond ((yaml-document? node)
	   (yaml->sexp (yaml-document-root-node node) builders))
	  ((yaml-node? node)
	   (let ((objs (make-eq-hashtable)))
	     (build-sexp node builders objs)))
	  (else (assertion-violation 'yaml->sexp
				     "YAML node or document required" node))))))

(define (build-sexp node builders constructed-objects)
  (define tag (yaml-node-tag node))
  (define (find-builder builder)
    (let ((pred (car builder))
	  (expected-tag (cadr builder)))
      (and (pred node) (string=? expected-tag tag))))
  (let ((builder (find find-builder builders)))
    (unless builder
      (yaml-builder-error node "While searching for a builder"
       "The node/tag is not supported with the given set of builders"
       (yaml-node-tag node)))
    ((cddr builder) node
     (lambda (next-node)
       (when (eq? next-node node)
	 (error 'yaml->sexp "[Internal] Recursive building of the node" node))
       (cond ((hashtable-ref constructed-objects next-node #f))
	     (else
	      (let ((r (build-sexp next-node builders constructed-objects)))
		(hashtable-set! constructed-objects next-node r)
		r)))))))

;;; Builders
(define (->binary v) (base64-decode-string v :transcoder #f))
(define (->bool v)
  (or (and (memv (char-downcase (string-ref v 0)) '(#\y #\t)) #t)
      (string=? (string-downcase v) "on")))

(define (parse-sexagesimal n)
  (let ((tokens (reverse
		 (map string->number (string-tokenize n char-set:digit)))))
    (do ((tokens tokens (cdr tokens))
	 (coef 0 (+ coef 1))
	 (r 0 (+ (* (car tokens) (expt 60 coef)) r)))
	((null? tokens) r))))
(define (strip-sign v)
  (case (string-ref v 0)
    ((#\-) (values #t (substring v 1 (string-length v))))
    ((#\+) (values #f (substring v 1 (string-length v))))
    (else  (values #f v))))
(define (->int v)
  (define (handle-number n radix) (string->number (string-delete #\_ n) radix))
  (define (handle-binary n) (handle-number n 2))
  (define (handle-hex n) (handle-number n 16))
  (define (handle-octet n) (handle-number n 8))
  (define (handle-decimal n) (handle-number n 10))
  (define (handle-sexagesimal n) (parse-sexagesimal (string-delete #\_ n)))
  (let-values (((neg n) (strip-sign v)))
    (let ((i (case (string-ref n 0)
	       ((#\0)
		(if (= (string-length n) 1)
		    0
		    (case (string-ref n 1)
		      ((#\b) (handle-binary (substring n 2 (string-length n))))
		      ((#\x) (handle-hex (substring n 2 (string-length n))))
		      (else
		       (if (> (string-length n) 1)
			   (handle-octet (substring n 1 (string-length n)))
			   (handle-decimal n))))))
	       (else
		(if (string-index n #\:)
		    (handle-sexagesimal n)
		    (handle-decimal n))))))
      (if neg
	  (- i)
	  i))))

(define (->float v)
  (let-values (((neg n) (strip-sign v)))
    (let* ((ln (string-downcase (string-delete #\_ n)))
	   (v (cond ((member ln '(".inf" ".Inf" ".INF")) +inf.0)
		     ((member ln '(".nan" ".NaN" ".NAN")) +nan.0)
		     ((string-index ln #\:)
		      (let* ((i (string-index ln #\.))
			     (int (substring ln 0 i))
			     (frac (substring ln i (string-length ln))))
			(+ (parse-sexagesimal int) (string->number frac))))
		     (else (string->number ln)))))
      (inexact (if neg (- v) v)))))

(define +time-separators-set+ (char-set #\space #\tab #\T #\t))
(define (->date v)
  (define len (string-length v))
  (define (parse-ymd ymd)
    (values (string->number (substring ymd 0 4))
	    (string->number (substring ymd 5 7))
	    (string->number (substring ymd 8 10))))
  (define (fraction->nanosecond v)
    (* (case (string-length v)
	 ((0) 0) ;; error?
	 ((1) (* (string->number v) 100))
	 ((2) (* (string->number v) 10))
	 ((3) (string->number v))
	 (else ;; we truncate to millis
	  (string->number (substring v 0 3))))
       1000000))
  (define (parse-fraction&offset f&o?)
    (cond ((zero? (string-length f&o?)) (values 0 0))
	  ((string-index f&o? #\Z) =>
	   (lambda (i) (values (fraction->nanosecond (substring f&o? 0 i)) 0)))
	  ((string-index f&o? (char-set #\- #\+)) =>
	   (lambda (i)
	     (let ((f (fraction->nanosecond (substring f&o? 0 i)))
		   (neg (eqv? (string-ref f&o? i) #\-))
		   (h&m (substring f&o? (+ i 1) (string-length f&o?))))
	       (let-values (((h m)
			     (cond ((string-index h&m #\:) =>
				    (lambda (i)
				      (values (substring h&m 0 i)
					      (substring h&m (+ i 1)
							 (string-length h&m)))))
				   (else (values h&m #f)))))
		 (let ((n (+ (* (string->number h) 3600)
			     (if m (string->number m) 0))))
		   (values f (if neg (- n ) n)))))))))
			       
  (let-values (((ymd rest)
		(cond ((string-index v +time-separators-set+) =>
		       (lambda (i)
			 (values (substring v 0 i) (substring v i len))))
		      (else (values v "")))))
    (if (= len (string-length ymd))
	(let-values (((y m d) (parse-ymd ymd)))
	  (make-date 0 0 0 0 d m y 0))
	(let-values (((hms rest)
		      (let ((hms-i (string-skip rest +time-separators-set+)))
			(cond ((string-index rest #\.) =>
			       (lambda (i)
				 (values (substring rest hms-i i)
					 (substring rest (+ i 1)
						    (string-length rest)))))
			      ((string-index rest (char-set #\Z #\+ #\-)) =>
			       (lambda (i)
				 (values (substring rest hms-i i)
					 (substring rest i
						    (string-length rest)))))
			      (else (values (substring rest hms-i
						       (string-length rest))
					    ""))))))
	  (let-values (((fraction offset)
			(parse-fraction&offset
			 (string-delete char-set:whitespace rest)))
		       ((h m s) (apply values
				       (map string->number
					    (string-tokenize hms
							     char-set:digit))))
		       ((y M d) (parse-ymd ymd)))
	    (make-date fraction s m h d M y offset))))))
;; TODO move it somewhere
(define (date->yaml-canonical-date d)
  (define (->iso-zone offset)
    (define o (abs offset))
    (let* ((h (div o 3600))
	   (m (- o (* h 3600))))
      (string-append (if (negative? offset) "-" "+")
		     (number->string h)
		     ":"
		     (number->string m))))
  (let ((iso-time (date->string d "~5"))
	(milli (div (date-nanosecond d) 1000000))
	(offset (date-zone-offset d)))
    (string-append iso-time
		   (if (zero? milli)
		       ""
		       (string-append "." (number->string milli)))
		   (if (zero? offset)
		       "Z"
		       (->iso-zone offset)))))
(define (->date-string v) (date->yaml-canonical-date (->date v)))

;; (node builder) -> list
(define (->sequence seq builder) (map builder (yaml-node-value seq)))
;; (node builder) -> vector
(define (generic-mapping mapping key-builder value-builder)
  (define (flatten-mapping value)
    (define (handle-merge k v acc)
      (cond ((yaml-mapping-node? v) (append (flatten-mapping v) acc))
	    ((yaml-sequence-node? v)
	     (append (append-map (lambda (v)
				   (unless (yaml-mapping-node? v)
				     (yaml-builder-error v
				      "While building a mapping"
				      "Expected a mapping for merging"))
				   (flatten-mapping v))
				 (yaml-node-value v))
		     acc))
	    (else
	     (yaml-builder-error v
	      "While building a mapping"
	      "Expected a mapping or list of mappings for merging"))))
    (fold-right (lambda (k&v acc)
		  (let ((k (car k&v))
			(v (cdr k&v)))
		    (if (string=? (yaml-node-tag k) +yaml-tag:merge+)
			(handle-merge k v acc)
			(cons (cons k v) acc))))
		'() (yaml-node-value value)))
  
  (let ((value (flatten-mapping mapping)))
    (delete-duplicates!
     (map (lambda (k&v)
	    (cons (key-builder (car k&v)) (value-builder (cdr k&v)))) value)
     (lambda (x y) (equal? (car x) (car y))))))

(define (->mapping mapping builder)
  (list->vector (generic-mapping mapping builder builder)))
(define (->set mapping builder)
  (list->vector (generic-mapping mapping builder (lambda (_) 'null))))

(define (->pairs pairs builder)
  (map (lambda (m)
	 (let ((v (car (yaml-node-value m))))
	   (list (builder (car v)) (builder (cdr v)))))
       (yaml-node-value pairs)))

(define (list-of-node? v)
  (and (list? v) (for-all yaml-node? v)))
(define (list-of-pair? v)
  (and (list? v) (for-all (lambda (k&v)
			    (and (pair? k&v)
				 (yaml-node? (car k&v))
				 (yaml-node? (cdr k&v)))) v)))
(define (list-of-single-valued-mapping? v)
  (and (list? v) (for-all (lambda (m)
			    (and (mapping-node? m)
				 (= (length (yaml-node-value m)) 1))) v)))

(define (entry pred tag builder) (cons* pred tag builder))
(define (single-valued p) (lambda (node builders) (p (yaml-node-value node))))
(define (single-entry pred tag p) (cons* pred tag (single-valued p)))
(define (regexp-pred pred sre)
  (define re (regexp sre))
  (lambda (node)
    (and (pred node)
	 (regexp-matches re (yaml-node-value node)))))
(define (value-pred pred value-pred)
  (lambda (node)
    (and (pred node)
	 (value-pred (yaml-node-value node)))))
(define (add-yaml-builder-entry base . entry*) (append base entry*))
(define (replace-yaml-builder-entry base . entry*)
  (let* ((tags (map cadr entry*))
	 (l (remp (lambda (e) (member (cadr e) tags string=?)) base)))
    (append l entry*)))

(define sequence-node? (value-pred yaml-sequence-node? list-of-node?))
(define mapping-node? (value-pred yaml-mapping-node? list-of-pair?))
(define pairs-node?
  (value-pred yaml-sequence-node? list-of-single-valued-mapping?))
(define omap-node? pairs-node?)

;; explicit tag should allow float without fraction
(define float-value
  `(or ,+yaml-regexp:float+
       ;; integer 10 base
       (: (? ("-+")) (or #\0 (: (/ "19") (* ("_9876543210")))))))

(define +json-compat-yaml-builders+
  `(
    ,(single-entry yaml-scalar-node? +yaml-tag:null+ (lambda (_) 'null))
    ,(single-entry yaml-scalar-node? +yaml-tag:str+ values)
    ,(single-entry yaml-scalar-node? +yaml-tag:value+ values)
    ,(single-entry yaml-scalar-node? +yaml-tag:binary+ ->binary)
    ,(single-entry (regexp-pred yaml-scalar-node? +yaml-regexp:bool+)
		   +yaml-tag:bool+ ->bool)
    ,(single-entry (regexp-pred yaml-scalar-node? +yaml-regexp:int+)
		   +yaml-tag:int+ ->int)
    ,(single-entry (regexp-pred yaml-scalar-node? float-value)
		   +yaml-tag:float+ ->float)
    ,(single-entry (regexp-pred yaml-scalar-node? +yaml-regexp:timestamp+)
		   +yaml-tag:timestamp+ ->date-string)
    ,(entry sequence-node? +yaml-tag:seq+ ->sequence)
    ,(entry mapping-node? +yaml-tag:map+ ->mapping)
    ,(entry pairs-node? +yaml-tag:pairs+ ->pairs)
    ,(entry omap-node? +yaml-tag:omap+ ->pairs) ;; lazy
    ,(entry mapping-node? +yaml-tag:set+ ->set)
    ))

(define +default-yaml-builders+ +json-compat-yaml-builders+)

;;; convert to scheme object
(define (->hashtable mapping builder)
  (let ((ht (make-hashtable equal-hash equal?)))
    (for-each (lambda (kv) (hashtable-set! ht (car kv) (cdr kv)))
	      (generic-mapping mapping builder builder))
    ht))
(define (->vector seq builder) (list->vector (->sequence seq builder)))
(define (->pair-vector pairs builder)
  (vector-map (lambda (v) (cons (vector-ref v 0) (vector-ref v 1)))
	      (->pairs pairs builder)))
(define (->srfi-set mapping builder)
  (apply set default-comparator
	 (map car (generic-mapping mapping builder values))))

(define +scheme-object-yaml-builders+
  (replace-yaml-builder-entry +default-yaml-builders+
    (single-entry (regexp-pred yaml-scalar-node? +yaml-regexp:timestamp+)
		  +yaml-tag:timestamp+ ->date)
    (entry mapping-node? +yaml-tag:map+ ->hashtable)
    (entry sequence-node? +yaml-tag:seq+ ->vector)
    (entry sequence-node? +yaml-tag:pairs+ ->pair-vector)
    (entry mapping-node? +yaml-tag:set+ ->srfi-set)
    ;; TODO omap, use SRFI-146 mapping?
    ))

)
