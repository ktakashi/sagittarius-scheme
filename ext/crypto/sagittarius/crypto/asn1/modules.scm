;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/asn1/modules.scm - ASN.1 module API
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

;; This library is basically separated from (sagittarius crypto asn1)
;; It only provides a macro which defines ASN.1 module, its reader and
;; encoder
#!nounbound
(library (sagittarius crypto asn1 modules)
    (export define-asn1-encodable of
	    asn1-object->asn1-encodable
	    ;; For now only these two, maybe we want to add
	    ;; asn1-choice and others as well
	    asn1-sequence
	    asn1-set)
    (import (rnrs)
	    (clos core)
	    (clos user)
	    (srfi :1 lists) ;; for concatenate!
	    (sagittarius)
	    (sagittarius crypto asn1 types))

(define-generic asn1-object->asn1-encodable)
(define-syntax of (syntax-rules ()))
;; we want to define ASN.1 module like this
;; e.g.
;; AlgorithmIdentifier
;; (define-asn1-encodable <algorithm-identifier>
;;   (asn1-sequence
;;    ((algorithm :type <der-object-identifier>)
;;     (parameter :type <asn1-encodable> :optional #t))))
;; 
;; TBSRequest
;; (define-asn1-encodable <tbs-request>
;;   (asn1-sequence
;;    ((version :type <der-integer> :tag 0 :explicit #t :default v1)
;;     (requestor-name :type <general-name> :tag 1 :explicit #t :optional #t)
;;                         not sure if we want sequence-of, or set-of...
;;     (request-list :type (sequence-of <request>))
;;     (request-extensions :type <extensions> :tag 2 :optional #t))))
(define-syntax define-asn1-encodable
  (syntax-rules (of)
    ((_ name (base-type (of spec ...) opts ...))
     (begin
       (define-asn1-encodable "emit-class" name
	 (base-type ((of :multiple #t
			 spec ...
			 :init-keyword :elements ;; default
			 :init-value '())) opts ...))
       (define-method write-object ((o name) p)
	 (asn1-generic-write name
			     (asn1-object-list->string (slot-ref o 'of)) p))))
    ((_ name (base-type ((slot spec* ...) ...) opts ...))
     (begin
       (define-asn1-encodable "emit-class" name
	 (base-type ((slot spec* ...) ...) opts ...))
       (define-method write-object ((o name) p)
	 (define (slots->list o)
	   (filter-map (lambda (s) (slot-ref o (slot-definition-name s)))
		       (class-slots name)))
	 (asn1-generic-write name
			     (asn1-object-list->string (slots->list o)) p))))
    ((_ "emit-class" name (base-type ((slot spec* ...) ...) opts ...))
     (begin
       (define-class name (<asn1-encodable>)
	 ((slot spec* ...
		;; needs to be after
		:init-keyword (symbol->keyword 'slot)
		;; for optional
		:init-value #f) ...))
       (define asn1-object->this
	 (make-asn1-object->asn1-encodable name (base-type predicate)))
       (define this->asn1-object
	 (make-asn1-encodable->asn1-object name
					   (base-type constructor opts ...)))
       (define-method asn1-object->asn1-encodable ((m (eql name))
						   (o <asn1-object>))
	 (asn1-object->this o))
       (define-method asn1-encodable->asn1-object ((o name) type)
	 (this->asn1-object o type))))))

;; <asn1-object> is always <asn1-encodable>, so this is fine
(define-method asn1-object->asn1-encodable (m (o <asn1-object>)) o)

;; dispatcher
(define-syntax predicate (syntax-rules ()))
(define-syntax constructor (syntax-rules ()))

(define-syntax asn1-sequence
  (syntax-rules (predicate constructor)
    ((_ predicate) ber-sequence?)
    ((_ constructor opts ...)
     (lambda (type) (if (eq? type 'der) make-der-sequence make-ber-sequence)))))
(define-syntax asn1-set
  (syntax-rules (predicate constructor)
    ((_ predicate) ber-set?)
    ((_ constructor opts ...)
     (lambda (type)
       (lambda (lis)
	 (if (eq? type 'der)
	     (make-der-set lis opts ...)
	     (make-ber-set lis opts ...)))))))

;; Internal APIs
(define ((make-asn1-object->asn1-encodable class type?) (o type?))
  (define (asn1-object->slots class o slots)
    (define (err reason)
      (error 'asn1-object->asn1-encodable
	     (format
	      "Given ASN.1 object can't be converted to an object of ~a: ~a"
	      (class-name class)
	      reason)
	     o))
    (define (entry->slot slot entry)
      (list (symbol->keyword (slot-definition-name slot)) entry))
    (define (optional? slot) (slot-definition-option slot :optional #f))
    (define (try-deserialize type entry optional?)
      (guard (e (else (if optional? #f (raise e))))
	(asn1-object->asn1-encodable type entry)))
    (define (entry->object entry type optional?)
      (cond ((is-a? entry type) entry)
	    ((and (asn1-collection? entry)
		  (subtype? type <asn1-encodable>)
		  (try-deserialize type entry optional?)))
	    (else #f)))
    (let loop ((entries (asn1-collection->list o)) (slots slots) (r '()))
      (cond ((and (null? entries) (null? slots))
	     ;; order doesn't matter ;)
	     (concatenate! r))
	    ((null? entries)
	     (if (or (null? slots) (for-all optional? slots))
		 (concatenate! r)
		 (err "missing field(s)")))
	    ((null? slots)
	     (if (null? entries) (concatenate! r) (err "too many fields")))
	    (else
	     (let* ((slot (car slots))
		    (entry (car entries))
		    (type (slot-definition-option slot :type <asn1-object>))
		    (tag (slot-definition-option slot :tag #f))
		    (optional? (slot-definition-option slot :optional #f)))
	       ;; check tag first
	       (cond (tag
		      (cond ((and (ber-tagged-object? entry)
				  (= (ber-tagged-object-tag-no entry) tag))
			     (let ((obj (ber-tagged-object-obj entry)))
			       (cond ((entry->object obj type optional?) =>
				      (lambda (obj)
					(loop (cdr entries) (cdr slots)
					      (cons (entry->slot slot obj) r))))
				     (else (err "Invalid tag object")))))
			    (optional? (loop entries (cdr slots) r))
			    (else (err "Missing tag object"))))
		     ((entry->object entry type optional?) =>
		      (lambda (obj)
			(loop (cdr entries) (cdr slots)
			      (cons (entry->slot slot obj) r))))
		     (optional? (loop entries (cdr slots) r))
		     (else (err "Unknown field"))))))))
  (let ((slots (class-slots class)))
    (if (and (null? (cdr slots))
	     (slot-definition-option (car slots) :multiple #f))
	(let ((key (slot-definition-option (car slots)
					   :init-keyword :elements))
	      (type (slot-definition-option (car slots) :type <asn1-object>)))
	  (make class
	    key (map (lambda (o) (asn1-object->asn1-encodable type o))
		     (asn1-collection->list o))))
	(apply make class (asn1-object->slots class o slots)))))


(define ((make-asn1-encodable->asn1-object class make-ctr) o type)
  (define (object->asn1-object o)
    (define (ensure-asn1-object o)
      (cond ((asn1-object? o) o)
	    ((asn1-encodable? o) (asn1-encodable->asn1-object o type))
	    (else o)))
    (let ((slots (class-slots (class-of o))))
      (if (and (null? (cdr slots))
	       (slot-definition-option (car slots) :multiple #f))
	  (map ensure-asn1-object (slot-ref o 'of))
	  (filter-map (lambda (s)
			(let ((tag (slot-definition-option s :tag #f))
			      (explicit?
			       (slot-definition-option s :explicit #f))
			      (obj (ensure-asn1-object
				    (slot-ref o (slot-definition-name s)))))
			  (and obj
			       (if tag
				   (make (if (eq? type 'der)
					     <der-tagged-object>
					     <ber-tagged-object>)
				     :tag-no tag :explicit? explicit? :obj obj)
				   obj))))
		      slots))))
  ((make-ctr type) (object->asn1-object o)))
  
)
