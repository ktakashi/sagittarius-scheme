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
    (export define-asn1-encodable
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
  (syntax-rules ()
    ((_ name (base-type ((slot spec* ...) ...) opts ...))
     (begin
       (define-class name (<asn1-encodable>)
	 ((slot :init-keyword (symbol->keyword 'slot)
		:init-value #f ;; for optional
		spec* ...) ...))
       (define asn1-object->this
	 (make-asn1-object->asn1-encodable name (base-type predicate)))
       (define this->asn1-object
	 (make-asn1-encodable->asn1-object name
					   (base-type constructor opts ...)))
       (define-method asn1-object->asn1-encodable ((m (eql name))
						   (o <asn1-object>))
	 (asn1-object->this o))
       (define-method asn1-encodable->asn1-object ((o name) type)
	 (this->asn1-object o type))
       (define-method write-object ((o name) p)
	 (define (slots->list o)
	   (map (lambda (s) (slot-ref o (slot-definition-name s)))
		(class-slots name)))
	 (asn1-generic-write name
			     (asn1-object-list->string (slots->list o)) p))))))

;; dispatcher
(define-syntax predicate (syntax-rules ()))
(define-syntax constructor (syntax-rules ()))

(define-syntax asn1-sequence
  (syntax-rules (predicate constructor)
    ((_ predicate) ber-sequence?)
    ((_ constructor opts ...)
     (lambda (type) (if (eq? type 'der) make-der-sequence make-ber-sequence)))))
(define-syntax asn1-set
  (syntax-rules (predicate constructor)))

;; Internal APIs
(define ((make-asn1-object->asn1-encodable class type?) (o type?))
  (define (asn1-object->slots class o slots)
    (define (err)
      (error 'asn1-object->asn1-encodable
	     (format "Given ASN.1 object can't be converted to an object of ~a"
		     (class-name class))
	     o))
    (define (entry->slot slot entry)
      (list (symbol->keyword (slot-definition-name slot)) entry))
    (define (optional? slot) (slot-definition-option slot :optional #f))
    (let loop ((entries (asn1-collection->list o)) (slots slots) (r '()))
      (cond ((and (null? entries) (null? slots))
	     ;; order doesn't matter ;)
	     (concatenate! r))
	    ((null? entries)
	     (if (or (null? slots) (for-all optional? slots))
		 (concatenate! r)
		 (err)))
	    ((null? slots) (if (null? entries) (concatenate! r) (err)))
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
			       (unless (is-a? obj type) (err))
			       (loop (cdr entries) (cdr slots)
				     (cons (entry->slot slot obj) r))))
			    (optional? (loop entries (cdr slots) r))
			    (else (err))))
		     ((is-a? entry type)
		      (loop (cdr entries) (cdr slots)
			    (cons (entry->slot slot entry) r)))
		     (optional? (loop entries (cdr slots) r))
		     (else (err))))))))
  (apply make class (asn1-object->slots class o (class-slots class))))


(define ((make-asn1-encodable->asn1-object class make-ctr) o type)
  (define (object->asn1-object o)
    (define slots (class-slots (class-of o)))
    (map (lambda (s) (slot-ref o (slot-definition-name s))) slots))
  ((make-ctr type) (object->asn1-object o)))
  
)
