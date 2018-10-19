;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/patch.scm - JSON Patch
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

;; reference:
;; RFC 6902: https://tools.ietf.org/html/rfc6902
#!nounbound
(library (text json patch)
    (export json-patcher

	    json-patch-error? json-patch-error-path
	    json-patch-path-not-found-error?
	    json-patch-illegal-type-error?
	    )
    (import (rnrs)
	    (text json pointer)
	    (text json parse)
	    (text json convert)
	    (srfi :1 lists)
	    (srfi :133 vectors)
	    (util vector)
	    (util hashtables)
	    (util flexible-vector))

(define (json-patcher patch)
  (define (->patcher patch)
    (fold-left (lambda (combined-patcher patch)
		 (let ((patcher (make-patcher patch)))
		   (lambda (json)
		     (combined-patcher (patcher json))))) values patch))
  (unless (list? patch)
    (assertion-violation 'json-patcher "A list is required" patch))
  (let ((patcher (->patcher patch)))
    (lambda (json)
      (let ((mutable-json (json->mutable-json json)))
	(patcher mutable-json)
	(mutable-json->json mutable-json)))))

(define-condition-type &json-patch &error
  make-json-patch-error json-patch-error?
  (path json-patch-error-path))
(define-condition-type &json-patch-path-not-found &json-patch
  make-json-patch-path-not-found-error json-patch-path-not-found-error?)
(define-condition-type &json-patch-illegal-type &json-patch
  make-json-patch-illegal-type-error json-patch-illegal-type-error?)

(define (json-patch-path-not-found-error path who message . irr)
  (raise (condition (make-json-patch-path-not-found-error path)
		    (make-who-condition who)
		    (make-message-condition message)
		    (make-irritants-condition irr))))
(define (json-patch-illegal-type-error path who message . irr)
  (raise (condition (make-json-patch-path-not-found-error path)
		    (make-who-condition who)
		    (make-message-condition message)
		    (make-irritants-condition irr))))

;; mutable json is an alternative form for sexp json to make modification
;; a json object easier
;; it uses hashtable for json object, and flexible array for array
(define (json->mutable-json json)
  (define (convert json)
    (cond ((vector? json)
	   (let ((ht (make-hashtable string-hash string=?)))
	     (vector-for-each (lambda (e)
				(hashtable-set! ht (car e) (convert (cdr e))))
			      json)
	     ht))
	  ((list? json) (list->flexible-vector (map convert json)))
	  (else json)))
  (case (*json-map-type*)
    ((vector) (convert json))
    ((alist) (convert (alist-json->vector-json json)))))
(define (mutable-json->json mutable-json)
  (define (convert mjson)
    (cond ((hashtable? mjson)
	   (list->vector
	    (hashtable-map (lambda (k v) (cons k (convert v))) mjson)))
	  ((flexible-vector? mjson)
	   (map convert (flexible-vector->list mjson)))
	  (else mjson)))
  (let ((json (convert mutable-json)))
    (case (*json-map-type*)
      ((vector) (convert json))
      ((alist) (convert (vector-json->alist-json json))))))

(define mutable-json-object? hashtable?)
(define mutable-json-array? flexible-vector?)
(define-record-type not-found)
(define +json-not-found+ (make-not-found))
(define (mutable-json-object-set! mj key value) (hashtable-set! mj key value))
(define (mutable-json-object-delete! mj key) (hashtable-delete! mj key))
(define (mutable-json-object-contains? mj key) (hashtable-contains? mj key))
(define (mutable-json-object-ref mj key)
  (hashtable-ref mj key +json-not-found+))
(define (mutable-json-not-found? o) (eq? +json-not-found+ o))
(define (mutable-json-array-set! mj key value)
  ;; TODO proper key?
  ;; TODO handle error case
  (flexible-vector-set! mj (string->number key) value))
(define (mutable-json-array-insert! mj key value)
  ;; TODO proper key?
  ;; TODO handle error case
  (flexible-vector-insert! mj (string->number key) value))
(define (mutable-json-array-delete! mj key)
  ;; TODO proper key?
  ;; TODO handle error case
  (flexible-vector-delete! mj (string->number key)))
(define (mutable-json-array-ref mj key)
  (flexible-vector-ref mj (string->number key)))
(define (mutable-json-array-size mj) (flexible-vector-size mj))

(define (key=? key) (lambda (e) (string=? (car e) key)))
(define op? (key=? "op"))
(define path? (key=? "path"))
(define value? (key=? "value"))
(define from? (key=? "from"))
(define (make-patcher patch)
  (define (err)
    (assertion-violation 'json-patcher "Invalid JSON patch command" patch))
  (define (find pred)
    (cond ((vector-find pred patch) => cdr)
	  (else (err))))
  (case (string->symbol (find op?))
    ((add) (make-add-command (find path?) (find value?)))
    ((remove) (make-remove-command (find path?)))
    ((replace) (make-replace-command (find path?) (find value?)))
    ;;((move) (make-move-command (find from?) (find path?)))
    ;;((copy) (make-copy-command (find from?) (find path?)))
    ;;((test) (make-test-command (find path?) (find value?)))
    (else (err))))

(define-syntax call-with-last-entry
  (syntax-rules ()
    ((_ name ?path ?object-handler ?array-handler)
     (let ((path ?path)
	   (object-handler ?object-handler)
	   (array-handler ?array-handler))
       (define tokens (parse-json-pointer path))
       (define (ile mutable-json)
	 (json-patch-illegal-type-error path
	  'name "Parent path to add is not a container"
	  (mutable-json->json mutable-json)))
       (define (pne mutable-json)
	 (json-patch-path-not-found-error path
	  'name "Parent node to add does not exist"
	  (mutable-json->json mutable-json)))
       (let-values (((first last-list)
		     (split-at! tokens (- (length tokens) 1))))
	 (define last (car last-list))
	 (lambda (mutable-json)
	   (let loop ((tokens first) (json mutable-json))
	     (cond ((null? tokens)
		    (cond ((mutable-json-object? json)
			   (object-handler last json mutable-json))
			  ((mutable-json-array? json)
			   (array-handler last json mutable-json))
			  (else (ile mutable-json))))
		   ((mutable-json-object? json)
		    (let ((e (mutable-json-object-ref json (car tokens))))
		      (if (mutable-json-not-found? e)
			  (pne mutable-json)
			  (loop (cdr tokens) e))))
		   ((mutable-json-array? json)
		    (let ((e (mutable-json-array-ref json (car tokens))))
		      (if (mutable-json-not-found? e)
			  (pne mutable-json)
			  (loop (cdr tokens) e))))
		   (else (ile mutable-json))))))))))

(define (make-add-command path value)
  (define mutable-value (json->mutable-json value))
  (call-with-last-entry add path
    (lambda (last json _)
      (mutable-json-object-set! json last mutable-value))
    (lambda (last json _)
      (mutable-json-array-insert! json last mutable-value))))

(define (make-remove-command path)
  (call-with-last-entry add path
    (lambda (last json _)
      (mutable-json-object-delete! json last))
    (lambda (last json _)
      (mutable-json-array-delete! json last))))

(define (make-replace-command path value)
  (define mutable-value (json->mutable-json value))
  (define (nsp mutable-json)
    (json-patch-path-not-found-error path
     'replace "no such path in target JSON document"
     (mutable-json->json mutable-json)))
  (call-with-last-entry add path
    (lambda (last json root-json)
      (if (mutable-json-object-contains? json last)
	  (mutable-json-object-set! json last mutable-value)
	  (nsp root-json)))
    (lambda (last json root-json)
      (if (< (mutable-json-array-size json) (string->number last))
	  (mutable-json-array-set! json last mutable-value)
	  (nsp root-json)))))

)
