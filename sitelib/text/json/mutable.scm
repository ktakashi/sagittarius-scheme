;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/mutable.scm - Mutable JSON representation
;;;  
;;;   Copyright (c) 2019  Takashi Kato  <ktakashi@ymail.com>
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
(library (text json mutable)
    (export json->mutable-json
	    mutable-json->json
	    mutable-json?
	    mutable-json-object?
	    mutable-json-object-set!
	    mutable-json-object-merge!
	    mutable-json-object-delete!
	    mutable-json-object-contains?
	    mutable-json-object-ref
	    mutable-json-not-found?
	    mutable-json-array?
	    mutable-json-array-set!
	    mutable-json-array-insert!
	    mutable-json-array-insert-back!
	    mutable-json-array-delete!
	    mutable-json-array-ref
	    mutable-json-array-size)
    (import (rnrs)
	    (text json) ;; for *json-map-type*...
	    ;; The srfi provides own make-hashtable and it'd be slower
	    ;; than buildin version.
	    (except (srfi :126 hashtables) make-hashtable)
	    (srfi :133 vectors)
	    (util vector)
	    (util hashtables)
	    (util flexible-vector))


;; mutable json is an alternative form for sexp json to make modification
;; a json object easier
;; it uses hashtable for json object, and flexible array for array
(define (json->mutable-json json)
  (case (*json-map-type*)
    ((vector) (->mutable-json json))
    ((alist) (->mutable-json (alist-json->vector-json json)))))
;; internal
(define (->mutable-json json)
  (cond ((vector? json) (make-mutable-json-object json))
	((list? json)   (make-mutable-json-array json))
	(else json)))

(define (mutable-json->json mutable-json)
  (define (convert mjson)
    (cond ((mutable-json-object? mjson)
	   (list->vector
	    (hashtable-map (lambda (k v) (cons k (convert v)))
			   (mutable-json-object-entries mjson))))
	  ((mutable-json-array? mjson)
	   (map convert
		(flexible-vector->list (mutable-json-array-elements mjson))))
	  ((lazy-mutable-json? mjson) (lazy-mutable-json-value mjson))
	  (else mjson)))
  (let ((json (convert mutable-json)))
    (case (*json-map-type*)
      ((vector) (convert json))
      ((alist) (convert (vector-json->alist-json json))))))

;; we are exporting mutable json so, make a proper type...
(define-record-type mutable-json)
(define-record-type lazy-mutable-json
  (parent mutable-json)
  (fields value)
  (protocol (lambda (n)
	      (lambda (j)
		;; we don't wrap mutable-json twice
		;; non-container can be as it is
		(if (or (mutable-json? j) (string? j) (number? j) (boolean? j))
		    j
		    ((n) j))))))
(define (realize-lazy-mutable-json lmj)
  (->mutable-json (lazy-mutable-json-value lmj)))
;; for my laziness
(define ensure-mutable-json make-lazy-mutable-json)

(define-record-type mutable-json-object
  (fields entries)
  (parent mutable-json)
  (protocol (lambda (n)
	      (lambda (json)
		(let ((ht (make-hashtable string-hash string=?)))
		  (vector-for-each
		   (lambda (e)
		     (hashtable-set! ht (car e) (ensure-mutable-json (cdr e))))
		   json)
		  ((n) ht))))))
(define-record-type mutable-json-array
  (fields elements)
  (parent mutable-json)
  (protocol (lambda (n)
	      (lambda (json)
		((n) (list->flexible-vector
		      (map (lambda (e) (ensure-mutable-json e)) json)))))))

(define-record-type not-found)
(define +json-not-found+ (make-not-found))
(define (mutable-json-object-set! mj key value)
  (hashtable-set! (mutable-json-object-entries mj) key
		  (ensure-mutable-json value)))
(define (mutable-json-object-merge! base-mj mj . mj*)
  (define (merge1 base mj)
    (hashtable-merge! (mutable-json-object-entries base)
		      (mutable-json-object-entries mj)))
  (define (err)
    (assertion-violation 'mutable-json-object-merge!
			 "Mutable JSON object requried" base-mj mj mj*))
  (unless (and (mutable-json-object? base-mj) (mutable-json-object? mj))
    (err))
  (cond ((null? mj*) (merge1 base-mj mj))
	((for-all mutable-json-object? mj*)
	 (merge1 base-mj mj)
	 (for-each (lambda (mj) (merge1 base-mj mj)) mj*))
	(else (err)))
  base-mj)
(define (mutable-json-object-delete! mj key)
  (hashtable-delete! (mutable-json-object-entries mj) key))
(define (mutable-json-object-contains? mj key)
  (hashtable-contains? (mutable-json-object-entries mj) key))
(define (mutable-json-object-ref mj key . maybe-default)
  (define default
    (if (null? maybe-default) +json-not-found+ (car maybe-default)))
  (define entries (mutable-json-object-entries mj))
  (define (unwrap mj key e)
    (if (lazy-mutable-json? e)
	(let ((v (realize-lazy-mutable-json e)))
	  (hashtable-set! entries key v)
	  v)
	e))
  (unwrap mj key (hashtable-ref entries key default)))
(define (mutable-json-not-found? o) (eq? +json-not-found+ o))
(define (mutable-json-array-set! mj key value)
  (flexible-vector-set! (mutable-json-array-elements mj) key
			(ensure-mutable-json value)))
(define (mutable-json-array-insert! mj key value)
  (flexible-vector-insert! (mutable-json-array-elements mj) key
			   (ensure-mutable-json value)))
(define (mutable-json-array-delete! mj key)
  (flexible-vector-delete! (mutable-json-array-elements mj) key))
(define (mutable-json-array-insert-back! mj value)
  (flexible-vector-insert-back! (mutable-json-array-elements mj)
				(ensure-mutable-json value)))

(define (mutable-json-array-ref mj key)
  (define elements (mutable-json-array-elements mj))
  (define (unwrap mj key e)
    (if (lazy-mutable-json? e)
	(let ((v (realize-lazy-mutable-json e)))
	  (flexible-vector-set! elements key v)
	  v)
	e))
  (unwrap mj key (flexible-vector-ref elements key)))
(define (mutable-json-array-size mj)
  (flexible-vector-size (mutable-json-array-elements mj)))

)
