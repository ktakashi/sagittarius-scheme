;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; cache/apis.scm - Cache APIs
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (cache apis)
    (export <cache>
	    cache-put!
	    cache-get
	    cache-evict!
	    cache-clear!
	    cache-size
	    cache-values

	    ;; internal APIs
	    cache-pop!
	    cache-access
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (srfi :114 comparators)
	    (only (srfi :126 hashtables) 
		  hashtable-pop! hashtable-walk hashtable-lookup))

;; base class of <cache>
;; storage is a hashtable, seems faster than treemap
;; ref: http://timday.bitbucket.org/lru.html
;;
;; NB: it's a bit too dangerous to assume hashtable is always faster than
;;     treemap but we don't have generic interface so for now it's fine.
(define-class <cache> ()
  (storage ;; hashtable
   (comparator :init-keyword :comparator :init-value default-comparator)
   (max-size :init-keyword :max-size :init-value +inf.0)
   (on-evict :init-keyword :on-evict :init-value #f)))

(define-method initialize ((o <cache>) initargs)
  (call-next-method)
  (let ((s (slot-ref o 'max-size))
	(c (slot-ref o 'comparator)))
    (unless (number? s) 
      (asserion-violation 'make-cache "max-size must be a number" s))
    (slot-set! o 'storage (make-hashtable/comparator c (if (fixnum? s) s 10))))
  o)

(define (call-on-evict cache v)
  (and-let* ((on-evict (slot-ref cache 'on-evict)))
    (on-evict v))
  v)

;; APIs
(define (cache-put! cache k v)
  (let ((size (cache-size cache)))
    (when (> (+ size 1) (slot-ref cache 'max-size)) 
      (call-on-evict cache (cache-pop! cache))))
  (hashtable-set! (slot-ref cache 'storage) k v)
  (cache-access cache :put k v))
(define (cache-get cache k :optional (fallback #f))
  (let-values (((v found?) (hashtable-lookup (slot-ref cache 'storage) k)))
    (if found?
	(begin
	  (cache-access cache :get k v)
	  v)
	fallback)))

(define (cache-size cache)
  (hashtable-size (slot-ref cache 'storage)))

(define (cache-values cache)
  (hashtable-values-list (slot-ref cache 'storage)))

(define-method cache-evict! ((o <cache>) k)
  (let ((storage (slot-ref o 'storage)))
    (let-values (((v found?) (hashtable-lookup storage k)))
      (hashtable-delete! storage k)
      (and found? (call-on-evict o v)))))

(define-method cache-clear! ((o <cache>))
  (for-each (lambda (key) (cache-evict! o key))
	    (hashtable-keys-list (slot-ref o 'storage))))

;; internal APIs
;; To make thing works fine, override this.
(define-method cache-pop! ((o <cache>)) 
  (let-values (((k v) (hashtable-pop! (slot-ref o 'storage)))) v))
(define-method cache-access ((o <cache>) on k v) #t)
)
