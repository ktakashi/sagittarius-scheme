;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a146/mappings.scm - Mappings
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
(library (srfi :146 mappings)
    (export (rename (make-mapping mapping)) mapping-unfold
	    mapping/ordered mapping-unfold/ordered
	    mapping? mapping-contains? mapping-empty? mapping-disjoint?
	    mapping-ref mapping-ref/default mapping-key-comparator
	    mapping-adjoin mapping-adjoin!
	    mapping-set mapping-set!
	    mapping-replace mapping-replace!
	    mapping-delete mapping-delete!
	    mapping-delete-all mapping-delete-all!

	    mapping-intern mapping-intern!
	    mapping-update mapping-update!
	    mapping-update/default mapping-update!/default

	    mapping-pop mapping-pop!
	    mapping-search mapping-search!
	    mapping-size mapping-find mapping-count mapping-any? mapping-every?
	    mapping-keys mapping-values mapping-entries
	    mapping-map mapping-map->list mapping-for-each mapping-fold
	    mapping-filter mapping-filter!
	    mapping-remove mapping-remove!
	    mapping-partition mapping-partition!
	    mapping-copy mapping->alist alist->mapping alist->mapping!
	    alist->mapping/ordered alist->mapping/ordered!
	    mapping=? mapping<? mapping>? mapping<=? mapping>=?
	    mapping-union mapping-intersection mapping-difference mapping-xor
	    mapping-union! mapping-intersection!
	    mapping-difference! mapping-xor!

	    make-mapping-comparator
	    mapping-comparator
	    mapping-min-key mapping-max-key
	    mapping-min-value mapping-max-value
	    mapping-key-predecessor mapping-key-successor
	    mapping-range= mapping-range< mapping-range>
	    mapping-range<= mapping-range>=
	    mapping-range=! mapping-range<!
	    mapping-range>! mapping-range<=! mapping-range>=!

	    mapping-split
	    mapping-catenate mapping-catenate!
	    mapping-map/monotone mapping-map/monotone!
	    mapping-fold/reverse
	    comparator?)
    (import (rnrs)
	    (srfi :114 comparators)
	    (only (srfi :128) comparator-register-default!)
	    (only (sagittarius treemap) treemap-iterator)
	    (clos user)
	    (util treemap))

(define-record-type mapping
  (fields key-comparator tree)
  (protocol (lambda (p)
	      (lambda (comparator . args)
		(assert (comparator? comparator))
		(let ((m (make-rb-treemap/comparator comparator)))
		  (dopairs (k v args) (treemap-adjoin! m k v))
		  (p comparator m))))))
(define %tree mapping-tree)

(define-syntax dopairs
  (syntax-rules ()
    ((_ (k v lis) body ...)
     (let ((l lis))
       (do ((xs l (cddr xs)))
	   ((null? xs))
	 (when (null? (cdr xs))
	   (assertion-violation 'mapping "kv-list isn't even" l))
	 (let ((k (car xs)) (v (cadr xs)))
	   body ...))))))

(define (mapping-unfold p f g seed comparator)
  (assert (comparator? comparator))
  (let ((m (make-mapping comparator)))
    (do ((seed seed (g seed)))
	((p seed) m)
      (let-values (((k v) (f seed)))
	(mapping-adjoin! m k v)))))

(define mapping/ordered mapping)
(define mapping-unfold/ordered mapping-unfold)

(define (mapping-empty? m) (zero? (treemap-size (%tree m))))
(define (mapping-contains? m k) (treemap-contains? (%tree m) k))
(define (mapping-disjoint? m1 m2)
  (assert (mapping? m1))
  (assert (mapping? m2))
  (let ((t2 (%tree m2)))
    (treemap-seek (%tree m1)
		  (lambda (k _) (treemap-contains? t2 k))
		  (lambda (r k v) #f)
		  (lambda () #t))))

(define (mapping-fold kons knil m)
  (assert (mapping? m))
  (treemap-fold kons (%tree m) knil))

(define (mapping-copy m)
  (assert (mapping? m))
  (mapping-fold (lambda (k v m) (mapping-adjoin! m k v))
		(make-mapping (mapping-key-comparator m)) m))

(define (mapping-adjoin! m . args)
  (assert (mapping? m))
  (let ((t (%tree m))) (dopairs (k v args) (treemap-adjoin! t k v)))
  m)

(define (mapping-adjoin m . args)
  (assert (mapping? m))
  (apply mapping-adjoin! (mapping-copy m) args))

(define %unique (list #t))

(define (mapping-ref m key :optional (failure #f) (success values))
  (assert (mapping? m))
  (let ((v (treemap-ref (%tree m) key %unique)))
    (if (eq? v %unique)
	(if failure
	    (failure)
	    (error 'mapping-ref "Given key isn't in the mapping" m key))
	(success v))))

(define (mapping-ref/default m key default)
  (assert (mapping? m))
  (treemap-ref (%tree m) key default))

(define (mapping-set m . args)
  (assert (mapping? m))
  (apply mapping-set! (mapping-copy m) args))

(define (mapping-set! m . args)
  (assert (mapping? m))
  (let ((t (%tree m))) (dopairs (k v args) (treemap-set! t k v)))
  m)

(define (mapping-replace m k v)
  (assert (mapping? m))
  (mapping-replace! (mapping-copy m) k v))
(define (mapping-replace! m k v)
  (assert (mapping? m))
  (let ((t (%tree m))) (treemap-replace! t k v))
  m)

(define (mapping-delete m . keys) (mapping-delete-all m keys))
(define (mapping-delete! m . keys) (mapping-delete-all! m keys))

(define (mapping-delete-all m keys)
  (assert (mapping? m))
  (mapping-delete-all! (mapping-copy m) keys))

(define (mapping-delete-all! m keys)
  (assert (mapping? m))
  (let ((t (%tree m)))
    (for-each (lambda (k) (treemap-delete! t k)) keys)
    m))

(define (mapping-intern m k newval)
  (assert (mapping? m))
  (mapping-intern! (mapping-copy m) k newval))

(define (mapping-intern! m k newval)
  (assert (mapping? m))
  (let ((v (mapping-ref/default m k %unique)))
    (if (eq? v %unique)
	(let ((v (newval)))
	  (mapping-set! m k v)
	  (values m v))
	(values m v))))

(define (mapping-update m k updater . opts)
  (assert (mapping? m))
  (apply mapping-update! (mapping-copy m) k updater opts))

(define (mapping-update! m k updater :optional
			 (failure (lambda ()
				    (error 'mapping-update!
				     "Given mapping doesn't have key" m k)))
			 (success values))
  (assert (mapping? m))
  (let* ((v (mapping-ref/default m k %unique))
	 (v1 (if (eq? v %unique)
		 (updater (failure))
		 (updater (success v)))))
    (mapping-set! m k v1)))

(define (mapping-update/default m k updater default)
  (mapping-update m k updater (lambda () default)))

(define (mapping-update!/default m k updater default)
  (mapping-update! m k updater (lambda () default)))

(define (mapping-pop m . opt)
  (assert (mapping? m))
  (apply mapping-pop! (mapping-copy m) opt))

(define (mapping-pop! m :optional
		      (failure
		       (lambda ()
			 (error 'mapping-pop!
				"Can't pop from an empty map" m))))
  (assert (mapping? m))
  (let ((v (treemap-pop-first-entry! (%tree m))))
    (if v
	(values m (car v) (cdr v))
	(failure))))

(define (mapping-search m k failure success)
  (assert (mapping? m))
  (mapping-search! (mapping-copy m) k failure success))

(define (mapping-search! m k failure success)
  (assert (mapping? m))
  (let ((v (mapping-ref/default m k %unique)))
    (if (eq? v %unique)
	(failure (lambda (v o) (mapping-set! m k v) (values m o)) ;; insert
		 (lambda (o) (values m o))) ;; ignore
	(success k v
		 (lambda (k v o) (mapping-set! m k v) (values m o)) ;; update
		 (lambda (o) (mapping-delete! m k) (values m o)))))) ;; remove

(define (mapping-size m)
  (assert (mapping? m))
  (treemap-size (%tree m)))

(define (mapping-find pred m failure)
  (assert (mapping? m))
  (treemap-seek (%tree m) pred (lambda (r k v) (values k v)) failure))

(define (mapping-count pred m)
  (assert (mapping? m))
  (mapping-fold (lambda (k v c) (if (pred k v) (+ c 1) c)) 0 m))

(define (mapping-any? pred m)
  (assert (mapping? m))
  (treemap-seek (%tree m) pred (lambda (r k v) #t) (lambda () #f)))

(define (mapping-every? pred m)
  (assert (mapping? m))
  (treemap-seek (%tree m)
		(lambda (k v) (not (pred k v)))
		(lambda (r k v) #f) (lambda () #t)))

(define (mapping-keys m) (treemap-keys-list (%tree m)))
(define (mapping-values m) (treemap-values-list (%tree m)))
(define (mapping-entries m)
  (let ((t (%tree m)))
    (values (treemap-keys-list t) (treemap-values-list t))))

(define (mapping-map proc comparator m)
  (assert (mapping? m))
  (assert (comparator? comparator))
  (let ((r (make-mapping comparator)))
    (treemap-for-each (lambda (k v)
			(let-values (((k v) (proc k v)))
			  (mapping-set! r k v))) (%tree m))
    r))

(define (mapping-for-each proc m)
  (assert (mapping? m))
  (treemap-for-each proc (%tree m)))

(define (mapping-map->list proc m)
  (assert (mapping? m))
  (treemap-map proc (%tree m)))

(define (mapping-filter pred m)
  (assert (mapping? m))
  (mapping-filter! pred (mapping-copy m)))

(define (mapping-filter! pred m)
  (assert (mapping? m))
  (let-values (((keys values) (mapping-entries m)))
    (for-each (lambda (k v)
		(unless (pred k v)
		  (mapping-delete! m k))) keys values))
  m)

(define (mapping-remove pred m)
  (assert (mapping? m))
  (mapping-remove! pred (mapping-copy m)))
(define (mapping-remove! pred m)
  (assert (mapping? m))
  (mapping-filter! (lambda (k v) (not (pred k v))) m))

(define (mapping-partition pred m)
  (assert (mapping? m))
  (mapping-partition! pred (mapping-copy m)))

(define (mapping-partition! pred m)
  (assert (mapping? m))
  (let ((r (make-mapping (mapping-key-comparator m))))
    (let-values (((keys values) (mapping-entries m)))
      (for-each (lambda (k v)
		  (unless (pred k v)
		    (mapping-delete! m k)
		    (mapping-set! r k v))) keys values))
    (values m r)))

(define (alist->mapping comparator alist)
  (assert (comparator? comparator))
  (alist->mapping! (make-mapping comparator) alist))
(define (alist->mapping! m alist)
  (assert (mapping? m))
  (for-each (lambda (p) (mapping-adjoin! m (car p) (cdr p))) alist)
  m)

(define alist->mapping/ordered alist->mapping)
(define alist->mapping/ordered! alist->mapping!)

(define (mapping->alist m)
  (assert (mapping? m))
  (mapping-map->list cons m))

(define (%mapping-compare who v=? pred ms)
  (define (mapping-compare-as-sets m1 m2 value=?)
    (define eof (cons #f #f))
    (define kc1 (mapping-key-comparator m1))
    (define kc2 (mapping-key-comparator m2))
    (define (key-compare key1 key2) (comparator-compare kc1 key1 key2))
    (cond ((eq? m1 m2) 0)
	  ((not (eq? kc1 kc2)) #f)
	  (else
	   (let ((i1 (treemap-iterator (%tree m1)))
		 (i2 (treemap-iterator (%tree m2))))
	     (define (loop k1 v1 k2 v2 r)
               (if (eq? k1 eof)
		   (cond ((eq? k2 eof) r)
			 ((<= r 0) -1)
			 (else #f))
		   (if (eq? k2 eof)
		       (if (>= r 0) 1 #f)
		       (case (key-compare k1 k2)
			 ((0)  (and (value=? v1 v2)
				    (let-values (((k1 v1) (i1 eof))
						 ((k2 v2) (i2 eof)))
				      (loop k1 v1 k2 v2 r))))
			 ((-1) (and (>= r 0)
				    (let-values (((k1 v1) (i1 eof)))
				      (loop k1 v1 k2 v2 1))))
			 (else (and (<= r 0)
				    (let-values (((k2 v2) (i2 eof)))
				      (loop k1 v1 k2 v2 -1))))))))
	     (let-values (((k1 v1) (i1 eof))
			  ((k2 v2) (i2 eof)))
	       (loop k1 v1 k2 v2 0))))))
  (let loop ((ms ms))
    (cond ((null? (cdr ms)))
          ((mapping-compare-as-sets (car ms) (cadr ms) v=?)
           => (lambda (r) (and (pred r) (loop (cdr ms)))))
          (else #f))))

(define-syntax define-mapping-compare
  (syntax-rules ()
    ((_ name op)
     (define (name vcmp m . more)
       (assert (comparator? vcmp))
       (%mapping-compare 'name
			 (comparator-equality-predicate vcmp)
			 (lambda (x) (op x 0))
			 (cons m more))))))

(define-mapping-compare mapping=? =)
(define-mapping-compare mapping<? <)
(define-mapping-compare mapping<=? <=)
(define-mapping-compare mapping>? >)
(define-mapping-compare mapping>=? >=)

(define (mapping-union! m1 . more)
  (define (union-2 m1 m2)
    (assert (mapping? m1))
    (assert (mapping? m2))
    (mapping-for-each (lambda (k v) (mapping-adjoin! m1 k v)) m2)
    m1)
  (if (null? more)
      m1
      (apply mapping-union! (union-2 m1 (car more)) (cdr more))))
(define (mapping-union m1 . more)
  (apply mapping-union! (mapping-copy m1) more))

(define (mapping-intersection! m1 . more)
  (define (intersection-2 m1 m2)
    (assert (mapping? m1))
    (assert (mapping? m2))
    (for-each (lambda (k)
		(unless (mapping-contains? m2 k)
		  (mapping-delete! m1 k))) (mapping-keys m1))
    m1)
  (if (null? more)
      m1
      (apply mapping-intersection! (intersection-2 m1 (car more)) (cdr more))))
(define (mapping-intersection m1 . more)
  (apply mapping-intersection! (mapping-copy m1) more))

(define (mapping-difference! m1 . more)
  (define (difference-2 m1 m2)
    (assert (mapping? m1))
    (assert (mapping? m2))
    (mapping-for-each (lambda (k v) (mapping-delete! m1 k)) m2)
    m1)
  (if (null? more)
      m1
      (apply mapping-difference! (difference-2 m1 (car more)) (cdr more))))
(define (mapping-difference m1 . more)
  (apply mapping-difference! (mapping-copy m1) more))

(define (mapping-xor! m1 m2)
  (assert (mapping? m1))
  (assert (mapping? m2))
  (mapping-for-each (lambda (k v)
		      (if (mapping-contains? m1 k)
			  (mapping-delete! m1 k)
			  (mapping-set! m1 k v))) m2)
  m1)
(define (mapping-xor m1 m2) (mapping-xor! (mapping-copy m1) m2))

(define (mapping-min-key m)
  (assert (mapping? m))
  (let-values (((k v) (treemap-first-entry (%tree m)))) k))
(define (mapping-max-key m)
  (assert (mapping? m))
  (let-values (((k v) (treemap-last-entry (%tree m)))) k))

(define (mapping-min-value m)
  (assert (mapping? m))
  (let-values (((k v) (treemap-first-entry (%tree m)))) v))
(define (mapping-max-value m)
  (assert (mapping? m))
  (let-values (((k v) (treemap-last-entry (%tree m)))) v))

(define (mapping-key-predecessor m key failure)
  (define (wrapped) (values (failure) #f))
  (assert (mapping? m))
  (let-values (((k v) (treemap-lower-entry (%tree m) key wrapped))) k))
(define (mapping-key-successor m key failure)
  (define (wrapped) (values (failure) #f))
  (assert (mapping? m))
  (let-values (((k v) (treemap-higher-entry (%tree m) key wrapped))) k))

(define-syntax define-mapping-range
  (syntax-rules ()
    ((_ name! name op)
     (begin
       (define (name! m probe)
         (assert (mapping? m))
         (let ((c (mapping-key-comparator m)))
           (for-each (lambda (k)
		       (unless (op (comparator-compare c k probe) 0)
			 (mapping-delete! m k))) (mapping-keys m)))
         m)
       (define (name m probe)
         (name! (mapping-copy m) probe))))))

(define-mapping-range mapping-range=!  mapping-range=  =)
(define-mapping-range mapping-range<!  mapping-range<  <)
(define-mapping-range mapping-range<=! mapping-range<= <=)
(define-mapping-range mapping-range>!  mapping-range>  >)
(define-mapping-range mapping-range>=! mapping-range>= >=)

(define (mapping-split m probe)
  (assert (mapping? m))
  (values (mapping-range< m probe)
          (mapping-range<= m probe)
          (mapping-range= m probe)
          (mapping-range>= m probe)
          (mapping-range> m probe)))

(define (mapping-catenate! comparator m1 key value m2)
  (define (too-small key m)
    (error 'mapping-catenate! "Catenating key is too small " key m))
  (define (too-large key m)
    (error 'mapping-catenate! "Catenating key is too large" key m))
  (cond ((eq? comparator (mapping-key-comparator m1))
         ;; Reuse m1
         (when (and (not (mapping-empty? m1))
                    (>= (comparator-compare comparator
					    (mapping-max-key m1) key) 0))
           (too-small key m1))
         (mapping-set! m1 key value)
         (mapping-for-each (lambda (k v)
			     (when (<= (comparator-compare comparator k key) 0)
			       (too-large key m2))
			     (mapping-set! m1 k v))
			   m2)
         m1)
        ((eq? comparator (mapping-key-comparator m2))
         ;; Reuse m2
         (when (and (not (mapping-empty? m2))
                    (>= (comparator-compare comparator
					    (mapping-max-key m2) key) 0))
           (too-large key m2))
         (mapping-set! m2 key value)
         (mapping-for-each (lambda (k v)
			     (when (>= (comparator-compare comparator k key) 0)
			       (too-small key m1))
			     (mapping-set! m2 k v))
			   m1)
         m2)
        (else
         (let ((m (make-mapping comparator)))
           (mapping-set! m key value)
           (mapping-for-each
	    (lambda (k v)
	      (when (>= (comparator-compare comparator k key) 0)
                (too-small key m1))
              (mapping-set! m k v))
	    m1)
           (mapping-for-each
	    (lambda (k v)
              (when (<= (comparator-compare comparator k key) 0)
                (too-large key m2))
              (mapping-set! m k v))
            m2)
	   m))))
(define (mapping-catenate comparator m1 key value m2)
  ;; inefficient...
  (mapping-catenate! comparator (mapping-copy m1) key value (mapping-copy m2)))

(define mapping-map/monotone! mapping-map)
(define mapping-map/monotone mapping-map)

(define (mapping-fold/reverse kons knil m)
  (assert (mapping? m))
  (treemap-fold-reverse kons (%tree m) knil))

(define (make-mapping-compare value-comparator)
  (define (mapping-compare m1 m2)
    (define eof (cons #f #f))
    (define kc1 (mapping-key-comparator m1))
    (define kc2 (mapping-key-comparator m2))
    (define (key-compare key1 key2) (comparator-compare kc1 key1 key2))
    (unless (eq? kc1 kc2)
      (error 'mapping-comparator "Mapping key comparaters are not the same"
	     m1 m2))
    (cond ((eq? m1 m2) 0)
	  (else
	   (let ((i1 (treemap-iterator (%tree m1)))
		 (i2 (treemap-iterator (%tree m2))))
	     (define (loop k1 v1 k2 v2)
               (if (eq? k1 eof)
		   (if (eq? k2 eof) 0 -1)
		   (if (eq? k2 eof)
		       1
		       (case (key-compare k1 k2)
			 ((0)
			  (case (comparator-compare value-comparator v1 v2)
			    ((0) (let-values (((k1 v1) (i1 eof))
					      ((k2 v2) (i2 eof)))
				   (loop k1 v1 k2 v2)))
			    (else => values)))
			 (else => values)))))
	     (let-values (((k1 v1) (i1 eof))
			  ((k2 v2) (i2 eof)))
	       (loop k1 v1 k2 v2))))))
  mapping-compare)
(define (make-mapping-comparator value-comparator)
  (make-comparator mapping? #t (make-mapping-compare value-comparator) #f))

(define mapping-comparator (make-mapping-comparator default-comparator))

(define default-mapping-compare (make-mapping-compare default-comparator))
(define-method object-compare ((m1 mapping) (m2 mapping))
  (default-mapping-compare m1 m2))

;; for SRFI 128
(define dummy
  (comparator-register-default! mapping-comparator))
)
