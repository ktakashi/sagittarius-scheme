;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sparse - sparse collection
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

;; for now only vector
(library (util sparse)
    (export <sparse-vector>
	    ;; API names are taken from Gauche
	    make-sparse-vector
	    sparse-vector?
	    sparse-vector-ref
	    sparse-vector-set!
	    sparse-vector-exists?
	    sparse-vector-delete!
	    sparse-vector-clear!
	    sparse-vector-update!
	    sparse-vector-size
	    sparse-vector-copy)
    (import (rnrs)
	    (clos user)
	    (sagittarius object)
	    (util treemap))

  ;; for now, it's just a treemap which keys are integer
  (define (integer-cmp a b)
    (cond ((< a b) -1)
	  ((> a b) 1)
	  (else 0)))
  (define-class <sparse-vector> (<sequence>)
    ((tree-map :init-keyword :tree-map ;; for copy
	       :init-form (make-rb-treemap integer-cmp))))

  (define (sparse-vector? o) (is-a? o <sparse-vector>))

  ;; helper
  (define (non-negative-exact-integer? o)
    (and (exact-integer? o) (not (negative? o))))
  ;; this must be defined in C ...
  (define (exact-integer? i) (and (integer? i) (exact? i)))

  (define-syntax check-type
    (syntax-rules ()
      ((_ who pred v)
       (or (pred v)
	   (assertion-violation who "wrong type" v)))))

  ;; for now
  (define (make-sparse-vector) (make <sparse-vector>))

  ;; getter setter
  (define (sparse-vector-ref spvec index :optional fallback)
    (check-type 'sparse-vector-ref sparse-vector? spvec)
    (check-type 'sparse-vector-ref non-negative-exact-integer? index)
    (treemap-ref (slot-ref spvec 'tree-map) index fallback))
  (define (sparse-vector-set! spvec index value)
    (check-type 'sparse-vector-set! sparse-vector? spvec)
    (check-type 'sparse-vector-set! non-negative-exact-integer? index)
    (treemap-set! (slot-ref spvec 'tree-map) index value))

  ;; update, delete and clear
  (define (sparse-vector-delete! spvec index)
    (check-type 'sparse-vector-delete! sparse-vector? spvec)
    (check-type 'sparse-vector-delete! non-negative-exact-integer? index)
    (treemap-delete! (slot-ref spvec 'tree-map) index))
  (define (sparse-vector-update! spvec index proc default)
    (check-type 'sparse-vector-delete! sparse-vector? spvec)
    (check-type 'sparse-vector-delete! non-negative-exact-integer? index)
    (treemap-update! (slot-ref spvec 'tree-map) index proc default))
  (define (sparse-vector-clear! spvec)
    (check-type 'sparse-vector-delete! sparse-vector? spvec)
    (treemap-clear! (slot-ref spvec 'tree-map)))

  ;; check
  (define (sparse-vector-exists? spvec index)
    (treemap-contains? (slot-ref spvec 'tree-map) index))
  
  ;; size
  (define (sparse-vector-size spvec)
    (treemap-size (slot-ref spvec 'tree-map)))

  ;; copy
  (define (sparse-vector-copy spvec)
    (make <sparse-vector> :tree-map (treemap-copy (~ spvec 'tree-map))))

  ;; for generic ref
  (define-method ref ((spvec <sparse-vector>) index)
    (sparse-vector-ref spvec index #f))
  (define-method ref ((spvec <sparse-vector>) index fallback)
    (sparse-vector-ref spvec index fallback))
  (define-method (setter ref) ((spvec <sparse-vector>) index value)
    (sparse-vector-set! spvec index value))

)