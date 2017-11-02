;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a158/generators-and-accumulators.scm - Generators and Accumulators
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

(library (srfi :158 generators-and-accumulators)
    (export generator circular-generator make-iota-generator
	    make-range-generator make-coroutine-generator
	    list->generator vector->generator
	    reverse-vector->generator string->generator
	    bytevector->generator
	    make-for-each-generator make-unfold-generator
	    gcons* gappend gcombine gfilter gremove
	    gtake gdrop gtake-while gdrop-while
	    gflatten ggroup gmerge gmap gstate-filter
	    gdelete gdelete-neighbor-dups gindex gselect gpath
	    generator->list generator->reverse-list
	    generator->vector generator->vector!  generator->string
	    generator-fold generator-map->list generator-for-each generator-find
	    generator-count generator-any generator-every generator-unfold

	    make-accumulator count-accumulator list-accumulator
	    reverse-list-accumulator vector-accumulator
	    reverse-vector-accumulator vector-accumulator!
	    string-accumulator bytevector-accumulator bytevector-accumulator!
	    sum-accumulator product-accumulator)
    (import (rnrs)
	    (srfi :121 generators)
	    (srfi :133 vectors)
	    (sagittarius)
	    (sagittarius generators))

(define (ggroup gen k :optional padding)
  (if (undefined? padding)
      (gslices gen k)
      (gslices gen k #t padding)))

(define (generator-map->list proc gen . opts)
  (generator->list (apply gmap proc gen opts)))

(define (make-accumulator kons knil finalizer)
  (lambda (v)
    (if (eof-object? v)
	(finalizer knil)
	(set! knil (kons v knil)))))

(define (count-accumulator)
  (make-accumulator (lambda (_ count) (+ 1 count)) 0 values))

(define (list-accumulator) (make-accumulator cons '() reverse!))

(define (reverse-list-accumulator) (make-accumulator cons '() values))
(define (vector-accumulator) (make-accumulator cons '() reverse-list->vector))
(define (reverse-vector-accumulator) (make-accumulator cons '() list->vector))
(define (vector-accumulator! vector at)
  (make-accumulator (lambda (v index)
		      (vector-set! vector index v)
		      (+ index 1))
		    at
		    (lambda (_) vector)))
(define (string-accumulator)
  (let-values (((out extract) (open-string-output-port)))
    (make-accumulator (lambda (c p) (put-char p c) p)
		      out
		      (lambda (_) (extract)))))
(define (bytevector-accumulator)
  (let-values (((out extract) (open-bytevector-output-port)))
    (make-accumulator (lambda (c p) (put-u8 p c) p)
		      out
		      (lambda (_) (extract)))))
(define (bytevector-accumulator! bv at)
  (make-accumulator (lambda (v index)
		      (bytevector-u8-set! bv index v)
		      (+ index 1))
		    at
		    (lambda (_) bv)))
(define (sum-accumulator) (make-accumulator + 0 values))
(define (product-accumulator) (make-accumulator * 1 values))
)
