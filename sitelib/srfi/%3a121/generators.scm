;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a121/generators.scm - Generators
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :121 generators)
    (export make-generator make-iota-generator make-range-generator 
	    make-coroutine-generator make-bits-generator make-port-generator
	    make-for-each-generator make-unfold-generator
	    

	    ;; re-exported
	    list->generator vector->generator
	    reverse-vector->generator string->generator
	    bytevector->generator 
	    gcons* gappend gcombine gfilter gremove 
	    gtake gdrop gtake-while gdrop-while
	    gdelete gdelete-neighbor-dups gindex gselect

	    generator->list generator->reverse-list
	    generator->vector generator->vector!  generator->string
	    generator-fold generator-for-each generator-find
	    generator-length generator-count generator-any generator-every
	    generator-unfold
	    )
    (import (rnrs)
	    (only (scheme base) read-line)
	    (sagittarius generators))

  (define (make-generator . args) (list->generator args))
  (define (make-iota-generator count . args) (apply giota count args))
  (define (make-range-generator start . args) (apply grange start args))
  (define make-coroutine-generator generate)
  (define (make-bits-generator n)
    (let ((k 0)
	  (len (bitwise-length n)))
      (lambda ()
	(if (= k len)
	    (eof-object)
	    (let ((set? (bitwise-bit-set? n k)))
	      (set! k (+ k 1))
	      set?)))))

  (define (make-port-generator p :optional (reader read-line))
    (lambda () (reader p)))

  (define (make-for-each-generator for-each coll)
    (generate (lambda (y) (for-each y coll))))

  (define (make-unfold-generator stop? mapper successor seed)
    (gunfold stop? mapper successor seed))
)
