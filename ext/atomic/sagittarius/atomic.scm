;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/atomic - Basic Atomic library
;;;
;;;  Copyright (c) 2024  Takashi Kato <ktakashi@ymail.com>
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#!nounbound
(library (sagittarius atomic)
    (export memory-order?
	    *memory-order:relaxed*
	    *memory-order:consume*
	    *memory-order:acquire*
	    *memory-order:release*
	    *memory-order:acq-rel*
	    *memory-order:seq-cst*

	    make-atomic	        atomic?
	    make-atomic-pair    atomic-pair?
	    make-atomic-fixnum  atomic-fixnum?

	    atomic-load atomic-store!
	    atomic-fixnum-load atomic-fixnum-store!

	    atomic-fixnum-add! atomic-fixnum-sub!
	    atomic-fixnum-ior! atomic-fixnum-xor! atomic-fixnum-and!

	    atomic-exchange! atomic-fixnum-exchange!

	    atomic-compare-and-swap!

	    atomic-thread-fence
	    ;; utilities
	    atomic-fixnum-inc!
	    atomic-fixnum-dec!)
    (import (rnrs)
	    (sagittarius dynamic-module))
(load-dynamic-module "sagittarius--atomic")

(define (atomic-fixnum-inc! atomic . opts) 
  (apply atomic-fixnum-add! atomic 1 opts))
(define (atomic-fixnum-dec! atomic . opts)
  (apply atomic-fixnum-sub! atomic 1 opts))

)
	    

