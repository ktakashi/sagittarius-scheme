;;; -*- Scheme -*-
;;;
;;; object.scm - object library
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; Gauche like ref, ~, ->string, ->integer and ->number
(library (sagittarius object)
    (export ref ~
	    ->string ->integer ->number)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (rnrs mutable-strings)
	    (sagittarius)
	    (clos user))
  ;; we don't support fallback for slot ref, since we don't have slot-bound?
  ;; procedure.
  (define-method ref ((o <top>) (slot <symbol>))
    (slot-ref o slot))
  (define-method (setter ref) ((o <top>) (slot <symbol>) value)
    (slot-set! o slot value))

  ;; hashtable
  ;; since 0.3.4, hashtable-ref's fallback is optional
  (define-method ref ((ht <hashtable>) key)
    (hashtable-ref obj key))
  (define-method (setter ref) ((ht <hashtable>) key value)
    (hashtable-set! obj key value))

  ;; sequences
  (define-method ref ((obj <list>) (index <integer>))
    (list-ref obj index))
  (define-method ref ((obj <string>) (index <integer>))
    (string-ref obj index))
  (define-method ref ((obj <vector>) (index <integer>))
    (vector-ref obj index))
  (define-method (setter ref) ((obj <list>) (index <integer>) val)
    (set-car! (list-tail obj index) val))
  (define-method (setter ref) ((obj <string>) (index <integer>) val)
    (string-set! obj index val))
  (define-method (setter ref) ((obj <vector>) (index <integer>) val)
    (vector-set! obj index val))

  ;; the same as in srfi 17 library, however we can not refer from here.
  (define (%getter-with-setter get set)
    (let ((proc (lambda x (apply get x))))
      (set! (setter proc) set)
      proc))
  ;; from Gauche.
  (define ~
    (%getter-with-setter
     (case-lambda
      ((obj selector) (ref obj selector))
      ((obj selector . more) (apply ~ (ref obj selector) more)))
     (case-lambda
      ((obj selector val) ((setter ref) obj selector val))
      ((obj selector selector2 . rest)
       (apply (setter ~) (ref obj selector) selector2 rest)))))

  ;; From R6RS ->... is standard identifier, so we can simply use this.
  (define-method ->string ((obj <string>)) obj)
  (define-method ->string ((obj <number>)) (number->string obj))
  (define-method ->string ((obj <symbol>)) (symbol->string obj))
  (define-method ->string ((obj <char>))   (string obj))
  (define-method ->string ((obj <top>))
    (call-with-string-output-port (lambda (o) (display obj o))))

  (define-method ->integer ((obj <integer>)) obj)
  (define-method ->integer ((obj <real>))   (round (exact obj)))
  (define-method ->integer ((obj <number>)) 0) ; complex numbers to 0
  (define-method ->integer ((obj <char>))   (char->integer obj))
  (define-method ->integer ((obj <top>))    (->integer (->number obj)))
  
  (define-method ->number ((obj <number>)) obj)
  (define-method ->number ((obj <string>)) (string->number obj))
  (define-method ->number ((obj <char>))   (char->integer obj))
  (define-method ->number ((obj <top>))    0)
)