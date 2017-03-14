;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a147/custom-macro-transformers.scm - Custom macro transformsers
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

;; NB: the SRFI requires to use R7RS syntax-rules.
(library (srfi :147 custom-macro-transformers)
    (export (rename 
	     (srfi:define-syntax define-syntax)
	     (srfi:let-syntax	 let-syntax)
	     (srfi:letrec-syntax letrec-syntax))
	    syntax-rules)
    (import (scheme base)
	    (only (rnrs) identifier-syntax syntax-case syntax identifier?))

  (define-syntax maybe-identifier-syntax
    (lambda (x)
      (syntax-case x ()
	((_ keyword)
	 (identifier? #'keyword)
	 #'(identifier-syntax keyword))
	((_ transformer)
	 #'transformer))))
  
  (define-syntax srfi:define-syntax
    (syntax-rules ()
      ((_ name transformer)
       (define-syntax name (maybe-identifier-syntax transformer)))))

  (define-syntax srfi:let-syntax
    (syntax-rules ()
      ((_ ((var trans) ...) body ...)
       (let-syntax ((var (maybe-identifier-syntax trans)) ...) body ...))))

  (define-syntax srfi:letrec-syntax
    (syntax-rules ()
      ((_ ((var trans) ...) body ...)
       (letrec-syntax ((var (maybe-identifier-syntax trans)) ...) body ...))))

 )
