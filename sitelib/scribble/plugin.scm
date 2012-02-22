;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; scribble/plugin.scm - scribble plugin mechanism
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

(library (scribble plugin)
    (export define-scribble-macro
	    define-scribble-plugin
	    scribble-lookup-macro
	    scribble-context-copy)
    (import (rnrs)
	    (clos user)
	    (srfi :39 parameters))

  (define-class <scribble-context> ()
    ((macros  :init-keyword :macros)))

  (define-method initialize ((c <scribble-context>) initargs)
    (call-next-method)
    (slot-set! c 'macros (make-eq-hashtable)))

  (define scribble-context (make-parameter (make <scribble-context>)))

  (define (register-macro! name expander :optional (context (scribble-context)))
    (hashtable-set! (slot-ref context 'macros) name expander))

  (define-syntax define-scribble-macro
    (syntax-rules (lambda)
      ((_ (name . args) body ...)
       (define-scribble-macro :lambda name (lambda args body ...)))
      ((_ name var)
       (define-scribble-macro :var name var))
      ((_ :var name (lambda args body ...))
       (define-scribble-macro :lambda name (lambda args body ...)))
      ((_ :var name var) ;; re-use the existed expander
       (register-macro! 'name (or (scribble-lookup-macro 'var)
				  (error "define-scribble-macro"
					 "unknown macro" 'var))))
      ((_ :lambda name var)
       (register-macro! 'name var))))

  (define-syntax define-scribble-plugin
    (syntax-rules ()
      ((_ spec body ...)
       (define-scribble-macro spec body ...))))

  (define (scribble-lookup-macro name :optional (context (scribble-context)))
    (hashtable-ref (slot-ref context 'macros) name #f))

  (define (scribble-context-copy :optional (context (scribble-context)))
    (make <scribble-context>
      :macros (hashtable-copy (slot-ref context 'macros) #t)))

)
