;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/parameters.scm - parameter library
;;;  
;;;   Copyright (c) 2000-2013  Takashi Kato  <ktakashi@ymail.com>
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

;; to make current-dynamic-environment weak-hashtable
(library (sagittarius parameters)
    (export make-parameter parameterize)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (only (sagittarius) current-dynamic-environment))
#|
  (define-class <parameter> ()
    ((converter :init-keyword :converter)))

  (define-method object-apply ((p <parameter>))
    (~ (current-dynamic-environment) p))
  (define-method object-apply ((p <parameter>) v)
    (let ((conv (~ p 'converter)))
      (if conv
	  (set! (~ (current-dynamic-environment) p) (conv v))	  
	  (set! (~ (current-dynamic-environment) p) v))))

  (define (make-parameter init :optional (converter #f))
    (let ((p (make <parameter> :converter converter))
	  (init (if converter (converter init) init)))
      ;; to keep parameter thread local
      (set! (~ (current-dynamic-environment) p) init)
      p))
|#
  (define (%parameter-value-set! p v)
    (if (is-a? p <parameter>)
	(set! (~ (current-dynamic-environment) p) v)
	(p v)))

  (define-syntax parameterize-aux
    (syntax-rules ()
      ((_ () ((save new param value) ...) body ...)
       (let ((save #f) ... (new value) ...)
         (dynamic-wind
	     (lambda () (set! save (param)) ... (param new) ...)
	     (lambda () body ...)
	     (lambda () (%parameter-value-set! param save) ...))))
      ((_ ((e1 e2) . more) (stash ...) body ...)
       (parameterize-aux more (stash ... (tmp1 tmp2 e1 e2)) body ...))))

  (define-syntax parameterize
    (syntax-rules ()
      ((_ ((e1 e2) ...) body ...)
       (parameterize-aux ((e1 e2) ...) () body ...))))

)