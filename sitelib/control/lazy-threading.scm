;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; control/lazy-theading.scm - Lazy Threading macro
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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

;; sort of threading macros for promise

#!nounbound
(library (control lazy-threading)
    (export lazy-chain lazy-if lazy-cond
	    lazy-guard
	    => else force ;; for convenience
	    )
    (import (rnrs)
	    (scheme lazy))

;; inspired by SRFI-197
;; promise only supports single value return, so no need for multiple value
;; consideration ;)
(define-syntax lazy-chain
  (lambda (x)
    (define (next k seed p step)
      (with-syntax (((t) (generate-temporaries '(t))))
	(let loop ((out '()) (vars '()) (in step))
	  (syntax-case in ()
	    ((u . rest)
	     (and (identifier? #'u) (free-identifier=? #'u p))
	     (loop (cons #'t out) (cons #'t vars) #'rest))
	    ((x . rest)
	     (loop (cons #'x out) vars #'rest))
	    (()
	     (with-syntax ((result (reverse out))
			   (seed seed))
	       (syntax-case vars ()
		 ;; discard the result
		 (() #'(begin (force seed) result))
		 ;; use the result
		 ;; (a _ _ _) => (let ((x (force seed))) (a x x x))
		 ((x . r) #'(let ((x (force seed))) result)))))
	    (v
	     (identifier? #'v)
	     (with-syntax ((seed seed))
	       #'(v (force seed))))))))
		   
    (syntax-case x ()
      ((_ seed (step ...) ...)
       #'(lazy-chain seed _ (step ...) ...))
      
      ((_ seed placeholder)
       (and (identifier? #'placeholder))
       #'(delay-force seed))
      
      ((k seed placeholder step rest ...)
       (with-syntax ((n (next #'k #'(delay-force seed) #'placeholder #'step)))
	 #'(lazy-chain n placeholder rest ...))))))
#;(define-syntax lazy-chain
  (syntax-rules (lazy-guard)
    ((_ "thread" seed) seed) ;; returns promise here ;)
    ((_ "thread" seed (lazy-guard exp) exp* ...)
     (lazy-chain "thread" (lazy-guard seed exp) exp* ...))
    ((_ "thread" seed exp exp* ...)
     (let ((v seed)
	   (proc exp))
       (lazy-chain "thread" (delay-force (proc (force v))) exp* ...)))
    ((_ seed exp* ...)
     (lazy-chain "thread" (delay-force seed) exp* ...))))

(define-syntax lazy-if
  (syntax-rules ()
    ((_ seed pred then) (lazy-if seed pred then (lambda (v) #f)))
    ((_ seed pred then else)
     (let ((p pred) (t then) (e else))
       (lazy-chain seed (lambda (v) (if (p v) (t v) (e v))))))))

(define-syntax lazy-cond
  (syntax-rules (=> else)
    ((_ "parse" seed ((pred exp* ...)))
     (let ((t (lambda (v) (lazy-chain v exp* ...))))
       (lazy-if seed pred t)))
    ((_ "parse" seed ((pred exp* ...) (else exp2* ...)))
     (let ((t (lambda (v) (lazy-chain v exp* ...)))
	   (e (lambda (v) (lazy-chain v exp2* ...))))
       (lazy-if seed pred t e)))
    ((_ "parse" seed ((pred exp* ...) clause* ...))
     (let ((t (lambda (v) (lazy-chain v exp* ...))))
       (lazy-if seed pred t (lazy-cond "parse" seed (clause* ...)))))
    ((_ seed clause* ...)
     (lazy-cond "parse" seed (clause* ...)))))

(define-syntax lazy-guard
  (syntax-rules ()
    ((_ seed exp)
     (let ((v seed)
	   (proc exp))
       (delay-force (guard (e (else (proc e))) (force v)))))))

)
