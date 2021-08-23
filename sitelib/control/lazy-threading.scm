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
    (export lazy-chain
	    (rename (lazy-if lazy-chain-if)
		    (lazy-cond lazy-chain-cond)
		    (lazy-guard lazy-chain-guard))
	    _ => else force ;; for convenience
	    )
    (import (rnrs)
	    (scheme lazy))

;; inspired by SRFI-197
;; promise only supports single value return, so no need for multiple value
;; consideration ;)
(define-syntax lazy-chain
  (lambda (x)
    (define (next seed p step)
      (with-syntax (((t) (generate-temporaries '(t)))
		    (seed seed))
	(let loop ((out '()) (vars '()) (in step))
	  (syntax-case in (lazy-guard)
	    ((lazy-guard guard ...)
	     (with-syntax ((p p))
	       #'(lazy-guard seed p guard ...)))
	    ((u . rest)
	     (and (identifier? #'u) (free-identifier=? #'u p))
	     (loop (cons #'t out) (cons #'t vars) #'rest))
	    ((x . rest)
	     (loop (cons #'x out) vars #'rest))
	    (()
	     (with-syntax ((result (reverse out)))
	       (syntax-case vars ()
		 ;; discard the result
		 (() #'(begin (force seed) result))
		 ;; use the result
		 ;; (a _ _ _) => (let ((x (force seed))) (a x x x))
		 ((x . r) #'(let ((x (force seed))) result)))))
	    (v
	     (identifier? #'v)
	     #'(v (force seed)))))))
		   
    (syntax-case x ()
      ((_ seed (step ...) ...)
       #'(lazy-chain seed _ (step ...) ...))
      
      ((_ seed placeholder)
       (and (identifier? #'placeholder))
       #'(delay-force seed))
      
      ((_ seed placeholder step rest ...)
       (with-syntax ((n (next #'seed #'placeholder #'step)))
	 #'(lazy-chain n placeholder rest ...))))))

(define-syntax lazy-if
  (syntax-rules ()
    ((_ seed (pred ...) then)
     (lazy-if seed (pred ...) then (values #f)))
    ((_ seed (pred ...) then else)
     (lazy-if seed _ (pred ...) then else))
    ((_ seed placeholder pred then else)
     (delay-force
      (let ((v (force seed)))
	(if (force (lazy-chain v placeholder pred))
	    (lazy-chain v placeholder then)
	    (lazy-chain v placeholder else)))))))

(define-syntax lazy-cond
  (syntax-rules (=> else)
    ((_ "parse" seed p ((pred => exp)))
     (let ((v (force (lazy-chain seed p pred))))
       (if v
	   (lazy-chain v p exp))))
    ((_ "parse" seed p ((pred exp* ...)))
     (let ((v (force (lazy-chain seed p pred))))
       (if v
	   (lazy-chain seed p exp* ...))))
    
    ((_ "parse" seed p ((pred => exp) (else exp2* ...)))
     (let ((v (force (lazy-chain seed p pred))))
       (if v
	   (lazy-chain v p exp)
	   (lazy-chain seed p exp2* ...))))
    ((_ "parse" seed p ((pred exp* ...) (else exp2* ...)))
     (let ((v (force (lazy-chain seed p pred))))
       (if v
	   (lazy-chain seed p exp* ...)
	   (lazy-chain seed p exp2* ...))))
    
    ((_ "parse" seed p ((pred => exp) clause* ...))
     (let ((v (force (lazy-chain seed p pred))))
       (if v
	   (lazy-chain v p exp)
	   (lazy-cond "parse" seed p (clause* ...)))))
    ((_ "parse" seed p ((pred exp* ...) clause* ...))
     (let ((v (force (lazy-chain seed p pred))))
       (if v
	   (lazy-chain seed p exp* ...)
	   (lazy-cond "parse" seed p (clause* ...)))))
    
    ((_ seed (guard? exp ...) ...)
     (lazy-cond seed _ (guard? exp ...) ...))
    ((_ seed placeholder (guard? exp ...) ...)
     (delay-force
      (let ((v (force seed)))
	(lazy-cond "parse" v placeholder ((guard? exp ...) ...)))))))

(define-syntax lazy-guard
  (syntax-rules ()
    ((_ seed (exp ...) ...)
     (lazy-guard seed _ (exp ...) ...))
    ((_ seed placeholder exp rest ...)
     (delay-force
      (guard (e (else (lazy-chain e placeholder exp rest ...)))
	(force seed))))))

)
