;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; peg/derived.scm - PEG syntax sugers et al
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

(library (peg derived)
    (export $bind $do $optional $repeat $sequence-of
	    $parameterize $if $when $unless $cond else
	    $peek-match
	    $eqv?)
    (import (rnrs)
	    (core inline)
	    (peg primitives)
	    (srfi :39 parameters))

(define ($$bind p f)
  (lambda (l)
    (let-values (((s v nl) (p l)))
      (if (parse-success? s)
	  ((f v) nl)
	  (values s v l)))))

(define-syntax $bind
  (lambda (x)
    (syntax-case x ()
      ((_ p? f?)
       #'(let ((p p?) (f f?))
	   (lambda (l)
	     (let-values (((s v nl) (p l)))
	       (if (parse-success? s)
		   ((f v) nl)
		   (values s v l))))))
      (k (identifier? #'k) #'$$bind))))

;; $do clause ... body
;;   clause := (var parser)
;;          |  (parser)
;;          |  parser
(define-syntax $do
  (syntax-rules ()
    ((_ body) body)
    ((_ (var parser) clause rest ...)
     ($bind parser (lambda (var) ($do clause rest ...))))
    ((_ (parser) clause rest ...)
     ($bind parser (lambda (_) ($do clause rest ...))))
    ((_ parser clause rest ...)
     ($bind parser (lambda (_) ($do clause rest ...))))))

(define ($$optional parser . maybe-fallback)
  (define fallback (if (null? maybe-fallback) #f (car maybe-fallback)))
  ($do (r ($many parser 0 1))
       ($return (if (null? r) fallback (car r)))))
(define-syntax $optional
  (lambda (x)
    (syntax-case x ()
      ((_ parser) #'($optional parser #f))
      ((_ parser fallback)
       #'($do (r ($many parser 0 1))
	      ($return (if (null? r) fallback (car r)))))
      (k (identifier? #'k) #'$$optional))))
       
(define-inline ($repeat parser n) ($many parser n n))

(define ($sequence-of preds)
  (apply $seq (map $satisfy preds)))

(define-syntax $parameterize
  (syntax-rules ()
    ((_ ((p c) ...) parser)
     (let ((e parser))
       (lambda (l)
	 (parameterize ((p c) ...)
	   (e l)))))))

(define-syntax $if
  (syntax-rules ()
    ((_ pred consequence alternative)
     (lambda (l)
       (if pred
	   (consequence l)
	   (alternative l))))))
(define-syntax $when
  (syntax-rules ()
    ((_ pred body)
     ($if pred body ($fail 'pred)))))
(define-syntax $unless
  (syntax-rules ()
    ((_ pred body)
     ($when (not pred) body))))

(define-syntax $cond
  (syntax-rules (else)
    ((_ "emit" ((pred val) ...))
     (lambda (input)
       (cond (pred (val input)) ...)))
    ((_ "collect" (p&c ...) ()) ($cond "emit" (p&c ...)))
    ((_ "collect" (p&c ...) ((else alternative)))
     (let ((tmp alternative))
       ($cond "emit" (p&c ... (else tmp)))))
    ((_ "collect" (p&c ...) ((pred consequence) rest ...))
     (let ((tmp consequence))
       ($cond "collect" (p&c ... (pred tmp)) (rest ...))))
    ((_  clause ...)
     ($cond "collect" () (clause ...)))))

(define-syntax $peek-match
  (syntax-rules ()
    ((_ parser consequence alternative)
     (let ((peek ($peek parser)))
       (lambda (input)
	 (let-values (((s v n) (peek input)))
	   (if (parse-success? s)
	       (consequence input)
	       (alternative input))))))))

(define-inline ($eqv? v) ($satisfy (lambda (c) (eqv? c v)) v))
)
