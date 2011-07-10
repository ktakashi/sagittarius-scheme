;; -*- mode: scheme; coding: utf-8; -*-
;; This file is a part of Sagittarius Scheme system.
(library (sagittarius control)
    (export define-optional
	    let-optionals*
	    get-optional
	    begin0
	    let1
	    check-arg)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax)
	    (sagittarius))

  (define-syntax define-optional
    (syntax-rules (optional)
      ((_ (name . bindings) . bodies)
       (define-optional "seek-optional" bindings () ((name . bindings) . bodies)))

      ((_ "seek-optional" ((optional . _opt-bindings))
	 (reqd ...) ((name . _bindings) . _bodies))
       (define (name reqd ... . _rest)
	 (letrec-syntax
	     ((handle-opts
	       (syntax-rules ()
		 ((_ rest bodies (var init))
		  (let ((var (if (null? rest) init
				 (if (null? (cdr rest)) (car rest)
				     (error 'name "extra rest" rest)))))
		    . bodies))
		 ((_ rest bodies var) (handle-opts rest bodies (var #f)))
		 ((_ rest bodies (var init) . other-vars)
		  (let ((var (if (null? rest) init (car rest)))
			(new-rest (if (null? rest) '() (cdr rest))))
		    (handle-opts new-rest bodies . other-vars)))
		 ((_ rest bodies var . other-vars)
		  (handle-opts rest bodies (var #f) . other-vars))
		 ((_ rest bodies)		; no optional args, unlikely
		  (let ((_ (or (null? rest) (error 'name "extra rest" rest))))
		    . bodies)))))
	   (handle-opts _rest _bodies . _opt-bindings))))

      ((_ "seek-optional" (x . rest) (reqd ...) form)
       (define-optional "seek-optional" rest (reqd ... x) form))

      ((_ "seek-optional" not-a-pair reqd form)
       (define . form))			; No optional found, regular define

      ((_ name body)		; Just the definition for 'name',
       (define name body))		; for compatibilibility with define
      ))
#|

 ;;; LET-OPTIONALS macros
 ;;; Copyright (c) 2001 by Olin Shivers.

 Copyright (c) 1993-2003 Richard Kelsey and Jonathan Rees
 Copyright (c) 1994-2003 by Olin Shivers and Brian D. Carlstrom.
 Copyright (c) 1999-2003 by Martin Gasbichler.
 Copyright (c) 2001-2003 by Michael Sperber.
 All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are
 permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of
      conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, this list
      of conditions and the following disclaimer in the documentation and/or other
      materials provided with the distribution.
   3. The name of the authors may not be used to endorse or promote products derived from
      this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
 INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT,
 INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

|#

  (define-syntax get-optional
    (syntax-rules ()
      ((_ rest default-exp)
       (let ((maybe-arg rest))
         (if (pair? maybe-arg)
             (if (null? (cdr maybe-arg)) (car maybe-arg)
                 (error 'get-optional "too many optional arguments" maybe-arg))
             default-exp)))

      ((_ rest default-exp arg-test)
       (let ((maybe-arg rest))
         (if (pair? maybe-arg)
             (if (null? (cdr maybe-arg))
                 (let ((val (car maybe-arg)))
                   (if (arg-test val) val
                       (error "Optional argument failed test"
                              'arg-test val)))
                 (error 'get-optional "too many optional arguments" maybe-arg))
             default-exp)))))

  (define-syntax let-optionals*
    (syntax-rules ()
      ((let-optionals* arg (opt-clause ...) body ...)
       (let ((rest arg))
         (%let-optionals* rest (opt-clause ...) body ...)))))

  (define-syntax %let-optionals*
    (syntax-rules ()
      ((%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
       (receive (rest var ...) (xparser arg)
	 (%let-optionals* rest (opt-clause ...) body ...)))

      ((%let-optionals* arg ((var default) opt-clause ...) body ...)
       (receive (var rest) (if (null? arg) (values default '())
			       (values (car arg) (cdr arg)))
	 (%let-optionals* rest (opt-clause ...) body ...)))

      ((%let-optionals* arg ((var default test) opt-clause ...) body ...)
       (receive (var rest) (if (null? arg) (values default '())
                               (let ((var (car arg)))
                                 (if test (values var (cdr arg))
                                     (error 'let-optionals* "arg failed LET-OPT test" var))))
	 (%let-optionals* rest (opt-clause ...) body ...)))

      ((%let-optionals* arg ((var default test supplied?) opt-clause ...) body ...)
       (receive (var supplied? rest)
	   (if (null? arg) (values default #f '())
	       (let ((var (car arg)))
		 (if test (values var #t (cdr arg))
		     (error 'let-optionals* "arg failed LET-OPT test" var))))
	 (%let-optionals* rest (opt-clause ...) body ...)))

      ((%let-optionals* arg (rest) body ...)
       (let ((rest arg)) body ...))

      ((%let-optionals* arg () body ...)
       (if (null? arg) (begin body ...)
           (error 'let-optionals* "Too many arguments in let-opt" arg)))))


  (define-syntax begin0
    (syntax-rules ()
      ((begin0 exp exp2 ...)
       (receive r exp exp2 ... (apply values r)))))
  
  ;; from Gauche
  (define-syntax let1
    (syntax-rules ()
      ((let1 var exp . body)
       (let ((var exp)) . body))))

  (define-syntax check-arg
    (syntax-rules ()
      ((_ pred val proc)
       (if (pred val)
           val
           (assertion-violation
            'proc
            (format "invalid argument, assertion failed in expression ~s" (list 'pred 'val))
            (list pred val))))))
)
