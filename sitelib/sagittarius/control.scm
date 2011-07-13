;; -*- mode: scheme; coding: utf-8; -*-
;; This file is a part of Sagittarius Scheme system.
#!compatible
(library (sagittarius control)
    (export define-optional
	    let-optionals*
	    get-optional
	    define-with-key
	    let-keywords
	    let-keywords*
	    begin0
	    let1
	    check-arg)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax)
	    (core misc)
	    (srfi :2 and-let*)
	    (srfi :26 cut)
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

  ;; from Gauche
  (define-syntax let-keywords
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((arg   (cadr form))
	     (specs (caddr form))
	     (body  (cdddr form)))
	 (%let-keywords-rec arg specs body (rename 'let) rename)))))

  (define-syntax let-keywords*
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((arg   (cadr form))
	     (specs (caddr form))
	     (body  (cdddr form)))
	 (%let-keywords-rec arg specs body (rename 'let) rename)))))

  ;; like Gauche's :key
  ;; (define-with-key (name arg0 arg1 :key key1 (key2 #f) :allow-other-keys opt)
  ;;   body ...)
  ;; ->
  ;; (define (name arg0 arg1 . optional)
  ;;   (let-keywords* optional (key1
  ;;                            (key2 #f)
  ;;                            . opt)
  ;;     body ...))
  (define-syntax define-with-key
    (er-macro-transformer
     (lambda (form rename compare)
       (define (process-specs ospecs)
	 (let loop ((specs ospecs)
		    (required '())
		    (keys '())
		    (key-appear? #f))
	   (define (finish restvar)
	     (values (reverse! required)
		     (reverse! keys)
		     restvar))
	   (cond ((null? specs) (finish (gensym "dummy")))
		 ((pair? specs)
		  (cond (key-appear?
			 (if (eq? (car specs) :allow-other-keys)
			     (finish (cadr specs))
			     (let ((name (car specs)))
			       (loop (cdr specs) required
				     (cons (if (variable? name) (list name (undefined)) name) keys)
				     #t))))
			((eq? (car specs) :key)
			 (loop (cdr specs) required keys #t))
			(else
			 (loop (cdr specs) (cons (car specs) required) keys #f))))
		 (else
		  (syntax-violation 'define-with-key
				    "spec must be proper list"
				    form
				    ospec)))))
       (let ((spec (cadr form))
	     (body (cddr form))
	     (opt  (gensym "opt")))
	 (receive (required keys rest) (process-specs (cdr spec))
	   `(define (,(car spec) ,@required . ,opt)
	      (,(rename 'let-keywords*) ,opt (,@keys
					      . ,rest)
	       ,@body)))))))
	 

  ;;(define-macro (let-keywords arg specs . body)
  ;;  (%let-keywords-rec arg specs body 'let))

  ;;(define-macro (let-keywords* arg specs . body)
  ;;  (%let-keywords-rec arg specs body 'let*))

  (define (%let-keywords-rec arg specs body %let rename)
    (define (triplet var&default)
      (or (and-let* ([ (list? var&default) ]
		     [var (unwrap-syntax (car var&default))]
		     [ (symbol? var) ])
	    (case (length var&default)
	      [(2) (values (car var&default)
			   (make-keyword var)
			   (cadr var&default))]
	      [(3) (values (car var&default)
			   (unwrap-syntax (cadr var&default))
			   (caddr var&default))]
	      [else #f]))
	  (and-let* ([var (unwrap-syntax var&default)]
		     [ (symbol? var) ])
	    (values var (make-keyword var) (undefined)))
	  (error 'let-keywords "bad binding form in let-keywords" var&default)))
    (define (process-specs specs)
      (let loop ((specs specs)
		 (vars '()) (keys '()) (defaults '()) (tmps '()))
	(define (finish restvar)
	  (values (reverse! vars)
		  (reverse! keys)
		  (reverse! defaults)
		  (reverse! tmps)
		  restvar))
	(cond [(null? specs) (finish #f)]
	      [(pair? specs)
	       (receive (var key default) (triplet (car specs))
		 (loop (cdr specs)
		       (cons var vars)
		       (cons key keys)
		       (cons default defaults)
		       (cons (gensym) tmps)))]
	      [else (finish (or specs #t))])))

    (let ((argvar (gensym "args")) (loop (gensym "loop"))
	  ;; we make a little bit hygine.
	  (_error (rename 'error))
	  (_undefined? (rename 'undefined?)))
      (receive (vars keys defaults tmps restvar) (process-specs specs)
	`(let ,loop ((,argvar ,arg)
		     ,@(if (boolean? restvar) '() `((,restvar '())))
		     ,@(map (cut list <> (undefined)) tmps))
	      (cond
	       [(null? ,argvar)
		(,%let ,(map (lambda (var tmp default)
			       `(,var (if (,_undefined? ,tmp) ,default ,tmp)))
			     vars tmps defaults)
		       ,@body)]
	       [(null? (cdr ,argvar))
		(,_error 'let-keywords "keyword list not even" ,argvar)]
	       [else
		(case (car ,argvar)
		  ,@(map (lambda (key)
			   `((,key)
			     (,loop (cddr ,argvar)
				    ,@(if (boolean? restvar)
					  '()
					  `(,restvar))
				    ,@(map (lambda (k t)
					     (if (eq? key k)
						 `(cadr ,argvar)
						 t))
					   keys tmps))))
			 keys)
		  (else
		   ,(cond [(eq? restvar #t)
			   `(,loop (cddr ,argvar) ,@tmps)]
			  [(eq? restvar #f)
			   `(begin
			      (,_error 'let-keywords "unknown keyword " (car ,argvar))
			      (,loop (cddr ,argvar) ,@tmps))]
			  [else
			   `(,loop
			     (cddr ,argvar)
			     (,(rename 'cons*) (car ,argvar) (cadr ,argvar) ,restvar)
			     ,@tmps)])))
		]))))
    )       


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
