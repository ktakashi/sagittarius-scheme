;; -*- scheme -*-
(library (sagittarius let-optionals*)
    (export let-optionals*
	    get-optional)
    (import (core)
	    (core syntax)
	    (sagittarius))

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

)