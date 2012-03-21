;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; shorten.scm - shorten lambda syntax
;;;  
;;;   Copyright (c) 2009-2012  Takashi Kato  <ktakashi@ymail.com>
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

#!compatible
#!nobacktrace
(library (shorten helper)
    (export constructor)
    (import (rnrs))

  (define (constructor args mark)
    (define (ctr arg)
      (syntax-case (list arg) ()
	((k)
	 (with-syntax ((name (datum->syntax
			      arg
			      (string->symbol
			       (string-append
				(symbol->string (syntax->datum arg))
				mark))))
		       (arg-name (datum->syntax 
				  arg
				  (string->symbol
				   (string-append
				    "^"
				    (symbol->string (syntax->datum arg))
				    mark)))))
	   (with-syntax ((formals (if (zero? (string-length mark))
				      (list #'name)
				      #'name)))
	     #`(define-syntax arg-name
		 (lambda (x)
		   (syntax-case x ()
		     ((k body (... ...))
		      (with-syntax ((name (datum->syntax #'k 'name)))
			(syntax (lambda formals body (... ...)))))))))
	   ))))
    (define (qs args r* k)
      (syntax-case args ()
	((arg args ...)
	 (qs #'(args ...) (cons (ctr #'arg) r*) k))
	(() (k r*))))
    (qs args '() (lambda (r) #`(begin #,@r))))
)

(library (shorten)
    (export ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n
	    ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z ^_ ^
	    ^a* ^b* ^c* ^d* ^e* ^f* ^g* ^h* ^i* ^j* ^k*
	    ^l* ^m* ^n* ^o* ^p* ^q* ^r* ^s* ^t* ^u* ^v* ^w*
	    ^x* ^y* ^z* ^_*
	    )
    (import (rnrs) (shorten helper))

  (define-syntax ^
    (syntax-rules ()
      ((_ args ...)
       (lambda args ...))))

  (define-syntax define-^
    (lambda (x)
      (syntax-case x ()
	((_ args ...)
	 (constructor #'(args ...) "")))))

  (define-syntax define-^*
    (lambda (x)
      (syntax-case x ()
	((_ args ...)
	 (constructor #'(args ...) "*")))))

  (define-^ _ a b c d e f g h i j k l m n o p q r s t u v w x y z)
  (define-^* _ a b c d e f g h i j k l m n o p q r s t u v w x y z)
  )
