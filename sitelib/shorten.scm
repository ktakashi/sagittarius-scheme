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
(library (shorten helper)
    (export %define-^)
    (import (rnrs))

  (define (%define-^ rename symbols type)
    (define (gen-name seed need^?)
      (let ((seed (string-append (if need^? "^" "") seed)))
	(if type
	    (string->symbol (string-append seed type))
	    (string->symbol seed))))
    (let ((_define-syntax (rename 'define-syntax))
	  (_lambda (rename 'lambda))
	  (_er-macro-transformer (rename 'er-macro-transformer))
	  (_match (rename 'match))
	  (_cons* (rename 'cons*))
	  (_form  (rename 'form)))
      (map (lambda (symbol)
	     (let* ((name (gen-name (symbol->string symbol) #t))
		    (args (gen-name (symbol->string symbol) #f))
		    (larg (if type args `(,args))))
	       `(,_define-syntax ,name
		  (,_er-macro-transformer
		   (,_lambda (,_form . ignore)
		     (,_match ,_form
		       ((_ args ...)
		        (,_cons*
		         ',_lambda ',larg args))))))))
	   symbols)))
)

(library (shorten)
    (export ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n
	    ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z ^_ ^
	    ^a* ^b* ^c* ^d* ^e* ^f* ^g* ^h* ^i* ^j* ^k*
	    ^l* ^m* ^n* ^o* ^p* ^q* ^r* ^s* ^t* ^u* ^v* ^w*
	    ^x* ^y* ^z* ^_*)
    (import (rnrs)
	    (match)
	    (shorten helper)
	    (sagittarius))

  (define-syntax ^
    (syntax-rules ()
      ((_ args ...)
       (lambda args ...))))

  (define-syntax define-^
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((syms (cdr form)))
	 `(,(rename 'begin)
	   ,@(%define-^ rename syms #f))))))

  (define-syntax define-^*
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((syms (cdr form)))
	 `(,(rename 'begin)
	   ,@(%define-^ rename syms "*"))))))

  (define-^ _ a b c d e f g h i j k l m n o p q r s t u v w x y z)
  (define-^* _ a b c d e f g h i j k l m n o p q r s t u v w x y z)
)
