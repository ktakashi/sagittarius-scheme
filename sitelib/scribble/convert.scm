;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; scribble/convert.scm - collection of scribble converters
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

;; for now we only need ->sxml
(library (scribble convert)
    (export scribble->sxml
	    (rename (process-1 scribble->sxml-inner)
		    (parse-attribute scribble-parse-attribute)))
    (import (rnrs)
	    (rnrs eval)
	    (clos user)
	    (sagittarius)
	    (sagittarius io)
	    (sagittarius vm) ;; TODO it's better to make current-library subr
	    (scribble parser)
	    (scribble plugin)
	    (srfi :39 parameters))

  (define-generic scribble->sxml)
  (define-method scribble->sxml ((in <port>))
    (scrib->sxml (scribble-parse in)))

  (define-method scribble->sxml ((in <string>))
    (call-with-input-string in
      (lambda (p)
	(scrib->sxml (scribble-parse p)))))

  (define (process-1 item)
    (with-exception-handler
     (lambda (e)
       (display (describe-condition e) (current-error-port))
       (newline (current-error-port))
       (if (warning? e) (condition-message e) (raise e)))
     (lambda ()
       (if (pair? item)
	   (cond ((scribble-lookup-macro (car item))
		  => (lambda (proc)
		       (apply proc (cdr item))))
		 (else (apply generic-processor (car item) (cdr item))))
	   item))))

  (define (scrib->sxml sexp)
    (define *top* '*TOP*)
    (define *pi*  '(*PI* xml "version=\"1.0\" encoding=\"utf-8\""))
    (append (list *top* *pi*)
	    (list `(scribble
		    ,@(map process-1 sexp)))))

  ;; default expanders
  (define-scribble-macro (include-section file)
    (if (file-exists? file)
	(call-with-input-file file
	  (lambda (in)
	    `(included ,@(map process-1 (scribble-parse in)))))
	(raise-continuable
	 (condition (make-warning)
		    (make-who-condition 'include-section)
		    (make-message-condition (format "file not found [~a]" file))
		    (make-irritants-condition file)))))

  (define (parse-attribute items)
    (let loop ((items items)
	       (attr '()))
      (cond ((null? items)
	     (values attr '())) ;; should not happen
	    ((and (pair? items)
		  (keyword? (car items)))
	     (loop (cddr items)
		   (acons (keyword->symbol (car items))
			  (list (cadr items))
			  attr)))
	    (else
	     (values attr items)))))

  (define (generic-processor tag . items)
    (let-values (((attr contents) (parse-attribute items)))
      (if (null? attr)
	  `(,tag ,@(map process-1 contents))
	  `(,tag (@ ,@attr) ,@(map process-1 contents)))))

;;  The same as generic-processor
;;  (define-scribble-plugin (table-of-contents . maybe-id)
;;    (let ((i (if (and (not (null? maybe-id))
;;		      (pair? (cdr maybe-id))
;;		      (eq? (car maybe-id) :id))
;;		 (cadr maybe-id)
;;		 #f)))
;;      (if i
;;	  `(table-of-contents (@ (id ,i)))
;;	  `(table-of-contents))))

  (define-scribble-plugin (secref t . args)
    `(secref (@ (tag ,t)) ,@(process-1 args)))

  (define-scribble-plugin (codeblock . args)
    (let ((e (if (eq? (car args) '=>)
		 (format "~a" (cadr args))
		 #f)))
      (if e
	  `(codeblock (@ (result ,e)) ,@(process-1 (cddr args)))
	  `(codeblock ,@(process-1 args)))))


  (define-scribble-plugin (eval expr)
    (eval (read (open-string-input-port expr)) (vm-current-library)))

  ;; deprecated
  (define-scribble-plugin (snipet . args)
    (format (current-error-port)
	    "deprecated tag @snipet is appeared [args: ~s]~%" args)
    (let ((e (if (eq? (car args) '=>)
		 (format "~a" (cadr args))
		 #f)))
      (if e
	  `(snipet (@ (result ,e)) ,@(process-1 (cddr args)))
	  `(snipet ,@(process-1 args)))))

  )
