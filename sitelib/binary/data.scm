;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; binary/data.aux.scm - Binary data framework
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/regex
(library (binary data)
    (export define-simple-datum-define
	    define-composite-data-define
	    ;; utilities
	    put-u16 put-s16 get-u16 get-s16
	    put-u32 put-s32 get-u32 get-s32
	    put-u64 put-s64 get-u64 get-s64
	    put-f32 put-f64 get-f32 get-f64)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius regex))

  (define-syntax define-put&get
    (lambda (x)
      (define (->syntax k s)
	(datum->syntax k (string->symbol s)))
      (define (make-put&get k type)
	(let ((s (symbol->string (syntax->datum type))))
	  (list (->syntax k (string-append "bytevector-" s "-set!"))
		(->syntax k (string-append "bytevector-" s "-ref")))))
      (define (make-names k type)
	(let ((s (symbol->string (syntax->datum type))))
	  (list (->syntax k (string-append "put-" s))
		(->syntax k (string-append "get-" s)))))
      (define (get-size type)
	(let ((s (symbol->string (syntax->datum type))))
	  (div (string->number (string-copy s 1)) 8)))
      (syntax-case x ()
	((k type)
	 (with-syntax (((put get) (make-put&get #'k #'type))
		       ((pname gname) (make-names #'k #'type))
		       (size (get-size #'type)))
	   #'(begin
	       (define (pname out v endian)
		 (let ((buf (make-bytevector size)))
		   (put buf 0 v endian)
		   (put-bytevector out buf)))
	       (define (gname in endian)
		 (let ((buf (get-bytevector-n in size)))
		   (get buf 0 endian)))))))))

  (define-put&get u16)
  (define-put&get s16)
  (define-put&get u32)
  (define-put&get s32)
  (define-put&get u64)
  (define-put&get s64)
  ;; flonum needs to be treated differently ... 
  (define (put-f32 out v endian)
    (let ((buf (make-bytevector 4)))
      (bytevector-ieee-single-set! buf 0 v endian)
      (put-bytevector out buf)))
  (define (get-f32 in endian)
    (let ((buf (get-bytevector-n in 4)))
      (bytevector-ieee-single-ref buf 0 endian)))
  (define (put-f64 out v endian)
    (let ((buf (make-bytevector 8)))
      (bytevector-ieee-double-set! buf 0 v endian)
      (put-bytevector out buf)))
  (define (get-f64 in endian)
    (let ((buf (get-bytevector-n in 8)))
      (bytevector-ieee-double-ref buf 0 endian)))

  ;; structured data must be read/write invariance
  ;; thus we can define how it's read and written simultaneously.
  ;; all what we need is definition of simple (atom) data and
  ;; composite data

  ;; macro to define simple data definition macro
  ;; (define-simple-data-define name read write)
  ;; for example;
  ;; (define-simple-data-define define-sample read-sample write-sample)
  ;; this will define a macro can be used like this
  ;; (define-sample <name-list> (<sample-meta>)
  ;;   names names-reader names-writer)
  ;; simple datum can't have multiple data so it must have only one
  ;; slot.
  (define-syntax define-simple-datum-define
    (lambda (x)
      (syntax-case x ()
	((_ define-name reader writer)
	 #'(define-syntax define-name
	     (lambda (xx)
	       (define (->syntax k s) (datum->syntax k (string->symbol s)))
	       (define (gen-meta k name)
		 (let ((s (symbol->string (syntax->datum name))))
		   (cond ((#/<(.+?)>/ s) =>
			  (lambda (m)
			    (->syntax k (string-append "<" (m 1) "-meta>"))))
			 (else
			  (->syntax k (string-append "<" s "-meta>"))))))
				      
	       (syntax-case xx ()
		 ((k datum-class (parent-class (... ...))
		     field default field-reader field-write
		     . options)
		  (with-syntax ((meta-class (gen-meta #'k #'datum-class))
				(parent-meta (get-keyword :parent-metaclass
							  #'options #'<class>)))
		    #'(begin
			;; should we add parent meta-class?
			(define-class meta-class (parent-meta) ())
			(define-class datum-class (parent-class (... ...))
			  ((field :init-form default))
			  :metaclass meta-class)
			(define-method reader ((t meta-class) in . ignore)
			  (define fr field-reader)
			  (let ((o (make datum-class)))
			    (slot-set! o 'field (fr in))
			    o))
			(define-method writer ((t datum-class) out)
			  (define fw field-write)
			  (fw out (slot-ref t 'field)))
			(define-method write-object ((o datum-class) out)
			  (format out "#<~a ~a:~s>" 
				  (slot-ref datum-class 'name)
				  'field (slot-ref o 'field)))))))))))))

)
