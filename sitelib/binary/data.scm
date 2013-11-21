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
	    (clos core)
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

  ;; helper
  (define (gen-meta k name)
    (define (->syntax k s) (datum->syntax k (string->symbol s)))
    (let ((s (symbol->string (syntax->datum name))))
      (cond ((#/<(.+?)>/ s) =>
	     (lambda (m)
	       (->syntax k (string-append "<" (m 1) "-meta>"))))
	    (else
	     (->syntax k (string-append "<" s "-meta>"))))))

  (define-syntax define-simple-datum-define
    (lambda (x)
      (syntax-case x ()
	((_ define-name reader writer)
	 #'(define-syntax define-name
	     (lambda (xx)
	       (define (parse k fields fs ds)
		 (define (next name default)
		   (parse k (cdr fields) (cons name fs)
			  (cons (list name
				      :init-keyword (make-keyword 
						     (syntax->datum name))
				      :init-form default)
				ds)))
		 (syntax-case fields ()
		   (() (list (reverse! fs)  (reverse! ds)))
		   (((name) . rest)         (next #'name #f))
		   (((name default) . rest) (next #'name #'default))
		   ((name . rest) (identifier? #'name) (next #'name #f))
		   (_ (syntax-violation 'define-name "malformed fields" 
					(syntax->datum fields)))))
	       (syntax-case xx ()
		 ((k datum-class (parent-class (... ...))
		     (fields (... ...))
		     field-reader field-write
		     . options)
		  (with-syntax ((meta-class (gen-meta #'k #'datum-class))
				(parent-meta (get-keyword :parent-metaclass
							  #'options #'<class>))
				(((field (... ...)) (defs (... ...)))
				 (parse #'k #'(fields (... ...)) '() '())))
		    #'(begin
			;; should we add parent meta-class?
			(define-class meta-class (parent-meta) ())
			(define-class datum-class (parent-class (... ...))
			  (defs (... ...))
			  :metaclass meta-class)
			(define-method reader ((t meta-class) in . ignore)
			  (define fr field-reader)
			  (let ((o (make datum-class)))
			    (let-values ((params (fr in)))
			      (for-each (lambda (f p)(slot-set! o f p))
					'(field (... ...)) params)
			      o)))
			(define-method writer ((t meta-class) (o datum-class)
					       out . ignore)
			  (define fw field-write)
			  (apply fw out 
				 (map (lambda (s) (slot-ref o s))
				      '(field (... ...)))))
			(define-method write-object ((o datum-class) out)
			  (display "#<" out)
			  (display (slot-ref datum-class 'name) out)
			  (let ((fs '(field (... ...))))
			    (if (null? (cdr fs))
				(format out "~a:~s" (car fs)
					(slot-ref o (car fs)))
				(for-each (lambda (f)
					    (format out "~%  ~a:~s" f 
						    (slot-ref o f))) fs)))
			  (display ">" out))))))))))))

  (define-syntax define-composite-data-define
    (lambda (x)
      (syntax-case x ()
	((_ defined-name reader writer)
	 #'(define-syntax defined-name
	     (lambda (xx)
	       (define (parse-slots k slots r)
		 (define (make-slot-def name default)
		   (list name
			 :init-keyword (make-keyword (syntax->datum name))
			 :init-form default))
		 (define (parse-type type)
		   (syntax-case type ()
		     ((name size) #'(cons name size))
		     (k (or (identifier? #'k) (keyword? #'k)) #'(cons k #f))
		     (_ (syntax-violation 'defined-name "invalid type spec" 
					  type))))
		 (syntax-case slots ()
		   (((name type) . rest)
		    (parse-slots k (cdr slots)
				 (cons (list (make-slot-def #'name #f)
					     (parse-type #'type)) r)))
		   (((name type default) . rest)
		    (parse-slots k (cdr slots)
				 (cons (list (make-slot-def #'name #'default)
					     (parse-type #'type))
				       r)))
		   (() (reverse! r))))
	       (syntax-case xx ()
		 ((k data-name (parent (... ...)) (slots (... ...))
		     . options )
		  (with-syntax ((((slot-def types) (... ...)) 
				 (parse-slots #'k #'(slots (... ...)) '()))
				(meta-class (gen-meta #'k #'data-name))
				(parent-meta (get-keyword :parent-metaclass
							  #'options #'<class>)))
		    #'(begin
			(define-class meta-class (parent-meta) ())
			(define-class data-name (parent (... ...))
			  (slot-def (... ...))
			  :metaclass meta-class)
			(define-method writer ((t meta-class) (m data-name) out)
			  ;; the order is important
			  (map (lambda (slot type)
				 (let ((o (slot-ref m slot))
				       (t (car type))
				       (size? (cdr type)))
				   ;; should we do sanity check for array?
				   (if (and size? (vector? o))
				       (vector-for-each (lambda (v)
							  (writer t v out #f))
							o)
				       (writer t o out size?))))
			       (map car '(slot-def (... ...)))
			       (list types (... ...))))
			(define-method reader ((t meta-class) in . ignore)
			  (define (read-data type)
			    (let ((meta  (car type))
				  (size? (cdr type)))
			      ;; not a good solution but for now
			      (if (and size? (is-a? meta <class>))
				  (let ((v (make-vector size?)))
				    (do ((i 0 (+ i 1)))
					((= i size?) v)
				      (vector-set! v i (reader meta in #f))))
				  (reader meta in size?))))
			  (let ((o (make t)))
			    (for-each (lambda (slot type)
					(slot-set! o slot (read-data type)))
				      (map car '(slot-def (... ...)))
				      (list types (... ...)))
			    o))
			(define-method write-object ((o data-name) out)
			  (display "#<" out)
			  (display (slot-ref data-name 'name) out)
			  (newline out)
			  (for-each (lambda (slot)
				      (display "  " out)
				      (display slot out)
				      (display ":" out)
				      (write (slot-ref o slot) out)
				      (newline out))
				    (map slot-definition-name
					 (class-slots data-name)))
			  (display ">" out))))))))))))
  
)
