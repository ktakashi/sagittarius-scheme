;; -*- mode: scheme; coding: utf-8; -*-
#!read-macro=sagittarius/regex
(library (sagittarius ffi)
    (export open-shared-library
	    lookup-shared-library
	    close-shared-library
	    shared-object-suffix
	    c-function
	    make-c-function ;; issue 83
	    pointer->c-function

	    c-callback
	    make-c-callback ;; in some cases, we want this as well
	    free-c-callback
	    callback?
	    ;; malloc
	    c-malloc
	    c-free
	    ;; finalizer
	    register-ffi-finalizer
	    unregister-ffi-finalizer
	    c-memcpy

	    ;; pointer
	    pointer?
	    integer->pointer
	    uinteger->pointer
	    pointer->integer
	    pointer->uinteger

	    object->pointer
	    pointer->object
	    allocate-pointer
	    set-pointer-value!

	    ;; c-struct
	    define-c-struct
	    define-c-union
	    c-struct?
	    allocate-c-struct
	    size-of-c-struct
	    c-struct-ref
	    c-struct-set!
	    describe-c-struct

	    ;; typedef
	    define-c-typedef
	    ;; sizes
	    size-of-bool
	    size-of-char
	    size-of-short
	    size-of-unsigned-short
	    size-of-int
	    size-of-unsigned-int
	    size-of-long
	    size-of-unsigned-long
	    size-of-long-long
	    size-of-unsigned-long-long
	    size-of-void*
	    size-of-size_t
	    size-of-float
	    size-of-double
	    size-of-int8_t
	    size-of-int16_t
	    size-of-int32_t
	    size-of-int64_t
	    size-of-uint8_t
	    size-of-uint16_t
	    size-of-uint32_t
	    size-of-uint64_t
	    size-of-intptr_t
	    size-of-uintptr_t
	    size-of-wchar_t

	    ;; address
	    pointer-address
	    address 			;for convenience
	    ;; ref
	    pointer-ref-c-uint8
	    pointer-ref-c-int8
	    pointer-ref-c-uint16
	    pointer-ref-c-int16
	    pointer-ref-c-uint32
	    pointer-ref-c-int32
	    pointer-ref-c-uint64
	    pointer-ref-c-int64
	    ;; for convenience
	    (rename (pointer-ref-c-uint8  pointer-ref-c-uint8_t)
		    (pointer-ref-c-int8	  pointer-ref-c-int8_t)
		    (pointer-ref-c-uint16 pointer-ref-c-uint16_t)
		    (pointer-ref-c-int16  pointer-ref-c-int16_t)
		    (pointer-ref-c-uint32 pointer-ref-c-uint32_t)
		    (pointer-ref-c-int32  pointer-ref-c-int32_t)
		    (pointer-ref-c-uint64 pointer-ref-c-uint64_t)
		    (pointer-ref-c-int64  pointer-ref-c-int64_t))
	    pointer-ref-c-unsigned-char
	    pointer-ref-c-char
	    pointer-ref-c-unsigned-short
	    pointer-ref-c-short
	    pointer-ref-c-unsigned-int
	    pointer-ref-c-int
	    pointer-ref-c-unsigned-long
	    pointer-ref-c-long
	    pointer-ref-c-unsigned-long-long
	    pointer-ref-c-long-long
	    pointer-ref-c-intptr
	    pointer-ref-c-uintptr
	    ;; for convenience
	    (rename (pointer-ref-c-intptr  pointer-ref-c-intptr_t)
		    (pointer-ref-c-uintptr pointer-ref-c-uintptr_t))
	    pointer-ref-c-float
	    pointer-ref-c-double
	    pointer-ref-c-pointer
	    pointer-ref-c-wchar
	    (rename (pointer-ref-c-wchar pointer-ref-c-wchar_t))
	    ;; set!
	    pointer-set-c-uint8!
	    pointer-set-c-int8!
	    pointer-set-c-uint16!
	    pointer-set-c-int16!
	    pointer-set-c-uint32!
	    pointer-set-c-int32!
	    pointer-set-c-uint64!
	    pointer-set-c-int64!
	    ;; for convenience
	    (rename (pointer-set-c-uint8!  pointer-set-c-uint8_t! )
		    (pointer-set-c-int8!   pointer-set-c-int8_t!  )
		    (pointer-set-c-uint16! pointer-set-c-uint16_t!)
		    (pointer-set-c-int16!  pointer-set-c-int16_t! )
		    (pointer-set-c-uint32! pointer-set-c-uint32_t!)
		    (pointer-set-c-int32!  pointer-set-c-int32_t! )
		    (pointer-set-c-uint64! pointer-set-c-uint64_t!)
		    (pointer-set-c-int64!  pointer-set-c-int64_t! ))
	    pointer-set-c-unsigned-char!
	    pointer-set-c-char!
	    pointer-set-c-unsigned-short!
	    pointer-set-c-short!
	    pointer-set-c-unsigned-int!
	    pointer-set-c-int!
	    pointer-set-c-unsigned-long!
	    pointer-set-c-long!
	    pointer-set-c-unsigned-long-long!
	    pointer-set-c-long-long!
	    pointer-set-c-intptr!
	    pointer-set-c-uintptr!
	    (rename (pointer-set-c-intptr!  pointer-set-c-intptr_t!)
		    (pointer-set-c-uintptr! pointer-set-c-uintptr_t!))
	    pointer-set-c-float!
	    pointer-set-c-double!
	    pointer-set-c-pointer!
	    pointer-set-c-wchar!
	    (rename (pointer-set-c-wchar! pointer-set-c-wchar_t!))
	    ;; alignment
	    align-of-bool
	    align-of-char
	    align-of-short
	    align-of-unsigned-short
	    align-of-int
	    align-of-unsigned-int
	    align-of-long
	    align-of-unsigned-long
	    align-of-long-long
	    align-of-unsigned-long-long
	    align-of-void*
	    align-of-size_t
	    align-of-float
	    align-of-double
	    align-of-int8_t
	    align-of-int16_t
	    align-of-int32_t
	    align-of-int64_t
	    align-of-uint8_t
	    align-of-uint16_t
	    align-of-uint32_t
	    align-of-uint64_t
	    align-of-intptr_t
	    align-of-uintptr_t
	    align-of-wchar_t

	    ;; ffi procedures
	    pointer-ref-c-of
	    pointer-set-c!-of
	    size-of-of
	    align-of-of

	    ;; c-primitives
	    void
	    char short int long
	    ;; for some convenience
	    (rename (short short-int)
		    (char  unsigned-char)
		    (char  signed-char)
		    (short signed-short)
		    (short signed-short-int)
		    (int   signed-int)
		    (long  signed-long)
		    (long  signed-long-int))
	    unsigned-short unsigned-int unsigned-long
	    ;; for some convenience
	    (rename (unsigned-short unsigned-short-int)
		    (unsigned-int   unsigned)
		    (unsigned-long  unsigned-long-int))

	    int8_t int16_t int32_t uint8_t uint16_t uint32_t size_t
	    int64_t uint64_t long-long unsigned-long-long
	    bool void* char* float double callback struct array
	    intptr_t uintptr_t wchar_t wchar_t* ___
	    bit-field

	    ;; utility
	    null-pointer
	    null-pointer?
	    empty-pointer
	    pointer->string
	    pointer->bytevector
	    bytevector->pointer
	    wchar-pointer->string
	    deref

	    ;; c-variable
	    c-variable c-variable?

	    ;; clos
	    <pointer> <function-info> <callback> <c-struct>)
    (import (rnrs)
	    (core base)
	    (clos user)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius dynamic-module))
  (load-dynamic-module "sagittarius--ffi")

  (define void               'void)
  (define char               'char)
  (define short	     	     'short)
  (define int	     	     'int)
  (define long	     	     'long)
  (define intptr_t	     'intptr_t)
  (define uintptr_t	     'uintptr_t)
  (define unsigned-short     'unsigned-short)
  (define unsigned-int       'unsigned-int)
  (define unsigned-long      'unsigned-long)
  (define int8_t	     'int8_t)
  (define int16_t	     'int16_t)
  (define int32_t	     'int32_t)
  (define uint8_t	     'uint8_t)
  (define uint16_t	     'uint16_t)
  (define uint32_t	     'uint32_t)
  (define size_t	     'size_t)
  (define int64_t	     'int64_t)
  (define uint64_t	     'uint64_t)
  (define long-long	     'long-long)
  (define unsigned-long-long 'unsigned-long-long)
  (define bool	     	     'bool)
  (define void*	     	     'void*)
  (define char*	     	     'char*)
  (define float	     	     'float)
  (define double	     'double)
  (define callback           'callback)
  (define struct             'struct)
  (define array              'array)
  (define wchar_t            'wchar_t)
  (define wchar_t*           'wchar_t*)
  (define ___                '___)
  (define bit-field          'bit-field)

  ;; helper
  (define (pointer-ref-c-char* p offset)
    (pointer->string (pointer-ref-c-pointer p offset)))
  (define (pointer-ref-c-wchar_t* p offset)
    (wchar-pointer->string (pointer-ref-c-pointer p offset)))
  (define (pointer-set-c-char*! s/bv offset)
    (if (string? s/bv)
	(pointer-set-c-char*! (string->utf8 s/bv))
	(pointer-set-c-pointer! s/bv)))
  (define (pointer-set-c-wchar_t*! s/bv offset)
    (if (string? s/bv)
	(pointer-set-c-wchar_t*! (string->utf16 s/bv (endianness native)))
	(pointer-set-c-pointer! s/bv)))
  ;; should be either
  (define pointer-ref-c-size_t
    (if (= size-of-size_t size-of-int32_t)
	pointer-ref-c-int32
	pointer-ref-c-int64))
  (define pointer-set-c-size_t!
    (if (= size-of-size_t size-of-int32_t)
	pointer-set-c-int32!
	pointer-set-c-int64!))

  ;; type ref set size-of align-of
  (define %type-proc-table
    `((char               . #(,pointer-ref-c-char               ,pointer-set-c-char!                ,size-of-char               ,align-of-char              ))
      (short              . #(,pointer-ref-c-short              ,pointer-set-c-short!               ,size-of-short              ,align-of-short             ))
      (int                . #(,pointer-ref-c-int                ,pointer-set-c-int!                 ,size-of-int                ,align-of-int               ))
      (long               . #(,pointer-ref-c-long               ,pointer-set-c-long!                ,size-of-long               ,align-of-long              ))
      (intptr_t           . #(,pointer-ref-c-intptr             ,pointer-set-c-intptr!              ,size-of-intptr_t           ,align-of-intptr_t          ))
      (uintptr_t          . #(,pointer-ref-c-uintptr            ,pointer-set-c-uintptr!             ,size-of-uintptr_t          ,align-of-uintptr_t         ))
      (unsigned-short     . #(,pointer-ref-c-unsigned-short     ,pointer-set-c-unsigned-short!      ,size-of-unsigned-short     ,align-of-unsigned-short    ))
      (unsigned-int       . #(,pointer-ref-c-unsigned-int       ,pointer-set-c-unsigned-int!        ,size-of-unsigned-int       ,align-of-unsigned-int      ))
      (unsigned-long      . #(,pointer-ref-c-unsigned-long      ,pointer-set-c-unsigned-long!       ,size-of-unsigned-long      ,align-of-unsigned-long     ))
      (int8_t             . #(,pointer-ref-c-int8               ,pointer-set-c-int8!                ,size-of-int8_t             ,align-of-int8_t            ))
      (int16_t            . #(,pointer-ref-c-int16              ,pointer-set-c-int16!               ,size-of-int16_t            ,align-of-int16_t           ))
      (int32_t            . #(,pointer-ref-c-int32              ,pointer-set-c-int32!               ,size-of-int32_t            ,align-of-int32_t           ))
      (uint8_t            . #(,pointer-ref-c-uint8              ,pointer-set-c-uint8!               ,size-of-uint8_t            ,align-of-uint8_t           ))
      (uint16_t           . #(,pointer-ref-c-uint16             ,pointer-set-c-uint16!              ,size-of-uint16_t           ,align-of-uint16_t          ))
      (uint32_t           . #(,pointer-ref-c-uint32             ,pointer-set-c-uint32!              ,size-of-uint32_t           ,align-of-uint32_t          ))
      (size_t             . #(,pointer-ref-c-size_t             ,pointer-set-c-size_t!              ,size-of-size_t             ,align-of-size_t            ))
      (int64_t            . #(,pointer-ref-c-int64              ,pointer-set-c-int64!               ,size-of-int64_t            ,align-of-int64_t           ))
      (uint64_t           . #(,pointer-ref-c-uint64             ,pointer-set-c-uint64!              ,size-of-uint64_t           ,align-of-uint64_t          ))
      (long-long          . #(,pointer-ref-c-long-long          ,pointer-set-c-long-long!           ,size-of-long-long          ,align-of-long-long         ))
      (unsigned-long-long . #(,pointer-ref-c-unsigned-long-long ,pointer-set-c-unsigned-long-long!  ,size-of-unsigned-long-long ,align-of-unsigned-long-long))
      (bool               . #(,pointer-ref-c-uint8              ,pointer-set-c-uint8!               ,size-of-bool               ,align-of-bool              ))
      (void*              . #(,pointer-ref-c-pointer            ,pointer-set-c-pointer!             ,size-of-void*              ,align-of-void*             ))
      (char*              . #(,pointer-ref-c-char*              ,pointer-set-c-char*!               ,size-of-void*              ,align-of-void*             ))
      (float              . #(,pointer-ref-c-float              ,pointer-set-c-float!               ,size-of-float              ,align-of-float             ))
      (double             . #(,pointer-ref-c-double             ,pointer-set-c-double!              ,size-of-double             ,align-of-double            ))
    ;; how should we treat callback?
    ;;(callback           . #(,pointer-ref-c-callback           ,pointer-set-c-callback!            ,size-of-callback           ,align-of-callback          ))
      (wchar_t            . #(,pointer-ref-c-wchar              ,pointer-set-c-wchar!               ,size-of-wchar_t            ,align-of-wchar_t           ))
      (wchar_t*           . #(,pointer-ref-c-wchar_t*           ,pointer-set-c-wchar_t*!            ,size-of-void*              ,align-of-void*             ))))

  (define (%type-procedure type pos)
    (cond ((assq type %type-proc-table) =>
	   (lambda (v) (vector-ref (cdr v) pos)))
	  ((c-struct? type)
	   (case pos
	     ((0 1 3)
	      (error 'type-procecure
		     "ref/set!/align-of for c-struct is not supported" type))
	     ((2) (size-of-c-struct type))
	     (else (error 'type-procecure "invalid position" pos))))
	  (else (error 'type-procecure "unknown type" type))))
  (define (pointer-ref-c-of type)  (%type-procedure type 0))
  (define (pointer-set-c!-of type) (%type-procedure type 1))
  (define (size-of-of type)        (%type-procedure type 2))
  (define (align-of-of type)       (%type-procedure type 3))

  (define null-pointer (integer->pointer 0))
  (define (null-pointer? p)
    (and (pointer? p)
	 (= (pointer->integer p) 0)))

  (define (empty-pointer) (integer->pointer 0))

  (define (pointer->string pointer
			   :optional (transcoder (native-transcoder)))
    (if (null-pointer? pointer)
	(assertion-violation 'pointer->string "NULL pointer is given")
	(let-values (((out getter) (open-bytevector-output-port)))
	  (do ((i 0 (+ i 1)))
	      ((zero? (pointer-ref-c-uint8 pointer i))
	       (bytevector->string (getter) transcoder))
	    (put-u8 out (pointer-ref-c-uint8 pointer i))))))

  (define (wchar-pointer->string pointer)
    (if (null-pointer? pointer)
	(assertion-violation 'pointer->string "NULL pointer is given")
	(let-values (((out getter) (open-bytevector-output-port)))
	  (let ((buf (make-bytevector size-of-wchar_t)))
	    (do ((i 0 (+ i size-of-wchar_t)))
		((zero? (pointer-ref-c-wchar pointer i))
		 (bytevector->string (getter)
				     (make-transcoder
				      (case size-of-wchar_t
					((2) (utf-16-codec))
					((4) (utf-32-codec)))
				      (native-eol-style))))
	      (let ((wc (pointer-ref-c-wchar pointer i)))
		(case size-of-wchar_t
		  ((2)
		   (bytevector-u16-set! buf 0 wc (endianness big))
		   (put-bytevector out buf))
		  ((4)
		   ;; utf-32-codec uses native endian if it's not specified
		   ;; endianness.
		   (bytevector-u32-native-set! buf 0 wc)
		   (put-bytevector out buf)))))))))

  ;; we can't determine the size of given pointer
  ;; so trust the user.
  (define (pointer->bytevector p size :optional (offset 0) (share #t))
    (cond ((null-pointer? p)
	   (assertion-violation 'pointer->bytevector "NULL pointer is given"))
	  (share (%pointer->bytevector p size offset))
	  (else
	   (do ((i 0 (+ i 1)) (bv (make-bytevector size)))
	       ((= i size) bv)
	     (bytevector-u8-set! bv i (pointer-ref-c-uint8 p (+ i offset)))))))
  (define (bytevector->pointer bv :optional (offset 0) (share #t))
    (if share
	(%bytevector->pointer bv offset)
	(let ((size (- (bytevector-length bv) offset)))
	  (do ((i 0 (+ i 1)) (p (allocate-pointer size)))
	      ((= i size) p)
	    (pointer-set-c-uint8! p i (bytevector-u8-ref bv (+ i offset)))))))

  (define (set-pointer-value! p n)
    (slot-set! p 'value n))

  (define (deref pointer offset)
    (if (null-pointer? pointer)
	(assertion-violation 'pointer->string "NULL pointer is given")
	(pointer-ref-c-pointer pointer (* size-of-void* offset))))

  (define-syntax define-c-typedef
    (syntax-rules (* s*)
      ((_ old (* new) rest ...)
       (begin
	 (define new void*)
	 (define-c-typedef old rest ...)))
      ((_ old (s* new) rest ...)
       (begin
	 (define new char*)
	 (define-c-typedef old rest ...)))
      ((_ old new rest ...)
       (begin
	 (define new old)
	 (define-c-typedef old rest ...)))
      ((_ old)
       #t)))

  (define-syntax address
    (syntax-rules ()
      ;; underling process can handle 2 elements list as well
      ;; to avoid unnecessary allocation, we do like this.
      ((_ p) (list 'address p))
      ((_ p offset)
       (if (and (fixnum? offset) (>= offset 0))
	   (list 'address p offset)
	   (error 'address "offset must be zero or positive fixnum" offset)))))

  (define (pointer->c-function pointer ret-type name arg-types)
    (let ((stub-ret-type (assoc ret-type c-function-return-type-alist)))
      (unless stub-ret-type
	(assertion-violation 'c-function "wrong return type" ret-type))
      (let* ((ret-type (cdr stub-ret-type))
	     (signatures (list->string (make-signatures arg-types)))
	     (function (create-function-info pointer name ret-type
					    signatures
					    (car stub-ret-type) arg-types)))
	(lambda args
	  (let ((args-length (length args)))
	    (if (memq ___ arg-types)
		(let-values (((rest required)
			      (partition (lambda (e) (eq? ___ e)) arg-types)))
		  (unless (< (length required) args-length)
		    (assertion-violation
		     name
		     (format "wrong arguments number at least ~d required, but got ~d"
			     (length required)
			     args-length) args)))
		(unless (= (length arg-types) (length args))
		  (assertion-violation
		   name
		   (format "wrong arguments number ~d required, but got ~d"
			   (length arg-types)
			   args-length) args))))
	  (apply %ffi-call ret-type function args)))))

  (define (make-signatures arg-types)
    (let loop ((arg-types arg-types) (r '()))
      (if (null? arg-types)
	  (reverse! r)
	  (loop (cdr arg-types)
		(cons (case (car arg-types)
			((char short int long unsigned-short int8_t
			       int16_t int32_t uint8_t uint16_t)
			 #\i)
			((unsigned-int unsigned-long uint32_t size_t)
			 #\u)
			((int64_t long-long)
			 #\x)
			((uint64_t unsigned-long-long)
			 #\U)
			((bool) #\b)
			((void* char*) #\p)
			((float) #\f)
			((double) #\d)
			((callback) #\c)
			((wchar_t*) #\w)
			((intptr_t) (if (= size-of-intptr_t 4) #\i #\x))
			((uintptr_t) (if (= size-of-intptr_t 4) #\u #\U))
			((___)
			 ;; varargs must be the last
			 (unless (null? (cdr arg-types))
			   (assertion-violation 'make-signatures
						"___ must be the last"
						arg-types))
			 #\v)
			(else =>
			  (lambda (arg-type)
			    (if (and (pair? arg-type) 
				     (not (null? (cdr arg-type)))
				     (eq? (cadr arg-type) '*))
				#\p
				(assertion-violation 'make-signatures
						     "invalid argument type"
						     arg-types)))))
		      r)))))

  (define-syntax arg-list
    (syntax-rules (*)
      ((_ "next" (arg *) args ...)
       (cons (list arg '*) (arg-list "next" args ...)))
      ((_ "next" arg args ...)
       (cons arg (arg-list "next" args ...)))
      ((_ "next") '())
      ((_ args ...) (arg-list "next" args ...))))

  (define-syntax c-function
    (syntax-rules (*)
      ((_ lib (ret *) func (args ...))
       (make-c-function lib void* 'func (arg-list args ...)))
      ((_ lib ret func (args ...))
       (make-c-function lib ret 'func (arg-list args ...)))))

  (define (make-c-function lib ret-type name arg-types)
    (let ((func (lookup-shared-library lib (symbol->string name))))
      (when (null-pointer? func)
	(assertion-violation 'c-function "c-function not found" name))
      (pointer->c-function func ret-type name arg-types)))


  ;; callback
  (define (make-callback-signature name ret args)
    (apply string
	   (map (lambda (a)
		  (cond ((assq a callback-argument-type-class) => cdr)
			(else (assertion-violation name (format "invalid argument type ~a" a)
						   (list ret args)))))
		args)))

  (define-syntax c-callback
    (lambda (x)
      (syntax-case x ()
	((_ ret (args ...) proc)
	 #'(make-c-callback ret (list args ...) proc)))))

  (define (make-c-callback ret args proc)
    (cond ((assq ret c-function-return-type-alist)
	   => (lambda (type)
		(create-c-callback (cdr type)
				   (make-callback-signature
				    'make-c-callback ret args)
				   proc)))
	  (else
	   (assertion-violation 'make-c-callback
				(format "invalid return type ~a" ret)
				(list ret args proc)))))

  ;; c-struct
  (define (make-c-struct name defs packed?)
    (define (bit-field-check fields)
      (and (for-all (lambda (field)
		      (and (pair? field)
			   (= (length field) 2))) fields)
	   (or (unique-id-list? (map car fields))
	       (assertion-violation 'make-c-struct
				    "bit-field contains duplicate field name(s)"
				    fields))))
    (let ((layouts
	   (map (lambda (def)
		  (cond
		   ((and (eq? 'struct (car def))
			 (= (length def) 3))
		    `(,(caddr def) -1 struct . ,(cadr def)))
		   ((eq? 'callback (car def))
		    ;; speciall case
		    `(,(cadr def) ,FFI_RETURN_TYPE_CALLBACK . callback))
		   ((and (eq? 'array (cadr def))
			 (= (length def) 4)
			 (assq (car def) c-function-return-type-alist))
		    => (lambda (type)
			 `(,(cadddr def) ,(cdr type)
			   ,(caddr def) . ,(car type))))
		   ((and (eq? 'bit-field (car def))
			 (memq (caddr def) '(big little))
			 (bit-field-check (cdddr def))
			 (memq (cadr def) c-function-integers)
			 (assq (cadr def) c-function-return-type-alist))
		    => (lambda (type)
			 `(bit-field ,(cdr type) ,@(cddr def))))
		   ((assq (car def) c-function-return-type-alist)
		    => (lambda (type)
			 `(,(cadr def) ,(cdr type) . ,(car type))))
		   (else
		    (assertion-violation
		     'make-c-struct
		     (format "invalid struct declaration ~a" def)
		     (list name defs)))))
		defs)))
      (unless (unique-id-list? (filter-map (lambda (layout)
					     (let ((name (car layout)))
					       (and (not (eq? name 'bit-field))
						    name)))
					   layouts))
	(assertion-violation
	 'make-c-struct
	 "struct declaration contains duplicated member name"
	 (list name defs)))
      (create-c-struct name layouts packed?)))

  ;; (define-c-struct name (int x) (int y) (struct st s))
  (define (generate-member-name base lst)
    (string->symbol
     (string-append (format "~a." base)
		    (string-join (map symbol->string lst) "."))))

  ;; handle struct keyword in the define-c-struct.
  (define-syntax type-list
    (lambda (x)
      (define (build type* r)
	(syntax-case type* (struct array bit-field *)
	  (() (reverse! r))
	  (((struct type member) rest ...)
	   (build #'(rest ...)
		  (cons (cons* #'list 'struct #'type #'('member)) r)))

	  (((bit-field (type endian) (member bit) ...) rest ...)
	   (build #'(rest ...)
		  (cons (cons* #'list 'bit-field #'type
			       #'(endianness endian)
			       #'((list 'member bit) ...))
			r)))
	  (((bit-field type (member bit) ...) rest ...)
	   (identifier? #'type)
	   (build #'(rest ...)
		  (cons (cons* #'list 'bit-field #'type
			       #'(native-endianness)
			       #'((list 'member bit) ...))
			r)))
	  (((type array n args ...) rest ...)
	   (build #'(rest ...)
		  (cons (cons* #'list #'type 'array #'n #'('args ...)) r)))
	  ((((type *) args ...) rest ...)
	   (build #'(rest ...) (cons (cons* #'list #'void* #'('args ...)) r)))
	  (((type args ...) rest ...)
	   (build #'(rest ...) (cons (cons* #'list #'type #'('args ...)) r)))))
      (syntax-case x ()
	((_ types ...)
	 (build #'(types ...) (list #'list))))))

  (define-syntax define-c-struct
    (lambda (x)
      (define (generate-accessors name spec r)
	;; the struct members should can't be created at runtime
	;; so we can define the accessor here
	(define (gen m suffix)
	  (datum->syntax name (string->symbol
			       (format "~a-~a-~a"
				       (syntax->datum name)
				       (syntax->datum m)
				       suffix))))
	(define (gen-getters members struct?)
	  (let loop ((members members) (r '()))
	    (syntax-case members ()
	      (() r)
	      ((member . d)
	       (with-syntax ((name  name)
			     (getter (gen #'member "ref"))
			     (struct? (datum->syntax name struct?)))
		 (loop #'d
		  (cons #'(define (getter st . opt)
			  (if (and struct? (not (null? opt)))
			      (let ((m (generate-member-name 'member opt)))
				(c-struct-ref st name m))
			      (c-struct-ref st name 'member)))
			r)))))))
	(define (gen-setters members struct?)
	  (let loop ((members members) (r '()))
	    (syntax-case members ()
	      (() r)
	      ((member . d)
	       (with-syntax ((name  name)
			     (setter (gen #'member "set!"))
			     (struct? (datum->syntax name struct?)))
		 (loop #'d
		  (cons #'(define (setter st v . opt)
			    ;; the same trick as above
			    (if (and struct? (not (null? opt)))
				(let ((m (generate-member-name 'member opt)))
				  (c-struct-set! st name m v))
				(c-struct-set! st name 'member v)))
			r)))))))

	(define (continue members rest struct?)
	  (with-syntax (((getters ...) (gen-getters members struct?))
			((setters ...) (gen-setters members struct?)))

	    (generate-accessors name rest
	     (cons #'(begin
		       getters ...
		       setters ...)
		   r))))
	(syntax-case spec (struct array bit-field)
	  (() (reverse! r))
	  (((type member) rest ...)
	   (continue #'(member) #'(rest ...) #f))
	  (((struct type member) rest ...)
	   (continue #'(member) #'(rest ...) #t))
	  (((type array elements member) rest ...)
	   (continue #'(member) #'(rest ...) #f))
	  (((bit-field type (member bit) ...) rest ...)
	   (continue #'(member ...) #'(rest ...) #f))))

      (syntax-case x ()
	((_ name (type . rest) ...)
	 #'(define-c-struct name #f (type . rest) ...))
	((_ name packed? (type . rest) ...)
	 ;;(or (eq? #'packed? :packed) (not #'packed?))
	 ;; disabled for now
	 (not #'packed?)
	 ;; with black magic ...
	 (with-syntax (((accessors ...)
			(generate-accessors #'name
					    #'((type . rest) ...)
					    '())))
	   #'(begin
	       (define name (make-c-struct 'name
					   (type-list (type . rest) ...)
					   (eq? packed? :packed)))
	       accessors ...))))))


  (define (find-max-size types)
    (define (find-size type n) (* (size-of-of type) n))
    (apply max
	   (map (lambda (spec)
		  (cond ((eq? (car spec) 'struct)
			 (size-of-c-struct (cadr spec)))
			((eq? (cadr spec) 'array)
			 (or (find-size (car spec) (caddr spec)) 0))
			(else (or (find-size (car spec) 1) 0)))) types)))

  ;; the union is a type of c-struct which has uninterned symbol
  ;; member (thus not accessible), and provides bunch of procedures
  ;; which manipulate the member memory storage.
  (define-syntax define-c-union
    (lambda (x)
      (define (generate-accessors name spec r)
	;; the struct members should can't be created at runtime
	;; so we can define the accessor here
	(define (gen m suffix)
	  (datum->syntax name (string->symbol
			       (format "~a-~a-~a"
				       (syntax->datum name)
				       (syntax->datum m)
				       suffix))))
	;; TODO how to solve struct inner members?
	(define (continue type member n rest struct-type)
	  (with-syntax ((name    name)
			(getter (gen member "ref"))
			(setter (gen member "set!"))
			(?t     type)
			(?n     n))
	    (generate-accessors
	     #'name rest
	     (cons (case struct-type
		     ((struct)
		      #'(begin
			  (define (getter p) p)
			  (define (setter p v)
			    (c-memcpy p 0 v 0 (size-of-c-struct ?t)))))
		     ((array)
		      #'(begin
			  (define (getter p)
			    (define sizeof (size-of-of ?t))
			    (define ref (pointer-ref-c-of ?t))
			    (let* ((len ?n) (v (make-vector len)))
			      (let loop ((i 0))
				(cond ((= i len) v)
				      (else
				       (vector-set! v i (ref p (* sizeof i)))
				       (loop (+ i 1)))))))
			  (define (setter p v)
			    (define sizeof (size-of-of ?t))
			    (define set (pointer-set-c!-of ?t))
			    (let* ((len ?n))
			      (let loop ((i 0))
				(cond ((= i len) v)
				      (else
				       (set p (* sizeof i) (vector-ref v i))
				       (loop (+ i 1)))))))))
		     (else
		      #'(begin
			  (define (getter p)
			    (define ref (pointer-ref-c-of ?t))
			    (ref p 0))
			  (define (setter p v)
			    (define set (pointer-set-c!-of ?t))
			    (set p 0 v)))))
		   r))))
	(syntax-case spec (struct array)
	  (() (reverse! r))
	  (((type member) rest ...)
	   (continue #'type #'member #f #'(rest ...) #f))
	  (((struct type member) rest ...)
	   (continue #'type #'member #f #'(rest ...) 'struct))
	  (((type array elements member) rest ...)
	   (continue #'type #'member #'elements #'(rest ...) 'array))))

      (syntax-case x ()
	((_ name (type rest ...) ...)
	 ;; with black magic ...
	 (with-syntax (((accessors ...)
			(generate-accessors #'name
					    #'((type rest ...) ...)
					    '())))
	   #'(begin
	       (define name
		 (let* ((types (type-list (type rest ...) ...))
			(max (find-max-size types)))
		   (make-c-struct 'name
				  (list (list uint8_t 'array max (gensym)))
				  #f)))
	       accessors ...))))))

  (define c-function-integers
    `(char
      short
      int
      long
      long-long
      intptr_t
      unsigned-short
      unsigned-int
      unsigned-long
      unsigned-long-long
      uintptr_t
      size_t
      int8_t
      uint8_t
      int16_t
      uint16_t
      int32_t
      uint32_t
      int64_t
      uint64_t))

  (define c-function-return-type-alist
    `((void               . ,FFI_RETURN_TYPE_VOID    )
      (bool               . ,FFI_RETURN_TYPE_BOOL    )
      (char               . ,FFI_RETURN_TYPE_INT8_T  )
      (short              . ,FFI_RETURN_TYPE_SHORT   )
      (int                . ,FFI_RETURN_TYPE_INT     )
      (long               . ,FFI_RETURN_TYPE_LONG    )
      (long-long          . ,FFI_RETURN_TYPE_INT64_T )
      (intptr_t           . ,FFI_RETURN_TYPE_INTPTR  )
      (unsigned-short     . ,FFI_RETURN_TYPE_USHORT  )
      (unsigned-int       . ,FFI_RETURN_TYPE_UINT    )
      (unsigned-long      . ,FFI_RETURN_TYPE_ULONG   )
      (unsigned-long-long . ,FFI_RETURN_TYPE_UINT64_T)
      (uintptr_t          . ,FFI_RETURN_TYPE_UINTPTR )
      (float              . ,FFI_RETURN_TYPE_FLOAT   )
      (double             . ,FFI_RETURN_TYPE_DOUBLE  )
      (void*              . ,FFI_RETURN_TYPE_POINTER )
      (char*              . ,FFI_RETURN_TYPE_STRING  )
      (size_t             . ,FFI_RETURN_TYPE_SIZE_T  )
      (int8_t             . ,FFI_RETURN_TYPE_INT8_T  )
      (uint8_t            . ,FFI_RETURN_TYPE_UINT8_T )
      (int16_t            . ,FFI_RETURN_TYPE_INT16_T )
      (uint16_t           . ,FFI_RETURN_TYPE_UINT16_T)
      (int32_t            . ,FFI_RETURN_TYPE_INT32_T )
      (uint32_t           . ,FFI_RETURN_TYPE_UINT32_T)
      (int64_t            . ,FFI_RETURN_TYPE_INT64_T )
      (uint64_t           . ,FFI_RETURN_TYPE_UINT64_T)
      (wchar_t*           . ,FFI_RETURN_TYPE_WCHAR_STR)
      (callback           . ,FFI_RETURN_TYPE_CALLBACK)))

  (define callback-argument-type-class
    `((bool               . #\l)
      (char               . #\b)
      (short              . #\h)
      (int                . ,(if (= size-of-int 4) #\w #\q))
      (long               . ,(if (= size-of-long 4) #\w #\q))
      (long-long          . #\q)
      (intptr_t           . ,(if (= size-of-intptr_t 4) #\w #\q))
      (unsigned-char      . #\B)
      (unsigned-short     . #\H)
      (unsigned-int       . ,(if (= size-of-int 4) #\W #\Q))
      (unsigned-long      . ,(if (= size-of-long 4) #\W #\Q))
      (unsigned-long-long . #\Q)
      (uintptr_t          . ,(if (= size-of-uintptr_t 4) #\W #\Q))
      (int8_t             . #\b)
      (int16_t            . #\h)
      (int32_t            . #\w)
      (int64_t            . #\Q)
      (uint8_t            . #\B)
      (uint16_t           . #\H)
      (uint32_t           . #\W)
      (uint64_t           . #\Q)
      (float              . #\f)
      (double             . #\d)
      (size_t             . ,(if (= size-of-size_t 4) #\W #\Q))
      (void*              . #\p)))

  ;; c-varibale
  (define-class <c-variable> ()
    ((pointer :init-keyword :pointer)
     (getter  :init-keyword :getter)
     (setter  :init-keyword :setter)))

  ;; make this work like parameters :)
  ;; TODO how should we treat with void*?
  (define-method object-apply ((o <c-variable>))
    ((slot-ref o 'getter) (slot-ref o 'pointer)))
  (define-method object-apply ((o <c-variable>))
    ((slot-ref o 'getter) (slot-ref o 'pointer)))

  (define-method object-apply ((o <c-variable>) v)
    (let ((setter (slot-ref o 'setter)))
      (if setter
	  (setter (slot-ref o 'pointer) v)
	  (error 'c-variable "variable is immutable" o))))

  (define-method (setter object-apply) ((o <c-variable>) v) (o v))

  (define-method write-object ((o <c-variable>) out)
    (format out "#<c-varible ~a>" (slot-ref o 'pointer)))

  (define (make-c-variable lib name getter setter)
    (let ((p (lookup-shared-library lib (symbol->string name))))
      (make <c-variable> :pointer p :getter getter :setter setter)))

  (define (c-variable? o) (is-a? o <c-variable>))
  (define-syntax c-variable
    (lambda (x)
      (define (get-accessor type)
	(let ((name (symbol->string (syntax->datum type))))
	  (regex-match-cond
	    ((#/(.+?)_t$/ name) (#f name)
	     (list (string->symbol (string-append "pointer-ref-c-" name))
		   (string->symbol (string-append "pointer-set-c-" name "!"))))
	    (else
	     (list (string->symbol (string-append "pointer-ref-c-" name))
		   (string->symbol (string-append "pointer-set-c-" name "!")))))
	  ))
      (syntax-case x (char* void* wchar_t*)
	;; We make char* and wchar_t* immutable from Scheme.
	((_ lib char* name)
	 #'(c-variable lib name (lambda (p) (pointer->string (deref p 0))) #f))
	((_ lib wchar_t* name)
	 #'(c-variable lib name
		       (lambda (p) (wchar-pointer->string (deref p 0))) #f))
	;; TODO how should we treat this? for now direct pointer access
	((_ lib void* name)
	 ;; pointer is not immutable but I don't know the best way to
	 ;; handle set! with setter. So for now make it like this
	 #'(c-variable lib name (lambda (p) p) #f))
	((_ lib type name)
	 (with-syntax (((getter setter)
			(datum->syntax #'k (get-accessor #'type))))
	   #'(c-variable lib name
			 (lambda (p) (getter p 0))
			 (lambda (p v) (setter p 0 v)))))
	((_ lib name getter setter)
	 #'(make-c-variable lib 'name getter setter)))))


  )
