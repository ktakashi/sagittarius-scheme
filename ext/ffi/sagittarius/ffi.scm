;; -*- mode: scheme; coding: utf-8; -*-
(load-dynamic-library "sagittarius--ffi")
(library (sagittarius ffi)
    (export open-shared-library
	    lookup-shared-library
	    close-shared-library
	    c-function
	    pointer->c-function

	    c-callback
	    free-c-callback
	    ;; malloc
	    c-malloc
	    c-free
	    ;; c-struct
	    define-c-struct
	    size-of-c-struct
	    c-struct-ref
	    c-struct-set!

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

	    ;; ref
	    pointer-ref-c-uint8
	    pointer-ref-c-int8)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax)
	    (core misc)
	    (sagittarius)
	    (sagittarius ffi impl))

  (define (pointer->c-function pointer ret-type name arg-types)
    (let* ((stub-ret-type (assoc ret-type c-function-return-type-alist))
	   (signatures (list->string (make-sigunatures arg-types)))
	   (function (create-function-info pointer (cdr stub-ret-type) signatures)))
      (unless stub-ret-type
	(assertion-violation 'c-function "wrong return type" ret-type))
      (lambda args
	(unless (= (length arg-types) (length args))
	  (assertion-violation name (format "wrong arguments number ~d required, but got ~d"
					    (length arg-types)
					    (length args)) args))
	(apply %ffi-call
	       (cdr stub-ret-type)
	       function
	       args))))

  (define (make-sigunatures arg-types)
    (map (lambda (arg-type)
	   (case arg-type
	     ((char short int long unsigned-short unsigned-int unsigned-long int8_t int16_t int32_t uint8_t uint16_t uint32_t size_t)
	      #\i)
	     ((int64_t uint64_t long-long unsigned-long-long)
	      #\x)
	     ((bool) #\b)
	     ((void* char*) #\p)
	     ((float) #\f)
	     ((double) #\d)
	     ((callback) #\c)
	     (else (assertion-violation 'make-sigunatures "invalid argument type" arg-type))))
       arg-types))

  (define-syntax c-function
    (lambda (x)
      (syntax-case x ()
	((_ lib ret func (args ...))
	 #'(make-c-function lib 'ret 'func '(args ...))))))

  (define (make-c-function lib ret-type name arg-types)
    (let ((func (lookup-shared-library lib (symbol->string name))))
      (unless func
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
	((_ ret args proc)
	 #'(make-c-callback 'ret 'args proc)))))

  (define (make-c-callback ret args proc)
    (cond ((assq ret c-function-return-type-alist)
	   => (lambda (type)
		(create-c-callback (cdr type)
				 (make-callback-signature 'make-c-callback ret args)
				 proc)))
	  (else
	   (assertion-violation 'make-c-callback (format "invalid return type ~a" ret)
				(list ret args proc)))))

  ;; c-struct
  (define (make-c-struct name defs)
    (let ((layouts (map (lambda (def)
			  (cond ((eq? 'struct (car def))
				 `(,(caddr def) -1 struct . ,(cadr def)))
				((assq (car def) c-function-return-type-alist)
				 => (lambda (type)
				      `(,(cadr def) ,(cdr type) . (car type))))
				(else
				 (assertion-violation 'make-c-struct
						      (format "invalid struct declaration ~a" def)
						      (list name defs)))))
			defs)))
      (unless (unique-id-list? (map car layouts))
	(assertion-violation 'make-c-struct
			     "struct declaration contains duplicated member name"
			     (list name defs)))
      (create-c-struct name layouts)))

  ;; (define-c-struct name (int x) (int y) (struct st s))
  (define-syntax define-c-struct
    (lambda (x)
      (syntax-case x ()
	((_ name defs ...)
	 #'(define name (make-c-struct 'name '(defs ...)))))))

  (define c-function-return-type-alist
    '((void               . #x00)    ; FFI_RETURN_TYPE_VOID
      (bool               . #x01)    ; FFI_RETURN_TYPE_BOOL
      (char               . #x0c)    ; FFI_RETURN_TYPE_INT8_T
      (short              . #x02)    ; FFI_RETURN_TYPE_SHORT
      (int                . #x03)    ; FFI_RETURN_TYPE_INT
      (long               . #x04)    ; FFI_RETURN_TYPE_INTPTR
      (long-long          . #x12)    ; FFI_RETURN_TYPE_INT64_T
      (unsigned-short     . #x05)    ; FFI_RETURN_TYPE_USHORT
      (unsigned-int       . #x06)    ; FFI_RETURN_TYPE_UINT
      (unsigned-long      . #x07)    ; FFI_RETURN_TYPE_UINTPTR
      (unsigned-long-long . #x13)    ; FFI_RETURN_TYPE_UINT64_T
      (float              . #x08)    ; FFI_RETURN_TYPE_FLOAT
      (double             . #x09)    ; FFI_RETURN_TYPE_DOUBLE
      (void*              . #x14)    ; FFI_RETURN_TYPE_POINTER
      (char*              . #x0a)    ; FFI_RETURN_TYPE_STRING
      (size_t             . #x0b)    ; FFI_RETURN_TYPE_SIZE_T
      (int8_t             . #x0c)    ; FFI_RETURN_TYPE_INT8_T
      (uint8_t            . #x0d)    ; FFI_RETURN_TYPE_UINT8_T
      (int16_t            . #x0e)    ; FFI_RETURN_TYPE_INT16_T
      (uint16_t           . #x0f)    ; FFI_RETURN_TYPE_UINT16_T
      (int32_t            . #x10)    ; FFI_RETURN_TYPE_INT32_T
      (uint32_t           . #x11)    ; FFI_RETURN_TYPE_UINT32_T
      (int64_t            . #x12)    ; FFI_RETURN_TYPE_INT64_T
      (uint64_t           . #x13)))  ; FFI_RETURN_TYPE_UINT64_T

  (define callback-argument-type-class
    `((bool               . #\l)
      (char               . #\b)
      (short              . #\h)
      (int                . ,(if (= size-of-int 4) #\w #\q))
      (long               . ,(if (= size-of-long 4) #\w #\q))
      (long-long          . #\q)
      (unsigned-char      . #\B)
      (unsigned-short     . #\H)
      (unsigned-int       . ,(if (= size-of-int 4) #\W #\Q))
      (unsigned-long      . ,(if (= size-of-long 4) #\W #\Q))
      (unsigned-long-long . #\Q)
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
)