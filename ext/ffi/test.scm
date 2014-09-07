;; -*- mode: scheme; coding: utf-8 -*-
(add-load-path "./ffi")
(import (srfi :64 testing)
	(srfi :0 cond-expand)
	(rnrs)
	(core base)
	(sagittarius)
	(sagittarius vm)
	(sagittarius ffi))

(test-begin "(run-ffi-test)")

(cond-expand
 (sagittarius.ffi
  (define ffi-test-lib
    (open-shared-library
        (cond-expand
          (darwin (string-append build-directory-path "/test-lib.dylib"))
          (else (string-append build-directory-path "/test-lib.so")))))

  ;; originally this was array but now it must be an
  ;; different name otherwise define-c-struct
  ;; would confuse so that array is an keyword and exported by
  ;; (sagittarius ffi) and re-definition would make free-identifier=?
  ;; fails. the reason why this worked before was that
  ;; it's in cond-expand so this variable was not defined
  ;; yet when the macros were expanded. however because
  ;; of compilation time constant folding if 'array' in
  ;; quicksort would be folded by symbol array. to avoid
  ;; that rename was needed.
  ;; this basically breaking backward compatibility however
  ;; hmmmm the behaviour itself was a bug...
  (define bv-array (u8-list->bytevector '(6 6 1 4 2 9 3 7)))

  (test-assert "suffix" shared-object-suffix)
  (test-assert "suffix(1)" (string? (shared-object-suffix)))

  ;; for now, we do not support anonymous struct
  (define-c-struct inner
    (int value2)
    (char* str))
  (define-c-struct data-to-store
    (int value1)
    (struct inner inner))

  ;; for test convenience
  (define pointer-ref-c-uint8_t   pointer-ref-c-uint8)
  (define pointer-ref-c-int8_t    pointer-ref-c-int8) 
  (define pointer-ref-c-uint16_t  pointer-ref-c-uint16)
  (define pointer-ref-c-int16_t   pointer-ref-c-int16)
  (define pointer-ref-c-uint32_t  pointer-ref-c-uint32)
  (define pointer-ref-c-int32_t   pointer-ref-c-int32)
  (define pointer-ref-c-uint64_t  pointer-ref-c-uint64)
  (define pointer-ref-c-int64_t   pointer-ref-c-int64)
  (define pointer-ref-c-wchar_t   pointer-ref-c-wchar)
  (define pointer-set-c-uint8_t!  pointer-set-c-uint8!)
  (define pointer-set-c-int8_t!   pointer-set-c-int8!) 
  (define pointer-set-c-uint16_t! pointer-set-c-uint16!)
  (define pointer-set-c-int16_t!  pointer-set-c-int16!)
  (define pointer-set-c-uint32_t! pointer-set-c-uint32!)
  (define pointer-set-c-int32_t!  pointer-set-c-int32!)
  (define pointer-set-c-uint64_t! pointer-set-c-uint64!)
  (define pointer-set-c-int64_t!  pointer-set-c-int64!)
  (define pointer-set-c-wchar_t!  pointer-set-c-wchar!)

  (define-syntax pointer-ref-test
    (lambda (x)
      (syntax-case x ()
	((k type exact?)
	 (let* ((t (syntax->datum #'type))
		(m (format "pointer-ref-test ~a" t)))
	   (with-syntax
	       ((ref (datum->syntax #'k
				    (string->symbol 
				     (format "pointer-ref-c-~a" t))))
		(set (datum->syntax #'k
				    (string->symbol
				     (format "pointer-set-c-~a!" t))))
		(size (datum->syntax #'k
				     (string->symbol 
				      (format "size-of-~a" t))))
		(msg (datum->syntax #'k m)))
	     #'(begin
		 (test-error (string-append msg " null-pointer ref")
			     assertion-violation?
			     (ref null-pointer 0))
		 (test-error (string-append msg " null-pointer set")
			     assertion-violation?
			     (set null-pointer 0 1))
		 (test-assert
		  msg
		  (let ((expect (map (lambda (v)
				       (if exact?
					   (exact v)
					   (inexact v)))
				     '(0 1 2 3 4 5 6 7 8 9))))
		    (let ((p (allocate-pointer (* size 10))))
		      (let loop ((i 0))
			(unless (= i 10)
			  (set p (* size i) i)
			  (loop (+ i 1))))
		      (let loop ((i 0)
				 (r '()))
			(if (= i 10)
			    (equal? expect (reverse! r))
			    (loop (+ i 1)
				  (cons (ref p (* size i)) r))))))))))))))

  (test-assert "open ffi-test-lib" (not (null-pointer? ffi-test-lib)))
  (test-equal "simple call"
	      3
	      (let ((add (c-function ffi-test-lib int add (int int))))
		(add 1 2)))

  (test-equal "callback"
	      #vu8(1 2 3 4 6 6 7 9)
	      (let ((qsort (c-function ffi-test-lib void quicksort
				       (void* size_t size_t callback)))
		    (compare (c-callback int (void* void*)
					 (lambda (x y)
					   (- (pointer-ref-c-int8 x 0)
					      (pointer-ref-c-int8 y 0))))))
		(qsort bv-array (bytevector-length bv-array) 1 compare)
		(free-c-callback compare)
		bv-array))

  ;; pointer address
  (test-equal "address passing"
	      #\a
	      (let* ((p (allocate-pointer size-of-char))
		     (ap (pointer-address p))
		     (setter (c-function ffi-test-lib void address_passing
					 (void*))))
		(setter ap)
		(integer->char (pointer-ref-c-char p 0))))

  (test-equal "address passing allocate"
	      "hello"
	      (let* ((p (empty-pointer))
		     (setter (c-function ffi-test-lib void
					 address_passing_string
					 (void*)))
		     (dctr (c-function ffi-test-lib void
					 address_passing_free
					 (void*))))
		(setter (address p))
		(let ((r (pointer->string p)))
		  (dctr (address p))
		  r)))

  (test-equal "set int" #xF5
	      (let ((p (empty-pointer))
		    (set (c-function ffi-test-lib void set_int (void*))))
		(set (address p))
		(pointer->integer p)))

  (test-equal "set int(-1)" #xF5
	      (let ((p (integer->pointer -1))
		    (set (c-function ffi-test-lib void set_int (void*))))
		(set (address p))
		(pointer->integer p 32)))

  (test-equal "set int(-1)" #xF5
	      (let ((p (integer->pointer -1))
		    (set (c-function ffi-test-lib void set_int (void*))))
		(set (address p))
		(pointer->integer p 8)))

  (test-equal "set int(-1)" #x5
	      (let ((p (integer->pointer -1))
		    (set (c-function ffi-test-lib void set_int (void*))))
		(set (address p))
		(pointer->integer p 4)))

  (test-assert "c-struct?" (c-struct? data-to-store))
  (test-equal "c-struct"
	      '(100 200 "message from C")
	      (let* ((st (allocate-c-struct data-to-store))
		     (store (c-function ffi-test-lib void store_data 
					(void*))))
		(store st)

		(let ((r (list (c-struct-ref st data-to-store 'value1)
			       (c-struct-ref st data-to-store 'inner.value2)
			       (c-struct-ref st data-to-store 'inner.str))))
		  r)))

  ;; new api's for c-struct
  (test-equal "c-struct accessor"
	      100 
	      (let ((st (allocate-c-struct data-to-store)))
		(data-to-store-value1-set! st 100)
		(data-to-store-value1-ref st)))

  ;; new feature array
  (define-c-struct struct-with-array
    (int array 4 int*))

  (test-equal "c-struct array"
	      #(1 2 3 4)
	      (let* ((st (allocate-c-struct struct-with-array)))
		(struct-with-array-int*-set! st #(1 2 3 4))
		(struct-with-array-int*-ref st)))

  ;; this depends on the memmory condition but
  ;; fresh memory is always initialised with 0 padding.
  (test-equal "c-struct array"
	      #(1 2 3 0)
	      (let* ((st (allocate-c-struct struct-with-array)))
		(struct-with-array-int*-set! st #(1 2 3))
		(struct-with-array-int*-ref st)))
  
  ;; should we check the array size on runtime?
  (test-equal "c-struct array"
	      #(1 2 3 4)
	      (let* ((st (allocate-c-struct struct-with-array)))
		(struct-with-array-int*-set! st #(1 2 3 4 5))
		(struct-with-array-int*-ref st)))

  ;;(pointer-ref-test bool #t)
  ;; for now char and wchar_t returns integer
  (pointer-ref-test char #t)
  (pointer-ref-test wchar_t #t)

  (pointer-ref-test short #t)
  (pointer-ref-test unsigned-short #t)
  (pointer-ref-test int #t)
  (pointer-ref-test unsigned-int #t)
  (pointer-ref-test long #t)
  (pointer-ref-test unsigned-long #t)
  (pointer-ref-test long-long #t)
  (pointer-ref-test unsigned-long-long #t)

  ;; we don't test void*
  ;;(pointer-ref-test size_t #t)
    
  (pointer-ref-test float #f)
  (pointer-ref-test double #f)
  (pointer-ref-test int8_t #t)
  (pointer-ref-test int16_t #t)
  (pointer-ref-test int32_t #t)
  (pointer-ref-test int64_t #t)
  ;;(pointer-ref-test intptr_t #t)
    
  (pointer-ref-test uint8_t #t)
  (pointer-ref-test uint16_t #t)
  (pointer-ref-test uint32_t #t)
  (pointer-ref-test uint64_t #t)
  ;;(pointer-ref-test uintptr_t #t)

  
  (let* ((size (string-length "abcde"))
	 (bv   (string->utf8 "abcde"))
	 (p (allocate-pointer (+ size 1))))
    (do ((i 0 (+ i 1)))
	((= i size))
      (pointer-set-c-uint8! p i (bytevector-u8-ref bv i)))
    (test-equal "pointer->string" "abcde" (pointer->string p)))
  
  (define-c-struct env-holder (void* envp))
  (test-equal "ref-c-pointer" '("test" "buzz")
	      (let ((st (allocate-c-struct env-holder))
		    (f (c-function ffi-test-lib void setTest (void*))))
		(f st)
		(let ((envp (c-struct-ref st env-holder 'envp)))
		  (let loop ((i 0) (r '()))
		    (let ((p (deref envp i)))
		      (if (null-pointer? p)
			  (reverse! r)
			  (let* ((s (pointer->string p)))
			    (loop (+ i 1) (cons s r)))))))))
    
  (test-error "null-pointer 1" assertion-violation? 
	      (pointer->string null-pointer))
  (test-error "null-pointer 2" assertion-violation? 
	      (deref null-pointer 0))

  (let ((p (empty-pointer)))
    (test-assert "set-pointer-value!" (set-pointer-value! p 1))
    (test-equal "set-pointer-value!" 1 (pointer->integer p))
    (test-error "set-pointer-value!" values (set-pointer-value! p 'a)))

  ;; extra
  (define-c-struct size-check
    (char  c)
    (short s))
  (test-equal "size-check" 4 (size-of-c-struct size-check))

  ;; local struct
  (let ()
    (define-c-struct local-struct (int32_t v))
    (test-equal "local-struct" 4 (size-of-c-struct local-struct)))
  (test-error "local-struct(outside)" local-struct)

  ;; varargs
  (let ()
    (define va-fn (c-function ffi-test-lib int va_fn (void* int ___)))
    (let ((result (allocate-pointer (* 4 size-of-void*))))
      (let ((r (va-fn result 4 1 #t "abcdef" 4)))
	(test-equal "varargs" 4 r)
	(test-equal "result(1)" 1 (pointer->integer (deref result 0)))
	(test-equal "result(2)" 1 (pointer->integer (deref result 1)))
	(test-equal "result(3)" "abcdef"
		    (pointer->string (deref result 2)))
	(test-equal "result(4)" 4 (pointer->integer (deref result 3))))
      (let ((r (va-fn result 4 1 2 3 4 5 6 7)))
	(test-equal "varargs" 4 r)
	(test-equal "result(1~)" 1 (pointer->integer (deref result 0)))
	(test-equal "result(2~)" 2 (pointer->integer (deref result 1)))
	(test-equal "result(3~)" 3 (pointer->integer (deref result 2)))
	(test-equal "result(4~)" 4 (pointer->integer (deref result 3))))
      ))

  ;; wchar_t*
  (let ()
    (define wide-fn (c-function ffi-test-lib wchar_t* wide_fn (wchar_t*)))
    (define input "wide string")
    (test-assert "size-of-wchar_t" size-of-wchar_t)
    (test-assert "align-of-wchar_t" align-of-wchar_t)
    (test-equal "wchar_t*" input (wide-fn input)))

  ;; callback return
  (define set-compare! (c-function ffi-test-lib void set_compare (callback)))
  (define get-compare (c-function ffi-test-lib callback get_compare ()))
  (test-assert "get-compare (NULL)" (not (get-compare)))
  (set-compare! (c-callback int (void* void*)
			    (lambda (x y)
			      (- (pointer-ref-c-uint8 y 0)
				 (pointer-ref-c-uint8 x 0)))))
  (test-assert "get-compare" (callback? (get-compare)))

  ;; c-variable
  (let ()
    (define var   (c-variable ffi-test-lib int var))
    (define cvar  (c-variable ffi-test-lib char* c_var))
    (define pvar  (c-variable ffi-test-lib void* pointer))
    (define wcvar (c-variable ffi-test-lib wchar_t* wc_var))
   
    (test-assert "c-variable?" (c-variable? var))
    (test-equal "c variable 0" 0 (var))
    (test-assert "(set! var 1)" (set! (var) 1))
    (test-equal "c variable 1" 1 (var))

    (test-equal "c variable char"  "test char"  (cvar))
    (test-error "set! char*" (set! (cvar) "char* is immutable"))
    (test-equal "c variable char"  "test char"  (cvar))

    ;; pointer variable is simply the address of the variable.
    (test-equal "c variable pointer" 1 (pointer->integer (deref (pvar) 0)))
    (test-equal "c variable pointer" 2 (pointer->integer (deref (pvar) 1)))
    ;; for pointer variable is not immutable but no setter is defined
    ;; to mutate it user need to modify the pointer directory
    (let ((p (pvar)))
      (pointer-set-c-int! p 0 10)
      (test-equal "c variable pointer" 10 (pointer->integer (deref (pvar) 0))))

    ;; this test doesn't work on Cygwin because of wide-exec-chaset stuff.
    (cond-expand
     (cygwin #f)
     (else 
      (test-equal "c variable wchar" "test wchar" (wcvar))
      (test-error "set! wchar_t*" (set! (wcvar) "wchar_t* is immutable"))
      (test-equal "c variable wchar" "test wchar" (wcvar))))
    )
  (let ((n 4))
    ;; variable length struct array
    (define-c-struct foo
      (int32_t array (* size-of-char n) dummy))
    (test-equal "size" (* size-of-int32_t size-of-char n)
		(size-of-c-struct foo)))
  ;; memcpy
  (test-error "wrong argument" condition? (c-memcpy 'a 0 'b 0 0))
  (test-error "wrong argument" condition? 
	      (c-memcpy (allocate-pointer 0) 0 'b 0 0))
  (let ((p (allocate-pointer (* size-of-char 5)))
	(p2 (allocate-pointer (* size-of-char 5))))
    (c-memcpy p 0 (string->utf8 "1234") 0 4)
    (test-equal "c-memcpy" "1234" (pointer->string p))
    (c-memcpy p2 0 (string->utf8 "987") 0 3)
    (test-equal "c-memcpy" "987" (pointer->string p2))
    (c-memcpy p 1 p2 1 2)
    (test-equal "c-memcpy" "1874" (pointer->string p))
    )

  ;; union
  (let ()
    (define-c-struct a-st
      (short s1)
      (short s2))
    (define-c-union a-union
      (int i)
      (char array size-of-int c*)
      (struct a-st st))
    ;; we can use c-struct allocation
    (let ((p (allocate-c-struct a-union)))
      (a-union-i-set! p #x12345678)
      (test-equal "union a" #x12345678 (a-union-i-ref p))
      ;; shares the memory
      (test-equal "union st s1" 
		  (if (eq? (endianness native) (endianness little))
		      #x5678 #x1234)
		  (a-st-s1-ref p))
      (test-equal "union c*" 
		  (if (eq? (endianness native) (endianness little))
		      #(#x78 #x56 #x34 #x12) #(#x12 #x34 #x56 #x78))
		  (a-union-c*-ref p))

      (a-union-c*-set! p (if (eq? (endianness native) (endianness little))
			     #(#x78 #x56 #x34 #x12) #(#x12 #x34 #x56 #x78)))
      (test-equal "union a(1)" #x12345678 (a-union-i-ref p))
      ;; shares the memory
      (test-equal "union st s1(1)" 
		  (if (eq? (endianness native) (endianness little))
		      #x5678 #x1234)
		  (a-st-s1-ref p))
      (test-equal "union c*(1)" 
		  (if (eq? (endianness native) (endianness little))
		      #(#x78 #x56 #x34 #x12) #(#x12 #x34 #x56 #x78))
		  (a-union-c*-ref p))
      ))

  ;; call #60
  (let ()
    (define-c-struct bar
      (int bi)
      (char* bc*))
    (define-c-struct foo
      (short fs)
      (struct bar fb)
      (long fl))
    (define foo-bar-bi (c-function ffi-test-lib int foo_bar_bi (void*)))
    (define foo-bar-bc* (c-function ffi-test-lib char* foo_bar_bc (void*)))
    (let ((fp (allocate-c-struct foo))
	  (bp (allocate-c-struct bar)))
      (bar-bi-set! bp 1234)
      (bar-bc*-set! bp "hello world")
      (test-assert "set internal struct as a pointer" (foo-fb-set! fp bp))
      (test-assert "ref" (pointer? (foo-fb-ref fp)))
      (test-equal "bi" 1234 (foo-bar-bi fp))
      (test-equal "bc*" "hello world" (foo-bar-bc* fp))
      )
    )
  )
 (else
  #t))
(test-end)
