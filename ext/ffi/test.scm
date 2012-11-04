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
  (define ffi-test-lib (open-shared-library "test-lib.so"))

  (define array (u8-list->bytevector '(6 6 1 4 2 9 3 7)))

  ;; for now, we do not support annonymous struct
  (cond-expand
   (x86_64
    ;; on X86_64 environment, struct alignment was bit different
    ;; than I expected.
    (define-c-struct inner
      (intptr_t value2)
      (char* str))
    
    (define-c-struct data-to-store
      (intptr_t value1)
      (struct inner inner)))
   (else
    (define-c-struct inner
      (int value2)
      (char* str))
    
    (define-c-struct data-to-store
      (int value1)
      (struct inner inner))))

  ;; for test convenience
  (define pointer-ref-c-uint8_t   pointer-ref-c-uint8)
  (define pointer-ref-c-int8_t    pointer-ref-c-int8) 
  (define pointer-ref-c-uint16_t  pointer-ref-c-uint16)
  (define pointer-ref-c-int16_t   pointer-ref-c-int16)
  (define pointer-ref-c-uint32_t  pointer-ref-c-uint32)
  (define pointer-ref-c-int32_t   pointer-ref-c-int32)
  (define pointer-ref-c-uint64_t  pointer-ref-c-uint64)
  (define pointer-ref-c-int64_t   pointer-ref-c-int64)
  (define pointer-set-c-uint8_t!  pointer-set-c-uint8!)
  (define pointer-set-c-int8_t!   pointer-set-c-int8!) 
  (define pointer-set-c-uint16_t! pointer-set-c-uint16!)
  (define pointer-set-c-int16_t!  pointer-set-c-int16!)
  (define pointer-set-c-uint32_t! pointer-set-c-uint32!)
  (define pointer-set-c-int32_t!  pointer-set-c-int32!)
  (define pointer-set-c-uint64_t! pointer-set-c-uint64!)
  (define pointer-set-c-int64_t!  pointer-set-c-int64!)

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
	     #'(test-assert
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
				(cons (ref p (* size i)) r)))))))))))))

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
		(qsort array (bytevector-length array) 1 compare)
		(free-c-callback compare)
		array))


  ;; pointer address
  (test-equal "address passing"
	      #\a
	      (let* ((p (allocate-pointer size-of-char))
		     (ap (pointer-address p))
		     (setter (c-function ffi-test-lib void address_passing
					 (void*))))
		(setter ap)
		(integer->char (pointer-ref-c-char p 0))))

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
  ;;(pointer-ref-test bool #t)

  (pointer-ref-test char #t)
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
    

  )
 (else
  #t))
(test-end)
