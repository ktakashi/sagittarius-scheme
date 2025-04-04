(import (rnrs)
	(sagittarius atomic)
	(srfi :1)
	(srfi :18)
	(srfi :64))

(test-begin "Basic atomic operations")

(test-assert (atomic? (make-atomic #t)))
(test-assert (not (atomic-fixnum? (make-atomic #t))))

(test-assert (atomic? (make-atomic-fixnum 100)))
(test-assert (atomic-fixnum? (make-atomic-fixnum 100)))
(test-error (make-atomic-fixnum #t))

(test-assert (fixnum? *memory-order:relaxed*))
(test-assert (fixnum? *memory-order:consume*))
(test-assert (fixnum? *memory-order:acquire*))
(test-assert (fixnum? *memory-order:release*))
(test-assert (fixnum? *memory-order:acq-rel*))
(test-assert (fixnum? *memory-order:seq-cst*))

(test-assert (memory-order? *memory-order:seq-cst*))
(test-assert (not (memory-order? (+ *memory-order:seq-cst* 100))))

(test-group "Basic check"
 (let ()
   (define atomic-boolean (make-atomic #t))
   (atomic-exchange! atomic-boolean #f)
   (test-equal #f (atomic-load atomic-boolean))
   (test-error (atomic-load atomic-boolean #f))
   ;; it's a bit depending on the platform specific value,
   ;; I hope memory order can't be this number
   (test-error (atomic-load atomic-boolean #xffffffff))
   
   (test-equal #t (atomic-compare-and-swap! atomic-boolean #f #t))
   (test-equal #t (atomic-load atomic-boolean)))
 (let ()
   (define atomic-symbol (make-atomic 'symbol))
   (test-equal 'symbol (atomic-exchange! atomic-symbol 'symbol2))
   (test-equal 'symbol2 (atomic-load atomic-symbol))
   (test-equal #f (atomic-compare-and-swap! atomic-symbol 'symbol 'symbol3))
   (test-equal 'symbol2 (atomic-load atomic-symbol))

   (test-assert (atomic-store! atomic-symbol 'symbol3))
   (test-equal 'symbol3 (atomic-load atomic-symbol))
   )
 (let ()
   (define atomic-fixnum (make-atomic-fixnum 100))

   (test-assert (atomic-fixnum-store! atomic-fixnum -1))
   (test-equal -1 (atomic-fixnum-load atomic-fixnum))
   (test-equal -1 (atomic-fixnum-exchange! atomic-fixnum 100))
   (test-equal 100 (atomic-fixnum-load atomic-fixnum))

   (test-equal 100 (atomic-fixnum-add! atomic-fixnum 50))
   (test-equal 150 (atomic-load atomic-fixnum))
   (test-equal 150 (atomic-fixnum-load atomic-fixnum))
   (test-equal 150 (atomic-fixnum-sub! atomic-fixnum 50))
   (test-equal 100 (atomic-fixnum-load atomic-fixnum))
   (test-equal 100 (atomic-fixnum-inc! atomic-fixnum))
   (test-equal 101 (atomic-fixnum-load atomic-fixnum))
   (test-equal 101 (atomic-fixnum-dec! atomic-fixnum))
   (test-equal 100 (atomic-fixnum-load atomic-fixnum))

   (test-error (atomic-store! atomic-fixnum 'symbol))
   
   (test-assert (atomic-fixnum-store! atomic-fixnum #x00))
   (test-equal #x00 (atomic-fixnum-ior! atomic-fixnum #x01))
   (test-equal #x01 (atomic-fixnum-load atomic-fixnum))
   (test-equal #x01 (atomic-fixnum-and! atomic-fixnum #x11))
   (test-equal #x01 (atomic-fixnum-load atomic-fixnum))
   (test-equal #x01 (atomic-fixnum-xor! atomic-fixnum #x11))
   (test-equal #x10 (atomic-fixnum-load atomic-fixnum))
   )
 )

(test-group "Concurrent check"
 (let ()
   (define val (make-atomic-fixnum 0))
   (define (increment)
     (do ((i 0 (+ i 1))) ((= i 1000))
       (atomic-fixnum-add! val 1)))
   (define threads (map (lambda (i) (make-thread increment)) (iota 10)))
   (for-each thread-start! threads)
   (for-each thread-join! threads)
   (test-equal 10000 (atomic-load val))))

(test-group "Atomic pair"
 (let ()
   (define val (make-atomic-pair 'a 'b))
   (test-error (atomic-compare-and-swap! val (cons 'a 'b) '()))
   (test-assert (atomic-compare-and-swap! val (cons 'a 'b) '(c . d)))
   (test-equal '(c . d) (atomic-load val))
   (test-equal '(c . d) (atomic-fetch-compare-and-swap! val (cons 'c 'd) '(e . f)))
   ))

(test-end)
