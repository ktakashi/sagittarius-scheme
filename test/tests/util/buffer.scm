(import (rnrs)
	(util buffer)
	(srfi :64))

(test-begin "pre-allocated buffer")

;; predicate
(test-assert "pre-allocated-buffer?" 
	     (make-binary-pre-allocated-buffer #vu8()))
(test-assert "binary-pre-allocated-buffer?"
	     (make-binary-pre-allocated-buffer #vu8()))

(let ((buf (make-binary-pre-allocated-buffer (make-bytevector 1 0))))
  (test-equal "buffer" #vu8(0) (pre-allocated-buffer-buffer buf))
  (test-equal "buffer size" 0 (pre-allocated-buffer-size buf)))

(define (binary-pre-allocated-buffer-put-u8!/endian buf v endian)
  (binary-pre-allocated-buffer-put-u8! buf v))
(define (binary-pre-allocated-buffer-put-s8!/endian buf v endian)
  (binary-pre-allocated-buffer-put-s8! buf v))
(define (bytevector-u8-ref/endian bv index endian)
  (bytevector-u8-ref bv index))
(define (bytevector-s8-ref/endian bv index endian)
  (bytevector-s8-ref bv index))

(define-syntax test-binary-buffer-number-put! 
  (syntax-rules ()
    ((_ size setter bv-ref value endian)
     (let* ((bv (make-bytevector (* size 2)))
	    (buf (make-binary-pre-allocated-buffer bv)))
       (test-assert 'setter (setter buf value endian))
       (test-equal "buffer size(1)" size (pre-allocated-buffer-size buf))
       (test-equal 'bv-ref value (bv-ref bv 0 endian))))))
(test-binary-buffer-number-put! 1 binary-pre-allocated-buffer-put-u8!/endian
				bytevector-u8-ref/endian 5 (endianness big))
(test-binary-buffer-number-put! 1 binary-pre-allocated-buffer-put-s8!/endian
				bytevector-s8-ref/endian 5 (endianness big))
;; 16 bits
(test-binary-buffer-number-put! 2 binary-pre-allocated-buffer-put-u16!
				bytevector-u16-ref 5 (endianness big))
(test-binary-buffer-number-put! 2 binary-pre-allocated-buffer-put-s16!
				bytevector-s16-ref 5 (endianness big))

;; 32 bits
(test-binary-buffer-number-put! 4 binary-pre-allocated-buffer-put-u32!
				bytevector-u32-ref 5 (endianness big))
(test-binary-buffer-number-put! 4 binary-pre-allocated-buffer-put-s32!
				bytevector-s32-ref 5 (endianness big))
;; 64 bits
(test-binary-buffer-number-put! 8 binary-pre-allocated-buffer-put-u64!
				bytevector-u64-ref 5 (endianness big))
(test-binary-buffer-number-put! 8 binary-pre-allocated-buffer-put-s64!
				bytevector-s64-ref 5 (endianness big))

;; float
(test-binary-buffer-number-put! 4 binary-pre-allocated-buffer-put-f32!
				bytevector-ieee-single-ref 5.0 (endianness big))

;; double
(test-binary-buffer-number-put! 8 binary-pre-allocated-buffer-put-f64!
				bytevector-ieee-double-ref 5.0 (endianness big))

(define (binary-pre-allocated-buffer-set-u8!/endian buf pos v . opts)
  (binary-pre-allocated-buffer-set-u8! buf pos v))
(define (binary-pre-allocated-buffer-set-s8!/endian buf pos v . opts)
  (binary-pre-allocated-buffer-set-s8! buf pos v))

(define-syntax test-binary-buffer-number-set! 
  (syntax-rules ()
    ((_ size setter bv-ref value endian)
     (let* ((bv (make-bytevector (* size 2)))
	    (buf (make-binary-pre-allocated-buffer bv)))
       (test-assert 'setter (setter buf 0 value endian))
       (test-equal "buffer size(1)" size (pre-allocated-buffer-size buf))
       (test-equal 'bv-ref value (bv-ref bv 0 endian))

       (test-assert 'setter (setter buf 1 value endian))
       (test-equal "buffer size(1)" (+ size 1) (pre-allocated-buffer-size buf))
       (test-equal 'bv-ref value (bv-ref bv 1 endian))))))

(test-binary-buffer-number-set! 1 binary-pre-allocated-buffer-set-u8!/endian
				bytevector-u8-ref/endian 5 (endianness big))
(test-binary-buffer-number-set! 1 binary-pre-allocated-buffer-set-s8!/endian
				bytevector-s8-ref/endian 5 (endianness big))
;; 16 bits
(test-binary-buffer-number-set! 2 binary-pre-allocated-buffer-set-u16!
				bytevector-u16-ref 5 (endianness big))
(test-binary-buffer-number-set! 2 binary-pre-allocated-buffer-set-s16!
				bytevector-s16-ref 5 (endianness big))

;; 32 bits
(test-binary-buffer-number-set! 4 binary-pre-allocated-buffer-set-u32!
				bytevector-u32-ref 5 (endianness big))
(test-binary-buffer-number-set! 4 binary-pre-allocated-buffer-set-s32!
				bytevector-s32-ref 5 (endianness big))
;; 64 bits
(test-binary-buffer-number-set! 8 binary-pre-allocated-buffer-set-u64!
				bytevector-u64-ref 5 (endianness big))
(test-binary-buffer-number-set! 8 binary-pre-allocated-buffer-set-s64!
				bytevector-s64-ref 5 (endianness big))

;; float
(test-binary-buffer-number-set! 4 binary-pre-allocated-buffer-set-f32!
				bytevector-ieee-single-ref 5.0 (endianness big))

;; double
(test-binary-buffer-number-set! 8 binary-pre-allocated-buffer-set-f64!
				bytevector-ieee-double-ref 5.0 (endianness big))

;; set-bytevector!
(let* ((bv (make-bytevector 10))
       (buf (make-binary-pre-allocated-buffer bv)))
  (test-assert "set-bytevector!(1)" 
	       (binary-pre-allocated-buffer-set-bytevector! buf 0 #vu8(1 2 3 4 5)))
  (test-equal "size" 5 (pre-allocated-buffer-size buf))
  (test-equal "crop" #vu8(1 2 3 4 5) (crop-binary-buffer buf))
  
  (test-assert "set-bytevector!(2)" 
	       (binary-pre-allocated-buffer-set-bytevector! buf 0 #vu8(6 7 8)))
  (test-equal "size" 5 (pre-allocated-buffer-size buf))
  (test-equal "crop" #vu8(6 7 8 4 5) (crop-binary-buffer buf))
  (test-error "overflow" pre-allocated-buffer-overflow? 
	      (binary-pre-allocated-buffer-set-bytevector! buf 8 
							   #vu8(10 11 12)))
  (test-assert "expand" 
	       (binary-pre-allocated-buffer-set-bytevector! buf 5
							    #vu8(10 11 12)))
  (test-equal "size" 8 (pre-allocated-buffer-size buf))
  (test-equal "crop" #vu8(6 7 8 4 5 10 11 12) (crop-binary-buffer buf))
)

(let* ((bv (make-bytevector 10))
       (buf (make-binary-pre-allocated-buffer bv))
       (out (->binary-pre-allocated-buffer-output-port buf)))
  (test-assert "put-bytevector (0)" (put-bytevector out #vu8(1 2 3 4 5)))
  (test-assert "can-store?" (binary-pre-allocated-buffer-can-store? buf 5))
  (test-assert "put-bytevector (1)" (put-bytevector out #vu8(1 2 3 4 5)))
  (test-assert "can-store?" 
	       (not (binary-pre-allocated-buffer-can-store? buf 1)))
  (test-error "overflow" pre-allocated-buffer-overflow? (put-u8 out 1))
  
  (test-equal "crop" #vu8(1 2 3 4 5 1 2 3 4 5) (crop-binary-buffer buf))
  (test-assert "reset!" (pre-allocated-buffer-reset! buf))
  (test-assert "can-store?" (binary-pre-allocated-buffer-can-store? buf 5)))

(test-end)
