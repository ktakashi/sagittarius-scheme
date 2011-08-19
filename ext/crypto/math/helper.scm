;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; helper.scm math library
;;; 
(library (math helper)
    (export bytevector->integer
	    integer->bytevector
	    align-size)
    (import (rnrs) (sagittarius))

;; moved to builtin function  
;;  (define (bytevector->integer bv)
;;    (let ((len (bytevector-length bv)))
;;      (do ((i len (- i 1))
;;	   (ans 0 (+ ans (bitwise-arithmetic-shift (bytevector-u8-ref bv (- i 1))
;;						   (* (- len i) 8)))))
;;	  ((= i 0) ans))))

  (define-syntax align-size
    (syntax-rules (bit)
      ((_ (bit n))
       (let ((bitlen n))
	 (+ (bitwise-arithmetic-shift-right bitlen 3)
	    (if (zero? (bitwise-and bitlen 7)) 0 1))))
      ((_ n)
       (let ((bitlen (bitwise-length n)))
	 (+ (bitwise-arithmetic-shift-right bitlen 3)
	    (if (zero? (bitwise-and bitlen 7)) 0 1))))))

;; moved to builtin function  
;;   (define (integer->bytevector int)
;;     (let* ((len (align-size int))
;;	    (bv (make-bytevector len 0)))
;;       (do ((int int (bitwise-arithmetic-shift-right int 8))
;;	    (i (- len 1) (- i 1)))
;;	   ((< i 0) bv)
;;	 (bytevector-u8-set! bv i (bitwise-and int #xFF)))))
 )
