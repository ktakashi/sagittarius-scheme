;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/cmac.scm - Cipher-based Message Authentication Code library.
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

;; references
;;  RFC 4493 - http://tools.ietf.org/html/rfc4493
;;  RFC 4494 - http://tools.ietf.org/html/rfc4494

(library (rfc cmac)
    (export CMAC <cmac> verify-mac)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (crypto)
	    (math)
	    (clos user)
	    (srfi :1 lists)
	    (util bytevector))

  ;; do nothing
  ;; TODO should we generate k1 and k2 here?
  (define (cmac-init cmac)
    (set! (~ cmac 'buffer) (~ cmac 'zero))
    (set! (~ cmac 'last) #vu8()))

  (define (cmac-process cmac in)
    ;; split input message to block size.
    (define (split-message cmac in)
      (let1 in (bytevector-append (~ cmac 'last) in)
	(if (zero? (bytevector-length in))
	    (values '() (list in))
	    (let1 m* (bytevector-slices in (~ cmac 'block-size))
	      (split-at m* (- (length m*) 1))))))

    (let ((X  (~ cmac 'buffer)))
      (let-values (((M* M-t) (split-message cmac in)))
	(unless (null? M*)
	  (let1 cipher (~ cmac 'cipher)
	    (set! (~ cmac 'buffer)
		  (fold-left (lambda (X M)
			       (let1 Y (bytevector-xor X M)
				 (encrypt cipher Y))) X M*))))
	(set! (~ cmac 'last) (car M-t)))))

  (define (cmac-done cmac out)
    ;; generate sub key
    (define (MSB bv)
      (let1 msb (bytevector-u8-ref bv 0)
	(zero? (bitwise-and msb #x80))))
    (define (derive-key L len const-rb)
      (let* ((i (bitwise-arithmetic-shift-left (bytevector->integer L 0 len) 1))
	     (bv (integer->bytevector i len)))
	(if (MSB L)
	    bv
	    (bytevector-xor bv const-rb))))
    ;; r = octets of x
    ;; padding(x) = x || 10^i      where i is 128-8*r-1
    (define (padding x)
      ;; TODO better implementation
      (let* ((size (~ cmac 'block-size))
	     (r (bytevector-length x))
	     (i (- (* size 8) (* 8 r) 1))
	     (p (integer->bytevector (expt 2 i))))
	(bytevector-append x p)))
    (define (last-block cmac)
      (let* ((rb (~ cmac 'rb))
	     (len (~ cmac 'block-size))
	     (k1 (derive-key (encrypt (~ cmac 'cipher) (~ cmac 'zero)) len rb))
	     (k2 (derive-key k1 len rb))
	     (last (~ cmac 'last)))
	(if (or (zero? (bytevector-length last))
		(not (zero? (mod (bytevector-length last) len))))
	    (bytevector-xor (padding last) k2)
	    (bytevector-xor last k1))))
    (let* ((last (last-block cmac))
	   (T (encrypt (~ cmac 'cipher)
		       (bytevector-xor (~ cmac 'buffer) last))))
      (bytevector-copy! T 0 out 0 (min (~ cmac 'hash-size) 
				       (bytevector-length out)))
      (set! (~ cmac 'buffer) #f)
      (set! (~ cmac 'last) #vu8())
      out))

  (define-class <cmac> (<mac>)
    ((cipher :init-keyword :cipher)
     ;; const zero
     (zero)
     ;; const rb
     (rb)
     ;; result buffer
     (buffer)
     ;; previous processed last message
     (last)))
  (define-method initialize ((o <cmac>) initargs)
    (call-next-method)
    (let1 cipher (~ o 'cipher)
      (let-keywords initargs
	  ((size (cipher-blocksize cipher))
	   . others)
	(let* ((len (cipher-blocksize cipher))
	       (const-zero (make-bytevector len 0))
	       (const-rb   (make-bytevector len 0)))
	  (bytevector-u8-set! const-rb (- len 1) #x87)
	  (set! (~ o 'zero) const-zero)
	  (set! (~ o 'rb) const-rb)
	  ;; hash operations
	  (set! (~ o 'init) cmac-init)
	  (set! (~ o 'process) cmac-process)
	  (set! (~ o 'done) cmac-done)
	  (set! (~ o 'block-size) len)
	  (set! (~ o 'hash-size) size))))
    o)

  (define-method write-object ((o <cmac>) out)
    (format out "#<cmac ~a>" (~ o 'cipher)))

  (define-class <cmac-marker> () ())
  (define CMAC (make <cmac-marker>))
  (register-hash CMAC <cmac>)
  ;; only for verify
  (register-spi CMAC <cmac>)

)