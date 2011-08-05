;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; pkcs.scm Cryptographic library
;;; 
(library (crypto pkcs)
    (export ;; padder
	    pkcs5-padder)
    (import (rnrs)
	    (rnrs r5rs)
	    (math helper)
	    (sagittarius crypto))

  ;; PKCS #5 padding.
  ;; reference http://www.rsa.com/rsalabs/node.asp?id=2127
  (define (pkcs5-padder bv block-size pad?)
    (if pad?
	(let* ((len (bytevector-length bv))
	       (mod (modulo len block-size))
	       (padding (- block-size mod)))
	  (when (zero? padding)
	    (set! padding 8))
	  (let ((new (make-bytevector (+ len padding) 0)))
	    ;; lazyness
	    (bytevector-fill! new padding)
	    (bytevector-copy! bv 0 new 0 len)
	    new))
	(let* ((len (bytevector-length bv))
	       (pad (bytevector-u8-ref bv (- len 1)))
	       (new (make-bytevector (- len pad) 0)))
	  (bytevector-copy! bv 0 new 0 (- len pad))
	  new)))
)
