;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; crypto/mac - MAC interface library
;;; 
(library (crypto mac)
    (export <mac> verify-mac)
    (import (rnrs)
	    (clos user)
	    (math hash))
  ;; super class
  ;; MAC is basically digest of the message so it's the same as
  ;; hash algorithm. so this can be a subclass of hash algorithm.
  (define-class <mac> (<user-hash-algorithm>) ())

  (define-generic verify-mac)
  ;; object message sign.
  ;; The reason why this is a method is to make space for implementation
  ;; specific stuff. (AFAIK there is non though)
  (define-method verify-mac (mac M S . opt)
    (let ((T (apply hash mac M opt)))
      (or (bytevector=? T S) 
	  (error 'verify-mac "Invalid MAC"))))
)