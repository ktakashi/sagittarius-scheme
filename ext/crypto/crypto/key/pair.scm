;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; pair.scm Cryptographic library
;;; key pair
(library (crypto key pair)
    (export make-keypair
	    keypair-private
	    keypair-public
	    ;;
	    private-key
	    private-key?
	    public-key
	    public-key?)
    (import (rnrs))

  ;; interface for private/public keys
  (define-record-type private-key)
  (define-record-type public-key)

  (define-record-type keypair
    (fields (immutable private)
	    (immutable public)))
)