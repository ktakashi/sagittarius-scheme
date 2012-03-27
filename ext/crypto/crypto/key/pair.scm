;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; pair.scm Cryptographic library
;;; key pair
(library (crypto key pair)
    (export make-keypair
	    keypair?
	    keypair-private
	    keypair-public
	    <keypair>
	    ;;
	    <private-key>
	    private-key?
	    <public-key>
	    public-key?

	    ;; generic methods
	    generate-key-pair
	    generate-private-key
	    generate-public-key
	    export-public-key
	    export-private-key
	    import-public-key
	    import-private-key
	    )
    (import (rnrs)
	    (clos user)
	    ;;(clos core)
	    (sagittarius crypto))

  (define-class <private-key> (<asymmetric-key>) ())
  (define-class <public-key> (<asymmetric-key>) ())
  (define-class <keypair> ()
    ((private :init-keyword :private)
     (public  :init-keyword :public)))

  (define (private-key? o) (is-a? o <private-key>))
  (define (public-key? o) (is-a? o <public-key>))
  
  (define (keypair? o) (is-a? o <keypair>))
  (define (make-keypair private public)
    (make <keypair> :private private :public public))
  (define (keypair-private keypair) (slot-ref keypair 'private))
  (define (keypair-public keypair) (slot-ref keypair 'public))


  (define-generic generate-key-pair)
  (define-generic generate-private-key)
  (define-generic generate-public-key)
  (define-generic export-public-key)
  (define-generic export-private-key)
  (define-generic import-public-key)
  (define-generic import-private-key)
)