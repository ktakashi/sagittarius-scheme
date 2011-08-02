;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; key.scm Cryptographic library
;;; 
(library (crypto key)
    (export key?
	    generate-secret-key
	    generate-key-pair
	    RSA
	    keypair-private
	    keypair-public
	    private-key
	    private-key?
	    public-key
	    public-key?)
    (import (core)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius crypto)
	    (crypto key pair)
	    (crypto key rsa))

  (define *key-generators* (make-eq-hashtable))

  (define RSA 'RSA)

  (define-with-key (generate-key-pair type :key (size 1024))
    (case type
      ((RSA) (rsa-generate-key-pair size))
      (else
       (cond ((hashtable-ref *key-generators* type #f)
	      => (lambda (generator)
		   (generator length)))
	     (else
	      (assertion-violation 'generate-key-pair
				   (format "~a is not supporeted" type)))))))
    

)
