;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; key.scm Cryptographic library
;;; 
(library (crypto key)
    (export key?
	    generate-secret-key
	    generate-key-pair
	    generate-private-key
	    generate-public-key
	    RSA
	    keypair-private
	    keypair-public
	    private-key
	    private-key?
	    public-key
	    public-key?
	    ;; cipher
	    public-key-cipher)
    (import (core)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius crypto)
	    (crypto random)
	    (crypto key pair)
	    (crypto key rsa))

  (define *keypair-generators* (make-eq-hashtable))
  (define *private-key-generators* (make-eq-hashtable))

  (define RSA 'RSA)

  (define-with-key (generate-key-pair type :key (size 1024) (prng (pseudo-random RC4)) (e 65537))
    (case type
      ((RSA) (rsa-generate-key-pair size prng e))
      (else
       (cond ((hashtable-ref *keypair-generators* type #f)
	      => (lambda (generator)
		   (generator size prng)))
	     (else
	      (assertion-violation 'generate-key-pair
				   (format "~a is not supporeted" type)))))))

  (define (generate-private-key type . opt)
    (case type
      ((RSA) (apply rsa-generate-private-key opt))
      (else
       (cond ((hashtable-ref *private-key-generators* type #f)
	      => (lambda (generator)
		   (generator size prng)))
	     (else
	      (assertion-violation 'generate-private-key
				   (format "~a is not supporeted" type)))))))

  (define (generate-public-key type . opt)
    (case type
      ((RSA) (apply rsa-generate-public-key opt))
      (else
       (assertion-violation 'generate-private-key
			    (format "~a is not supporeted" type)))))

  (define-with-key (public-key-cipher type key :key (prng (pseudo-random RC4))
				               :allow-other-keys rest)
    (case type
      ((RSA) (apply rsa-cipher key prng rest))
      (else
       (assertion-violation 'public-key-cipher
			    (format "~a is not supporeted" type)))))
)
