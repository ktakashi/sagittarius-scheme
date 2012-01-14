;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; key.scm Cryptographic library
;;; 
#!compatible
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
	    public-key-cipher
	    ;; padding
	    pkcs-v1.5-padding
	    PKCS-1-EME
	    PKCS-1-EMSA)
    (import (core)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius crypto)
	    (math random)
	    (crypto key pair)
	    (crypto key rsa))

  (define RSA 'RSA)

  (define-with-key (generate-key-pair type
				      :key (size 1024) 
				           (prng (secure-random RC4))
					   (e 65537))
    (case type
      ((RSA) (rsa-generate-key-pair size prng e))
      (else
       (assertion-violation 'generate-key-pair
			    (format "~a is not supporeted" type)))))

  (define (generate-private-key type . opt)
    (case type
      ((RSA) (apply rsa-generate-private-key opt))
      (else
       (assertion-violation 'generate-private-key
			    (format "~a is not supporeted" type)))))

  (define (generate-public-key type . opt)
    (case type
      ((RSA) (apply rsa-generate-public-key opt))
      (else
       (assertion-violation 'generate-private-key
			    (format "~a is not supporeted" type)))))

  (define-with-key (public-key-cipher type key :key (prng (secure-random RC4))
				               :allow-other-keys rest)
    (case type
      ((RSA) (apply rsa-cipher key prng rest))
      (else
       (assertion-violation 'public-key-cipher
			    (format "~a is not supporeted" type)))))

)
