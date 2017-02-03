;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; key.scm Cryptographic library
;;; 
#!core
(library (crypto key)
    (export key?
	    generate-secret-key
	    generate-key-pair
	    generate-private-key
	    generate-public-key
	    RSA

	    <rsa-private-key> <rsa-private-crt-key>
	    <rsa-public-key>

	    keypair?
	    keypair-private
	    keypair-public
	    <private-key>
	    private-key?
	    <public-key>
	    public-key?
	    ;; padding
	    pkcs-v1.5-padding
	    PKCS-1-EME
	    PKCS-1-EMSA

	    rsa-oaep-padding
	    ;; DSA
	    DSA
	    generate-dsa-parameter
	    <dsa-private-key>
	    <dsa-public-key>

	    ;; ECDSA
	    ECDSA
	    <ecdsa-private-key>
	    <ecdsa-public-key>

	    ;; NIST parameters
	    NIST-P-192
	    NIST-P-224
	    NIST-P-256

	    ;; SEC 2 parameters
	    sect113r1
	    sect163k1
	    secp192r1 ;; the same as NIST-P-192
	    secp224r1 ;; the same as NIST-P-224
	    secp256r1 ;; the same as NIST-P-256
	    
	    ;; external representive
	    export-public-key
	    export-private-key
	    import-public-key
	    import-private-key

	    ;; key components
	    split-key
	    combine-key-components
	    combine-key-components!
	    )
    (import (sagittarius crypto)
	    (math random)
	    (crypto key pair)
	    (crypto key rsa)
	    (crypto key dsa)
	    (crypto key des)
	    (crypto key ecdsa)
	    (crypto key component)))
