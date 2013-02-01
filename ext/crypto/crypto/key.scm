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
	    (crypto key component)))
