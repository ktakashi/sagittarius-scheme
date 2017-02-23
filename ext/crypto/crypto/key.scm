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

	    keypair?
	    keypair-private
	    keypair-public
	    <private-key>
	    private-key?
	    <public-key>
	    public-key?

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
	    (crypto key des)
	    (crypto key component)))
