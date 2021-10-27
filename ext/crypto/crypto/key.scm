;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; key.scm Cryptographic library
;;; 
#!core
(library (crypto key)
    (export key?
	    symmetric-key?
	    asymmetric-key?
	    generate-secret-key
	    symmetric-key-raw-key
	    
	    generate-key-pair
	    generate-private-key
	    generate-public-key

	    keypair? make-keypair
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
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto)
	    (crypto key pair)
	    (crypto key des)
	    (crypto key component))

(define-generic symmetric-key-raw-key)

;; for now, I think we should add key->bytevector or something for more
;; completion
(define-method symmetric-key-raw-key ((key <builtin-symmetric-key>))
  (builtin-symmetric-key-raw-key key))

(define-method symmetric-key-raw-key ((key <symmetric-key>))
  (error 'symmetric-key-raw-key "The given custom key must implement the method"
	 key))

  )
