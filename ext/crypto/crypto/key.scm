;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; key.scm Cryptographic library
;;; 
#!core
(library (crypto key)
    (export (rename (crypto-key? key?))
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
	    (rename (<crypto-key> <key>))
	    <symmetric-key>
	    <asymmetric-key>
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

	    ;; key agreement
	    calculate-key-agreement
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys)
	    (asn.1)
	    (crypto key pair)
	    (crypto key des)
	    (crypto key component)
	    (crypto key agreement))

(define-generic symmetric-key-raw-key)

(define-method symmetric-key-raw-key ((key <symmetric-key>))
  (symmetric-key-value key))

;; TODO remove after ASN.1 library migration otherwise it'd be a infinte loop
(define-method import-private-key (m (o <asn.1-sequence>))
  (import-private-key m (asn.1-encode o)))
(define-method import-public-key (m (o <asn.1-sequence>))
  (import-public-key m (asn.1-encode o)))
;; Damn...
(define-method import-private-key (m (o <asn.1-sequence>)
				     (id <asn.1-encodable>))
  (import-private-key m (asn.1-encode o)
		      (bytevector->asn1-object (asn.1-encode id))))

)
