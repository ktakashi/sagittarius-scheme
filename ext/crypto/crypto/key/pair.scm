;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; pair.scm Cryptographic library
;;; key pair
#!deprecated
#!nounbound
(library (crypto key pair)
    (export (rename (<key-pair> <<keypair>)
		    (make-key-pair make-keypair)
		    (key-pair? keypair?)
		    (key-pair-private keypair-private)
		    (key-pair-public keypair-public))
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
    (import (sagittarius crypto keys types)
	    (sagittarius crypto keys operations asymmetric apis)))
