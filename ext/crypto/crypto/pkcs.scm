;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; pkcs.scm Cryptographic library
;;;
#!deprecated
#!nounbound
(library (crypto pkcs)
    (export (rename (pkcs7-padding pkcs5-padder)
		    (no-padding no-padder))
	    ;; encoder
	    pkcs1-emsa-pss-encode
	    pkcs1-emsa-pss-verify
	    pkcs1-emsa-v1.5-encode
	    pkcs1-emsa-v1.5-verify
	    mgf-1)
    (import (rnrs)
	    (sagittarius crypto ciphers symmetric)
	    (sagittarius crypto signatures)))
