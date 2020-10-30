;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dsig/algorithms.scm - XML signature algorithms
;;;
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;;; reference
;;; XML Signature Syntax and Processing Version 1.1
;;; Section 6: Algorithms
;;; - URL https://www.w3.org/TR/xmldsig-core1/
(library (text xml dsig algorithms)
    (export ds:method-uri
	    ds:digest-method-algorithm
	    
	    ds:canonicalize-method-options
	    
	    ds:signature-method-cipher
	    ds:signature-method-digest

	    *xmldsig:digest-sha1*
	    *xmldsig:digest-sha256*
	    *xmldsig:digest-sha224*
	    *xmldsig:digest-sha384*
	    *xmldsig:digest-sha512*

	    *xmldsig:rsa-sha1*
	    *xmldsig:rsa-sha256*
	    *xmldsig:rsa-sha224*
	    *xmldsig:rsa-sha384*
	    *xmldsig:rsa-sha512*

	    *xmldsig:canonicalization-c14n*
	    *xmldsig:canonicalization-c14n11*
	    *xmldsig:canonicalization-exc-c14n*
	    *xmldsig:canonicalization-c14n-w/comment*
	    *xmldsig:canonicalization-c14n11-w/comment*
	    *xmldsig:canonicalization-exc-c14n-w/comment*
	    )
    (import (rnrs)
	    (crypto)
	    (math)
	    (text xml dom writer))
;; accesors
;; we define them so that in the future we may want to use records
(define ds:method-uri car)
(define ds:canonicalize-method-options cadr)
(define ds:digest-method-algorithm cadr)
(define ds:signature-method-cipher cadr)
(define ds:signature-method-digest caddr)

;;; Digestt
(define *xmldsig:digest-sha1*
  `("http://www.w3.org/2000/09/xmldsig#sha1" ,SHA-1))
(define *xmldsig:digest-sha256*
  `("http://www.w3.org/2001/04/xmlenc#sha256" ,SHA-256))
(define *xmldsig:digest-sha224*
  `("http://www.w3.org/2001/04/xmldsig-more#sha224" ,SHA-224))
(define *xmldsig:digest-sha384*
  `("http://www.w3.org/2001/04/xmldsig-more#sha224" ,SHA-384))
(define *xmldsig:digest-sha512*
  `("http://www.w3.org/2001/04/xmlenc#sha512" ,SHA-512))

;;; Encoding
;; TBD

;;; MAC
;; TBD

;;; Signature
;; We don't support DSA and ECDSA yet
(define *xmldsig:rsa-sha1*
  `("http://www.w3.org/2000/09/xmldsig#rsa-sha1" ,RSA ,SHA-1))
(define *xmldsig:rsa-sha256*
  `("http://www.w3.org/2001/04/xmldsig-more#rsa-sha256" ,RSA ,SHA-256))
(define *xmldsig:rsa-sha224*
  `("http://www.w3.org/2001/04/xmldsig-more#rsa-sha224" ,RSA ,SHA-224))
(define *xmldsig:rsa-sha384*
  `("http://www.w3.org/2001/04/xmldsig-more#rsa-sha384" ,RSA ,SHA-384))
(define *xmldsig:rsa-sha512*
  `("http://www.w3.org/2001/04/xmldsig-more#rsa-sha512" ,RSA ,SHA-512))

;;; Canonicalization
(define *xmldsig:canonicalization-c14n*
  `("http://www.w3.org/TR/2001/REC-xml-c14n-20010315" ,*xml:c14n*))
(define *xmldsig:canonicalization-c14n11*
  `("http://www.w3.org/2006/12/xml-c14n11" ,*xml:c14n*))
(define *xmldsig:canonicalization-exc-c14n*
  `("http://www.w3.org/2001/10/xml-exc-c14n#" ,*xml:exc-c14n*))
(define *xmldsig:canonicalization-c14n-w/comment*
  `("http://www.w3.org/TR/2001/REC-xml-c14n-20010315#WithComments"
    ,*xml:c14n-w/comment*))
(define *xmldsig:canonicalization-c14n11-w/comment*
  `("http://www.w3.org/2006/12/xml-c14n11#WithComments"
    ,*xml:c14n-w/comment*))
(define *xmldsig:canonicalization-exc-c14n-w/comment*
  `("http://www.w3.org/2001/10/xml-exc-c14n#WithComments"
    ,*xml:exc-c14n-w/comment*))

;;; Transform
;; TBD

)
