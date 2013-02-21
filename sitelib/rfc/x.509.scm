;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/x.509 - X.509 certificate utility library.
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

(library (rfc x.509)
    (export make-x509-certificate
	    x509-certificate?
	    x509-certificate->bytevector
	    <x509-certificate>
	    x509-certificate-get-version
	    x509-certificate-get-serial-number
	    x509-certificate-get-issuer-dn
	    x509-certificate-get-subject-dn
	    x509-certificate-get-not-before
	    x509-certificate-get-not-after
	    x509-certificate-get-signature
	    x509-certificate-get-signature-algorithm
	    x509-certificate-get-public-key
	    verify
	    check-validity
	    ;; certificate generation
	    make-x509-issuer
	    make-validity
	    make-x509-basic-certificate)
    (import (rnrs)
	    (rnrs mutable-strings)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (rename (crypto) (verify crypto:verify))
	    (srfi :19 time)
	    (srfi :26 cut)
	    (math)
	    (asn.1))
  ;; these might be somewhere
  (define-class <rdn> (<asn.1-encodable>)
    ((values :init-keyword :values)))
  (define-method make-rdn ((s <asn.1-set>))
    (make <rdn> :values s))
  (define-method make-rdn ((oid <der-object-identifier>) (name <string>))
    (let1 seq (make-der-sequence oid (make-der-printable-string name #t))
      (make <rdn> :values (make-der-set seq))))
  (define (rdn-get-types-and-values rdn)
    (map (lambda (p)
	   (cons (asn.1-sequence-get p 0) (asn.1-sequence-get p 1)))
	 (~ rdn 'values 'set)))
  (define (rdn-get-first rdn)
    (let ((p (asn.1-set-get (~ rdn 'values) 0)))
      (cons (asn.1-sequence-get p 0) (asn.1-sequence-get p 1))))
  (define *default-symbols*
    `((,(make-der-object-identifier "2.5.4.6") . C)
      (,(make-der-object-identifier "2.5.4.10") . O)
      (,(make-der-object-identifier "2.5.4.11") . OU)
      (,(make-der-object-identifier "2.5.4.3") . CN)
      (,(make-der-object-identifier "2.5.4.7") . L)
      (,(make-der-object-identifier "2.5.4.5") . SERIALNUMER)
      (,(make-der-object-identifier "1.2.840.113549.1.9.1") . E)
      (,(make-der-object-identifier "0.9.2342.19200300.100.1.25") . DC)
      (,(make-der-object-identifier "0.9.2342.19200300.100.1.1") . UID)
      (,(make-der-object-identifier "2.5.4.9") . STREET)
      (,(make-der-object-identifier "2.5.4.4") . SURNAME)
      (,(make-der-object-identifier "2.5.4.44") . GENERATION)
      (,(make-der-object-identifier "2.5.4.26") . DN)
      (,(make-der-object-identifier "2.5.4.42") . GIVENNAME)))

  (define-class <x500-name> (<asn.1-encodable>)
    ((style :init-keyword :style :init-value #f)
     (rdns  :init-keyword :rdns)))
  (define-method make-x500-name ((s <asn.1-sequence>))
    (let* ((len (asn.1-sequence-size s))
	   (rdns (make-vector len)))
      (dotimes (i len)
	(vector-set! rdns i (make-rdn (asn.1-sequence-get s i))))
      (make <x500-name> :rdns rdns)))
  (define-method asn.1-encodable->asn.1-object ((o <x500-name>))
    (apply make-der-sequence (vector->list (~ o 'rdns))))
  (define-method write-object ((o <x500-name>) (p <port>))
    (define (print-type-and-value p out)
      (let ((type (car p))
	    (value (cdr p)))
	(cond ((assoc type *default-symbols*) =>
	       (lambda (slot) (display (cdr slot) out)))
	      (else
	       (display (~ type 'identifier) out)))
	(display #\= out)
	(if (and (is-a? value <asn.1-string>)
		 (not (is-a? value <der-universal-string>)))
	    (display (asn.1-string->string value) out)
	    (display value out))))
    (define (print-rdn rdn out) 
      (let* ((set (~ rdn 'values))
	     (len (asn.1-set-size set)))
	(if (> len 1)
	    (let ((atv (rdn-get-types-and-values rdn)))
	      (unless (null? atv)
		(print-type-and-value (car atv) out)
		(for-each (lambda (at)
			    (display #\, out)
			    (print-type-and-value at out)) (cdr atv))))
	    (print-type-and-value (rdn-get-first rdn) out))))
    (let ((buf (call-with-string-output-port
		(lambda (out)
		  (let ((rdns (vector->list (~ o 'rdns))))
		    (unless (null? rdns)
		      (print-rdn (car rdns) out)
		      (for-each (lambda (rdn)
				  (display #\, out)
				  (print-rdn rdn out))
				(cdr rdns))))))))
      (display buf p)))

  ;; base on boucycasle's x509 package.
  (define-class <algorithm-identifier> (<asn.1-encodable>)
    ((object-id  :init-keyword :object-id)
     (parameters :init-keyword :parameters :init-value #f)
     (parameters-defined? :init-keyword :defined? :init-value #f)))
  (define-method make-algorithm-identifier ((o <der-object-identifier>))
    (make <algorithm-identifier> :object-id o))
  (define-method make-algorithm-identifier ((s <asn.1-sequence>))
    (let ((len (asn.1-sequence-size s)))
      (unless (<= 1 len 2)
	(assertion-violation 'make-algorithm-identifier
			     "bad sequence size" len))
      (if (= len 2)
	  (make <algorithm-identifier>
	    :object-id (asn.1-sequence-get s 0)
	    :parameters (asn.1-sequence-get s 1)
	    :defined? #t)
	  (make <algorithm-identifier>
	    :object-id (asn.1-sequence-get s 0)))))
  (define-method get-id ((id <algorithm-identifier>))
    (~ id 'object-id 'identifier))
  (define-method der-encode ((id <algorithm-identifier>) out)
    (if (~ id 'parameters-defined?)
	(der-encode (make-der-sequence (~ id 'object-id) 
				       (~ id 'parameters)) out)
	(der-encode (make-der-sequence (~ id 'object-id) (make-der-null)) out)))

  (define-class <x509-principal> (<x500-name>) ())
  (define-method make-x509-principal ((o <x500-name>))
    (make <x509-principal> :rdns (~ o 'rdns)))

  (define-class <x509-time> (<asn.1-encodable>)
    ((time :init-keyword :time)))
  (define-method make-x509-time ((o <der-utc-time>))
    (make <x509-time> :time o))
  (define-method make-x509-time ((o <der-generalized-time>))
    (make <x509-time> :time o))
  (define-method x509-time->date ((o <x509-time>))
    (der-time->date (~ o 'time)))

  (define-class <x509-extension> (<asn.1-encodable>)
    ((critical :init-keyword :critical)
     (value    :init-keyword :value)))
  (define-method make-x509-extension ((critical <der-boolean>)
				      (value <asn.1-octet-string>))
    (make-x509-extension (der-boolean->boolean critical) value))
  (define-method make-x509-extension ((critical <boolean>)
				      (value <asn.1-octet-string>))
    (make <x509-extension> :critical critical :value value))

  (define-class <x509-extensions> (<asn.1-encodable>)
    ((extensions :init-keyword :extensions)))
  (define-method make-x509-extensions ((o <asn.1-tagged-object>))
    (make-x509-extensions (der-encodable->der-object o)))
  (define-method make-x509-extensions ((o <asn.1-sequence>))
    (let ((extensions 
	   (map (lambda (e)
		  (case (asn.1-sequence-size e)
		    ((3)
		     (cons (asn.1-sequence-get e 0)
			   (make-x509-extension (asn.1-sequence-get e 1)
						(asn.1-sequence-get e 2))))
		    ((2)
		     (cons (asn.1-sequence-get e 0)
			   (make-x509-extension #f (asn.1-sequence-get e 1))))
		    (else
		     => (lambda (size)
			  (assertion-violation 'make-x509-extensions
					       "bas sequense size" size)))))
		(~ o 'sequence))))
      (make <x509-extensions> :extensions extensions)))
  (define-method get-x509-extension ((o <x509-extensions>)
				     (oid <der-object-identifier>))
    (and-let* ((ext (assoc oid (~ o 'extensions))))
      (cdr ext)))


  (define-class <subject-public-key-info> (<asn.1-encodable>)
    ((algorithm-identifier :init-keyword :algorithm-identifier)
     (key-data :init-keyword :key-data)))
  (define-method make-subject-public-key-info ((s <asn.1-sequence>))
    ;; TODO check length
    (make <subject-public-key-info>
      :algorithm-identifier (make-algorithm-identifier (asn.1-sequence-get s 0))
      :key-data (asn.1-sequence-get s 1)))

  (define-class <tbs-certificate-structure> (<asn.1-encodable>)
    ((sequence      :init-keyword :sequence) ;; original asn.1 object
     (version       :init-keyword :version)
     (serial-number :init-keyword :serial-number)
     (signature     :init-keyword :signature)
     (issuer        :init-keyword :issuer)
     (start-date    :init-keyword :start-date)
     (end-date      :init-keyword :end-date)
     (subject       :init-keyword :subject)
     (subject-public-key-info :init-keyword :public-key-info)
     (issuer-unique-id :init-keyword :issuer-id)
     (subject-unique-id :init-keyword :subject-id)
     (extensions    :init-keyword :extensions)))
  (define-method make-tbs-certificate-structure ((s <asn.1-sequence>))
    (let* ((start 0)
	   (version (cond ((is-a? (asn.1-sequence-get s 0) <der-tagged-object>)
			   (make-der-integer (asn.1-sequence-get s 0) #t))
			  (else
			   (set! start -1)
			   (make-der-integer 0))))
	   (serial-number (asn.1-sequence-get s (+ start 1)))
	   (signature (make-algorithm-identifier
		       (asn.1-sequence-get s (+ start 2))))
	   (issuer (make-x500-name (asn.1-sequence-get s (+ start 3))))
	   (dates (asn.1-sequence-get s (+ start 4)))
	   (start-date (make-x509-time (asn.1-sequence-get dates 0)))
	   (end-date (make-x509-time (asn.1-sequence-get dates 1)))
	   (subject (make-x500-name (asn.1-sequence-get s (+ start 5))))
	   (public-key (make-subject-public-key-info
			(asn.1-sequence-get s (+ start 6))))
	   (issuer-id #f)
	   (subject-id #f)
	   (extensions #f))
      (do ((extras (- (asn.1-sequence-size s) (+ start 6) 1) (- extras 1)))
	  ((zero? extras))
	(let ((extra (asn.1-sequence-get s (+ start 6 extras))))
	  ;; assume tagged object
	  (case (~ extra 'tag-no)
	    ((1) (set! issuer-id (make-der-bit-string extra #f)))
	    ((2) (set! subject-id (make-der-bit-string extra #f)))
	    ((3) (set! extensions (make-x509-extensions extra))))))
      (make <tbs-certificate-structure>
	:sequence s
	:version version
	:serial-number serial-number
	:signature signature
	:issuer issuer
	:start-date start-date
	:end-date end-date
	:subject subject
	:public-key-info public-key
	:issuer-id issuer-id
	:subject-id subject-id
	:extensions extensions)))

  (define-class <x509-certificate-structure> (<asn.1-encodable>)
    ((sequence :init-keyword :sequence) ;; original asn.1 object
     (tbs-cert :init-keyword :tbs-cert)
     (algorithm-identifier :init-keyword :algorithm-identifier)
     (signature :init-keyword :signature)))
  (define-method make-x509-certificate-structure ((s <asn.1-sequence>))
    (make <x509-certificate-structure>
      :sequence s
      :tbs-cert (make-tbs-certificate-structure (asn.1-sequence-get s 0))
      :algorithm-identifier (make-algorithm-identifier (asn.1-sequence-get s 1))
      ;; must be der bit string
      :signature (asn.1-sequence-get s 2)))
  (define-method der-encode ((o <x509-certificate-structure>) p)
    (der-encode (~ o 'sequence) p))

  (define-class <basic-constraints> (<asn.1-encodable>)
    ((ca :init-keyword :ca :init-form (make-der-boolean #f))
     (path-length-constraint :init-keyword :path-length-constraint)))
  (define-method make-basic-constrains ((s <asn.1-sequence>))
    (case (asn.1-sequence-size s)
      ((0)
       (make <basic-constraints> :ca #f :path-length-constraint #f))
      (else 
       => (lambda (size)
	    (let ((ca (asn.1-sequence-get s 0))
		  (path-len #f))
	      (unless (is-a? ca <der-boolean>)
		(set! ca #f)
		(set! path-len ca))
	      (when (> size 1)
		(if ca
		    (set! path-len (asn.1-sequence-get s 1))
		    (assertion-violation 'make-basic-constrains
					 "wrong sequence in constructor")))
	      (make <basic-constraints>
		:ca ca
		:path-length-constraint path-len))))))

  ;; should we make <certificate>?
  ;; based on bouncy castle X509CertificateObject
  (define-class <x509-certificate> ()
    ((c :init-keyword :c)
     (basic-constraints :init-keyword :basic-constraints)
     (key-usage :init-keyword :key-usage)))
  (define-method make-x509-certificate ((bv <bytevector>))
    (make-x509-certificate (open-bytevector-input-port bv)))
  (define-method make-x509-certificate ((p <port>))
    (make-x509-certificate (read-asn.1-object p)))
  (define-method make-x509-certificate ((s <asn.1-sequence>))
    ;; TODO constrains and key usage
    (define (get-extension-bytes c oid)
      (let ((exts (~ c 'tbs-cert 'extensions)))
	(if exts
	    (let ((ext (get-x509-extension 
			exts
			(make-der-object-identifier oid))))
	      (if ext
		  (~ ext 'value 'string)
		  #f))
	    #f)))
    (define (read-from-bytes bytes)
      (if bytes
	  (read-asn.1-object (open-bytevector-input-port bytes))
	  #f))
    (let* ((c (make-x509-certificate-structure s))
	   (constraints (cond
			 ((read-from-bytes (get-extension-bytes c "2.5.29.19"))
			  => make-basic-constrains)
			 (else #f)))
	   (key-usage (cond
		       ((read-from-bytes (get-extension-bytes c "2.5.29.15"))
			=> (lambda (obj)
			     ;; obj must be der-bit-string
			     (let* ((bytes (~ obj 'data))
				    (len (- (* (bytevector-length bytes) 8)
					    (~ obj 'padding-bits)))
				    (ks (make-vector (if (< len 9) 9 len) #f)))
			       (dotimes (i len)
				 (let ((byte (bytevector-u8-ref bytes 
								(div i 8)))
				       (mask (bitwise-arithmetic-shift-right
					      #x80 (mod i 8))))
				   (vector-set! 
				    ks
				    i
				    (not (zero?
					  (bitwise-and byte mask))))))
			       ks)
			     ))
		       (else #f))))
      (make <x509-certificate>
	:c c
	:basic-constraints constraints
	:key-usage key-usage)))
  (define (x509-certificate? o) (is-a? o <x509-certificate>))
  (define (x509-certificate->bytevector o) (encode (~ o 'c)))

  ;; accessor
  (define (x509-certificate-get-version cert)
    (let ((c (~ cert 'c 'tbs-cert)))
      (+ 1 (der-integer->integer (~ c 'version)))))
  (define (x509-certificate-get-serial-number cert)
    (let ((c (~ cert 'c 'tbs-cert)))
      (der-integer->integer (~ c 'serial-number))))
  (define (x509-certificate-get-issuer-dn cert)
    (let ((c (~ cert 'c 'tbs-cert)))
      (make-x509-principal (~ c 'issuer))))
  (define (x509-certificate-get-subject-dn cert)
    (let ((c (~ cert 'c 'tbs-cert)))
      (make-x509-principal (~ c 'subject))))
  (define (x509-certificate-get-not-before cert)
    (let ((c (~ cert 'c 'tbs-cert)))
      (x509-time->date (~ c 'start-date))))
  (define (x509-certificate-get-not-after cert)
    (let ((c (~ cert 'c 'tbs-cert)))
      (x509-time->date (~ c 'end-date))))
  (define (x509-certificate-get-signature cert)
    (~ cert 'c 'signature 'data))
  ;; should return meaningful name, but for now we just return oid
  (define (x509-certificate-get-signature-algorithm cert)
    (~ cert 'c 'algorithm-identifier 'object-id 'identifier))

  (define *rsa-oids*
    `(,(make-der-object-identifier "1.2.840.113549.1.1.1")
      ,(make-der-object-identifier "1.2.840.113549.1.1.2")
      ,(make-der-object-identifier "1.2.840.113549.1.1.3")
      ,(make-der-object-identifier "1.2.840.113549.1.1.4")
      ,(make-der-object-identifier "1.2.840.113549.1.1.5")
      ,(make-der-object-identifier "1.2.840.113549.1.1.7")
      ,(make-der-object-identifier "1.2.840.113549.1.1.10")
      ,(make-der-object-identifier "1.2.840.113549.1.1.11")
      ,(make-der-object-identifier "2.5.8.1.1")
      ))

  (define (x509-certificate-get-public-key cert)
    (let* ((c (~ cert 'c 'tbs-cert))
	   (info (~ c 'subject-public-key-info))
	   (oid (~ info 'algorithm-identifier 'object-id)))
      ;; we only support RSA
      (if (member oid *rsa-oids*)
	  (import-public-key RSA (open-bytevector-input-port
				  (~ info 'key-data 'data)))
	  (assertion-violation 'x509-certificate-get-public-key
			       "not supported public key oid"))))

  (define-syntax ash (identifier-syntax bitwise-arithmetic-shift))
  (define-method write-object ((o <x509-certificate>) (p <port>))
    (define (bv->hex-string bv)
      (define (->hex-char i) (string-ref "0123456789abcdef" i))
      (let1 s (make-string (* (bytevector-length bv) 2))
	(dotimes (i (bytevector-length bv))
	  (let1 u8 (bytevector-u8-ref bv i)
	    (string-set! s (* i 2) (->hex-char (ash u8 -4)))
	    (string-set! s (+ (* i 2) 1) (->hex-char (bitwise-and u8 #xF)))))
	s))
    (let ((buf (call-with-string-output-port
		(lambda (out)
		  (format out "#<x509-certificate~%")
		  (format out "  [0]         Version: ~a~%"
			  (x509-certificate-get-version o))
		  (format out "         SerialNumber: ~a~%"
			  (x509-certificate-get-serial-number o))
		  (format out "             IssuerDN: ~a~%"
			  (x509-certificate-get-issuer-dn o))
		  (format out "           Start Date: ~a~%"
			  (x509-certificate-get-not-before o))
		  (format out "           Final Date: ~a~%"
			  (x509-certificate-get-not-after o))
		  (format out "            SubjectDN: ~a~%"
			  (x509-certificate-get-subject-dn o))
		  (format out "           Public Key: ~a~%"
			  (x509-certificate-get-public-key o))
		  (format out "  Signature Algorithm: ~a~%"
			  (x509-certificate-get-signature-algorithm o))
		  (let* ((sig (bv->hex-string
			       (x509-certificate-get-signature o)))
			 (len (string-length sig)))
		    (format out "            Signature: ~a~%"
			    (substring sig 0 40))
		    (do ((i 40 (+ i 40)))
			((>= i len))
		      (if (< i (- len 40))
			  (format out "                       ~a~%"
				  (substring sig i (+ i 40)))
			  (format out "                       ~a~%"
				  (substring sig i len)))))
		  (let ((extensions (~ o 'c 'tbs-cert 'extensions)))
		    (when extensions
		      (let ((oids (map car (~ extensions 'extensions))))
			(unless (null? oids)
			  (format out "       Extensions: ~%")
			  (for-each 
			   (lambda (oid)
			     (let* ((ext (get-x509-extension extensions oid))
				    ;; do we need to check value if it's null?
				    (octs (asn.1-encodable->asn.1-object
					   (~ ext 'value))))
			       (format out "                       critical(~a)"
				       (~ ext 'critical))
			       ;; for now we don't check known oids.
			       (format out "~a value = ~a~%"
				       (~ oid 'identifier)
				       (read-asn.1-object
					(open-bytevector-input-port
					 (~ octs 'string))))))
			   oids)))))
		  (display ">" out)))))
      (display buf p)))

  ;; TODO for now we only support RSA
  ;;      currently user needs to do more task like choose hash algorithm
  ;;      and verifier. this must be done by the process.
  (define (verify cert message signature
		  :key (verify pkcs1-emsa-v1.5-verify)
		       (hash (hash-algorithm SHA-1)))
    (let* ((public-key (x509-certificate-get-public-key cert))
	   (rsa-cipher (cipher RSA public-key)))
      ;; TODO check certificate encoder
      (crypto:verify rsa-cipher message signature
		     :verify verify :hash hash)))

  (define (check-validity cert :optional (date (current-date)))
    (let ((time (date->time-utc date)))
      (when (time>? time (date->time-utc (x509-certificate-get-not-after cert)))
	(assertion-violation 'check-veridity
			     "certificate expored"
			     (x509-certificate-get-not-after cert)))
      (when (time<? time
		    (date->time-utc (x509-certificate-get-not-before cert)))
	(assertion-violation 'check-veridity
			     "certificate not valid yet"
			     (x509-certificate-get-not-before cert)))
      #t))

#|
Reference: RFC 3280 http://www.ietf.org/rfc/rfc3280.txt
Section 4.1
   Certificate  ::=  SEQUENCE  {
        tbsCertificate       TBSCertificate,
        signatureAlgorithm   AlgorithmIdentifier,
        signatureValue       BIT STRING  }

   TBSCertificate  ::=  SEQUENCE  {
        version         [0]  EXPLICIT Version DEFAULT v1,
        serialNumber         CertificateSerialNumber,
        signature            AlgorithmIdentifier,
        issuer               Name,
        validity             Validity,
        subject              Name,
        subjectPublicKeyInfo SubjectPublicKeyInfo,
        issuerUniqueID  [1]  IMPLICIT UniqueIdentifier OPTIONAL,
                             -- If present, version MUST be v2 or v3
        subjectUniqueID [2]  IMPLICIT UniqueIdentifier OPTIONAL,
                             -- If present, version MUST be v2 or v3
        extensions      [3]  EXPLICIT Extensions OPTIONAL
                             -- If present, version MUST be v3
        }

   Version  ::=  INTEGER  {  v1(0), v2(1), v3(2)  }

   CertificateSerialNumber  ::=  INTEGER

   Validity ::= SEQUENCE {
        notBefore      Time,
        notAfter       Time }

   Time ::= CHOICE {
        utcTime        UTCTime,
        generalTime    GeneralizedTime }

   UniqueIdentifier  ::=  BIT STRING

   SubjectPublicKeyInfo  ::=  SEQUENCE  {
        algorithm            AlgorithmIdentifier,
        subjectPublicKey     BIT STRING  }

  NOTE: we don't support optional fields such as issuerUniqueID
|#
  ;; take alist of issuer info
  ;; for now we don't check the order or mandatory fields
  (define (make-x509-issuer issuers)
    (define (rdn oid string)
      (make-der-set 
       (make-der-sequence oid (make-der-printable-string string #t))))
    (define (gen-rdn issuer)
      (or (and-let* ((slot (find (^s (eq? (cdr s) (car issuer)))
				 *default-symbols*)))
	    (rdn (car slot) (cdr issuer)))
	  (error 'make-x509-issuer "unknown parameter" issuer issuers)))
     (apply make-der-sequence (map (cut gen-rdn <>) issuers)))
  ;; we don't check
  (define (make-validity start end)
    (make-der-sequence (make-der-utc-time start) (make-der-utc-time end)))
  ;; for now we only use sha1withRSAEncryption as signature algorithm
  ;; OID 1 2 840 113549 1 1 5
  (define +sha1-with-rsa-encryption+
    (make-der-object-identifier "1.2.840.113549.1.1.5"))
  ;; for now we only use rsaEncryption as subject public key info
  ;; OID 1 2 840 113549 1 1 1
  (define +rsa-encryption+
    (make-der-object-identifier "1.2.840.113549.1.1.1"))
  (define (make-x509-basic-certificate keypair serial-number
				       issuer validity subject)
    (define (make-algorithm-id oid) (make-der-sequence oid (make-der-null)))
    (define (generate-signature-algorithm)
      (make-algorithm-id +sha1-with-rsa-encryption+))
    (define (generate-tbs)
      (let1 tbs (make-der-sequence)
	;; basic should be v1
	;; serialnumber
	(asn.1-sequence-add tbs (make-der-integer serial-number))
	(asn.1-sequence-add tbs (generate-signature-algorithm))
	;; CA certificate issuer
	(asn.1-sequence-add tbs issuer)
	(asn.1-sequence-add tbs validity)
	(asn.1-sequence-add tbs subject)
	;; public key
	(let1 spki (make-der-sequence)
	  (asn.1-sequence-add spki (make-algorithm-id +rsa-encryption+))
	  (let1 pbkey (make-der-bit-string
		       (export-public-key RSA (keypair-public keypair)))
	    (asn.1-sequence-add spki pbkey)
	    (asn.1-sequence-add tbs spki)))
	tbs))
    (let* ((tbs (generate-tbs))
	   (cert (make-der-sequence tbs (generate-signature-algorithm)
				    (let1 signature (hash SHA-1 (encode tbs))
				      (make-der-bit-string signature)))))
      (make-x509-certificate cert)))

)