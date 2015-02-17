;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; pkcs 12 keystore.scm - PKCS#12 library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; only the part I want, for now

(library (rsa pkcs :12 keystore)
    (export <pkcs12-keystore> pkcs12-keystore? make-pkcs12-keystore
	    load-pkcs12-keystore-file load-pkcs12-keystore
	    ;; entry accessors
	    pkcs12-keystore-get-key
	    pkcs12-keystore-get-certificate
	    pkcs12-keystore-get-certificate-chain

	    ;; store
	    store-pkcs12-keystore
	    store-pkcs12-keystore-to-file
	    
	    pkcs12-keystore-set-key!
	    pkcs12-keystore-set-certificate!

	    pkcs12-keystore-delete-entry!

	    ;; should we document this?
	    pkcs12-keystore-aliases

	    ;; read only accessors 
	    ;; (for debugging use won't be documented)
	    pkcs12-keystore-keys pkcs12-keystore-key-certificates
	    pkcs12-keystore-certificates)
    (import (rnrs)
	    (clos user)
	    (srfi :1)
	    (rsa pkcs :5)
	    (rsa pkcs :8)
	    (rename (rsa pkcs :10) (algorithm-identifier-id get-id))
	    (rsa pkcs :12 cipher)
	    (rfc hmac)
	    (rename (rfc x.509) (verify x509-verity))
	    (crypto)
	    (math)
	    (asn.1)
	    (util hashtables)
	    (sagittarius)
	    (sagittarius control)
	    (security keystore interface))

  ;; oid-cipher mapping
  (define *mapping*
    `(("1.2.840.113549.1.12.1.3" . ,pbe-with-sha-and3-keytripledes-cbc)
      ("1.2.840.113549.1.12.1.6" . ,pbe-with-sha-and-40bit-rc2-cbc)))
  ;; for storing
  (define *reverse-mapping*
    `((,pbe-with-sha-and3-keytripledes-cbc . "1.2.840.113549.1.12.1.3")
      (,pbe-with-sha-and-40bit-rc2-cbc . "1.2.840.113549.1.12.1.6")))

  (define-class <content-info> (<asn.1-encodable>)
    ((content-type :init-keyword :content-type)
     (content      :init-keyword :content)))
  (define (content-info? o) (is-a? o <content-info>))
  (define-method make-content-info ((s <asn.1-sequence>))
    ;; assume it has both
    (let ((info (asn.1-sequence-get s 0))
	  (content (asn.1-sequence-get s 1)))
      ;; content must be tagged object
      (make <content-info>
	:content-type info
	:content (der-encodable->der-object content))))
  (define-method make-content-info ((type <der-object-identifier>)
				    (content <asn.1-encodable>))
    (make <content-info> :content-type type :content content))
  (define-method asn.1-encodable->asn.1-object ((o <content-info>))
    (make-ber-sequence (slot-ref o 'content-type) 
		       (make-ber-tagged-object #t 0 (slot-ref o 'content))))

  (define-class <digest-info> (<asn.1-encodable>)
    ((digest :init-keyword :digest)
     (algorithm-identifier :init-keyword :algorithm-identifier)))
  (define-method make-digest-info ((s <asn.1-sequence>))
    (make <digest-info> 
      :digest (der-octet-string-octets (asn.1-sequence-get s 1))
      :algorithm-identifier (make-algorithm-identifier
			     (asn.1-sequence-get s 0))))
  (define-method make-digest-info ((id <algorithm-identifier>)
				   (digest <bytevector>))
    (make <digest-info> :digest digest :algorithm-identifier id))
  (define-method asn.1-encodable->asn.1-object ((di <digest-info>))
    (make-der-sequence (slot-ref di 'algorithm-identifier)
		       (make-der-octet-string (slot-ref di 'digest))))

  ;; for now we don't do this much  
  (define-class <mac-data> (<asn.1-encodable>)
    ((digest-info :init-keyword :digest-info)
     (salt        :init-keyword :salt)
     (iteration-count :init-keyword :iteration-count :init-value 1)))
  (define-method make-mac-data ((s <asn.1-sequence>))
    (let ((di (make-digest-info (asn.1-sequence-get s 0)))
	  (salt (der-octet-string-octets (asn.1-sequence-get s 1)))
	  (ic (if (= (asn.1-sequence-size s) 3)
		  (der-integer->integer (asn.1-sequence-get s 2))
		  1)))
      (make <mac-data> :digest-info di :salt salt :iteration-count ic)))
  (define-method make-mac-data ((dig-info <digest-info>)
				(salt <bytevector>)
				(iteration-count <integer>))
    (make <mac-data> :digest-info dig-info :salt salt
	  :iteration-count iteration-count))
  (define-method asn.1-encodable->asn.1-object ((md <mac-data>))
    (let ((count (slot-ref md 'iteration-count)))
      (if (= count 1)
	  (make-der-sequence (slot-ref md 'digest-info)
			     (make-der-octet-string (slot-ref md 'salt)))
	  (make-der-sequence (slot-ref md 'digest-info)
			     (make-der-octet-string (slot-ref md 'salt))
			     (make-der-integer count)))))

  (define-class <authenticated-safe> (<asn.1-encodable>)
    ((info :init-keyword :info)))
  (define-method make-authenticated-safe ((s <asn.1-sequence>))
    (let* ((len (asn.1-sequence-size s))
	   (info (make-vector len)))
      (dotimes (i len)
	(vector-set! info i (make-content-info (asn.1-sequence-get s i))))
      (make <authenticated-safe> :info info)))
  (define-method make-authenticated-safe ((v <vector>))
    (vector-for-each
     (lambda (o) (or (content-info? o)
		     (assertion-violation 'make-authenticated-safe
					  "vector of ContentInfo required" v)))
     v)
    (make <authenticated-safe> :info v))
  (define-method asn.1-encodable->asn.1-object ((o <authenticated-safe>))
    (let ((seq (make-ber-sequence)))
      (vector-for-each
       (lambda (ci) (asn.1-sequence-add seq ci))
       (slot-ref o 'info))
      seq))

  (define-class <pfx> (<asn.1-encodable>)
    ((content-info :init-keyword :content-info)
     (mac-data     :init-keyword :mac-data)))

  (define-method make-pfx ((seq <asn.1-sequence>))
    (let ((v (asn.1-sequence-get seq 0)))
      (unless (= (der-integer->integer v) 3)
	(assertion-violation 'make-pfx
			     "wrong version of for PFX PDU"))
      (make <pfx>
	:content-info (make-content-info (asn.1-sequence-get seq 1))
	:mac-data (if (= (asn.1-sequence-size seq) 3)
		      (make-mac-data (asn.1-sequence-get seq 2))
		      #f))))
  (define-method make-pfx ((mi <content-info>)
			   (md <mac-data>))
    (make <pfx> :content-info mi :mac-data md))
  (define-method asn.1-encodable->asn.1-object ((o <pfx>))
    (let ((s (make-ber-sequence)))
      (asn.1-sequence-add s (make-der-integer 3))
      (asn.1-sequence-add s (slot-ref o 'content-info))
      ;; TODO mac-data can be null
      (and-let* ((mac (slot-ref o 'mac-data)))
	(asn.1-sequence-add s (slot-ref o 'mac-data)))
      s))

  ;; object identifiers
  (define *pkcs-7-data* (make-der-object-identifier "1.2.840.113549.1.7.1"))
  (define *pkcs-7-encrypted-data* (make-der-object-identifier
				   "1.2.840.113549.1.7.6"))
  (define *pkcs-8-shrouded-key-bag* (make-der-object-identifier
				     "1.2.840.113549.1.12.10.1.2"))
  (define *pkcs-9-at-friendly-name* 
    (make-der-object-identifier "1.2.840.113549.1.9.20"))
  (define *pkcs-9-at-local-key-id* 
    (make-der-object-identifier "1.2.840.113549.1.9.21"))
  (define *pkcs-9-x509-certificate* 
    (make-der-object-identifier "1.2.840.113549.1.9.22.1"))
  (define *pkcs-12-key-bag*
    (make-der-object-identifier "1.2.840.113549.1.12.10.1.1"))
  (define *pkcs-12-cert-bag*
    (make-der-object-identifier "1.2.840.113549.1.12.10.1.3"))

  ;; kinda silly...
  (define *sha1-oid* "1.3.14.3.2.26")

  (define-class <safe-bag> (<asn.1-encodable>)
    ((id    :init-keyword :id)
     (value :init-keyword :value)
     (attributes :init-keyword :attributes :init-value #f)))
  (define-method make-safe-bag ((s <asn.1-sequence>))
    (let* ((len (asn.1-sequence-size s))
	   (id  (asn.1-sequence-get s 0))
	   (value  (der-encodable->der-object (asn.1-sequence-get s 1)))
	   (attr (if (= len 3) (asn.1-sequence-get s 2) #f)))
      (make <safe-bag>
	:id id :value value :attributes attr)))
  (define-method make-safe-bag ((id <der-object-identifier>)
				(o <asn.1-encodable>)
				(attrs <asn.1-set>))
    (make <safe-bag> :id id :value o :attributes attrs))

  (define-method asn.1-encodable->asn.1-object ((o <safe-bag>))
    (let ((id (slot-ref o 'id))
	  (value (make-der-tagged-object #t 0 (slot-ref o 'value)))
	  (attr (slot-ref o 'attributes)))
      (if attr
	  (make-der-sequence id value attr)
	  (make-der-sequence id value))))

  (define-class <encrypted-data> (<asn.1-encodable>)
    ((data         :init-keyword :data) ;; for ->asn.1-object
     (content-type :init-keyword :content-type)
     (id           :init-keyword :id)
     (content      :init-keyword :content)))
  (define-method make-encrypted-data ((s <asn.1-sequence>))
    (let ((version (der-integer->integer (asn.1-sequence-get s 0))))
      (unless (zero? version)
	(assertion-violation 'make-encrypted-data
			     "sequence not version 0")))
    (let ((data (asn.1-sequence-get s 1)))
      (make <encrypted-data>
	:data data
	:content-type (asn.1-sequence-get data 0)
	:id   (make-algorithm-identifier (asn.1-sequence-get data 1))
	:content (if (= (asn.1-sequence-size data) 3)
		     (let ((o (der-encodable->der-object
			       (asn.1-sequence-get data 2))))
		       (if (is-a? o <asn.1-octet-string>)
			   o
			   (apply make-ber-constructed-octet-string
				  (slot-ref o 'sequence))))
		     #f))))
  (define-method make-encrypted-data ((type <der-object-identifier>)
				      (algo <algorithm-identifier>)
				      (content <asn.1-encodable>))
    (make <encrypted-data>
	:data (make-ber-sequence type 
				 (asn.1-encodable->asn.1-object algo)
				 (make-ber-tagged-object #f 0 content))
	:content-type type
	:id   algo
	:content content))
  (define-method asn.1-encodable->asn.1-object ((ed <encrypted-data>))
    (make-ber-sequence (make-der-integer 0) (slot-ref ed 'data)))

  (define-class <cert-bag> (<asn.1-encodable>)
    ((seq :init-keyword :seq)
     (id  :init-keyword :id)
     (value :init-keyword :value)))
  (define-method make-cert-bag ((s <asn.1-sequence>))
    (make <cert-bag>
      :seq s
      :id (asn.1-sequence-get s 0)
      :value (der-encodable->der-object (asn.1-sequence-get s 1))))
  (define-method make-cert-bag ((id <der-object-identifier>)
				(value <asn.1-encodable>))
    (make <cert-bag> :seq (make-der-sequence id value) :id id :value value))
  (define-method asn.1-encodable->asn.1-object ((cb <cert-bag>))
    (make-der-sequence (slot-ref cb 'id) 
		       (make-der-tagged-object 0 (slot-ref cb 'value))))


  (define-class <pkcs12-pbe-params> (<asn.1-encodable>)
    ((iterations :init-keyword :iterations)
     (iv :init-keyword :iv)))
  (define-method make-pkcs12-pbe-params ((s <asn.1-sequence>))
    (make <pkcs12-pbe-params>
      :iv (asn.1-sequence-get s 0)
      :iterations (asn.1-sequence-get s 1)))
  (define-method make-pkcs12-pbe-params ((iv <bytevector>)
					 (iterations <integer>))
    (make <pkcs12-pbe-params>
      :iv (make-der-octet-string iv)
      :iterations (make-der-integer iterations)))
  (define-method der-encodable->der-object ((p <pkcs12-pbe-params>))
    (make-der-sequence (slot-ref p 'iv) (slot-ref p 'iterations)))

  (define (pkcs12-pbe-params-get-iterations o)
    (der-integer->integer (slot-ref o 'iterations)))
  (define (pkcs12-pbe-params-get-iv o)
    (slot-ref (slot-ref o 'iv) 'string))

  ;; helper class
  (define-class <cert-id> ()
    ((id :init-keyword :id)))
  (define-method make-cert-id ((key <public-key>))
    (make <cert-id> :id (subject-key-identifier-key-identifier
			 (create-subject-key-id key))))
  (define-method make-cert-id ((id <bytevector>))
    (make <cert-id> :id id))
  (define (cert-id=? a b)
    (bytevector=? (slot-ref a 'id) (slot-ref b 'id)))
  (define (cert-id-hash a)
    (equal-hash (slot-ref a 'id)))

  (define-class <pkcs12-keystore> (<keystore>)
    ;; we are not so flexible for algorithms
    ((key-algorithm :init-value pbe-with-sha-and3-keytripledes-cbc)
     ;; storing encrypted-private-key-info
     (keys      :init-form (make-hashtable string-ci-hash string-ci=?)
		:reader pkcs12-keystore-keys)
     (key-certs :init-form (make-string-hashtable)
		:reader pkcs12-keystore-key-certificates)
     (cert-algorithm :init-value pbe-with-sha-and3-keytripledes-cbc)
     (certs     :init-form (make-string-hashtable)
		:reader pkcs12-keystore-certificates)
     (chain-certs :init-form (make-hashtable cert-id-hash cert-id=?)
		  :reader pkcs12-keystore-chain-certificates)
     (local-ids :init-form (make-string-hashtable))
     ;; bag attributes holds bag attributes of the object
     (bag-attributes :init-form (make-eq-hashtable))
     ;; prng
     (prng :init-form (secure-random RC4)
	   :reader pkcs12-keystore-prng
	   :writer pkcs12-keystore-set-prng!)))

  (define-constant salt-size 20) ;; SHA-1 hash size
  (define-constant min-iteration 1024)

  (define (make-pkcs12-keystore) (make <pkcs12-keystore>))
  (define (pkcs12-keystore? o) (is-a? o <pkcs12-keystore>))

  (define *rsa-private-key-oid* "1.2.840.113549.1.1.1")

  (define (default-extra-data-handler data)
    ;; do nothing
    )

  (define (pkcs12-keystore-get-key keystore name password)
    ;; JCE PKCS#12 is implemented properly as the spec mentioned.
    ;; so make it compatible
    (define (unwrap-key alg-id data password)
      (let* ((plain-key (cipher-util alg-id password 
				     (slot-ref data 'string) decrypt))
	     (asn.1-key
	      (read-asn.1-object (open-bytevector-input-port plain-key)))
	     (priv-key-info (make-private-key-info asn.1-key)))
	priv-key-info))

    (let ((keys (slot-ref keystore 'keys)))
      (cond ((hashtable-ref keys name #f)
	     => (lambda (pki)
		  (cond ((private-key-info? pki) (pki->private-key pki))
			((encrypted-private-key-info? pki)
			 (pki->private-key (unwrap-key (slot-ref pki 'id)
						       (slot-ref pki 'data)
						       password)))
			(else 
			 (error 'pkcs12-keystore-get-key 
				"unknown object" pki)))))
	    (else #f))))

  (define (pkcs12-keystore-get-certificate keystore name)
    (let ((certs (slot-ref keystore 'certs)))
      (cond ((hashtable-ref certs name #f))
	    (else 
	     (let ((key-certs (slot-ref keystore 'key-certs))
		   (id (cond ((hashtable-ref 
			       (local-ids (slot-ref keystore 'local-ids))
			       name #f))
			     (else name))))
	       (hashtable-ref key-certs id #f))))))

  (define (load-pkcs12-keystore-file in-file password . opt)
    (call-with-input-file in-file
      (lambda (in) (apply load-pkcs12-keystore in password opt))
      :transcoder #f))

  (define *digest-mapping*
    `((,*sha1-oid* . ,SHA-1)
      ("2.16.840.1.101.3.4.2.1" . ,SHA-256)
      ("2.16.840.1.101.3.4.2.2" . ,SHA-384)
      ("2.16.840.1.101.3.4.2.3" . ,SHA-512)))
  
  (define (compute-mac oid data password salt iteration)
    (define (find-method oid)
      (cond ((assoc oid *digest-mapping*) => cdr)
	    (else (error 'compute-mac "Digest not supported" oid))))
    (let* ((param (make-pbe-parameter salt iteration))
	   ;; key derivation doesn't consider encryption scheme
	   ;; so just use DES for now.
	   (key (generate-secret-key pbe-with-sha1-and-des password)))
      (let ((mac-key (derive-mac-key key param)))
	(hash HMAC data :key mac-key :hash (find-method oid)))))

  (define (cipher-util alg-id password data processer)
    (let ((alg-name (cond ((assoc (get-id alg-id) *mapping*) => cdr)
			  (else #f)))
	  (pbe-params (make-pkcs12-pbe-params (slot-ref alg-id 'parameters))))
      (unless alg-name
	(assertion-violation 'load-pkcs12-keystore
			     "unknown algorithm identifier" alg-id))
      
      (let* ((param (make-pbe-parameter 
		     (pkcs12-pbe-params-get-iv pbe-params)
		     (pkcs12-pbe-params-get-iterations pbe-params)))
	     (k (generate-secret-key alg-name password))
	     (pbe-cipher (cipher alg-name k :parameter param)))
	(processer pbe-cipher data))))

  (define (load-pkcs12-keystore in password
			:key (extra-data-handler default-extra-data-handler))

    (define (crypt-data alg-id password data)
      (cipher-util alg-id password data decrypt))

    (define (store-bag-attribute! keystore obj oid attr)
      (let* ((bag-attrs (slot-ref keystore 'bag-attributes))
	     (attrs (cond ((hashtable-ref bag-attrs obj #f))
			  (else (let ((ht (make-equal-hashtable)))
				  (hashtable-set! bag-attrs obj ht)
				  ht)))))
	(cond ((hashtable-ref attrs oid #f)
	       => (lambda (v)
		    ;; TODO should we raise an error?
		    ;; for now do nothing
		    ))
	      (else (hashtable-set! attrs oid attr)))))

    (define (process-attributes obj keystore attributes alias-handler)
      (if attributes
	  (let loop ((seqs (slot-ref attributes 'set))
		     (local-id #f)
		     (alias #f))
	    (if (null? seqs)
		(values local-id alias)
		(let* ((seq (car seqs))
		       (oid (asn.1-sequence-get seq 0))
		       (attr-set (asn.1-sequence-get seq 1))
		       (attr (if (> (asn.1-set-size attr-set) 0)
				 (asn.1-set-get attr-set 0)
				 #f)))
		  ;; store bag attribute
		  (store-bag-attribute! keystore obj oid attr)
		  (loop (cdr seqs)
			(or (and (equal? oid *pkcs-9-at-local-key-id*) attr)
			    local-id)
			(or 
			 (and-let* (( (equal? oid *pkcs-9-at-friendly-name*) )
				    (alias (asn.1-string->string attr)))
			   (when alias-handler (alias-handler alias))
			   alias)
			 alias)))))
	  (values #f #f)))
    (define (process-shrouded-key-bag b keystore unwrap?)
      (let ((private-key (if unwrap?
			     (let ((e-in (make-encrypted-private-key-info 
					  (slot-ref b 'value))))
			       ;; just return this
			       e-in)
			     ;; just return this
			     (make-private-key-info (slot-ref b 'value)))))
	 (let-values (((local-id alias)
		       (process-attributes private-key keystore
			 (slot-ref b 'attributes)
			 (lambda (alias)
			   (hashtable-set! (slot-ref keystore 'keys) alias
					   private-key)))))
	   (if local-id
	       (let ((name (format "~X" (bytevector->integer
					 (slot-ref local-id 'string)))))
		 (if alias
		     (hashtable-set! (slot-ref keystore 'local-ids)
				     alias 
				     name)
		     (hashtable-set! (slot-ref keystore 'keys) name
				     private-key)))
	       (hashtable-set! (slot-ref keystore 'keys) "unmarkded"
			       private-key)))))

    (define (process-data c keystore)
      (let* ((obj (read-asn.1-object 
		   (open-bytevector-input-port
		    (slot-ref (slot-ref c 'content) 'string))))
	     (size (asn.1-sequence-size obj)))
	(let loop ((i 0) (r '()))
	  (if (= i size)
	      (reverse! r)
	      (let ((b (make-safe-bag (asn.1-sequence-get obj i))))
		(cond ((equal? (slot-ref b 'id) *pkcs-8-shrouded-key-bag*)
		       (process-shrouded-key-bag b keystore #t)
		       (loop (+ i 1) r))
		      ((equal? (slot-ref b 'id) *pkcs-12-cert-bag*)
		       (loop (+ i 1) (cons b r)))
		      (else
		       ;; FIXME what should we do?
		       (extra-data-handler b)
		       (loop (+ i 1) r))))))))

    (define (process-encrypted-data c keystore)
      (let* ((d (make-encrypted-data (slot-ref c 'content)))
	     (octets (crypt-data (slot-ref d 'id)
				 password 
				 (slot-ref (slot-ref d 'content) 'string)))
	     (seq (read-asn.1-object (open-bytevector-input-port octets)))
	     (size (asn.1-sequence-size seq)))
	(let loop ((i 0) (r '()))
	  (if (= i size)
	      (reverse! r)
	      (let ((b (make-safe-bag (asn.1-sequence-get seq i))))
		(cond ((equal? (slot-ref b 'id) *pkcs-12-cert-bag*)
		       (loop (+ i 1) (cons b r)))
		      ((equal? (slot-ref b 'id) *pkcs-8-shrouded-key-bag*)
		       (process-shrouded-key-bag b keystore #t)
		       (loop (+ i 1) r))
		      ((equal? (slot-ref b 'id) *pkcs-12-key-bag*)
		       (process-shrouded-key-bag b keystore #f)
		       (loop (+ i 1) r))
		      (else
		       ;; FIXME what should we do?
		       (extra-data-handler b)
		       (loop (+ i 1) r))))))))
    (define (process-keys keystore info)
      (if (equal? (slot-ref info 'content-type) *pkcs-7-data*)
	  (let* ((auth-safe (make-authenticated-safe
			     (read-asn.1-object
			      (open-bytevector-input-port
			       (slot-ref (slot-ref info 'content) 'string)))))
		 (ac (slot-ref auth-safe 'info))
		 (len (vector-length ac)))
	    (let loop ((i 0) (chain '()))
	      (if (= i len)
		  chain
		  (let ((c (vector-ref ac i)))
		    (cond ((equal? *pkcs-7-data* (slot-ref c 'content-type))
			   (loop (+ i 1) 
				 (append! chain (process-data c keystore))))
			  ((equal? *pkcs-7-encrypted-data* 
				   (slot-ref c 'content-type))
			   (loop (+ i 1)
				 (append! chain 
					  (process-encrypted-data c keystore)
					  )))
			  (else 
			   ;; FIXME what should we do?
			   (extra-data-handler c)
			   (loop (+ i 1) chain)))))))
	  '()))
    (define (validate-mac pfx info)
      (let* ((md (slot-ref pfx 'mac-data))
	     (di (slot-ref md 'digest-info))
	     (id (slot-ref di 'algorithm-identifier))
	     (salt (slot-ref md 'salt))
	     (count (slot-ref md 'iteration-count))
	     (data (der-octet-string-octets (slot-ref info 'content))))
	(unless (bytevector=? (slot-ref di 'digest)
			      (compute-mac (get-id id)
					   data password salt count))
	  (error 'load-pkcs12-keystore 
		 "key store mac invalid - wrong password or corrupted file."))
	info))
    (let* ((r (read-asn.1-object in))
	   (pkcs12 (make-pfx r))
	   (info   (validate-mac pkcs12 (slot-ref pkcs12 'content-info)))
	   (keystore (make-pkcs12-keystore))
	   ;; first keys
	   (chain (process-keys keystore info)))
      ;; then certs
      (dolist (b chain)
	(let ((cb (make-cert-bag (slot-ref b 'value))))
	  (unless (equal? (slot-ref cb 'id) *pkcs-9-x509-certificate*)
	    (assertion-violation 'load-pkcs12-keystore
				 "Unsupported certificate type"
				 (slot-ref cb 'id)))
	  (let ((cert (make-x509-certificate 
		       (slot-ref (slot-ref cb 'value) 'string))))
	    (let-values (((local-id alias) (process-attributes cert keystore
					    (slot-ref b 'attributes)
					    #f)))
	      ;; associate cert and it's id
	      (hashtable-set! (slot-ref keystore 'chain-certs)
			      (make-cert-id
			       (x509-certificate-get-public-key cert))
			      cert)
	      ;; TODO unmarked key
	      (when local-id
		(let ((name (format "~X" (slot-ref local-id 'string))))
		  (hashtable-set! (slot-ref keystore 'key-certs) name cert)))
	      (when alias
		(hashtable-set! (slot-ref keystore 'certs) alias cert))))))
      keystore))

  ;; Storing PKCS#12 keystore to given output port
  ;; I'm not sure if this is properly implemented...

  ;; this is also used on cert-id
  (define (create-subject-key-id pubkey)
    (let ((info (make-subject-public-key-info
		 (make-der-sequence
		  (make-algorithm-identifier
		   *rsa-private-key-oid*
		   (make-der-null))
		  (read-asn.1-object 
		   (open-bytevector-input-port
		    (export-public-key pubkey)))))))
      (make-subject-key-identifier
       (hash SHA-1 (encode (subject-public-key-info-key-data info))))))
  (define (store-pkcs12-keystore keystore out password)
    (define prng (slot-ref keystore 'prng))

    (define (bag-attributes keystore obj)
      (hashtable-ref (slot-ref keystore 'bag-attributes) obj #f))
	    
    (define (process-key-bag-attribute keystore obj bag-attr name)
      (let ((sets (make-der-set)))
	(when bag-attr
	  (let ((nm (hashtable-ref bag-attr *pkcs-9-at-friendly-name* #f)))
	    (unless (and nm (equal? (asn.1-string->string nm) name))
	      (hashtable-set! bag-attr *pkcs-9-at-friendly-name* 
			      (make-der-bmp-string name)))
	    (unless (hashtable-ref bag-attr *pkcs-9-at-local-key-id* #f)
	      (let ((ct (if (x509-certificate? obj) ;; other certificate?
			    obj
			    (pkcs12-keystore-get-certificate keystore name))))
		(hashtable-set! bag-attr *pkcs-9-at-local-key-id*
				(create-subject-key-id 
				 (x509-certificate-get-public-key ct)))))
	    (hashtable-for-each
	     (lambda (oid value)
	       (asn.1-set-add sets 
			      (make-der-sequence oid (make-der-set value))))
	     bag-attr)))
	(when (zero? (asn.1-set-size sets))
	  (let ((ct (if (x509-certificate? obj) ;; other certificate?
			obj
			(pkcs12-keystore-get-certificate keystore name))))
	    (asn.1-set-add sets
	      (make-der-sequence
	       *pkcs-9-at-local-key-id*
	       (make-der-set 
		(create-subject-key-id 
		 (x509-certificate-get-public-key ct)))))
	    (asn.1-set-add sets
	      (make-der-sequence
	       *pkcs-9-at-friendly-name*
	       (make-der-set (make-der-bmp-string name))))))
	sets))
	    
    (define (process-keys keys)
      (let ((seq (make-der-sequence)))
	(hashtable-for-each
	 (lambda (name key)
	   (let* ((bag-attr (bag-attributes keystore key))
		  (names (process-key-bag-attribute keystore key 
						    bag-attr name)))
	     (let ((bag (make-safe-bag *pkcs-8-shrouded-key-bag*
				       (der-encodable->der-object key)
				       names)))
	       (asn.1-sequence-add seq bag))))
	 keys)
	(make-ber-constructed-octet-string (encode seq))))
    (define (process-certificates keystore)
      (define seq (make-der-sequence)) ;; return value
      (define done-certs (make-equal-hashtable))

      ;; for encryption
      (define salt (read-random-bytes prng salt-size))
      (define param (make-pkcs12-pbe-params salt min-iteration))
      (define cert-algorithm (slot-ref keystore 'cert-algorithm))
      (define alg-id (make-algorithm-identifier 
		      (cdr (assq cert-algorithm *reverse-mapping*))
		      (der-encodable->der-object param)))

      (define (process-keys-certificate keystore keys)
	(hashtable-for-each
	 (lambda (name key)
	   (let* ((cert (pkcs12-keystore-get-certificate keystore name))
		  (cert-bag (make-cert-bag 
			     *pkcs-9-x509-certificate*
			     (make-der-octet-string 
			      (x509-certificate->bytevector cert))))
		  (bag-attr (bag-attributes keystore cert))
		  (names (process-key-bag-attribute keystore cert 
						    bag-attr name)))
	     (let ((bag (make-safe-bag *pkcs-12-cert-bag*
				       (asn.1-encodable->asn.1-object cert-bag)
				       names)))
	       (asn.1-sequence-add seq bag)
	       (hashtable-set! done-certs cert cert))))
	 keys))
      (define (process-cert-bag-attributes bag-attr name handle-empty?)
	(let ((sets (make-der-set)))
	  (when bag-attr
	    (let ((nm (hashtable-ref bag-attr *pkcs-9-at-friendly-name* #f)))
	      (unless (and nm (equal? (asn.1-string->string nm) name))
		(hashtable-set! bag-attr *pkcs-9-at-friendly-name* 
				(make-der-bmp-string name)))
	      (hashtable-for-each
	       (lambda (oid value)
		 ;; comment from Bouncy Castle
		 ;; a certificate not immediately linked to a key doesn't
		 ;; require a local-key-id and will confuse some PKCS12 
		 ;; implementations
		 (unless (equal? oid *pkcs-9-at-local-key-id*)
		   (asn.1-set-add sets 
		     (make-der-sequence oid (make-der-set value)))))
	       bag-attr)))
	  (when (and handle-empty? (zero? (asn.1-set-size sets)))
	    (asn.1-set-add sets
	      (make-der-sequence
	       *pkcs-9-at-friendly-name*
	       (make-der-set (make-der-bmp-string name)))))
	  sets))
      (define (process-certificates keys certs)
	(hashtable-for-each
	 (lambda (cert-id cert)
	   ;; it's already done
	   (unless (hashtable-contains? keys cert-id)
	     (let* ((cert-bag (make-cert-bag 
			       *pkcs-9-x509-certificate*
			       (make-der-octet-string 
				(x509-certificate->bytevector cert))))
		    (bag-attr (bag-attributes keystore cert))
		    (names (process-cert-bag-attributes bag-attr cert-id #t)))
	       (let ((bag (make-safe-bag 
			   *pkcs-12-cert-bag*
			   (asn.1-encodable->asn.1-object cert-bag)
			   names)))
		 (asn.1-sequence-add seq bag)
		 (hashtable-set! done-certs cert cert)))))
	 certs))
      (define (process-chain-certificates chain-certs)
	(hashtable-for-each
	 (lambda (cert-id cert)
	   ;; it's already done
	   ;; TODO yes we need object-equal? for x509 certificate
	   (unless (hashtable-contains? done-certs cert)
	     (let* ((cert-bag (make-cert-bag 
			       *pkcs-9-x509-certificate*
			       (make-der-octet-string 
				(x509-certificate->bytevector cert))))
		    (bag-attr (bag-attributes keystore cert))
		    (names (process-cert-bag-attributes bag-attr cert #f)))
	       (let ((bag (make-safe-bag 
			   *pkcs-12-cert-bag*
			   (asn.1-encodable->asn.1-object cert-bag)
			   names)))
		 (asn.1-sequence-add seq bag)
		 (hashtable-set! done-certs cert cert)))))
	 chain-certs))
      (process-keys-certificate keystore (pkcs12-keystore-keys keystore))
      (process-certificates (pkcs12-keystore-keys keystore)
			    (pkcs12-keystore-certificates keystore))
      (process-chain-certificates (pkcs12-keystore-chain-certificates keystore))
      
      ;; seq is done destructively so just use it
      (let* ((certs-bytes (cipher-util alg-id password (encode seq) encrypt))
	     (info (make-encrypted-data *pkcs-7-data* alg-id
					(make-der-octet-string certs-bytes))))
	(asn.1-encodable->asn.1-object info)))

    (define (compute-mac-data data password)
      (define salt (read-random-bytes prng salt-size))
      (let ((res (compute-mac *sha1-oid* data password salt min-iteration))
	    (alg-id (make-algorithm-identifier *sha1-oid* (make-der-null))))
	(make-mac-data (make-digest-info alg-id res) salt min-iteration)))

    (let* ((key-string (process-keys (pkcs12-keystore-keys keystore)))
	   (cert-string (process-certificates keystore))
	   (auth (make-authenticated-safe 
		  (vector (make-content-info *pkcs-7-data* key-string)
			  (make-content-info *pkcs-7-encrypted-data* 
					     cert-string))))
	   (pkg (encode auth))
	   (main-info (make-content-info *pkcs-7-data*
					 (make-ber-constructed-octet-string
					  pkg)))
	   (mac-data (compute-mac-data pkg password))
	   (pfx (make-pfx main-info mac-data)))
      (put-bytevector out (encode pfx))))
	     
  (define (store-pkcs12-keystore-to-file keystore file password)
    (call-with-output-file file
      (lambda (out)
	(store-pkcs12-keystore keystore out password))
      :transcoder #f))

  (define (pkcs12-keystore-set-certificate! keystore alias cert)
    (unless (x509-certificate? cert)
      (assertion-violation 'pkcs12-keystore-set-certificate!
			   "X509 certificate is required" cert))
    (when (hashtable-contains? (slot-ref keystore 'keys) alias)
      (error 'pkcs12-keystore-set-certificate!
	     "There is a key entry with the same name" alias))
    (hashtable-set! (slot-ref keystore 'certs) alias cert)
    (hashtable-set! (slot-ref keystore 'chain-certs) 
		    (make-cert-id (x509-certificate-get-public-key cert))
		    cert))

  (define (pkcs12-keystore-delete-entry! keystore alias)
    (let* ((keys (slot-ref keystore 'keys))
	   (certs (slot-ref keystore 'certs))
	   (chain-certs (slot-ref keystore 'chain-certs))
	   (key (hashtable-ref keys alias #f))
	   (c   (hashtable-ref certs alias #f)))
      (hashtable-delete! keys alias)
      (hashtable-delete! certs alias)
      (when c (hashtable-delete! chain-certs
				 (make-cert-id
				  (x509-certificate-get-public-key c))))
      (when key
	(let* ((local-ids (slot-ref keystore 'local-ids))
	       (key-certs (slot-ref keystore 'key-certs))
	       (id (hashtable-ref local-ids alias #f))
	       (c  (and id (hashtable-ref key-certs id #f))))
	  (hashtable-delete! local-ids alias)
	  (when id (hashtable-delete! key-certs id))
	  (when c 
	    (hashtable-delete! chain-certs
			       (make-cert-id
				(x509-certificate-get-public-key c))))))))

  (define (pkcs12-keystore-set-key! keystore alias key password certs)
    (define prng (slot-ref keystore 'prng))
    (define key-algorithm (slot-ref keystore 'key-algorithm))

    (define (wrap-key alg-name key-bv password pbe-params)
      (let* ((param (make-pbe-parameter 
		     (pkcs12-pbe-params-get-iv pbe-params)
		     (pkcs12-pbe-params-get-iterations pbe-params)))
	     (k (generate-secret-key alg-name password))
	     (pbe-cipher (cipher alg-name k :parameter param)))
	(encrypt pbe-cipher key-bv)))
    (define (make-encrypted-key-content key)
      (encode
       (make-private-key-info 
	(make-algorithm-identifier *rsa-private-key-oid* (make-der-null))
	key)))
    (define (->epki key password)
      (let* ((salt (read-random-bytes prng salt-size))
	     (param (make-pkcs12-pbe-params salt min-iteration))
	     (key-bytes (wrap-key key-algorithm
				  (make-encrypted-key-content key)
				  password param))
	     (alg-id (make-algorithm-identifier 
		      (cdr (assq key-algorithm *reverse-mapping*))
		      (der-encodable->der-object param))))
	(make-encrypted-private-key-info alg-id key-bytes)))

    (unless (private-key? key)
      (assertion-violation 'pkcs12-keystore-set-key!
			   "Private key is required" key))
    (when (and (private-key? key) (or (null? certs) (not certs)))
      (assertion-violation 'pkcs12-keystore-set-key!
			   "no certificate chain for private key" key certs))
    (let ((keys (slot-ref keystore 'keys))
	  (chain (slot-ref keystore 'chain-certs)))
      (when (hashtable-contains? keys alias)
	(pkcs12-keystore-delete-entry! keystore alias))
      (hashtable-set! keys alias (->epki key password))
      (hashtable-set! (slot-ref keystore 'certs) alias (car certs))
      (for-each (lambda (cert)
		  (hashtable-set! chain
		    (make-cert-id (x509-certificate-get-public-key cert))
		    cert))
		certs)))
  
  (define (pkcs12-keystore-get-certificate-chain keystore alias)
    (define (process-chain c acc)
      (define (find-by-issuer-dn c)
	;; verify raises an error when failed...
	(define (verify c msg sig)
	  (guard (e (else #f))
	    (x509-verity c msg sig)))
	(let ((issuer-dn (x509-certificate-get-issuer-dn c))
	      (subject-dn (x509-certificate-get-subject-dn c)))
	  (if (equal? issuer-dn subject-dn)
	      #f
	      (let ((certs (hashtable->alist (slot-ref keystore 'chain-certs))))
		(find 
		 (lambda (k&v)
		   (and-let* ((crt (cdr k&v))
			      ( (equal? issuer-dn
					(x509-certificate-get-subject-dn crt)) )
			      (pk (x509-certificate-get-public-key crt))
			      ;; is this correct?
			      ( (verify c (export-public-key pk) 
					(x509-certificate-get-signature c))) )
		     crt))
		 certs)))))
      (if c
	  ;; TODO handle authority key identifier 
	  (let ((next-c (find-by-issuer-dn c)))
	    (process-chain next-c (cons c acc)))
	  (reverse! acc)))
    (if (hashtable-contains? (slot-ref keystore 'keys) alias)
	'()
	(let ((c (pkcs12-keystore-get-certificate keystore alias)))
	  (process-chain c '()))))

  (define (pkcs12-keystore-aliases keystore)
    (let ((ca (hashtable-keys-list (slot-ref keystore 'certs)))
	  (ks (hashtable-keys-list (slot-ref keystore 'keys))))
      (lset-union string=? ca ks)))
)
