;;; -*- Scheme -*-
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
    (export pkcs12-keystore? make-pkcs12-keystore
	    load-pkcs12-keystore-file load-pkcs12-keystore
	    ;; entry accessors
	    pkcs12-keystore-get-key
	    pkcs12-keystore-get-certificate

	    ;; store
	    store-pkcs12-keystore
	    store-pkcs12-keystore-to-file
	    ;; TODO support
	    ;; pkcs12-keystore-set-key!
	    ;; pkcs12-keystore-set-certificate!

	    ;; read only accessors 
	    ;; (for debugging use won't be documented)
	    pkcs12-keystore-keys pkcs12-keystore-key-certificates
	    pkcs12-keystore-certificates)
    (import (rnrs)
	    (clos user)
	    (rsa pkcs :5)
	    (rsa pkcs :12 cipher)
	    (rfc hmac)
	    (rename (rfc x.509) (algorithm-identifier-id get-id))
	    (crypto)
	    (math)
	    (asn.1)
	    (util hashtables)
	    (sagittarius)
	    (sagittarius control))

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
    (make-ber-sequence (slot-ref o 'content-type) (slot-ref o 'content)))

  (define-class <digest-info> (<asn.1-encodable>)
    ((digest :init-keyword :digest)
     (algorithm-identifier :init-keyword :algorithm-identifier)))
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
    ;; todo
    )
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
	:mac-data (make-mac-data (asn.1-sequence-get seq 2)))))
  (define-method make-pfx ((mi <content-info>)
			   (md <mac-data>))
    (make <pfx> :content-info mi :mac-data md))
  (define-method asn.1-encodable->asn.1-object ((o <pfx>))
    (let ((s (make-ber-sequence)))
      (asn.1-sequence-add s (make-der-integer 3))
      (asn.1-sequence-add s (slot-ref o 'content-info))
      ;; TODO mac-data can be null
      (asn.1-sequence-add s (slot-ref o 'mac-data))
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
	  (value (slot-ref o 'value))
	  (attr (slot-ref o 'attributes)))
      (if attr
	  (make-der-sequence id value attr)
	  (make-der-sequence id value))))

  (define-class <private-key-info> (<asn.1-encodable>)
    ((id :init-keyword :id)
     (attributes :init-keyword :attributes)
     (private-key :init-keyword :private-key)))
  (define-method make-private-key-info ((s <asn.1-sequence>))
    (unless (zero? (der-integer->integer (asn.1-sequence-get s 0)))
      (assertion-violation 'make-private-key-info
			   "wrong version for private key info"))
    (let* ((id (make-algorithm-identifier (asn.1-sequence-get s 1)))
	   (ain (open-bytevector-input-port
		 (slot-ref (asn.1-sequence-get s 2) 'string)))
	   (priv-key (read-asn.1-object ain))
	   (attributes (if (> (asn.1-sequence-size s) 3)
			   (make-der-set (asn.1-sequence-get s 3))
			   #f)))
      (make <private-key-info> :id id :attributes attributes 
	    :private-key priv-key)))

  (define-class <encrypted-private-key-info> (<asn.1-encodable>)
    ((id   :init-keyword :id)
     (data :init-keyword :data)))
  (define-method make-encrypted-private-key-info ((s <asn.1-sequence>))
    (make <encrypted-private-key-info>
      :id (make-algorithm-identifier (asn.1-sequence-get s 0))
      :data (asn.1-sequence-get s 1)))
  (define-method make-encrypted-private-key-info ((id <algorithm-identifier>)
						  (data <bytevector>))
    (make <encrypted-private-key-info> :id id 
	  :data (make-der-octet-string data)))
  (define-method asn.1-encodable->asn.1-object 
    ((o <encrypted-private-key-info>))
    (make-der-sequence (slot-ref o 'id) (slot-ref o 'data)))
  (define-method write-object ((o <encrypted-private-key-info>) (p <port>))
    (format p "#<encrypted-private-key-info~%~a~%~a>"
	    (slot-ref o 'id)
	    (slot-ref o 'data)))

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

  (define-class <pkcs12-keystore> ()
    ;; we are not so flexible for algorithms
    ((key-algorithm :init-value pbe-with-sha-and3-keytripledes-cbc)
     (keys      :init-form (make-hashtable string-ci-hash string-ci=?)
		:reader pkcs12-keystore-keys)
     (key-certs :init-form (make-string-hashtable)
		:reader pkcs12-keystore-key-certificates)
     (cert-algorithm :init-value pbe-with-sha-and3-keytripledes-cbc)
     (certs     :init-form (make-string-hashtable)
		:reader pkcs12-keystore-certificates)
     (chain-certs :init-form (make-equal-hashtable)
		  :reader pkcs12-keystore-chain-certificates)
     (local-ids :init-form (make-string-hashtable))
     ;; bag attributes holds bag attributes of the object
     (bag-attributes :init-form (make-eq-hashtable))))
  (define (make-pkcs12-keystore) (make <pkcs12-keystore>))
  (define (pkcs12-keystore? o) (is-a? o <pkcs12-keystore>))

  (define *rsa-private-key-oid* "1.2.840.113549.1.1.1")

  (define (default-extra-data-handler data)
    ;; do nothing
    )

  (define (pkcs12-keystore-get-key keystore name)
    (let ((keys (slot-ref keystore 'keys)))
      (cond ((hashtable-ref keys name #f))
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

    (define (create-private-key-from-private-key-info info)
      (if (equal? (get-id (slot-ref info'id)) *rsa-private-key-oid*)
	  (import-private-key RSA (slot-ref info 'private-key))
	  (assertion-violation 'load-pkcs12-keystore "not supported"
			       (slot-ref info 'id))))

    ;; Seems PKCS#12 uses the same password as store password.
    ;; NB: at least bouncy castle does like that. so keep it simple.
    (define (unwrap-key alg-id data password)
      (let* ((plain-key (cipher-util alg-id password 
				     (slot-ref data 'string) decrypt))
	     (asn.1-key
	      (read-asn.1-object (open-bytevector-input-port plain-key)))
	     (priv-key-info (make-private-key-info asn.1-key)))
	(create-private-key-from-private-key-info priv-key-info)))

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
			       (unwrap-key (slot-ref e-in 'id)
					   (slot-ref e-in 'data)
					   password))
			     (create-private-key-from-private-key-info
			      (make-private-key-info (slot-ref b 'value))))))
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
    (let* ((r (read-asn.1-object in))
	   (pkcs12 (make-pfx r))
	   (info   (slot-ref pkcs12 'content-info))
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
	      ;; TODO implementis
	      #;
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
  (define (store-pkcs12-keystore keystore out password)
    ;; should we make this keystore slot?
    (define prng (secure-random RC4))
    (define salt-size 20) ;; SHA-1 hash size
    (define min-iteration 1024)

    (define (wrap-key alg-name key-bv password pbe-params)
      (let* ((param (make-pbe-parameter 
		     (pkcs12-pbe-params-get-iv pbe-params)
		     (pkcs12-pbe-params-get-iterations pbe-params)))
	     (k (generate-secret-key alg-name password))
	     (pbe-cipher (cipher alg-name k :parameter param)))
	(encrypt pbe-cipher key-bv)))

    (define (bag-attributes keystore obj)
      (hashtable-ref (slot-ref keystore 'bag-attributes) obj #f))

    (define (create-subject-key-id pubkey)
      (let ((info (make-subject-public-key-info
		   (make-der-sequence
		    (make-algorithm-identifier
		     *rsa-private-key-oid*
		     (make-der-null))
		   (read-asn.1-object 
		    (open-bytevector-input-port
		     (export-public-key RSA pubkey)))))))
	(make-subject-key-identifier
	 (hash SHA-1 (encode (subject-public-key-info-key-data info))))))
	    
    (define (process-key-bag-attribute keystore obj bag-attr name)
      (let ((nm (hashtable-ref bag-attr *pkcs-9-at-friendly-name* #f))
	    (sets (make-der-set)))
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
	   (asn.1-set-add sets (make-der-sequence oid (make-der-set value))))
	 bag-attr)
	(when (zero? (asn.1-set-size sets))
	  (let ((ct (if (x509-certificate? obj) ;; other certificate?
			obj
			(pkcs12-keystore-get-certificate keystore name))))
	    (asn.1-sequence-add sets
				(make-der-sequence
				 *pkcs-9-at-local-key-id*
				 (make-der-set 
				  (create-subject-key-id 
				   (x509-certificate-get-public-key ct)))))
	    (asn.1-sequence-add sets
				(make-der-sequence
				 *pkcs-9-at-friendly-name*
				 (make-der-set (make-der-bmp-string name))))))
	sets))
	    
    (define (process-keys keys)
      (let ((seq (make-der-sequence))
	    (key-algorithm (slot-ref keystore 'key-algorithm)))
	(hashtable-for-each
	 (lambda (name key)
	   (define (make-encrypted-key-content key)
	     (let ((key-bytes (export-private-key RSA key)))
	       (encode
		(make-der-sequence
		 (make-der-integer 0)
		 (make-algorithm-identifier *rsa-private-key-oid*
					    (make-der-null))
		 (make-der-octet-string key-bytes)))))
	   (let* ((salt (read-random-bytes prng salt-size))
		  (param (make-pkcs12-pbe-params salt min-iteration))
		  (key-bytes (wrap-key key-algorithm
				       (make-encrypted-key-content key)
				       ;; (export-private-key RSA key)
				       password param))
		  (alg-id (make-algorithm-identifier 
			   (cdr (assq key-algorithm *reverse-mapping*))
			   (der-encodable->der-object param)))
		  (key-info (make-encrypted-private-key-info alg-id key-bytes))
		  (bag-attr (bag-attributes keystore key))
		  (names (process-key-bag-attribute keystore key 
						    bag-attr name)))
	     (let ((bag (make-safe-bag *pkcs-8-shrouded-key-bag*
				       (der-encodable->der-object key-info)
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
	(let ((nm (hashtable-ref bag-attr *pkcs-9-at-friendly-name* #f))
	      (sets (make-der-set)))
	  (unless (and nm (equal? (asn.1-string->string nm) name))
	    (hashtable-set! bag-attr *pkcs-9-at-friendly-name* 
			    (make-der-bmp-string name)))
	  (hashtable-for-each
	   (lambda (oid value)
	     ;; comment from Bouncy Castle
	     ;; a certificate not immediately linked to a key doesn't require
	     ;; a local-key-id and will confuse some PKCS12 implementations
	     (unless (equal? oid *pkcs-9-at-local-key-id*)
	       (asn.1-set-add sets 
			      (make-der-sequence oid (make-der-set value)))))
	   bag-attr)
	  (when (and handle-empty? (zero? (asn.1-set-size sets)))
	    (asn.1-sequence-add sets
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
		    (names (process-cert-bag-attributes bag-attr cert-id #f)))
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
		    (names (process-bag-attributes bag-attr cert-id #f)))
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
      (define (compute-mac data password)
	(let* ((param (make-pbe-parameter salt min-iteration))
	       ;; key derivation doesn't consider encryption scheme
	       ;; so just use DES for now.
	       (key (generate-secret-key pbe-with-sha1-and-des password)))
	  (let-values (((dkey iv) (derive-key&iv (slot-ref key 'type)
						 key param)))
	    (hash HMAC data :key dkey :hash SHA-1))))
      (let ((res (compute-mac data password))
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
)
