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
    (export load-keystore
	    ;; read only accessors
	    keystore-keys keystore-key-certificates
	    keystore-certificates)
    (import (rnrs)
	    (clos user)
	    (rsa pkcs :12 cipher)
	    (rfc x.509)
	    (crypto)
	    (asn.1)
	    (sagittarius)
	    (sagittarius control))

  ;; oid-cipher mapping
  (define *mapping*
    `(("1.2.840.113549.1.12.1.3" . ,pbe-with-sha-and3-keytripledes-cbc)
      ("1.2.840.113549.1.12.1.6" . ,pbe-with-sha-and-40bit-rc2-cbc)))

  (define-class <content-info> (<asn.1-encodable>)
    ((content-type :init-keyword :content-type)
     (content      :init-keyword :content)))
  (define-method make-content-info ((s <asn.1-sequence>))
    ;; assume it has both
    (let ((info (asn.1-sequence-get s 0))
	  (content (asn.1-sequence-get s 1)))
      ;; content must be tagged object
      (make <content-info>
	:content-type info
	:content (der-encodable->der-object content))))
  (define-method der-encode ((o <content-info>))
    (let ((s (make-ber-sequence)))
      (asn.1-sequence-add s (slot-ref o 'content-type))
      (asn.1-sequence-add s (slot-ref o 'content))
      (der-encode s)))

  ;; for now we don't do this much
  (define-class <mac-data> (<asn.1-encodable>)
    ((digest-info :init-keyword :digest-info)
     (salt        :init-keyword :salt)
     (iteration-count :init-keyword :iteration-count :init-value 1)))
  (define-method make-mac-data ((s <asn.1-sequence>))
    ;; todo
    )

  (define-class <authenticated-safe> (<asn.1-encodable>)
    ((info :init-keyword :info)))
  (define-method make-authenticated-safe ((s <asn.1-sequence>))
    (let* ((len (asn.1-sequence-size s))
	   (info (make-vector len)))
      (dotimes (i len)
	(vector-set! info i (make-content-info (asn.1-sequence-get s i))))
      (make <authenticated-safe> :info info)))


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
  (define-method der-encode ((o <pfx>))
    (let ((s (make-ber-sequence)))
      (asn.1-sequence-add s (make-der-integer 3))
      (asn.1-sequence-add s (slot-ref o 'content-info))
      ;; TODO mac-data can be null
      (asn.1-sequence-add s (slot-ref o 'mac-data))
      (der-encode s)))

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
    (slot-ref (slot-ref id 'object-id) 'identifier))
  (define-method write-object ((o <algorithm-identifier>) (p <port>))
    (format p "#<algorithm-identifier ~a~%~a>" (get-id o)
	    (slot-ref o 'parameters)))

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

  (define-class <cert-bag> (<asn.1-encodable>)
    ((seq :init-keyword :seq)
     (id  :init-keyword :id)
     (value :init-keyword :value)))
  (define-method make-cert-bag ((s <asn.1-sequence>))
    (make <cert-bag>
      :seq s
      :id (asn.1-sequence-get s 0)
      :value (der-encodable->der-object (asn.1-sequence-get s 1))))

  (define-class <pkcs12-pbe-params> (<asn.1-encodable>)
    ((iterations :init-keyword :iterations)
     (iv :init-keyword :iv)))
  (define-method make-pkcs12-pbe-params ((s <asn.1-sequence>))
    (make <pkcs12-pbe-params>
      :iv (asn.1-sequence-get s 0)
      :iterations (asn.1-sequence-get s 1)))
  (define (pkcs12-pbe-params-get-iterations o)
    (der-integer->integer (slot-ref o 'iterations)))
  (define (pkcs12-pbe-params-get-iv o)
    (slot-ref (slot-ref o 'iv) 'string))

  (define-class <pkcs12-keystore> ()
    ((key-algorithm)
     (keys      :init-form (make-hashtable string-ci-hash string-ci=?)
		:reader keystore-keys)
     (key-certs :init-form (make-string-hashtable)
		:reader keystore-key-certificates)
     (cert-algorithm)
     (certs     :init-form (make-string-hashtable)
		:reader keystore-certificates)
     (chain-certs)
     (local-ids :init-form (make-string-hashtable))))

  (define *rsa-private-key-oid* "1.2.840.113549.1.1.1")

  (define (default-extra-data-handler data)
    ;; do nothing
    )

  (define (load-keystore in-file password
			 :key (extra-data-handler default-extra-data-handler))

    (define (cipher-util alg-id password data processer)
      (let ((alg-name (cond ((assoc (get-id alg-id) *mapping*) => cdr)
			    (else #f)))
	    (pbe-params (make-pkcs12-pbe-params (slot-ref alg-id 'parameters))))
	(unless alg-name
	  (assertion-violation 'load-keystore
			       "unknown algorithm identifier" alg-id))
	(let* ((param (make-pbe-parameter 
		       (pkcs12-pbe-params-get-iv pbe-params)
		       (pkcs12-pbe-params-get-iterations pbe-params)))
	       (k (generate-secret-key alg-name password))
	       (pbe-cipher (cipher alg-name k :parameter param)))
	  (processer pbe-cipher data))))

    (define (create-private-key-from-private-key-info info)
      (if (equal? (get-id (slot-ref info'id)) *rsa-private-key-oid*)
	  (import-private-key RSA (slot-ref info 'private-key))
	  (assertion-violation 'load-keystore "not supported"
			       (slot-ref info 'id))))

    (define (unwrap-key alg-id data password)
      (let* ((plain-key (cipher-util alg-id password (slot-ref data 'string)
				     decrypt))
	     (asn.1-key
	      (read-asn.1-object (open-bytevector-input-port plain-key)))
	     (priv-key-info (make-private-key-info asn.1-key)))
	(create-private-key-from-private-key-info priv-key-info)))

    (define (crypt-data alg-id password data)
      (cipher-util alg-id password data decrypt))

    (define (process-shrouded-key-bag b keystore unwrap?)
      (let* ((private-key (if unwrap?
			      (let ((e-in (make-encrypted-private-key-info 
					   (slot-ref b 'value))))
				(unwrap-key (slot-ref e-in 'id)
					    (slot-ref e-in 'data)
					    password))
			      (create-private-key-from-private-key-info
			       (make-private-key-info (slot-ref b 'value)))))
	     (alias #f)
	     (local-id #f))
	(dolist (seq (slot-ref (slot-ref b 'attributes) 'set))
	  (let ((oid (asn.1-sequence-get seq 0))
		(attr-set (asn.1-sequence-get seq 1))
		(attr #f))
	    (when (> (asn.1-set-size attr-set) 0)
	      (set! attr (asn.1-set-get attr-set 0)))
	    ;; TODO check existing
	    (cond ((equal? oid *pkcs-9-at-friendly-name*)
		   (set! alias (asn.1-string->string attr))
		   (hashtable-set! (slot-ref keystore 'keys)
				   alias
				   private-key))
		  ((equal? oid *pkcs-9-at-local-key-id*)
		   (set! local-id attr)))))
	(if local-id
	    (let ((name (format "~X" (bytevector->integer
				      (slot-ref local-id 'string)))))
	      (if alias
		  (hashtable-set! (slot-ref keystore 'keys) name
				  private-key)
		  (hashtable-set! (slot-ref keystore 'local-ids)
				  alias 
				  name)))
	    (hashtable-set! (slot-ref keystore 'keys) "unmarkded"
			    private-key))))

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
		       (extra-data-handler b)
		       (loop (+ i 1) r))))))))

    (call-with-input-file in-file
      (lambda (in)
	(let* ((r (read-asn.1-object in))
	       (pkcs12 (make-pfx r))
	       (info   (slot-ref pkcs12 'content-info))
	       (keystore (make <pkcs12-keystore>))
	       (chain '()))
	  ;; first keys
	  (when (equal? (slot-ref info 'content-type) *pkcs-7-data*)
	    (let* ((auth-safe (make-authenticated-safe
			       (read-asn.1-object
				(open-bytevector-input-port
				 (slot-ref (slot-ref info 'content) 'string)))))
		   (ac (slot-ref auth-safe 'info)))
	      (vector-for-each 
	       (lambda (c)
		 (cond ((equal? *pkcs-7-data*
				(slot-ref c 'content-type))
			(set! chain (append chain (process-data c keystore))))
		       ((equal? *pkcs-7-encrypted-data*
				(slot-ref c 'content-type))
			(set! chain (append chain (process-encrypted-data
						    c keystore))))
		       (else
			(extra-data-handler c))))
	       ac)))
	  ;; then certs
	  (dolist (b chain)
	    (let ((cb (make-cert-bag (slot-ref b 'value))))
	      (unless (equal? (slot-ref cb 'id) *pkcs-9-x509-certificate*)
		(assertion-violation 'load-keystore
				     "Unsupported certificate type"
				     (slot-ref cb 'id)))
	      (let ((cert (make-x509-certificate (slot-ref (slot-ref cb 'value)
							   'string)))
		    (local-id #f)
		    (alias #f))
		(when (slot-ref b 'attributes)
		  (dolist (seq (slot-ref (slot-ref b 'attributes) 'set))
		    (let ((oid (asn.1-sequence-get seq 0))
			  (attr-set (asn.1-sequence-get seq 1))
			  (attr #f))
		      (when (> (asn.1-set-size attr-set) 0)
			(set! attr (asn.1-set-get attr-set 0)))
		      ;; TODO check existing
		      (cond ((equal? oid *pkcs-9-at-friendly-name*)
			     (set! alias (asn.1-string->string attr)))
			    ((equal? oid *pkcs-9-at-local-key-id*)
			     (set! local-id attr)))))
		  ;; TODO cert-id
		  #;(hashtable-set! (slot-ref keystore 'chain-certs)
		  (make-cert-id
		  (x509-certificate-get-public-key cert))
		  cert)
		  ;; TODO unmarked key
		  (when local-id
		    (let ((name (format "~X" (slot-ref local-id 'string))))
		      (hashtable-set! (slot-ref keystore 'key-certs)
				      name cert)))
		  (when alias
		    (hashtable-set! (slot-ref keystore 'certs)
				    alias cert))))))
	  keystore))
      :transcoder #f))
)