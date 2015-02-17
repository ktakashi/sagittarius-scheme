;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/keystore/jceks/keystore.scm - JCEKS keystore
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (security keystore jceks keystore)
    (export <base-jceks-keystore>
	    generate-load-jceks-key-store
	    generate-jceks-get-key
	    generate-jceks-get-certificate
	    generate-jceks-get-certificate-chain
	    generate-jceks-get-creation-date
	    generate-jceks-set-key!
	    generate-jceks-set-certificate!)
    (import (rnrs)
	    (clos user)
	    (crypto)
	    (math)
	    (asn.1)
	    (rsa pkcs :5)
	    (rsa pkcs :8)
	    (rsa pkcs :10)
	    (binary io)
	    (rfc x.509)
	    (srfi :19 time)
	    (util bytevector)
	    (sagittarius)
	    (sagittarius control)
	    (security keystore interface)
	    (security keystore jceks cipher))

  (define-class <jks-entry> ()
    ((date :init-keyword :date)))

  (define-class <private-key-entry> (<jks-entry>)
    (;; encrypted private key
     (protected-key :init-keyword :protected-key)
     ;; certificate chain
     (chain :init-keyword :chain)))

  (define-class <certificate-entry> (<jks-entry>)
    ((certificate :init-keyword :certificate)))

  ;; it's here but not used
  (define-class <secret-key-entry> (<jks-entry>)
    ((sealed-key :init-keyword :sealed-key)))
  
  (define-class <base-jceks-keystore> (<keystore>)
    ((entries :init-form (make-hashtable string-ci-hash string-ci=?))
     (prng :init-form (secure-random RC4))))

  ;; 1.3.6.1.4.1.42 is Sun's OID thus these are Sun's specific ones
  ;; geez
  (define-constant +jks-keystore-oid+          "1.3.6.1.4.1.42.2.17.1.1")
  (define-constant +pbe-with-md5-and-des3-oid+ "1.3.6.1.4.1.42.2.19.1")

  ;; SHA-1 hash size
  (define-constant +salt-length+ 20)
  (define-constant +digest-length+ 20)

  (define (compute-xor xor salt pw)
    (define sha (hash-algorithm SHA-1))
    (define xor-len (bytevector-length xor))
    (define round (ceiling (/ (bytevector-length xor) +digest-length+)))
    (let loop ((i 0) (offset 0) (digest salt))
      (if (= i round)
	  xor
	  (begin
	    (hash-init! sha)
	    (hash-process! sha pw)
	    (hash-process! sha digest)
	    (hash-done! sha digest)
	    (if (< i (- round 1))
		(bytevector-copy! digest 0 xor offset +digest-length+)
		(bytevector-copy! digest 0 xor offset 
				  (- xor-len offset)))
	    (loop (+ i 1) (+ offset +digest-length+) digest)))))
  ;; getters
  (define (generate-jceks-get-key keystore? crypto?)
    (lambda (keystore alias password)
      (define (unwrap-jks data)
	(define sha (hash-algorithm SHA-1))
	(define (compute-round data)
	  (- (bytevector-length data) +salt-length+ +digest-length+))
	(let* ((enc-len (compute-round data))
	       (enc-data (bytevector-copy data +salt-length+
					  (+ +salt-length+ enc-len)))
	       (salt (bytevector-copy data 0 +salt-length+))
	       (pw-bv (string->utf16 password 'big))
	       (xor (compute-xor (make-bytevector enc-len) salt pw-bv)))

	  (let ((r (bytevector-xor enc-data xor)))
	    ;; check integrity.
	    (hash-init! sha)
	    (hash-process! sha pw-bv)
	    (hash-process! sha r)
	    (hash-done! sha salt)
	    (unless (bytevector=? salt (bytevector-copy data (+ +salt-length+
								enc-len)))
	      (error 'jks-keystore-get-key "Cannot recover key"))
	    (pki->private-key (make-private-key-info r)))))

      (define (unwrap-key alg-id data)
	;; must be der-sequence like this structure
	;; sequence
	;;   octet-string : salt
	;;   integer      : iteration-count
	(let* ((param (algorithm-identifier-parameters alg-id))
	       (pbe-param (make-pbe-parameter 
			   (der-octet-string-octets 
			    (asn.1-sequence-get param 0))
			   (der-integer->integer (asn.1-sequence-get param 1))))
	       (key (generate-secret-key pbe-with-md5-and-des3 password))
	       (pbe-cipher (cipher pbe-with-md5-and-des 
				   key :parameter pbe-param)))

	  (pki->private-key
	   (make-private-key-info 
	    (decrypt pbe-cipher (der-octet-string-octets data))))))
      (define (unwrap key)
	;; assume it's private-key-entry for now
	(let* ((epki (make-encrypted-private-key-info 
		      (slot-ref key 'protected-key)))
	       (id (encrypted-private-key-info-id epki))
	       (data (encrypted-private-key-info-data epki)))
	  (cond ((string=? (algorithm-identifier-id id) +jks-keystore-oid+)
		 (unwrap-jks (der-octet-string-octets data)))
		((and crypto?
		      (string=? (algorithm-identifier-id id) 
				+pbe-with-md5-and-des3-oid+))
		 (unwrap-key id data))
		(else (error 'jks-keystore-get-key "unknown oid" id)))))
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-get-key 
			       "Unknown keystore" keystore))
      (cond ((hashtable-ref (slot-ref keystore 'entries) alias #f) => unwrap)
	    (else #f))))
  (define (generate-jceks-get-certificate keystore?)
    (lambda (keystore alias)
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-get-key 
			       "Unknown keystore" keystore))
      (cond ((hashtable-ref (slot-ref keystore 'entries) alias #f) => 
	     (lambda (e)
	       (cond ((is-a? e <certificate-entry>)
		      (slot-ref e 'certificate))
		     ((is-a? e <private-key-entry>)
		      (car (slot-ref e 'chain)))
		     (else #f))))
	    (else #f))))
  (define (generate-jceks-get-certificate-chain keystore?)
    (lambda (keystore alias)
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-get-key 
			       "Unknown keystore" keystore))
      (cond ((hashtable-ref (slot-ref keystore 'entries) alias #f) => 
	     (lambda (e)
	       (cond ((is-a? e <private-key-entry>) (slot-ref e 'chain))
		     (else #f))))
	    (else #f))))

  (define (generate-jceks-get-creation-date keystore?)
    (lambda (keystore alias)
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-get-key 
			       "Unknown keystore" keystore))
      (cond ((hashtable-ref (slot-ref keystore 'entries) alias #f) => 
	     (lambda (e) (slot-ref e 'date)))
	    (else #f))))

  ;; setters
  (define (generate-jceks-set-key! keystore? crypto?)
    (lambda (keystore alias key password certs)
      (define prng (slot-ref keystore 'prng))
      (define (wrap-jks key) 
	(define sha (hash-algorithm SHA-1))
	(define (gen-salt data)
	  ;; compute-xor change given salt destructively so we need to
	  ;; put it here...
	  (let ((salt (read-random-bytes prng +salt-length+)))
	    (bytevector-copy! salt 0 data 0 +salt-length+)
	    salt))
	(let* ((pki-bv (encode (make-private-key-info key)))
	       ;; encrypted value
	       (data (make-bytevector (+ (bytevector-length pki-bv)
					 +salt-length+
					 +digest-length+)))
	       (salt (gen-salt data))
	       (len (bytevector-length pki-bv))
	       (pw  (string->utf16 password 'big))
	       (xor  (compute-xor (make-bytevector len) salt pw)))

	  (bytevector-copy! (bytevector-xor pki-bv xor) 0
			    data +salt-length+ len)

	  (hash-init! sha)
	  (hash-process! sha pw)
	  (hash-process! sha pki-bv)
	  (hash-done! sha salt)

	  (bytevector-copy! salt 0 data (+ len +salt-length+) +digest-length+)
	  (make-encrypted-private-key-info
	   (make-algorithm-identifier +jks-keystore-oid+ (make-der-null))
	   data)))
	  
      (define (wrap-jceks key)
	(let* ((salt (read-random-bytes 8)) ;; it's fixed length ... *sigh*
	       (count 1024)		  ;; make it a bit bigger
	       (param (make-algorithm-identifier
		       +pbe-with-md5-and-des3-oid+
		       (make-der-sequence
			(make-der-octet-string salt)
			(make-der-integer count))))
	       (pbe-param (make-pbe-parameter salt count))
	       (key (generate-secret-key pbe-with-md5-and-des3 password))
	       (pbe-cipher (cipher pbe-with-md5-and-des 
				   key :parameter pbe-param)))
	  (make-encrypted-private-key-info param
	    (encrypt pbe-cipher (encode (make-private-key-info key))))))
      (define (wrap key)
	(if crypto?
	    (wrap-jceks key)
	    (wrap-jks key)))
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-set-key!
			       "Unknown keystore" keystore))
      (unless (for-all x509-certificate? certs)
	(assertion-violation 'jks-keystore-set-key!
	  "Private key must be accompanied by certificate chain"))
      (let ((epki (wrap key))
	    (entries (slot-ref keystore 'entries)))
	(hashtable-set! entries alias (make <private-key-entry>
					:data (current-time)
					:protected-key (encode epki)
					:chain certs)))))

  (define (generate-jceks-set-certificate! keystore?)
    (lambda (keystore alias cert)
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-set-certificate!
			       "Unknown keystore" keystore))
      (let ((entries (slot-ref keystore 'entries)))
	(cond ((hashtable-ref entries alias #f) => 
	       (lambda (e)
		 (cond ((is-a? e <private-key-entry>)
			(error 'jks-keystore-set-certificate!
			       "Can't overwrite own certificate"))))))
	(hashtable-set! entries alias
			(make <certificate-entry>
			  :date (current-time)
			  :certificate cert)))))

  (define (default-secret-key-handler . ignore) 
    (error 'load-jks-keystore "secret key is not supported"))

  (define (generate-load-jceks-key-store class magics)
    (lambda (bin password :key (secret-key-handler default-secret-key-handler))
      (define (pre-key-hash password)
	(and password
	     (let ((md (hash-algorithm SHA-1)))
	       (hash-init! md)
	       (hash-process! md (string->utf16 password 'big))
	       ;; funny huh?
	       (hash-process! md (string->utf8 "Mighty Aphrodite"))
	       md)))
      (define (get-utf8 in)
	(let ((len (get-u16 in 'big)))
	  (utf8->string (get-bytevector-n in len))))

      (define (load-key in)
	(let ((len (get-s32 in 'big)))
	  (get-bytevector-n in len)))
      (define (load-certificate in version)
	;; ignore certificate type, we only support X.509
	(when (= version 2) (get-utf8 in))
	(let ((bv (get-bytevector-n in (get-u32 in 'big))))
	  (make-x509-certificate bv)))
      
      (define (load-chain in version)
	(let ((count (get-u32 in 'big)))
	  (let loop ((i 0) (r '()))
	    (if (= i count)
		(reverse! r)
		(loop (+ i 1) (load-certificate in version))))))

      (define (timestamp->time millis)
	(define (milliseconds->sec&nano msec)
	  (let ((sec (div msec 1000))
		(nsec (* (mod msec 1000) 1000000)))
	    (values sec nsec)))
	(let-values (((sec nsec) (milliseconds->sec&nano millis)))
	  (make-time time-utc nsec sec)))
      (define done? #f)
      (define (make-digest-input-port in digest)
	(define (read! bv start count)
	  (let ((t (get-bytevector-n in count)))
	    (if (eof-object? t)
		0
		(let ((len (bytevector-length t)))
		  (bytevector-copy! t 0 bv start len)
		  (unless done? (hash-process! digest t))
		  len))))
	(define (close) (close-port in))
	(make-custom-binary-input-port "digest port" read! #f #f close))
      
      (let* ((md (pre-key-hash password))
	     (in (if password (make-digest-input-port bin md) bin))
	     (magic (get-u32 in 'big))
	     (version (get-s32 in 'big)))
	(unless (and (memv magic magics)
		     (or (= version 1) (= version 2)))
	  (error 'load-jks-keystore "invalid magic or version number"))
	(let* ((ks (make class))
	       (entries (slot-ref ks 'entries)))
	  (dotimes (i (get-u32 in 'big))
	    (let ((tag (get-u32 in 'big))
		  (alias (get-utf8 in))
		  (timestamp (get-u64 in 'big)))
	      (case tag
		((1) 
		 (let ((key (load-key in))
		       (chain (load-chain in version)))
		   (hashtable-set! entries alias
				   (make <private-key-entry> 
				     :date (timestamp->time timestamp)
				     :protected-key key
				     :chain chain))))
		((2)
		 (let ((cert (load-certificate in version)))
		   (hashtable-set! entries alias
				   (make <certificate-entry>
				     :date (timestamp->time timestamp)
				     :certificate cert))))
		((3)
		 (when (= magic #xfeedfeed)
		   (error 'load-jks-keystore "Unrecogised keystore entry" tag))
		 (let ((key (secret-key-handler in)))
		   (hashtable-set! entries alias
				   (make <secret-key-entry>
				     :sealed-key key))))
		(else
		 (error 'load-jks-keystore "Unrecogised keystore entry" tag)))))
	  (and-let* (( md )
		     (bv (make-bytevector (hash-size md)))
		     ( (hash-done! md bv) )
		     ( (set! done? #t) )
		     (actual (get-bytevector-n in (hash-size md))))
	    (unless (bytevector=? bv actual)
	      (error 'load-jks-keystore 
		     "Keystore was tampered with, or password was incorrect")))
	  ks))))

  )
