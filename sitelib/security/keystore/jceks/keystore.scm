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

#!nounbound
(library (security keystore jceks keystore)
    (export <base-jceks-keystore>
	    generate-load-jceks-key-store
	    generate-store-jceks-keystore

	    generate-jceks-get-key
	    generate-jceks-get-certificate
	    generate-jceks-get-certificate-chain
	    generate-jceks-contains-alias?
	    generate-jceks-get-creation-date
	    generate-jceks-set-key!
	    generate-jceks-set-certificate!
	    generate-jceks-delete-entry!

	    generate-jceks-aliases)
    (import (rnrs)
	    (clos user)
	    (binary io)
	    (rfc x509)
	    (srfi :19 time)
	    (util bytevector)
	    (util hashtables)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius crypto asn1)
	    (sagittarius crypto random)
	    (sagittarius crypto digests)
	    (sagittarius crypto pkcs keys)
	    (sagittarius crypto pkcs pbes)
	    (sagittarius crypto pkix algorithms)
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
     (prng :init-form (secure-random-generator *prng:chacha20*))))

  ;; SHA-1 hash size
  (define-constant +salt-length+ 20)
  (define-constant +digest-length+ 20)

  (define (compute-xor xor salt pw)
    (define sha (make-message-digest *digest:sha-1*))
    (define xor-len (bytevector-length xor))
    (define round (ceiling (/ (bytevector-length xor) +digest-length+)))
    (let loop ((i 0) (offset 0) (digest salt))
      (if (= i round)
	  xor
	  (begin
	    (message-digest-init! sha)
	    (message-digest-process! sha pw)
	    (message-digest-process! sha digest)
	    (message-digest-done! sha digest)
	    (if (< i (- round 1))
		(bytevector-copy! digest 0 xor offset +digest-length+)
		(bytevector-copy! digest 0 xor offset 
				  (- xor-len offset)))
	    (loop (+ i 1) (+ offset +digest-length+) digest)))))
  ;; getters
  (define (generate-jceks-get-key keystore? crypto?)
    (lambda (keystore alias password)
     (define (unwrap-jks data)
	(define sha (make-message-digest *digest:sha-1*))
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
	    (message-digest-init! sha)
	    (message-digest-process! sha pw-bv)
	    (message-digest-process! sha r)
	    (message-digest-done! sha salt)
	    (unless (bytevector=? salt (bytevector-copy data (+ +salt-length+
								enc-len)))
	      (error 'jks-keystore-get-key "Cannot recover key"))
	    (pkcs-one-asymmetric-key-private-key
	     (bytevector->pkcs-one-asymmetric-key r)))))

      (define (unwrap key)
	;; assume it's private-key-entry for now
	(let ((epki (bytevector->pkcs-encrypted-private-key-info 
		     (slot-ref key 'protected-key))))
	  (if (string=? (x509-algorithm-identifier-oid
			 (pkcs-encrypted-private-key-info-encryption-algorithm
			  epki))
			+jks-keystore-oid+)
	      (unwrap-jks (pkcs-encrypted-private-key-info-encrypted-data epki))
	      (pkcs-one-asymmetric-key-private-key
	       (pkcs-encrypted-private-key-info->pkcs-one-asymmetric-key
		epki password)))))
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
	  (assertion-violation 'jks-keystore-get-certificate-chain 
			       "Unknown keystore" keystore))
      (cond ((hashtable-ref (slot-ref keystore 'entries) alias #f) => 
	     (lambda (e)
	       (cond ((is-a? e <private-key-entry>) (slot-ref e 'chain))
		     (else #f))))
	    (else #f))))

  (define (generate-jceks-get-creation-date keystore?)
    (lambda (keystore alias)
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-get-creation-date 
			       "Unknown keystore" keystore))
      (cond ((hashtable-ref (slot-ref keystore 'entries) alias #f) => 
	     (lambda (e) (time-utc->date (slot-ref e 'date))))
	    (else #f))))
  (define (generate-jceks-contains-alias? keystore?)
    (lambda (keystore alias)
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-contains? 
			       "Unknown keystore" keystore))
      (hashtable-contains? (slot-ref keystore 'entries) alias)))

  (define (generate-jceks-aliases keystore?)
    (lambda (keystore)
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-aliases 
			       "Unknown keystore" keystore))
      (hashtable-keys-list (slot-ref keystore 'entries))))
  
  ;; setters
  (define (generate-jceks-set-key! keystore? crypto?)
    (lambda (keystore alias key password certs)
      (define prng (slot-ref keystore 'prng))
      (define (wrap-jks key) 
	(define sha (make-message-digest *digest:sha-1*))
	(define (gen-salt data)
	  ;; compute-xor change given salt destructively so we need to
	  ;; put it here...
	  (let ((salt (random-generator-read-random-bytes prng +salt-length+)))
	    (bytevector-copy! salt 0 data 0 +salt-length+)
	    salt))
	(let* ((pki-bv (pkcs-one-asymmetric-key->bytevector
			(private-key->pkcs-one-asymmetric-key key)))
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
	  
	  (message-digest-init! sha)
	  (message-digest-process! sha pw)
	  (message-digest-process! sha pki-bv)
	  (message-digest-done! sha salt)

	  (bytevector-copy! salt 0 data (+ len +salt-length+) +digest-length+)
	  (make-pkcs-encrypted-private-key-info
	   (make-x509-algorithm-identifier +jks-keystore-oid+)
	   data)))

      (define (wrap-jceks key)
	;; it's fixed length ... *sigh*
	(let* ((salt (random-generator-read-random-bytes prng 8))
	       (count 1024))
	  (pkcs-one-asymmetric-key->pkcs-encrypted-private-key-info
	   (private-key->pkcs-one-asymmetric-key key)
	   (make-x509-algorithm-identifier
	    +pbe-with-md5-and-des3-oid+
	    (make-pkcs-pbe-parameter salt count))
	   password)))
      (define (wrap key)
	(if crypto?
	    (wrap-jceks key)
	    (wrap-jks key)))
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-set-key!
			       "Unknown keystore" keystore))
      (unless (and (not (null? certs)) (for-all x509-certificate? certs))
	(assertion-violation 'jks-keystore-set-key!
	  "Private key must be accompanied by certificate chain"))
      (let ((bytes (pkcs-encrypted-private-key-info->bytevector (wrap key)))
	    (entries (slot-ref keystore 'entries)))
	(hashtable-set! entries alias
			(make <private-key-entry>
			  :date (current-time)
			  :protected-key bytes
			  :chain certs)))))

  (define (generate-jceks-set-certificate! keystore?)
    (lambda (keystore alias cert)
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-set-certificate!
			       "Unknown keystore" keystore))
      (unless (x509-certificate? cert)
	(assertion-violation 'jks-keystore-set-certificate!
			     "X509 certificate required" cert))
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

  (define (pre-key-hash password)
    (and password
	 (let ((md (make-message-digest *digest:sha-1*)))
	   (message-digest-init! md)
	   (message-digest-process! md (string->utf16 password 'big))
	   ;; funny huh?
	   (message-digest-process! md (string->utf8 "Mighty Aphrodite"))
	   md)))

  (define (generate-load-jceks-key-store class magics)
    (lambda (bin password :key (secret-key-handler default-secret-key-handler))
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
		(loop (+ i 1) (cons (load-certificate in version) r))))))

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
		  (unless done? (message-digest-process! digest t))
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
		     (size (message-digest-digest-size md))
		     (bv (make-bytevector size))
		     ( (message-digest-done! md bv) )
		     ( (set! done? #t) )
		     (actual (get-bytevector-n in size)))
	    (unless (bytevector=? bv actual)
	      (error 'load-jks-keystore 
		     "Keystore was tampered with, or password was incorrect")))
	  ks))))

  (define (generate-store-jceks-keystore keystore? magic)
    ;; TODO should we add secret key handler?
    (lambda (keystore bout password)
      (define done? #f)
      (define (make-digest-output-port out md)
	(define (write! bv start count)
	  (put-bytevector out bv start count)
	  (unless done? (message-digest-process! md bv start (+ start count)))
	  count)
	(define (close))
	(make-custom-binary-output-port "digest port" write! #f #f close))
      
      (define (put-utf8 out str)
	(let ((bv (string->utf8 str)))
	  (put-u16 out (bytevector-length bv) 'big)
	  (put-bytevector out bv)))

      (define (write-cert out cert)
	(put-utf8 out "X509") ;; we only support this...
	(let ((bv (x509-certificate->bytevector cert)))
	  (put-u32 out (bytevector-length bv) 'big)
	  (put-bytevector out bv)))

      (define (time->millisecond time)
	(let ((sec (time-second time))
	      (nsec (time-nanosecond time)))
	  (+ (* sec 1000) (div nsec 1000000))))

      (or (keystore? keystore)
	  (assertion-violation 'store-jks-keystore
			       "Unknown keystore" keystore))
      (let* ((md (pre-key-hash password))
	     (out (if password (make-digest-output-port bout md) bout))
	     (entries (slot-ref keystore 'entries)))
	(put-u32 out magic 'big)
	(put-u32 out 2 'big) ;; always latest version
	(put-u32 out (hashtable-size entries) 'big)
	(hashtable-for-each 
	 (lambda (alias e)
	   (cond ((is-a? e <certificate-entry>)
		  (put-u32 out 2 'big)
		  (put-utf8 out alias)
		  (put-u64 out (time->millisecond (slot-ref e 'date)) 'big)
		  (write-cert out (slot-ref e 'certificate)))
		 ((is-a? e <private-key-entry>)
		  (put-u32 out 1 'big)
		  (put-utf8 out alias)
		  (put-u64 out (time->millisecond (slot-ref e 'date)) 'big)
		  (let ((epki-bv (slot-ref e 'protected-key))
			(chain (slot-ref e 'chain)))
		    (put-u32 out (bytevector-length epki-bv) 'big)
		    (put-bytevector out epki-bv)
		    (put-u32 out (length chain) 'big)
		    (for-each (lambda (cert) (write-cert out cert)) chain)))
		 ((is-a? e <secret-key-entry>)
		  ;; TODO
		  (error 'store-jks-keystore "Secret key is not supported"))
		 (else (error 'store-jks-keystore "Unknown entry" e))))
	 entries)
	(and-let* (( md )
		   (digest (make-bytevector (message-digest-digest-size md)))
		   ( (message-digest-done! md digest) )
		   ;; so that hash won't raise an error
		   ( (set! done? #t) ))
	  (put-bytevector out digest))
	(undefined))))

  (define (generate-jceks-delete-entry! keystore?)
    (lambda (keystore alias)
      (or (keystore? keystore)
	  (assertion-violation 'jks-keystore-delete-entry!
			       "Unknown keystore" keystore))
      (hashtable-delete! (slot-ref keystore 'entries) alias)))
  )
