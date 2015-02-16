;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/keystore/jks.scm - JKS (JCEKS)
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

(library (security keystore jks)
    (export <jks-keystore> jks-keystore?
	    <jceks-keystore> jceks-keystore?

	    ;; load
	    load-jks-keystore
	    
	    )
    (import (rnrs)
	    (clos user)
	    (crypto)
	    (math)
	    (rsa pkcs :5)
	    (rsa pkcs :8)
	    (rsa pkcs :10)
	    (binary io)
	    (rfc x.509)
	    (srfi :19 time)
	    (sagittarius)
	    (sagittarius control)
	    (security keystore interface))

  (define-class <jks-entry> ()
    ((date :init-keyword :date)))

  (define-class <private-key-entry> (<jks-entry>)
    (;; encrypted private key
     (protected-key :init-keyword :protected-key)
     ;; certificate chain
     (chain :init-keyword :chain)))

  (define-class <certificate-entry> (<jks-entry>)
    ((certificate :init-keyword :certificate)))

  (define-class <secret-key-entry> (<jks-entry>)
    ((sealed-key :init-keyword :sealed-key)))
  
  (define-class <keystore-meta> (<class>)
    ((magic :init-keyword :magic)))

  (define-class <base-keystore> (<keystore>)
    ((entries :init-form (make-hashtable string-ci-hash string-ci=?))))

  ;; Keystores. These are not is-a? relation
  ;; TODO Should they?
  (define-class <jks-keystore> (<base-keystore>)
    ()
    :metaclass <keystore-meta>
    :magic #xfeedfeed)
  (define (jks-keystore? o) (is-a? o <jks-keystore>))

  (define-class <jceks-keystore> (<base-keystore>)
    ()
    :metaclass <keystore-meta>
    :magic #xcececece)
  (define (jceks-keystore? o) (is-a? o <jceks-keystore>))

  (define (default-secret-key-handler . ignore) 'ignore)
  (define (load-jks-keystore bin password 
		     :key (secret-key-handler default-secret-key-handler))
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
      (unless (and (or (= magic #xfeedfeed) (= magic #xcececece))
		   (or (= version 1) (= version 2)))
	(error 'load-jks-keystore "invalid magic or version number"))
      (let* ((ks (make (if (= magic #xfeedfeed)
			   <jks-keystore>
			   <jceks-keystore>)))
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
	ks)))
	
)
