;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; cipher.scm Cryptographic library
;;;
#!deprecated
#!nounbound
(library (crypto cipher)
    (export crypto-object?
	    cipher-keysize
	    cipher-blocksize
	    cipher-iv
	    cipher-update-aad!
	    cipher-tag! cipher-tag
	    cipher-max-tag-size
	    (rename (legacy-cipher? cipher?))
	    make-cipher
	    cipher-encrypt
	    cipher-decrypt
	    cipher-signature
	    cipher-verify
	    key-check-value

	    cipher-encrypt/tag
	    cipher-decrypt/tag
	    cipher-decrypt/verify

	    ;; parameters
	    define-mode-parameter
	    (rename (make-cipher-parameter make-composite-parameter)
		    (cipher-parameter? mode-parameter?))
	    
	    (rename (mode-name-parameter <mode-name-parameter>))
	    make-mode-name-parameter mode-name-parameter?
	    parameter-mode-name
	    <iv-parameter> make-iv-parameter iv-parameter?
	    (rename (cipher-parameter-iv parameter-iv))
	    <ctr-parameter> make-ctr-parameter ctr-parameter?
	    parameter-ctr-mode
	    <rfc3686-parameter> make-rfc3686-parameter rfc3686-parameter?
	    (rename (padding-parameter <padding-parameter>))
	    make-padding-parameter padding-parameter?
	    parameter-padder
	    <round-parameter> make-round-parameter round-parameter?
	    (rename (cipher-parameter-rounds parameter-rounds))

	    ;; signing
	    ;; supported algorithms
            (rename (*scheme:blowfish*    Blowfish)
		    (*scheme:x-tea*       X-Tea)
		    (*scheme:rc2*         RC2)
		    (*scheme:rc5*         RC5-32/12/b)
		    (*scheme:rc6*         RC6-32/20/b)
		    (*scheme:safer+*      SAFER+)
		    (*scheme:safer-k64*   SAFER-K64)
		    (*scheme:safer-sk64*  SAFER-SK64)
		    (*scheme:safer-k128*  SAFER-K128)
		    (*scheme:safer-sk128* SAFER-SK128)
		    (*scheme:aes*         AES)
		    (*scheme:aes-128*     AES-128)
		    (*scheme:aes-192*     AES-192)
		    (*scheme:aes-256*     AES-256)
		    (*scheme:twofish*     Twofish)
		    (*scheme:des*         DES)
		    (*scheme:des3*        DES3)
		    (*scheme:desede*      DESede)
		    (*scheme:cast5*       CAST5)
		    (*scheme:cast-128*    CAST-128)
		    (*scheme:noekeon*     Noekeon)
		    (*scheme:skipjack*    Skipjack)
		    (*scheme:khazad*      Khazad)
		    (*scheme:seed*        SEED)
		    (*scheme:kasumi*      KASUMI)
		    (*scheme:camellia*    Camellia))
	    ;; supported modes
	    (rename (*mode:ecb* MODE_ECB)
		    (*mode:cbc* MODE_CBC)
		    (*mode:cfb* MODE_CFB)
		    (*mode:ofb* MODE_OFB)
		    (*mode:ctr* MODE_CTR)
		    (*mode:lrw* MODE_LRW)
		    (*mode:f8*  MODE_F8)
		    (*mode:eax* MODE_EAX)
		    (*mode:ocb* MODE_OCB)
		    (*mode:ocb3* MODE_OCB3)
		    (*mode:gcm* MODE_GCM))
	    
	    ;; ctr conter mode
	    (rename (*ctr-mode:little-endian* CTR_COUNTER_LITTLE_ENDIAN)
		    (*ctr-mode:big-endian* CTR_COUNTER_BIG_ENDIAN)
		    (*ctr-mode:rfc3686* LTC_CTR_RFC3686))

	    (rename (<legacy-crypto> <crypto>)
		    (<legacy-cipher> <cipher>)
		    (<legacy-cipher-spi> <cipher-spi>))
	    <key>
	    <symmetric-key>
	    <asymmetric-key>
	    ;; for backward compatibility
	    cipher
	    (rename (cipher-encrypt encrypt)
		    (cipher-decrypt decrypt)
		    (cipher-signature sign)
		    (cipher-verify verify))
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius crypto parameters)
	    (sagittarius crypto ciphers symmetric)
	    (clos core)
	    (crypto spi)
	    (crypto key)
	    (crypto pkcs))
(define-syntax define-mode-parameter (make-define-cipher-parameter))
(define-mode-parameter mode-name-parameter
  make-mode-name-parameter mode-name-parameter?
  (mode-name parameter-mode-name))

(define-mode-parameter padding-parameter
  make-padding-parameter padding-parameter?
  (padder parameter-padder))

(define-mode-parameter (<ctr-parameter> <iv-parameter>)
  (make-ctr-parameter (lambda (p)
			(lambda (iv :optional (mode *ctr-mode:big-endian*))
			  ((p iv) mode))))
  ctr-parameter?
  (mode parameter-ctr-mode))
    
(define-mode-parameter (<rfc3686-parameter> <ctr-parameter>)
  (make-rfc3686-parameter
   (lambda (p)
     (lambda (iv nonce :optional (mode *ctr-mode:big-endian*))
       (let ((v (make-bytevector 16 0))
	     (nlen (bytevector-length nonce))
	     (ivlen (bytevector-length iv)))
	 (if (= mode *ctr-mode:big-endian*)
	     ;; NONCE || IV || ONE = 16
	     (begin
	       (bytevector-copy! nonce 0 v 0 nlen)
	       (bytevector-copy! iv 0 v nlen ivlen))
	     ;; ONE || IV || NONCE (i guess)
	     (begin
	       (bytevector-copy! iv 0 v 4 ivlen)
	       (bytevector-copy! nonce 0 v (+ 4 ivlen) nlen)))
	 ;; let it do libtomcrypt
	 ((p v (+ mode *ctr-mode:rfc3686*)))))))
  rfc3686-parameter?)

(define (crypto-object? o)
  (or (key? o)
      (is-a? o <legacy-crypto>)))
;; This is why we want to replace lagacy library...
(define (cipher-verify cipher M S . opts)
  (let ((spi (cipher-spi cipher)))
    (or (apply (cipher-spi-verifier spi) M S (cipher-spi-key spi) opts)
	(error 'cipher-verify "Inconsistent"))))
(define (cipher-signature cipher M . opts)
  (let ((spi (cipher-spi cipher)))
    (apply (cipher-spi-signer spi) M (cipher-spi-key spi) opts)))
(define (cipher-decrypt cipher ct :optional (len 0))
  (let ((spi (cipher-spi cipher)))
    (if (legacy-builtin-cipher-spi? spi)
	((cipher-spi-decrypt spi) ct len (cipher-spi-key spi))
	((cipher-spi-decrypt spi) ct (cipher-spi-key spi)))))
(define (cipher-encrypt cipher pt :optional (len 0))
  (let ((spi (cipher-spi cipher)))
    (if (legacy-builtin-cipher-spi? spi)
	((cipher-spi-encrypt spi) pt len (cipher-spi-key spi))
	((cipher-spi-encrypt spi) pt (cipher-spi-key spi)))))
(define (cipher-max-tag-size cipher)
  (let ((spi (cipher-spi cipher)))
    (cipher-spi-tagsize spi)))
(define (cipher-update-aad! cipher aad . opts)
  (let ((spi (cipher-spi cipher)))
    (cond ((cipher-spi-update-aad spi) => (lambda (proc) (apply proc aad opts)))
	  (else #f))))
(define (cipher-iv cipher :optional (iv #f))
  (let ((spi (cipher-spi cipher)))
    (if (bytevector? iv)
	(slot-set! spi 'iv iv)
	(cipher-spi-iv spi))))

(define (cipher-keysize cipher test)
  (let ((spi (cipher-spi cipher)))
    ((cipher-spi-keysize spi) test)))

(define (cipher type key 
		:key (mode *mode:ecb*)
		(iv #f)
		(padder pkcs5-padder)
		(rounds 0)
		(ctr-mode *ctr-mode:big-endian*)
		:allow-other-keys
		:rest rest)
  (define (rfc3686?)
    (not (zero? (bitwise-and ctr-mode *ctr-mode:rfc3686*))))
  (apply make-cipher type key 
	 :mode-parameter
	 (apply make-cipher-parameter
		(filter values
			(list
			 (make-mode-name-parameter mode)
			 (make-padding-parameter padder)
			 (make-round-parameter rounds)
			 (and iv
			      (if (rfc3686?)
				  ;; should we add nonce?
				  (make-rfc3686-parameter iv
							  #vu8(0 0 0 0)
							  ctr-mode)
				  (make-ctr-parameter iv ctr-mode))))))
	 rest))

(define (make-cipher type key
		     :key (mode-parameter #f)
		     :allow-other-keys
		     :rest rest)
  (define parameter mode-parameter)
  ;; kinda silly but for now
  (let ((mode (or (and parameter (parameter-mode-name parameter *mode:ecb*))
		  *mode:ecb*))
	(iv (and parameter (cipher-parameter-iv parameter #f)))
	;; these 2 doesn't have to be there but
	;; make-builtin-cipher-spi requires it.
	;; TODO better construction
	(rounds (or (and parameter (cipher-parameter-rounds parameter 0)) 0))
	(ctr-mode (or (and parameter 
			   (parameter-ctr-mode parameter *ctr-mode:big-endian*))
		      *ctr-mode:big-endian*))
	(padder (or (and parameter (parameter-padder parameter #f))
		    no-padding)))
    (unless (or (eq? mode *mode:ecb*) (bytevector? iv))
      (assertion-violation 'cipher "the given mode iv is required"))
    (let ((spi (cond ((cipher-descriptor? type)
		      (make-builtin-cipher-spi
		       type mode key iv rounds padder ctr-mode))
		     ((lookup-cipher-spi type) =>
		      (lambda (spi)
			(apply make spi key :mode-parameter parameter
			       rest)))
		     ((legacy-cipher-spi? type) type) ;; reuse 
		     (else
		      (assertion-violation 'cipher
					   "unknown cipher type" type)))))
      (make <legacy-cipher> :spi spi))))

(define-constant +check-value+ (make-bytevector 8 0))
(define (key-check-value type key :optional (size 3))
  (unless (<= 3 size 8)
    (assertion-violation 'key-check-value "size must be between 3 to 8"
			 size))
  (let ((c (make-cipher type key)))
    (bytevector-copy (cipher-encrypt c +check-value+) 0 size)))

;; with authentication
(define (cipher-tag! cipher out)
  (let ((spi (cipher-spi cipher)))
    (cond ((cipher-spi-tag spi) => (lambda (proc) (proc out)))
	  (else 0))))
(define (cipher-tag cipher :key (tag #f))
  (let ((out (if tag tag (make-bytevector (cipher-max-tag-size cipher)))))
    (cipher-tag! cipher out)
    out))

(define (cipher-encrypt/tag cipher data
			    :key (tag-size (cipher-max-tag-size cipher)))
  (let ((encrypted (cipher-encrypt cipher data)))
    (values encrypted (cipher-tag cipher :tag (make-bytevector tag-size)))))
(define (cipher-decrypt/tag cipher data :optional (tag #f))
  (let ((pt (cipher-decrypt cipher data)))
    (values pt (cipher-tag cipher :tag tag))))
(define (cipher-decrypt/verify cipher encrypted tag)
  (let-values (((pt target) (cipher-decrypt/tag cipher encrypted)))
    (unless (bytevector=? tag target)
      (error (slot-ref cipher 'name) 'cipher-decrypt/verify
	     "invalid tag is given"))
    pt))

)
