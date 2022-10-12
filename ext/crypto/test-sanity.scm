;; sanity check tests.
;; Basically, just testing self enc/dec. For test with test vectors
;; is written in other locations
(import (rnrs)
	(sagittarius crypto digests)
	(sagittarius crypto random)
	(sagittarius crypto keys)
	(sagittarius crypto ciphers)
	(sagittarius crypto math prime)
	(sagittarius crypto signatures)
	(srfi :64))

(define all-prngs-w/o-system
  (list *prng:yarrow* *prng:fortuna* *prng:rc4* *prng:sober-128*
	*prng:chacha20*))
(define all-prngs
  (cons *prng:system* all-prngs-w/o-system))

(define (prng-descriptor-test prng)
  (test-assert (prng-descriptor? prng))
  (test-assert (string? (prng-descriptor-name prng))))
(define (prng-test prng)
  (define name (prng-descriptor-name prng))
  (let ((rg0 (pseudo-random-generator prng))
	(rg1 (pseudo-random-generator prng)))
    (test-equal (string-append "Pseudo: " name)
		(random-generator-read-random-bytes rg0 20)
		(random-generator-read-random-bytes rg1 20))
    (test-assert (random-generator?
		  (random-generator-randomize! rg0 #vu8(1 2))))
    (test-assert (random-generator?
		  (random-generator-randomize! rg1 #vu8(1 2))))
    (test-equal (string-append "Pseudo: " name " same seed")
		(random-generator-read-random-bytes rg0 20)
		(random-generator-read-random-bytes rg1 20)))
  (let ((srg0 (secure-random-generator prng))
	(srg1 (secure-random-generator prng)))
    (test-assert (string-append "Secure " name)
		 (not (equal? (random-generator-read-random-bytes srg0 20)
			      (random-generator-read-random-bytes srg1 20))))
    (test-assert (random-generator?
		  (random-generator-randomize! srg0 #vu8(1 2))))
    (test-assert (random-generator?
		  (random-generator-randomize! srg1 #vu8(1 2))))
    (test-assert (string-append "Secure: " name " same seed")
		 (not (equal? (random-generator-read-random-bytes srg0 20)
			      (random-generator-read-random-bytes srg1 20))))))

(test-begin "Pseudo random generator")
(for-each prng-descriptor-test all-prngs)
(for-each prng-test all-prngs-w/o-system)
(test-end)

(define all-ciphers
  (list *scheme:blowfish*
	*scheme:x-tea*
	*scheme:rc2* *scheme:rc5* *scheme:rc6*
	*scheme:safer+* *scheme:safer-k64* *scheme:safer-sk64*
	*scheme:safer-k128* *scheme:safer-sk128*
	*scheme:aes* *scheme:aes-128* *scheme:aes-192* *scheme:aes-256*
	*scheme:twofish*
	*scheme:des* *scheme:des3* *scheme:desede*
	*scheme:cast5* *scheme:cast-128*
	*scheme:noekeon*
	*scheme:skipjack*
	*scheme:khazad*
	*scheme:seed*
	*scheme:kasumi*
	*scheme:camellia*))
(define (test-basic cipher)
  (test-assert (list (cipher-descriptor-name cipher) "base") (cipher-descriptor? cipher))
  (test-assert (list (cipher-descriptor-name cipher) "symmetric") (symmetric-cipher-descriptor? cipher))
  (test-assert (list (cipher-descriptor-name cipher) "block") (block-cipher-descriptor? cipher)))
(for-each test-basic all-ciphers)

(test-begin "Key operations")
(define (symmetric-key-operations-test cipher)
  (define prng (secure-random-generator *prng:chacha20*))
  (let ((key (generate-symmetric-key cipher prng)))
    (test-assert (symmetric-key? key))
    ;; suggested key size is in bits, so divide by 8
    (test-equal (cipher-descriptor-name cipher)
		(block-cipher-descriptor-suggested-keysize cipher)
		(bytevector-length (symmetric-key-value key)))))
(for-each symmetric-key-operations-test all-ciphers)

;; key wrap
(define (key-wrap-test scheme)
  (define key (generate-symmetric-key scheme))
  (let ((wrap (make-rfc3394-key-wrap scheme key))
	(unwrap (make-rfc3394-key-unwrap scheme key))
	(pt (make-bytevector 16 5))) ;; 2 blocks
    (test-equal (string-append "Key wrap with " (cipher-descriptor-name scheme))
		pt (unwrap (wrap pt)))))
(for-each key-wrap-test all-ciphers)

(define (asymmetric-key-operations-test op)
  (define (test-public-key-export op key)
    (define spki (public-key-format subject-public-key-info))
    (let ((raw-encoded (export-public-key op key))
	  (spki-encoded (export-public-key op key spki)))
      (test-assert op (bytevector? raw-encoded))
      (test-assert op (bytevector? spki-encoded))
      (test-assert op (public-key? (import-public-key op raw-encoded)))
      (test-assert op (public-key? (import-public-key op spki-encoded spki)))))
  (define (test-private-key-export op key)
    (let ((encoded (export-private-key op key)))
      (test-assert op (bytevector? encoded))
      (test-assert op (private-key? (import-private-key op encoded)))))
  (define prng (secure-random-generator *prng:chacha20*))
  (let ((kp (generate-key-pair op :prng prng)))
    (test-assert op (key-pair? kp))
    (test-assert op (private-key? (key-pair-private kp)))
    (test-assert op (public-key? (key-pair-public kp)))
    (test-public-key-export op (key-pair-public kp))
    (test-private-key-export op (key-pair-private kp))))
(for-each asymmetric-key-operations-test
	  (list *key:rsa* *key:dsa* *key:ecdsa*
		*key:ed25519* *key:ed448*
		*key:x25519* *key:x448*))
(test-end)

(test-begin "Symmetric ciphers")

(define (make-mode-test mode parameter-provider)
  (lambda (cipher-descriptor)
    (define prng (pseudo-random-generator *prng:chacha20*))
    (define key (generate-symmetric-key cipher-descriptor prng))
    (define parameter (parameter-provider cipher-descriptor))
    (define (encrypt cipher msg)
      (symmetric-cipher-init! cipher (cipher-direction encrypt) key parameter)
      (let ((r (symmetric-cipher-encrypt-last-block cipher msg)))
	(symmetric-cipher-done! cipher)
	r))
    (define (decrypt cipher msg)
      (symmetric-cipher-init! cipher (cipher-direction decrypt) key parameter)
      (let ((r (symmetric-cipher-decrypt-last-block cipher msg)))
	(symmetric-cipher-done! cipher)
	r))
    (let ((cipher (make-symmetric-cipher cipher-descriptor mode))
	  (msg (string->utf8 "this is a message to be encrypted and decrypted")))
      (test-assert "cipher?" (cipher? cipher))
      (test-assert "symmetric-cipher?" (symmetric-cipher? cipher))
      #;(let ((ct (encrypt cipher msg)))
	(print ct)
	(print (cipher-descriptor-name cipher-descriptor) ":"
	       (mode-descriptor-name mode) ":"
	       (utf8->string (decrypt cipher ct))))
      (let ((pt (decrypt cipher (encrypt cipher msg))))
	(test-assert
	 (string-append (cipher-descriptor-name cipher-descriptor) ":"
			(mode-descriptor-name mode))
	 (equal? msg pt))))))

(define ecb-test (make-mode-test *mode:ecb* (lambda (_) #f)))
(define cbc-test
  (make-mode-test *mode:cbc*
   (lambda (cipher)
     (make-iv-parameter
      (make-bytevector (block-cipher-descriptor-block-length cipher) 1)))))
(define ctr-test
  (make-mode-test *mode:ctr*
   (lambda (cipher)
     (make-cipher-parameter
      (make-iv-parameter
       (make-bytevector (block-cipher-descriptor-block-length cipher) 1))
      (make-counter-mode-parameter *ctr-mode:rfc3686*)))))
(define cfb-test
  (make-mode-test *mode:cfb*
   (lambda (cipher)
     (make-iv-parameter
      (make-bytevector (block-cipher-descriptor-block-length cipher) 1)))))
(define ofb-test
  (make-mode-test *mode:cfb*
   (lambda (cipher)
     (make-iv-parameter
      (make-bytevector (block-cipher-descriptor-block-length cipher) 1)))))
(define lrw-test
  (make-mode-test *mode:lrw*
   (lambda (cipher)
     (make-cipher-parameter
      (make-iv-parameter
       (make-bytevector (block-cipher-descriptor-block-length cipher) 1))
      (make-tweak-parameter (make-bytevector 16))))))
(define f8-test
  (make-mode-test *mode:f8*
   (lambda (cipher)
     (make-cipher-parameter
      (make-iv-parameter
       (make-bytevector (block-cipher-descriptor-block-length cipher) 1))
      (make-salt-parameter (make-bytevector 8))))))

(define (make-encauth-mode-test mode parameter-provider)
  (lambda (cipher-descriptor)
    (define prng (pseudo-random-generator *prng:chacha20*))
    (define key (generate-symmetric-key cipher-descriptor prng))
    (define parameter (parameter-provider cipher-descriptor))
    (define (encrypt cipher msg)
      (symmetric-cipher-init! cipher (cipher-direction encrypt) key parameter)
      (let ((r (symmetric-cipher-encrypt-last-block cipher msg)))
	(values r (symmetric-cipher-done/tag cipher 16))))
    (define (decrypt cipher msg tag)
      (symmetric-cipher-init! cipher (cipher-direction decrypt) key parameter)
      (let ((r (symmetric-cipher-decrypt-last-block cipher msg)))
	(symmetric-cipher-done/tag! cipher tag)
	r))
    (let ((cipher (make-symmetric-cipher cipher-descriptor mode))
	  (msg (string->utf8
		"this is a message to be encrypted and decrypted")))
      (test-assert "cipher?" (cipher? cipher))
      (test-assert "symmetric-cipher?" (symmetric-cipher? cipher))
      
      (let-values (((ct tag) (encrypt cipher msg)))
	#;(print (cipher-descriptor-name cipher-descriptor) ":"
	       (mode-descriptor-name mode) ":"
	       tag)
	;; (print tag (utf8->string (decrypt cipher ct tag)))
	(test-assert
	 (string-append (cipher-descriptor-name cipher-descriptor) ":"
			(mode-descriptor-name mode) " EncAuth")
	 (equal? msg (decrypt cipher ct tag)))
	(test-error
	 (string-append (cipher-descriptor-name cipher-descriptor) ":"
			(mode-descriptor-name mode) " EncAuth invalid tag")
	 (equal? msg (decrypt cipher ct #vu8(1 2 3 4 5))))))))

(define eax-test (make-mode-test *mode:eax* (lambda (cipher) #f)))
(define eax-enc-test
  (make-encauth-mode-test *mode:eax*
			  (lambda (cipher)
			    (make-cipher-parameter
			     (make-nonce-parameter #vu8(1 2 3 4))
			     (make-aad-parameter #vu8(5 6 7 8))))))
(define ocb-enc-test
  (make-encauth-mode-test *mode:ocb*
   (lambda (cipher)
     (make-nonce-parameter 
      (make-bytevector (block-cipher-descriptor-block-length cipher))))))

(define ocb3-test
  (make-mode-test *mode:ocb3*
   (lambda (cipher)
     (make-cipher-parameter
      (make-tag-length-parameter 16)
      (make-nonce-parameter #vu8(1 2 3 4 5))))))
(define ocb3-enc-test
  (make-encauth-mode-test *mode:ocb3*
   (lambda (cipher) 
     (make-cipher-parameter
      (make-tag-length-parameter 16)
      (make-nonce-parameter #vu8(1 2 3 4 5))))))

(define gcm-test
  (make-mode-test
   *mode:gcm*
   (lambda (cipher)
     (make-iv-parameter 
      (make-bytevector (block-cipher-descriptor-block-length cipher))))))
(define gcm-enc-test
  (make-encauth-mode-test *mode:gcm*
   (lambda (cipher)
     (make-iv-parameter 
      (make-bytevector (block-cipher-descriptor-block-length cipher))))))

(define (cipher-test cipher)
  (ecb-test cipher)
  (cbc-test cipher)
  (cfb-test cipher)
  (ofb-test cipher)
  (ctr-test cipher)
  ;; lrw requires block length of 16
  (when (= (block-cipher-descriptor-block-length cipher) 16) (lrw-test cipher))
  (f8-test cipher)
  (eax-test cipher)
  (eax-enc-test cipher)
  (ocb-enc-test cipher) ;; OCB can't be used without tag operation
  ;; OCB3 requires block length of 16
  (when (= (block-cipher-descriptor-block-length cipher) 16)
    (ocb3-test cipher)
    (ocb3-enc-test cipher))
  ;; gcm requires block length of 16
  (when (= (block-cipher-descriptor-block-length cipher) 16)
    (gcm-test cipher)
    (gcm-enc-test cipher)))

(for-each cipher-test all-ciphers)
(test-end)

(test-begin "Asymmetric ciphers")

(define all-asymmetric-ciphers (list *scheme:rsa*))
(define (test-asymmetric-cipher scheme)
  (define kp (generate-key-pair scheme))
  (define msg (string->utf8 "hello message"))
  (define (test-it encoding)
    (let ((cipher (make-asymmetric-cipher scheme :encoding encoding)))
      (asymmetric-cipher-init! cipher (key-pair-public kp))
      (let ((ct (asymmetric-cipher-encrypt-bytevector cipher msg)))
	(asymmetric-cipher-init! cipher (key-pair-private kp))
	(test-equal msg (asymmetric-cipher-decrypt-bytevector cipher ct)))))
  (test-it oaep-encoding)
  (test-it pkcs1-v1.5-encoding))

(for-each test-asymmetric-cipher all-asymmetric-ciphers)
(test-end)

(test-begin "Digest")

(define digests/size
  (list *digest:whirlpool*
	*digest:ripemd-320*
	*digest:ripemd-256*
	*digest:sha-1*
	*digest:sha-224* *digest:sha-256*
	*digest:sha-384*
	*digest:sha-512* *digest:sha-512/224* *digest:sha-512/256*
	*digest:sha3-224* *digest:sha3-256* *digest:sha3-384*
	*digest:sha3-512*
	*digest:keccak-224* *digest:keccak-256* *digest:keccak-384*
	*digest:keccak-512*
	*digest:tiger-192*
	*digest:ripemd-160* *digest:ripemd-128*
	*digest:md5* *digest:md4* *digest:md2*
	*digest:blake2s-128* *digest:blake2s-160* *digest:blake2s-224*
	*digest:blake2s-256* *digest:blake2b-160* *digest:blake2b-256*
	*digest:blake2b-384* *digest:blake2b-512*))

(define (digest/size-test desc)
  (test-assert (digest-descriptor? desc))
  (let ((size (digest-descriptor-digest-size desc))
	(md (make-message-digest desc)))
    (test-assert (message-digest? md))
    (test-equal (digest-descriptor-name desc)
		size (bytevector-length (digest-message md #vu8())))
    (test-equal (digest-message md #vu8()) (digest-message md #vu8()))))

(for-each digest/size-test digests/size)

(define digests-w/o-size (list *digest:shake-128* *digest:shake-256*))
(define (digest-w/o-size-test desc)
  (test-assert (digest-descriptor? desc))
  (let ((size (digest-descriptor-digest-size desc))
	(md (make-message-digest desc)))
    (test-assert "No size" (not size))
    (test-assert (message-digest? md))
    ;; just use random number to retrieve digest
    (test-equal (digest-descriptor-name desc)
		64 (bytevector-length (digest-message md #vu8() 64)))))
(for-each digest-w/o-size-test digests-w/o-size)

(test-end)

(test-begin "Prime")

(define *small-primes*
  '(3 5 7 11 13 17 19 23 29 31 37 41 43 
    47 53 59 61 67 71 73 79 83 89 97 101 
    103 107 109 113
    127 131 137 139 149 151 157 163 167 173
    179 181 191 193 197 199 211 223 227 229
    233 239 241 251 257 263 269 271 277 281
    283 293 307 311 313 317 331 337 347 349
    353 359 367 373 379 383 389 397 401 409
    419 421 431 433 439 443 449 457 461 463
    467 479 487 491 499 503 509 521 523 541
    547 557 563 569 571 577 587 593 599 601
    607 613 617 619 631 641 643 647 653 659
    661 673 677 683 691 701 709 719 727 733
    739 743 751 757 761 769 773 787 797 809
    811 821 823 827 829 839 853 857 859 863
    877 881 883 887 907 911 919 929 937 941
    947 953 967 971 977 983 991 997))

(do ((i 0 (+ i 1)))
    ((= i 100))
  ;; 1 byte prime
  (test-assert (memv (generate-random-prime 1) *small-primes*)))

(define *pseudo-primes*
  '(341 561 645 1105 1387 1729 1905 2047 2465 2701 
    2821 3277 4033 4369 4371 4681 5461 6601 7957 8321 
    8481 8911))

(test-assert "probable-prime?"
	     (not (exists probable-prime? *pseudo-primes*)))
(test-assert (probable-prime? 359334085968622831041960188598043661065388726959079837))

(test-end)

(test-begin "Signature")

;; basically *signature:...* and *key:...* are the same
;; though that's the prespective of library implementator, me obviously,
;; and unless it's not written in the test, it's subjected to change
;; (also write document as well, but that comes later)
(define all-signature-scheme
  (list
   ;; RSA, some are the same value
   *signature:rsa* *scheme:rsa* *key:rsa*
   ;; DSA
   *signature:dsa* *key:dsa*
   ;; ECDSA
   *signature:ecdsa* *key:ecdsa*
   ;; EdDSA
   *signature:ed25519* *signature:ed25519ctx* *signature:ed25519ph*
   *key:ed25519*
   *signature:ed448* *signature:ed448ph* *key:ed448*
   )
  )
(define ((test-signer/verifier param) scheme)
  (define kp (generate-key-pair scheme))
  (define msg (string->utf8 "keep my integrity"))
  (let ((signer (apply make-signer scheme (key-pair-private kp) param))
	(verifier (apply make-verifier scheme (key-pair-public kp) param)))
    (test-assert scheme (verifier-verify-signature verifier msg
			   (signer-sign-message signer msg)))))
(define parameter1
  (list :encoder pkcs1-emsa-pss-encode
	:verifier pkcs1-emsa-pss-verify
	:der-encode #t))
(define parameter2
  (list :encoder pkcs1-emsa-v1.5-encode
	:verifier pkcs1-emsa-v1.5-verify
	:der-encode #f
	:context (string->utf8 "This is EdDSA context")))
(for-each (test-signer/verifier parameter1) all-signature-scheme)
(for-each (test-signer/verifier parameter2) all-signature-scheme)

;; ECDSA curve tests
;; Note NIST curves are commented out as SEC has the equivalent ones
;; e.g. *ec-parameter:p192* = *ec-parameter:secp192r1*
(define all-curves (list ;; *ec-parameter:p192*
			 ;; *ec-parameter:p224*
			 ;; *ec-parameter:p256*
			 ;; *ec-parameter:p384*
			 ;; *ec-parameter:p521*
			 ;; *ec-parameter:k163*
			 ;; *ec-parameter:k233*
			 ;; *ec-parameter:k283*
			 ;; *ec-parameter:k409*
			 ;; *ec-parameter:k571*
			 ;; *ec-parameter:b163*
			 ;; *ec-parameter:b233*
			 ;; *ec-parameter:b283*
			 ;; *ec-parameter:b409*
			 ;; *ec-parameter:b571*
			 *ec-parameter:secp192r1*
			 *ec-parameter:secp224r1*
			 *ec-parameter:secp256r1*
			 *ec-parameter:secp384r1*
			 *ec-parameter:secp521r1*
			 *ec-parameter:sect163k1*
			 *ec-parameter:sect233k1*
			 *ec-parameter:sect283k1*
			 *ec-parameter:sect409k1*
			 *ec-parameter:sect571k1*
			 *ec-parameter:sect163r2*
			 *ec-parameter:sect233r1*
			 *ec-parameter:sect283r1*
			 *ec-parameter:sect409r1*
			 *ec-parameter:sect571r1*
			 *ec-parameter:secp192k1*
			 *ec-parameter:secp224k1*
			 *ec-parameter:secp256k1*
			 *ec-parameter:sect163r1*
			 *ec-parameter:sect239k1*
			 *ec-parameter:sect113r1*
			 *ec-parameter:brainpool-p160r1*
			 *ec-parameter:brainpool-p160t1*
			 *ec-parameter:brainpool-p192r1*
			 *ec-parameter:brainpool-p192t1*
			 *ec-parameter:brainpool-p224r1*
			 *ec-parameter:brainpool-p224t1*
			 *ec-parameter:brainpool-p256r1*
			 *ec-parameter:brainpool-p256t1*
			 *ec-parameter:brainpool-p320r1*
			 *ec-parameter:brainpool-p320t1*
			 *ec-parameter:brainpool-p384r1*
			 *ec-parameter:brainpool-p384t1*
			 *ec-parameter:brainpool-p512r1*
			 *ec-parameter:brainpool-p512t1*
			 ))

(define (test-ec-parameters ec-parameter)
  (define kp (generate-key-pair *key:ecdsa* :ec-parameter ec-parameter))
  (define msg (string->utf8 "keep my integrity"))
  (let ((signer (make-signer *signature:ecdsa* (key-pair-private kp)))
	(verifier (make-verifier *signature:ecdsa* (key-pair-public kp))))
    (test-equal ec-parameter (ecdsa-key-parameter (key-pair-public kp)))
    (test-equal ec-parameter (ecdsa-key-parameter (key-pair-private kp)))
    (test-assert (verifier-verify-signature verifier msg
					    (signer-sign-message signer msg)))))
(for-each test-ec-parameters all-curves)

(test-end)
