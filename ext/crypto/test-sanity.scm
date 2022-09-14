;; sanity check tests.
;; Basically, just testing self enc/dec. For test with test vectors
;; is written in other locations
(import (rnrs)
	(crypto)
	(sagittarius crypto secure)
	(sagittarius crypto random)
	(sagittarius crypto keys)
	(sagittarius crypto ciphers)
	;; (sagittarius crypto descriptors)
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

;; (define (check cipher)
;;   (define prng (secure-random-generator *prng:chacha20*))
;;   (define key (random-generator-read-random-bytes
;; 	       prng (cipher-descriptor-suggested-keysize cipher)))
;;   (define len (* (cipher-descriptor-block-length cipher) 2))
;;   (define msg (make-bytevector len 1))
;;   (let ((ecb-enc (mode-start *mode:ecb* cipher key #f))
;; 	(ecb-dec (mode-start *mode:ecb* cipher key #f))
;; 	(ct (make-bytevector len))
;; 	(pt (make-bytevector len)))
;;     (mode-encrypt! ecb-enc msg 0 ct 0 len)
;;     (mode-decrypt! ecb-dec ct 0 pt 0  len)
;;     (test-assert (equal? msg pt))))
;; (for-each check all-ciphers)

(test-begin "Key operations")
(define (symmetric-key-operations-test cipher)
  (define prng (secure-random-generator *prng:chacha20*))
  (let ((key (generate-symmetric-key cipher prng)))
    (test-assert (symmetric-key? key))
    ;; suggested key size is in bits, so divide by 8
    (test-equal (cipher-descriptor-name cipher)
		(cipher-descriptor-suggested-keysize cipher)
		(bytevector-length (symmetric-key-value key)))))
(for-each symmetric-key-operations-test all-ciphers)
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
      (make-bytevector (cipher-descriptor-block-length cipher) 1)))))
(define ctr-test
  (make-mode-test *mode:ctr*
   (lambda (cipher)
     (make-cipher-parameter
      (make-iv-parameter
       (make-bytevector (cipher-descriptor-block-length cipher) 1))
      (make-counter-mode-parameter *ctr-mode:rfc3686*)))))
(define cfb-test
  (make-mode-test *mode:cfb*
   (lambda (cipher)
     (make-iv-parameter
      (make-bytevector (cipher-descriptor-block-length cipher) 1)))))
(define ofb-test
  (make-mode-test *mode:cfb*
   (lambda (cipher)
     (make-iv-parameter
      (make-bytevector (cipher-descriptor-block-length cipher) 1)))))
(define lrw-test
  (make-mode-test *mode:lrw*
   (lambda (cipher)
     (make-cipher-parameter
      (make-iv-parameter
       (make-bytevector (cipher-descriptor-block-length cipher) 1))
      (make-tweak-parameter (make-bytevector 16))))))
(define f8-test
  (make-mode-test *mode:f8*
   (lambda (cipher)
     (make-cipher-parameter
      (make-iv-parameter
       (make-bytevector (cipher-descriptor-block-length cipher) 1))
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
      (make-bytevector (cipher-descriptor-block-length cipher))))))

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
      (make-bytevector (cipher-descriptor-block-length cipher))))))
(define gcm-enc-test
  (make-encauth-mode-test *mode:gcm*
   (lambda (cipher)
     (make-iv-parameter 
      (make-bytevector (cipher-descriptor-block-length cipher))))))

(define (cipher-test cipher)
  (ecb-test cipher)
  (cbc-test cipher)
  (cfb-test cipher)
  (ofb-test cipher)
  (ctr-test cipher)
  ;; lrw requires block length of 16
  (when (= (cipher-descriptor-block-length cipher) 16) (lrw-test cipher))
  (f8-test cipher)
  (eax-test cipher)
  (eax-enc-test cipher)
  (ocb-enc-test cipher) ;; OCB can't be used without tag operation
  ;; OCB3 requires block length of 16
  (when (= (cipher-descriptor-block-length cipher) 16)
    (ocb3-test cipher)
    (ocb3-enc-test cipher))
  ;; gcm requires block length of 16
  (when (= (cipher-descriptor-block-length cipher) 16)
    (gcm-test cipher)
    (gcm-enc-test cipher)))

(for-each cipher-test all-ciphers)
(test-end)

