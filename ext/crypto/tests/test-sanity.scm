;; sanity check tests.
;; Basically, just testing self enc/dec. For test with test vectors
;; is written in other locations
#!read-macro=sagittarius/bv-string
(import (rnrs)
	(sagittarius crypto digests)
	(sagittarius crypto mac)
	(sagittarius crypto random)
	(sagittarius crypto keys)
	(sagittarius crypto ciphers)
	(sagittarius crypto math prime)
	(sagittarius crypto signatures)
	(util bytevector)
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
  (test-assert (list (cipher-descriptor-name cipher) "symmetric") (block-cipher-descriptor? cipher))
  (test-assert (list (cipher-descriptor-name cipher) "block") (block-cipher-descriptor? cipher)))
(for-each test-basic all-ciphers)

(test-begin "Key operations")
(define (symmetric-key-operations-test cipher)
  (define prng (secure-random-generator *prng:chacha20*))
  (let ((key (generate-symmetric-key cipher prng)))
    (test-assert (symmetric-key? key))
    ;; suggested key size is in bits, so divide by 8
    (test-equal (cipher-descriptor-name cipher)
		(block-cipher-descriptor-suggested-key-length cipher)
		(bytevector-length (symmetric-key-value key)))))
(for-each symmetric-key-operations-test all-ciphers)
(test-error (generate-symmetric-key *scheme:aes-256* #vu8(0)))

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
      (block-cipher-init! cipher (cipher-direction encrypt) key parameter)
      (let ((r (block-cipher-encrypt-last-block cipher msg)))
	(block-cipher-done! cipher)
	r))
    (define (decrypt cipher msg)
      (block-cipher-init! cipher (cipher-direction decrypt) key parameter)
      (let ((r (block-cipher-decrypt-last-block cipher msg)))
	(block-cipher-done! cipher)
	r))
    (let ((cipher (make-block-cipher cipher-descriptor mode))
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
      (block-cipher-init! cipher (cipher-direction encrypt) key parameter)
      (let ((r (block-cipher-encrypt-last-block cipher msg)))
	(values r (block-cipher-done/tag cipher 16))))
    (define (decrypt cipher msg tag)
      (block-cipher-init! cipher (cipher-direction decrypt) key parameter)
      (let ((r (block-cipher-decrypt-last-block cipher msg)))
	(block-cipher-done/tag! cipher tag)
	r))
    (let ((cipher (make-block-cipher cipher-descriptor mode))
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
    (test-equal (digest-message md #vu8()) (digest-message md #vu8()))
    (let* ((msg (string->utf8 "12345678901234567890"))
	   (hash (digest-message md msg)))
      (message-digest-init! md)
      (do ((i 0 (+ i 1)))
	  ((= i (bytevector-length msg)))
	(message-digest-process! md msg i 1))
      (test-equal "Process each one byte" hash (message-digest-done md)))))

(for-each digest/size-test digests/size)

(define digests-w/o-size
  (list *digest:shake-128* *digest:shake-256*
	*digest:cshake-128* *digest:cshake-256*))
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

(define (test-shake/cshake shake cshake)
  (test-equal (digest-message (make-message-digest shake) #vu8() 32)
	      (digest-message (make-message-digest cshake) #vu8() 32)))
(test-shake/cshake *digest:shake-128* *digest:cshake-128*)
(test-shake/cshake *digest:shake-256* *digest:cshake-256*)

(define (test-cshake cshake N S msg size expected)
  (define md (make-message-digest cshake))
  (message-digest-init! md :name N :custom S)
  (message-digest-process! md msg)
  (let ((digest (message-digest-done md size)))
    (test-equal expected digest)))
;; From https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/cSHAKE_samples.pdf
;; seems they don't have test vectors...
(test-cshake *digest:cshake-128* #*"" #*"Email Signature" #vu8(0 1 2 3)
	     (div 256 8)
	     (hex-string->bytevector "C1C36925B6409A04F1B504FCBCA9D82B4017277CB5ED2B2065FC1D3814D5AAF5"))

(test-cshake *digest:cshake-256* #*"" #*"Email Signature" #vu8(0 1 2 3)
	     (div 512 8)
	     (hex-string->bytevector "D008828E2B80AC9D2218FFEE1D070C48B8E4C87BFF32C9699D5B6896EEE0EDD164020E2BE0560858D9C00C037E34A96937C561A74C412BB4C746469527281C8C"))


(test-end)

(test-begin "MAC")

(define (test-kmac kmac key S size msg* m mx)
  (define (test xof? m)
    (define mac (make-mac kmac key :xof xof? :custom S))
    (define out (make-bytevector size))
    (mac-init! mac)
    (for-each (lambda (msg) (mac-process! mac msg)) msg*)
    (mac-done! mac out)
    (test-equal (if xof? "KMACXOF" "KMAC") m out))
  (test #f m)
  (test #t mx))

;; From
;; - https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/KMAC_samples.pdf
;; - https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/KMACXOF_samples.pdf
;; the same as cSHAKE...
(test-kmac *mac:kmac-128*
	   (hex-string->bytevector "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F")
	   #*""
	   (div 256 8)
	   '(#vu8(0 1 2 3))
	   (hex-string->bytevector "E5780B0D3EA6F7D3A429C5706AA43A00FADBD7D49628839E3187243F456EE14E")
	   (hex-string->bytevector "CD83740BBD92CCC8CF032B1481A0F4460E7CA9DD12B08A0C4031178BACD6EC35"))

(test-kmac *mac:kmac-128*
	   (hex-string->bytevector "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F")
	   #*"My Tagged Application"
	   (div 256 8)
	   '(#vu8(0 1 2 3))
	   (hex-string->bytevector "3B1FBA963CD8B0B59E8C1A6D71888B7143651AF8BA0A7070C0979E2811324AA5")
	   (hex-string->bytevector "31A44527B4ED9F5C6101D11DE6D26F0620AA5C341DEF41299657FE9DF1A3B16C"))

(test-kmac *mac:kmac-128*
	   (hex-string->bytevector "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F")
	   #*"My Tagged Application"
	   (div 256 8)
	   '(#vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F)
	     #vu8(#x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F)
	     #vu8(#x20 #x21 #x22 #x23 #x24 #x25 #x26 #x27 #x28 #x29 #x2A #x2B #x2C #x2D #x2E #x2F)
	     #vu8(#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39 #x3A #x3B #x3C #x3D #x3E #x3F)
	     #vu8(#x40 #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48 #x49 #x4A #x4B #x4C #x4D #x4E #x4F)
	     #vu8(#x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A #x5B #x5C #x5D #x5E #x5F)
	     #vu8(#x60 #x61 #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 #x6A #x6B #x6C #x6D #x6E #x6F)
	     #vu8(#x70 #x71 #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x7B #x7C #x7D #x7E #x7F)
	     #vu8(#x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x8B #x8C #x8D #x8E #x8F)
	     #vu8(#x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9A #x9B #x9C #x9D #x9E #x9F)
	     #vu8(#xA0 #xA1 #xA2 #xA3 #xA4 #xA5 #xA6 #xA7 #xA8 #xA9 #xAA #xAB #xAC #xAD #xAE #xAF)
	     #vu8(#xB0 #xB1 #xB2 #xB3 #xB4 #xB5 #xB6 #xB7 #xB8 #xB9 #xBA #xBB #xBC #xBD #xBE #xBF)
	     #vu8(#xC0 #xC1 #xC2 #xC3 #xC4 #xC5 #xC6 #xC7))
	   (hex-string->bytevector "1F5B4E6CCA02209E0DCB5CA635B89A15E271ECC760071DFD805FAA38F9729230")
	   (hex-string->bytevector "47026C7CD793084AA0283C253EF658490C0DB61438B8326FE9BDDF281B83AE0F"))

;; KMAC 256
(test-kmac *mac:kmac-256*
	   (hex-string->bytevector "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F")
	   #*"My Tagged Application"
	   (div 512 8)
	   '(#vu8(0 1 2 3))
	   (hex-string->bytevector "20C570C31346F703C9AC36C61C03CB64C3970D0CFC787E9B79599D273A68D2F7F69D4CC3DE9D104A351689F27CF6F5951F0103F33F4F24871024D9C27773A8DD")
	   (hex-string->bytevector "1755133F1534752AAD0748F2C706FB5C784512CAB835CD15676B16C0C6647FA96FAA7AF634A0BF8FF6DF39374FA00FAD9A39E322A7C92065A64EB1FB0801EB2B"))

(test-kmac *mac:kmac-256*
	   (hex-string->bytevector "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F")
	   #*""
	   (div 512 8)
	   '(#vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F)
	     #vu8(#x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F)
	     #vu8(#x20 #x21 #x22 #x23 #x24 #x25 #x26 #x27 #x28 #x29 #x2A #x2B #x2C #x2D #x2E #x2F)
	     #vu8(#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39 #x3A #x3B #x3C #x3D #x3E #x3F)
	     #vu8(#x40 #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48 #x49 #x4A #x4B #x4C #x4D #x4E #x4F)
	     #vu8(#x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A #x5B #x5C #x5D #x5E #x5F)
	     #vu8(#x60 #x61 #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 #x6A #x6B #x6C #x6D #x6E #x6F)
	     #vu8(#x70 #x71 #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x7B #x7C #x7D #x7E #x7F)
	     #vu8(#x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x8B #x8C #x8D #x8E #x8F)
	     #vu8(#x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9A #x9B #x9C #x9D #x9E #x9F)
	     #vu8(#xA0 #xA1 #xA2 #xA3 #xA4 #xA5 #xA6 #xA7 #xA8 #xA9 #xAA #xAB #xAC #xAD #xAE #xAF)
	     #vu8(#xB0 #xB1 #xB2 #xB3 #xB4 #xB5 #xB6 #xB7 #xB8 #xB9 #xBA #xBB #xBC #xBD #xBE #xBF)
	     #vu8(#xC0 #xC1 #xC2 #xC3 #xC4 #xC5 #xC6 #xC7))
	   (hex-string->bytevector "75358CF39E41494E949707927CEE0AF20A3FF553904C86B08F21CC414BCFD691589D27CF5E15369CBBFF8B9A4C2EB17800855D0235FF635DA82533EC6B759B69")
	   (hex-string->bytevector "FF7B171F1E8A2B24683EED37830EE797538BA8DC563F6DA1E667391A75EDC02CA633079F81CE12A25F45615EC89972031D18337331D24CEB8F8CA8E6A19FD98B"))

(test-kmac *mac:kmac-256*
	   (hex-string->bytevector "404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F")
	   #*"My Tagged Application"
	   (div 512 8)
	   '(#vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F)
	     #vu8(#x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F)
	     #vu8(#x20 #x21 #x22 #x23 #x24 #x25 #x26 #x27 #x28 #x29 #x2A #x2B #x2C #x2D #x2E #x2F)
	     #vu8(#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39 #x3A #x3B #x3C #x3D #x3E #x3F)
	     #vu8(#x40 #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48 #x49 #x4A #x4B #x4C #x4D #x4E #x4F)
	     #vu8(#x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A #x5B #x5C #x5D #x5E #x5F)
	     #vu8(#x60 #x61 #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 #x6A #x6B #x6C #x6D #x6E #x6F)
	     #vu8(#x70 #x71 #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x7B #x7C #x7D #x7E #x7F)
	     #vu8(#x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x8B #x8C #x8D #x8E #x8F)
	     #vu8(#x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9A #x9B #x9C #x9D #x9E #x9F)
	     #vu8(#xA0 #xA1 #xA2 #xA3 #xA4 #xA5 #xA6 #xA7 #xA8 #xA9 #xAA #xAB #xAC #xAD #xAE #xAF)
	     #vu8(#xB0 #xB1 #xB2 #xB3 #xB4 #xB5 #xB6 #xB7 #xB8 #xB9 #xBA #xBB #xBC #xBD #xBE #xBF)
	     #vu8(#xC0 #xC1 #xC2 #xC3 #xC4 #xC5 #xC6 #xC7))
	   (hex-string->bytevector "B58618F71F92E1D56C1B8C55DDD7CD188B97B4CA4D99831EB2699A837DA2E4D970FBACFDE50033AEA585F1A2708510C32D07880801BD182898FE476876FC8965")
	   (hex-string->bytevector "D5BE731C954ED7732846BB59DBE3A8E30F83E77A4BFF4459F2F1C2B4ECEBB8CE67BA01C62E8AB8578D2D499BD1BB276768781190020A306A97DE281DCC30305D"))

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

(let ((k-generator (make-hmac-k-generator *digest:sha-256*)))
  (test-equal #x23AF4074C90A02B3FE61D286D5C87F425E6BDD81B
	      (k-generator #x4000000000000000000020108A2E0CC0D99F8A5EF
			   #x09A4D6792295A7F730FC3F2B49CBC0F62E862272F
			   (hex-string->bytevector "AF2BDBE1AA9B6EC1E2ADE1D694F41FC71A831D0268E9891562113D8A62ADD1BF"))))

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
    (let ((r (verifier-verify-signature verifier msg
					(signer-sign-message signer msg))))
    (test-assert (list scheme param) r))))
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
(define-syntax curve-list
  (syntax-rules ()
    ((_) '())
    ((_ name names* ...)
     (cons (list 'name name)
	   (curve-list names* ...)))))
;; Note NIST curves are commented out as SEC has the equivalent ones
;; e.g. *ec-parameter:p192* = *ec-parameter:secp192r1*
(define all-curves (curve-list ;; *ec-parameter:p192*
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

(define (test-ec-parameters name&ec-parameter)
  (define ec-parameter (cadr name&ec-parameter))
  (define kp (generate-key-pair *key:ecdsa* :ec-parameter ec-parameter))
  (define msg (string->utf8 "keep my integrity"))
  (let ((signer (make-signer *signature:ecdsa* (key-pair-private kp)
			     :digest *digest:sha-224* :der-encode #f))
	(verifier (make-verifier *signature:ecdsa* (key-pair-public kp)
				 :digest *digest:sha-224* :der-encode #f)))
    (test-equal ec-parameter (ecdsa-key-parameter (key-pair-public kp)))
    (test-equal ec-parameter (ecdsa-key-parameter (key-pair-private kp)))
    (let ((sig (signer-sign-message signer msg)))
      (test-assert (car name&ec-parameter)
		   (verifier-verify-signature verifier msg sig)))))
(for-each test-ec-parameters all-curves)

(test-end)

