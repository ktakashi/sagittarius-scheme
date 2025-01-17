(import (rnrs)
	(sagittarius)
	(sagittarius crypto ciphers types) ;; for cipher-scheme...
	(sagittarius crypto ciphers)
	(sagittarius crypto keys)
	(srfi :64))

(define (test-chacha20-poly1305 name :key algorithm key-size tag-size tests)
  (define chacha20-scheme
    (cond ((string=? algorithm "CHACHA20-POLY1305") *scheme:chacha20-poly1305*)
	  ((string=? algorithm "XCHACHA20-POLY1305") *scheme:xchacha20-poly1305*)
	  (else (assertion-violation 'chacha20-poly1305-test
				     "Unknown algorithm" algorithm))))
  (define (cipher-name cipher) (cipher-descriptor-name (cipher-scheme cipher)))
  (define (setup-cipher cipher key iv aad direction)
    (stream-cipher-init! cipher direction key (make-iv-parameter iv))
    (stream-cipher-update-aad! cipher aad))
  (define (test-encrypt id cipher key iv aad msg ct tag)
    (setup-cipher cipher key iv aad (cipher-direction encrypt))
    (let ((v (stream-cipher-encrypt cipher msg)))
      (let ((tag1 (stream-cipher-done/tag cipher (bytevector-length tag))))
	(test-equal `(,(cipher-name cipher) "encrypt tag" ,id) tag tag1))
      (test-equal `(,(cipher-name cipher) "encrypt" ,id) ct v)))
  (define (test-decrypt id cipher key iv aad msg ct tag)
    (setup-cipher cipher key iv aad (cipher-direction decrypt))
    (let ((v (stream-cipher-decrypt cipher ct)))
      (stream-cipher-done/tag! cipher tag)
      (test-equal `(,(cipher-name cipher) "decrypt" ,id) msg v)))
  (define (run-test id cipher key iv aad msg ct tag)
    (test-encrypt id cipher key iv aad msg ct tag)
    (test-decrypt id cipher key iv aad msg ct tag))
  (define (check-chacha20-poly1305 test)
    (let ((id (vector-ref test 0))
	  (comment (vector-ref test 1))
	  (key (vector-ref test 2))
	  (iv (vector-ref test 3))
	  (aad (vector-ref test 4))
	  (msg (vector-ref test 5))
	  (ct (vector-ref test 6))
	  (tag (vector-ref test 7))
	  (result (vector-ref test 8)))
      (let ((cipher (make-stream-cipher chacha20-scheme))
	    (skey (make-symmetric-key key)))
	(if result
	    (run-test id cipher skey iv aad msg ct tag)
	    (test-error `("Error:" ,(cipher-name cipher) ,id ,comment) 
	     serious-condition?
	     ;; decrypt will raise an error instead of crush it...
	     (test-decrypt id cipher skey iv aad msg ct tag))))))
  (for-each check-chacha20-poly1305 tests))


(test-begin "ChaCha20-Poly1305 test vectors")
(include "./testvectors/chacha20-poly1305.scm")
(test-end)

(import (util bytevector))
(define (test-aes-aead name :key algorithm key-size tag-size tests)
  (define mode (cond ((string=? algorithm "AES-GCM-SIV") *mode:gcm-siv*)
		     ((string=? algorithm "AES-CCM")     *mode:ccm*)
		     (else (assertion-violation
			    'test-aes-aead "Unknown AEAD mode" algorithm))))
  (define (test-decrypt id cipher key parameter msg ct tag)
    (block-cipher-init! cipher (cipher-direction decrypt) key parameter)
    (test-equal `(,id "Partial decryption size 0 (no buffer needed)")
		0
		(block-cipher-decrypt! cipher ct 0 #vu8() 0))
    (let* ((size (block-cipher-last-block-size cipher 0))
	   (dec (make-bytevector size))
	   (dec-size (block-cipher-decrypt-last-block! cipher #vu8() 0 dec 0)))
      (test-equal `(,id "size (decrypt)") size dec-size)
      (test-equal `(,id "decrypt") msg dec)
      (test-assert `(,id "Tag (decrypt)") (block-cipher-done/tag! cipher tag))))
  (define (run-test id cipher key parameter msg ct tag)
    (block-cipher-init! cipher (cipher-direction encrypt) key parameter)
    (test-equal `(,id "Partial encryption size 0 (no buffer needed)")
		0
		(block-cipher-encrypt! cipher msg 0 #vu8() 0))
    (let* ((size (block-cipher-last-block-size cipher 0))
	   (enc (make-bytevector size)))
      (test-equal `(,id "size (encrypt)")
		  size (block-cipher-encrypt-last-block! cipher #vu8() 0 enc 0))
      (test-equal `(,id "encrypt") ct enc)
      (let ((this-tag (block-cipher-done/tag cipher (div tag-size 8))))
	(test-equal `(,id "Tag") tag this-tag)
	(let ((new-param (make-cipher-parameter parameter
						(make-tag-parameter this-tag))))
	  (test-decrypt `(,id 1) cipher key new-param msg ct this-tag)
	  (test-decrypt `(,id 2) cipher key new-param msg enc this-tag)))))
  (define (invalid-case id cipher key parameter msg ct tag)
    (block-cipher-init! cipher (cipher-direction encrypt) key parameter)
    (block-cipher-encrypt! cipher msg 0 #vu8() 0)
    (let* ((size (block-cipher-last-block-size cipher 0))
	   (enc (make-bytevector size)))
      (block-cipher-encrypt-last-block! cipher #vu8() 0 enc 0)
      (let ((this-tag (block-cipher-done/tag cipher (div tag-size 8))))
	(test-assert `(,algorithm ,id "Tag") (not (equal? tag this-tag)))
	(let ((new-param (make-cipher-parameter parameter
						(make-tag-parameter this-tag))))
	  (block-cipher-init! cipher (cipher-direction decrypt) key parameter)
	  (block-cipher-decrypt! cipher ct 0 #vu8() 0)
	  (let* ((size (block-cipher-last-block-size cipher 0))
		 (dec (make-bytevector size)))
	    (block-cipher-decrypt-last-block! cipher #vu8() 0 dec 0)
	    (test-error (block-cipher-done/tag! cipher tag)))))))
  (define ((check-aead mode) test)
    (let ((id (vector-ref test 0))
	  (comment (vector-ref test 1))
	  (key (vector-ref test 2))
	  (iv (vector-ref test 3))
	  (aad (vector-ref test 4))
	  (msg (vector-ref test 5))
	  (ct (vector-ref test 6))
	  (tag (vector-ref test 7))
	  (result (vector-ref test 8)))
      (let ((cipher (make-block-cipher *scheme:aes* mode no-padding))
	    (skey (make-symmetric-key key))
	    (parameter (make-cipher-parameter
			(make-iv-parameter iv)
			(make-aad-parameter aad)
			;; For CCM
			(make-tag-length-parameter (div tag-size 8))))
	    (test-id `(,algorithm ,id ,comment)))
	(if result
	    (run-test test-id cipher skey parameter msg ct tag)
	    (guard (e (else (test-assert test-id #t)))
	      (invalid-case test-id cipher skey parameter msg ct tag))))))
  (for-each (check-aead mode) tests))

(test-begin "AES AEAD test vectors")
(include "./testvectors/aes-aead.scm")
(test-end)
