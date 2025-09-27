(import (rnrs)
        (sagittarius crypto ciphers)
        (sagittarius crypto keys))

;; Randomly generates AES-256 secret key.
(define key (generate-symmetric-key *scheme:aes-256*))

;; Initial Vector (use randomly generated one in production code)
(define iv
 (make-bytevector (block-cipher-descriptor-block-length *scheme:aes-256*) 1))

;; AES-256 cipher with CTR mode, without padding 
(define aes-cipher 
 (make-block-cipher *scheme:aes-256* *mode:ctr* no-padding))

;; Cipher parameter.
;; NOTE, IV will be copied so modifying original IV doesn't affect the result
(define parameter
  (make-cipher-parameter
   (make-iv-parameter iv)
   (make-counter-mode-parameter *ctr-mode:big-endian*)))

(define (encrypt cipher key parameter plain-text)
  (block-cipher-init! cipher (cipher-direction encrypt) key parameter)
  (let ((cipher-text (block-cipher-encrypt-last-block cipher plain-text)))
    (block-cipher-done! cipher)
    cipher-text))

(define (decrypt cipher key parameter cipher-text)
  (block-cipher-init! cipher (cipher-direction decrypt) key parameter)
  (let ((plain-text (block-cipher-decrypt-last-block cipher cipher-text)))
    (block-cipher-done! cipher)
    plain-text))

(define plain-text (string->utf8 "Hello Sagittarius Scheme!"))
(decrypt aes-cipher key parameter (encrypt aes-cipher key parameter plain-text))
;; -> A bytevector of UTF-8 representation of `plain-text`
