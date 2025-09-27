(import (rnrs)
        (sagittarius crypto ciphers)
        (sagittarius crypto keys)
        (sagittarius crypto digests))

;; Randomly genrates RSA key pair. Default 2048 bits
(define key-pair (generate-key-pair *key:rsa*))

;; Making RSA cipher with OAEP encoding (default) with SHA-256
(define rsa-cipher
 (make-asymmetric-cipher *scheme:rsa* :digest *digest:sha-256*))

;; Encryption can only be done by a public key
(define (encrypt cipher key plain-text)
  (asymmetric-cipher-init! cipher key)
  (let ((cipher-text (asymmetric-cipher-encrypt-bytevector cipher plain-text)))
    (asymmetric-cipher-done! cipher)
    cipher-text))

;; Decryption can only be done by a private key
(define (decrypt cipher key cipher-text)
  (asymmetric-cipher-init! cipher key)
  (let ((plain-text (asymmetric-cipher-decrypt-bytevector cipher cipher-text)))
    (asymmetric-cipher-done! cipher)
    plain-text))

(define message (string->utf8 "Hello Sagittarius Scheme"))
(decrypt rsa-cipher (key-pair-private key-pair)
  (encrypt rsa-cipher (key-pair-public key-pair) message))
;; -> A bytevector of UTF-8 representation of `plain-text`
