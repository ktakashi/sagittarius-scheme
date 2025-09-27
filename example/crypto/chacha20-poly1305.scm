(import (rnrs)
        (sagittarius crypto ciphers)
	(sagittarius crypto random)
        (sagittarius crypto keys))

;; Randomly generates secret key.
(define key (generate-symmetric-key *scheme:chacha20*))

;; secure random generator
(define prng (secure-random-generator *prng:chacha20*))

(define cipher (make-stream-cipher *scheme:chacha20-poly1305*))
;; 96 bits nonce
(define nonce (make-iv-parameter (random-generator-read-random-bytes prng 12)))

;; init cipher for encryption
(stream-cipher-init! cipher (cipher-direction encrypt) key nonce)
(define plain-text (string->utf8 "Hello ChaCha20-Poly1305"))

(define cipher-text (stream-cipher-encrypt cipher plain-text))
;; 128 bits tag length
(define tag (stream-cipher-done/tag cipher 16))

;; reinitialize cipher for decryption
(stream-cipher-init! cipher (cipher-direction decrypt) key nonce)

(let ((pt (stream-cipher-decrypt cipher cipher-text)))
  ;; make sure you verify the tag
  (stream-cipher-done/tag! cipher tag)
  ;; the plain text
  (utf8->string pt))
