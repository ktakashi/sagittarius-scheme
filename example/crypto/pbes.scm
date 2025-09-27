(import (rnrs)
        (sagittarius crypto ciphers)
        (sagittarius crypto keys)
        (sagittarius crypto kdfs)    ;; for PBKDF2
        (sagittarius crypto digests) ;; for digests, e.g. SHA-256
        (sagittarius crypto mac))    ;; for MAC, NOTE: PRF = MAC

;; If everyone uses this kind of password, then it'd be almost impossibe
;; to crack :)
(define password "You can't guess this password! It's so strong!! :D")

;; PRF
(define prf (mac->prf-provider *mac:hmac* :digest *digest:sha-256*))

(define (derive-key scheme password)
  ;; I'm not sure if this is a bad practice or not, but for simplicity
  ;; we use hashed password as a salt
  (define md (make-message-digest *digest:sha-256*))
  (define bv (string->utf8 password))
  (define salt (digest-message md bv))
  ;; 310,000 iteration, required by FIPS-140
  (pbkdf-2 bv salt 310000
    (block-cipher-descriptor-suggested-key-length scheme)
	:prf prf))

(define key 
 (generate-symmetric-key *scheme:aes-256*
  (derive-key *scheme:aes-256* password)))

;; The rest can be the same as above CTR mode example.
