[§2] Cryptographic libraries {#lib.sagittarius.crypto}
------------------------------------------------------

Sagittarius provides a comprehensive set of cryptographic libraries.
Currently the libraries support below:

- Block cipher
- Asymmetric cipher
- Cryptographic hash functions
- Cryptographically secure pseudo random number generators (CSPRNG)
- Signature generation / verification
- Key generate, agreement and import/export
- MAC
- X.509 CSR, CRL and certificate
- PKCS#5 via PBKDF
- PKCS#8
- PKCS#12 keystore
- PEM
- Various KDFs

Not like legacy `(crypto)` and `(math)` libraries, these libraries are
split per components. Users need to combine some libraries to achieve
their goals.

### Examples

This section shows some basic examples how to use the crypto graphic
libraries.

#### Block cipher

This example encrypts and decrypts a plain text with a radomly generated
secret key. The encryption scheme is AES-256 and encryption mode is CTR.

```scheme
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
```

This example shows how to use a string password as a key
(Password-Based Encryption Scheme, a.k.a. PBES). PBES is just a block cipher
using a derived key. To do so, you need to use KDF, Key Derivation Function.

```scheme
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
```

This example shows how to handle `offline` encryption mode.

```scheme


```

#### Stream cipher

This example encrypts and decrypts a plain text with a randomly generated
secret key. The encryption scheme is ChaCha20 Poly1305.

```scheme
(import (rnrs)
        (sagittarius crypto ciphers)
        (sagittarius crypto keys))

;; Randomly generates AES-256 secret key.
(define key (generate-symmetric-key *scheme:aes-256*))



```

#### Asymmetric cipher

This example encrypts and decrypts a plain text with a randomly generated
RSA key pair.

```scheme
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
```

#### Cryptographic hash function

In the libraries, hash is called digest. So, from now on we use digest instead
of hash.

Below example shows how to generate a digest of SHA-256.

```scheme
(import (rnrs)
        (sagittarius crypto digests))

(define md (make-message-digest *digest:sha-256*))

(digest-message md (string->utf8 "Hello Sagittarius Scheme"))
;; -> #vu8(65 109 154 253 119 192 195 187 255 90 75 208 135 51 25 43 106 121 236 172 96 233 38 189 154 240 32 8 116 58 169 237)
```

Also below example shows how to generate a digest of SHAKE-256, which
generates a variable length digest.

```scheme
(import (rnrs)
        (sagittarius crypto digests))

(define md (make-message-digest *digest:shake-256*))

;; Generates 32 bytes digests
(digest-message md (string->utf8 "Hello Sagittarius Scheme") 32)
;; -> #vu8(127 141 98 85 40 216 103 129 10 71 136 179 158 103 163 218 109 65 244 77 119 4 109 54 135 126 225 162 188 58 16 64)
```

#### Cryptographically secure pseudo random number generators (CSPRNG)

Below example shows how to generate a random integer.

```scheme
(import (rnrs)
        (sagittarius crypto random))

;; Pseudo random generator, it returns the same value each execution
(define prng (pseudo-random-generator *prng:chacha20*))
;; Secure random generator, it returns random value each execution
;; (define prng (secure-random-generator *prng:chacha20*))

;; range of 0 <= r < 100
(random-generator-random-integer prng 100)
;; -> each time the same result as it's pseudo random, not secure random
```

* @[[sagittarius/crypto/keys.md](sagittarius/crypto/keys.md)]
* @[[sagittarius/crypto/ciphers.md](sagittarius/crypto/ciphers.md)]
* @[[sagittarius/crypto/digests.md](sagittarius/crypto/digests.md)]
* @[[sagittarius/crypto/random.md](sagittarius/crypto/random.md)]
* @[[sagittarius/crypto/signatures.md](sagittarius/crypto/signatures.md)]
* @[[sagittarius/crypto/mac.md](sagittarius/crypto/mac.md)]
* @[[sagittarius/crypto/x509.md](sagittarius/crypto/x509.md)]
* @[[sagittarius/crypto/keystore.md](sagittarius/crypto/keystore.md)]
* @[[sagittarius/crypto/kdfs.md](sagittarius/crypto/kdfs.md)]
