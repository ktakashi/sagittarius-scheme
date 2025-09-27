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

* @[[AES-256 CTR](../../example/crypto/aes-ctr.scm)]

This example shows how to use a string password as a key
(Password-Based Encryption Scheme, a.k.a. PBES). PBES is just a block cipher
using a derived key. To do so, you need to use KDF, Key Derivation Function.

* @[[PBES](../../example/crypto/pbes.scm)]

This example shows how to handle `offline` encryption mode.

* @[[AES-SIV](../../example/crypto/aes-siv.scm)]

#### Stream cipher

This example encrypts and decrypts a plain text with a randomly generated
secret key. The encryption scheme is ChaCha20 Poly1305.

* @[[ChaCha20 Poly1305](../../example/crypto/chacha20-poly1305.scm)]

#### Asymmetric cipher

This example encrypts and decrypts a plain text with a randomly generated
RSA key pair.

* @[[RSA Cipher](../../example/crypto/rsa-cipher.scm)]

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

* @[[sagittarius/crypto/keys.md](crypto/keys.md)]
* @[[sagittarius/crypto/ciphers.md](crypto/ciphers.md)]
* @[[sagittarius/crypto/digests.md](crypto/digests.md)]
* @[[sagittarius/crypto/random.md](crypto/random.md)]
* @[[sagittarius/crypto/signatures.md](crypto/signatures.md)]
* @[[sagittarius/crypto/mac.md](crypto/mac.md)]
* @[[sagittarius/crypto/x509.md](crypto/x509.md)]
* @[[sagittarius/crypto/keystore.md](crypto/keystore.md)]
* @[[sagittarius/crypto/kdfs.md](crypto/kdfs.md)]
