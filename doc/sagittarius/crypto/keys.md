[§3] Key library - (sagittarius crypto keys) {#sagittarius.crypto.keys}
------------------------------------------------------

The key library provides key operations for both symmetric and asymmetric
keys.

###### [!Library] `(sagittarius crypto keys)`

The key library, this library exports the procedures listed below sections.

### [§4] Key type

Sagittarius crypto library uses hierachial key structure. The hierarchy of
the key type is below.

```
                 +--------------+
                 | <crypto-key> |
                 +------+-------+
                        |
         +--------------+-------------+
         |                            |
+--------+--------+          +--------+---------+
| <symmetric-key> |          | <asymmetric-key> |
+-----------------+          +--------+---------+
                                      |
                             +--------+--------+
                             |                 |
                     +-------+------+  +-------+------+
                     | <public-key> |  | <private-key |
                     +--------------+  +--------------+
```

###### [!Function] `crypto-key?` _obj_

Returns `#t` if the given _obj_ is an instance of `<crypto-key>`,
otherwise `#f`.

###### [!Function] `symmetric-key?` _obj_

Returns `#t` if the given _obj_ is an instance of `<symmetric-key>`,
otherwise `#f`.

###### [!Function] `asymmetric-key?` _obj_

Returns `#t` if the given _obj_ is an instance of `<asymmetric-key>`,
otherwise `#f`.

###### [!Function] `public-key?` _obj_

Returns `#t` if the given _obj_ is an instance of `<public-key>`,
otherwise `#f`.

###### [!Function] `private-key?` _obj_

Returns `#t` if the given _obj_ is an instance of `<private-key>`,
otherwise `#f`.

### [§4] Symmetric key operations

###### [!Function] `make-symmetric-key` (_bv_ `bytevector?`)

Creates a symmetric key of the value of _bv_.  
The key can be used for any of the symmetric ciphers supported by
Sagittarius, however it is users' responsibility to align the key
size. If you want to make sure the _bv_ has the right key size,
consier using `generate-symmetric-key` method.

###### [!Function] `symmetric-key-value` (_key_ `symmetric-key?`)

Retrieves the raw key value of the given _key_.

###### [!Method] `generate-symmetric-key` `<block-cipher-descriptor>`
###### [!Method] `generate-symmetric-key` `<block-cipher-descriptor>` `<random-generator>`

Generates a symmetric key suitable for the given block cipher scheme, 
randomly.

###### [!Method] `generate-symmetric-key` `<block-cipher-descriptor>` `<bytevector>`

Creates a symmetric from the bytevector, this method checks the
key size if it's appropriate for the given cipher or not.

###### [!Function] `make-rfc3394-key-wrap` (_scheme_ `block-cipher-descriptor?`) (_key_ `symmetric-key?`) :key _iv_

Creates a key wrap procedure using given _scheme_ and _key_. The optional
keyword parameter _iv_ is used as an initial vector if provided.

###### [!Function] `make-aes-key-wrap` (_key_ `symmetric-key?`) _opts_ _..._

Creates an AES key wrap procedure.

###### [!Function] `make-camellia-key-wrap` (_key_ `symmetric-key?`) _opts_ _..._

Creates an Camellia key wrap procedure.

###### [!Function] `make-rfc3394-key-unwrap` (_scheme_ `block-cipher-descriptor?`) (_key_ `symmetric-key?`) :key _iv_

Creates a key unwrap procedure using given _scheme_ and _key_. The optional
keyword parameter _iv_ is used as an initial vector if provided.

###### [!Function] `make-aes-key-unwrap` (_key_ `symmetric-key?`) _opts_ _..._

Creates an AES key unwrap procedure.

###### [!Function] `make-camellia-key-unwrap` (_key_ `symmetric-key?`) _opts_ _..._

Creates an Camellia key unwrap procedure.


### [§4] Asymmetric key operations

###### [!Function] `key-pair?` _obj_

Returns `#t` if the given _obj_ is a key pair, otherwise `#f`.

###### [!Function] `key-pair-private` (_kp_ `key-pair?`)

Returns the private key of the given _kp_.

###### [!Function] `key-pair-public`

Returns the public key of the given _kp_.

###### [!Method] `generate-key-pair` _scheme_ _opts_ _..._

Generates a key pair of the given _scheme_. Followings are the supported
schemes:

###### [!Key scheme] `*key:rsa`
###### [!Key scheme] `*key:dsa`
###### [!Key scheme] `*key:ecdsa`
###### [!Key scheme] `*key:ed25519`
###### [!Key scheme] `*key:ed448`
###### [!Key scheme] `*key:x25519`
###### [!Key scheme] `*key:x448`

Those are RSA, DSA, ECDSA, Ed5519, Ed448, X25519 and X448 respectively.  
For convenience `*scheme:rsa` has the same effect as `*key:rsa`.

###### [!Function] `generate-public-key` _scheme_ _opts_ ...
###### [!Function] `generate-private-key` _scheme_ _opts_ ...

###### [!Macro] `public-key-format` _format_

A macro returns a symbol representation of _format_. The _format_ must be
either `raw` or `subject-public-key-info`.

`raw`:
  Imports/exports raw public key.
`subject-public-key-info`:
  Imports/exports subject public key info.

###### [!Enum set] `*public-key-formats*`

Enum set of the `public-key-format`.

###### [!Function] `public-key-format?` _obj_

Returns `#t` if the given _obj_ is a member of `public-key-format` enum.

###### [!Method] `import-public-key` `<bytevector>` `'subject-public-key-info`
###### [!Method] `import-public-key` `<port>` `'subject-public-key-info`

Imports a public from the given `<bytevector>` or `<port>` which must be
a valid subject public key info.  
The returning public key type is detected via the OID inside of the
subject public key info.

###### [!Method] `import-public-key` `*key:rsa*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:rsa*` `<port>` :optional _format_

Imports RSA key from given `<bytevector>` or `<port>`. The optional _format_
specifies the format of the given key. Default value is `raw`.

###### [!Method] `export-public-key` `<rsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:rsa*` `<rsa-public-key>` :optional _format_

Exports RSA key from the given `<rsa-public-key>`. The optional _format_
controls the format of the exporting key. Default value is `raw`.

###### [!Method] `import-public-key` `*key:dsa*` `<bytevector>` :optional _format_ _parameter_
###### [!Method] `import-public-key` `*key:dsa*` `<port>` :optional _format_ _parameter_

Imports DSA key from given `<bytevector>` or `<port>`. The optional
_format_ specifies the format of the given key. Default value is
`subject-public-key-info`.

If `raw` is specified, then optional argument _parameter_ must be specified
and it must be `<dsa-key-parameter>`

###### [!Method] `export-public-key` `<dsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:dsa*` `<dsa-public-key>` :optional _format_

Exports DSA key from the given `<dsa-public-key>`. The optional
_format_ controls the format of the exporting key. Default value
is `subject-public-key-info`.

###### [!Method] `import-public-key` `*key:ecdsa*` `<bytevector>` :optional _format_ _ec-parameter_
###### [!Method] `import-public-key` `*key:ecdsa*` `<port>` :optional _format_ _ec-parameter_

Imports ECDSA key from given `<bytevector>` or `<port>`. The optional
_format_ specifies the format of the given key. Default value is
`subject-public-key-info`.  
If `raw` format is specified, then optional parameter `ec-parameter` must be
specified. Below are the supported EC parameters.

###### [!EC parameter] `*ec-parameter:p192*`
###### [!EC parameter] `*ec-parameter:p224*`
###### [!EC parameter] `*ec-parameter:p256*`
###### [!EC parameter] `*ec-parameter:p384*`
###### [!EC parameter] `*ec-parameter:p521*`
###### [!EC parameter] `*ec-parameter:k163*`
###### [!EC parameter] `*ec-parameter:k233*`
###### [!EC parameter] `*ec-parameter:k283*`
###### [!EC parameter] `*ec-parameter:k409*`
###### [!EC parameter] `*ec-parameter:k571*`
###### [!EC parameter] `*ec-parameter:b163*`
###### [!EC parameter] `*ec-parameter:b233*`
###### [!EC parameter] `*ec-parameter:b283*`
###### [!EC parameter] `*ec-parameter:b409*`
###### [!EC parameter] `*ec-parameter:b571*`
###### [!EC parameter] `*ec-parameter:secp160r1*` **[@since] `0.9.12`**
###### [!EC parameter] `*ec-parameter:secp160k1*` **[@since] `0.9.12`**
###### [!EC parameter] `*ec-parameter:secp160r2*` **[@since] `0.9.12`**
###### [!EC parameter] `*ec-parameter:secp192r1*`
###### [!EC parameter] `*ec-parameter:secp224r1*`
###### [!EC parameter] `*ec-parameter:secp256r1*`
###### [!EC parameter] `*ec-parameter:secp384r1*`
###### [!EC parameter] `*ec-parameter:secp521r1*`
###### [!EC parameter] `*ec-parameter:sect163k1*`
###### [!EC parameter] `*ec-parameter:sect233k1*`
###### [!EC parameter] `*ec-parameter:sect283k1*`
###### [!EC parameter] `*ec-parameter:sect409k1*`
###### [!EC parameter] `*ec-parameter:sect571k1*`
###### [!EC parameter] `*ec-parameter:sect163r2*`
###### [!EC parameter] `*ec-parameter:sect233r1*`
###### [!EC parameter] `*ec-parameter:sect283r1*`
###### [!EC parameter] `*ec-parameter:sect409r1*`
###### [!EC parameter] `*ec-parameter:sect571r1*`
###### [!EC parameter] `*ec-parameter:secp192k1*`
###### [!EC parameter] `*ec-parameter:secp224k1*`
###### [!EC parameter] `*ec-parameter:secp256k1*`
###### [!EC parameter] `*ec-parameter:sect163r1*`
###### [!EC parameter] `*ec-parameter:sect239k1*`
###### [!EC parameter] `*ec-parameter:sect113r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p160r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p160t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p192r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p192t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p224r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p224t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p256r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p256t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p320r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p320t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p384r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p384t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p512r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p512t1*`

After `ec-parameter:` represents the parameter name.

###### [!Method] `export-public-key` `<ecdsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:ecdsa*` `<ecdsa-public-key>` :optional _format_

Exports ECDSA key from the given `<ecdsa-public-key>`. The optional
_format_ controls the format of the exporting key. Default value
is `subject-public-key-info`.

###### [!Method] `import-public-key` `*key:ed25519*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:ed25519*` `<port>` :optional _format_
###### [!Method] `import-public-key` `*key:ed448*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:ed448*` `<port>` :optional _format_

Imports Ed25519 or Ed448 public key from the given `<bytevector>` or `<port>`.
The optional _format_ controls the key format, default is `raw`.

###### [!Method] `export-public-key` `<eddsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:ed25519*` `<eddsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `<eddsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:ed448*` `<eddsa-public-key>` :optional _format_

Exports Ed25519 or Ed448 key from the given `<eddsa-public-key>`. The optional
_format_ controls the format of the exporting key. Default value
is `raw`.

If the first form is used, then the method automatically detects the key type.

###### [!Method] `import-public-key` `*key:x25519*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:x25519*` `<port>` :optional _format_
###### [!Method] `import-public-key` `*key:x448*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:x448*` `<port>` :optional _format_

Imports X25519 or X448 public key from the given `<bytevector>` or `<port>`.
The optional _format_ controls the key format, default is `raw`.

###### [!Method] `export-public-key` `<rfc7748-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:x25519*` `<rfc7748-public-key>` :optional _format_
###### [!Method] `export-public-key` `<rfc7748-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:x448*` `<rfc7748-public-key>` :optional _format_

Exports X25519 or X448 key from the given `<rfc7748-public-key>`. The optional
_format_ controls the format of the exporting key. Default value
is `raw`.

If the first form is used, then the method automatically detects the key type.

###### [!Macro] `private-key-format` _format_

A macro returns a symbol representation of _format_. The _format_ must be
`raw`, `private-key-info`, `seed`, or `expanded`. The latter two formats
are specific to post-quantum keys.

`raw`:
  Imports/exports raw private key.
`private-key-info`:
  Imports/exports private key info or one asymmetric key.
`seed`:
  Imports a PQC generation seed; exports a retained ML-KEM or ML-DSA seed.
`expanded`:
  Imports/exports a PQC expanded private key without ASN.1 wrapping.

###### [!Enum set] `*private-key-formats*`

Enum set of the `private-key-format`.

###### [!Function] `private-key-format?` _obj_

Returns `#t` if the given _obj_ is a member of `private-key-format` enum.

###### [!Method] `import-private-key` `<bytevector>` `'private-key-info`
###### [!Method] `import-private-key` `<port>` `'private-key-info`

Imports a private key from the `<bytevector>` or `<port>` which must
be a valid private key info or one asymmetric key.  
The returning private key type is detected by the OID inside of the
private key info.

###### [!Method] `import-private-key` `*key:rsa` `<bytevector>` 
###### [!Method] `import-private-key` `*key:rsa` `<port>` 

Imports a RSA private key from the `<bytevector>` or `<port>`. The
format of the input must be a raw RSAPrivateKey.

###### [!Method] `export-private-key` `<rsa-crt-private-key>` :optional _format_
###### [!Method] `export-private-key` `*key:rsa*` `<rsa-crt-private-key>` :optional _format_

Exporting RSA private key is only supported with CRT key.  
Exports the given `<rsa-crt-private-key>`. The optional _format_ controls
the format of the exporting key. Default value is `raw`.

###### [!Method] `import-private-key` `*key:dsa` `<bytevector>` 
###### [!Method] `import-private-key` `*key:dsa` `<port>` 

Imports a DSA private key from the `<bytevector>` or `<port>`. The
format of the input must be a raw DSAPrivateKey.

###### [!Method] `export-private-key` `<dsa-private-key>` :optional _format_
###### [!Method] `export-private-key` `*key:dsa*` `<dsa-private-key>` :optional _format_

Exports the given `<dsa-private-key>`. The optional _format_ controls
the format of the exporting key. Default value is `raw`.

###### [!Method] `import-private-key` `*key:ecdsa` `<bytevector>` :optional _ec-parameter_
###### [!Method] `import-private-key` `*key:ecdsa` `<port>` :optional _ec-parameter_

Imports a ECDSA private key from the `<bytevector>` or `<port>`. The
format of the input must be a raw ECPrivateKey.  
If the given raw key doesn't contain EC parameter, then the optional
_ec-parameter_ must be provided.

###### [!Method] `export-private-key` `<ecdsa-private-key>` :optional _format_
###### [!Method] `export-private-key` `*key:ecdsa*` `<ecdsa-private-key>` :optional _format_

Exports the given `<ecdsa-private-key>`. The optional _format_ controls
the format of the exporting key. Default value is `raw`.

###### [!Method] `import-private-key` `*key:ed25519` `<bytevector>`
###### [!Method] `import-private-key` `*key:ed25519` `<port>`
###### [!Method] `import-private-key` `*key:ed448` `<bytevector>`
###### [!Method] `import-private-key` `*key:ed448` `<port>`

Imports a Ed25519 or Ed448 private key from the `<bytevector>` or `<port>`.

###### [!Method] `export-private-key` `<eddsa-private-key>` :optional _format_
###### [!Method] `export-private-key` `*key:ed25519*` `<eddsa-private-key>` :optional _format_
###### [!Method] `export-private-key` `*key:ed448*` `<eddsa-private-key>` :optional _format_

Exports the given `<eddsa-private-key>`. The optional _format_ controls
the format of the exporting key. Default value is `raw`.  
If the first form is used, then the exporting key type is detected 
from the given key.

###### [!Method] `calculate-key-agreement` `*key:ecdh*` `<ecdsa-private-key>` `<ecdsa-public-key>`
###### [!Method] `calculate-key-agreement` `*key:x25519*` `<rfc7748-private-key>` `<rfc7748-public-key>`
###### [!Method] `calculate-key-agreement` `*key:x449*` `<rfc7748-private-key>` `<rfc7748-public-key>`

Calculate a secret key from the given private key and public key.

###### [!Function] `oid->key-operation`
###### [!Function] `key->oid`

### [§4] Experimental post-quantum keys

ML-KEM (FIPS 203), ML-DSA (FIPS 204), and SLH-DSA (FIPS 205) are
available through this library. This support is built by default but is
**experimental**: its isolated backend uses a pinned, unmerged LibTomCrypt
implementation, not a certified cryptographic module. Pre-standard Kyber,
Dilithium and SPHINCS+ encodings are not interchangeable with these keys.

###### [!Key scheme] `*key:ml-kem-512*`
###### [!Key scheme] `*key:ml-kem-768*`
###### [!Key scheme] `*key:ml-kem-1024*`
###### [!Key scheme] `*key:ml-dsa-44*`
###### [!Key scheme] `*key:ml-dsa-65*`
###### [!Key scheme] `*key:ml-dsa-87*`

The SLH-DSA schemes are `*key:slh-dsa-sha2-128s*`,
`*key:slh-dsa-sha2-128f*`, `*key:slh-dsa-sha2-192s*`,
`*key:slh-dsa-sha2-192f*`, `*key:slh-dsa-sha2-256s*`,
`*key:slh-dsa-sha2-256f*`, `*key:slh-dsa-shake-128s*`,
`*key:slh-dsa-shake-128f*`, `*key:slh-dsa-shake-192s*`,
`*key:slh-dsa-shake-192f*`, `*key:slh-dsa-shake-256s*`, and
`*key:slh-dsa-shake-256f*`.

HashSLH-DSA uses distinct schemes, formed by adding `hash-` before `slh-dsa`
and the following suffix to each corresponding key name:

| Parameter family | Suffix |
|---|---|
| SHA2-128s/f | `-with-sha256` |
| SHA2-192s/f, SHA2-256s/f | `-with-sha512` |
| SHAKE-128s/f | `-with-shake128` |
| SHAKE-192s/f, SHAKE-256s/f | `-with-shake256` |

For example, `*key:hash-slh-dsa-sha2-128f-with-sha256*` is different
from `*key:slh-dsa-sha2-128f*`. Parameter sets and pure/prehash SLH-DSA
keys cannot be mixed implicitly.

###### [!Method] `generate-key-pair` _pqc-scheme_ :key _prng_
###### [!Method] `generate-public-key` _pqc-scheme_ _bytes_
###### [!Method] `generate-private-key` _pqc-scheme_ _bytes_

Key-pair generation defaults to a secure ChaCha20 random generator with
32 bytes of initial entropy. The optional _prng_ uses the existing
random-generator API; insecure or fixed generators are suitable only for tests.

Public _bytes_ are a raw public key. Private _bytes_ are either a generation
seed or an expanded private key, distinguished by length. Seeds contain
64 bytes for ML-KEM (`d || z`), 32 bytes for ML-DSA, and `3n` bytes for
SLH-DSA (`SK.seed || SK.prf || PK.seed`, with `n` equal to 16, 24, or 32).
SLH-DSA generation seeds are not retained for export.

###### [!Method] `encapsulate` _ml-kem-scheme_ _public-key_ :key _prng_
###### [!Method] `decapsulate` _ml-kem-scheme_ _private-key_ _ciphertext_

Encapsulation returns two values: ciphertext and a 32-byte shared secret.
The default _prng_ is secure. Decapsulation requires no randomness and returns
only the shared secret. Correctly sized invalid ciphertexts produce the
FIPS 203 implicit-rejection secret, not an exception or validity flag.
Incorrect lengths and invalid key encodings raise an assertion violation.
Authenticate the surrounding protocol; a returned secret is not proof of
ciphertext validity. These methods do not replace `calculate-key-agreement`.

```scheme
(import (rnrs) (sagittarius crypto keys))
(define kem-pair (generate-key-pair *key:ml-kem-768*))
(let-values (((ciphertext secret)
              (encapsulate *key:ml-kem-768* (key-pair-public kem-pair))))
  (display (equal? secret
             (decapsulate *key:ml-kem-768* (key-pair-private kem-pair)
                          ciphertext)))
  (newline)) ; prints #t
(destroy-key! (key-pair-private kem-pair))
```

###### [!Method] `import-public-key` _pqc-scheme_ _input_ :optional _format_
###### [!Method] `import-private-key` _pqc-scheme_ _input_ :optional _format_
###### [!Method] `export-public-key` _pqc-key_ :optional _format_
###### [!Method] `export-private-key` _pqc-key_ :optional _format_

The default format is `raw`. Public formats are `raw` and
`subject-public-key-info`; private formats are `raw`, `seed`, `expanded`,
and `private-key-info`. Raw private export returns the expanded key.
Seed export fails if no ML-KEM/ML-DSA seed is retained. Input bytevectors
and exported bytevectors do not alias live key material.

PKIX import/export follows RFC 9935, RFC 9881, and RFC 9909 respectively.
AlgorithmIdentifier parameters must be absent, not NULL. Bytevector PKIX
input must contain exactly one DER object. Generic OID-detecting imports
also support these encodings.

Private-key-info export accepts `:representation` (`auto`, `seed`,
`expanded`, or `both`) and `:public-key?` (default `#f`). The latter selects
OneAsymmetricKey with its public key. For ML-KEM and ML-DSA, `auto` prefers
a retained seed; `both` includes the seed and expanded key and requires
their consistency on import. SLH-DSA accepts only `auto` and `expanded`;
its privateKey OCTET STRING contains raw private bytes without inner ASN.1.
Optional embedded public keys must match the private key.

ML-KEM OIDs are `2.16.840.1.101.3.4.4.1` through `.3`. Pure ML-DSA OIDs
are `2.16.840.1.101.3.4.3.17` through `.19`; pure SLH-DSA uses `.20`
through `.31`, and HashSLH-DSA uses `.35` through `.46`. RFC 9881 key
encodings describe pure ML-DSA keys, not HashML-DSA signature OIDs.

PQC private keys support `destroy-key!`, which wipes retained secret bytes
and seeds and prevents further private operations. GC wiping is best effort;
explicit destruction cannot erase exported copies, all runtime temporaries,
or OS copies. Applications must protect exported secrets and serialize
destruction against concurrent operations on the same key.

