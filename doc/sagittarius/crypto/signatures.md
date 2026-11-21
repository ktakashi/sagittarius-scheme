[§3] Signature library - (sagittarius crypto signatures) {#sagittarius.crypto.signatures}
------------------------------------------------------

The signature library provides signature generation / verification
operations.

###### [!Library] `(sagittarius crypto signatures)`

The signature library, this library exports the procedures listed
below sections.

### [§4] Algorithms

Below are the algorithms supported by the library.

###### [!Signature algorithm] `*signature:rsa*`
###### [!Signature algorithm] `*signature:dsa*`
###### [!Signature algorithm] `*signature:ecdsa*`
###### [!Signature algorithm] `*signature:eddsa*`
###### [!Signature algorithm] `*signature:ed25519*`
###### [!Signature algorithm] `*signature:ed25519ctx*`
###### [!Signature algorithm] `*signature:ed25519ph*`
###### [!Signature algorithm] `*signature:ed448*`
###### [!Signature algorithm] `*signature:ed448ph*`

These algorithms can be used both signer and verifier.

### [§4] Signer

###### [!Function] `signer?` _obj_

Returns `#t` if the given _obj_ is a signer, otherwise `#f`.

###### [!Function] `make-signer` _scheme_ _key_ _opts_ _..._

Creates a signer of the _scheme_. The _scheme_ must be one of the
signature algorithms listed above. The _key_ must be an appropriate
private key for the specified _scheme_.

_opts_ must be keyword arguments. Existing classical algorithms ignore
unsupported keywords; PQC algorithms reject them. The supported keywords
and their schemes are below.

`:digest`
: For all signature algorithms. Specifying digest algorithm. Most of
  the algorithm uses `*digest:sha-256*` as its default digest algorithm.

`:encoder`
: For `*signature:rsa*`. Specifying signature encoding, default value
  is `pkcs1-emsa-pss-encode`.

`:k-generator`
: For `*signature:dsa*` and `*signature:ecdsa*`. Specifying _k_ generator.
  It uses random generator as default value.

`:der-encode`
: For `*signature:dsa*` and `*signature:ecdsa*`. Specifying if the result
  signature is DER encoded or not. Default value is `#t`.

`:scheme`
: For `*signature:eddsa*`. Specifying EdDSA scheme.

`:context`
: For `*signature:eddsa*` `*signature:ed25519ctx*`, `*signature:ed25519ph*`,
  `*signature:ed448*`, and `*signature:ed448ph*`.
  Specifying context, default value is `#vu8()`.

For the scheme specific procedures such as `k-generator`, see the below 
sections.

###### [!Function] `signer-sign-message` (_signer_ `signer?`) (_message_ `bytevector?`)

Signs the given _message_ with _signer_ and returns the signature.

###### [!Function] `signer-init!` (_signer_ `signer?`)

Initialises the given _signer_

###### [!Function] `signer-process!` (_signer_ `signer?`) (_message_ `bytevector?`) :optional _start_ _length_

Feeds the given _message_ to the _signer_. The optional arguments specifies
the range of the _message_ to feed.

###### [!Function] `signer-sign!` (_signer_ `signer?`)

Signs the accumulated message of _signer_ and returns a bytevector.

### [§4] Verifier

###### [!Function] `verifier?` _obj_

Returns `#t` if the given _obj_ is a verifier, otherwise `#f`.

###### [!Function] `make-verifier` _scheme_ _key_ _opts_ _..._

Creates a verifier of the _scheme_. The _scheme_ must be one of the
signature algorithms listed above. The _key_ must be an appropriate
public key for the specified _scheme_.

_opts_ must be keyword arguments. Existing classical algorithms ignore
unsupported keywords; PQC algorithms reject them. The supported keywords
and their schemes are below.

`:digest`
: For all signature algorithms. Specifying digest algorithm. Most of
  the algorithm uses `*digest:sha-256*` as its default digest algorithm.

`:verifier`
: For `*signature:rsa*`. Specifying signature encoding, default value
  is `pkcs1-emsa-pss-verify`.

`:der-encode`
: For `*signature:dsa*` and `*signature:ecdsa*`. Specifying if the result
  signature is DER encoded or not. Default value is `#t`.

`:scheme`
: For `*signature:eddsa*`. Specifying EdDSA scheme.

`:context`
: For `*signature:eddsa*` `*signature:ed25519ctx*`, `*signature:ed25519ph*`,
  `*signature:ed448*`, and `*signature:ed448ph*`.
  Specifying context, default value is `#vu8()`.

###### [!Function] `verifier-verify-signature` (_verifier_ `verifier?`) (_message_ `bytevector?`) (_signature_ `bytevector?`)

Verifies if the given _signature_ is valid signature of the given _message_
with _verifier_.

It returns `#t` if the given _signature_ is a valid signature.  
If the signature verification is failed, then returns `#f`.  
If the signature format is not valid, then raise an error.

###### [!Function] `verifier-init!` (_verifier_ `verifier?`)

Initialises the given _verifier_.

###### [!Function] `verifier-process!` (_verifier_ `verifier?`) (_message_ `bytevector?`) :optional _start_ _length_

Feeds the given _message_ to the _verifier_. The optional arguments specifies
the range of the _message_ to feed.

###### [!Function] `verifier-verify!`  (_verifier_ `verifier?`) (_signature_ `bytevector?`)

Verifies the given _signature_ against the accumulated messgage.

It returns `#t` if the given _signature_ is a valid signature.  
If the signature verification is failed, then returns `#f`.  
If the signature format is not valid, then raise an error.

### [§4] Experimental post-quantum signatures

PQC support is experimental and uses a pinned, unmerged LibTomCrypt backend.
It is not a security certification. Keys come from `(sagittarius crypto keys)`;
no separate public PQC library is needed.

###### [!Signature algorithm] `*signature:ml-dsa-44*`
###### [!Signature algorithm] `*signature:ml-dsa-65*`
###### [!Signature algorithm] `*signature:ml-dsa-87*`
###### [!Signature algorithm] `*signature:hash-ml-dsa-44-with-sha512*`
###### [!Signature algorithm] `*signature:hash-ml-dsa-65-with-sha512*`
###### [!Signature algorithm] `*signature:hash-ml-dsa-87-with-sha512*`

ML-DSA signatures use the corresponding ML-DSA parameter-set key. The first
three schemes default to pure FIPS 204 signing (`:digest #f`). Supplying an
approved digest selects HashML-DSA, including its FIPS domain separation and
digest OID; the message must not be prehashed by the caller. Approved digests
are SHA-224/256/384/512, SHA-512/224 and /256, SHA3-224/256/384/512, and
SHAKE128/256. SHAKE prehash outputs are respectively 32 and 64 bytes.
The three explicitly named HashML-DSA schemes fix SHA-512.

All twelve pure SLH-DSA and twelve HashSLH-DSA schemes have the same names
as their key schemes, replacing `*key:` with `*signature:`. For example,
`*signature:slh-dsa-sha2-128f*` and
`*signature:hash-slh-dsa-sha2-128f-with-sha256*` require their respective
key schemes. The named prehash variant determines its hash; arbitrary
`:digest` overrides are not supported for SLH-DSA.

PQC signer/verifier options:

| Keyword | Meaning |
|---|---|
| `:context` | Bytevector of at most 255 bytes, copied into the state; default empty |
| `:digest` | ML-DSA prehash selection as described above; otherwise `#f` |
| `:deterministic?` | Signers only; default `#f`, selecting randomized/hedged signing |
| `:prng` | Signers only; defaults to a secure ChaCha20 generator with 32 bytes of initial entropy |
| `:der-encode` | Must be `#f`; PQC signatures always have their raw fixed-length encoding |

Deterministic signing must be selected explicitly with `:deterministic? #t`.
It uses zero randomness for ML-DSA and `PK.seed` for SLH-DSA, as specified
by their respective standards. A fixed or insecure _prng_ is for tests only.

```scheme
(import (rnrs) (sagittarius crypto keys) (sagittarius crypto signatures))
(define signature-pair (generate-key-pair *key:ml-dsa-44*))
(define message (string->utf8 "PQC example"))
(define signature
  (signer-sign-message
    (make-signer *signature:ml-dsa-44* (key-pair-private signature-pair))
    message))
(display (verifier-verify-signature
           (make-verifier *signature:ml-dsa-44* (key-pair-public signature-pair))
           message signature))
(newline) ; prints #t
(destroy-key! (key-pair-private signature-pair))
```

Incremental signer/verifier operations are supported by buffering the message;
they are not constant-memory streaming. Invalid signatures, including incorrect
lengths, return `#f`. Invalid options, contexts, key encodings, mismatched key
parameters, and destroyed private keys raise errors.

`oid->signer-maker` and `oid->verifier-maker` support all named PQC signature
OIDs. The corresponding constants replace `*signature:` with
`*signature-algorithm:`. PKIX signing fixes an empty context and its named
pure/prehash mode. HashML-DSA-SHA512 signature OIDs have suffixes `.32`
through `.34` under `2.16.840.1.101.3.4.3`; these are not pure ML-DSA
key OIDs. PQC signature AlgorithmIdentifier parameters must be absent.
Use separate keys for different signature modes/protocols.

### [§4] RSA signature specific

These procedures are RSA signature specific, i.e. `:encoder` and `:verifier`
keyword arguments, the keyword arguments specified in these procedures
are passed via either `make-signer` or `make-verifier`.

###### [!Function] `pkcs1-emsa-pss-encode` :key salt (mgf `mgf-1`) mgf-digest

PKCS#1 EMSA PSS encode. The keyword arguments specifies its salt, MGF and
digest algorithm of the MGF.

###### [!Function] `pkcs1-emsa-pss-verify` :key salt-length (mgf `mgf-1`) mgf-digest

PKCS#1 EMSA PSS verify. The keyword arguments specifies its salt, MGF and
digest algorithm of the MGF.

###### [!Function] `pkcs1-emsa-v1.5-encode`

PKCS#1 EMSA PKCS1-v1_5 encode.

###### [!Function] `pkcs1-emsa-v1.5-verify`

PKCS#1 EMSA PKCS1-v1_5 verify.

###### [!Function] `mgf-1`

MGF1 function, the same as the one exported from `(sagittarius crypto ciphers)`.

### [§4] DSA and ECDSA signature specific

###### [!Function] `make-random-k-generator` (_prng_ `random-generator?`)

Creates a random k-generator.

###### [!Function] `make-hmac-k-generator` (_digest_ `digest-descriptor?`)

Creates a RFC 6979 determistic k-generator.

### [§4] Supporting algorithms' OIDs

The below bindings are string representaion of OIDs which can be used
to create X.509 algorithm identifier, i.e. `make-x509-algorithm-identifier`,
described in X.509 library.

###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha1*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha256*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha384*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha512*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha224*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha512/224*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha512/256*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha3-224*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha3-256*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha3-384*`
###### [!Constant] `*signature-algorithm:rsa-pkcs-v1.5-sha3-512*`
###### [!Constant] `*signature-algorithm:rsa-ssa-pss*`
###### [!Constant] `*signature-algorithm:dsa-sha224*`
###### [!Constant] `*signature-algorithm:dsa-sha256*`
###### [!Constant] `*signature-algorithm:dsa-sha384*`
###### [!Constant] `*signature-algorithm:dsa-sha512*`
###### [!Constant] `*signature-algorithm:ecdsa-sha1*`
###### [!Constant] `*signature-algorithm:ecdsa-sha224*`
###### [!Constant] `*signature-algorithm:ecdsa-sha256*`
###### [!Constant] `*signature-algorithm:ecdsa-sha384*`
###### [!Constant] `*signature-algorithm:ecdsa-sha512*`
###### [!Constant] `*signature-algorithm:ecdsa-sha3-224*`
###### [!Constant] `*signature-algorithm:ecdsa-sha3-256*`
###### [!Constant] `*signature-algorithm:ecdsa-sha3-384*`
###### [!Constant] `*signature-algorithm:ecdsa-sha3-512*`
###### [!Constant] `*signature-algorithm:ed25519*`
###### [!Constant] `*signature-algorithm:ed448*`

The name must be descriptive enough to see which algorithm of OID it
represents.
