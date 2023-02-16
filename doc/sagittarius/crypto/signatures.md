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

_opts_ must be a keyword arguments, the procedure ignores unsupported
keywords. The supporting keyword arguments and its scheme are below

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

_opts_ must be a keyword arguments, the procedure ignores unsupported
keywords. The supporting keyword arguments and its scheme are below

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
