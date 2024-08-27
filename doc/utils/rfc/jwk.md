[§2] (rfc jwk) - Json Web Key {#rfc.jwk}
-------------

###### [!Library] `(rfc jwk)` 

This library provides Json Web Key (JWK) APIs. JWS is defined in
[RFC 7517](https://datatracker.ietf.org/doc/html/rfc7517).

This library also supports OKP defined in
[RFC 8037](https://datatracker.ietf.org/doc/html/rfc8037).

This library also supports curve `secp256k1`
[RFC 8812](https://datatracker.ietf.org/doc/html/rfc8812),
as well as the `P-256K` which is defined in the draft 00 of the RFC.


The following examples show how to interact with keys from 
`(sagittarius crypto keys)` library.

```scheme
;; (sagittarius crypto keys) keys to JWK/JWKS
(import (rnrs)
        (sagittarius crypto keys)
        (rfc jwk))

(define keypair (generate-key-pair *key:ed25519*))

(define private-key (key-pair-private keypair))

(define jwk-config (jwk-config-builder (kid "my key id")))

(let ((jwks (make-jwk-set (list (key->jwk private-key jwk-config)))))
  (jwk-set->json-string jwks) ;; -> {"keys":[{"kid":"my key id",...}]}
  (jwk-set:find-key jwks (jwk-matcher:kid "my key id")) ;; -> #<jwk>
  (jwk-set->public-jwk-set jwks)) ;; -> #<jwk-set> contains only public key
```

```scheme
;; JWK/JWKS to (sagittarius crypto keys) key
(import (rnrs)
        (sagittarius crypto keys)
        (rfc jwk))

;; JWKS with EC private key
(define jwks-json
  #(("keys"
     #(("kty" . "EC")
       ("crv" . "P-256")
       ("x"   . "MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4")
       ("y"   . "4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM")
       ("d"   . "870MB6gfuTJ4HtUnUvYMyJpr5eUZNP4Bk43bVdj3eAE")
       ("use" . "enc")
       ("kid" . "1")))))
(define jwks (json->jwk-set jwks-json))
(define kid-matcher (jwk-matcher:kid "1"))

(define jwk (jwk-set:find-key jwks kid-matcher))

(jwk->public-key jwk)   ;; -> ECDSA public key
(jwk->private-key jwk)  ;; -> ECDSA private key
```

### [§3] JWK Set

JWK Set (JWKS) is an object represents a set of JWKs.

###### [!Function] `jwk-set?`  _obj_

Returns #t if the given _obj_ is a JWKS object otherwise #f.

###### [!Function] `make-jwk-set`  _keys_

Construct a newly allocated JWKS object whose keys are _keys_.

_keys_ must be a list of JWK objects.


###### [!Function] `jwk-set-keys`  _jwk-set_

Retrieves a list of JWKs from the given _jwk-set_.

###### [!Function] `json->jwk-set`  _obj_
###### [!Function] `read-jwk-set` 
###### [!Function] `read-jwk-set`  _port_
###### [!Function] `json-string->jwk-set`  _string_

Construct JWKS from S-exp JSON representation of _obj_,
from input port _port_ or a string _string_.

If the first form of `read-jwk-set` is used, then it reads from
current input port.


###### [!Function] `jwk-set->json`  _jwk-set_
###### [!Function] `write-jwk-set`  _jwk-set_
###### [!Function] `write-jwk-set`  _jwk-set_ _port_
###### [!Function] `jwk-set->json-string`  _jwk-set_

Serialize the given _jwk-set_ to a S-exp representaion,
to _port_ or string.

If first form of `write-jwk-set` is used, then it writes the
serialized JWK set to current output port.


###### [!Function] `jwk-set:find-key`  _jwk-set_ _jwk-matcher_

Finds a key which matches to _jwk-matcher_ from given _jwk-set_.

### [§3] JWK Matcher

A JWK matcher is a procedure takes one argument, _jwk_, and returns
the given _jwk_ if it matches the condition otherwise returns #f.

The matchers provided by this library complies to the above so that
users can compose matchers like this:

```scheme
(import (rnrs)
        (rfc jwk)
        (sagittarius combinators))

(define kid/alg-matcher
 (compose (jwk-matcher:kid "kid") (jwk-matcher:alg 'EdDSA)))
```

###### [!Function] `jwk-matcher:kty`  _obj_
###### [!Function] `jwk-matcher:use`  _obj_
###### [!Function] `jwk-matcher:alg`  _obj_
###### [!Function] `jwk-matcher:kid`  _obj_
###### [!Function] `jwk-matcher:x5t`  _obj_
###### [!Function] `jwk-matcher:x5t-s256`  _obj_
###### [!Function] `jwk-matcher:crv`  _obj_

Creates a JWK matcher which checks `kty`, `use`, `alg`,
`kid`, `x5t`, `x5t-s256` or `crv` field of the
target JWK is equal to _obj_, respectively.

###### [!Function] `jwk-matcher:key-ops`  _obj_

Creates a JWK matcher which checks `key-ops` field of the
target JWK contains given _obj_.

###### [!Function] `jwk-matcher:rsa`  _jwk_
###### [!Function] `jwk-matcher:ec`  _jwk_
###### [!Function] `jwk-matcher:oct`  _jwk_
###### [!Function] `jwk-matcher:okp`  _jwk_

Convenient JWK matchers which check `kty` to be `RSA`,
`EC`, `oct` or `OKP`, respectively.

### [§3] JWK

JWK is an object which contains key information. The object contains
the following fields:

- `kty`: key type, symbol
- `use`: key usage, symbol, must be either `sig` or `enc`
- `key-ops`: key operation, a list of symbols
- `alg`: key algorithm, symbol
- `kid`: key ID
- `x5u`: URL of certificate
- `x5c`: Certificate chain, list of x509 certificate
- `x5t`: SHA-1 certificate finger print, bytevector
- `x5t-s256`: SHA-256 certificate finger print, bytevector

###### [!Function] `jwk?`  _obj_

Returns #t if the given _obj_ is a JWK object otherwise #f.

###### [!Function] `jwk-kty`  _jwk_
###### [!Function] `jwk-use`  _jwk_
###### [!Function] `jwk-key-ops`  _jwk_
###### [!Function] `jwk-alg`  _jwk_
###### [!Function] `jwk-kid`  _jwk_
###### [!Function] `jwk-x5u`  _jwk_
###### [!Function] `jwk-x5c`  _jwk_
###### [!Function] `jwk-x5t`  _jwk_
###### [!Function] `jwk-x5t-s256`  _jwk_

Retrieves the field value of _jwk_.

###### [!Function] `json->jwk`  _obj_
###### [!Function] `read-jwk` 
###### [!Function] `read-jwk`  _:optional_ _port_
###### [!Function] `json-string->jwk`  _string_

Construct JWK from S-exp JSON representation of _obj_,
from input port _port_ or a string _string_.

If the first form of `read-jwk` is used, then it reads from
current input port.


###### [!Function] `jwk->json`  _jwk_
###### [!Function] `write-jwk`  _jwk_
###### [!Function] `write-jwk`  _jwk_ _port_
###### [!Function] `jwk->json-string`  _jwk_

Serialize the given _jwk_ to a S-exp representaion,
to _port_ or string.

If first form of `write-jwk` is used, then it writes the
serialized JWK to current output port.


#### [§4] From JWK Conversion

The below conversion procedures raise an error if the conversion is
not possible. For example, key type `oct` can't be public key.

###### [!Function] `jwk->public-key`  _jwk_
###### [!Function] `jwk->private-key`  _jwk_

Convert given _jwk_ to `(sagittarius crypto keys)` public key and private key,
respectively.

###### [!Function] `jwk->octet-key`  _jwk_

Convert given _jwk_ to octet key bytevector.

###### [!Function] `jwk->public-jwk`  _jwk_

Convert given _jwk_ to JWK which only contains public key
information.

#### [§4] To JWK Conversion

###### [!Function] `jwk:config?`  _obj_

Returns #t if the given _obj_ is a JWK config object otherwise #f.

JWK may contain meta data, such as `kid`, to provide the information,
users can use JWK config object. The object has the below fields:

- `use`: key usage, symbol, must be either `sig` or `enc`
- `kid`: key ID
- `key-ops`: key operation, a list of symbols
- `alg`: key algorithm, symbol
- `x5u`: URL of certificate
- `x5c`: Certificate chain, list of x509 certificate
- `e`: RSA public key exponent

`e` is provided due to the historical reason of not to have public
exponent in non CRT RSA private key. By default, the value is 65537.


###### [!Macro] `jwk-config-builder` 

A builder macro of JWK config. The macro is generated by
`(record builder)`. see [(record builder)](#record.builder)for more details.


###### [!Function] `jwk->jwk-config`  _jwk_

Construct JWK config from given _jwk_.

###### [!Function] `key->jwk`  _key_
###### [!Function] `key->jwk`  _key_ _jwk-config_

Converts given _key_ to JWK object.

If the second form is used, then the returning JWK contains the configured
information.

The _key_ must be one of public key, private key, or secret key of
`(sagittarius crypto keys)`.


