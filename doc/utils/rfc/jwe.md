[§2] (rfc jwe) - Json Web Encryption {#rfc.jwe}
-------------

###### [!Library] `(rfc jwe)` 

This library provides Json Web Encryption (JWE) APIs. JWE is defined in
[RFC 7516](https://datatracker.ietf.org/doc/html/rfc7516).

The library also supports ChaCha20-Poly1305 and XChaCha20-Poly1305
enhancement defined in 
[the draft RFC](https://tools.ietf.org/html/draft-amringer-jose-chacha-02)

The following example shows how to exchange secret key.

```scheme
(import (rnrs)
        (rfc jwe)
        (rfc jwk))

(define jwk-bob
  (json-string->jwk
   "{\"kty\":\"EC\",
     \"crv\":\"P-256\",
     \"x\":\"weNJy2HscCSM6AEDTDg04biOvhFhyyWvOHQfeF_PxMQ\",
     \"y\":\"e8lnCO-AlStT-NJVX-crhB7QRYhiix03illJOVAOyck\",
     \"d\":\"VEmDZpDXXK8p8N0Cndsxs924q6nS1RXFASRl6BfUqdw\"}"))

(define jwe-header
  (jwe-header-builder
   (alg 'ECDH-ES+A128KW)
   (enc 'A128GCM)
   (apu "QWxpY2U")
   (apv "Qm9i")))

;; Alice wants to encrypt with Bob's public key
(define alice-encryptor (make-ecdh-jwe-encryptor (jwk->public-key jwk-bob)))

;; Bob needs to decrypt Alice's message with his private key
(define bob-decryptor (make-ecdh-jwe-decryptor jwk-bob))

(define secret-key (string->utf8 "down the rabbit hole"))

(let ((jwe-object (jwe:encrypt alice-encryptor jwe-header secret-key)))
  (jwe:serialize jwe-object) ;; -> compact JWE string
  (let ((secret-key (jwe:decrypt bob-decryptor jwe-object)))
    (utf8->string secret-key))) ;; -> "down the rabbit hole"
```

The above is just a taste of how to share a secret key without shared
secret. In the real world application, you should implement your
application more carefully.

### [§3] JWE Object

###### [!Record Type] `<jwe-object>` 

The record type of JWE objects.
JWS object has 5 fields, `header`, `encrypted-key`, `iv`,
`cipher-text` and `authentication-tag`.


###### [!Function] `jwe-object?`  _obj_

Returns #t if the given _obj_ is a JWE object otherwise #f.

###### [!Function] `make-jws-object`  _jwe-header_ _encrypted-key_ _iv_ _cipher-text_ _authentication-tag_

Construct a newly allocated JWE object.

This constructor doesn't validate if the _cipher-text_ can be decrypted
by the _encrypted-key_ as that's not possible.

NOTE: This constructor is not meant be used by users.


###### [!Function] `jwe-object-header`  _jwe-object_

Returns the value of `header` field of given _jwe-object_

###### [!Function] `jwe-object-encrypted-key`  _jwe-object_

Returns the value of `encrypted-key` field of given _jwe-object_

###### [!Function] `jwe-object-iv`  _jwe-object_

Returns the value of `iv` field of given _jwe-object_

###### [!Function] `jwe-object-cipher-text`  _jwe-object_

Returns the value of `cipher-text` field of given _jwe-object_

###### [!Function] `jwe-object-authentication-tag`  _jwe-object_

Returns the value of `authentication-tag` field of given
 _jwe-object_

### [§3] JWE Header

###### [!Record Type] `<jwe-header>` 

The record type of JWE header.

This record type has the below fields:

- `typ`: JWE type, must be a symbol
- `cty`: JWE content type
- `alg`: JWE algorithm, must be a symbol
- `enc`: JWE encryption algorithm, must be a symbol
- `jku`: JWK Set URL
- `jwk`: JWK, must be a JWK object
- `kid`: Key ID
- `x5u`: X.509 certificate URL
- `x5c`: X.509 certiticate chain, must be a list of X.509 certificate
- `x5t`: X.509 certificate SHA-1 Thumbprint, must be a bytevector
- `x5t-s256`: X.509 certificate SHA-256 Thumbprint, must be a bytevector
- `crit`: Critical header parameter, must be a list of string
- `zip`: Compression algorithm
- `p2s`: Salt, must be a bytevector
- `p2c`: Iteration count, must be an integer
- `iv`: Initial vector, must be a bytevector
- `tag`: Authentication tag, must be a bytevector
- `apu`: Agreement party uinfo, must be a bytevector
- `apv`: Agreement party vinfo, must be a bytevector
- `epk`: Ephemeral public key, must be either JSON or JWK object
- `custom-parameters`

The above fields have accessors prefixed _jwe-header-_. For example,
to read `typ` field, you can use `jwe-header-typ` procedure.


Below are the supported encryption algorithms:

- `A128CBC-HS256`
- `A192CBC-HS384`
- `A256CBC-HS512`
- `A128GCM`
- `A192GCM`
- `A256GCM`
- `C20P`  **[@since] `0.9.13`**
- `XC20P` **[@since] `0.9.13`**

###### [!Function] `jwe-header?`  _obj_

Returns #t if the given _obj_ is a JWE header, otherwise #f.

###### [!Macro] `jwe-header-builder`  _(field_ _value)_ _..._

A builder macro of JWE header. The macro is generated by
`(record builder)`. see [(record builder)](#record.builder)for more details.


###### [!Function] `jwe-header->json`  _jwe-header_
###### [!Function] `write-jwe-header`  _jwe-header_
###### [!Function] `write-jwe-header`  _jwe-header_ _port_
###### [!Function] `jwe-header->json-string`  _jwe-header_

Serialize the given _json-header_ to a S-exp representaion,
to _port_ or string.

If first form of `write-jwe-header` is used, then it writes the
serialized JWE header to current output port.


###### [!Function] `json->jwe-header`  _obj_
###### [!Function] `read-jwe-header` 
###### [!Function] `read-jwe-header`  _:optional_ _port_
###### [!Function] `json-string->jwe-header`  _string_

Construct JWE header from S-exp JSON representation of _obj_,
from input port _port_ or a string _string_.

If the first form of `read-jwe-header` is used, then it reads from
current input port.


### [§3] JWE Operations

###### [!Function] `jwe:parse`  _string_

Parse the given compact JWE of _string_ and return JWE object.

If the format of the given _string_ is invalid, then an error is signaled.


###### [!Function] `jwe:serialize`  _jwe-object_

Serialize the given _jwe-object_ to compact JWE form.

###### [!Function] `jwe:encrypt`  _jwe-encryptor_ _jwe-header_ _plain-text_

Returns a JWE object whose `cipher-text` is the encrypted
_payload_.

The _jwe-encryptor_ must be one of the JWE encryptors described below
section.

The _jwe-header_ must be a JWE header object.

The _plain-text_ must be a bytevector.


###### [!Function] `jwe:encrypt`  _jwe-decryptor_ _jwe-object_
###### [!Function] `jwe:encrypt`  _jwe-decryptor_ _jwe-object_ _critical-headers_

Returns decrypted `cipher-text` as a bytevector.

The _jwe-decryptor_ must be one of the JWE decryptors described below
section.

The _jwe-object_ must be a JWE object.

If the second form is used, then the `crit` paramteters of the
`header` will be checked.


### [§3] JWE Encryptors

JWE encryptor is a procedure takes two arguments, JWE header and plain text.

###### [!Function] `make-ecdh-jwe-encryptor`  _key_ **[@deprecated]**
###### [!Function] `make-ecdh-es-jwe-encryptor`  _key_ **[@since] `0.9.13`**

_key_ must be a EC JWK, OKP JWK, EcDSA public key, X25519 public key
or X448 public key.

Creates a ECDH JWE encryptor.

Below are the supported algorithms:
- `ECDH-ES`
- `ECDH-ES+A128KW`
- `ECDH-ES+A198KW`
- `ECDH-ES+A256KW`
- `ECDH-ES+C20PKW` **[@since] `0.9.13`**
- `ECDH-ES+XC20PKW` **[@since] `0.9.13`**

###### [!Function] `make-rsa-jwe-encryptor`  _key_

_key_ must be a RSA JWK or RSA public key.

Creates a RSA JWE encryptor.

Below are the supported algorithms:
- `RSA1_5`
- `RSA-OAEP`
- `RSA-OAEP-256`


###### [!Function] `make-aeskw-jwe-encryptor`  _key_

_key_ must be a OCT JWK or AES secret key.

Creates a AESKW JWE encryptor.

Below are the supported algorithms:
- `A128KW`
- `A192KW`
- `A256KW`
- `A128GCMKW`
- `A192GCMKW`
- `A256GCMKW`

###### [!Function] `make-c20pkw-jwe-encryptor`  _key_ **[@since] `0.9.13`**

_key_ must be a OCT JWK or a symmetric key of size 16 or 32 octet.

Creates a C20PKW or XC20PKW JWE encryptor.

Below are the supported algorithms:
- `C20PKW`
- `XC20PKW`


###### [!Function] `make-pbes2-jwe-encryptor`  _password_

_password_ must be a string or a bytevector.

Creates a PBES2 JWE encryptor.

Below are the supported algorithms:
- `PBES2-HS256+A128KW` 
- `PBES2-HS384+A192KW` 
- `PBES2-HS512+A256KW`


###### [!Function] `make-direct-jwe-encryptor`  _key_

_key_ must be an AES secret key.

Creates a direct JWE encryptor.

This encryptor uses given _key_ as CEK.


### [§3] JWE Decryptors

JWE decryptor is a procedure takes 5 arguments, JWE header, encrypted key,
IV, cipher text and authentication tag.

###### [!Function] `make-ecdh-jwe-decryptor`  _key_ **[@deprecated]**
###### [!Function] `make-ecdh-es-jwe-decryptor`  _key_ **[@since] `0.9.13`**

_key_ must be a EC JWK, OKP JWK, EcDSA private key, X25519 private key
or X448 private key.

Creates a ECDH JWE decryptor.

Below are the supported algorithms:
- `ECDH-ES`
- `ECDH-ES+A128KW`
- `ECDH-ES+A198KW`
- `ECDH-ES+A256KW`
- `ECDH-ES+C20PKW` **[@since] `0.9.13`**
- `ECDH-ES+XC20PKW` **[@since] `0.9.13`**


###### [!Function] `make-rsa-jwe-decryptor`  _key_

_key_ must be a RSA JWK or RSA private key.

Creates a RSA JWE decryptor.

Below are the supported algorithms:
- `RSA1_5`
- `RSA-OAEP` 
- `RSA-OAEP-256`


###### [!Function] `make-aeskw-jwe-decryptor`  _key_

_key_ must be a OCT JWK or AES secret key.

Creates a AESKW JWE decryptor.

Below are the supported algorithms:
- `A128KW`
- `A192KW`
- `A256KW`
- `A128GCMKW`
- `A192GCMKW`
- `A256GCMKW`

###### [!Function] `make-c20pkw-jwe-decryptor`  _key_  **[@since] `0.9.13`**

_key_ must be a OCT JWK or a secret key of size 16 or 32 octed.

Creates a C20PKW or XC20PKW JWE decryptor.

Below are the supported algorithms:
- `C20PKW`
- `XC20PKW`


###### [!Function] `make-pbes2-jwe-decryptor`  _password_

_password_ must be a string or a bytevector.

Creates a PBES2 JWE decryptor.

Below are the supported algorithms:
- `PBES2-HS256+A128KW` 
- `PBES2-HS384+A192KW` 
- `PBES2-HS512+A256KW`


###### [!Function] `make-direct-jwe-decryptor`  _key_

_key_ must be an AES secret key.

Creates a direct JWE decryptor.

This decryptor uses given _key_ as CEK.


