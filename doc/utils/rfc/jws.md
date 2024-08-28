[§2] (rfc jws) - Json Web Signature {#rfc.jws}
-------------

###### [!Library] `(rfc jws)` 

This library provides Json Web Signature (JWS) APIs. JWS is defined
in [RFC 7515](https://tools.ietf.org/html/rfc7515).

The library supports `ES256K` defined in
[RFC 8812](https://datatracker.ietf.org/doc/html/rfc8812)section 3.1.

It also supports EdDSA of both Ed25519 and Ed448 defined in
[RFC 8037](https://datatracker.ietf.org/doc/html/rfc8037).


The following examples show how to sign and verify JWS token

```scheme
;; Signing
(import (rnrs)
        (rfc jws)
        (rfc jwk) ;; to convert key pair to JWK
        (sagittarius crypto keys))

;; Generate Ed25519 key pair
(define keypair (generate-key-pair *key:ed25519*))

;; JWS header with crit
(define jws-header
  (jws-header-builder
   (alg 'EdDSA)
   (crit '("sagittarius:iss" "sagittarius:sub"))
   (custom-parameters '(("sagittarius:iss" "Sagittarius Scheme")
                        ("sagittarius:sub" "JWS test")))))

(define payload
  (string->utf8 "Payload can be anything as long as a bytevector"))

;; Get signer
(define signer (private-key->jws-signer (key-pair-private keypair)))

;; The JWS object is not signed yet
(define unsecure-jws-object (make-jws-object jws-header payload))

(let ((jws-object (jws:sign unsecure-jws-object signer)))
  (jws:serialize jws-object)
  ;; -> eyJzYWdpdHRhcml1czpzdWIiOiJKV1MgdGVzdCIsInNhZ2l0dGFyaXVzOmlzcyI6IlNhZ2l0dGFyaXVzIFNjaGVtZSIsImFsZyI6IkVkRFNBIiwiY3JpdCI6WyJzYWdpdHRhcml1czppc3MiLCJzYWdpdHRhcml1czpzdWIiXX0.UGF5bG9hZCBjYW4gYmUgYW55dGhpbmcgYXMgbG9uZyBhcyBhIGJ5dGV2ZWN0b3I.5Aj_AJh4DW01kV80XtFbxRRMw2ktxIrQ5-UXoCwKVWI0Ke0q0t3vpcFnESL39zYDwi3Ps8eLxfmEb-TvhkQGBg
  (jwk->json-string (public-key->jwk (key-pair-public keypair)))
  ;; -> {"kty":"OKP","crv":"Ed25519","x":"o_t1R4fWf7obqTZWlXxrgPG09BMU-zuhqHvb9_ayOew"}
  )
```

```scheme
;; Verify
(import (rnrs)
        (rfc jws)
        (rfc jwk)) ;; to convert JWK to public key

;; JWS string
(define jws-string
  "eyJzYWdpdHRhcml1czpzdWIiOiJKV1MgdGVzdCIsInNhZ2l0dGFyaXVzOmlzcyI6IlNhZ2l0dGFyaXVzIFNjaGVtZSIsImFsZyI6IkVkRFNBIiwiY3JpdCI6WyJzYWdpdHRhcml1czppc3MiLCJzYWdpdHRhcml1czpzdWIiXX0.UGF5bG9hZCBjYW4gYmUgYW55dGhpbmcgYXMgbG9uZyBhcyBhIGJ5dGV2ZWN0b3I.5Aj_AJh4DW01kV80XtFbxRRMw2ktxIrQ5-UXoCwKVWI0Ke0q0t3vpcFnESL39zYDwi3Ps8eLxfmEb-TvhkQGBg")

;; Ed25519 public key
(define jwk
  (json-string->jwk
   "{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"x\":\"o_t1R4fWf7obqTZWlXxrgPG09BMU-zuhqHvb9_ayOew\"}"))

;; Parse the string
(define jws-object (jws:parse jws-string))

;; Make a verifier from the JWK
(define verifier (public-key->jws-verifier jwk))

(when (jws:verify jws-object verifier '("sagittarius:iss" "sagittarius:sub"))
  (utf8->string (jws-object-payload jws-object))) ;; -> payload
```

The above examples only show the flavour of the APIs. In real world
applications, the users of the library must consider a lot more to
make the application secure.

### [§3] JWS Object

###### [!Record Type] `<jws-object>` 

The record type of JWS objects.
JWS object has 2 fields, `header` and `payload`.


###### [!Function] `jws-object?`  _obj_

Returns #t if the given _obj_ is a JWS object otherwise #f.

###### [!Function] `make-jws-object`  _jws-header_ _payload_

Construct a newly allocated JWS object.

_Jws-header_ must be a JWS header object.

_payload_ must be a bytevector.


###### [!Function] `jws-object-header`  _jws-object_

Returns the value of `header` field of given _jws-object_.

###### [!Function] `jws-object-payload`  _jws-object_

Returns the value of `payload` field of given _jws-object_.

###### [!Record Type] `<jws-signed-object>` 

The record type of signed JWS objects.

Signed JWS object is a sub record of JWS object, which has `signature`field.


###### [!Function] `jws-signed-object?`  _obj_

Returns #t if the given _obj_ is a signed JWS object otherwise #f.

This object can be obtained by `jws:sign` or `jws:parse`.


###### [!Function] `jws-signed-object-signature`  _signed-jws-object_

Returns the value of `signature` field of
given _signed-jws-object_.

### [§3] JWS Header

###### [!Record Type] `<jws-header>` 

The record type of JWS header.

This record type has the below fields:

- `typ`: JWS type, must be a symbol
- `cty`: JWS content type
- `alg`: JWS algorithm, must be a symbol
- `jku`: JWK Set URL
- `jwk`: JWK, must be a JWK object
- `kid`: Key ID
- `x5u`: X.509 certificate URL
- `x5c`: X.509 certiticate chain, must be a list of X.509 certificate
- `x5t`: X.509 certificate SHA-1 Thumbprint, must be a bytevector
- `x5t-s256`: X.509 certificate SHA-256 Thumbprint, must be a bytevector
- `crit`: Critical header parameter, must be a list of string
- `custom-parameters`

The above fields have accessors prefixed _jws-header-_. For example,
to read `typ` field, you can use `jws-header-typ` procedure.


###### [!Function] `jws-header?`  _obj_

Returns #t if the given _obj_ is a JWS header, otherwise #f.

###### [!Macro] `jws-header-builder`  _(field_ _value)_ _..._

A builder macro of JWS header. The macro is generated by
`(record builder)`. see [(record builder)](#record.builder)for more details.


###### [!Function] `jws-header->json`  _jws-header_
###### [!Function] `write-jws-header`  _jws-header_
###### [!Function] `write-jws-header`  _jws-header_ _port_
###### [!Function] `jws-header->json-string`  _jws-header_

Serialize the given _json-header_ to a S-exp representaion,
to _port_ or string.

If first form of `write-jws-header` is used, then it writes the
serialized JWS header to current output port.


###### [!Function] `json->jws-header`  _obj_
###### [!Function] `read-jws-header` 
###### [!Function] `read-jws-header`  _:optional_ _port_
###### [!Function] `json-string->jws-header`  _string_

Construct JWS header from S-exp JSON representation of _obj_,
from input port _port_ or a string _string_.

If the first form of `read-jws-header` is used, then it reads from
current input port.


### [§3] JWS Operations

###### [!Function] `jws:parse`  _string_

Parse the given compact JWS of _string_ and return signed JWS object.

If the format of the given _string_ is invalid, then an error is signaled.


###### [!Function] `jws:serialize`  _jws-object_
###### [!Function] `jws:serialize`  _jws-object_ _detach-payload?_

Serialize the given _jws-object_ to compact JWS form.

If the second form is used, then the payload is omitted. (Detached form)


###### [!Function] `jws:sign`  _jws-object_ _signer_

Sign the given _jws-object_ with the given _signer_ and returns
signed JWS object.


###### [!Function] `jws:verify`  _jws-object_ _verifier_
###### [!Function] `jws:verify`  _jws-object_ _verifier_ _critical-headers_

Verify the given _jws-object_ with the given _verifier_.
If the verification is success, then it returns #t.

Otherwise, it may return #f or signals an error, depending on the
underlying verifier.

If the second form of `jws:verify` is used, then it uses the given
_critical-headers_ list to check `crit` header value.


### [§3] JWS Verifiers

JWS verifier is a procedure takes 3 arguments, JWS header, signing content
and signature.

This library doesn't support `none` algorithm verifier. It is obvious
that if you want to support it, you just don't have to verify it.

###### [!Function] `make-mac-jws-verifier`  _key_

_key_ must be a JWK:oct or a bytevector.

Creates a MAC JWS verifier.


###### [!Function] `make-rsa-jws-verifier`  _key_

_key_ must be a JWK:RSA or a RSA public key.

Creates a RSA JWS verifier.


###### [!Function] `make-ecdsa-jws-verifier`  _key_

_key_ must be a JWK:EC or a ECDSA public key.

Creates a ECDSA JWS verifier.


###### [!Function] `make-eddsa-jws-verifier`  _key_

_key_ must be a JWK:OKP or a EdDSA public key.

Creates a EdDSA JWS verifier.


### [§3] JWS Signers

JWS signer is a procedure takes 2 arguments, JWS header, signing content.

This library doesn't support `none` algorithm signer. If you want to
support it, you need to create a JWS object with `alg` header with
value of `none`.

###### [!Function] `make-mac-jws-signer`  _key_

_key_ must be a JWK:oct or a bytevector.

Creates a MAC JWS signer.


###### [!Function] `make-rsa-jws-signer`  _key_

_key_ must be a JWK:RSA or a RSA private key.

Creates a RSA JWS signer.


###### [!Function] `make-ecdsa-jws-signer`  _key_

_key_ must be a JWK:EC or a ECDSA private key.

Creates a ECDSA JWS signer.


###### [!Function] `make-eddsa-jws-signer`  _key_

_key_ must be a JWK:OKP or a EdDSA private key.

Creates a EdDSA JWS signer.


