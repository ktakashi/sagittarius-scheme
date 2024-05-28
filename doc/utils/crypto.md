[§2] (crypto) - Cryptographic library {#crypto}
-------------

### Deprecated library

**This library is deprecated, please consider to use
[Cryptographic libraries](#lib.sagittarius.crypto) instead.**

This documentation does not describe cryptography itself. For example, it does
not describe what initial vector is and how long it must be. So users must know
about cryptography's this library supports.

This library uses `libtomcrypt`'s functionalities. The library is public
domain. Thank you for the great library.

Note: the `libtomcrypt` is a huge cryptographic library and I am not so 
good with cryptographics, so the `(crypto)` library is not totally tested.
Just the functionalities which I usually use are tested. If you find a bug or
wrong documentation, pleas report it.

###### [!Library] `(crypto)` **[@deprecated]**

This library is the top most library, it exports all the other libraries
procedures. Users must import only this and not to use the others.

`(crypto)` library supports both symmetric cryptography and public/private
key mechanism. For public/private key, it only supports RSA for now.


###### [!Function] `crypto-object?`  _obj_

Returns #t if _obj_ is `crypto-object`.

`crypto-object` can be either `cipher` or `key`.


### [§3] Cipher operations

###### [!Function] `make-cipher`  _type_ _key_ _mode_ _:key_ _(mode-parameter_ _#f)_ _:allow-other-keys_ _:rest_ _options_

Creates a cipher object.

_type_ must be known cryptographic algorithm. Currently, `(crypto)`library exports the algorithm below.

The symmetric key algorithms.
###### [!Constant] `Blowfish` 
###### [!Constant] `X-Tea` 
###### [!Constant] `RC2` 
###### [!Constant] `RC5-32/12/b` 
###### [!Constant] `RC6-32/20/b` 
###### [!Constant] `SAFER+` 
###### [!Constant] `SAFER-K64` 
###### [!Constant] `SAFER-SK64` 
###### [!Constant] `SAFER-K128` 
###### [!Constant] `SAFER-SK128` 
###### [!Constant] `AES`
###### [!Constant] `AES-128`
###### [!Constant] `AES-192`
###### [!Constant] `AES-256`
###### [!Constant] `Twofish` 
###### [!Constant] `DES` 
###### [!Constant] `DES3` 
###### [!Constant] `DESede` 
###### [!Constant] `CAST5` 
###### [!Constant] `CAST-128` 
###### [!Constant] `Noekeon` 
###### [!Constant] `Skipjack` 
###### [!Constant] `Khazad` 
###### [!Constant] `SEED` 
###### [!Constant] `KASUMI` 
###### [!Constant] `Camellia`

`AES-128`, `AES-192` and `AES-256` are fixed key size AES algorithms.
`AES` allows key size of 16, 24 or 32 bytes, however, fixed sized
version only accepts 16 for `AES-128`, 24 for `AES-192` and 32 for
`AES-256`.

The public key algorithm
###### [!Constant] `DSA` 
###### [!Constant] `ECDSA` 
###### [!Constant] `RSA` 
_key_ must be a key object which will be created by key generate procedures
described below.

_mode_ specifies the **symmetric cipher**'s encryption and description
mode. If the cipher type is public key cipher, it will be ignored. Some modes
require initial vector _iv_. The possible mods are below.
###### [!Constant] `MODE_ECB` 
###### [!Constant] `MODE_CBC` 
###### [!Constant] `MODE_CFB` 
###### [!Constant] `MODE_OFB` 
###### [!Constant] `MODE_CTR` 
###### [!Constant] `MODE_GCM` 
The keyword argument _mode-parameter_ is specified, it must be a mode
parameter object, then the object will be parsed properly by the cipher.


###### [!Function] `cipher-keysize`  _cipher_ _test_

Returns given cipher type's recommended keysize.

_cipher_ must cipher object created by `cipher` procedure.

_test_ must be fixnum.

If _test_ is too small for the cipher, it will raise an error.

Note: this procedure is for helper. It is designed to use check keysize you want
to use.


###### [!Function] `cipher?`  _obj_

Returns #t if the _obj_ is cipher object.

###### [!Function] `cipher-encrypt`  _cipher_ _pt_

_cipher_ must be a cipher object.

_pt_ must be a bytevector.

`encrypt` encrypts given plain text _pt_ according to the given
_cipher_.


###### [!Function] `cipher-encrypt/tag`  _cipher_ _pt_ _:key_ _tag-size_

Similar with `cipher-encrypt`. The difference is that this
procedure returns 2 values, encrypted _pt_ and calculated tag if the
given _cipher_ supports authentication.

If the keyword argument _tag-size_ is specified and must be an integer,
then the returning tag has the specified size. If the _cipher_ supports
less size than the specified size, then the remaining tag contains unspecified
value. To retrieve the maximum length of tag, use `cipher-max-tag-size`.


###### [!Function] `cipher-decrypt`  _cipher_ _ct_

_cipher_ must be a cipher object.

_ct_ must be a bytevector.

`decrypt` decrypts given encrypted text _ct_ according to the given
_cipher_.


###### [!Function] `cipher-decrypt/tag`  _cipher_ _ct_ _:key_ _tag-size_

Similar with `cipher-decrypt`. The difference is that this
procedure returns 2 values, decrypted _ct_ and calculated tag if the
given _cipher_ supports authentication.

If the keyword argument _tag-size_ is specified and must be an integer,
then the returning tag has the specified size. If the _cipher_ supports
less size than the specified size, then the remaining tag contains unspecified
value. To retrieve the maximum length of tag, use `cipher-max-tag-size`.


###### [!Function] `cipher-decrypt/verify`  _cipher_ _ct_ _tag_

Similar with `cipher-decrypt`. The difference is that this
procedure validates the calculated tag. If the given _tag_ and
calculated tag are not the same, then `&decrypt` is raised. 

NOTE: if the tag is \*not\* calculated by the cipher, then this procedure
always raises an error.


###### [!Function] `cipher-max-tag-size`  _cipher_

Returns the maximum length of tag size.

If the given _cipher_ does not support authentication tag, then
returning value is `0`.


###### [!Function] `cipher-update-aad!`  _cipher_ _aad_

Updates the additional authentication data.

###### [!Function] `cipher-tag!`  _cipher_ _dst_

_dst_ must be a bytevector.

Retrieves the authentication tag from given _cipher_.

NOTE: this procedure must be called after either `cipher-encrypt` or
`cipher-decrypt`. Otherwise the filled value is unspecified.


###### [!Function] `cipher-tag`  _cipher_ _:key_ _size_

Retrieves the authentication tag from given _cipher_.

If the keyword argument _size_ is specified, then the returning
bytevector has the _size_ length, otherwise maximum tag length.

NOTE: this procedure must be called after either `cipher-encrypt` or
`cipher-decrypt`. Otherwise the filled value is unspecified.


###### [!Function] `cipher-signature`  _public-cipher_ _data_ _:optional_ _opt_

_public-cipher_ must be a cipher object created with public/private
key algorithm.

_data_ must be a bytevector.

Signs given _data_. This procedure is just a wrapper for the real
implementations. Currently Sagittarius supports only RSA sign.

_opt_ can specify the signer behaviour. Default supported RSA cipher can
accept keyword argument _encode_.

_encode_ specifies the encoder. The default encoder is
`pkcs1-emsa-pss-encode`. And the rest keyword arguments will be passed to
the encoder. Supported encoders are described below.

The following shows how to implement `SHA256WithRSA` signature

``````````scheme
;; Importing a RSA private key.
(define private-key (import-private-key RSA #vu8(...)))

(define signer (make-cipher RSA private-key))
(cipher-signature signer #vu8() :encode pkcs1-emsa-v1.5-encode :hash SHA-256)
``````````



###### [!Function] `cipher-verify`  _public-cipher_ _M_ _S_ _:optional_ _opt_

_public-cipher_ must be a cipher object created with public/private
key algorithm.

_M_ and _S_ must be bytevectors.

_M_ is master message which will be compared with encoded message.

_S_ is signed message.

The `verity` procedure verifies two messages.

_opt_ can specify the verifier behaviour. Default supported RSA cipher can
accept keyword argument _verify_.

_verify_ specifies the verifier. The defaule verifier is
`pkcs1-emsa-pss-verify`. And the rest keyword arguments will be passed to
verifier. Supported verifiers are described below.


### [§3] Mode parameters

Mode parameters are pareters which can be used by specified mode. For example,
`iv-parameter` is a parameter contains an IV.

###### [!Function] `mode-parameter?`  _obj_

Returns #t if the given _obj_ is a mode parameter, otherwise #f.

###### [!Function] `make-composite-parameter`  _parameters_ _..._

Creates a composite mode parameter.

A composite parameter can hold multiple parameters.


###### [!Record Type] `<iv-paramater>` 

Record type for initial vector (IV) parameter.

###### [!Function] `make-iv-paramater`  _iv_

_iv_ must be a bytevector or #f. 

Creates an IV mode parameter.

IV is required by some modes. e.g. `MODE_CBC`.


###### [!Record Type] `<ctr-paramater>` 

Record type for counter mode parameter.

This parameter is a subclass of `<iv-parameter>`.


###### [!Function] `make-ctr-paramater`  _iv_ _:key_ _(rounds_ _0)_ _(mode_ _CTR_COUNTER_BIG_ENDIAN)_

Creates a counter mode parameter. This is used by `MODE_CTR`.

_iv_ is passed to parent constructor.

The followings are description of keyword parameters.

_rounds_ specify how many times the cipher rounds the key.

_ctr-mode_ specifies counter mode. The possible mode is blow.
###### [!Constant] `CTR_COUNTER_LITTLE_ENDIAN` 
###### [!Constant] `CTR_COUNTER_BIG_ENDIAN` 


###### [!Record Type] `<rfc3686-paramater>` 

Record type for AES-CTR mode parameter defined in RFC 3686.

This parameter is a subclass of `<ctr-parameter>`.


###### [!Function] `make-rfc3686-paramater`  _iv_ _nonce_ _:key_ _(rounds_ _0)_ _(mode_ _CTR_COUNTER_BIG_ENDIAN)_

Creates RFC3686 mode parameter.

###### [!Record Type] `<padding-paramater>` 

Record type for padding parameter.

###### [!Function] `make-padding-paramater`  _padder_

Creates a padding mode parameter.

_padder_ must be an procedure such as `pkcs5-padder`.


### [§3] Key operations

###### [!Generic] `generate-secret-key`  _(type_ _<string>)_ _(key_ _<bytevector>)_

_type_ must be one of the supported symmetric algorithm.

_key_ must be a bytevector and its length must satisfy the keysize which
the given algorithm requires.

Returns a sercret key object.


###### [!Generic] `generate-key-pair`  _(type_ _<top>)_ _._ _options_

_type_ is for despatch. For default implementation, it must be
`RSA`.

Generates a key pair object.

Default implementation supports RSA key geneartion and _options_ can be
keyword arguments described below.

_size_ keyword argument is decides key length. Default value is 1024.

_prng_ keyword argument is given, it will be passed to `random-prime`.
For more detail, see [(math random)](#math.random) library. Default
value is `(secure-random RC4)`.

_e_ keyword argument is an exponent. Usually it does not have to be
specified with other number. Default value is 65537.


###### [!Generic] `generate-private-key`  _type_ _._ _options_

_type_ is for despatch. For default implementation, it must be
`RSA`.

Returns private key object.

Default RSA implementation _options_ can be these arguments.

modulus
: The private key's modulus

private-exponent
: The private key's exponent

public-exponent
: keyword argument. Used for CRT private key object.

p
: keyword argument. Used for CRT private key object.

q
: keyword argument. Used for CRT private key object.



###### [!Function] `generate-public-key`  _type_ _:optional_ _opt_

_type_ is for despatch. For default implementation, it must be
`RSA`.

Returns public key object.

Default RSA implementation _opt_ can be these arguments.

modulus
: The public key's modulus

exponent
: The public key's exponent



###### [!Function] `keypair?`  _obj_

Returns #t if given _obj_ is keypair object, otherwise #f

###### [!Function] `keypair-private`  _keypair_

Returns private key from _keypair_

###### [!Function] `keypair-public`  _keypair_

Returns public key from _keypair_

###### [!Function] `key?`  _obj_

Returns #t if given _obj_ is key object, otherwise #f

###### [!Class] `<private-key>` 

CLOS class of private key object.

###### [!Class] `<public-key>` 

CLOS class of public key object.

###### [!Function] `private-key?`  _obj_

Returns #t if given _obj_ is private key object, otherwise #f

###### [!Function] `public-key?`  _obj_

Returns #t if given _obj_ is public key object, otherwise #f

###### [!Function] `split-key`  _key_ _:optional_ _(count_ _3)_ _(prng_ _(secure-random_ _RC4))_

_key_ must be a bytevector and plain key.

Splits the given _key_ to _count_ components and returns _count_values as key components.

The return values might be different each time.


###### [!Function] `combine-key-components`  _component1_ _components_ _..._
###### [!Function] `combine-key-components!`  _result_ _component1_ _components_ _..._

Renaming export of `bytevector-xor` and `bytevector-xor!`respectively.

For more detail, see [(util bytevector)](#util.bytevector).


### [§3] PKCS operations

The procedures described in this section is implemented according to PKCS#1. I
don't have any intend to describe functionality. If you need to know what
exactly these procedures do, please see the PKCS#1 document.

###### [!Function] `pkcs5-padder`  _bv_ _block-size_ _padding?_

_bv_ must be a bytevector.

_block-size_ must be a non negative exact integer.

_padding?_ must be a boolean.

Pads or Unpads paddings from _bv_ according to PKCS#5.

If _padding?_ is #t, the procedure will pad. otherwise it will unpad.


###### [!Function] `pkcs-v1.5-padding`  _prng_ _key_ _block-type_

_prng_ must be prng object. See [(math random)](#math.random).

_key_ must be either private or public key object.

_block-type_ must be one of these.
###### [!Constant] `PKCS-1-EME` 
###### [!Constant] `PKCS-1-EMSA` 
Returns a padding procedure. The procedure signature is the same as
_pkcs5-padder_.


###### [!Function] `pkcs1-emsa-pss-encode`  _m_ _em-bits_ _
_ _:key_ _(hash_ `(hash-algorithm SHA-1)` _)_ _(mgf_ _mgf-1)_ _(salt-length_ _#f)_ _
_ _(prng_ `(secure-random RC4)` _)_

_m_ must be a bytevector.

_em-bits_ must be non negative exact integer.

Encodes given message _m_ according to the PKCS#1 section 9.1.1.

The keyword arguments specified some behaviour.

_hash_ specifies the hash algorithm. For more detail, see
[(math hash)](#math.hash) library.

_mgf_ specifies mask generation procedure. 

Note: PKCS#1 only specifies MGF-1.

_salt-length_ specifies salt's length. If it's #f encoder does not use salt.

_prng_ is a pseudo random see [(math random)](#math.random).


###### [!Function] `pkcs1-emsa-pss-verify`  _m_ _em-bits_ _
_ _:key_ _(hash_ `(hash-algorithm SHA-1)` _)_ _(mgf_ _mgf-1)_ _
_ _(prng_ `(secure-random RC4)` _)_

_m_ must be a bytevector.

_em-bits_ must be non negative exact integer.

Verify given message _m_ according to the PKCS#1 section 9.1.1.

Other keyword arguments are the same as `pkcs1-emsa-pss-encode`.


###### [!Function] `mgf-1`  _mgf-seed_ _mask-length_ _hasher_

_mgf-seed_ must be a bytevector.

_mask-length_ must be a non negative exact integer.

_hasher_ must be a hash algorithm. See
[(math random)](#math.random).

Creates a mask bytevector, according to PKCS#1 MGF-1.


###### [!Function] `pkcs1-emsa-v1.5-encode`  _m_ _em-bits_ _:key_ _(hash_ `(hash-algorithm SHA-1)` _)_

_m_ must be a bytevector.

_em-bits_ must be non negative exact integer.

Encodes given message _m_ according to the PKCS#1 section 9.2.

Other keyword arguments are the same as `pkcs1-emsa-pss-encode`.


###### [!Function] `pkcs1-emsa-v1.5-verify`  _m_ _em-bits_ _:key_ _(hash_ `(hash-algorithm SHA-1)` _)_

_m_ must be a bytevector.

_em-bits_ must be non negative exact integer.

Verify given message _m_ according to the PKCS#1 section 9.2.
Other keyword arguments are the same as `pkcs1-emsa-pss-encode`.


### [§3] Key wrapping

###### [!Function] `make-aes-key-wrap`  _wrapping-key_ _:key_ _iv_

Returns AES key wrapping procedure which accepts one argument of
bytevector to be wrapped.

The returning procedure wraps the given bytevector with AES Key Wrap algorithm
specified by NIST or RFC 3394.

The keyword argument _iv_ specifies initial value (IV) for data integrity.
The default value is `#vu8(#xa6 #xa6 #xa6 #xa6 #xa6 #xa6 #xa6 #xa6)`specified by the specification.


###### [!Function] `make-aes-key-unwrap`  _wrapping-key_ _:key_ _iv_

Returns AES key unwrapping procedure which accepts one argument of
bytevector to be unwrapped.

The returning procedure wraps the given bytevector with AES Key Wrap algorithm
specified by NIST or RFC 3394.

The keyword argument _iv_ specifies initial value (IV) for data integrity.
The default value is `#vu8(#xa6 #xa6 #xa6 #xa6 #xa6 #xa6 #xa6 #xa6)`specified by the specification.


### [§3] Cryptographic conditions

###### [!Condition Type] `&crypto-error` 
###### [!Function] `crypto-error?`  _obj_

Subcondition of `&error`.

Base condition type of all cryptographic conditions.

###### [!Condition Type] `&encrypt-error` 
###### [!Function] `encrypt-error?`  _obj_
###### [!Function] `condition-encrypt-mechanism`  _encrypt-error_

This condition will be raised when encrypt operation is failed.

###### [!Condition Type] `&decrypt-error` 
###### [!Function] `decrypt-error?`  _obj_
###### [!Function] `condition-decrypt-mechanism`  _decrypt-error_

This condition will be raised when decrypt operation is failed.

###### [!Condition Type] `&encode-error` 
###### [!Function] `encode-error?`  _obj_

This condition will be raised when encoding operation is failed.

###### [!Condition Type] `&decode-error` 
###### [!Function] `decode-error?`  _obj_

This condition will be raised when decoding operation is failed.

###### [!Function] `raise-encrypt-error`  _who_ _message_ _mechanism_ _:optional_ _irritants_

_who_, _message_ and _irritants_ are the same as
`assertion-violation`.

_mechanism_ should be a name of cryptographic algorithm.

Raises `&encrypt-error`.


###### [!Function] `raise-decrypt-error`  _who_ _message_ _mechanism_ _:optional_ _irritants_

_who_, _message_ and _irritants_ are the same as
`assertion-violation`.

_mechanism_ should be a name of cryptographic algorithm.

Raises `&decrypt-error`.


###### [!Function] `raise-encode-error`  _who_ _message_ _:optional_ _irritants_

_who_, _message_ and _irritants_ are the same as
`assertion-violation`.

Raises `&encode-error`.


###### [!Function] `raise-decode-error`  _who_ _message_ _:optional_ _irritants_

_who_, _message_ and _irritants_ are the same as
`assertion-violation`.

Raises `&decode-error`.


###### [!Condition Type] `&integrity-error` 
###### [!Function] `integrity-error?`  _obj_

Subcondition of `&crypto-error`This condition will be raised when key unwrap failed due to the
integrity check error.


### [§3] Creating own cipher {#custom.cipher}

If Sagittarius does not support sufficient cipher algorithm for you, then you
can write own cipher such as DSA. For this purpose, you might need to know how
this library works. It will be described the bottom of this section. If you just
want to create a new cipher, you just need to follow the example.

``````````scheme
(import (rnrs) (crypto) (clos user) (sagittarius))

(define (sample-encrypt pt key) pt)
(define (sample-decrypt ct key) ct)

(define-class <sample-cipher-spi> (<cipher-spi>) ())
(define-method initialize ((o <sample-cipher-spi>) initargs)
  (slot-set! o 'name 'sample)
  (slot-set! o 'key #f)
  (slot-set! o 'encrypt sample-encrypt)
  (slot-set! o 'decrypt sample-decrypt)
  (slot-set! o 'padder #f)
  (slot-set! o 'signer (lambda _ #vu8()))
  (slot-set! o 'verifier (lambda _ #t))
  (slot-set! o 'keysize (lambda _ 0)))

(define sample :sample)
(register-spi sample <sample-cipher-spi>)
;; test sample-cipher
(define sample-cipher (cipher sample #f))
(define message (string->utf8 "sample message"))
(let ((encrypted-message (encrypt sample-cipher message)))
  (decrypt sample-cipher encrypted-message))
;; -> #vu8(115 97 109 112 108 101 32 109 101 115 115 97 103 101)
``````````

The sample code actually does nothing. If you want to see real working code,
`ext/crypto/crypto/key/rsa.scm` might be a good example for you.

The basic idea of creating a new cipher is that you need to define own subclass
of `<cipher-spi>` and register it. 

###### [!Class] `<cipher-spi>` 

The base class for all SPI (Service Provider Interface).

Subclass must set these slots.

encrypt
: The value must be a procedure which takes 2 arguments.

decrypt
: The value must be a procedure which takes 2 arguments.

NOTE: Default symmetric key ciphers use `pkcs5-padder` which takes 3
arguments, bytevector, block-size and padding flag. This is because the
procedure can be used by multi ciphers. And custom cipher must know its own
block size.

These slots are optional.

name
: Describe the cipher.

key
: The value will be passed to `encrypt`, `decrypt`,
      `sign` and `verify` to be used.

signer
: A procedure for signing. The given procedure must accept at
      least 2 arguments.
    

verifier
: A procedure for verifying. The given procedure must
      accept at least 3 arguments.
    

keysize
: A procedure to get recommended keysize of this cipher. The
      given procedure must accept 1 argument.
    

padder
: The value must be #f or a procedure which takes 2
      arguments.
    

update-aad
: The value must be #f or a procedure which takes 1
      arguments. This is called when `cipher-update-aad!` is called.
    

tag
: The value must be #f or a procedure which takes 1
      arguments. This is called when `cipher-tag!` is called.
    

tagsize
: The value must be an integer. This represents the
      maximum length of authentication tag.

NOTE: Even required slots, Sagittarius does not check if it's set or not.

During its initialisation, the _initargs_ (the second argument of
the `initialize` method) takes at least 3 arguments, _key_, 
`:mode-parameter` keyword and _parameter_ passed to
`make-cipher`. The rest of other arguments are also passed if exist.


###### [!Function] `cipher-spi?`  _o_

Returns #t if the given object _o_ is a cipher SPI. Otherwise #f.

###### [!Function] `register-cipher-spi`  _mark_ _spi_

Register custom cipher SPI.

_mark_ can be any thing which returns #t then compared by `equal?`_spi_ must be subclass of `<cipher-spi>`NOTE: We recommend to make _mark_ the same as example code does and export
the registered _mark_. And the _mark_ must be unique enough not to
overwrite existing SPI names. 

e.g. `:rsa`, `:dsa` and `:ecdsa` are already used for
RSA, DSA and ECDSA


The concept of this SPI is influenced by Java's JCE. The toplevel of cipher is
just a wrapper for real implementaion (SPI). When a cipher is created, the
`cipher` procedure actually creates an instance of SPI class and set it to
the cipher object. So users need not to know about the implementation and if the
implementation supply default parameter then users even can use it by default.

This is the class hierarchy of these crypto objects.

``````````scheme
+ <top>
  + <crypto>
      + <cipher>
      + <cipher-spi>
          + <builtin-cipher-spi> <- default implementations of symmetric keys.
          + <rsa-cipher-spi>     <- default RSA implementation
      + <key>
          + <symmetric-key>
              + <builtin-symmetric-key> <- default symmetric key. ex. DES
          + <asymmetric-key>
              + <private-key>
                  + <rsa-private-key>
                      + <rsa-private-crt-key>
              + <public-key>
                  + <rsa-public-key>
``````````

The `<cipher>` and `builtin-` prefixed classes can not have any
subclass.

### [§3] Backward compatibility

The following procedures are kept for backward compatibility.


- `cipher` - Use `make-cipher` instead.
- `encrypt` - Use `cipher-encrypt` instead.
- `decrypt` - Use `cipher-decrypt` instead.
- `sign`  - Use `cipher-signature` instead.
- `verity` - Use `cipher-verify` instead.
- `register-spi` - Use `register-cipher-spi` instead.

