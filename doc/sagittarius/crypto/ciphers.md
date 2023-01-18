[§3] Cipher library - (sagittarius crypto ciphers) {#sagittarius.crypto.ciphers}
------------------------------------------------------

The cipher library provides both symmetric and asymmetric cipher
operations.

###### [!Library] `(sagittarius crypto ciphers)`

This library provides both symmetric and asymmetric cipher operations.

### [§4] Ciphers parameters

Cipher parameters are parameters to pass to the cipher, such as
Initial Vector (IV). 

All the parameter value retriever accept optional argument. If it's
provided and the parameter is not or does not contain the target
parameter, then the default value is returned, otherwise `&assertion`
is signalled.

NOTE: The cipher parameters are only used on symmetric ciphers.
Asymmetric ciphers use keywords.

###### [!Function] `cipher-parameter?` _obj_

Returns `#t` if the given _obj_ is a cipher parameter, otherwise `#f`.

A cipher parameter can be a simple parameter or composite parameter
like the condition system.

###### [!Function] `make-cipher-parameter` _parameter_ _..._

Creates a composite cipher parameter whose contents are given *parameter*s.

###### [!Function] `round-parameter?` _obj_

Returns `#t` if the given _obj_ is a round cipher parameter, otherwise `#f`.

This parameter is used all the modes.

###### [!Function] `make-round-parameter` (_round_ `integer?`)

Creates a round cipher parameter with the given _round_.

###### [!Function] `cipher-parameter-rounds` (_parameter_ `cipher-parameter?`) :optional default

Retrieves the `round` field of the given _parameter_.

###### [!Function] `iv-parameter?` _obj_

Returns `#t` if the given _obj_ is a iv cipher parameter, otherwise `#f`.

This parameter is used by CBC, CFB, OFB, CTR, LRW F8 and GCM, and all of the
modes require it.

###### [!Function] `make-iv-parameter` (_iv_ `bytevector?`)

Creates a IV cipher parameter with the given _iv_. The _iv_ is copied
during the creation so modifying the original value does not affect
the parameter.

###### [!Function] `cipher-parameter-iv` (_parameter_ `cipher-parameter?`) :optional default

Retrieves the `iv` field of the given _parameter_.

###### [!Function] `counter-mode-parameter?` _obj_

Returns `#t` if the given _obj_ is a counter mode cipher parameter,
otherwise `#f`.

This parameter is used by CTR mode and if it's not provided, then
`*ctr-mode:big-endian*` is used.

###### [!Function] `make-counter-mode-parameter` _mode_

Creates a counter mode parameter with the given _mode_. The _mode_ must
be one of the following:

###### [!Function] `*ctr-mode:little-endian*`
###### [!Function] `*ctr-mode:big-endian*`
###### [!Function] `*ctr-mode:rfc3686`

These modes are little-endian, big-endian, and IPSec ESP described 
[RFC 3686](https://datatracker.ietf.org/doc/html/rfc3686), respectively.

###### [!Function] `cipher-parameter-counter-mode` (_parameter_ `cipher-parameter?`) :optional default

Retrieves the `counter-mode` field of the given _parameter_.

###### [!Function] `tweak-parameter?` _obj_

Returns `#t` if the given _obj_ is a tweak cipher parameter, otherwise `#f`.

This parameter is used by LRW mode, and it's a required parameter.

###### [!Function] `make-tweak-parameter` (_tweak_ `bytevector?`)

Creates a tweak cipher parameter with the given _tweak_. The _tweak_
is copied during the creation so modifying the original value does not
affect the parameter.

###### [!Function] `cipher-parameter-tweak` (_parameter_ `cipher-parameter?`) :optional default

Retrieves the `tweak` field of the given _parameter_.

###### [!Function] `salt-parameter?` _obj_

Returns `#t` if the given _obj_ is a salt cipher parameter, otherwise `#f`.

This parameter is used by F8 mode, and it's a required parameter.

###### [!Function] `make-salt-parameter` (_salt_ `bytevector?`)

Creates a salt cipher parameter with the given _salt_. The _salt_
is copied during the creation so modifying the original value does not
affect the parameter.

###### [!Function] `cipher-parameter-salt` (_parameter_ `cipher-parameter?`) :optional default

Retrieves the `salt` field of the given _parameter_.

###### [!Function] `nonce-parameter?` _obj_

Returns `#t` if the given _obj_ is a nonce cipher parameter, otherwise `#f`.

This parameter is used by EAX, OCB and OCB3. EAX doesn't require it,
others require it.

###### [!Function] `make-nonce-parameter` (_nonce_ `bytevector?`)

Creates a nonce cipher parameter with the given _nonce_. The _nonce_
is copied during the creation so modifying the original value does not
affect the parameter.

###### [!Function] `cipher-parameter-nonce` (_parameter_ `cipher-parameter?`) :optional default

Retrieves the `nonce` field of the given _parameter_.

###### [!Function] `aad-parameter?` _obj_

Returns `#t` if the given _obj_ is a aad cipher parameter, otherwise `#f`.

This parameter is used by EAX and GCM. It's an optional parameter.

###### [!Function] `make-aad-parameter` (_aad_ `bytevector?`)

Creates a aad cipher parameter with the given _aad_. The _aad_
is copied during the creation so modifying the original value does not
affect the parameter.

###### [!Function] `cipher-parameter-aad` (_parameter_ `cipher-parameter?`) :optional default

Retrieves the `aad` field of the given _parameter_.

###### [!Function] `tag-length-parameter?` _obj_

Returns `#t` if the given _obj_ is a tag length cipher parameter,
otherwise `#f`.

This parameter is used by OCB3 and it's a required parameter.

###### [!Function] `make-tag-length-parameter` (_tag-length_ `integer?`)

Creates a tag length cipher parameter with the given _tag-length_.

###### [!Function] `cipher-parameter-tag-length` (_parameter_ `cipher-parameter?`) :optional default

Retrieves the `tag-length` field of the given _parameter_.

### [§4] Ciphers

###### [!Function] `cipher-descriptor?` _obj_

Returns `#t` if the given _obj_ is a cipher descriptor, otherwise `#f`.

A cipher descriptor is an object which holds encryption scheme information,
such as algorithm, key length, etc.  
This object itself doesn't provide any cipher operations, to execute
them, users need to create a cipher object.

###### [!Function] `cipher-descriptor-name` (_descriptor_ `cipher-descriptor?`)

Returns the name of the given _descriptor_.


###### [!Function] `cipher?` _obj_

Returns `#t` if the given _obj_ is a cipher object, otherwise `#f`.

A cipher object is an actual working object to operate cipher operation.  
A cipher may holds operation state, and it is users' responsibility to
make sure not to mix up the state.


###### [!Macro] `cipher-direction` _direction_

A macro returns a symbol representation of _direction_. The _direction_
must be either `encrypt` or `decrypt`.


### [§4] Symmetric ciphers

Symmetric cipher can be either block cipher or stream cipher. At this
moment, the library only supports block cipher. Stream ciphers may
come in the future.

###### [!Function] `symmetric-cipher-descriptor?` _obj_

Returns `#t` if the given _obj_ is a symmetric cipher descriptor,
otherwise `#f`.

###### [!Function] `symmetric-cipher-descriptor-min-key-length` (_descriptor_ `symmetric-cipher-descriptor?`)

Returns minimum key length of the given _descriptor_'s algorithm.

###### [!Function] `symmetric-cipher-descriptor-max-key-length` (_descriptor_ `symmetric-cipher-descriptor?`)

Returns maximum key length of the given _descriptor_'s algorithm.

###### [!Function] `symmetric-cipher?` _obj_

Returns `#t` if the given _obj_ is a symmetric cipher object, otherwise `#f`.


###### [!Function] `block-cipher-descriptor?` _obj_

Returns `#t` if the given _obj_ is a block cipher descriptor,
otherwise `#f`.

Currently, below encryption algorithms are supported:

###### [!Block cipher descriptor] `*scheme:aes*`
###### [!Block cipher descriptor] `*scheme:aes-128*`
###### [!Block cipher descriptor] `*scheme:aes-192*`
###### [!Block cipher descriptor] `*scheme:aes-256*`
###### [!Block cipher descriptor] `*scheme:blowfish*`
###### [!Block cipher descriptor] `*scheme:camellia*`
###### [!Block cipher descriptor] `*scheme:cast-128*`
###### [!Block cipher descriptor] `*scheme:cast5*`
###### [!Block cipher descriptor] `*scheme:des*`
###### [!Block cipher descriptor] `*scheme:des3*`
###### [!Block cipher descriptor] `*scheme:desede*`
###### [!Block cipher descriptor] `*scheme:kasumi*`
###### [!Block cipher descriptor] `*scheme:khazad*`
###### [!Block cipher descriptor] `*scheme:noekeon*`
###### [!Block cipher descriptor] `*scheme:rc2*`
###### [!Block cipher descriptor] `*scheme:rc5*`
###### [!Block cipher descriptor] `*scheme:rc6*`
###### [!Block cipher descriptor] `*scheme:safer+*`
###### [!Block cipher descriptor] `*scheme:safer-k128*`
###### [!Block cipher descriptor] `*scheme:safer-k64*`
###### [!Block cipher descriptor] `*scheme:safer-sk128*`
###### [!Block cipher descriptor] `*scheme:safer-sk64*`
###### [!Block cipher descriptor] `*scheme:seed*`
###### [!Block cipher descriptor] `*scheme:skipjack*`
###### [!Block cipher descriptor] `*scheme:twofish*`
###### [!Block cipher descriptor] `*scheme:x-tea*`

Some of the algorithms are considered as broken cipher, such as DES.  
It is users' responsibility to use those ciphers.

NOTE: NIST recommends to use AES.

###### [!Function] `block-cipher-descriptor-block-length` (_descriptor_ `block-cipher-descriptor?`)

Returns block size of the given _descriptor_'s algorithm.

###### [!Function] `block-cipher-descriptor-suggested-key-length` (_descriptor_ `block-cipher-descriptor?`)

Returns suggested key length of the given _descriptor_'s algorithm.

Most of the time, this returns the result of 
the `symmetric-cipher-descriptor-max-key-length` procedure.

###### [!Function] `mode-descriptor?` _obj_

Returns `#t` if the given _obj_ is a mode descriptor, otherwise `#f`.

Currently, below encryption modes are supported.

###### [!Mode descriptor] `*mode:ecb*`
###### [!Mode descriptor] `*mode:cbc*`
###### [!Mode descriptor] `*mode:cfb*`
###### [!Mode descriptor] `*mode:ofb*`
###### [!Mode descriptor] `*mode:ctr*`
###### [!Mode descriptor] `*mode:lrw*`
###### [!Mode descriptor] `*mode:f8*`

Mode descriptors for ECB, CBC, CFB, OFB, CTR, LRW and F8 respectively.

###### [!Mode descriptor] `*mode:eax*`
###### [!Mode descriptor] `*mode:ocb*`
###### [!Mode descriptor] `*mode:ocb3*`
###### [!Mode descriptor] `*mode:gcm*`

Mode descriptor for EAX, OCB, OCB3 and GCM respectively. These are
authenticated encryption modes.

###### [!Function] `mode-descriptor-name` (_descriptor_ `mode-descriptor?`)

Returns the name of the given _descriptor_.


###### [!Function] `block-cipher?` _obj_

Returns `#t` if the given _obj_ is a block cipher object, otherwise `#f`.

###### [!Function] `make-block-cipher` (_scheme_ `block-cipher-descriptor?`) (_mode_ `mode-descriptor`) :optional (_padding_ `pkcs7-padding`)

Creates a block cipher object of _scheme_ encryption scheme and
_mode_ encryption mode.  
_padding_ is used to encrypt or decrypt the last block of the plain text.

###### [!Function] `block-cipher-block-length` (_cipher_  `block-cipher?`)

Returns the block length of the given _cipher_.

###### [!Function] `block-cipher-init!` (_cipher_  `block-cipher?`) (_direction_ `symbol?`) (_key_ `symmetric-key?`) :optional (_parameter_ `cipher-parameter?`)

Initialise the given _cipher_ for the given _direction_ purpose with
the given _key_ and _parameter_.  
The _direction_ must be a symbol returned by the `cipher-direction` macro.

###### [!Function] `block-cipher-init` (_cipher_  `block-cipher?`) (_direction_ `symbol?`) (_key_ `symmetric-key?`) :optional (_parameter_ `cipher-parameter?`)

Initialise the give _cipher_. This procedure is an analogy to the
`block-cipher-init!`, the difference is this procedure returns a
copy of the given _cipher_.

###### [!Function] `block-cipher-encrypt!` (_cipher_  `block-cipher?`) (_pt_ `bytevector?`) (_ps_ `integer?`) (_ct_ `bytevector?`) (_cs_ `integer?`)

Encrypts the given plain text _pt_ from the position of _ps_, and store the
cipher text into _ct_ from the position of _cs_. Then returns the byte
size of the encrypted plain text.  
The encryption is executed by the given _cipher_.

###### [!Function] `block-cipher-encrypt` (_cipher_  `block-cipher?`) (_pt_ `bytevector?`) :optional (_ps_ 0)

Encrypts the given plain text _pt_ from the position of _ps_, and
returns the cipher text.  
The encryption is executed by the given _cipher_.

NOTE: the given _pt_ length and result cipher text length may differ
if the _pt_ length is not multiple of the cipher block size.

###### [!Function] `block-cipher-encrypt-last-block!` (_cipher_  `block-cipher?`) (_pt_ `bytevector?`) (_ps_ `integer?`) (_ct_ `bytevector?`) (_cs_ `integer?`)

Encrypts the given plain text _pt_ from the position of _ps_, and store the
cipher text into _ct_ from the position of _cs_. Then returns the byte
size of the encrypted plain text.  
The encryption is executed by the given _cipher_.

This procedure consumes all the plain text, and applies padding if needed.

###### [!Function] `block-cipher-encrypt-last-block` (_cipher_  `block-cipher?`) (_pt_ `bytevector?`) :optional (_ps_ 0)

Encrypts the given plain text _pt_ from the position of _ps_, and
returns the cipher text.  
The encryption is executed by the given _cipher_.

This procedure consumes all the plain text, and applies padding if needed.

###### [!Function] `block-cipher-decrypt! ` (_cipher_  `block-cipher?`) (_ct_ `bytevector?`) (_cs_ `integer?`) (_pt_ `bytevector?`) (_ps_ `integer?`)

Decrypts the given cipher text _ct_ from the position of _cs_, store the
plain text info _pt_ from the position of _cs_. Then returns the byte
size of the decrypted cipher text.
The decryption is executed by the given _cipher_.

###### [!Function] `block-cipher-decrypt` (_cipher_  `block-cipher?`) (_ct_ `bytevector?`) :optional (_cs_ 0)

Decrypts the given cipher text _ct_ from the position of _cs_, and
returns the plain text.  
The decryption is executed by the given _cipher_.

NOTE: the given _ct_ length and result plain text length may differ
if the _ct_ length is not multiple of the cipher block size.


###### [!Function] `block-cipher-decrypt-last-block!` (_cipher_  `block-cipher?`) (_ct_ `bytevector?`) (_cs_ `integer?`) (_pt_ `bytevector?`) (_ps_ `integer?`)

Decrypts the given cipher text _ct_ from the position of _cs_, store the
plain text info _pt_ from the position of _cs_. Then returns the byte
size of the decrypted cipher text.

This procedure applies unpadding if the _cipher_ is created with padding.

###### [!Function] `block-cipher-decrypt-last-block` (_cipher_  `block-cipher?`) (_ct_ `bytevector?`) :optional (_cs_ 0)

Decrypts the given cipher text _ct_ from the position of _cs_, and
returns the plain text.  
The decryption is executed by the given _cipher_.

This procedure applies unpadding if the _cipher_ is created with padding.

###### [!Function] `block-cipher-done!` (_cipher_  `block-cipher?`)

Cleanup the given _cipher_ and make it neutral state.

###### [!Function] `block-cipher-update-aad!` (_cipher_ `block-cipher?`) (_aad_ `bytevector?`) :optional (_start_ 0) (_length_ `(- (bytevector-length aad) start)`)

Updating Additional Authentication Data _aad_ of the given _cipher_.  
Optional arguments restricts the range of the _aad_.

This procedure is effective on authenticated encryption modes.

###### [!Function] `block-cipher-update-iv!` (_cipher_ `block-cipher?`) (_iv_ `bytevector?`) :optional (_start_ 0) (_length_ `(- (bytevector-length aad) start)`)

Updating Initial Vector _iv_ of the given _cipher_.  
Optional arguments restricts the range of the _iv_.

This procedure is effective only GCM mode.

###### [!Function] `block-cipher-max-tag-length` (_cipher_ `block-cipher?`)

Returns the max tag length of the given _cipher_. If the cipher doesn't
support authentication tag, then it returns `0`.

###### [!Function] `block-cipher-done/tag!` (_cipher_ `block-cipher?`) (_tag_ `bytevector?`) :optional (_start_ 0)

If the _cipher_ is encryption mode, then the procedure stores the
authentication tag into the give _tag_ of the position starting
_start_.

If the _cipher_ is decryption mode, then the procedure validates the
given _tag_ against the cipher's authentication tag starting from the
position of _start_.

###### [!Function] `block-cipher-done/tag` (_cipher_ `block-cipher?`) (_tag-len_ `integer?`)

This procedure is only for encryption mode.  
Stores the *cipher*'s authentication tag into the given _tag_,
starting position of _start_.
