[§3] Cipher library - (sagittarius crypto ciphers) {#sagittarius.crypto.ciphers}
------------------------------------------------------

The cipher library provides both symmetric and asymmetric cipher
operations.

###### [!Library] `(sagittarius crypto ciphers)`

This library provides both symmetric and asymmetric cipher operations.

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


