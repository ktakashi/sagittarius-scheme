[§3] Digest library - (sagittarius crypto digests) {#sagittarius.crypto.digests}
------------------------------------------------------

The digest library provides cryptographic hash functions.

###### [!Library] `(sagittarius crypto digests)`

The digest library, this library exports the procedures listed below sections.

### [§4] Digest descriptor

A digest descriptor describes the characteristic of the digest, such as
name, digest size (if applicable), block size and OID (if exists).

###### [!Function] `digest-descriptor?` _obj_

Returns `#t` if the given _obj_ is a digest descriptor, otherwise `#f`.

###### [!Function] `digest-descriptor-name` (_descriptor_ `digest-descriptor?`)

Returns the human readable name of the _descriptor_.

###### [!Function] `digest-descriptor-digest-size` (_descriptor_ `digest-descriptor?`)

Returns the digest size of the _descriptor_. Some of the digests, 
e.g. SHAKE, don't have fixed digest size.

###### [!Function] `digest-descriptor-block-size` (_descriptor_ `digest-descriptor?`)

Returns the block size of the _descriptor_.

###### [!Function] `digest-descriptor-oid` (_descriptor_ `digest-descriptor?`)

Returns the OID of the _descriptor_. If the digest doesn't have OID, then
it returns `#f`.

---

Blow are the digest descriptors supported by this library:

###### [!Digest descriptor] `*digest:whirlpool*`
###### [!Digest descriptor] `*digest:ripemd-128*`
###### [!Digest descriptor] `*digest:ripemd-160*`
###### [!Digest descriptor] `*digest:ripemd-256*`
###### [!Digest descriptor] `*digest:ripemd-320*`
###### [!Digest descriptor] `*digest:sha-1*`
###### [!Digest descriptor] `*digest:sha-224*`
###### [!Digest descriptor] `*digest:sha-256*`
###### [!Digest descriptor] `*digest:sha-384*`
###### [!Digest descriptor] `*digest:sha-512*`
###### [!Digest descriptor] `*digest:sha-512/224*`
###### [!Digest descriptor] `*digest:sha-512/256*`
###### [!Digest descriptor] `*digest:sha3-224* `
###### [!Digest descriptor] `*digest:sha3-256* `
###### [!Digest descriptor] `*digest:sha3-384*`
###### [!Digest descriptor] `*digest:sha3-512*`
###### [!Digest descriptor] `*digest:keccak-224*`
###### [!Digest descriptor] `*digest:keccak-256*`
###### [!Digest descriptor] `*digest:keccak-384*`
###### [!Digest descriptor] `*digest:keccak-512*`
###### [!Digest descriptor] `*digest:tiger-192*`
###### [!Digest descriptor] `*digest:md5*`
###### [!Digest descriptor] `*digest:md4*`
###### [!Digest descriptor] `*digest:md2*`
###### [!Digest descriptor] `*digest:blake2s-128*`
###### [!Digest descriptor] `*digest:blake2s-160*`
###### [!Digest descriptor] `*digest:blake2s-224*`
###### [!Digest descriptor] `*digest:blake2s-256*`
###### [!Digest descriptor] `*digest:blake2b-160*`
###### [!Digest descriptor] `*digest:blake2b-256*`
###### [!Digest descriptor] `*digest:blake2b-384*`
###### [!Digest descriptor] `*digest:blake2b-512*`
###### [!Digest descriptor] `*digest:shake-128*`
###### [!Digest descriptor] `*digest:shake-256*`
###### [!Digest descriptor] `*digest:cshake-128*`
###### [!Digest descriptor] `*digest:cshake-256*`

We supports weak / broken digest algorithms, such as MD5. It is users'
responsibility to choose appropriate algorithm.

### [§4] Message Digest

Message Digest is a stateful object which ingests input and provides
digests.

###### [!Function] `message-digest?` _obj_

Returns `#t` if the given _obj_ is a message digest, otherwise `#f`.

###### [!Function] `make-message-digest` (_descriptor_ `digest-descriptor?`)

Creates a message digest of the algorithm _descriptor_.

###### [!Function] `message-digest-descriptor` (_md_ `message-digest?`)

Returns the digest descriptor of given _md_.

###### [!Function] `message-digest-digest-size` (_md_ `message-digest?`)

Returns the digest size of given _md_.

###### [!Function] `digest-message` (_md_ `message-digest?`) (_msg_ `bytevector?`) :optional _length_

Returns the digest of the given _msg_.  
The optional argument _length_ is only required for the variable
length digest algorithms, such as SHAKE. Otherwise, the value will be
ignored.

###### [!Function] `digest-message!` (_md_ `message-digest?`) (_msg_ `bytevector?`) (_out_ `bytevector?`) :optional _start_ _length_

Digests the given _msg_, then fill the result into the _out_.  
Optional argument _start_ and _length_ specify the start position of the
_out_ and the length of the filling.

---
Below procedures are low level APIs which can be used not to allocate 
entire input buffer, say using `get-bytevector-n!` instead of 
`get-bytevector-all`.

###### [!Function] `message-digest-init!` (_md_ `message-digest?`)

Initialises the given _md_. This procedure rests the previous input as well.

###### [!Function] `message-digest-process!` (_md_ `message-digest?`) (_msg_ `bytevector?`) :optional _start_ _length_

Ingests the given _msg_ into _md_.  
Optional argument _start_ and _length_ controls the range of the _msg_.

###### [!Function] `message-digest-done!` (_md_ `message-digest?`) (_out_ `bytevector?`) :optional _start_ _length_

Fill the result digest of _md_ into the _out_.  
Optional argument _start_ and _length_ controls the range of the _out_.

###### [!Function] `message-digest-done` (_md_ `message-digest?`) :optional _length_

Returns a freshly allocated bytevector whose content is the result of
_md_, digest.  
Optional argument _length_ is only used for the variable length digest
algorithm, such as SHAKE. Otherwise it will be ignored.

