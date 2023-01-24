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

###### [!Function] `make-rfc3394-key-wrap`
###### [!Function] `make-aes-key-wrap`
###### [!Function] `make-camellia-key-wrap`
###### [!Function] `make-rfc3394-key-unwrap`
###### [!Function] `make-aes-key-unwrap`
###### [!Function] `make-camellia-key-unwrap`


### [§4] Asymmetric key operations

###### [!Function] `key-pair?`
###### [!Function] `key-pair-private`
###### [!Function] `key-pair-public`
