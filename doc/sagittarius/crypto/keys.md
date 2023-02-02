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

###### [!Function] `make-rfc3394-key-wrap` (_scheme_ `block-cipher-descriptor?`) (_key_ `symmetric-key?`) :key _iv_

Creates a key wrap procedure using given _scheme_ and _key_. The optional
keyword parameter _iv_ is used as an initial vector if provided.

###### [!Function] `make-aes-key-wrap` (_key_ `symmetric-key?`) _opts_ _..._

Creates an AES key wrap procedure.

###### [!Function] `make-camellia-key-wrap` (_key_ `symmetric-key?`) _opts_ _..._

Creates an Camellia key wrap procedure.

###### [!Function] `make-rfc3394-key-unwrap` (_scheme_ `block-cipher-descriptor?`) (_key_ `symmetric-key?`) :key _iv_

Creates a key unwrap procedure using given _scheme_ and _key_. The optional
keyword parameter _iv_ is used as an initial vector if provided.

###### [!Function] `make-aes-key-unwrap` (_key_ `symmetric-key?`) _opts_ _..._

Creates an AES key unwrap procedure.

###### [!Function] `make-camellia-key-unwrap` (_key_ `symmetric-key?`) _opts_ _..._

Creates an Camellia key unwrap procedure.


### [§4] Asymmetric key operations

###### [!Function] `key-pair?` _obj_

Returns `#t` if the given _obj_ is a key pair, otherwise `#f`.

###### [!Function] `key-pair-private` (_kp_ `key-pair?`)

Returns the private key of the given _kp_.

###### [!Function] `key-pair-public`

Returns the public key of the given _kp_.

###### [!Method] `generate-key-pair` _scheme_ _opts_ _..._

Generates a key pair of the given _scheme_. Followings are the supported
schemes:

###### [!Key scheme] `*key:rsa`
###### [!Key scheme] `*key:dsa`
###### [!Key scheme] `*key:ecdsa`
###### [!Key scheme] `*key:ed25519`
###### [!Key scheme] `*key:ed448`
###### [!Key scheme] `*key:x25519`
###### [!Key scheme] `*key:x448`

Those are RSA, DSA, ECDSA, Ed5519, Ed448, X25519 and X448 respectively.  
For convenience `*scheme:rsa` has the same effect as `*key:rsa`.

###### [!Function] `generate-public-key` _scheme_ _opts_ ...
###### [!Function] `generate-private-key` _scheme_ _opts_ ...

###### [!Macro] `public-key-format` _format_

A macro returns a symbol representation of _format_. The _format_ must be
either `raw` or `subject-public-key-info`.

`raw`:
  Imports/exports raw public key.
`subject-public-key-info`:
  Imports/exports subject public key info.

###### [!Enum set] `*public-key-formats*`

Enum set of the `public-key-format`.

###### [!Function] `public-key-format?` _obj_

Returns `#t` if the given _obj_ is a member of `public-key-format` enum.

###### [!Method] `import-public-key` `<bytevector>` `'subject-public-key-info`
###### [!Method] `import-public-key` `<port>` `'subject-public-key-info`

Imports a public from the given `<bytevector>` or `<port>` which must be
a valid subject public key info.  
The returning public key type is detected via the OID inside of the
subject public key info.

###### [!Method] `import-public-key` `*key:rsa*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:rsa*` `<port>` :optional _format_

Imports RSA key from given `<bytevector>` or `<port>`. The optional _format_
specifies the format of the given key. Default value is `raw`.

###### [!Method] `export-public-key` `<rsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:rsa*` `<rsa-public-key>` :optional _format_

Exports RSA key from the given `<rsa-public-key>`. The optional _format_
controls the format of the exporting key. Default value is `raw`.

###### [!Method] `import-public-key` `*key:dsa*` `<bytevector>` :optional _format_ _parameter_
###### [!Method] `import-public-key` `*key:dsa*` `<port>` :optional _format_ _parameter_

Imports DSA key from given `<bytevector>` or `<port>`. The optional
_format_ specifies the format of the given key. Default value is
`subject-public-key-info`.

If `raw` is specified, then optional argument _parameter_ must be specified
and it must be `<dsa-key-parameter>`

###### [!Method] `export-public-key` `<dsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:dsa*` `<dsa-public-key>` :optional _format_

Exports DSA key from the given `<dsa-public-key>`. The optional
_format_ controls the format of the exporting key. Default value
is `subject-public-key-info`.

###### [!Method] `import-public-key` `*key:ecdsa*` `<bytevector>` :optional _format_ _ec-parameter_
###### [!Method] `import-public-key` `*key:ecdsa*` `<port>` :optional _format_ _ec-parameter_

Imports ECDSA key from given `<bytevector>` or `<port>`. The optional
_format_ specifies the format of the given key. Default value is
`subject-public-key-info`.  
If `raw` format is specified, then optional parameter `ec-parameter` must be
specified. Below are the supported EC parameters.

###### [!EC parameter] `*ec-parameter:p192*`
###### [!EC parameter] `*ec-parameter:p224*`
###### [!EC parameter] `*ec-parameter:p256*`
###### [!EC parameter] `*ec-parameter:p384*`
###### [!EC parameter] `*ec-parameter:p521*`
###### [!EC parameter] `*ec-parameter:k163*`
###### [!EC parameter] `*ec-parameter:k233*`
###### [!EC parameter] `*ec-parameter:k283*`
###### [!EC parameter] `*ec-parameter:k409*`
###### [!EC parameter] `*ec-parameter:k571*`
###### [!EC parameter] `*ec-parameter:b163*`
###### [!EC parameter] `*ec-parameter:b233*`
###### [!EC parameter] `*ec-parameter:b283*`
###### [!EC parameter] `*ec-parameter:b409*`
###### [!EC parameter] `*ec-parameter:b571*`
###### [!EC parameter] `*ec-parameter:secp192r1*`
###### [!EC parameter] `*ec-parameter:secp224r1*`
###### [!EC parameter] `*ec-parameter:secp256r1*`
###### [!EC parameter] `*ec-parameter:secp384r1*`
###### [!EC parameter] `*ec-parameter:secp521r1*`
###### [!EC parameter] `*ec-parameter:sect163k1*`
###### [!EC parameter] `*ec-parameter:sect233k1*`
###### [!EC parameter] `*ec-parameter:sect283k1*`
###### [!EC parameter] `*ec-parameter:sect409k1*`
###### [!EC parameter] `*ec-parameter:sect571k1*`
###### [!EC parameter] `*ec-parameter:sect163r2*`
###### [!EC parameter] `*ec-parameter:sect233r1*`
###### [!EC parameter] `*ec-parameter:sect283r1*`
###### [!EC parameter] `*ec-parameter:sect409r1*`
###### [!EC parameter] `*ec-parameter:sect571r1*`
###### [!EC parameter] `*ec-parameter:secp192k1*`
###### [!EC parameter] `*ec-parameter:secp224k1*`
###### [!EC parameter] `*ec-parameter:secp256k1*`
###### [!EC parameter] `*ec-parameter:sect163r1*`
###### [!EC parameter] `*ec-parameter:sect239k1*`
###### [!EC parameter] `*ec-parameter:sect113r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p160r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p160t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p192r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p192t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p224r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p224t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p256r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p256t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p320r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p320t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p384r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p384t1*`
###### [!EC parameter] `*ec-parameter:brainpool-p512r1*`
###### [!EC parameter] `*ec-parameter:brainpool-p512t1*`

After `ec-parameter:` represents the parameter name.

###### [!Method] `export-public-key` `<ecdsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:ecdsa*` `<ecdsa-public-key>` :optional _format_

Exports ECDSA key from the given `<ecdsa-public-key>`. The optional
_format_ controls the format of the exporting key. Default value
is `subject-public-key-info`.

###### [!Method] `import-public-key` `*key:ed25519*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:ed25519*` `<port>` :optional _format_
###### [!Method] `import-public-key` `*key:ed448*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:ed448*` `<port>` :optional _format_

Imports Ed25519 or Ed448 public key from the given `<bytevector>` or `<port>`.
The optional _format_ controls the key format, default is `raw`.

###### [!Method] `export-public-key` `<eddsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:ed25519*` `<eddsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `<eddsa-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:ed448*` `<eddsa-public-key>` :optional _format_

Exports Ed25519 or Ed448 key from the given `<eddsa-public-key>`. The optional
_format_ controls the format of the exporting key. Default value
is `raw`.

If the first form is used, then the method automatically detects the key type.

###### [!Method] `import-public-key` `*key:x25519*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:x25519*` `<port>` :optional _format_
###### [!Method] `import-public-key` `*key:x448*` `<bytevector>` :optional _format_
###### [!Method] `import-public-key` `*key:x448*` `<port>` :optional _format_

Imports X25519 or X448 public key from the given `<bytevector>` or `<port>`.
The optional _format_ controls the key format, default is `raw`.

###### [!Method] `export-public-key` `<rfc7748-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:x25519*` `<rfc7748-public-key>` :optional _format_
###### [!Method] `export-public-key` `<rfc7748-public-key>` :optional _format_
###### [!Method] `export-public-key` `*key:x448*` `<rfc7748-public-key>` :optional _format_

Exports X25519 or X448 key from the given `<rfc7748-public-key>`. The optional
_format_ controls the format of the exporting key. Default value
is `raw`.

If the first form is used, then the method automatically detects the key type.

###### [!Macro] `private-key-format` _format_

A macro returns a symbol representation of _format_. The _format_ must be
either `raw` or `private-key-info`.

`raw`:
  Imports/exports raw public key.
`private-key-info`:
  Imports/exports private key info or one asymmetric key.

###### [!Enum set] `*private-key-formats*`

Enum set of the `private-key-format`.

###### [!Function] `private-key-format?` _obj_

Returns `#t` if the given _obj_ is a member of `private-key-format` enum.

###### [!Function] `import-private-key` `<bytevector>` `'private-key-info`
###### [!Function] `import-private-key` `<port>` `'private-key-info`

Imports a private key from the `<bytevector>` or `<port>` which must
be a valid private key info or one asymmetric key.  
The returning private key type is detected by the OID inside of the
private key info.

###### [!Function] `import-private-key` `*key:rsa` `<bytevector>` 
###### [!Function] `import-private-key` `*key:rsa` `<port>` 


###### [!Function] `export-private-key` `<private-key>` :optional _format_

###### [!Function] `oid->key-operation`
###### [!Function] `key->oid`

###### [!Function] `calculate-key-agreement`
###### [!Function] `private-key-format`
###### [!Function] `*private-key-formats*`
###### [!Function] `private-key-format?`
