[§3] Keystore library - (sagittarius crypto keystores) {#sagittarius.crypto.keystoress}
------------------------------------------------------

The keystore library provides keystore operation. More precisely, PKCS#12.

PKCS#12 keystore is very flexible format, and there's no standard
convension, but just de-fact. For example, PKCS#12 itself doesn't
require the entry to be key/value, however most of the existing
library uses the format as key/value map.

This library provides the de-fact convension layer. If you want to
use raw PKCS#12 format, see `(sagittarius crypto pkcs keystore)`.

###### [!Library] `(sagittarius crypto keystores)`

The keystore library, this library exports the precedures listed
blow sections.

###### [!Function] `pkcs12-keystore?` _obj_

Returns `#t` if the given _obj_ is PKCS#12 keystore, otherwise `#f`.

###### [!Function] `make-pkcs12-keystore`

Creates an empty PKCS#12 keystore.

###### [!Function] `read-pkcs12-keystore` (_password_ `string?`) :optiona _in_

Reads PKCS#12 keystore from _in_, if provided, otherwise `(current-input-port)`.

The _password_ must be a valid password for the reading PKCS#12, otherwise
an error is signalled.

###### [!Function] `bytevector->pkcs12-keystore` (_bv_ `bytevector?`) (_password_ `string?`)

Converts given _bv_ to PKCS#12 keystore.

The _password_ must be a valid password for the reading PKCS#12, otherwise
an error is signalled.

###### [!Function] `pkcs12-keystore->bytevector` (_ks_ `pkcs12-keystore?`) (_password_ `string?`)

Converts the given _ks_ to a bytevector using the given _password_ as
both integrity and privacy exchange key.

###### [!Function] `write-pkcs12-keystore` (_ks_ `pkcs12-keystore?`) (_password_ `string?`) :optiona _out_

Writes the given _ks_ to _out_ if provided, otherwise `(current-output-port)`,
using the given _password_ as both integrity and privacy exchange key.

###### [!Function] `pkcs12-keystore-private-key-ref` (_ks_ `pkcs12-keystore?`) (_alias_ `string?`) (_password_ `string?`)

Retrieves a private key associated to _alias_ from _ks_ decrypting with
the given _password_.

If the _alias_ doesn't exist in the _ks_, then returns `#f`.

###### [!Function] `pkcs12-keystore-private-key-set!` (_ks_ `pkcs12-keystore?`) (_alias_ `string?`) (_key_ `private-key?`) (_password_ `string?`) _certs_ 

_certs_ must be a list of X.509 certificates.

Stores the given _key_ into the _ks_ associating with _alias_. The
_key_ is encrypted with the _password_.

The _certs_ must be a valid (chain) certificate(s) for the given _key_.

###### [!Function] `pkcs12-keystore-certificate-ref` (_ks_ `pkcs12-keystore?`) (_alias_ `string?`)

Retrieves a certificate associated to _alias_ from _ks_.

If the _alias_ doesn't exist in the _ks_, then returns `#f`.

###### [!Function] `pkcs12-keystore-certificate-set!` (_ks_ `pkcs12-keystore?`) (_alias_ `string?`) (_cert_ `x509-certificate?`)

Stores the given _cert_ into the given _ks_ associating with _alias_.

###### [!Function] `pkcs12-keystore-certificate-chain-ref` (_ks_ `pkcs12-keystore?`) (_alias_ `string?`)

Retrieves certificate chain associated to _alias_ from the _ks_.

If there's no certificate associated to _alias_, then it returns `()`.

###### [!Function] `pkcs12-keystore-delete-entry!` (_ks_ `pkcs12-keystore?`) (_alias_ `string?`)

Removes the entry associated to _alias_ from the _ks_.

###### [!Function] `pkcs12-keystore-delete-all-entries!` (_ks_ `pkcs12-keystore?`)

Removes all entries from the _ks_.

