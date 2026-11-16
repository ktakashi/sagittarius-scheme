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

The keystore library, this library exports the procedures listed
blow sections.

###### [!Function] `pkcs12-keystore?` _obj_

Returns `#t` if the given _obj_ is PKCS#12 keystore, otherwise `#f`.

###### [!Function] `make-pkcs12-keystore`

Creates an empty PKCS#12 keystore.

### [§4] PKCS#12 integrity protection

The default integrity descriptor remains the traditional PKCS#12
HMAC-SHA-256 mechanism. To generate RFC 9879 PBMAC1 integrity protection,
select `*pkcs12-integrity-descriptor:pbmac1/pbkdf2/hmac-sha256*` when
creating a keystore with `(sagittarius crypto pkcs keystore)`, or use
`pkcs12-keystore-integrity-descriptor-set!` before serializing it. The
descriptor is also exported by `(sagittarius crypto keystores pkcs12)`.
Reading a keystore automatically selects the mechanism from its MAC
algorithm identifier; traditional PKCS#12 files remain supported. Use
`write-pkcs12-keystore` with a binary output port to save a PBMAC1-protected
`.p12` file and `read-pkcs12-keystore` with a binary input port to load it.

PBMAC1 generation uses PBKDF2 with HMAC-SHA-256 as both its PRF and MAC,
and explicitly encodes a 32-byte derived key. It uses UTF-8 password bytes,
without a BOM or added NUL terminator; an actual NUL in the password is
preserved. Traditional PKCS#12 integrity uses its existing password encoding.
The outer `MacData` salt and iteration count are generated for compatibility
but ignored when reading PBMAC1. The inner PBKDF2 parameters are authoritative.

###### [!Variable] `*pkcs12-integrity-descriptor:pbmac1/pbkdf2/hmac-sha256*`

Selects RFC 9879 PBMAC1 for PKCS#12 output. The existing
`*pkcs12-integrity-salt-size*` and `*pkcs12-integrity-iteration-count*`
parameters control its PBKDF2 salt size and iteration count.

###### [!Parameter] `*pkcs12-pbmac1-max-iterations*`

Maximum accepted PBKDF2 iteration count for PBMAC1 verification and
generation (default 1,000,000). Choose a lower limit when processing
untrusted files under tighter CPU budgets. PBMAC1 also rejects empty salts,
salts over 1024 bytes, missing key lengths, and derived key lengths outside
20 to 128 bytes. Only SHA-2 HMAC algorithms are accepted as PRF and MAC;
unsupported or malformed parameters cause an error rather than falling back
to legacy integrity verification.

The `(sagittarius crypto pkcs pbes)` library exports
`make-pbmac1-x509-algorithm-identifier`, `make-pkcs-pbmac1-params`,
`pkcs-pbmac1-params?`, and accessors for the nested key derivation and
message authentication algorithm identifiers. The constructor accepts
two X.509 algorithm identifiers (KDF and MAC), respectively; it does not
perform policy validation until the identifier is used for PKCS#12
verification.

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

