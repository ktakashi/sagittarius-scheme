[§3] KDF library - (sagittarius crypto kdfs) {#sagittarius.crypto.kdfs}
------------------------------------------------------

The KDF, Key Derivation Function, library provides various KDF algorithms.

###### [!Library] `(sagittarius crypto kdfs)`

The KDF library, this library exports the procedures listed below sections.

###### [!Function] `pbkdf-1` _P_ _S_ _c_ _dk-len_ :key (_digest_ `*digest:sha-1*`)

_P_and _S_ must be bytevectors.  
_c_ and _dk-len_ must be non-negative integers.

PBKDF1

###### [!Function] `pbkdf-2` _P_ _S_ _c_ _dk-len_ :key (_prf_ `*hmac-sha1-prf*`)

_P_and _S_ must be bytevectors.  
_c_ and _dk-len_ must be non-negative integers.

PBKDF2

###### [!Function] `mac->prf-provider` _scheme_ . _opts_

_scheme_ must be a MAC scheme, e.g. `*mac:hmac*`.

Provides a PRF generator which accepts _S_, secret, 
based on the given _scheme_.  
The _opts_ will be passed to the MAC generation.


###### [!Function] `hkdf` _digest_ _ikm_ _salt_ _info_ _dk-len_

HKDF

###### [!Function] `pkcf12-kdf` _digest_ _pw_ _salt_ _iteration_ _dk-len_

PKCS12 KDF
