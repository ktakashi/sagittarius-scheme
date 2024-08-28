[§2] (rfc cmac) - CMAC library {#rfc.cmac}
-------------

This section describes the CMAC extension of hash algorithm. The CMAC itself
is described [RFC 4493](http://tools.ietf.org/html/rfc4493).

###### [!Library] `(rfc cmac)` 

This library is kept only for backward compatibility purpose. For
newer application, use [`(sagittarius crypto mac)`](#sagittarius.crypto.mac).

Provides CMAC hash algorithm.

The RFC is defined for only AES block cipher however the implementation
can take any block cipher such as DES3.

CAUTION: only AES cipher is tested.

###### [!Function] `make-cmac` _key_ _opts_ _..._

Creates a MAC object of CMAC algorithm.
_key_ must be a bytevector.


###### [!Function] `generate-mac` (_mac_ `mac?`) (_message_ `bytevector?`) :optional _length_
###### [!Function] `generate-mac!` (_mac_ `mac?`) (_message_ `bytevector?`) (_out_ `bytevector?`) :optional _start_ _length_
###### [!Function] `verify-mac` (_mac_ `mac?`) (_message_ `bytevector?`) (_auth-mac_ `bytevector?`)
###### [!Function] `mac-init!` (_mac_ `mac?`)
###### [!Function] `mac-process!` (_mac_ `mac?`) (_message_ `bytevector?`) :optional _start_ _length_
###### [!Function] `mac-done!` (_mac_ `mac?`) (_out_ `bytevector?`) :optional _start_ _length_

These procedures are re-exported from `(sagittarius crypto mac)`.


###### [!Variable] `CMAC` **[@deprecated]**

The name for CMAC hash algorithm, which can be used with `(math hash)` library.

The following example explains, how to use it.

```scheme
(import (rnrs) (rfc cmac) (math) (crypto))

(define K (generate-secret-key AES (integer->bytevector 
				    #x2b7e151628aed2a6abf7158809cf4f3c)))

(define aes-cipher (cipher AES K))

(hash CMAC #vu8() :cipher aes-cipher)
;; => #vu8(187 29 105 41 233 89 55 40 127 163 125 18 155 117 103 70)

;; for AES-CMAC-96
(hash CMAC #vu8() :cipher aes-cipher :size (/ 96 8))
;; => #vu8(187 29 105 41 233 89 55 40 127 163 125 18)
```


