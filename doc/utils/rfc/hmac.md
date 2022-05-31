[§2] (rfc hmac) - HMAC library {#rfc.hmac}
-------------

This section describes the HMAC extension of hash algorithm. The HMAC itself
is described [RFC 2104](http://tools.ietf.org/html/rfc2104).
The provided library make user be able to use the algorithm with the APIs of
math library's hash algorithm APIs. For the APIs detail, see
[Hash operations](#math.hash).

###### [!Library] `(rfc hmac)` 

Provides HMAC hash algorithm.

###### [!Variable] `HMAC` 

The name for HMAC hash algorithm.

The following example explains, how to use it.

``````````scheme
(import (rnrs) (rfc hmac) (math))
;; the keyword arguments can be omitted, then it will use an empty bytevector
;; as HMAC key and SHA-1 algorithm as digest algorithm.
(define hmac (hash-algorithm HMAC :key (string->utf8 "hmac key") :hash SHA-1))
(hash hmac (string->utf8 "test"))
;; -> #vu8(57 255 153 118 15 133 255 73 12 199 199 115 185 32 43 225 61 254 159 94)
``````````



