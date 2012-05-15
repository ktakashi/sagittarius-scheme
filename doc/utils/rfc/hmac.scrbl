@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.hmac"]{(rfc hmac) - HMAC library}

This section describes the HMAC extension of hash algorithm. The HMAC itself
is described @hyperlink[:href "http://tools.ietf.org/html/rfc2104"]{RFC 2104}.
The provided library make user be able to use the algorithm with the APIs of
math library's hash algorithm APIs. For the APIs detail, see
@secref["math.hash"]{Hash operations}.

@define[Library]{@name{(rfc hmac)}}
@desc{Provides HMAC hash algorithm.}

@define[Variable]{@name{HMAC}}
@desc{The name for HMAC hash algorithm.

The following example explains, how to use it.

@codeblock{
(import (rnrs) (rfc hmac) (math))
;; the keyword arguments can be omitted, then it will use an empty bytevector
;; as HMAC key and SHA-1 algorithm as digest algorithm.
(define hmac (hash-algorithm HMAC :key (string->utf8 "hmac key") :hash SHA-1))
(hash hmac (string->utf8 "test"))
;; -> #vu8(57 255 153 118 15 133 255 73 12 199 199 115 185 32 43 225 61 254 159 94)
}
}
