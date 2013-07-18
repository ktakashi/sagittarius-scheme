@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.cmac"]{(rfc cmac) - CMAC library}

This section describes the CMAC extension of hash algorithm. The CMAC itself
is described @hyperlink[:href "http://tools.ietf.org/html/rfc4493"]{RFC 4493}.
The provided library make user be able to use the algorithm with the APIs of
math library's hash algorithm APIs. For the APIs detail, see
@secref["math.hash"]{Hash operations}.

@define[Library]{@name{(rfc cmac)}}
@desc{Provides CMAC hash algorithm.}

@define[Variable]{@name{CMAC}}
@desc{The name for CMAC hash algorithm.

The following example explains, how to use it.

@codeblock{
(import (rnrs) (rfc cmac) (math) (crypto))

(define K (generate-secret-key AES (integer->bytevector 
				    #x2b7e151628aed2a6abf7158809cf4f3c)))

(define aes-cipher (cipher AES K))

(hash CMAC #vu8() :cipher aes-cipher)
;; => #vu8(187 29 105 41 233 89 55 40 127 163 125 18 155 117 103 70)

;; for AES-CMAC-96
(hash CMAC #vu8() :cipher aes-cipher :size (/ 96 8))
;; => #vu8(187 29 105 41 233 89 55 40 127 163 125 18)
}

The RFC is defined for only AES block cipher however the implementation
can take any block cipher such as DES3.

CAUTION: only AES cipher is tested.
}
