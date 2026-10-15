[§2] (rfc punycode) - Punycode encoder/decoder {#rfc.punycode}
-------------

###### [!Library] `(rfc punycode)`

This library provides punycode encoding and decoding defined by 
[RFC 3492](https://www.rfc-editor.org/info/rfc3492) and
[RFC 5891](https://www.rfc-editor.org/info/rfc5891).

Example:

```scheme
(import (rnrs)
        (rfc punycode))

(punycode-encode "例え")     ;; -> "r8jz45g"
(punycode-decode "r8jz45g") ;; -> "例え"

(punycode-label? "xn--bcher-kva") ;; -> #t
```

###### [!Function] `punycode-encode`  _str_
###### [!Function] `punycode-decode`  _str_

Encodes and decodes the given _str_ to or from punycode.

`punycode-decode` signals an assertion if the given _str_
can't be decoded.

###### [!Function] `punycode-label?`  _str_

Returns `#t` if the given _str_ is a punycode label.  
A punycode label must start with `xn--` prefix followed by
the properly encoded punycode.
