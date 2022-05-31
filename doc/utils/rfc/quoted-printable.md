[§2] (rfc quoted-printable) - Base 64 encode and decode library {#rfc.quoted-printable}
-------------

###### [!Library] `(rfc quoted-printable)` 

This library provides quoted printable encoding and decoding procedures.

### [§3] Encoding procedures

###### [!Function] `quoted-printable-encode`  _bv_ _:key_ _(line-width_ _76)_ _(binary?_ _#f)_

_bv_ must be a bytevector.

Encodes given bytevector to quoted printable encoded bytevector.

The keyword argument _line-width_ specifies where the encode procedure
should put linefeed. If this is less than 1 or #f, encoder does not put
linefeed.

If the keyword argument _binary?_ is not #f, then the procedure encodes
#x0a and #x0b to `=0A` and `=0D` respectively.


###### [!Function] `quoted-printable-encode-string string`  _:key_ _(line-width_ _76)_ _transcoder_ _(binary?_ _#f)_

Convenient procedure for string.

Encodes given _string_ to quoted printable encoded string.

The keyword argument _transcoder_ is used to convert given string to
bytevector. The converted bytevector will be passed to the
`quoted-printable-encode` procedure. The default is utf-8 codec with
NONE eol style.


### [§3] Decoding procedures

###### [!Function] `quoted-printable-decode`  _bv_

_bv_ must be a bytevector.

Decode quoted printable encoded bytevector to original bytevector.


###### [!Function] `quoted-printable-decode-string`  _string_ _:key_ _(transcoder_ _(native-transcoder))_

Convenient procedure.

Decode quoted printable encoded string to original string. The procedure is
using `quoted-printable-decode`.

The keyword argument specifies how to convert the decoded bytevector to string.
If this is #f, the procedure returns raw bytevector.


