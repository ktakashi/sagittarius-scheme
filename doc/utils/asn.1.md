[§2] (asn.1) - Abstract Syntas Notation One library {#asn.1}
-------------

This section describes `(asn.1)` library. The library supports DER and BER
formats. We do not describe DER or BER format here.

###### [!Library] `(asn.1)` 

Top most library of asn.1 procedure collection libraries. There are
multiple of related libraries, however we do not mention those because it might
be changed in future. So do not use the child libraries directly, use this.

### [§3] High level user APIs

###### [!Function] `encode`  _obj_ _:key_ _(encoder_ _der-encode)_

_obj_ must be asn.1-object which described below sections.

If you specify keyword argument _encoder_, it must be generic function
which accepts an object and a binary output port respectively.

Encodes given _obj_ to bytevector. If _obj_ contains BER object, the
procedure encodes it to BER. This might be changed in future to configure which
format this should encode.

If you want to encode to other format such as CER or XER, then you need to
implement an encoder. This procedure does not check given arguments type, just
pass _obj_ to _encoder_ with binary output port.


###### [!Function] `read-asn.1-object`  _in_

_in_ must be binary input port.

Reads asn.1-object from given port. The port must be DER or BER encoded stream
otherwise it raises `&assertion`

### [§3] Middle level user APIs

This section is incompleted.

###### [!Generic] `make-der-application-specific`  _<boolean>_ _<integer>_ _<bytevector>_
###### [!Generic] `make-der-application-specific`  _<integer>_ _<bytevector>_
###### [!Generic] `make-der-application-specific`  _<boolean>_ _<integer>_ _<der-encodable>_
###### [!Generic] `make-der-application-specific`  _<integer>_ _<der-encodable>_

Creates a DER application specific object.

###### [!Generic] `make-der-bit-string`  _<bytevector>_ _<integer>_
###### [!Generic] `make-der-bit-string`  _<bytevector>_

Creates a DER bit string object.

###### [!Generic] `make-der-bmp-string`  _<bytevector>_
###### [!Generic] `make-der-bmp-string`  _<string>_

Creates a DER bmp string object.

###### [!Generic] `make-der-octet-string`  _<bytevector>_
###### [!Generic] `make-der-octet-string`  _<der-encodable>_

Creates a DER octet string object.

###### [!Generic] `make-der-general-string`  _<string>_
###### [!Generic] `make-der-general-string`  _<bytevector>_

Creates a DER general string object.

###### [!Generic] `make-der-ia5-string`  _<string>_
###### [!Generic] `make-der-ia5-string`  _<string>_ _<boolean>_
###### [!Generic] `make-der-ia5-string`  _<bytevector>_

Creates a DER IA5 string object.

###### [!Generic] `make-der-numeric-string`  _<string>_
###### [!Generic] `make-der-numeric-string`  _<string>_ _<boolean>_
###### [!Generic] `make-der-numeric-string`  _<bytevector>_

Creates a DER numeric string object.

###### [!Generic] `make-der-printable-string`  _<string>_
###### [!Generic] `make-der-printable-string`  _<string>_ _<boolean>_
###### [!Generic] `make-der-printable-string`  _<bytevector>_

Creates a DER printable string object.

###### [!Generic] `make-der-t61-string`  _<string>_
###### [!Generic] `make-der-t61-string`  _<bytevector>_

Creates a DER T61 string object.

###### [!Generic] `make-der-universal-string`  _<bytevector>_

Creates a DER universal string object.

###### [!Generic] `make-der-utf8-string`  _<string>_
###### [!Generic] `make-der-utf8-string`  _<bytevector>_

Creates a DER UTF8 string object.

###### [!Generic] `make-der-visible-string`  _<string>_
###### [!Generic] `make-der-visible-string`  _<bytevector>_

Creates a DER visible string object.

###### [!Generic] `make-der-boolean`  _<boolean>_
###### [!Generic] `make-der-boolean`  _<bytevector>_

Creates a DER boolean object.

###### [!Generic] `make-der-enumerated`  _<bytevector>_
###### [!Generic] `make-der-enumerated`  _<integer>_

Creates a DER enumerated object.

###### [!Generic] `make-der-integer`  _<integer>_
###### [!Generic] `make-der-integer`  _<bytevector>_

Creates a DER integer object.

###### [!Generic] `make-der-object-identifier`  _<string>_
###### [!Generic] `make-der-object-identifier`  _<bytevector>_

Creates a DER OID object.

###### [!Generic] `make-der-sequence` 
###### [!Generic] `make-der-sequence`  _<der-encodable>_
###### [!Generic] `make-der-sequence`  _o_ _..._

Creates a DER sequence object.

If the third form is used, _o_s must be list of `<der-encodable>`.


###### [!Generic] `make-der-set` 
###### [!Generic] `make-der-set`  _<der-encodable>_
###### [!Generic] `make-der-set`  _o_ _..._

Creates a DER set object.

If the third form is used, _o_s must be list of `<der-encodable>`.


###### [!Generic] `make-der-null` 

Creates a DER null object.

###### [!Generic] `make-der-generalized-time`  _<string>_
###### [!Generic] `make-der-generalized-time`  _<bytevector>_
###### [!Generic] `make-der-generalized-time`  _<date>_

Creates a DER generalized time object.

###### [!Generic] `make-der-utc-time`  _<string>_
###### [!Generic] `make-der-utc-time`  _<bytevector>_
###### [!Generic] `make-der-utc-time`  _<date>_

Creates a DER UTC time object.

###### [!Generic] `make-der-tagged-object`  _<boolean>_ _<integer>_ _<der-encodable>_
###### [!Generic] `make-der-tagged-object`  _<integer>_ _<der-encodable>_
###### [!Generic] `make-der-tagged-object`  _<integer>_

Creates a DER tagged object.

###### [!Generic] `make-der-external`  _<der-object-identifier>_ _<der-integer>_ _
_ _<asn.1-object>_ _<der-tagged-object>_
###### [!Generic] `make-der-external`  _<der-object-identifier>_ _<der-integer>_ _
_ _<asn.1-object>_ _<integer>_ _<der-object>_

Creates a DER external object.

###### [!Generic] `make-ber-constructed-octet-string`  _<bytevector>_
###### [!Generic] `make-ber-constructed-octet-string`  _l_ _..._

Creates a BER constructed object.

###### [!Generic] `make-ber-application-specific`  _<integer>_ _l_ _..._

Creates a BER application specific object.

###### [!Generic] `make-ber-tagged-object`  _<boolean>_ _<integer>_ _<der-encodable>_

Creates a BER tagged object.

###### [!Generic] `make-ber-sequence`  _l_ _..._

Creates a BER sequence object.

_l_s must be list of `<der-encodable>`.


###### [!Generic] `make-ber-set`  _l_ _..._

Creates a BER set object.

_l_s must be list of `<der-encodable>`.


###### [!Generic] `make-ber-null` 

Creates a BER null object.

### [§3] Low level User APIs

This section is incompleted. Here must desribe classes defined in `(asn.1)`library.