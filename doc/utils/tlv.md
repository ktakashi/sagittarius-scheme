[§2] (tlv) - TLV library {#util.tlv}
-------------

###### [!Library] `(tlv)` 

This library provides TLV (tag length value) data operation procedures.


### [§3] High level APIs

###### [!Function] `make-emv-tlv-parser`  _:key_ _(object-builder_ _tlv-builder)_

Creates EMV type TLV parser.

This procedure returns a procedure which accepts a binary port as its argument.

The keyword argument `object-builder` specifies how to construct a TLV
object. The default value is `tlv-builder`.


###### [!Function] `tlv-object?`  _o_

Returns #t if the given object is TLV object otherwise #f.

###### [!Function] `tlv-tag`  _tlv_

Returns TLV tag of given TLV object _tlv_.

###### [!Function] `tlv-data`  _tlv_

Returns TLV binary data if the given TLV object _tlv_ is not
constructed, otherwise #f.

###### [!Function] `tlv-components`  _tlv_

Returns TLV components of given TLV object _tlv_. If the _tlv_is not constructed, this returns `()`.

###### [!Function] `tlv->bytevector`  _tlv_

Converts given TLV object _tlv_ to bytevector.

###### [!Function] `write-tlv`  _tlv_ _:optional_ _(out_ _(current-output-port))_

_out_ must be binary output port.

Writes given TLV object _tlv_ to _out_ as TLV data.


###### [!Function] `dump-tlv`  _:optional_ _(out_ _(current-output-port))_

Dump given TLV object _tlv_ as human readable form.

### [§3] Custom object builder

Sometimes default TLV object is not convenient to use. Then users can create
own object from TLV data passing own object builder to the
`make-emv-tlv-parser` procedure.

###### [!Function] `tlv-builder`  _first-byte_ _tag_ _data_ _constructed?_

Default TLV object builder. User should not use this procedure directly.

_first-byte_ is the first byte of the leading TLV object.

_tag_ is the read tag of the TLV object.

_data_ is either a list of TLV object or bytevector. User can check it with
_constructed?_ argument.

_constructed?_ indicates if the TLV object is constructed or not. If this is
#t then _data_ is a list of TLV objects.


