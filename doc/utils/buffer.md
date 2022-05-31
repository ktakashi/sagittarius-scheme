[§2] (util buffer) - Buffer utilities {#util.buffer}
-------------

###### [!Library] `(util buffer)` 

This library provides buffer utitlities. Currently, it only provides
pre-allocated buffer and its procedures.


### [§3] Pre-allocated buffer

###### [!Record Type] `<pre-allocated-buffer>` 

Parent type of pre-allocated buffer. This library doesn't expose
the record constructor for this type.

This type contains `buffer` and `size` fields.


###### [!Function] `pre-allocated-buffer?`  _obj_

Returns #t if given _obj_ is a pre-allocated buffer. Otherwise #f.

###### [!Function] `pre-allocated-buffer-buffer`  _buffer_

Returns value of `buffer` field of given _buffer_.

The type of `buffer` field is implementation specific.


###### [!Function] `pre-allocated-buffer-size`  _buffer_

Returns value of `size` field of given _buffer_.

The returning value shall represents how much buffer of the given _buffer_is consumed.


###### [!Function] `pre-allocated-buffer-reset!`  _buffer_

Sets 0 to the `size` field of given `buffer`.

### [§3] Conditions

###### [!Condition] `&pre-allocated-buffer-overflow` 

Buffer overflow condition. This is raised when an operation tries
to put data exceed the buffer size.

This condition have `data` field which contains overflowing data.


###### [!Function] `pre-allocated-buffer-overflow?`  _obj_

Returns #t if given _obj_ is `&pre-allocated-buffer-overflow`condition, otherwise #f.


###### [!Function] `pre-allocated-buffer-overflow-data`  _condition_

Retrieves `data` field value of _condition_.

The _condition_ must be a `&pre-allocated-buffer-overflow` condition.


### [§3] Binary pre-allocated buffer

Binary pre-allocated buffer can be used when users don't want to allocate
bytevector number of times. This buffer can be used like a bytevector.

###### [!Function] `make-binary-pre-allocated-buffer`  _bv_

Creates pre-allocated buffer with given bytevector _bv_.

The returning value is a subtype of `<pre-allocated-buffer>`.


###### [!Function] `binary-pre-allocated-buffer?`  _obj_

Returns #t if given _obj_ is binary pre-allocated buffer, 
otherwise #f.

#### [§4] Operations

###### [!Function] `binary-pre-allocated-buffer-put-u8!`  _binary-buffer_ _u8_
###### [!Function] `binary-pre-allocated-buffer-put-u16!`  _binary-buffer_ _u16_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-put-u32!`  _binary-buffer_ _u32_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-put-u64!`  _binary-buffer_ _u64_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-put-s8!`  _binary-buffer_ _s8_
	  
###### [!Function] `binary-pre-allocated-buffer-put-s16!`  _binary-buffer_ _s16_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-put-s32!`  _binary-buffer_ _s32_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-put-s64!`  _binary-buffer_ _s64_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-put-f32!`  _binary-buffer_ _f32_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-put-f64!`  _binary-buffer_ _f64_ _endianness_

Appending given integer or flonum to _binary-buffer_. Setting
given values uses the following procedures, respectively:

`bytevector-u8-set!``bytevector-u16-set!``bytevector-u32-set!``bytevector-u64-set!``bytevector-s8-set!``bytevector-s16-set!``bytevector-s32-set!``bytevector-s64-set!``bytevector-ieee-single-set!``bytevector-ieee-double-set!`The _endianness_ is passed to the above procedures if required.

This procedure also updates the `size` field of _binary-buffer_.


###### [!Function] `binary-pre-allocated-buffer-put-bytevector!`  _binary-buffer_ _bv_ _:optional_ _start_ _count_

Appending given bytevector _bv_ to _binary-buffer_.

Optional arguments _start_ and _count_ specifies from where of
_bv_ and how much bytes need to be put. By default, _start_ is 0
and _count_ is `(- (bytevector-length _bv_) _start_)`.


This procedure also updates the `size` field of _binary-buffer_.


###### [!Function] `binary-pre-allocated-buffer-set-u8!`  _binary-buffer_ _index_ _u8_
###### [!Function] `binary-pre-allocated-buffer-set-u16!`  _binary-buffer_ _index_ _u16_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-set-u32!`  _binary-buffer_ _index_ _u32_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-set-u64!`  _binary-buffer_ _index_ _u64_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-set-s8!`  _binary-buffer_ _index_ _s8_
###### [!Function] `binary-pre-allocated-buffer-set-s16!`  _binary-buffer_ _index_ _s16_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-set-s32!`  _binary-buffer_ _index_ _s32_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-set-s64!`  _binary-buffer_ _index_ _s64_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-set-f32!`  _binary-buffer_ _index_ _f32_ _endianness_
###### [!Function] `binary-pre-allocated-buffer-set-f64!`  _binary-buffer_ _index_ _f64_ _endianness_

Setting given integer/flonum to _binary-buffer_. These procedures
are anology of the following procedures, respectively:

`bytevector-u8-set!``bytevector-s8-set!``bytevector-u16-set!``bytevector-u32-set!``bytevector-u64-set!``bytevector-s16-set!``bytevector-s32-set!``bytevector-s64-set!``bytevector-ieee-single-set!``bytevector-ieee-double-set!`The _endianness_ is passed to the above procedures if required.

This procedure updates the `size` field of _binary-buffer_ if
sum of given _index_ and number of bytes set in the buffer exceeds
the size of the buffer.


###### [!Function] `binary-pre-allocated-buffer-set-bytevector!`  _binary-buffer_ _index_ _bv_ _:optional_ _start_ _count_

Sets the given _bv_ to _binary-buffer_ at position of 
_index_.

Optional arguments _start_ and _count_ specifies from where of
_bv_ and how much bytes need to be put. By default, _start_ is 0
and _count_ is `(- (bytevector-length _bv_) _start_)`.

This procedure updates the `size` field of _binary-buffer_ if
sum of given _index_ and number of bytes set in the buffer exceeds
the size of the buffer.


All above operations may raises an `&pre-allocated-buffer-overflow`,
when it tries to exceed the pre-allocated buffer.

###### [!Function] `binary-pre-allocated-buffer-can-store?`  _binary-buffer_ _count_ _:optional_ _position_

Returns #t if _binary-buffer_ can store _count_ bytes.

If optional argument _position_ is given, then the procedure check
from the _position_.


###### [!Function] `binary-pre-allocated-buffer-swap!`  _binary-buffer_ _new-buf_ _new-size_

Swaps the buffer of _binary-buffer_ with _new-buf_and _new-size_.


###### [!Function] `binary-pre-allocated-buffer-get-bytevector-n!`  _binary-buffer_ _input-port_ _n_ _:optional_ _position_

Reads _n_ bytes from given _input-port_ and store it to
_binary-buffer_.

If optional argument _position_ is given, then the procedure stores
from the _position_.


###### [!Function] `crop-binary-buffer`  _binary-buffer_

Returns newly allocated bytevector which contains range of
0 to `size` field value of `buffer` field.


###### [!Function] `->binary-pre-allocated-buffer-output-port`  _binary-buffer_

Converts the given _binary-buffer_ to binary output port.

If port operations try to exceed the pre-allocated buffer, then
it raises `&pre-allocated-buffer-overflow`.


