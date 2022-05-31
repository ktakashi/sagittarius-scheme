[§2] (binary io) - Binary I/O utilities {#util.binary.io}
-------------

###### [!Library] `(binary data)` 

Binary I/O utility. In real world you sometimes want to treat
binary port like textual port (e.g. `get-line` for binary port).
This library exports those convenient procedures


### [§3] Binary I/O

###### [!Function] `get-line`  _in_ _:key_ _eol_ _(transcoder_ _#f)_

_in_ must be binary input port.

Reads a bytevector from _in_ until it hits the _eol_ data. _eol_can be multiple length such as `#vu8(#x0d #x0a)`. Default value is
`#vu8(#x0a)`If keyword argument _transcoder_ is given, then returning value will be
converted to string.


###### [!Function] `put-u16`  _out_ _v_ _endian_
###### [!Function] `put-s16`  _out_ _v_ _endian_
###### [!Function] `put-u32`  _out_ _v_ _endian_
###### [!Function] `put-s32`  _out_ _v_ _endian_

Re-exported procedure from `(sagittarius)` for convenience. See
[Sagittarius extensions](#ext.sagittarius).


###### [!Function] `put-u64`  _out_ _v_ _endian_
###### [!Function] `put-s64`  _out_ _v_ _endian_
###### [!Function] `put-f32`  _out_ _v_ _endian_
###### [!Function] `put-f64`  _out_ _v_ _endian_

_out_ must be binary output port. _endian_ must be a value
returned from `endianness` macro.

Write _v_ to _out_ as unsigned/signed 16/32/64 bit integer or
32/64 bit floating number.


###### [!Function] `get-u16`  _in_ _endian_
###### [!Function] `get-s16`  _in_ _endian_
###### [!Function] `get-u32`  _in_ _endian_
###### [!Function] `get-s32`  _in_ _endian_

Re-exported procedure from `(sagittarius)` for convenience. See
[Sagittarius extensions](#ext.sagittarius).


###### [!Function] `get-u64`  _in_ _endian_
###### [!Function] `get-s64`  _in_ _endian_
###### [!Function] `get-f32`  _in_ _endian_
###### [!Function] `get-f64`  _in_ _endian_

_in_ must be binary input port. _endian_ must be a value
returned from `endianness` macro.

Read a number from _in_ as unsigned/signed 16/32/64 bit integer or
32/64 bit floating number.


### [§3] Chunk buffered port

Default binary input port requires bytevector however if users want to
handle bigger data then it would not be suitable. Chunk buffered port
is a buffered port which buffer is chunks of bytevector so that it doesn't
allocate huge memory.

###### [!Function] `->chunked-binary-input-port`  _->chunks_ _:key_ _chunk-size_

Creates chunk buffered port.

_->chunks_ must be a procedure which takes one argument, _chunk-size_.
And must return a list of bytevectors which size is _chunk-size_ except
the last element.

The keyword argument _chunk-size_ is specified then it must be a positive
integer. By default `+default-chunk-size+` is used.

###### [!Constant] `+default-chunk-size+` 

Default chunk size of chunk buffered port.



###### [!Function] `input-port->chunked-binary-input-port`  _iport_ _:key_ _chunk-size_ _threshold_

Creates chunk buffered port from given _iport_.

_iport_ must be a binary input port.

The keyword argument _threshold_ is specified, it must be a positive
integer, then the procedure only reads the number of _threshold_ bytes
from _iport_.


