[§2] (rfc base32) - Base 32 encode and decode library {#rfc.base32}
-------------

###### [!Library] `(rfc base32)` **[@since] `0.9.12`** 

This library provides Base 64 encoding and decoding procedures.

### [§3] Encoding procedures

###### [!Function] `base32-encode`  _in_ . _keyword-options_
###### [!Function] `base32hex-encode`  _in_ . _keyword-options_

_in_ must be either a bytevector or binary input port.

Encodes given input _in_ to Base 64 encoded bytevector.


The `base32hex-encode` encodes the given input to Base 32 Hex
encoded bytevector.

The _keyword-options_ will be passed to the encoder. See `make-base32-encoder`
procedure for more detail.


###### [!Function] `base32-encode-string`  _string_ :key _transcoder_ :allow-other-keys _keyword-options_
###### [!Function] `base32hex-encode-string`  _string_ :key _transcoder_ :allow-other-keys _keyword-options_

Convenient procedure for string.

Encodes given _string_ to Base 64 encoded string.

The keyword argument _transcoder_ is used to convert given string to
bytevector. The converted bytevector will be passed to the `base32-encode`procedure. The default value is a transcoder with UTF-8 codec with EOL
style none.

The `base32hex-encode-string` encodes the given input to Base 32 Hex
encoded bytevector.

The _keyword-options_ will be passed to the encoder. See `make-base32-encoder`
procedure for more detail.


###### [!Function] `open-base32-encode-input-port`  _source_ :key (_owner?_ #f) :allow-other-keys _keyword-options_
###### [!Function] `open-base32-encode-output-port`  _sink_ :key (_owner?_ #f) :allow-other-keys _keyword-options_
###### [!Function] `open-base32hex-encode-input-port`  _source_ :key (_owner?_ #f) :allow-other-keys _keyword-options_
###### [!Function] `open-base32hex-encode-output-port`  _sink_ :key (_owner?_ #f) :allow-other-keys _keyword-options_

Creates binary Base32 encode input and output port, respectively.

_source_ must be binary inpurt port.

The input port reads bytes from _source_ and returns Base32 encoded
result.

_sink_ must be binary inpurt port.

The output port puts encoded bytes to _sink_. The port must be closed
to finish the encoding process properly.

The `open-base32hex-encode-input-port` and
`open-base32hex-encode-output-port` encode to Base32 Hex encode.

The _keyword-options_ will be passed to the encoder. See `make-base32-encoder`
procedure for more detail.

### [§3] Decoding procedures

###### [!Function] `base32-decode`  _in_ . _keyword-options_
###### [!Function] `base32hex-decode`  _in_ . _keyword-options_

_in_ must be a bytevector or binary input port.

Decode Base 64 encoded input _in_ to original bytevector.

The `base32hex-decode` decodes Base32 Hex encoded value.

The _keyword-options_ will be passed to the encoder. See `make-base32-decoder`
procedure for more detail.


###### [!Function] `base32-decode-string`  _string_ :key _(transcoder_ _(native-transcoder))_ :allow-other-keys _keyword-options_
###### [!Function] `base32hex-decode-string`  _string_ :key _(transcoder_ _(native-transcoder))_ :allow-other-keys _keyword-options_

Convenient procedure.

Decode Base 64 encoded string to original string. The procedure is using
`base32-decode`.

The keyword argument specifies how to convert the decoded bytevector to string.
If this is #f, the procedure returns raw bytevector.

The `base32hex-decode-string` decodes Base32 Hex safe encoded value.

The _keyword-options_ will be passed to the encoder. See `make-base32-decoder`
procedure for more detail.


###### [!Function] `open-base32-decode-input-port`  _source_ :key (_owner?_ #f) :allow-other-keys _keyword-options_
###### [!Function] `open-base32-decode-output-port`  _sink_ :key (_owner?_ #f) :allow-other-keys _keyword-options_
###### [!Function] `open-base32hex-decode-input-port`  _source_ :key (_owner?_ #f) :allow-other-keys _keyword-options_
###### [!Function] `open-base32hex-decode-output-port`  _sink_ :key (_owner?_ #f) :allow-other-keys _keyword-options_

Creates binary Base32 decode input and output port, respectively.

_source_ must be binary inpurt port.

The input port reads Base32 encoded bytes from _source_ and returns 
decoded results.

_sink_ must be binary inpurt port.

The output port puts decoded bytes to _sink_. The port must be closed
to finish the encoding process properly.

The `open-base32hex-decode-input-port` and
`open-base32hex-decode-output-port` decode Base32 Hex safe encoded
value.

The _keyword-options_ will be passed to the encoder. See `make-base32-decoder`
procedure for more detail.


### [§3] Low level APIs

Both encode and decode procedures are using encoder and decoder. Both
encoder and decoder are just a procedure which takes 2 arguments
_get_ and _put_ and not reentrant as they have own internal
buffer to process the input.

###### [!Function] `make-base32-encoder`  :key (_encode-table_ `*base32-encode-table*`) (_line-width_ 76) (_padding?_ #t) (_linefeeds_ #f)
###### [!Function] `make-base32hex-encoder`  :key (_encode-table_ `*base32hex-encode-table*`) (_line-width_ 76) (_padding?_ #t) (_linefeeds_ #f)

_encode-table_ must be a valid Base 32 encode table.  
_line-width_ must be either `#f` or integer.  
_padding?_ must be a boolean value.  
_linefeeds_ must be `#f` or u8 list reporesents end of line.__

Creates a Base32 encoder. An encoder is a procedure which takes 2
arguments _get_ and _put_.

_get_ must be a procedure takes 0 argument and it must return one
of the followings; a fixnum of range 0 to 255, EOF object, or negative
integer. The fixnum value is treated as an input of the encoding
value. The negative integer value is treated as a continue
marker. When the encoder receives this value, then it won't encode
until the next value is available.

_put_ must be a procedure takes 1 argument which is either a
fixnum of range 0 to 255 or `#f`. The fixnum is the encoded value
of the input. `#f` indicates line break point, so user can
determine which line break this encoder should use.

The keyword argument _encode-table_ specifies the encoding rule.
The procedure name represents the default value of the rule, however
if you specify the other table, then it can encode the value according
to the given value.

The keyword argument _line-width_ specifies where the encode procedure
should put linefeed. If this is less than 1 or #f, encoder does not put
linefeed.

The keyword argument _padding?_ controls if the result encoded value
contains padding character `#\=` or not. If this is #f, then the result
value won't contain padding character.

The keyword argument _linefeeds_ controls the end of line. By default,
it emits `LF (0x0a)`. If you want to put CRLF, then you can specify this
value with `(#x0d #x0a)`


###### [!Function] `make-base32-decoder` :key (_decode-table_ `*base32-decode-table*`)
###### [!Function] `make-base32hex-decoder` :key (_decode-table_ `*base32hex-decode-table*`)

_decode-table_ must be a valid Base 32 decode table.  

Creates a Base32 decoder. A decoder is a procedure which takes 2
arguments _get_ and _put_.

_get_ is the same as the one from encoder.

_put_ must be a procedure takes 1 arguments which is a fixnm of
range 0 to 255. The value is always a decoded byte.

The keyword argument _decode-table_ specifies the decoding rule.
The procedure name represents the default value of the rule, however
if you specify the other table, then it can decode the value according
to the given value.


###### [!Constant] `*base32-encode-table*` 
###### [!Constant] `*base32-decode-table*` 
###### [!Constant] `*base32hex-encode-table*` 
###### [!Constant] `*base32hex-decode-table*` 

Default encode or decode tables. If the name contains `url`, then it
is suitable for "base32hex".



