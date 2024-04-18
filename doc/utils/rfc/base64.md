[§2] (rfc base64) - Base 64 encode and decode library {#rfc.base64}
-------------

###### [!Library] `(rfc base64)` 

This library provides Base 64 encoding and decoding procedures.

### [§3] Encoding procedures

###### [!Function] `base64-encode`  _in_ :key (_line-width_ 76) (_padding?_ #t)
###### [!Function] `base64url-encode`  _in_ :key (_line-width_ #f) (_padding?_ #f)

_in_ must be either a bytevector or binary input port.

Encodes given input _in_ to Base 64 encoded bytevector.

The keyword argument _line-width_ specifies where the encode procedure
should put linefeed. If this is less than 1 or #f, encoder does not put
linefeed.

The keyword argument _padding?_ controls if the result encoded value
contains padding character `#\=` or not. If this is #f, then the result
value won't contain padding character.

The `base64url-encode` encodes the given input to Base 64 URL safe
encoded bytevector. Which doesn't use `+` and `/`.


###### [!Function] `base64-encode-string`  _string_ :key (_line-width_ 76) _transcoder_ (_padding?_ #t)
###### [!Function] `base64url-encode-string`  _string_ :key (_line-width_ #f) _transcoder_ (_padding?_ #f)

Convenient procedure for string.

Encodes given _string_ to Base 64 encoded string.

The keyword argument _transcoder_ is used to convert given string to
bytevector. The converted bytevector will be passed to the `base64-encode`
procedure. The default value is a transcoder with UTF-8 codec with EOL
style none.

The keyword argument _padding?_ is the same as `base64-encode`.

The `base64url-encode-string` encodes the given input to Base 64 URL safe
encoded bytevector. Which doesn't use `+` and `/`.


###### [!Function] `open-base64-encode-input-port`  _source_ :key (_owner?_ #f) (_line-width_ #f) (_padding?_ #t)
###### [!Function] `open-base64-encode-output-port`  _sink_ :key (_owner?_ #f) (_line-width_ #f) (_padding?_ #t)
###### [!Function] `open-base64url-encode-input-port`  _source_ :key (_owner?_ #f) (_line-width_ #f) (_padding?_ #f)
###### [!Function] `open-base64url-encode-output-port`  _sink_ :key (_owner?_ #f) (_line-width_ #f) (_padding?_ #f)

Creates binary Base64 encode input and output port, respectively.

_source_ must be binary inpurt port.

The input port reads bytes from _source_ and returns Base64 encoded
result.

_sink_ must be binary inpurt port.

The output port puts encoded bytes to _sink_. The port must be closed
to finish the encoding process properly.

The keyword argument _padding?_ is the same as `base64-encode`.

The `open-base64url-encode-input-port` and
`open-base64url-encode-output-port` encode to Base64 URL safe encode.


### [§3] Decoding procedures

###### [!Function] `base64-decode`  _in_
###### [!Function] `base64url-decode`  _in_

_in_ must be a bytevector or binary input port.

Decode Base 64 encoded input _in_ to original bytevector.

The `base64url-decode` decodes Base64 URL safe encoded value.


###### [!Function] `base64-decode-string`  _string_ :key (_transcoder_ `(native-transcoder)`)
###### [!Function] `base64url-decode-string`  _string_ :key (_transcoder_ `(native-transcoder)`)

Convenient procedure.

Decode Base 64 encoded string to original string. The procedure is using
`base64-decode`.

The keyword argument specifies how to convert the decoded bytevector to string.
If this is #f, the procedure returns raw bytevector.

The `base64url-decode-string` decodes Base64 URL safe encoded value.


###### [!Function] `open-base64-decode-input-port`  _source_ :key (_owner?_ #f)
###### [!Function] `open-base64-decode-output-port`  _sink_ :key (_owner?_ #f)
###### [!Function] `open-base64url-decode-input-port`  _source_ :key (_owner?_ #f)
###### [!Function] `open-base64url-decode-output-port`  _sink_ :key (_owner?_ #f)

Creates binary Base64 decode input and output port, respectively.

_source_ must be binary inpurt port.

The input port reads Base64 encoded bytes from _source_ and returns 
decoded results.

_sink_ must be binary inpurt port.

The output port puts decoded bytes to _sink_. The port must be closed
to finish the encoding process properly.

The `open-base64url-decode-input-port` and
`open-base64url-decode-output-port` decode Base64 URL safe encoded
value.


### [§3] Low level APIs

Both encode and decode procedures are using encoder and decoder. Both
encoder and decoder are just a procedure which takes 2 arguments
_get_ and _put_ and not reentrant as they have own internal
buffer to process the input.

###### [!Function] `make-base64-encoder`  :key (_encode-table_ `*base64-encode-table*`) (_line-width_ 76) (_padding?_ #t) (_linefeeds_ #f)

_encode-table_ must be a valid Base 32 encode table.  
_line-width_ must be either `#f` or integer.  
_padding?_ must be a boolean value.  
_linefeeds_ must be `#f` or u8 list reporesents end of line.__

Creates a Base64 encoder. An encoder is a procedure which takes 2
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

The following shows how to make Base64 encoder with line break of CRLF.

```scheme
(define (base64-encode-w/crlf bv)
  (let-values (((out e) (open-bytevector-output-port)))
    (define (put v)
      (if v
	  (put-u8 out v)
	  (begin (put-u8 out #x0d) (put-u8 out #x0a))))
    (define inp (open-bytevector-input-port bv))
    (define (in) (get-u8 inp))
    (define encoder (make-base64-encoder))
    (do () ((encoder in put)))
    (e)))
```



###### [!Function] `make-base64-decoder` :key (_decode-table_ `*base64-decode-table*`)

Creates a Base64 decoder. A decoder is a procedure which takes 2
arguments _get_ and _put_.

_get_ is the same as the one from encoder.

_put_ must be a procedure takes 1 arguments which is a fixnm of
range 0 to 255. The value is always a decoded byte.



###### [!Constant] `*base64-encode-table*` 
###### [!Constant] `*base64-decode-table*` 
###### [!Constant] `*base64-encode-url-table*` 
###### [!Constant] `*base64-decode-url-table*` 

Default encode or decode tables. If the name contains `url`, then it
is suitable for "base64url".



