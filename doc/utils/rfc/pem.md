[§2] (rfc pem) - PEM format  library {#rfc.pem}
-------------

###### [!Library] `(rfc pem)` 

This library provides PEM format file parser.

Currently only supports RFC 1421 format.


### [§3] Conditions

This library defines these conditions.

###### [!Condition Type] `&pem-error` 

Super condition of all PEM file process related conditions.

###### [!Condition Type] `&invalid-pem-format` 

This condition indicates, given PEM file contains invalid format.

###### [!Function] `invalid-pem-format?`  _obj_

Returns #t, if the given _obj_ is `&invalid-pem-format`,
otherwise #f.

###### [!Condition Type] `&pre-eb-as-boundary` 

This condition indicates, given PEM file contains Pre-Encapsulation 
Boundary as the end of Encapsulated Message without specifying 
`:multiple`.

###### [!Function] `pre-eb-as-boundary?`  _obj_

Returns #t, if the given _obj_ is `&pre-eb-as-boundary`,
otherwise #f.

### [§3] Operations

###### [!Function] `parse-pem`  _in_ _:key_ _(multiple_ _#f)_ _(builder_ _#f)_ _(asn1_ _#f)_ _decoder_

_in_ must be textual input port.

Parses given input port _in_ and returns 2 values, parameter alist and
decoded bytevector.

Keyword arguments

`multiple`
: When this keyword argument is #t, then the procedure returns a list which
  contains alist of parameter and content.
  This parameter is useful for the PEM files which contains multiple contents.

`builder`
: This keyword argument must take a procedure which accept one argument or
  #f. If _builder_ is specified then the given procedure will be called
  to build then contents of the PEM.
  This argument is not correspond with _asn1_ keyword argument and has
  higher priority. So if both arguments are specified, then _builder_    will be used.

`asn1`
: When this keyword argument is #t, then the procedure converts BASE64
  bytevector to ASN.1 object defined in `(asn.1)` library.

`decorder`
: When this keyword argument is specified, it must be a procedure which
  accepts one argument, then the `parse-pem` uses the specified
  procedure to convert body of the PEM content.
  If it's not specified, the procedure uses BASE64 decoding.

The procedure may raise following conditions:

`&invalid-pem-format`
: When given _in_ contains invalid PEM format.

`&pre-eb-as-boundary`
: When given _in_ contains Pre-Encapsulation Boundary as the end of
  Encapsulated Message and `:multiple` is #f.
  For example:
  ``````````scheme
  -----BEGIN FOO-----
  ... foo value ...
  -----BEGIN BAR-----
  ... bar value...
  -----END BAR-----
  ``````````
  parsing PEM like above must specify `:multiple` with true value.
  Otherwise, `&pre-eb-as-boundary` is signaled.



###### [!Function] `parse-pem-file`  _file_ _:rest_ _options_
###### [!Function] `parse-pem-string`  _pem-string_ _:rest_ _options_

Convenient procedures.

Parse given file and PEM string, respectively.

_option_ will be passed to the `parse-pem`.


