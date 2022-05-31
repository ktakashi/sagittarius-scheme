[§2] (rfc uri) - Parse and construct URIs {#rfc.uri}
-------------

###### [!Library] `(rfc uri)` 

This library provides RFC3986 'URI Generic Syntax' procedures.

###### [!Function] `uri-parse`  _uri_

_uri_ must be string.

Parses given _uri_ and returns following 7 values;

- _scheme_
- _user-info_
- _host_
- _port_
- _path_
- _query_
- _fragment_

Following examples are from RFC3986 text;

``````````scheme
   foo://example.com:8042/over/there?name=ferret#nose
   \_/   \______________/\_________/ \_________/ \__/
    |           |            |            |        |
 scheme     authority       path        query   fragment
    |   _____________________|__
   / \ /                        \
   urn:example:animal:ferret:nose
``````````

authority = [ _user-info_ "@" ] _host_ [ ":" _port_ ]

If given _uri_ does not contain the part described above, it will be #f.
ex)
``(uri-parse "http://localhost")`` => ``(values http #f localhost #f #f #f #f)``



###### [!Function] `uri-scheme&specific`  _uri_

_uri_ must be string.

Parse given _uri_ into scheme and rest. Returns the 2 values.


###### [!Function] `uri-decompose-hierarchical`  _specific_

_specific_ must be string.

_specific_ is a URI without scheme. For example, the specific of following 
URI 'http://localhost/foo.html' if '//localhost/foo.html'.

Parse given _specific_ into 4 values _authority_, _path_,
_query_ and _fragment_.

If the _specific_ does not contain the part, it will be #f.


###### [!Function] `uri-decompose-authority`  _authority_

_authority_ must be string.

Parse given _authority_ into  3 values, _user-info_, _host_ and
_post_.

If the _authority_ does not contain the part, it will be #f.


###### [!Function] `uri-decode`  _in_ _out_ _:key_ _(cgi-decode_ _#f)_

_in_ must be binary input port.

_out_ must binary output port.

Reads and decodes given _in_ and put the result into _out_.

If the keyword argument _cgi-decode_ is #t, the procedure decodes
`#x2b`('+') to `#x20`('#\\space').


###### [!Function] `uri-decode-string`  _string_ _:key_ _(encoding_ _'utf-8)_ _(cgi-decode_ _#f)_

Decodes given _string_ and returns decoded string.

###### [!Function] `uri-encode`  _in_ _out_ _:key_ _(noescape_ _*rfc3986-unreserved-char-set*)_ _(upper-case_ _#t)_

_in_ must be binary input port.

_out_ must binary output port.

Reads and encodes given _in_ and put the result into _out_.

The keyword argument _noescape_ specifies which character must be escaped.

The keyword argument _upper-case_ specifies the result case of encoded
value. If the value is true value then it encodes to upper case (default),
otherwise lower case.


###### [!Function] `uri-encode-string`  _string_ _:key_ _(encoding_ _'utf-8)_ _:allow-other-keys_

Encodes given _string_ and returns encoded string.

###### [!Function] `uri-compose`  _:key_ _(scheme_ _#f)_ _(userinfo_ _#f)_ _(host_ _#f)_ _(port_ _#f)_ _(authority_ _#f)_ _
_ _(path_ _#f)_ _(path*_ _#f)_ _(query_ _#f)_ _(fragment_ _#f)_ _(specific_ _#f)_

Composes URI from given arguments.

If all keyword arguments are #f, the procedure returns empty string.

The procedure put priority on bigger chunk of URI part. For example, if keyword
argument _specific_ is specified, the procedure uses only _scheme_ and
_specific_. Following describes the priority hierarchy;

``````````scheme
_scheme__specific_  +- _authority_       +- _userinfo_       +- _host_       +- _port_  +- _path\*_       +- _path_       +- _query_       +- _fragment_
``````````



###### [!Function] `uri-merge`  _base-uri_ _relative-uri1_ _relative-uri2_ _..._

Merges given _relative-uris_ to _base-uri_ according to RFC 3986
section 5.


###### [!Variable] `*rfc3986-unreserved-char-set*` 
###### [!Variable] `*rfc2396-unreserved-char-set*` 

Charsets which contains no escape needed characters.

There is slight difference between RFC2396 and RFC3986. This library uses
RFC3986 charset by default to encode.


