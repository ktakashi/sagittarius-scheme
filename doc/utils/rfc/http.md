[§2] (rfc http) - HTTP client {#rfc.http}
-------------

###### [!Library] `(rfc http)` 

This library provides simple HTTP client defined in 
[RFC 2616](https://www.ietf.org/rfc/rfc2616.txt).

The library only talks to HTTP 1.1 and provides part of the specified features.
We may implement complete specification in future.


Following is the simple example to send GET method to a HTTP server.

``````````scheme
(import (rfc http))

(define url "http://example.com/path")

(let-values (((server path) (url-server&path url)))
  (http-get server path))
;; -> return 3 values
;; status
;; header
;; body
``````````

###### [!Condition] `&http-error` 

HTTP error condition. 

###### [!Function] `http-error?`  _obj_

Return #t if given _obj_ is HTTP condition, otherwise #f.

### [§3] Request APIs

###### [!Function] `http-get`  _server_ _path_ _options_ _..._
###### [!Function] `http-head`  _server_ _path_ _options_ _..._
###### [!Function] `http-post`  _server_ _path_ _body_ _options_ _..._
###### [!Function] `http-put`  _server_ _path_ _body_ _options_ _..._
###### [!Function] `http-delete`  _server_ _path_ _options_ _..._

Sends HTTP request to given _path_ on _server_. The using
methods are `GET`, `HEAD`, `POST`, `PUT`, and
`DELETE`, respectively.

The _body_ argument of `http-post` and `http-put` can be
UTF-8 string, bytevector or list of body parameters. The parameters are
used for sending multipart form data. The detail parameter form is described
in the `http-compose-form-data`.

The keyword _:value_ and _file_ should not be represented 
simultaneously, if both keywords are found then _:file_ is used.

Optional arguments _options_ are passed to underling procedure
`http-request`.


###### [!Function] `http-request`  _method_ _server_ _request-uri_ _
_ _:key_ _(no-redirect_ _#f)_ _
_ _(auth-handler_ _#f)_ _
_ _(auth-user_ _#f)_ _
_ _	_ _(auth-password_ _#f)_ _
_ _	_ _(user-agent_ _(*http-user-agent*))_ _
_ _	_ _(secure_ _#f)_ _
_ _	_ _(receiver_ _(http-string-receiver))_ _
_ _	_ _(sender_ _#f)_ _
_ _:allow-other-keys_ _opts_

Sends HTTP request to _request-uri_ on _server_ with
_method_.

The keyword argument _receiver_ is used to receive the response data.
The value must be a procedure which takes four arguments, status code,
headers, size of content, and thunk to retrieve remote binary port and its 
size.

The keyword argument _sender_ is used for `POST` and `PUT`HTTP method to send data. If it's specified then it must be a procedure
which takes three arguments, headers, encoding and header-sink.

NOTE: Users can define own receiver and sender however the API may change
in the future. So if the predefined ones are sufficient, it is safe to use
them.

The keyword arguments start with _auth-_ handle authentication.
If the server respond status code `401` then those values are
used. _auth-handler_ must be a procedure and takes five arguments
alist of connection-info, _auth-user_, _auth-password_, response
headers and response body. If the handler returns list of "authorization"
value then `http-request` sends it as the authentication data. For
example, if the server requires BASIC authentication then the procedure
should return something like following value;

``(("authorization" "Basic dXNlcjpwYXNz"))``

Following is the complete example of _auth-handler_;

``````````scheme
(define (basic-auth-handler info user pass headers body)
  (let ((m (format "~a:~a" user pass)))
    `(("authorization" ,(format "Basic ~a" (base64-encode-string m))))))
``````````

`http-request` supports BASIC authentication and Digest authentication
by default. If you know that the server requires one of each then specifying
_auth-user_ and _auth-password_ is sufficient.

If keyword argument _secure_ is true value then TLS socket is used for
physical connection.

The rest arguments _opts_ is converted to request header.


###### [!Parameter] `*http-user-agent*` 

This parameter value is used for `user-agent` header.


### [§3] Senders and receivers

###### [!Function] `http-null-sender` 

Creates a sender which sends nothing.

###### [!Function] `http-string-sender`  _str_

Creates a sender which content is _str_.

The string will be converted to bytevector according to the
_encoding_ argument when the sender is called.


###### [!Function] `http-blob-sender`  _blob_

_blob_ must be a string or bytevector.

Creates a sender which content is _blob_. If the _blob_ is string,
it will be converted to bytevector with `string->utf8`.


###### [!Function] `http-multipart-sender`  _params_

Creates a sender which send `multipart/form-data` with
content of _param_.

The content will be created by `http-compose-form-data` procedure
passing _param_.


###### [!Function] `http-string-receiver` 
###### [!Function] `http-binary-receiver` 
###### [!Function] `http-null-receiver` 

Creates a receiver which returning content is string, bytevecotor and
unspecified value, respectively.


``````````scheme
(http-get "google.com" "/" :receiver (http-string-receiver))
``````````
=> ``status headers and string representation of received content``

``````````scheme
(http-get "google.com" "/" :receiver (http-binary-receiver))
``````````
=> ``status headers and bytevector representation of received content``

``````````scheme
(http-get "google.com" "/" :receiver (http-null-receiver))
``````````
=> ``status headers and unspecified value``



###### [!Function] `http-oport-receiver`  _sink_ _flusher_

The _sink_ must be a binary output port, _flusher_ must be
a procedure takes two arguments, _sink_ and headers.

Creates a receiver which stores the content of response to _sink_ and
returns the result of _flusher_.

``````````scheme
(http-get "google.com" "/"
          :receiver (let-values (((port extract) 
                                  (open-bytevector-output-port)))
                      (http-oport-receiver port 
                                           (lambda (port size) (extract)))))
``````````
=> ``status headers and bytevector representation of received content``



###### [!Function] `http-file-receiver`  _filename_ _:key_ _(temporary?_ _#f)_

_filename_ must be a string.

Creates a receiver which stores the content to a file. The receiver
returns the file name.

If keyword arguments _temporary?_ specified with true value, then
the returning file name is temporary file.

If there is no response or `content-length` header contains non
number value, then the file will be cleaned.

``````````scheme
(http-get "google.com" "/" :receiver (http-file-receiver "google.html"))
``````````
=> ``status headers and "google.html"``



###### [!Function] `http-gzip-receiver`  _receiver_

_receiver_ must be an HTTP receiver.


Creates a receiver which handles gzip encoded response. If HTTP response
contains `Content-Encoding` header with value `gzip`, then the
receiver decodes the response and propagates to given _receiver_. 
Otherwise, it simply forwards the response to _receiver_.

The following describes how to use it:

``````````scheme
(http-get "google.com" "/"
          :accept-encoding "gzip"
          :receiver (http-gzip-receiver (http-string-receiver)))
``````````



### [§3] Utilities

###### [!Function] `http-compose-query`  _path_ _params_ _:optional_ _(encoding_ _'utf-8)_

Composes query string.

If given _path_ is #f then only composed query string is returned,
otherwise this returns `_path_?_composed query_` form.

_params_ must be a list of name & value list or null list.


###### [!Function] `http-compose-form-data`  _params_ _port_ _:optional_ _(encoding_ _'utf-8)_

Composes  multipart form data.

If _port_ is #f then it returns composed string. if it's a port then
the result is stored in the given _port_.

The _params_ must be following form;

``````````scheme
 <params> : (<params> ...)
 <param>  : (<name> <value>)
          | (<name> <key> <value> <key2> <value2> ...)
 <key>    : :value | :file | :content-type | :content-transfer-encoding
          | other keyword (used as a header name)
``````````

_\<value>_ is the content of _\<name>_ parameter. It can be any of
Scheme object however it is converted to string representation except
 bytevector.

If _:file_ keyword is used then it read the content of _\<value>_.


###### [!Function] `url-server&path`  _url_

Decompose the given _url_ and returns auth part of URL and
path + query + fragment.


