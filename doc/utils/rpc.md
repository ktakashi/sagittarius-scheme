[§2] RPC support framework {#utils.rpc}
-------------

RPC is basically separated in 2 parts. One is message marshalling and other 
one is transporting. Following sections and libraries provide some framework
for generic use.

### [§3] Message framework

###### [!Library] `(rpc message)` 

This library provides generic functions for marshalling message.

###### [!Generic] `rpc-marshall-message` 
###### [!Generic] `rpc-unmarshall-message` 

These generic function will be used transport layer to marshall or
unmarshall messages.

### [§3] Http transport {#utils.rpc.transport.http}

Currently there is no generic transport framework and support HTTP transport.

For future, we may provide more generic way to send/receive request 
and response.

###### [!Library] `(rpc transport http)` 

This library provides procedures and generic function of 
HTTP transport for RCP

.

###### [!Function] `rpc-http-request`  _url_ _message_ _:key_ _(unmarshall?_ _#t)_ _:allow-other-keys_ _options_

Send given _message_ to _url_ and returns 3 values,
http status, header and response.

If the keyword argument _unmarshall?_ is #f then the returning response
value will not be unmarshalled.

_options_ will be passed to `http-request` procedure.


#### [§4] Http transport hook

###### [!Method] `rpc-http-method`  _request_

Returns http method. Default implementation returns `POST`

.

###### [!Method] `rpc-http-content-type`  _request_

Returns content type. Default implementation returns 
`application/octet-stream`

.

###### [!Method] `rpc-http-sender`  _request_

Returns http sender. Default implementation returns
`http-blob-sender` with marshalled request

.

###### [!Method] `rpc-http-receiver`  _request_

Returns http receiver. Default implementation returns
`http-binary-receiver`

.

###### [!Method] `rpc-http-response-type`  _request_

Returns a marker type to be used `rpc-http-unmarshall-message`.
Default implementation returns given _request_ itself

.

###### [!Method] `rpc-http-unmarshall-message`  _type_ _header_ _body_

Unmarshall the given _body_ according to the given _type_.
Default implementation ignores _header_ and passes _type_ and
_body_ to `rpc-unmarshall-message`.

* @[[utils/rpc/json.md](rpc/json.md)]
