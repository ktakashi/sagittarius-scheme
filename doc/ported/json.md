[§2] (json) -- JSON parser library {#ported.json}
-------------

###### [!Library] `(json)` 

This library is compatible with Chicken Scheme json module and provides
JSON reader and writer. The library is a thin wrapper of 
[(text json)](#text.json) library.


###### [!Function] `json-read`  _:optional_ _(port_ _(current-input-port))_

Reads JSON from given _port_ and returns representing S-expression.

Conversion rules:

``````````scheme
JSON array   <-> list
JSON table   <-> vector
JSON boolean <-> boolean
JSON null    <-> symbol `null`
``````````

Read and write procedure always use above conversion rules even if 
`*json-map-type*` is set to `'alist`.



###### [!Function] `json-write`  _json_ _:optional_ _(port_ _(current-output-port))_

Writes the given S-expression JSON representing object to given
_port_.


