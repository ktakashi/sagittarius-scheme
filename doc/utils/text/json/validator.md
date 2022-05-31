[§2] (text json validator) - JSON validator {#text.json.validator}
-------------

###### [!Library] `(text json validator)` 

This library provides abstraction layer of JSON validation.

###### [!Record Type] `<json-validator>` 

Base class of JSON validators.

The record have `validator` field.


###### [!Function] `json-validator?`  _obj_

Return `#t` if the given _obj_ is a JSON validator,
otherwise `#f`.

###### [!Function] `make-json-validator`  _validator_

Creates a JSON validator.

The _validator_ must be a procedure accepts one argument.


###### [!Function] `json-validator-validator`  _json-validator_

Returns the `validator` field value of the given
_json-validator_.


###### [!Function] `validate-json`  _json-validator_ _json_

Calls value of `validator` field of given JSON validator with
parameter of _json_.


