[§2] (sagittarius record) - Extra record inspection library {#lib.sagittarius.record}
-------------

###### [!Library] `(sagittarius record)` 

This library provides extra record operations.

###### [!Function] `record-type?`  _obj_

Returns #t if _obj_ is record type, otherwise #f.

###### [!Function] `make-record-type`  _name_ _rtd_ _rcd_

_Name_ must be symbol. _Rtd_ must be record type descriptor.
_Rcd_ must be record constructor descriptor.

Associates given _rcd_ to gien _rtd_.

NOTE: this procedure doesn't create fresh record type but modifies given
arguments destructively.


###### [!Function] `record-type-rtd`  _record-type_

_Record-type_ must be record type.

Returns associated rtd from _record-type_.


###### [!Function] `record-type-rtd`  _record-type_

_Record-type_ must be record type.

Returns associated rcd from _record-type_.


Note: These procedures are not for using casually.