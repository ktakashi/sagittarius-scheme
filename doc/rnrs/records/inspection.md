[§2] Records inspection {#rnrs.records.inspection.6}
-------------

###### [!Library] `(rnrs records syntactic (6))` 

The `(rnrs records inspection (6))`library provides procedures for
inspecting records and their record-type descriptors. These procedures are designed
to allow the writing of portable printers and inspectors.


###### [!Function] `record?`  _obj_

[R6RS] Returns #t if _obj_ is a record, and its record type is not
opaque, and returns #f otherwise.


###### [!Function] `record-rtd`  _record_

[R6RS] Returns the rtd representing the type of _record_ if the type
is not opaque. The rtd of the most precise type is returned; that is, the type
_t_ such that _record_ is of type _t_ but not of any type that
extends _t_. If the type is opaque, an exception is raised with condition
type `&assertion`.


###### [!Function] `record-type-name`  _rtd_
###### [!Function] `record-type-parent`  _rtd_
###### [!Function] `record-type-uid`  _rtd_

[R6RS] Returns the name/parent/uid of the record-type descriptor _rtd_.


###### [!Function] `record-type-generative?`  _rtd_
###### [!Function] `record-type-sealed?`  _rtd_
###### [!Function] `record-type-opaque?`  _rtd_

[R6RS] Returns #t if the record-type descriptor is generative/sealed/opaque,
and #f if not.


###### [!Function] `record-type-field-names`  _rtd_

[R6RS] Returns a vector of symbols naming the fields of the type represented
by _rtd_ (not including the fields of parent types) where the fields are
ordered as described under `make-record-type-descriptor`.


###### [!Function] `record-type-mutable?`  _rtd_ _k_

[R6RS] Returns #t if the field specified by _k_ of the type represented
by _rtd_ is mutable, and #f if not. _K_ is as in `record-accessor`.


