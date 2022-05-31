[§2] (record builder) - Record builder {#record.builder}
-------------

###### [!Library] `(record builder)` 

Record builder library. This library provides utilities to
construct a record instance easily.


The following example shows how to use the `make-record-builder`macro.

``````````scheme
(define-record-type base
  (fields a b c))
(define-syntax base-builder
  (make-record-builder base ((a 'a) (c 'c symbol->string))))

(base-builder)
``````````
=> ``#\<base a='a b=#f c="c">``

``````````scheme
(define-record-type child
  (parent base)
  (fields d e f))
(define-syntax child-builder
  (make-record-builder child ((a 'b))))

(child-builder)
``````````
=> ``#\<child a='b b=#f c=#f d=#f e=#f f=#f>``

###### [!Macro] `make-record-builder`  _rtd_ _(default_ _..._ _)_

Make a record builder macro transformer.

_Default_ must be one of the following form:

``(_field-name_ _value_)``

``(_field-name_ _value_ _converter_)``

_field-name_ must be a field name of the _rtd_. _value_ is a
default in case the builder didn't receive the field. The _converter_of the second form must be a procedure which accepts one argument. The
result of this procedure will be used to instantisate the record. It
will also apply the _value_.


The result of the macro transformer accepts the following forms:

``(_field-name_ _value_) ...``

``(`from` _record-instance_) (_field_ _value_) ...``

The `from` is an auxiliary macro bound in the `(record builder)`.
The value of `from`, _record-instance_, will be copied to the
creating record.


