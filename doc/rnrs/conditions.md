[§2] Conditions {#rnrs.conditions.6}
-------------

###### [!Library] `(rnrs conditions (6))` 

The section describes `(rnrs conditions (6))`library for creating and
inspecting condition types and values. A condition value encapsulates information
about an exceptional situation.


###### [!Function] `condition`  _condition_ _..._

[R6RS] The `condition` procedure returns a condition object with the
components of the _conditions_ as its components, in the same order. The
returned condition is compound if the total number of components is zero or
greater than one. Otherwise, it may be compound or simple.


###### [!Function] `simple-condition`  _condition_

[R6RS] The `simple-conditions` procedure returns a list of the
components of _condition_, in the same order as they appeared in the
construction of condition.


###### [!Function] `condition?`  _obj_

[R6RS] Returns #t if _obj_ is a (simple or compound) condition,
otherwise returns #f.

###### [!Function] `condition-predicate`  _rtd_

[R6RS] _Rtd_ must be a record-type descriptor of a subtype of
`&condition`. The `condition-predicate` procedure returns a procedure
that takes one argument. This procedure returns #t if its argument is a condition
of the condition type represented by _rtd_, i.e., if it is either a simple
condition of that record type (or one of its subtypes) or a compound conditition
with such a simple condition as one of its components, and #f otherwise.


###### [!Function] `condition-accessor`  _rtd_ _proc_

[R6RS] _Rtd_ must be a record-type descriptor of a subtype of
`&condition`. _Proc_ should accept one argument, a record of the
record type of _rtd_. The `condition-accessor` procedure returns a
procedure that accepts a single argument, which must be a condition of the type
represented by _rtd_. This procedure extracts the first component of the
condition of the type represented by _rtd_, and returns the result of
applying proc to that component.


###### [!Macro] `define-condition-type`  _condition-type_ _supertypes_ _constructor_ _predicate_ _field-spec_ _..._

[R6RS] _Condition-type_, _supertypes_, _constructor_, and 
_predicate_ must all be identifiers. Each _field-spec_ must be of the form

``(_field_ _accessor_)``

where both _field_ and _accessor_ must be identifiers.

The `define-condition-type` form expands into a record-type definition for
a record type _condition-type_. The record type will be non-opaque, non-sealed,
and its fields will be immutable. It will have supertype has its parent type.
The remaining identifiers will be bound as follows:


- _Constructor_ is bound to a default constructor for the type : It accepts
  one argument for each of the record type's complete set of fields (including parent
  types, with the fields of the parent coming before those of the extension in the
  arguments) and returns a condition object initialized to those arguments.
- _Predicate_ is bound to a predicate that identifies conditions of type
  _condition-type_ or any of its subtypes.
- Each _accessor_ is bound to a procedure that extracts the corresponding
  field from a condition of type _condition-type_.



### [§3] Standard condition types

Hierarchy of standard condition types:

``````````scheme
+ &condition
    + &warning
    + &serious
          + &error
          + &violation
                + &assertion
                + &non-continuable
                + &implementation-restriction
                + &lexical
                + &syntax
                + &undefined
    + &message
    + &irritants
``````````

###### [!Condition Type] `&message` 
###### [!Function] `make-message-condition`  _message_
###### [!Function] `message-condition?`  _obj_
###### [!Function] `condition-message`  _condition_

[R6RS] It carries a message further describing the nature of the condition
to humans.

###### [!Condition Type] `&warning` 
###### [!Function] `make-warning` 
###### [!Function] `warning?`  _obj_

[R6RS] This type describes conditions that do not, in principle, prohibit
immediate continued execution of the program, but may interfere with the program's
execution later.

###### [!Condition Type] `&serious` 
###### [!Function] `make-serious-condition` 
###### [!Function] `serious-condition?`  _obj_

[R6RS] This type describes conditions serious enough that they cannot safely
be ignored. This condition type is primarily intended as a supertype of other
condition types.

###### [!Condition Type] `&error` 
###### [!Function] `make-error` 
###### [!Function] `error?`  _obj_

[R6RS] This type describes errors, typically caused by something that has
gone wrong in the interaction of the program with the external world or the user.

###### [!Condition Type] `&violation` 
###### [!Function] `make-violation` 
###### [!Function] `violation?`  _obj_

[R6RS] This type describes violations of the language standard or a library
standard, typically caused by a programming error.

###### [!Condition Type] `&assertion` 
###### [!Function] `make-assertion-violation` 
###### [!Function] `assertion-violation?`  _obj_

[R6RS] This type describes an invalid call to a procedure, either passing
an invalid number of arguments, or passing an argument of the wrong type.

###### [!Condition Type] `&irritants` 
###### [!Function] `make-irritants-condition`  _irritants_
###### [!Function] `irritants-condition?`  _obj_
###### [!Function] `condition-irritants`  _condition_

[R6RS] _Irritants_ should be a list of objects. This condition provides
additional information about a condition, typically the argument list of a
procedure that detected an exception. Conditions of this type are created by the
`error` and `assertion-violation` procedures.

###### [!Condition Type] `&who` 
###### [!Function] `make-who-condition`  _who_
###### [!Function] `who-condition?`  _obj_
###### [!Function] `condition-who`  _condition_

[R6RS] _Who_ should be a symbol or string identifying the entity
reporting the exception. Conditions of this type are created by the `error`and `assertion-violation` procedures, and the `syntax-violation`procedure.

###### [!Condition Type] `&non-continuable` 
###### [!Function] `make-non-continuable-violation` 
###### [!Function] `non-continuable-violation?`  _obj_

[R6RS] This type indicates that an exception handler invoked via raise has
returned.

###### [!Condition Type] `&implementation-restriction` 
###### [!Function] `make-implementation-restriction-violation` 
###### [!Function] `implementation-restriction-violation?`  _obj_

[R6RS] This type describes a violation of an implementation restriction
allowed by the specification, such as the absence of representations for NaNs
and infinities.

###### [!Condition Type] `&lexical` 
###### [!Function] `make-lexical-violation` 
###### [!Function] `lexical-violation?`  _obj_

[R6RS] This type describes syntax violations at the level of the datum syntax.

###### [!Condition Type] `&syntax` 
###### [!Function] `make-syntax-violation`  _form_ _subform_
###### [!Function] `syntax-violation?`  _obj_
###### [!Function] `syntax-violation-form`  _condition_
###### [!Function] `syntax-violation-subform`  _condition_

[R6RS] This type describes syntax violations. _Form_ should be the
erroneous syntax object or a datum representing the code of the erroneous form.
_Subform_ should be an optional syntax object or datum within the erroneous
form that more precisely locates the violation. It can be #f to indicate the
absence of more precise information.


###### [!Condition Type] `&undefined` 
###### [!Function] `make-undefined-violation` 
###### [!Function] `undefined-violation?`  _obj_

[R6RS] This type describes unbound identifiers in the program.

