[§1] CLOS {#clos}
=============

Since Sagittarius version 0.3.0, we supports CLOS so that all Scheme objects
have its class. For example `1` is instance of `<integer>` class.

However CLOS has huge features and I don't have intension to implement all of
it.

This section does not describe CLOS itself.

[§2] (clos user) -CLOS user APIs {#lib.clos.user}
-------------

###### [!Library] `(clos user)` 

User level CLOS API collection library.

###### [!Macro] `define-class`  _name_ _supers_ _slots_ _._ _options_

_Name_ must be symbol or identifier.

_Supers_ must be list of class.

_Slots_ must be following structure:

```scheme
_slots_ ::= (_slot_ ...)
_slot_  ::= (_slot-name_ _specifiers_*)
_specifiers_ ::= `:init-keyword` _keyword_ 
              | `:init-value` _value_
              | `:init-form` _form_
              | `:reader` _reader-function_
              | `:writer` _writer-function_
```

Defines a new class.

Slot specifiers:

`:init-keyword`
: This keyword specifies initialisation keyword argument used by the
  `make` procedure. Following code describes how to use:
  ``(make <a-class> :slot-a 'slot-a-value)``
  `<a-class>` has a slot which slot definition contains the keyword
  `:init-keyword` with the keyword _:slot-a_. The code initialises
  an instance of the slot with given value _slot-a-value_.

`:init-value`
: This keyword specifies an initial value of target slot.

`:init-form`
: Similar with `:init-keyword` but this keyword takes expression which
  will be evaluated at initialiation time.

`:reader`
: This keyword creates a procedure takes 1 argument an instance of the class
  to access the slot, so users can read the slot without using `slot-ref`    procedure.

`:writer`
: This keyword creates a procedure takes 2 argument an instance of the class
  and object to set the slot value with given object, so users can set the
  slot without using `slot-set!` procedure.

_opttions_ can specify the metaclass of this class with keyword 
`:metaclass`. 

NOTE: Current implementation does not support `:allocation` keyword
by default. If you need it, see 
[(sagittarius mop allocation)](#sagittarius.mop.allocation).


###### [!Macro] `define-generic` _name_ :key _class_

_Name_ must be symbol.

Creates a new generic function.

By specifying _class_ keyword argument, users can customize the
behaviour of the method specialization.

We provide `<predicate-specializable-generic>` for `memq`, `memv`, `member`
and `predicate` specializer.


###### [!Macro] `define-method`  _name_ _specifiers_ _body_ _..._

_Name_ must be symbol.

_Specifiers_ must be following structure:

```scheme
_specifiers_ ::= (_spec_ ... _rest_)
_spec_ ::= (_argument-name_ _class_) 
       | (_argument-name_)
       | (_argument-name_ (_specializer_ value))
_rest_ ::= '() | symbol
_specializer_ ::= `eq` | `eql` | `equal` | `eq?` | `eqv?` | `equal?`
```

Adds defined method to _name_ generic. If the generic does not exist, this
will create a new generic function implicitly.


###### [!Function] `slot-ref`  _obj_ _slot-name_

Returns the slot value specified _slot-name_.

###### [!Function] `slot-set!`  _obj_ _slot-name_ _value_

Sets the slot value _value_ with specified _slot-name_.

###### [!Function] `slot-bound?`  _obj_ _slot-name_

Returns #t if the slot value specified _slot-name_ is bounded,
otherwise #f.

###### [!Generic] `make`  _class_ _args_ _..._

Creates a new instance of _class_

###### [!Function] `is-a?`  _object_ _class_

Returns #t if _object_ is an instance of _class_, otherwise #f.

###### [!Function] `subtype?`  _class1_ _class2_

Returns #t if _class1_ is a subclass of _class2_, otherwise #f.

###### [!Function] `slot-ref-using-accessor`  _object_ _accessor_

This procedure is for MOP.

Returns the slot value got by _accessor_.


###### [!Function] `slot-set-using-accessor!`  _object_ _accessor_ _value_

This procedure is for MOP.

Sets the slot value _value_ to _object_ using _accessor_.


###### [!Function] `slot-ref-using-class`  _class_ _object_ _slot-name_

This procedure is for MOP.

Returns the slot value according to the given _class_.

It is an error if the given _slot-name_ doesn't exist in the _class_.


###### [!Function] `slot-set-using-accessor!`  _class_ _object_ _slot-name_ _value_

This procedure is for MOP.

Sets the slot value _value_ to _object_ accoring to the given 
_class_.

It is an error if the given _slot-name_ doesn't exist in the _class_.


###### [!Function] `slot-bound-using-class?`  _class_ _object_ _slot-name_

This procedure is for MOP.

Returns #t if the slot is bounded according to the given _class_,
otherwise #f.

It is an error if the given _slot-name_ doesn't exist in the _class_.


###### [!Generic] `write-object`  _object_ _(out_ _<port>)_

This method will be called when writing the given _object_.

Defines how user defined class should be written.


###### [!Generic] `object-equal?`  _object1_ _object2_

This method will be called when `equal?` is called.

Defines how user defined class should be compared.


[§2] (clos core) - CLOS core library {#lib.clos.core}
-------------

###### [!Library] `(clos core)` 

Low level CLOS API collection library.

###### [!Generic] `add-method`  _generic_ _method_

_Generic_ must be generic function. _method_ must be method
object.

Adds _method_ to _generic_.


###### [!Generic] `compute-getter-and-setter`  _class_ _slot_

Returns a list of getter, setter and bound check for the given
_class_'s slot _slot_.

The returning list must have 3 elements, getter, setter and bound?
respectively. Each element must be either #f or procedure. If #f is used then
the default procedure will be used.

For the example code, see [Sagittarius MOP](#sagittarius.mop).


###### [!Generic] `compute-getters-and-setters`  _class_ _slots_

Returns all getters and setters for the given _class_'s slots.

The upper layer of `compute-getter-and-setter`. This method should only
be used if users absolutely need to use accessor to access the target slots.



###### [!Function] `slot-definition-name`  _slot_

Returns slot name of given _slot_.

###### [!Function] `slot-definition-options`  _slot_

Returns slot options of given _slot_.

###### [!Function] `slot-definition-option`  _slot_ _keyword_ _._ _default_

Returns slot option's value of given _slot_ if it has the 
_keyword_.

If _default_ is given, then it will be the fallback value when _keyword_is not found. Otherwise this procedure raises an error.


