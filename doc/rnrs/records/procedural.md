[§2] Records procedural layer {#rnrs.records.procedural.6}
-------------

###### [!Library] `(rnrs records procedural (6))` 

The procedural layer is provided by the `(rnrs records procedural (6))`library.


###### [!Function] `make-record-type-descriptor`  _name_ _parent_ _uid_ _sealed?_ _opaque?_ _fields_

[R6RS] Returns a record-type descriptor (rtd).

The _name_ argument must be a symbol. It names the record type, and is intended
purely for informational purposes.

The _parent_ argument must be either #f or an rtd. If it is an rtd, the returned
record type, t, extends the record type _p_ represented by _parent_. An
exception with condition type `&assertion` is raised if parent is sealed
(see below).

The _uid_ argument must be either #f or a symbol. If _uid_ is a symbol,
the record-creation operation is nongenerative i.e., a new record type is created
only if no previous call to `make-record-type-descriptor` was made with the
_uid_. If _uid_ is #f, the record-creation operation is generative, 
.e., a new record type is created even if a previous call to 
`make-record-type-descriptor` was made with the same arguments.

If `make-record-type-descriptor` is called twice with the same _uid_symbol, the _parent_ arguments in the two calls must be `eqv?`, the
_fields_ arguments `equal?`, the _sealed?_ arguments boolean-equivalent
(both #f or both true), and the _opaque?_ arguments boolean-equivalent. If
these conditions are not met, an exception with condition type `&assertion`is raised when the second call occurs. If they are met, the second call returns,
without creating a new record type, the same record-type descriptor (in the
sense of `eqv?`) as the first call.

The _sealed?_ flag must be a boolean. If true, the returned record type is
sealed, i.e., it cannot be extended.

The _opaque?_ flag must be a boolean. If true, the record type is opaque. If
passed an instance of the record type, _record?_ returns #f. Moreover, if
`record-rtd` (see [(rnrs records inspection (6))](#rnrs.records.inspection.6))
is called with an instance of the record type, an exception with condition
type `&assertion` is raised. The record type is also opaque if an opaque
parent is supplied. If _opaque?_ is #f and an opaque parent is not supplied,
the record is not opaque.

The _fields_ argument must be a vector of field specifiers. Each field specifier
must be a list of the form `(mutable _name_)` or a list of the form
`(immutable _name_)`. Each name must be a symbol and names the corresponding
field of the record type; the names need not be distinct. A field identified as
mutable may be modified, whereas, when a program attempts to obtain a mutator for
a field identified as immutable, an exception with condition type `&assertion`is raised. Where field order is relevant, e.g., for record construction and field
access, the fields are considered to be ordered as specified, although no particular
order is required for the actual representation of a record instance.

The specified fields are added to the parent fields, if any, to determine the
complete set of fields of the returned record type. If fields is modified after
`make-record-type-descriptor` has been called, the effect on the returned
rtd is unspecified.

A generative record-type descriptor created by a call to
`make-record-type-descriptor` is not `eqv?` to any record-type descriptor
(generative or nongenerative) created by another call to
`make-record-type-descriptor`. A generative record-type descriptor is `eqv?`only to itself, i.e., (eqv? rtd1 rtd2) iff (eq? rtd1 rtd2). Also, two nongenerative
record-type descriptors are `eqv?` if they were created by calls to
`make-record-type-descriptor` with the same uid arguments.


###### [!Function] `record-type-descriptor?`  _obj_

[R6RS] Returns #t if the argument is a record-type descriptor, #f otherwise.

###### [!Function] `make-record-constructor-descriptor`  _rtd_ _parent-constructor-descriptor_ _protocol_

[R6RS] Returns a _record-constructor descriptor_ (or 
var{constructor descriptor} for short) that specifies a _record constructor_(or _constructor_ for short), that can be used to construct record values of
the type specified by _rtd_, and which can be obtained via `record-constructor`.
A constructor descriptor can also be used to create other constructor descriptors
for subtypes of its own record type. _Rtd_ must be a record-type descriptor.
_Protocol_ must be a procedure or #f. If it is #f, a default protocol procedure
is supplied.

If _protocol_ is a procedure, it is handled analogously to the protocol
expression in a `define-record-type` form.

If _rtd_ is a base record type and protocol is a procedure, 
_parent-constructor-descriptor_ must be #f. In this case, _protocol_is called by `record-constructor` with a single argument _p_. _P_is a procedure that expects one argument for every field of _rtd_ and returns
a record with the fields of _rtd_ initialized to these arguments. The
procedure returned by protocol should call _p_ once with the number of
arguments _p_ expects and return the resulting record as shown in the
simple example below:

``````````scheme
(lambda (p)
  (lambda (v1 v2 v3)
    (p v1 v2 v3)))
``````````

Here, the call to _p_ returns a record whose fields are initialized with
the values of `v1`, `v2`, and `v3`. The expression above is
equivalent to `(lambda (p) p)`. Note that the procedure returned by protocol
is otherwise unconstrained; specifically, it can take any number of arguments.

If _rtd_ is an extension of another record type _parent-rtd_ and
_protocol_ is a procedure, _parent-constructor-descriptor_ must be a
constructor descriptor of _parent-rtd_ or #f. If 
_parent-constructor-descriptor_ is a constructor descriptor, _protocol_it is called by _record-constructor_ with a single argument _n_, which
is a procedure that accepts the same number of arguments as the constructor of
_parent-constructor-descriptor_ and returns a procedure _p_ that, when
called, constructs the record itself. The _p_ procedure expects one argument
for every field of _rtd_ (not including parent fields) and returns a record
with the fields of _rtd_ initialized to these arguments, and the fields of
_parent-rtd_ and its parents initialized as specified by
_parent-constructor-descriptor_.

The procedure returned by _protocol_ should call _n_ once with the number
of arguments _n_ expects, call the procedure _p_ it returns once with
the number of arguments _p_ expects and return the resulting record. A
simple _protocol_ in this case might be written as follows:

``````````scheme
(lambda (n)
  (lambda (v1 v2 v3 x1 x2 x3 x4)
    (let ((p (n v1 v2 v3)))
      (p x1 x2 x3 x4))))
``````````

This passes arguments `v1`, `v2`, `v3` to n for
_parent-constructor-descriptor_ and calls _p_ with `x1`, ...,
`x4` to initialize the fields of _rtd_ itself.

Thus, the constructor descriptors for a record type form a sequence of protocols
parallel to the sequence of record-type parents. Each constructor descriptor in
the chain determines the field values for the associated record type. Child record
constructors need not know the number or contents of parent fields, only the number
of arguments accepted by the parent constructor.

_Protocol_ may be #f, specifying a default constructor that accepts one
argument for each field of _rtd_ (including the fields of its parent type,
if any). Specifically, if _rtd_ is a base type, the default protocol procedure
behaves as if it were `(lambda (p) p)`. If _rtd_ is an extension of
another type, then _parent-constructor-descriptor_ must be either #f or
itself specify a default constructor, and the default protocol procedure behaves
as if it were:

``````````scheme
(lambda (n)
  (lambda (v1 ... vj x1 ... xk)
    (let ((p (n v1 ... vj)))
      (p x1 ... xk))))
``````````

The resulting constructor accepts one argument for each of the record type's complete
set of fields (including those of the parent record type, the parent's parent record
type, etc.) and returns a record with the fields initialized to those arguments,
with the field values for the parent coming before those of the extension in the
argument list. (In the example, _j_ is the complete number of fields of the
parent type, and _k_ is the number of fields of rtd itself.)

If _rtd_ is an extension of another record type, and _parent-constructor-descriptor_or the _protocol_ of _parent-constructor-descriptor_ is #f, protocol must
also be #f, and a default constructor descriptor as described above is also assumed.


###### [!Function] `record-constructor`  _constructor-descriptor_

[R6RS] Calls the _protocol_ of _constructor-descriptor_ (as described
for `make-record-constructor-descriptor`) and returns the resulting constructor
constructor for records of the record type associated with 
_constructor-descriptor_.


###### [!Function] `record-predicate`  _rtd_

[R6RS] Returns a procedure that, given an object _obj_, returns #t if
_obj_ is a record of the type represented by _rtd_, and #f otherwise.


###### [!Function] `record-accessor`  _rtd_ _k_

[R6RS] _K_ must be a valid field index of _rtd_. The
`record-accessor` procedure returns a one-argument procedure whose argument
must be a record of the type represented by _rtd_. This procedure returns
the value of the selected field of that record.

The field selected corresponds to the _k_th element (0-based) of the fields
argument to the invocation of `make-record-type-descriptor` that created
_rtd_. Note that _k_ cannot be used to specify a field of any type 
_rtd_ extends.


###### [!Function] `record-mutator`  _rtd_ _k_

[R6RS] _K_ must be a valid field index of _rtd_. The
`record-mutator` procedure returns a two-argument procedure whose arguments
must be a record record _r_ of the type represented by _rtd_ and an
object _obj_. This procedure stores _obj_ within the field of _r_specified by _k_. The _k_ argument is as in `record-accessor`. If
_k_ specifies an immutable field, an exception with condition type
`&assertion` is raised. The mutator returns unspecified values.


