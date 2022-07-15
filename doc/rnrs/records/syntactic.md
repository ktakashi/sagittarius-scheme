[§2] Records syntactic layer {#rnrs.records.syntactic.6}
-------------

###### [!Library] `(rnrs records syntactic (6))` 

The `(rnrs records syntactic (6))`library. Some details of the
specification are explained in terms of the specification of the procedural
layer below.


###### [!Macro] `define-record-type`  _name-spec_ _record-clase_ _..._
###### [!Auxiliary syntax] `fields`  _field-spec_ _..._
###### [!Auxiliary syntax] `parent`  _parent-name_
###### [!Auxiliary syntax] `protocol`  _expression_
###### [!Auxiliary syntax] `sealed`  _boolean_
###### [!Auxiliary syntax] `opaque`  _boolean_
###### [!Auxiliary syntax] `nongenerative`  _:optional_ _uid_
###### [!Auxiliary syntax] `parent-rtd`  _parent-rtd_ _parent-cd_

[R6RS] A `define-record-type` form defines a record type along with
associated constructor descriptor and constructor, predicate, field accessors,
and field mutators. The `define-record-type` form expands into a set of
definitions in the environment where `define-record-type` appears; hence,
it is possible to refer to the bindings (except for that of the record type
itself) recursively.

The _name-spec_ specifies the names of the record type, constructor, and
predicate. It must take one of the following forms:

``(_record-name_ _constructor-name_ _predicate-name_)``

``_record-name_``

_Record-name_, _constructor-name_, and _predicate-name_ must all be
identifiers.
_Record-name_, taken as a symbol, becomes the name of the record type.
(See the description of `make-record-type-descriptor`.) Additionally, it is
bound by this definition to an expand-time or run-time representation of the
record type and can be used as parent name in syntactic record-type definitions
that extend this definition. It can also be used as a handle to gain access to
the underlying record-type descriptor and constructor descriptor
(see `record-type-descriptor` and `record-constructor-descriptor`).

_Constructor-name_ is defined by this definition to be a constructor for the
defined record type, with a protocol specified by the `protocol` clause, or,
in its absence, using a default protocol. For details, see the description of the
`protocol` clause below.

_Predicate-name_ is defined by this definition to a predicate for the defined
record type.

The second form of _name-spec_ is an abbreviation for the first form, where
the name of the constructor is generated by prefixing the record name with 
`make-`, and the predicate name is generated by adding a question mark 
(`?`) to the end of the record name. For example, if the record name is 
`frob`, the name of the constructor is `make-frob`, and the predicate
name is `frob?`.
Each _record-clause_ must take one of the auxiliary syntax forms; it is a
syntax violation if multiple _record-clauses_ of the same kind appear in a
`define-record-type` form.

``(fields _field-spec_*)``

Each _field-spec_ has one of the following forms

``(immutable _field-name_ _accessor-name_)``

``(mutable _field-name_ _accessor-name_ _mutator-name_)``

``(immutable _field-name_)``

``(mutable _field-name_)``

``_field-name_``

_Field-name_, _accessor-name_, and _mutator-name_ must all be identifiers.
The first form declares an immutable field called field-name>, with the corresponding
accessor named _accessor-name_. The second form declares a mutable field called
_field-name_, with the corresponding accessor named _accessor-name_, and
with the corresponding mutator named _mutator-name_.

If _field-spec_ takes the third or fourth form, the accessor name is generated
by appending the record name and field name with a hyphen separator, and the mutator
name (for a mutable field) is generated by adding a `-set!` suffix to the
accessor name. For example, if the record name is `frob` and the field name
is `widget`, the accessor name is `frob-widget` and the mutator name is
`frob-widget-set!`.

If _field-spec_ is just a _field-name_ form, it is an abbreviation for
`(immutable _field-name_)`.

The _field-names_ become, as symbols, the names of the fields in the
`record-type` descriptor being created, in the same order.

The `fields` clause may be absent; this is equivalent to an empty `fields`clause.

``(parent _parent-name_)``

Specifies that the record type is to have parent type _parent-name_, where
_parent-name_ is the _record-name_ of a record type previously defined
using `define-record-type`. The record-type definition associated with
_parent-name_ must not be sealed. If no parent clause and no `parent-rtd`(see below) clause is present, the record type is a base type.

``(protocol _expression_)``

_Expression_ is evaluated in the same environment as the `define-record-type`form, and must evaluate to a protocol appropriate for the record type being defined.

The protocol is used to create a record-constructor descriptor as described below.
If no `protocol` clause is specified, a constructor descriptor is still created
using a default protocol. The clause can be absent only if the record type being
defined has no parent type, or if the parent definition does not specify a protocol.

``(sealed _boolean_)``

If this option is specified with operand #t, the defined record type is sealed,
i.e., no extensions of the record type can be created. If this option is specified
with operand #f, or is absent, the defined record type is not sealed.

``(opaque _boolean_)``

If this option is specified with operand #t, or if an opaque parent record type is
specified, the defined record type is opaque. Otherwise, the defined record type is
not opaque. See the specification of record-rtd below for details.

``(nongenerative _uid_)``

``(nongenerative)``

This specifies that the record type is nongenerative with uid _uid_, which must
be an identifier. If _uid_ is absent, a unique uid is generated at macro-expansion
time. If two record-type definitions specify the same uid, then the record-type
definitions should be equivalent, i.e., the implied arguments to 
`make-record-type-descriptor` must be equivalent as described under
`make-record-type-descriptor`. If this condition is not met, it is either
considered a syntax violation or an exception with condition type `&assertion`is raised. If the condition is met, a single record type is generated for both
definitions.

In the absence of a `nongenerative` clause, a new record type is generated
every time a `define-record-type` form is evaluated:

``````````scheme
(let ((f (lambda (x)
           (define-record-type r ...)
           (if x r? (make-r ...)))))
  ((f #t) (f #f)))
``````````

``(parent-rtd _parent-rtd_ _parent-cd_)``

Specifies that the record type is to have its parent type specified by
_parent-rtd_, which should be an expression evaluating to a record-type
descriptor, and _parent-cd_, which should be an expression evaluating to a
constructor descriptor. The record-type definition associated with the value of
_parent-rtd_ must not be sealed. Moreover, a record-type definition must not
have both a `parent` and a `parent-rtd` clause.

All bindings created by `define-record-typ`e (for the record type, the
constructor, the predicate, the accessors, and the mutators) must have names that
are pairwise distinct.

The constructor created by a `define-record-type` form is a procedure as
follows:


- If there is no `parent` clause and no `protocol` clause, the
  constructor accepts as many arguments as there are fields, in the same order
  as they appear in the `fields` clause, and returns a record object with
  the fields initialized to the corresponding arguments.
- If there is no `parent` or `parent-rtd` clause and a `protocol`clause, the protocol expression must evaluate to a procedure that accepts a
  single argument. The protocol procedure is called once during the evaluation of
  the `define-record-type` form with a procedure _p_ as its argument. It
  should return a procedure, which will become the constructor bound to
  _constructor-name_. The procedure _p_ accepts as many arguments as there
  are fields, in the same order as they appear in the fields clause, and returns
  a record object with the fields initialized to the corresponding arguments.
  The constructor returned by the protocol procedure can accept an arbitrary number
  of arguments, and should call _p_ once to construct a record object, and
  return that record object.
  For example, the following protocol expression for a record-type definition with
  three fields creates a constructor that accepts values for all fields, and
  initialized them in the reverse order of the arguments:
  ``````````scheme
  (lambda (p)
    (lambda (v1 v2 v3)
      (p v3 v2 v1)))
  ``````````
- If there is both a `parent` clause and a `protocol` clause, then
  the protocol procedure is called once with a procedure _n_as its argument.
  As in the previous case, the protocol procedure should return a procedure, which
  will become the constructor bound to _constructor-name_. However, _n_ is
  different from _p_ in the previous case: It accepts arguments corresponding
  to the arguments of the constructor of the parent type. It then returns a procedure
  _p_ that accepts as many arguments as there are (additional) fields in this
  type, in the same order as in the `fields` clause, and returns a record object
  with the fields of the parent record types initialized according to their constructors
  and the arguments to _n_, and the fields of this record type initialized to
  its arguments of _p_.
  The constructor returned by the protocol procedure can accept an arbitrary number
  of arguments, and should call _n_ once to construct the procedure _p_,
  and call _p_ once to create the record object, and finally return that record
  object.
  For example, the following protocol expression assumes that the constructor of
  the parent type takes three arguments:
  ``````````scheme
  (lambda (n)
    (lambda (v1 v2 v3 x1 x2 x3 x4)
      (let ((p (n v1 v2 v3)))
        (p x1 x2 x3 x4))))
  ``````````
  The resulting constructor accepts seven arguments, and initializes the fields of
  the parent types according to the constructor of the parent type, with `v1`,
  `v2`, and `v3` as arguments. It also initializes the fields of this
  record type to the values of `x1`, ..., `x4`.
- If there is a `parent` clause, but no `protocol` clause, then the
  parent type must not have a `protocol` clause itself. The constructor bound
  to _constructor-name_ is a procedure that accepts arguments corresponding to
  the parent types' constructor first, and then one argument for each field in the
  same order as in the `fields` clause. The constructor returns a record object
  with the fields initialized to the corresponding arguments.
- If there is a `parent-rtd` clause, then the constructor is as with a
  `parent` clause, except that the constructor of the parent type is determined
  by the constructor descriptor of the `parent-rtd` clause.

A protocol may perform other actions consistent with the requirements described
above, including mutation of the new record or other side effects, before returning
the record.

Any definition that takes advantage of implicit naming for the constructor,
predicate, accessor, and mutator names can be rewritten trivially to a definition
that specifies all names explicitly. For example, the implicit-naming record
definition:

``````````scheme
(define-record-type frob
  (fields (mutable widget))
  (protocol
    (lambda (p)
      (lambda (n) (p (make-widget n))))))
``````````

is equivalent to the following explicit-naming record definition.

``````````scheme
(define-record-type (frob make-frob frob?)
  (fields (mutable widget
                   frob-widget
                   frob-widget-set!))
  (protocol
    (lambda (p)
      (lambda (n) (p (make-widget n))))))
``````````

Also, the implicit-naming record definition:

``(define-record-type point (fields x y))``

is equivalent to the following explicit-naming record definition:

``````````scheme
(define-record-type (point make-point point?)
  (fields 
    (immutable x point-x)
    (immutable y point-y)))
``````````

With implicit naming, it is still possible to specify some of the names explicitly;
for example, the following overrides the choice of accessor and mutator names for
the widget field.

``````````scheme
(define-record-type frob
  (fields (mutable widget getwid setwid!))
  (protocol
    (lambda (p)
      (lambda (n) (p (make-widget n))))))
``````````



###### [!Macro] `record-type-descriptor`  _record-name_

[R6RS] Evaluates to the record-type descriptor (see 
[Records procedural layer](#rnrs.records.procedural.6)) associated with the type specified by
_record-name_.


###### [!Macro] `record-constructor-descriptor`  _record-name_

[R6RS] Evaluates to the record-type constructor (see
[Records procedural layer](#rnrs.records.procedural.6)) associated with the type specified by
_record-name_.

The following example uses the `record?` procedure from the
`(rnrs records inspection (6))` library:

``````````scheme
(define-record-type (point make-point point?)
  (fields (immutable x point-x)
           (mutable y point-y set-point-y!))
  (nongenerative point-4893d957-e00b-11d9-817f-00111175eb9e))
``````````

``````````scheme
(define-record-type (cpoint make-cpoint cpoint?)
  (parent point)
  (protocol (lambda (n)
                 (lambda (x y c) 
                   ((n x y) (color->rgb c)))))
  (fields (mutable rgb cpoint-rgb cpoint-rgb-set!)))
``````````

``(define (color->rgb c) (cons 'rgb c))``

``(define p1 (make-point 1 2))``

``(define p2 (make-cpoint 3 4 'red))``

``(point? p1)`` => ``#t``

``(point? p2)`` => ``#t``

``(point? (vector))``

``(point? (cons 'a 'b))``

``(cpoint? p1)``

``(cpoint? p2)`` => ``#t``

``(point-x p1)`` => ``1``

``(point-y p1)`` => ``2``

``(point-x p2)`` => ``3``

``(point-y p2)`` => ``4``

``(cpoint-rgb p2)`` => ``(rgb . red)``

``(set-point-y! p1 17)`` => ``unspecified``

``(point-y p1)`` => ``17``

``(record-rtd p1)`` => ``(record-type-descriptor point)``

``````````scheme
(define-record-type (ex1 make-ex1 ex1?)
  (protocol (lambda (p) (lambda a (p a))))
  (fields (immutable f ex1-f)))
``````````

``(define ex1-i1 (make-ex1 1 2 3))``

``(ex1-f ex1-i1)`` => ``(1 2 3)``

``````````scheme
(define-record-type (ex2 make-ex2 ex2?)
  (protocol
    (lambda (p) (lambda (a . b) (p a b))))
  (fields (immutable a ex2-a)
           (immutable b ex2-b)))
``````````

``(define ex2-i1 (make-ex2 1 2 3))``

``(ex2-a ex2-i1)`` => ``1``

``(ex2-b ex2-i1)`` => ``(2 3)``

``````````scheme
(define-record-type (unit-vector make-unit-vector unit-vector?)
  (protocol (lambda (p)
                 (lambda (x y z)
                   (let ((length (sqrt (+ (* x x) (* y y) (* z z)))))
                         (p (/ x length) (/ y length) (/ z length))))))
  (fields (immutable x unit-vector-x)
           (immutable y unit-vector-y)
           (immutable z unit-vector-z)))
``````````

``(define *ex3-instance* #f)``

``````````scheme
(define-record-type ex3
  (parent cpoint)
  (protocol (lambda (n)
                 (lambda (x y t)
                   (let ((r ((n x y 'red) t)))
                     (set! *ex3-instance* r)
                     r))))
  (fields  (mutable thickness))
  (sealed #t) (opaque #t))
``````````

``(define ex3-i1 (make-ex3 1 2 17))``

``(ex3? ex3-i1)`` => ``#t``

``(cpoint-rgb ex3-i1)`` => ``(rgb . red)``

``(ex3-thickness ex3-i1)`` => ``17``

``(ex3-thickness-set! ex3-i1 18)`` => ``unspecified``

``(ex3-thickness ex3-i1)`` => ``18``

``*ex3-instance*`` => ``ex3-i1``

``(record? ex3-i1)``


