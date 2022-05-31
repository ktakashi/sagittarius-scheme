[§2] Composite libraries {#rnrs.composite}
-------------

### [§3] eval {#rnrs.eval.6}

###### [!Library] `(rnrs eval (6))` 

[R6RS]The `(rnrs eval (6))` library allows a program to create Scheme
expressions as data at run time and evaluate them.


###### [!Function] `eval`  _expression_ _environment_

[R6RS] Evaluates _expression_ in the specified _environment_ and
returns its value. _Expression_ must be a syntactically valid Scheme
expression represented as a datum value.

R6RS requires _envionment_ an envitonment which must be created by the
`environment` procedure. However on Sagittarius, _environment_ can be
anything. This behaviour might be fixed in future.


###### [!Function] `environment`  _import-spec_ _..._

[R6RS] _Import-spec_ must be a datum representing an import spec. The
`environment` procedure returns an environment corresponding to _import-spec_.


### [§3] Mutable pairs {#rnrs.mutable-pairs.6}

###### [!Library] `(rnrs mutable-pairs (6))` 

[R6RS] This library exports `set-car!` and `set-cdr!`.

###### [!Function] `set-car!`  _pair_ _obj_
###### [!Function] `set-cdr!`  _pair_ _obj_

[R6RS] Store _obj_ in the car/cdr field of _pair_. These procedures
return unspecified value.

On Sagittarius Scheme, these procedures can modify immutable pairs.


### [§3] Mutable strings {#rnrs.mutable-strings.6}

###### [!Library] `(rnrs mutable-stringss (6))` 

[R6RS] This library exports `string-set!` and `string-fill!`.

###### [!Function] `string-set!`  _string_ _k_ _char_

[R6RS] _K_ must be a valid index of _string_.

The `string-set!` procedure stores _char_ in element _k_ of string
and returns unspecified values.


###### [!Function] `string-fill!`  _string_ _char_ _:optional_ _(start_ _0)_ _(end_ `(string-length _string_)` _)_

[R6RS+] Stores _char_ in every element of the given _string_ and
returns unspecified values. Optional arguments _start_ and _end_ restrict
the ragne of filling.

Passing an immutable string to these procedures cause an exception with condition
type `&assertion` to be raised.


### [§3] R5RS compatibility {#rnrs.r5rs.6}

###### [!Library] `(rnrs r5rs (6))` 

This library provides R5RS compatibility procedures such as
`exact->inexact`.

###### [!Function] `exact->inexect`  _z_
###### [!Function] `inexact->exect`  _z_

[R6RS] These are the same as the `inexact` and `exact` procedures.

###### [!Function] `quotient`  _n1_ _n2_
###### [!Function] `remainder`  _n1_ _n2_
###### [!Function] `modulo`  _n1_ _n2_

[R6RS] Returns the quotient, remainder and modulo of dividing an integer
_n1_ by an integer _n2_, respectively. The result is an exact number only
if both _n1_ and _n2_ are exact numbers.


###### [!Macro] `delay`  _expression_

[R6RS] The `delay` construct is used together with the procedure
`force` to implement _lazy evaluation_ or _call by need_.
`(delay _expression_)` returns an object called a _promise_ which
at some point in the future may be asked (by the `force` procedure) to
evaluate expression, and deliver the resulting value. The effect of expression
returning multiple values is unspecified.


###### [!Function] `force`  _promise_

[R6RS] _Promise_ must be a promise.

The `force` procedure forces the value of _promise_. If no value has
been computed for the promise, then a value is computed and returned. The value
of the promise is cached (or "memoized") so that if it is forced a second time,
the previously computed value is returned.


###### [!Function] `null-environment`  _n_
###### [!Function] `scheme-report-environment`  _n_

[R6RS] _N_ must be the exact integer object 5. These procedures return
an environment which is suitable to use with `eval`.


