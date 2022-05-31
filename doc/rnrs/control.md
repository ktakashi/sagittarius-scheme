[§2] Control structures {#rnrs.control.6}
-------------

###### [!Library] `(rnrs control (6))` 

The `(rnrs control (6))`library, which provides useful control structures.

###### [!Syntax] `when`  _test_ _expression_ _..._
###### [!Syntax] `unless`  _test_ _expression_ _..._

[R6RS] _Test_ must be an expression.

A `when` expression is evaluated by evaluating the _test_ expression.
If _test_ evaluates to a true value, the remaining _expressions_ are
evaluated in order, and the results of the last _expression_ are returned as
the results of the entire `when` expression. Otherwise, the `when`expression returns unspecified values. An `unless` expression is evaluated
by evaluating the _test_ expression. If _test_ evaluates to #f, the
remaining _expressions_ are evaluated in order, and the results of the last
_expression_ are returned as the results of the entire `unless`expression. Otherwise, the `unless` expression returns unspecified values.


###### [!Syntax] `do`  _((variable_ _init_ _step)_ _..._ _)_ _(test_ _expression_ _..._ _)_ _commend_

[R6RS] The _inits_, _steps_, _tests_, and _commands_ must
be expressions. The _variables_ must be pairwise distinct variables.

The `do` expression is an iteration construct. It specifies a set of variables
to be bound, how they are to be initialized at the start, and how they are to be
updated on each iteration.

A `do` expression is evaluated as follows: The _init_ expressions are
evaluated (in some unspecified order), the _variables_ are bound to fresh
locations, the results of the _init_ expressions are stored in the bindings
of the _variables_, and then the iteration phase begins.

Each iteration begins by evaluating _test_ if the result is #f, then the
_commands_ are evaluated in order for effect, the _step_ expressions are
evaluated in some unspecified order, the _variables_ are bound to fresh
locations holding the results, and the next iteration begins.

If _test_ evaluates to a true value, the _expressions_ are evaluated from
left to right and the values of the last _expression_ are returned. If no
_expressions_ are present, then the `do` expression returns unspecified
values.

The region of the binding of a _variable_ consists of the entire `do`expression except for the _inits_.

A _step_ may be omitted, in which case the effect is the same as if
(_variable_ _init_ _variable_) had been written instead of
(_variable_ _init_).

``````````scheme
(do ((vec (make-vector 5))
      (i 0 (+ i 1)))
     ((= i 5) vec)
  (vector-set! vec i i))
``````````
=> ``#(0 1 2 3 4)``

``````````scheme
(let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x))
        (sum 0 (+ sum (car x))))
       ((null? x) sum)))
``````````
=> ``25``



###### [!Syntax] `case-lambda`  _case-lambda-clause_ _..._

[R6RS] Each _case-lambda-clause_ must be of the form

``(_formals_ _body_)``

_Formals_ must be as in a `lambda` form.

A `case-lambda` expression evaluates to a procedure. This procedure, when
applied, tries to match its arguments to the `case-lambda-clauses` in order.
The arguments match a clause if one of the following conditions is fulfilled:

_Formals_ has the form `(_variable_ ...)` and the number of
arguments is the same as the number of formal parameters in _formals_.

_Formals_ has the form

``(_variable1_ ... _variablen_ . _variablen+1_)``

 
and the number of arguments is at least _n_.

_Formals_ has the form _variable_.

For the first clause matched by the arguments, the variables of the _formals_are bound to fresh locations containing the argument values in the same arrangement
as with `lambda`.

The last expression of a body in a `case-lambda` expression is in tail context.

If the arguments match none of the clauses, an exception with condition type
`&assertion` is raised.


