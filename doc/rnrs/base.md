[§2] Base Library {#rnrs.base.6}
-------------

###### [!Library] `(rnrs base (6))` 

[R6RS] This library exports many of the procedure and syntax bindings
that are traditionally associated with Scheme.

### [§3] Variable definitions {#rnrs.base.6.variable.definitions}

###### [!Syntax] `define`  _variable_ _expression_
###### [!Syntax] `define`  _variable_
###### [!Syntax] `define`  _(variable_ _formals)_ _body_ _..._
###### [!Syntax] `define`  _(variable_ _._ _formal)_ _body_ _..._

[R6RS] The `define` form is a definition used to create variable
bindings and may appear anywhere other definitions may appear.

The first from of `define` binds _variable_ to a new location before
assigning the value of _expression_ to it.

``(define add3 (lambda (x) (+ x 3)))``

``(add3 3)`` => ``6``

``(define first car)``

``(first '(1 2))`` => ``1``

The second form of `define` is equivalent to

``(define _variable_ _unspecified_)``

where _unspecified_ is a side-effect-free expression returning an
unspecified value.

In the third form of `define`, _formals_ must be either a sequence of
zero or more variables, or a sequence of one or more variables followed by a dot
`.` and another variable. This form is equivalent to

``(define _variable_ (lambda (_formals_) _body ..._))``

In the fourth form of `define`, _formal_ must be a single variable.
This form is equivalent to

``(define _variable_ (lambda _formal_ _body ..._))``



### [§3] Syntax definitions

###### [!Syntax] `define-syntax`  _keyword_ _expression_

[R6RS] The `define-syntax` form is a definition used to create keyword
bindings and may appear anywhere other definitions may appear.

Binds _keyword_ to the value of _expression_, which must evaluate,
at macro-expansion time, to a transformer.


### [§3] Quotation

###### [!Syntax] `quote`  _datum_

[R6RS] `quote` evaluates to the datum value represented by _datum_.

``(quote a)`` => ``a``

``(quote #(a b c))`` => ``#(a b c)``

``(quote (+ 1 2))`` => ``(+ 1 2)``



### [§3] Procedures

###### [!Syntax] `lambda`  _formals_ _body_ _..._

[R6RS+]A `lambda` expression evaluates to a procedure. The
environment in effect when the lambda expression is evaluated is remembered as
part of the procedure. When the procedure is later called with some arguments,
the environment in which the `lambda` expression was evaluated is extended 
by binding the variables in the parameter list to fresh locations, and the
resulting argument values are stored in those locations. Then, the expressions
in the _body_ of the `lambda` expression are evaluated sequentially in
the extended environment. The results of the last expression in the body are
returned as the results of the procedure call.

``(lambda (x) (+ x x))`` => ``a procedure``

``((lambda (x) (+ x x)) 4)`` => ``8``

``````````scheme
((lambda (x)
   (define (p y) (+ y 1))
   (+ (p x) x)) 5)
``````````
=> ``11``

``(define reverse-subtract (lambda (x y) (- y x)))``

``(reverse-subtract 7 10)`` => ``3``

``(define add4 (let ((x 4)) (lambda (y) (+ x y))))``

``(add4 6)`` => ``10``

_Formals_ must have one of the following forms:

- (_\<variable1>_ ...)
   The procedure takes a fixed number of arguments; when the procedure is
      called, the arguments are stored in the bindings of the corresponding
      variables.
- _\<variable>_ The procedure takes any number of arguments; when the procedure is called,
      the sequence of arguments is converted into a newly allocated list, and the
      list is stored in the binding of the _\<variable>_.
- (_\<variable1>_ ... _\<variablen>_ . _\<variablen+1>_)
   If a period `.` precedes the last variable, then the procedure takes
      _n_ or more arguments, where _n_ is the number of parameters before
      the period (there must be at least one). The value stored in the binding of
      the last variable is a newly allocated list of the arguments left over after
      all the other arguments have been matched up against the other
      parameters.
      ``((lambda x x) 3 4 5 6)`` => ``(3 4 5 6)``
      ``((lambda (x y . z) z)  3 4 5 6)`` => ``(5 6)``
      Any _variable_ must not appear more than once in _formals_.
- (_\<variable> ... _\<extended-spec>_ ..._)
    
    Extended argument specification. Zero or more variables that specifies
    required formal argument, followed by an _\<extended-spec>_, a list
    begginning with a keyword `:optional`, `:key` or `:rest`.
    The _\<extended-spec>_ part consists of the optional argument spec, the
    keyword argument spec and the rest argument spec. They can appear in any
    combinations.
    
  `:optional _optspec_ ...`
  : Specifies optional arguments. Each _optspec_ can be either one of the
    following forms:
    ``_variable_``
    ``(_variable_ _init-expr_)``
    The _variable_ names the formal argument, which is bound to the value
    of the actual argument if given, or the value of the expression
    _init-expr_ otherwise. If _optspec_ is just a variable, and the
    actual argument is not given, then it will be unspecified value.
    The expression _init-expr_ is only evaluated if the actual argument is
    not given. The scope in which _init-expr_ is evaluated includes the
    preceding formal arguments.
    ``((lambda (a b :optional (c (+ a b))) (list a b c)) 1 2)
    `` => ``(1 2 3)``
    ``((lambda (a b :optional (c (+ a b))) (list a b c)) 1 2 -1)
    `` => ``(1 2 -1)``
    ``((lambda (a b :optional c) (list a b c)) 1 2)
    `` => ``(1 2 #\<unspecified>)``
    ``((lambda (:optional (a 0) (b (+ a 1))) (list a b)))
    `` => ``(1 2)``
    The procedure raises an `&serious` if more actual arguments than the
    number of required and optional arguments are given, unless it also has
    `:key` or `:rest` arguments spec.
    ``((lambda (:optional a b) (list a b)) 1 2 3)
    `` => ``&serious``
    ``((lambda (:optional a b :rest r) (list a b r)) 1 2 3)
    `` => ``(1 2 (3))``
  `:key _keyspec_ ... [:allow-other-keys [_variable_]]`
  : Specifies keyword arguments. Each _keyspec_ can be one of the
    following forms.
    ``_variable_``
    ``(_variable_ _init-expr_)``
    ``((_keyword_ _variable_) _init-expr_)``
    ``(_variable_ _keyword_ _init-expr_)``
    The _variable_ names the formal argument, which is bound to the actual
    argument given with the keyword of the same name as _variable_. When
    the actual is not given, _init-expr_ is evaluated and the result is
    bound to _variable_ in the second, third and fourth form, or
    unspecified value is bound in the first form.
    ``````````scheme
    (define f (lambda (a :key (b (+ a 1)) (c (+ b 1)))
          (list a b c)))
    ``````````
    ``(f 10)`` => ``(10 11 12)``
    ``(f 10 :b 4)`` => ``(10 4 5)``
    ``(f 10 :c 8)`` => ``(10 11 8)``
    ``(f 10 :c 1 :b 3)`` => ``(10 3 1)``
    With the third and fourth form you can name the formal argument
    differently from the keyword to specify the argument.
    ``((lambda (:key ((:aa a) -1)) a) ::aa 2)`` => ``2``
    ``((lambda (:key (a :aa -1)) a) ::aa 2)`` => ``2``
    By default, the procedure with keyword argument spec raises
    `&serious` if a keyword argument with an unrecognized keyword is
    given. Giving `:allow-other-keys` in the formals suppresses this
    behaviour. If you give _variable_ after `:allow-other-keys`, the
    list of unrecognized keywords and their arguments are bound to it.
    ``((lambda (:key a) a) :a 1 :b 2)`` => ``&serious``
    ``((lambda (:key a :allow-other-keys) a) :a 1 :b 2)`` => ``1``
    ``((lambda (:key a :allow-other-keys z) (list a z)) :a 1 :b 2)
    `` => ``(1 (b 2))``
    When used with `:optional` argument spec, the keyword arguments are
    searched after all the optional arguments are bound.
    ``((lambda (:optional a b :key c) (list a b c)) 1 2 :c 3)
    `` => ``(1 2 3)``
    ``((lambda (:optional a b :key c) (list a b c)) :c 3)
    `` => ``(c 3 #\<unspecified>)``
    ``((lambda (:optional a b :key c) (list a b c)) 1 :c 3)
    `` => ``&serious``
  `:rest _variable_`
  : Specifies the rest argument. If specified without `:optional`      argument spec, a list of remaining arguments after required arguments are
    taken is bound to _variable_. If specified with `:optional`      argument spec, the actual arguments are first bound to required and all
    optional arguments, and the remaining arguments are bound to
    _variable_.
    ``((lambda (a b :rest z) (list a b z)) 1 2 3 4 5)
    `` => ``(1 2 (3 4 5))``
    ``((lambda (a b :optional c d :rest z) (list a b z)) 1 2 3 4 5)
    `` => ``(1 2 3 4 (5))``
    ``      ((lambda (a b :optional c d :rest z) (list a b z)) 1 2 3)
    `` => ``(1 2 3 #\<unspecified> ())``
    When the rest argument spec is used with the keyword argument spec, both
    accesses the same list of actual argument -- the remaining arguments after
    required and optional arguments are taken
    ``((lambda (:optional a :rest r :key k) (list a r k)) 1 :k 3)
    `` => ``(1 (k 3) 3)``
    



### [§3] Conditionals

###### [!Syntax] `if`  _test_ _consequent_ _alternate_
###### [!Syntax] `if`  _test_ _consequent_

[R6RS]An `if` expression is evaluated as follows: first, _test_is evaluated. If it yields a true value, then _consequent_ is evaluated and
its values are returned. Otherwise _alternate_ is evaluated and its values
are returned. If _test_ yields #f and no _alternate_ is specified, then
the result of the expression is unspecified.


### [§3] Assignment

###### [!Syntax] `set!`  _variable_ _expression_

[R6RS] _Expression_ is evaluated, and the resulting value is stored in
the location to which _variable_ is bound. Variable must be bound either in
some region enclosing the `set!` expression or at the top level. The result
of the `set!` expression is unspecified.

Note: R6RS requires to throw syntax violation if _variable_ refers immutable
binding. In Sagittarius, however, it won't throw any error.


### [§3] Derived conditionals

###### [!Syntax] `cond`  _clause_ _..._

[R6RS] Each _clause_ must be the form

``(_test_ _expression_ ...)``

``(_test_ => _expression_)``

``(else _expression_ ...)``

The last form can appear only in the last clause.

A `cond` expression is evaluated by evaluating the _test_ expressions of
successive _clauses_ in order until one of them evaluates to a true value.
When a _test_ evaluates to a true value, then the remaining expressions in
its _clause_ are evaluated in order, and the results of the last expression
in the _clause_ are returned as the results of the entire `cond` expression.
If the selected _clause_ contains only the _test_ and no expressions,
then the value of the _test_ is returned as the result. If the selected
_clause_ uses the `=>` alternate form, then the expression is evaluated.
Its value must be a procedure. This procedure should accept one argument; it is
called on the value of the _test_ and the values returned by this procedure
are returned by the `cond` expression. If all _tests_ evaluate to #f,
and there is no `else` clause, then the conditional expression returns
unspecified values; if there is an `else` clause, then its expressions are
evaluated, and the values of the last one are returned.


###### [!Syntax] `case`  _clause_ _..._

[R6RS] _Key_ must be an expression. Each _clause_ must have one of
the following forms:

``((_datum_ ...) _expression_ ...)``

``(else _expression_ ...)``

The last form can appear only in the last clause.

A `case` expression is evaluated as follows. _Key_ is evaluated and its
result is compared using `eqv?` against the data represented by the _datums_of each _clause_ in turn, proceeding in order from left to right through
the set of clauses. If the result of evaluating _key_ is equivalent to a datum
of a _clause_, the corresponding expressions are evaluated from left to right
and the results of the last expression in the _clause_ are returned as the
results of the `case` expression. Otherwise, the comparison process continues.
If the result of evaluating _key_ is different from every datum in each set,
then if there is an `else` clause its expressions are evaluated and the
results of the last are the results of the `case` expression; otherwise the
`case` expression returns unspecified values.


###### [!Syntax] `and`  _test_ _..._

[R6RS] If there are no _tests_, #t is returned. Otherwise, the
_test_ expressions are evaluated from left to right until a _test_returns #f or the last _test_ is reached. In the former case, the and
expression returns #f without evaluating the remaining expressions. In the
latter case, the last expression is evaluated and its values are returned.

``(and (= 2 2) (> 2 1))`` => ``#t``

``(and (= 2 2) (< 2 1))``

``(and 1 2 'c '(f g))`` => ``(f g)``

``(and)`` => ``#t``



###### [!Syntax] `or`  _test_ _..._

[R6RS] If there are no _tests_, #f is returned. Otherwise, the _test_expressions are evaluated from left to right until a _test_ returns a true
value or the last _test_ is reached. In the former case, the or expression
returns val without evaluating the remaining expressions. In the latter case,
the last expression is evaluated and its values are returned.

``(or (= 2 2) (> 2 1))`` => ``#t``

``(or (= 2 2) (< 2 1))`` => ``#t``

``(or #f #f #f)``

``(or '(b c) (/ 3 0))`` => ``(b c)``



### [§3] Binding constructs

###### [!Syntax] `let`  _bindings_ _body_ _..._

[R6RS] _Bindings_ must have the form

``((_variable1_ _init1_) ...)``

where each _init_ is an expression. Any variable must not appear more than
once in the _variables_.

The _inits_ are evaluated in the current environment, the variables are bound
to fresh locations holding the results, the _body_ is evaluated in the
extended environment, and the values of the last expression of _body_ are
returned. Each binding of a _variable_ has _body_ as its region.


###### [!Syntax] `let*`  _bindings_ _body_ _..._

[R6RS] _Bindings_ must have the form

``((_variable1_ _init1_) ...)``

The `let*` form is similar to `let`, but the _inits_ are evaluated
and bindings created sequentially from left to right, with the region of each
binding including the bindings to its right as well as _body_. Thus the second
_init_ is evaluated in an environment in which the first binding is visible
and initialized, and so on.


###### [!Syntax] `letrec`  _bindings_ _body_ _..._

[R6RS] _Bindings_ must have the form

``((_variable1_ _init1_) ...)``

where each _init_ is an expression. Any variable must not appear more than
once in the _variables_.

The _variables_ are bound to fresh locations, the _inits_ are evaluated
in the resulting environment, each _variable_ is assigned to the result of
the corresponding _init_, the _body_ is evaluated in the resulting environment,
and the values of the last expression in _body_ are returned. Each binding of
a _variable_ has the entire `letrec` expression as its region, making it
possible to define mutually recursive procedures.

In the most common uses of `letrec`, all the _inits_ are `lambda`expressions and the restriction is satisfied automatically.


###### [!Syntax] `letrec*`  _bindings_ _body_ _..._

[R6RS] _Bindings_ must have the form

``((_variable1_ _init1_) ...)``

where each _init_ is an expression. Any variable must not appear more than
once in the _variables_.

The _variables_ are bound to fresh locations, each _variable_ is assigned
in left-to-right order to the result of evaluating the corresponding _init_,
the _body_ is evaluated in the resulting environment, and the values of the
last expression in _body_ are returned. Despite the left-to-right evaluation
and assignment order, each binding of a _variable_ has the entire `letrec*`expression as its region, making it possible to define mutually recursive procedures.


###### [!Syntax] `let-values`  _mv-bindings_ _body_ _..._

[R6RS] _Mv-bindings_ must have the form

``((_formals_ _init1_) ...)``

where each _init_ is an expression. Any variable must not appear more than
once in the set of _formals_.

The _inits_ are evaluated in the current environment, and the variables
occurring in the _formals_ are bound to fresh locations containing the values
returned by the _inits_, where the _formals_ are matched to the return
values in the same way that the _formals_ in a `lambda` expression are
matched to the arguments in a procedure call. Then, the _body_ is evaluated
in the extended environment, and the values of the last expression of _body_are returned. Each binding of a variable has _body_ as its region. If the
_formals_ do not match, an exception with condition type `&assertion`is raised.


###### [!Syntax] `let*-values`  _mv-bindings_ _body_ _..._

[R6RS] _Mv-bindings_ must have the form

``((_formals_ _init1_) ...)``

where each _init_ is an expression. In each _formals_, any variable must
not appear more than once.

The `let*-values` form is similar to `let-values`, but the _inits_are evaluated and bindings created sequentially from left to right, with the
region of the bindings of each _formals_ including the bindings to its right
as well as _body_. Thus the second _init_ is evaluated in an environment
in which the bindings of the first _formals_ is visible and initialized, and
so on.


### [§3] Sequencing

###### [!Syntax] `begin`  _form_ _..._
###### [!Syntax] `begin`  _expression_ _..._

[R6RS]The begin keyword has two different roles, depending on its context:

- It may appear as a form in a body , library body, or top-level body, or
   directly nested in a begin form that appears in a body. In this case, the begin
   form must have the shape specified in the first header line. This use of begin
   acts as a splicing form - the forms inside the body are spliced into the
   surrounding body, as if the begin wrapper were not actually present.
   A begin form in a body or library body must be non-empty if it appears after
   the first expression within the body.
- It may appear as an ordinary expression and must have the shape specified
   in the second header line. In this case, the expressions are evaluated
   sequentially from left to right, and the values of the last expression are
   returned. This expression type is used to sequence side effects such as
   assignments or input and output.



### [§3] Equivalence predicates

A `predicate` is a procedure that always returns a boolean value (#t or #f).
An `equivalence predicate` is the computational analogue of a mathematical
equivalence relation (it is symmetric, reflexive, and transitive). Of the
equivalence predicates described in this section, `eq?` is the finest or
most discriminating, and `equal?` is the coarsest. The `eqv?` predicate
is slightly less discriminating than `eq?`.

###### [!Function] `eq?`  _obj1_ _obj2_
###### [!Function] `eqv?`  _obj1_ _obj2_
###### [!Function] `equal?`  _obj1_ _obj2_

[R6RS] `eq?` only sees if the given two objects are the same object
or not, `eqv?` compares numbers. `equal?` compares the values
equivalence.

On Sagittarius Scheme interned symbol, keyword(only compatible mode), character,
literal string, boolean, fixnum, and '() are used as the same objects. If these
objects indicates the same value then `eq?` returns #t.

The following examples are not specified R6RS. But it is always good to know how
it works.

``(let ((p (lambda (x) x))) (eqv? p p))`` => ``#t``

``(eqv? "" "")`` => ``#t``

``(eqv? "abc" "abc") ;; literal string are the same object`` => ``#t``

``(eqv? "abc" (list->string '(#\a #\b #\c)))``

``(eqv? '#() '#())``

``(eqv? (lambda (x) x) (lambda (x) x))``

``(eqv? (lambda (x) x) (lambda (y) y))``

``(eqv? +nan.0 +nan.0)``



### [§3] Procedure predicate

###### [!Function] `procedure?`  _obj_

[R6RS] Returns #t if _obj_ is a procedure, otherwise returns #f.

### [§3] Numerical type predicates

###### [!Function] `number?`  _obj_
###### [!Function] `complex?`  _obj_
###### [!Function] `real?`  _obj_
###### [!Function] `rational?`  _obj_
###### [!Function] `integer?`  _obj_

[R6RS] These numerical type predicates can be applied to any kind of
argument. They return #t if the object is a number object of the named type, and
#f otherwise. In general, if a type predicate is true of a number object then
all higher type predicates are also true of that number object. Consequently,
if a type predicate is false of a number object, then all lower type predicates
are also false of that number object.

If _z_ is a complex number object, then `(real? _z_)` is true if and
only if `(zero? (imag-part _z_))` and `(exact? (imag-part _z_))`are both true.

If _x_ is a real number object, then `(rational? _x_)` is true if
and only if there exist exact integer objects _k1_ and _k2_ such that
`(= _x_ (/ _k1_ _k2_))` and `(= (numerator _x_) _k1_)`and `(= (denominator _x_) _k2_)` are all true. Thus infinities and
NaNs are not rational number objects.

If _q_ is a rational number objects, then `(integer? _q_)` is true
if and only if `(= (denominator _q_) 1)` is true. If q is not a rational
number object, then `(integer? _q_)` is #f.


###### [!Function] `real-valued?`  _obj_
###### [!Function] `rational-valued?`  _obj_
###### [!Function] `integer-valued?`  _obj_

[R6RS] These numerical type predicates can be applied to any kind of argument.
The `real-valued?` procedure returns #t if the object is a number object and
is equal in the sense of `=` to some real number object, or if the object is
a NaN, or a complex number object whose real part is a NaN and whose imaginary part
is zero in the sense of zero?. The `rational-valued?` and `integer-valued?`procedures return #t if the object is a number object and is equal in the sense
of `=` to some object of the named type, and otherwise they return #f.


###### [!Function] `exact?`  _obj_
###### [!Function] `inexact?`  _obj_

[R6RS] These numerical predicates provide tests for the exactness of a
quantity. For any number object, precisely one of these predicates is true.


### [§3]  Generic conversion

###### [!Function] `exact`  _z_
###### [!Function] `inexact`  _z_

[R6RS] The `inexact` procedure returns an inexact representation of
_z_. If inexact number objects of the appropriate type have bounded precision,
then the value returned is an inexact number object that is nearest to the argument.

The `exact` procedure returns an exact representation of _z_. The value
returned is the exact number object that is numerically closest to the argument;
in most cases, the result of this procedure should be numerically equal to its argument.


### [§3] Arithmetic operations

###### [!Function] `=`  _z1_ _z2_ _z3_ _..._
###### [!Function] `>`  _x1_ _x2_ _x3_ _..._
###### [!Function] `<`  _x1_ _x2_ _x3_ _..._
###### [!Function] `>=`  _x1_ _x2_ _x3_ _..._
###### [!Function] `<=`  _x1_ _x2_ _x3_ _..._

[R6RS] These procedures return #t if their arguments are (respectively):
equal, monotonically increasing, monotonically decreasing, monotonically
nondecreasing, or monotonically nonincreasing, and #f otherwise.


###### [!Function] `zero?`  _z_
###### [!Function] `positive?`  _z_
###### [!Function] `negative?`  _z_
###### [!Function] `odd?`  _z_
###### [!Function] `even?`  _z_
###### [!Function] `finite?`  _z_
###### [!Function] `infinite?`  _z_
###### [!Function] `nan?`  _z_

[R6RS] These numerical predicates test a number object for a particular
property, returning #t or #f.

The `zero?` procedure tests if the number object is `=` to zero.

The `positive?` tests whether it is greater than zero.

The  `negative?` tests whether it is less than zero.

The `odd?` tests whether it is odd.

The `even?` tests whether it is even

The `finite?` tests whether it is not an infinity and not a NaN.

The `infinite?` tests whether it is an infinity.

The `nan?` tests whether it is a NaN.


###### [!Function] `max`  _x1_ _x2_ _..._
###### [!Function] `min`  _x1_ _x2_ _..._

[R6RS] These procedures return the maximum or minimum of their arguments.

###### [!Function] `+`  _z_ _..._
###### [!Function] `*`  _z_ _..._

[R6RS] These procedures return the sum or product of their arguments.

###### [!Function] `-`  _z_ _..._
###### [!Function] `-`  _z1_ _z2_ _..._

[R6RS] With two or more arguments, this procedures returns the difference
of its arguments, associating to the left. With one argument, however, it returns
the additive inverse of its argument.

If this procedure is applied to mixed non-rational real and non-real complex
arguments, it returns an unspecified number object.


###### [!Function] `/`  _z_ _..._
###### [!Function] `/`  _z1_ _z2_ _..._

[R6RS+] If all of the arguments are exact, then the divisors must all be
nonzero. With two or more arguments, this procedure returns the quotient of its
arguments, associating to the left. With one argument, however, it returns the
multiplicative inverse of its argument.

If this procedure is applied to mixed non-rational real and non-real complex
arguments, it returns an unspecified number object.

In R6RS, it requires to raise `&assertion` when the divisor was 0, on
Sagittarius, however, it returns NaN or infinite number when it is running with
compatible mode. In R6RS mode it raises `&assertion`.


###### [!Function] `abs`  _x_

[R6RS] Returns the absolute value of its argument.

###### [!Function] `div-and-mod`  _x1_ _x2_
###### [!Function] `div`  _x1_ _x2_
###### [!Function] `mod`  _x1_ _x2_
###### [!Function] `div0-and-mod0`  _x1_ _x2_
###### [!Function] `div0`  _x1_ _x2_
###### [!Function] `mod0`  _x1_ _x2_

[R6RS] These procedures implement number-theoretic integer division and
return the results of the corresponding mathematical operations. In each case,
_x1_ must be neither infinite nor a NaN, and _x2_ must be nonzero;
otherwise, an exception with condition type `&assertion` is raised.


###### [!Function] `gcd`  _n1_ _..._
###### [!Function] `lcm`  _n1_ _..._

[R6RS] These procedures return the greatest common divisor or least common
multiple of their arguments. The result is always non-negative.


###### [!Function] `numerator`  _q_
###### [!Function] `denominator`  _q_

[R6RS] These procedures return the numerator or denominator of their
argument; the result is computed as if the argument was represented as a fraction
in lowest terms. The denominator is always positive. The denominator of 0 is
defined to be 1.


###### [!Function] `floor`  _x_
###### [!Function] `ceiling`  _x_
###### [!Function] `truncate`  _x_
###### [!Function] `round`  _x_

[R6RS] These procedures return inexact integer objects for inexact arguments
that are not infinities or NaNs, and exact integer objects for exact rational
arguments. For such arguments, `floor` returns the largest integer object
not larger than _x_. The `ceiling` procedure returns the smallest
integer object not smaller than _x_. The `truncate` procedure returns
the integer object closest to _x_ whose absolute value is not larger than
the absolute value of _x_. The `round` procedure returns the closest
integer object to _x_, rounding to even when _x_ represents a number
halfway between two integers.

Although infinities and NaNs are not integer objects, these procedures return
an infinity when given an infinity as an argument, and a NaN when given a NaN.


###### [!Function] `rationalize`  _x1_ _x2_

[R6RS] The `rationalize` procedure returns the a number object
representing the simplest rational number differing from _x1_ by no more than
_x2_. A rational number _r1_ is simpler than another rational number
_r2_ if _r1_ = _p1_/_q1_ and _r2_ = _p2_/_q2_ (in
lowest terms) and |_p1_| ≤ |_p2_| and |_q1_| ≤ |_q2_|. Thus 3/5
is simpler than 4/7. Although not all rationals are comparable in this ordering
(consider 2/7 and 3/5) any interval contains a rational number that is simpler
than every other rational number in that interval (the simpler 2/5 lies between
2/7 and 3/5). Note that 0 = 0/1 is the simplest rational of all.


###### [!Function] `exp`  _z_
###### [!Function] `log`  _z_
###### [!Function] `log`  _z1_ _z2_
###### [!Function] `sin`  _z_
###### [!Function] `cos`  _z_
###### [!Function] `tan`  _z_
###### [!Function] `asin`  _z_
###### [!Function] `acos`  _z_
###### [!Function] `atan`  _z_
###### [!Function] `atan`  _z1_ _z2_

[R6RS] These procedures compute the usual transcendental functions. The
`exp` procedure computes the base-e exponential of _z_.

The `log` procedure with a single argument computes the natural logarithm
of _z_ (not the base-ten logarithm); `(log _z1_ _z2_)` computes
the base-_z2_ logarithm of _z1_.

The `asin`, `acos`, and `atan` procedures compute arcsine,
arccosine, and arctangent, respectively.

The two-argument variant of `atan` computes
`(angle (make-rectangular _x2_ _x1_))`.


###### [!Function] `sqrt`  _z_

[R6RS] Returns the principal square root of _z_. For rational _z_,
the result has either positive real part, or zero real part and non-negative imaginary part.

The _sqrt_ procedure may return an inexact result even when given an exact argument.


###### [!Function] `expt`  _z1_ _z2_

[R6RS] Returns _z1_ raised to the power _z2_. For nonzero _z1_,
this is _e^{z2 log z1}_. 0.0z is 1.0 if z = 0.0, and 0.0 if `(real-part _z_)` is
positive. For other cases in which the first argument is zero, an unspecified
number object(`+nan.0+nan.0i`) is returned.

For an exact real number object _z1_ and an exact integer object _z2_,
`(expt _z1_ _z2_)` must return an exact result. For all other values
of _z1_ and _z2_, `(expt _z1_ _z2_)` may return an inexact
result, even when both _z1_ and _z2_ are exact.


###### [!Function] `make-rectangular`  _x1_ _x2_
###### [!Function] `make-polar`  _x3_ _x4_
###### [!Function] `real-part`  _z_
###### [!Function] `imag-part`  _z_
###### [!Function] `magnitude`  _z_
###### [!Function] `angle`  _z_

[R6RS] Suppose _a1_, _a2_, _a3_, and _a4_ are real numbers,
and _c_ is a complex number such that the following holds:

_c = a1 + a2 ^ i = a3 e ^ ia4_Then, if _x1_, _x2_, _x3_, and _x4_ are number objects representing
_a1_, _a2_, _a3_, and _a4_, respectively,
`(make-rectangular _x1_ _x2_)` returns _c_, and
`(make-polar _x3_ _x4_)` returns _c_.


### [§3]  Numerical input and output

###### [!Function] `number->string`  _z_ _:optional_ _radix_ _precision_

[R6RS+] _Radix_ must be an exact integer object, between 2, and 32. If
omitted, _radix_ defaults to 10. In Sagittarius _precision_ will be
ignored. The `number->string` procedure takes a number object and a radix
and returns as a string an external representation of the given number object in
the given _radix_ such that

``````````scheme
(let ((number _z_) (radix _radix_))
  (eqv? (string->number
          (number->string number radix)
          radix)
        number))
``````````

is true.


###### [!Function] `number->string`  _string_ _:optional_ _radix_

[R6RS+] Returns a number object with maximally precise representation
expressed by the given _string_. _Radix_ must be an exact integer object,
between 2, and 32. If supplied, _radix_ is a default radix that may be
overridden by an explicit radix prefix in _string_ (e.g., "#o177"). If
_radix_ is not supplied, then the default radix is 10. If _string_ is
not a syntactically valid notation for a number object or a notation for a
rational number object with a zero denominator, then `string->number`returns #f.

These `number->string` and `string->number`'s resolutions of radix are
taken from Gauche.


### [§3] Booleans

###### [!Function] `not`  _obj_

[R6RS] Returns #t if _obj_ is #f, and returns #f otherwise.

###### [!Function] `boolean?`  _obj_

[R6RS] Returns #t if _obj_ is either #t or #f and returns #f otherwise.

###### [!Function] `boolean=?`  _bool1_ _bool2_ _bool3_ _..._

[R6RS] Returns #t if the booleans are the same.

### [§3] Pairs and lists

A _pair_ is a compound structure with two fields called the car and cdr fields
(for historical reasons). Pairs are created by the procedure `cons`. The car
and cdr fields are accessed by the procedures `car` and `cdr`.

Pairs are used primarily to represent lists. A list can be defined recursively as
either the empty list or a pair whose cdr is a list. More precisely, the set of
lists is defined as the smallest set _X_ such that


- The empty list is in
- If list is in _X_, then any pair whose cdr field contains _list_is also in _X_.

The objects in the car fields of successive pairs of a list are the elements of
the list. For example, a two-element list is a pair whose car is the first element
and whose cdr is a pair whose car is the second element and whose cdr is the empty
list. The length of a list is the number of elements, which is the same as the
number of pairs.

The empty listis a special object of its own type. It is not a pair. It has no
elements and its length is zero.

A chain of pairs not ending in the empty list is called an _improper list_.
Note that an improper list is not a list. The list and dotted notations can be
combined to represent improper lists:

``(a b c . d)``

is equivalent to

``(a . (b . (c . d)))``

Whether a given pair is a list depends upon what is stored in the cdr field.

###### [!Function] `pair?`  _obj_

[R6RS] Returns #t if _obj_ is a pair, and otherwise returns #f.

###### [!Function] `cons`  _obj1_ _obj2_

[R6RS] Returns a newly allocated pair whose car is _obj1_ and whose
cdr is _obj2_. The pair is guaranteed to be different (in the sense of 
`eqv?`) from every existing object.


###### [!Function] `car`  _pair_
###### [!Function] `cdr`  _pair_

[R6RS] Returns the contents of the car/cdr field of _pair_.

###### [!Function] `caar`  _pair_
###### [!Function] `cadr`  _pair_
  :
###### [!Function] `cadddr`  _pair_
###### [!Function] `cddddr`  _pair_

[R6RS] These procedures are compositions of `car` and `cdr`,
where for example `caddr` could be defined by

``(define caddr (lambda (x) (car (cdr (cdr x)))))``

.

Arbitrary compositions, up to four deep, are provided. There are twenty-eight
of these procedures in all.


###### [!Function] `null?`  _obj_

[R6RS] Returns #t if _obj_ is the empty list, #f otherwise.

###### [!Function] `list?`  _obj_

[R6RS] Returns #t if _obj_ is a list, #f otherwise. By definition, all
lists are chains of pairs that have finite length and are terminated by the
empty list.


###### [!Function] `list`  _obj_ _..._

[R6RS] Returns a newly allocated list of its arguments.

###### [!Function] `length`  _list_

[R6RS+] Returns the length of _list_. If the _list_ is improper
list, it returns -1 If the _list_ is infinite list , it returns -2.


###### [!Function] `append`  _list_ _..._ _obj_

[R6RS] Returns a possibly improper list consisting of the elements of the
first _list_ followed by the elements of the other _lists_, with _obj_as the cdr of the final pair. An improper list results if _obj_ is not a list.


###### [!Function] `reverse`  _list_

[R6RS] Returns a newly allocated list consisting of the elements of
_list_ in reverse order.


###### [!Function] `list-tail`  _list_ _k_ _:optaionl_ _fallback_

[R6RS+] The `list-tail` procedure returns the subchain of pairs of
_list_ obtained by omitting the first _k_ elements. If _fallback_is given and _k_ is out of range, it returns _fallback_ otherwise
`&assertion` is raised.


###### [!Function] `list-ref`  _list_ _k_ _:optaionl_ _fallback_

[R6RS+] The `list-ref` procedure returns the _k_th element of
_list_. If _fallback_ is given and _k_ is out of range, it returns
_fallback_ otherwise `&assertion` is raised.


###### [!Function] `map`  _proc_ _list1_ _list2_ _..._

[R6RS+ SRFI-1] The `map` procedure applies _proc_ element-wise to
the elements of the _lists_ and returns a list of the results, in order. The
order in which _proc_ is applied to the elements of the lists is unspecified.
If multiple returns occur from `map`, the values returned by earlier returns
are not mutated. If the given _lists_ are not the same length, when the
shortest list is processed the `map` will stop.


###### [!Function] `for-each`  _proc_ _list1_ _list2_ _..._

[R6RS+ SRFI-1] The `for-each` procedure applies _proc_ element-wise
to the elements of the _lists_ for its side effects, in order from the first
elements to the last. The return values of `for-each` are unspecified. If
the given _lists_ are not the same length, when the shortest list is
processed the `for-each` will stop.


### [§3] Symbols

###### [!Function] `symbol?`  _obj_

[R6RS] Returns #t if _obj_ is a symbol, otherwise returns #f.

###### [!Function] `symbol->string`  _symbol_

[R6RS] Returns the name of _symbol_ as an immutable string.

###### [!Function] `symbol=?`  _symbol1_ _symbol2_ _symbol3_ _..._

[R6RS] Returns #t if the _symbols_ are the same, i.e., if their names
are spelled the same.

###### [!Function] `string->symbol`  _string_

[R6RS] Returns the symbol whose name is _string_.

### [§3] Characters

_Characters_ are objects that represent Unicode scalar values.

###### [!Function] `char?`  _obj_

[R6RS] Returns #t if _obj_ is a character, otherwise returns #f.

###### [!Function] `char->integer`  _char_
###### [!Function] `integer->char`  _sv_

[R6RS] _Sv_ must be a Unicode scalar value, i.e., a non-negative exact
integer object in [0, #xD7FF] ∪ [#xE000, #x10FFFF].

Given a character, `char->integer` returns its Unicode scalar value as an
exact integer object. For a Unicode scalar value _sv_, `integer->char`returns its associated character.


###### [!Function] `char=?`  _char1_ _char2_ _char3_ _..._
###### [!Function] `char<?`  _char1_ _char2_ _char3_ _..._
###### [!Function] `char>?`  _char1_ _char2_ _char3_ _..._
###### [!Function] `char<=?`  _char1_ _char2_ _char3_ _..._
###### [!Function] `char>=?`  _char1_ _char2_ _char3_ _..._

[R6RS] These procedures impose a total ordering on the set of characters
according to their Unicode scalar values.


### [§3] Strings

Strings are sequences of characters.
The _length_ of a string is the number of characters that it contains. This
number is fixed when the string is created. The _valid indices_ of a string
are the integers less than the length of the string. The first character of a
string has index 0, the second has index 1, and so on.

###### [!Function] `string?`  _obj_

[R6RS] Returns #t if _obj_ is a string, otherwise returns #f.

###### [!Function] `make-string`  _k_ _:optional_ _char_

[R6RS] Returns a newly allocated string of length _k_. If _char_is given, then all elements of the string are initialized to _char_, otherwise
the contents of the string are `#\space`.

These are equivalence:
``(make-string 10)`` => ``(code (make-string 10 #\\space))``



###### [!Function] `string`  _char_ _..._

[R6RS] Returns a newly allocated string composed of the arguments.

###### [!Function] `string-length`  _string_

[R6RS] Returns the number of characters in the given _string_ as an
exact integer object.

###### [!Function] `string-ref`  _string_ _k_ _:optional_ _fallback_

[R6RS+] The `string-ref` procedure returns character. If a third
argument is given ant _k_ is not a valid index, it returns _fallback_,
otherwise raises `&assertion`.

_k_ of string using zero-origin indexing.


###### [!Function] `string=?`  _string1_ _string2_ _string3_ _..._

[R6RS] Returns #t if the _strings_ are the same length and contain the
same characters in the same positions. Otherwise, the `string=?` procedure
returns #f.


###### [!Function] `string<?`  _string1_ _string2_ _string3_ _..._
###### [!Function] `string>?`  _string1_ _string2_ _string3_ _..._
###### [!Function] `string<=?`  _string1_ _string2_ _string3_ _..._
###### [!Function] `string>=?`  _string1_ _string2_ _string3_ _..._

[R6RS] These procedures are the lexicographic extensions to strings of the
corresponding orderings on characters. For example, `string<?` is the
lexicographic ordering on strings induced by the ordering `char<?` on
characters. If two strings differ in length but are the same up to the length of
the shorter string, the shorter string is considered to be lexicographically less
than the longer string.


###### [!Function] `substring`  _string_ _start_ _end_

[R6RS] _String_ must be a string, and _start_ and _end_ must
be exact integer objects satisfying

0 ≤ _start_ ≤ _end_ ≤ `(string-length _string_)`.

The `substring` procedure returns a newly allocated string formed from the
characters of string beginning with index _start_ (inclusive) and ending with
index _end_ (exclusive).


###### [!Function] `string-append`  _string_ _..._

[R6RS] Returns a newly allocated string whose characters form the
concatenation of the given _strings_.


###### [!Function] `string->list`  _string_
###### [!Function] `list->string`  _list_

[R6RS+] _List_ must be a list of characters.

The `string->list` procedure returns a newly allocated list of the
characters that make up the given _string_.

The `list->string` procedure returns a newly allocated string formed from
the characters in _list_.

The `string->list` and `list->string` procedures are inverses so far
as `equal?` is concerned.

If optional argument _start_ and _end_ are given, it restrict the
conversion range. It convert from _start_ (inclusive) to _end_(exclusive).

If only _start_ is given, then the _end_ is the length of given string.


###### [!Function] `string-for-each`  _proc_ _string1_ _string2_ _..._

[R6RS+] _Proc_ should accept as many arguments as there are _strings_.
The `string-for-each` procedure applies _proc_ element-wise to the
characters of the _strings_ for its side effects, in order from the first
characters to the last. The return values of `string-for-each` are unspecified.

Analogous to for-each.


###### [!Function] `string-copy`  _string_ _:optional_ _(start_ _0)_ _(end_ _-1)_

[R6RS+] Returns a newly allocated copy of the given _string_.

If optional argument _start_ was given, the procedure copies from the given
_start_ index.

If optional argument _end_ was given, the procedure copies to the given
_end_ index (exclusive).


### [§3] Vectors

Vectors are heterogeneous structures whose elements are indexed by integers. A
vector typically occupies less space than a list of the same length, and the
average time needed to access a randomly chosen element is typically less for the
vector than for the list.

The _length_ of a vector is the number of elements that it contains. This
number is a non-negative integer that is fixed when the vector is created. The
_valid indices_ of a vector are the exact non-negative integer objects less
than the length of the vector. The first element in a vector is indexed by zero,
and the last element is indexed by one less than the length of the vector.

###### [!Function] `vector?`  _obj_

[R6RS] Returns #t if _obj_ is a vector. Otherwise returns #f.

###### [!Function] `make-vector`  _k_ _:optional_ _fill_

[R6RS] Returns a newly allocated vector of _k_ elements. If a second
argument is given, then each element is initialized to _fill_. Otherwise the
initial contents of each element is unspecified.


###### [!Function] `vector`  _obj_ _..._

[R6RS] Returns a newly allocated vector whose elements contain the given
arguments. Analogous to `list`.

###### [!Function] `vector-length`  _vector_

[R6RS] Returns the number of elements in _vector_ as an exact integer
object.

###### [!Function] `vector-ref`  _vector_ _k_ _:optional_ _fallback_

[R6RS+] The `vector-ref` procedure returns the contents of element
_k_ of _vector_. If a third argument is given and _k_ is not a valid
index, it returns _fallback_, otherwise raises `&assertion`.


###### [!Function] `vector-ref`  _vector_ _k_ _obj_

[R6RS] _K_ must be a valid index of _vector_. The `vector-set!`procedure stores _obj_ in element _k_ of _vector_, and returns
unspecified values.

It raises `&assertion` when it attempt to modify immutable vector on R6RS
mode.


###### [!Function] `vector->list`  _vector_ _:optional_ _start_ _end_
###### [!Function] `list->vector`  _list_ _:optional_ _start_ _end_

[R6RS+] The `vector->list` procedure returns a newly allocated list
of the objects contained in the elements of _vector_. The `list->vector`procedure returns a newly created vector initialized to the elements of the list
_list_. The optional _start_ and _end_ arguments limit the range of
the source.

``(vector->list '#(1 2 3 4 5))`` => ``(1 2 3 4 5)``

``(list->vector '(1 2 3 4 5))`` => ``#(1 2 3 4 5)``

``(vector->list '#(1 2 3 4 5) 2 4)`` => ``(3 4)``

``(list->vector (circular-list 'a 'b 'c) 1 6)`` => ``#(b c a b c)``



###### [!Function] `vector-fill!`  _vector_ _:optional_ _start_ _end_

[R6RS+] Stores _fill_ in every element of _vector_ and returns
unspecified values. Optional _start_ and _end_ limits the range of effect
between _start_-th index (inclusive) to _end_-th index (exclusive).
_Start_ defaults to zero, and _end_ defaults to the length of _vector_.


###### [!Function] `vector-map`  _proc_ _vector1_ _vactor2_ _..._

[R6RS+] _Proc_ should accept as many arguments as there are _vectors_.
The `vector-map` procedure applies _proc_ element-wise to the elements
of the _vectors_ and returns a vector of the results, in order. If multiple
returns occur from `vector-map`, the return values returned by earlier
returns are not mutated.

Analogous to `map`.


###### [!Function] `vector-for-each`  _proc_ _vector1_ _vactor2_ _..._

[R6RS+] _Proc_ should accept as many arguments as there are _vectors_.
The `vector-for-each` procedure applies _proc_ element-wise to the
elements of the _vectors_ for its side effects, in order from the first
elements to the last. The return values of `vector-for-each` are unspecified.

Analogous to `for-each`.


### [§3] Errors and violations

###### [!Function] `error`  _who_ _message_ _irritant_ _..._
###### [!Function] `assertion-violation`  _who_ _message_ _irritant_ _..._

[R6RS] _Who_ must be a string or a symbol or #f. _Message_ must be
a string. The _irritants_ are arbitrary objects.

These procedures raise an exception. The `error` procedure should be called
when an error has occurred, typically caused by something that has gone wrong in
the interaction of the program with the external world or the user. The
`assertion-violation` procedure should be called when an invalid call to a
procedure was made, either passing an invalid number of arguments, or passing an
argument that it is not specified to handle.

The _who_ argument should describe the procedure or operation that detected
the exception. The _message_ argument should describe the exceptional situation.
The _irritants_ should be the arguments to the operation that detected the
operation.


### [§3] Control features

###### [!Function] `apply`  _proc_ _arg1_ _..._ _rest-args_

[R6RS] _Rest-args_ must be a list. _Proc_ should accept n arguments,
where n is number of _args_ plus the length of _rest-args_. The `apply`procedure calls _proc_ with the elements of the list
`(append (list _arg1_ ...) _rest-args_)` as the actual arguments.

If a call to `apply` occurs in a tail context, the call to `proc` is
also in a tail context.


###### [!Function] `call-with-current-continuation`  _proc_
###### [!Function] `call/cc`  _proc_

[R6RS] _Proc_ should accept one argument. The procedure 
`call-with-current-continuation` (which is the same as the procedure
`call/cc`) packages the current continuation as an "escape procedure" and
passes it as an argument to _proc_. The escape procedure is a Scheme procedure
that, if it is later called, will abandon whatever continuation is in effect a
that later time and will instead reinstate the continuation that was in effect
when the escape procedure was created. Calling the escape procedure may cause
the invocation of _before_ and _after_ procedures installed using
`dynamic-wind`.

The escape procedure accepts the same number of arguments as the continuation
of the original call to `call-with-current-continuation`.


###### [!Function] `values`  _obj_ _..._

[R6RS] Returns _objs_ as multiple values.


###### [!Function] `call-with-values`  _producer_ _consumer_

[R6RS] _Producer_ must be a procedure and should accept zero arguments.
_Consumer_ must be a procedure and should accept as many values as
_producer_ returns. The `call-with-values` procedure calls _producer_with no arguments and a continuation that, when passed some values, calls the
_consumer_ procedure with those values as arguments. The continuation for
the call to _consumer_ is the continuation of the call to `call-with-values`.

If a call to `call-with-values` occurs in a tail context, the call to
_consumer_ is also in a tail context.


###### [!Function] `dynamic-wind`  _before_ _thunk_ _after_

[R6RS] _Before_, _thunk_, and _after_ must be procedures, and
each should accept zero arguments. These procedures may return any number of
values. The `dynamic-wind` procedure calls _thunk_ without arguments,
returning the results of this call. Moreover, `dynamic-wind` calls _before_without arguments whenever the dynamic extent of the call to _thunk_ is
entered, and _after_ without arguments whenever the dynamic extent of the
call to _thunk_ is exited. Thus, in the absence of calls to escape procedures
created by `call-with-current-continuation`, `dynamic-wind` calls
_before_, _thunk_, and _after_, in that order.


### [§3] Iteration

###### [!Syntax] `let`  _variable_ _bindings_ _body_ _..._

[R6RS] “Named let” is a variant on the syntax of `let` that provides
a general looping construct and may also be used to express recursion. It has
the same syntax and semantics as ordinary `let` except that _variable_is bound within _body_ to a procedure whose parameters are the bound variables
and whose body is _body_. Thus the execution of _body_ may be repeated
by invoking the procedure named by _variable_.


### [§3] Quasiquote

###### [!Syntax] `quasiquote`  _qq-template_
###### [!Auxiliary Syntax] `unquote`  _qq-template_
###### [!Auxiliary Syntax] `unquote-splicing`  _qq-template_

[R6RS] "Quasiquote" expressions is useful for constructing a list or
vector structure when some but not all of the desired structure is known in
advance.

_Qq-template_ should be as specified by the grammar at the end of this entry.

If no `unquote` or `unquote-splicing` forms appear within the
_qq-template_, the result of evaluating `(quasiquote _qq-template_)`is equivalent to the result of evaluating `(quote _qq-template_)`.

If an `(unquote _expression_ ...)` form appears inside a _qq-template_,
however, the expressions are evaluated `("unquoted")` and their results are
inserted into the structure instead of the `unquote` form.

If an `(unquote-splicing _expression_ ...)` form appears inside a
_qq-template_, then the expressions must evaluate to lists; the opening and
closing parentheses of the lists are then "stripped away" and the elements of
the lists are inserted in place of the `unquote-splicing` form.

Any `unquote-splicing` or multi-operand unquote form must appear only within
a list or vector _qq-template_.

Note: even though `unquote` and `unquote-splicing` are bounded, however
it does not work with import prefix nor renamed import. This may be fixed in future.


### [§3] Binding constructs for syntactic keywords

The `let-syntax` and `letrec-syntax` forms bind keywords. On R6RS mode
it works like a `begin` form, a `let-syntax` or `letrec-syntax`form may appear in a definition context, in which case it is treated as a
definition, and the forms in the body must also be definitions. A `let-syntax`or `letrec-syntax` form may also appear in an expression context, in which
case the forms within their bodies must be expressions.

###### [!Syntax] `let-syntax`  _bindings_ _form_ _..._

[R6RS] _Bindings_ must have the form

``((_keyword_ _expression_) ...)``

Each _keyword_ is an identifier, and each _expression_ is an expression
that evaluates, at macro-expansion time, to a transformer. Transformers may be
created by `syntax-rules` or `identifier-syntax` or by one of the other
mechanisms described in library chapter on ["syntax-case"](#rnrs.syntax-case.6).
It is a syntax violation for _keyword_ to appear more than once in the list
of keywords being bound.

The _forms_ are expanded in the syntactic environment obtained by extending
the syntactic environment of the `let-syntax` form with macros whose keywords
are the _keywords_, bound to the specified transformers. Each binding of a 
_keyword_ has the _forms_ as its region.


###### [!Syntax] `letrec-syntax`  _bindings_ _form_ _..._

[R6RS] _Bindings_ must have the form

``((_keyword_ _expression_) ...)``

Each _keyword_ is an identifier, and each _expression_ is an expression
that evaluates, at macro-expansion time, to a transformer. Transformers may be
created by `syntax-rules` or `identifier-syntax` or by one of the other
mechanisms described in library chapter on ["syntax-case"](#rnrs.syntax-case.6).
It is a syntax violation for _keyword_ to appear more than once in the list
of keywords being bound.

The _forms_ are expanded in the syntactic environment obtained by extending
the syntactic environment of the `letrec-syntax` form with macros whose
keywords are the _keywords_, bound to the specified transformers. Each
binding of a _keyword_ has the bindings as well as the _forms_ within its
region, so the transformers can transcribe _forms_ into uses of the macros
introduced by the `letrec-syntax` form.

Note: The _forms_ of a `let-syntax` and a `letrec-syntax` form are
treated, whether in definition or expression context, as if wrapped in an implicit
`begin` on R6RS mode, it is, then, treated as if wrapped in an implicit
`let` on compatible mode. Thus on compatible mode, it creates a scope.


### [§3] Macro transformers

In R6RS, it requires `'_'` `'...'` as bounded symbols but in Sagittarius
these are not bound. And if import clause has rename or prefix these auxiliary
syntax are not be renamed or prefixed. This behaivour may be fixed in future.

###### [!Syntax] `syntax-rules`  _(literal_ _..._ _)_ _rule_ _..._

[R6RS] Each _literal_ must be an identifier. Each _rule_ must have
the following form:

``(srpattern template)``

An _srpattern_ is a restricted form of _pattern_, namely, a nonempty
_pattern_ in one of four parenthesized forms below whose first subform is
an identifier or an underscore `_`. A _pattern_ is an identifier,
constant, or one of the following.


- (pattern ...)
- (pattern pattern ... . pattern)
- (pattern ... pattern ellipsis pattern ...)
- (pattern ... pattern ellipsis pattern ... . pattern)
- #(pattern ...)
- #(pattern ... pattern ellipsis pattern ...)

An _ellipsis_ is the identifier `"..."` (three periods).

A _template_ is a pattern variable, an identifier that is not a pattern
variable, a pattern datum, or one of the following.


- (subtemplate ...)
- (subtemplate ... . template)
- #(subtemplate ...)

A _subtemplate_ is a _template_ followed by zero or more ellipses.

An instance of `syntax-rules` evaluates, at macro-expansion time, to a new
macro transformer by specifying a sequence of hygienic rewrite rules. A use of a
macro whose keyword is associated with a transformer specified by `syntax-rules`is matched against the patterns contained in the _rules_, beginning with the
leftmost _rule_. When a match is found, the macro use is transcribed hygienically
according to the _template_. It is a syntax violation when no match is found.


###### [!Syntax] `identifier-syntax`  _template_
###### [!Syntax] `identifier-syntax`  _(id1_ _template1)_ _(set!_ _id2_ _pattern)_ _template2_

[R6RS] The _ids_ must be identifiers. The _templates_ must be as
for `syntax-rules`.

When a keyword is bound to a transformer produced by the first form of 
`identifier-syntax`, references to the keyword within the scope of the
binding are replaced by _template_.

``(define p (cons 4 5))``

``(define-syntax p.car (identifier-syntax (car p)))``

``p.car`` => ``4``

``(set! p.car 15)`` => ``&syntax exception``

The second, more general, form of `identifier-syntax` permits the transformer
to determine what happens when `set!` is used. In this case, uses of the
identifier by itself are replaced by _template1_, and uses of `set!` with
the identifier are replaced by _template2_``(define p (cons 4 5))``

``````````scheme
(define-syntax p.car
  (identifier-syntax
    (_ (car p))
    ((set! _ e) (set-car! p e))))
``````````

``(set! p.car 15)``

``p.car`` => ``15``

``p`` => ``(15 5)``



