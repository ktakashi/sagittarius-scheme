[§2] (sagittarius control) - control library {#lib.sagittarius.control}
-------------

###### [!Library] `(sagittarius control)` 

This library provides some useful macros using Sagittarius specific
functions.

###### [!Macro] `define-macro`  _name_ _procedure_
###### [!Macro] `define-macro`  _(name_ _._ _formals)_ _body_ _..._

Defines _name_ to be a macro whose transformer is _procedure_.
The second form is a shorthand notation of the following form:

``(define-macro _name_ (lambda _formals_ _body_ ...))``



###### [!Macro] `let-optionals*`  _restargs_ _(var-spec_ _..._ _)_ _body_ _..._
###### [!Macro] `let-optionals*`  _restargs_ _(var-spec_ _..._ _._ _restvar)_ _body_ _..._

Given a list of values _restargs_, binds variables according to
_var-spec_, then evaluates _body_.

_Var-spec_ can be either a symbol, or a list of two elements and its car is
a symbol. The symbol is the bound variable name. The values in _restargs_are bound to the symbol in order. If there are not as many values in restargs as
_var-spec_, the rest of symbols are bound to the default values, determined
as follows:

If _var-spec_ is just a symbol, the default value is undefined.

If _var-spec_ is a list, the default value is the result of evaluation of
the second element of the list.

In the latter case the second element is only evaluated when there are not
enough arguments. The binding proceeds in the order of _var-spec_, so the 
second element may refer to the bindings of previous _var-spec_.

In the second form, _restvar_ must be a symbol and bound to the list of
values whatever left from _restargs_ after binding to _var-spec_.

It is not an error if _restarg_ has more values than _var-specs_. The
extra values are simply ignored in the first form. 


###### [!Macro] `get-optionals`  _restargs_ _default_
###### [!Macro] `get-optionals`  _restargs_ _default_ _test_

This is a short version of `let-optionals*` where you have only one
optional argument. Given the optional argument list _restargs_, this macro
returns the value of optional argument if one is given, or the result of
_default_ otherwise. 

If latter form is used, _test_ must be procedure which takes one argument
and it will be called to test the given argument. If the test failed, it raises
`&error` condition.

_Default_ is not evaluated unless restargs is an empty list.


###### [!Macro] `let-keywords`  _restargs_ _(var-spec_ _..._ _)_ _body_ _..._
###### [!Macro] `let-keywords`  _restargs_ _(var-spec_ _..._ _._ _restvar)_ _body_ _..._

This macro is for keyword arguments. _Var-spec_ can be one of the
following forms:

``(_symbol_ _expr_)``

If the _restrag_ contains keyword which has the same name as _symbol_,
binds symbol to the corresponding value. If such a keyword doesn't appear in 
_restarg_, binds symbol to the result of _expr_.

``(_symbol_ _keyword_ _expr_)``

If the _restarg_ contains keyword _keyword_, binds symbol to the
corresponding value. If such a keyword doesn't appear in restarg, binds symbol
to the result of _expr_. 

The default value _expr_ is only evaluated when the keyword is not given to
the _restarg_.

If you use the first form, `let-keyword` raises `&error` condition
when _restarg_ contains a keyword argument that is not listed in
_var-specs_. When you want to allow keyword arguments other than listed in
_var-specs_, use the second form.

In the second form, _restvar_ must be either a symbol or #f. If it is a
symbol, it is bound to a list of keyword arguments that are not processed by
_var-specs_. If it is #f, such keyword arguments are just ignored.

``````````scheme
(define (proc x . options)
  (let-keywords options ((a 'a)
                         (b :beta 'b)
                         (c 'c)
                         . rest)
    (list x a b c rest)))
``````````

``(proc 0)`` => ``(0 a b c ())``

``(proc 0 :a 1)`` => ``(0 1 b c ())``

``(proc 0 :beta 1)`` => ``(0 a 1 c ())``

``(proc 0 :beta 1 :c 3 :unknown 4)`` => ``(0 a 1 3 (unknown 4))``



###### [!Macro] `let-keywords*`  _restargs_ _(var-spec_ _..._ _)_ _body_ _..._
###### [!Macro] `let-keywords*`  _restargs_ _(var-spec_ _..._ _._ _restvar)_ _body_ _..._

Like `let-keywords`, but the binding is done in the order of
_var-specs_. So each _expr_ can refer to the variables bound by
preceding _var-specs_.

These let-keywords and let-keywords\* are originally from Gauche. 


###### [!Macro] `define-with-key`  _variable_ _expression_
###### [!Macro] `define-with-key`  _variable_
###### [!Macro] `define-with-key`  _(variable_ _formals)_ _body_ _..._
###### [!Macro] `define-with-key`  _(variable_ _._ _formals)_ _body_ _..._

The `define-with-key` is synonym of `define`.

See more detail
[Variable definitions](#rnrs.base.6.variable.definitions).


###### [!Macro] `begin0`  _exp0_ _exp1_ _..._

Evaluate _exp0_, _exp1_, ..., then returns the result(s) of
_exp0_.


###### [!Macro] `let1`  _var_ _expr_ _body_ _..._

A convenient macro when you have only one variable. Expanded as follows:
``(let ((_var_ _expr_)) _body_ ...)``



###### [!Macro] `rlet1`  _var_ _expr_ _body_ _..._

A convenient macro when you have only one variable and is the returning
value. Expanded as follows:
``(let ((_var_ _expr_)) _body_ ... _var_)``



###### [!Macro] `dotimes`  _(variable_ _limit_ _[result])_ _body_ _..._
###### [!Macro] `dolist`  _(variable_ _lexpr_ _[result])_ _body_ _..._

Imported from Common List. These are equivalent to the following forms,
respectively.

``````````scheme
(dotimes (variable limit result) body ...)
``````````

=>

``````````scheme
(do ((tlimit limit)
     (variable 0 (+ variable 1)))
    ((>= variable tlimit) result)
  body ...)
``````````

``````````scheme
(dolist (variable lexpr result) body ...)
``````````

=>

``````````scheme
(begin
  (for-each (lambda (variable) body ...) lexpr)
  (let ((variable '())) result))
``````````



###### [!Macro] `push!`  _place_ _item_

Conses _item_ and the value of _place_. The _place_ must be
either a variable or a form _(proc arg ...)_, as the second argument of
`set!`. The result will be the same as `set!`.


###### [!Macro] `pop!`  _place_

Retrieves the value of _place_, sets its cde back to _place_.


###### [!Macro] `check-arg`  _pred_ _val_ _proc_

Check the given _val_ satisfies _pred_. If the _pred_ returns
#f then `&assertion` is raised.

The _proc_ should be the procedure name which uses this macro and is for
 debugging purpose.


###### [!Macro] `with-library`  _library_ _exprs_ _..._

_library_ must be a library name. ex. (srfi :1 lists)

_exprs_ must be expressions.

Evaluate given expressions one by one in the specified library and returns the
last result of the expressions.

This should not be used casually however you want to use some procedures or
variables which are not exported, such as a procedure written in C but not
exported or non exported record accessor. For thoese purpose, this might be a
quick solution.


###### [!Macro] `unwind-protect`  _body_ _cleanups_ _..._

Execute _body_ then execute _cleanups_ and returns the result(s)
of _body_.

It is not guaranteed to invoke the _cleanups_ only once if a continuation is
captured in _body_ and call it.


###### [!Macro] `datum`  _x_

Short form of `syntax->datum`.

###### [!Macro] `with-syntax*`  _((p_ _e0)_ _...)_ _e1_ _e2_ _..._

The macro is similar with `with-syntax`, the only difference is
that this macro can refer previous pattern of _p_ as if `let*` can.

This can reduce nest level when users need to write multiple 
`with-syntax` to refer bound syntax object.


###### [!Macro] `^`  _formals_ _body_ _..._

The alias of `lambda`.

###### [!Macro] `^c`  _body_ _..._

Shortened notation of `(lambda (c) body ...)`. Where `c`can be any character of lower case of ASCII alphabet.

``(map (^z (* z z)) '(1 2 3 4 5))`` => ``(1 4 9 16 25)``



###### [!Macro] `^c*`  _body_ _..._

Shortened notation of `(lambda c body ...)`. Where `c`can be any character of lower case of ASCII alphabet.

``(map (^z* z*) '(1 2 3 4 5))`` => ``((1) (2) (3) (4) (5))``



