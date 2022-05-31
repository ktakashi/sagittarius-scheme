[§2] Exceptions {#rnrs.exceptions.6}
-------------

###### [!Library] `(rnrs exceptions (6))` 

This section describes exception-handling and exception-raising constructs
provided by the `(rnrs exceptions (6))`library.


###### [!Function] `with-exception-handler`  _handler_ _thunk_

[R6RS] _Handler_ must be a procedure and should accept one argument.
_Thunk_ must be a procedure that accepts zero arguments. The
`with-exception-handler` procedure returns the results of invoking
_thunk_. When an exception is raised, _handler_ will be invoked with
the exception.


###### [!Macro] `guard`  _(variable_ _cond-clause_ _..._ _)_ _body_

[R6RS] Each _cond-clause_ is as in the specification of `cond`.
and `else` are the same as in the `(rnrs base (6))` library.

Evaluating a `guard` form evaluates _body_ with an exception handler
that binds the raised object to _variable_ and within the scope of that
binding evaluates the clauses as if they were the clauses of a `cond`expression. If every _cond-clause_'s test evaluates to #f and there is
no `else` clause, then raise is re-invoked on the raised object.


###### [!Function] `raise`  _obj_

[R6RS] Raises a non-continuable exception by invoking the current exception
handler on _obj_.


###### [!Function] `raise-continuable`  _obj_

[R6RS] Raises a continuable exception by invoking the current exception
handler on _obj_.


