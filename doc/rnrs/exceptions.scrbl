@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.exceptions.6"]{Exceptions}

@define[Library]{@name{(rnrs exceptions (6))}}
@desc{This section describes exception-handling and exception-raising constructs
provided by the @code{(rnrs exceptions (6))}library.
}

@define[Function]{@name{with-exception-handler} @args{handler thunk}}
@desc{[R6RS] @var{Handler} must be a procedure and should accept one argument.
@var{Thunk} must be a procedure that accepts zero arguments. The
@code{with-exception-handler} procedure returns the results of invoking
@var{thunk}. When an exception is raised, @var{handler} will be invoked with
the exception.
}

@define[Macro]{@name{guard} @args{(variable cond-clause @dots{}) body}}
@desc{[R6RS] Each @var{cond-clause} is as in the specification of @code{cond}.
and @code{else} are the same as in the @code{(rnrs base (6))} library.

Evaluating a @code{guard} form evaluates @var{body} with an exception handler
that binds the raised object to @var{variable} and within the scope of that
binding evaluates the clauses as if they were the clauses of a @code{cond}
expression. If every @var{cond-clause}'s test evaluates to #f and there is
no @code{else} clause, then raise is re-invoked on the raised object.
}

@define[Function]{@name{raise} @args{obj}}
@desc{[R6RS] Raises a non-continuable exception by invoking the current exception
handler on @var{obj}.
}

@define[Function]{@name{raise-continuable} @args{obj}}
@desc{[R6RS] Raises a continuable exception by invoking the current exception
handler on @var{obj}.
}