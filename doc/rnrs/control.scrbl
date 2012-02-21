@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.control.6"]{Control structures}

@define[Library]{@name{(rnrs control (6))}}
@desc{The @code{(rnrs control (6))}library, which provides useful control structures.}

@define[Syntax]{@name{when} @args{test expression @dots{}}}
@define[Syntax]{@name{unless} @args{test expression @dots{}}}
@desc{[R6RS] @var{Test} must be an expression.

A @code{when} expression is evaluated by evaluating the @var{test} expression.
If @var{test} evaluates to a true value, the remaining @var{expressions} are
evaluated in order, and the results of the last @var{expression} are returned as
the results of the entire @code{when} expression. Otherwise, the @code{when}
expression returns unspecified values. An @code{unless} expression is evaluated
by evaluating the @var{test} expression. If @var{test} evaluates to #f, the
remaining @var{expressions} are evaluated in order, and the results of the last
@var{expression} are returned as the results of the entire @code{unless}
expression. Otherwise, the @code{unless} expression returns unspecified values.
}

@define[Syntax]{@name{do} @args{((variable init step) @dots{}) (test expression @dots{}) commend}}
@desc{[R6RS] The @var{inits}, @var{steps}, @var{tests}, and @var{commands} must
be expressions. The @var{variables} must be pairwise distinct variables.

The @code{do} expression is an iteration construct. It specifies a set of variables
to be bound, how they are to be initialized at the start, and how they are to be
updated on each iteration.

A @code{do} expression is evaluated as follows: The @var{init} expressions are
evaluated (in some unspecified order), the @var{variables} are bound to fresh
locations, the results of the @var{init} expressions are stored in the bindings
of the @var{variables}, and then the iteration phase begins.

Each iteration begins by evaluating @var{test} if the result is #f, then the
@var{commands} are evaluated in order for effect, the @var{step} expressions are
evaluated in some unspecified order, the @var{variables} are bound to fresh
locations holding the results, and the next iteration begins.

If @var{test} evaluates to a true value, the @var{expressions} are evaluated from
left to right and the values of the last @var{expression} are returned. If no
@var{expressions} are present, then the @code{do} expression returns unspecified
values.

The region of the binding of a @var{variable} consists of the entire @code{do}
expression except for the @var{inits}.

A @var{step} may be omitted, in which case the effect is the same as if
(@var{variable} @var{init} @var{variable}) had been written instead of
(@var{variable} @var{init}).

@codeblock[=> #(0 1 2 3 4)]{
(do ((vec (make-vector 5))
      (i 0 (+ i 1)))
     ((= i 5) vec)
  (vector-set! vec i i))}

@codeblock[=> 25]{
(let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x))
        (sum 0 (+ sum (car x))))
       ((null? x) sum)))
}
}

@define[Syntax]{@name{case-lambda} @args{case-lambda-clause @dots{}}}
@desc{[R6RS] Each @var{case-lambda-clause} must be of the form

@snipet{(@var{formals} @var{body})}

@var{Formals} must be as in a @code{lambda} form.

A @code{case-lambda} expression evaluates to a procedure. This procedure, when
applied, tries to match its arguments to the @code{case-lambda-clauses} in order.
The arguments match a clause if one of the following conditions is fulfilled:

@var{Formals} has the form @code{(@var{variable} @dots{})} and the number of
arguments is the same as the number of formal parameters in @var{formals}.

@var{Formals} has the form

@snipet{(@var{variable1} @dots{} @var{variablen} . @var{variablen+1})} 
and the number of arguments is at least @var{n}.

@var{Formals} has the form @var{variable}.

For the first clause matched by the arguments, the variables of the @var{formals}
are bound to fresh locations containing the argument values in the same arrangement
as with @code{lambda}.

The last expression of a body in a @code{case-lambda} expression is in tail context.

If the arguments match none of the clauses, an exception with condition type
@code{&assertion} is raised.
}
