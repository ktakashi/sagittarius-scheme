@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.base.6"]{Base Library}

@define[Library]{@name{(rnrs base (6))}}
@desc{[R6RS] This library exports many of the procedure and syntax bindings that are
traditionally associated with Scheme.}

@subsubsection{Variable definitions}

@define[Syntax]{@name{define} @args{variable expression}}
@define[Syntax]{@name{define} @args{variable}}
@define[Syntax]{@name{define} @args{(variable formals) body ...}}
@define[Syntax]{@name{define} @args{(variable . formal) body ...}}
@desc{
[R6RS] The @code{define} form is a definition used to create variable bindings
and may appear anywhere other definitions may appear.

The first from of @code{define} binds @var{variable} to a new location before
assigning the value of @var{expression} to it.

@snipet{(define add3 (lambda (x) (+ x 3)))}
@snipet[=> 6]{(add3 3)}
@snipet{(define first car)}
@snipet[=> 1]{(first ’(1 2))}

The second form of @code{define} is equivalent to

@snipet{(define @var{variable} @var{unspecified})}

where @var{unspecified} is a side-effect-free expression returning an unspecified
value.

In the third form of @code{define}, @var{formals} must be either a sequence of
zero or more variables, or a sequence of one or more variables followed by a dot
@code{.} and another variable. This form is equivalent to

@snipet{(define @var{variable} (lambda (@var{formals}) @var{body ...}))}

In the fourth form of @code{define}, @var{formal} must be a single variable. This
form is equivalent to

@snipet{(define @var{variable} (lambda @var{formal} @var{body ...}))}
}

@subsubsection{Syntax definitions}

@define[Syntax]{@name{define-syntax} @args{keyword expression}}
@desc{
[R6RS] The @code{define-syntax} form is a definition used to create keyword
bindings and may appear anywhere other definitions may appear.

Binds @var{keyword} to the value of @var{expression}, which must evaluate,
at macro-expansion time, to a transformer.
}

@subsubsection{Quotation}

@define[Syntax]{@name{quote} @args{datum}}
@desc{[R6RS] @code{quote} evaluates to the datum value represented by @var{datum}.

@snipet[=> a]{(quote a)}
@snipet[=> #(a b c)]{(quote #(a b c))}
@snipet[=> (+ 1 2)]{(quote (+ 1 2))}
}

@subsubsection{Procedures}

@define[Syntax]{@name{lambda} @args{formals body ...}}
@desc{[R6RS]A @code{lambda} expression evaluates to a procedure. The environment
in effect when the lambda expression is evaluated is remembered as part of the
procedure. When the procedure is later called with some arguments, the environment
in which the @code{lambda} expression was evaluated is extended by binding the
variables in the parameter list to fresh locations, and the resulting argument
values are stored in those locations. Then, the expressions in the @var{body} of
the @code{lambda} expression are evaluated sequentially in the extended environment.
The results of the last expression in the body are returned as the results of the
procedure call.

@snipet[=> a procedure]{(lambda (x) (+ x x))}
@snipet[=> 8]{((lambda (x) (+ x x)) 4)}
@snipet[=> 11]{@codeblock{
((lambda (x)
   (define (p y) (+ y 1))
   (+ (p x) x)) 5)}}

@snipet{(define reverse-subtract (lambda (x y) (- y x)))}
@snipet[=> 3]{(reverse-subtract 7 10)}

@snipet{(define add4 (let ((x 4)) (lambda (y) (+ x y))))}
@snipet[=> 10]{(add4 6)}

@var{Formals} must have one of the following forms:
@itemlist[
@item{(@var{<variable1>} ...)
 @p{The procedure takes a fixed number of arguments; when the procedure is called,
    the arguments are stored in the bindings of the corresponding variables.}}
@item{@var{<variable>}
 @p{The procedure takes any number of arguments; when the procedure is called,
    the sequence of arguments is converted into a newly allocated list, and the
    list is stored in the binding of the @var{<variable>}.}}
@item{(@var{<variable1>} ... @var{<variablen>} . @var{<variablen+1>})
 @p{If a period @code{.} precedes the last variable, then the procedure takes
 @var{n} or more arguments, where @var{n} is the number of parameters before the
 period (there must be at least one). The value stored in the binding of the last
 variable is a newly allocated list of the arguments left over after all the other
 arguments have been matched up against the other parameters.

 @snipet[=> (3 4 5 6)]{((lambda x x) 3 4 5 6)}
 @snipet[=> (5 6)]{((lambda (x y . z) z)  3 4 5 6)}

 Any @var{variable} must not appear more than once in @var{formals}.}}
]

@subsubsection{Conditionals}

@define[Syntax]{@name{if} @args{test consequent alternate}}
@define[Syntax]{@name{if} @args{test consequent}}
@desc{
[R6RS]An @code{if} expression is evaluated as follows: first, @var{test} is
evaluated. If it yields a true value, then @var{consequent} is evaluated and
its values are returned. Otherwise @var{alternate} is evaluated and its values
are returned. If @var{test} yields #f and no @var{alternate} is specified, then
the result of the expression is unspecified.
}

@subsubsection{Assignment}

@define[Syntax]{@name{set!} @args{variable expression}}
@desc{
[R6RS] @var{Expression} is evaluated, and the resulting value is stored in the
location to which @var{variable} is bound. Variable must be bound either in some
region enclosing the @code{set!} expression or at the top level. The result of
the @code{set!} expression is unspecified.

Note: R6RS requires to throw syntax violation if @var{variable} refers immutable
binding. In Sagittarius, however, it won't throw any error.
}

@subsubsection{Derived conditionals}

@define[Syntax]{@name{cond} @args{clause ...}}
@desc{
[R6RS] Each @var{clause} must be the form

@snipet{(@var{test} @var{expression} ...)}
@snipet{(@var{test} => @var{expression})}
@snipet{(else @var{expression} ...)}

The last form can appear only in the last clause.

A @code{cond} expression is evaluated by evaluating the @var{test} expressions of
successive @var{clauses} in order until one of them evaluates to a true value.
When a @var{test} evaluates to a true value, then the remaining expressions in
its @var{clause} are evaluated in order, and the results of the last expression
in the @var{clause} are returned as the results of the entire @code{cond} expression.
If the selected @var{clause} contains only the @var{test} and no expressions,
then the value of the @var{test} is returned as the result. If the selected
@var{clause} uses the @code{=>} alternate form, then the expression is evaluated.
Its value must be a procedure. This procedure should accept one argument; it is
called on the value of the @var{test} and the values returned by this procedure
are returned by the @code{cond} expression. If all @var{tests} evaluate to #f,
and there is no @code{else} clause, then the conditional expression returns
unspecified values; if there is an @code{else} clause, then its expressions are
evaluated, and the values of the last one are returned.
}

@define[Syntax]{@name{case} @args{clause ...}}
@desc{
[R6RS] @var{Key} must be an expression. Each @var{clause} must have one of the
following forms:

@snipet{((@var{datum} ...) @var{expression} ...)}
@snipet{(else @var{expression} ...)}

The last form can appear only in the last clause.

A @code{case} expression is evaluated as follows. @var{Key} is evaluated and its
result is compared using @code{eqv?} against the data represented by the @var{datums}
of each @var{clause} in turn, proceeding in order from left to right through
the set of clauses. If the result of evaluating @var{key} is equivalent to a datum
of a @var{clause}, the corresponding expressions are evaluated from left to right
and the results of the last expression in the @var{clause} are returned as the
results of the @code{case} expression. Otherwise, the comparison process continues.
If the result of evaluating @var{key} is different from every datum in each set,
then if there is an @code{else} clause its expressions are evaluated and the
results of the last are the results of the @code{case} expression; otherwise the
@code{case} expression returns unspecified values.
}

@define[Syntax]{@name{and} @args{test ...}}
@desc{
[R6RS] If there are no @var{tests}, #t is returned. Otherwise, the @var{test}
expressions are evaluated from left to right until a @var{test} returns #f or
the last @var{test} is reached. In the former case, the and expression returns
#f without evaluating the remaining expressions. In the latter case, the last
expression is evaluated and its values are returned.

@snipet[=> #t]{(and (= 2 2) (> 2 1))}
@snipet[=> #f]{(and (= 2 2) (< 2 1))}
@snipet[=> (f g)]{(and 1 2 'c '(f g))}
@snipet[=> #t]{(and)}
}

@define[Syntax]{@name{or} @args{test ...}}
@desc{
[R6RS] If there are no @var{tests}, #f is returned. Otherwise, the @var{test}
expressions are evaluated from left to right until a @var{test} returns a true
value or the last @var{test} is reached. In the former case, the or expression
returns val without evaluating the remaining expressions. In the latter case,
the last expression is evaluated and its values are returned.

@snipet[=> #t]{(or (= 2 2) (> 2 1))}
@snipet[=> #t]{(or (= 2 2) (< 2 1))}
@snipet[=> #f]{(or #f #f #f)}
@snipet[=> (b c)]{(or '(b c) (/ 3 0))}
}

@subsubsection{Binding constructs}

@define[Syntax]{@name{let} @args{bindings body ...}}
@desc{
[R6RS] @var{Bindings} must have the form

@snipet{((@var{variable1} @var{init1}) ...)}

where each @var{init} is an expression. Any variable must not appear more than
once in the @var{variables}.

The @var{inits} are evaluated in the current environment, the variables are bound
to fresh locations holding the results, the @var{body} is evaluated in the
extended environment, and the values of the last expression of @var{body} are
returned. Each binding of a @var{variable} has @var{body} as its region.
}

@define[Syntax]{@name{let*} @args{bindings body ...}}
@desc{
[R6RS] @var{Bindings} must have the form

@snipet{((@var{variable1} @var{init1}) ...)}

The @code{let*} form is similar to @code{let}, but the @var{inits} are evaluated
and bindings created sequentially from left to right, with the region of each
binding including the bindings to its right as well as @var{body}. Thus the second
@var{init} is evaluated in an environment in which the first binding is visible
and initialized, and so on.
}

@define[Syntax]{@name{letrec} @args{bindings body ...}}
@desc{
[R6RS] @var{Bindings} must have the form

@snipet{((@var{variable1} @var{init1}) ...)}

where each @var{init} is an expression. Any variable must not appear more than
once in the @var{variables}.

The @var{variables} are bound to fresh locations, the @var{inits} are evaluated
in the resulting environment, each @var{variable} is assigned to the result of
the corresponding @var{init}, the @var{body} is evaluated in the resulting environment,
and the values of the last expression in @var{body} are returned. Each binding of
a @var{variable} has the entire @code{letrec} expression as its region, making it
possible to define mutually recursive procedures.

In the most common uses of @code{letrec}, all the @var{inits} are @code{lambda}
expressions and the restriction is satisfied automatically.
}

@define[Syntax]{@name{letrec*} @args{bindings body ...}}
@desc{
[R6RS] @var{Bindings} must have the form

@snipet{((@var{variable1} @var{init1}) ...)}

where each @var{init} is an expression. Any variable must not appear more than
once in the @var{variables}.

The @var{variables} are bound to fresh locations, each @var{variable} is assigned
in left-to-right order to the result of evaluating the corresponding @var{init},
the @var{body} is evaluated in the resulting environment, and the values of the
last expression in @var{body} are returned. Despite the left-to-right evaluation
and assignment order, each binding of a @var{variable} has the entire @code{letrec*}
expression as its region, making it possible to define mutually recursive procedures.
}

@define[Syntax]{@name{let-values} @args{mv-bindings body ...}}
@desc{
[R6RS] @var{Mv-bindings} must have the form

@snipet{((@var{formals} @var{init1}) ...)}

where each @var{init} is an expression. Any variable must not appear more than
once in the set of @var{formals}.

The @var{inits} are evaluated in the current environment, and the variables
occurring in the @var{formals} are bound to fresh locations containing the values
returned by the @var{inits}, where the @var{formals} are matched to the return
values in the same way that the @var{formals} in a @code{lambda} expression are
matched to the arguments in a procedure call. Then, the @var{body} is evaluated
in the extended environment, and the values of the last expression of @var{body}
are returned. Each binding of a variable has @var{body} as its region. If the
@var{formals} do not match, an exception with condition type @code{&assertion}
is raised.
}

@define[Syntax]{@name{let*-values} @args{mv-bindings body ...}}
@desc{
[R6RS] @var{Mv-bindings} must have the form

@snipet{((@var{formals} @var{init1}) ...)}

where each @var{init} is an expression. In each @var{formals}, any variable must
not appear more than once.

The @code{let*-values} form is similar to @code{let-values}, but the @var{inits}
are evaluated and bindings created sequentially from left to right, with the
region of the bindings of each @var{formals} including the bindings to its right
as well as @var{body}. Thus the second @var{init} is evaluated in an environment
in which the bindings of the first @var{formals} is visible and initialized, and
so on.
}

@subsubsection{Sequencing}

