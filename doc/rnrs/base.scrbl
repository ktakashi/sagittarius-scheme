@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.base.6"]{Base Library}

@define[Library]{@name{(rnrs base (6))}}
@desc{[R6RS] This library exports many of the procedure and syntax bindings
that are traditionally associated with Scheme.}

@subsubsection[:tag "rnrs.base.6.variable.definitions"]{Variable definitions}

@define[Syntax]{@name{define} @args{variable expression}}
@define[Syntax]{@name{define} @args{variable}}
@define[Syntax]{@name{define} @args{(variable formals) body @dots{}}}
@define[Syntax]{@name{define} @args{(variable . formal) body @dots{}}}
@desc{[R6RS] The @code{define} form is a definition used to create variable
bindings and may appear anywhere other definitions may appear.

The first from of @code{define} binds @var{variable} to a new location before
assigning the value of @var{expression} to it.

@snipet{(define add3 (lambda (x) (+ x 3)))}
@snipet[=> 6]{(add3 3)}
@snipet{(define first car)}
@snipet[=> 1]{(first '(1 2))}

The second form of @code{define} is equivalent to

@snipet{(define @var{variable} @var{unspecified})}

where @var{unspecified} is a side-effect-free expression returning an
unspecified value.

In the third form of @code{define}, @var{formals} must be either a sequence of
zero or more variables, or a sequence of one or more variables followed by a dot
@code{.} and another variable. This form is equivalent to

@snipet{(define @var{variable} (lambda (@var{formals}) @var{body @dots{}}))}

In the fourth form of @code{define}, @var{formal} must be a single variable.
This form is equivalent to

@snipet{(define @var{variable} (lambda @var{formal} @var{body @dots{}}))}
}

@subsubsection{Syntax definitions}

@define[Syntax]{@name{define-syntax} @args{keyword expression}}
@desc{[R6RS] The @code{define-syntax} form is a definition used to create keyword
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

@define[Syntax]{@name{lambda} @args{formals body @dots{}}}
@desc{[R6RS+]A @code{lambda} expression evaluates to a procedure. The
environment in effect when the lambda expression is evaluated is remembered as
part of the procedure. When the procedure is later called with some arguments,
the environment in which the @code{lambda} expression was evaluated is extended 
by binding the variables in the parameter list to fresh locations, and the
resulting argument values are stored in those locations. Then, the expressions
in the @var{body} of the @code{lambda} expression are evaluated sequentially in
the extended environment. The results of the last expression in the body are
returned as the results of the procedure call.

@snipet[=> "a procedure"]{(lambda (x) (+ x x))}
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
@item{(@var{<variable1>} @dots{})
 @p{The procedure takes a fixed number of arguments; when the procedure is
    called, the arguments are stored in the bindings of the corresponding
    variables.}}
@item{@var{<variable>}
 @p{The procedure takes any number of arguments; when the procedure is called,
    the sequence of arguments is converted into a newly allocated list, and the
    list is stored in the binding of the @var{<variable>}.}}
@item{(@var{<variable1>} @dots{} @var{<variablen>} . @var{<variablen+1>})
 @p{If a period @code{.} precedes the last variable, then the procedure takes
    @var{n} or more arguments, where @var{n} is the number of parameters before
    the period (there must be at least one). The value stored in the binding of
    the last variable is a newly allocated list of the arguments left over after
    all the other arguments have been matched up against the other
    parameters.

    @snipet[=> (3 4 5 6)]{((lambda x x) 3 4 5 6)}
    @snipet[=> (5 6)]{((lambda (x y . z) z)  3 4 5 6)}

    Any @var{variable} must not appear more than once in @var{formals}.}}
@item{(@var{<variable> @dots{} @var{<extended-spec>} @dots{}})
  
  Extended argument specification. Zero or more variables that specifies
  required formal argument, followed by an @var{<extended-spec>}, a list
  begginning with a keyword @code{:optional}, @code{:key} or @code{:rest}.

  The @var{<extended-spec>} part consists of the optional argument spec, the
  keyword argument spec and the rest argument spec. They can appear in any
  combinations.

  @dl-list[]{
    @dl-item[@code{:optional @var{optspec} @dots{}}]{
      Specifies optional arguments. Each @var{optspec} can be either one of the
      following forms:

      @snipet{@var{variable}}
      @snipet{(@var{variable} @var{init-expr})}

      The @var{variable} names the formal argument, which is bound to the value
      of the actual argument if given, or the value of the expression
      @var{init-expr} otherwise. If @var{optspec} is just a variable, and the
      actual argument is not given, then it will be unspecified value.

      The expression @var{init-expr} is only evaluated if the actual argument is
      not given. The scope in which @var{init-expr} is evaluated includes the
      preceding formal arguments.

      @codeblock[=> (1 2 3)]{
((lambda (a b :optional (c (+ a b))) (list a b c)) 1 2)
      }
      @codeblock[=> (1 2 -1)]{
((lambda (a b :optional (c (+ a b))) (list a b c)) 1 2 -1)
      }
      @codeblock[=> (1 2 "#<unspecified>")]{
((lambda (a b :optional c) (list a b c)) 1 2)
      }
      @codeblock[=> (1 2)]{
((lambda (:optional (a 0) (b (+ a 1))) (list a b)))
      }

      The procedure raises an @code{&serious} if more actual arguments than the
      number of required and optional arguments are given, unless it also has
      @code{:key} or @code{:rest} arguments spec.

      @codeblock[=> &serious]{
((lambda (:optional a b) (list a b)) 1 2 3)
      }
      @codeblock[=> (1 2 (3))]{
((lambda (:optional a b :rest r) (list a b r)) 1 2 3)
      }
    }
    @dl-item[@code{:key @var{keyspec} @dots{} [:allow-other-keys [@var{variable}]]}]{
      Specifies keyword arguments. Each @var{keyspec} can be one of the
      following forms.

      @snipet{@var{variable}}
      @snipet{(@var{variable} @var{init-expr})}
      @snipet{((@var{keyword} @var{variable}) @var{init-expr})}
      @snipet{(@var{variable} @var{keyword} @var{init-expr})}

      The @var{variable} names the formal argument, which is bound to the actual
      argument given with the keyword of the same name as @var{variable}. When
      the actual is not given, @var{init-expr} is evaluated and the result is
      bound to @var{variable} in the second, third and fourth form, or
      unspecified value is bound in the first form.

      @codeblock{
(define f (lambda (a :key (b (+ a 1)) (c (+ b 1)))
            (list a b c)))
      }
      @snipet[=> (10 11 12)]{(f 10)}
      @snipet[=> (10 4 5)]{(f 10 :b 4)}
      @snipet[=> (10 11 8)]{(f 10 :c 8)}
      @snipet[=> (10 3 1)]{(f 10 :c 1 :b 3)}

      With the third and fourth form you can name the formal argument
      differently from the keyword to specify the argument.

      @snipet[=> 2]{((lambda (:key ((:aa a) -1)) a) ::aa 2)}
      @snipet[=> 2]{((lambda (:key (a :aa -1)) a) ::aa 2)}

      By default, the procedure with keyword argument spec raises
      @code{&serious} if a keyword argument with an unrecognized keyword is
      given. Giving @code{:allow-other-keys} in the formals suppresses this
      behaviour. If you give @var{variable} after @code{:allow-other-keys}, the
      list of unrecognized keywords and their arguments are bound to it.

      @snipet[=> &serious]{((lambda (:key a) a) :a 1 :b 2)}
      @snipet[=> 1]{((lambda (:key a :allow-other-keys) a) :a 1 :b 2)}
      @codeblock[=> (1 (:b 2))]{
((lambda (:key a :allow-other-keys z) (list a z)) :a 1 :b 2)
      }

      When used with @code{:optional} argument spec, the keyword arguments are
      searched after all the optional arguments are bound.

      @codeblock[=> (1 2 3)]{
((lambda (:optional a b :key c) (list a b c)) 1 2 :c 3)
      }
      @codeblock[=> (:c 3 "#<unspecified>")]{
((lambda (:optional a b :key c) (list a b c)) :c 3)
      }
      @codeblock[=> &serious]{
((lambda (:optional a b :key c) (list a b c)) 1 :c 3)
      }
    }
    @dl-item[@code{:rest @var{variable}}]{
      Specifies the rest argument. If specified without @code{:optional}
      argument spec, a list of remaining arguments after required arguments are
      taken is bound to @var{variable}. If specified with @code{:optional}
      argument spec, the actual arguments are first bound to required and all
      optional arguments, and the remaining arguments are bound to
      @var{variable}.

      @codeblock[=> (1 2 (3 4 5))]{
((lambda (a b :rest z) (list a b z)) 1 2 3 4 5)
      }
      @codeblock[=> (1 2 3 4 (5))]{
((lambda (a b :optional c d :rest z) (list a b z)) 1 2 3 4 5)
      }
      @codeblock[=> (1 2 3 "#<unspecified>" ())]{
      ((lambda (a b :optional c d :rest z) (list a b z)) 1 2 3)
      }

      When the rest argument spec is used with the keyword argument spec, both
      accesses the same list of actual argument -- the remaining arguments after
      required and optional arguments are taken

      @codeblock[=> (1 (:k 3) 3)]{
((lambda (:optional a :rest r :key k) (list a r k)) 1 :k 3)
      }
    }
  }
  }
     
]
}

@subsubsection{Conditionals}

@define[Syntax]{@name{if} @args{test consequent alternate}}
@define[Syntax]{@name{if} @args{test consequent}}
@desc{[R6RS]An @code{if} expression is evaluated as follows: first, @var{test}
is evaluated. If it yields a true value, then @var{consequent} is evaluated and
its values are returned. Otherwise @var{alternate} is evaluated and its values
are returned. If @var{test} yields #f and no @var{alternate} is specified, then
the result of the expression is unspecified.
}

@subsubsection{Assignment}

@define[Syntax]{@name{set!} @args{variable expression}}
@desc{[R6RS] @var{Expression} is evaluated, and the resulting value is stored in
the location to which @var{variable} is bound. Variable must be bound either in
some region enclosing the @code{set!} expression or at the top level. The result
of the @code{set!} expression is unspecified.

Note: R6RS requires to throw syntax violation if @var{variable} refers immutable
binding. In Sagittarius, however, it won't throw any error.
}

@subsubsection{Derived conditionals}

@define[Syntax]{@name{cond} @args{clause @dots{}}}
@desc{[R6RS] Each @var{clause} must be the form

@snipet{(@var{test} @var{expression} @dots{})}
@snipet{(@var{test} => @var{expression})}
@snipet{(else @var{expression} @dots{})}

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

@define[Syntax]{@name{case} @args{clause @dots{}}}
@desc{[R6RS] @var{Key} must be an expression. Each @var{clause} must have one of
the following forms:

@snipet{((@var{datum} @dots{}) @var{expression} @dots{})}
@snipet{(else @var{expression} @dots{})}

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

@define[Syntax]{@name{and} @args{test @dots{}}}
@desc{[R6RS] If there are no @var{tests}, #t is returned. Otherwise, the
@var{test} expressions are evaluated from left to right until a @var{test}
returns #f or the last @var{test} is reached. In the former case, the and
expression returns #f without evaluating the remaining expressions. In the
latter case, the last expression is evaluated and its values are returned.

@snipet[=> #t]{(and (= 2 2) (> 2 1))}
@snipet[=> #f]{(and (= 2 2) (< 2 1))}
@snipet[=> (f g)]{(and 1 2 'c '(f g))}
@snipet[=> #t]{(and)}
}

@define[Syntax]{@name{or} @args{test @dots{}}}
@desc{[R6RS] If there are no @var{tests}, #f is returned. Otherwise, the @var{test}
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

@define[Syntax]{@name{let} @args{bindings body @dots{}}}
@desc{[R6RS] @var{Bindings} must have the form

@snipet{((@var{variable1} @var{init1}) @dots{})}

where each @var{init} is an expression. Any variable must not appear more than
once in the @var{variables}.

The @var{inits} are evaluated in the current environment, the variables are bound
to fresh locations holding the results, the @var{body} is evaluated in the
extended environment, and the values of the last expression of @var{body} are
returned. Each binding of a @var{variable} has @var{body} as its region.
}

@define[Syntax]{@name{let*} @args{bindings body @dots{}}}
@desc{[R6RS] @var{Bindings} must have the form

@snipet{((@var{variable1} @var{init1}) @dots{})}

The @code{let*} form is similar to @code{let}, but the @var{inits} are evaluated
and bindings created sequentially from left to right, with the region of each
binding including the bindings to its right as well as @var{body}. Thus the second
@var{init} is evaluated in an environment in which the first binding is visible
and initialized, and so on.
}

@define[Syntax]{@name{letrec} @args{bindings body @dots{}}}
@desc{[R6RS] @var{Bindings} must have the form

@snipet{((@var{variable1} @var{init1}) @dots{})}

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

@define[Syntax]{@name{letrec*} @args{bindings body @dots{}}}
@desc{[R6RS] @var{Bindings} must have the form

@snipet{((@var{variable1} @var{init1}) @dots{})}

where each @var{init} is an expression. Any variable must not appear more than
once in the @var{variables}.

The @var{variables} are bound to fresh locations, each @var{variable} is assigned
in left-to-right order to the result of evaluating the corresponding @var{init},
the @var{body} is evaluated in the resulting environment, and the values of the
last expression in @var{body} are returned. Despite the left-to-right evaluation
and assignment order, each binding of a @var{variable} has the entire @code{letrec*}
expression as its region, making it possible to define mutually recursive procedures.
}

@define[Syntax]{@name{let-values} @args{mv-bindings body @dots{}}}
@desc{[R6RS] @var{Mv-bindings} must have the form

@snipet{((@var{formals} @var{init1}) @dots{})}

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

@define[Syntax]{@name{let*-values} @args{mv-bindings body @dots{}}}
@desc{[R6RS] @var{Mv-bindings} must have the form

@snipet{((@var{formals} @var{init1}) @dots{})}

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

@define[Syntax]{@name{begin} @args{form @dots{}}}
@define[Syntax]{@name{begin} @args{expression @dots{}}}
@desc{[R6RS]The begin keyword has two different roles, depending on its context:
@itemlist[
 @item{It may appear as a form in a body , library body, or top-level body, or
 directly nested in a begin form that appears in a body. In this case, the begin
 form must have the shape specified in the first header line. This use of begin
 acts as a splicing form - the forms inside the body are spliced into the
 surrounding body, as if the begin wrapper were not actually present.

 A begin form in a body or library body must be non-empty if it appears after
 the first expression within the body.}
 @item{It may appear as an ordinary expression and must have the shape specified
 in the second header line. In this case, the expressions are evaluated
 sequentially from left to right, and the values of the last expression are
 returned. This expression type is used to sequence side effects such as
 assignments or input and output.}
]}

@subsubsection{Equivalence predicates}

A @code{predicate} is a procedure that always returns a boolean value (#t or #f).
An @code{equivalence predicate} is the computational analogue of a mathematical
equivalence relation (it is symmetric, reflexive, and transitive). Of the
equivalence predicates described in this section, @code{eq?} is the finest or
most discriminating, and @code{equal?} is the coarsest. The @code{eqv?} predicate
is slightly less discriminating than @code{eq?}.

@define[Function]{@name{eq?} @args{obj1 obj2}}
@define[Function]{@name{eqv?} @args{obj1 obj2}}
@define[Function]{@name{equal?} @args{obj1 obj2}}
@desc{[R6RS] @code{eq?} only sees if the given two objects are the same object or
not, @code{eqv?} compares numbers. @code{equal?} compares the values equivalence.

On Sagittarius Scheme interned symbol, keyword(only compatible mode), character,
literal string, boolean, fixnum, and '() are used as the same objects. If these
objects indicates the same value then @code{eq?} returns #t.

The following examples are not specified R6RS. But it is always good to know how
it works.

@snipet[=> #t]{(let ((p (lambda (x) x))) (eqv? p p))}
@snipet[=> #t]{(eqv? "" "")}
@snipet[=> #t]{(eqv? "abc" "abc") ;; literal string are the same object}
@snipet[=> #f]{(eqv? "abc" (list->string '(#\a #\b #\c)))}
@snipet[=> #f]{(eqv? '#() '#())}
@snipet[=> #f]{(eqv? (lambda (x) x) (lambda (x) x))}
@snipet[=> #f]{(eqv? (lambda (x) x) (lambda (y) y))}
@snipet[=> #f]{(eqv? +nan.0 +nan.0)}
}

@subsubsection{Procedure predicate}

@define[Function]{@name{procedure?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a procedure, otherwise returns #f.}

@subsubsection{Numerical type predicates}

@define[Function]{@name{number?} @args{obj}}
@define[Function]{@name{complex?} @args{obj}}
@define[Function]{@name{real?} @args{obj}}
@define[Function]{@name{rational?} @args{obj}}
@define[Function]{@name{integer?} @args{obj}}
@desc{[R6RS] These numerical type predicates can be applied to any kind of
argument. They return #t if the object is a number object of the named type, and
#f otherwise. In general, if a type predicate is true of a number object then
all higher type predicates are also true of that number object. Consequently,
if a type predicate is false of a number object, then all lower type predicates
are also false of that number object.

If @var{z} is a complex number object, then @code{(real? @var{z})} is true if and
only if @code{(zero? (imag-part @var{z}))} and @code{(exact? (imag-part @var{z}))}
are both true.

If @var{x} is a real number object, then @code{(rational? @var{x})} is true if
and only if there exist exact integer objects @var{k1} and @var{k2} such that
@code{(= @var{x} (/ @var{k1} @var{k2}))} and @code{(= (numerator @var{x}) @var{k1})}
and @code{(= (denominator @var{x}) @var{k2})} are all true. Thus infinities and
NaNs are not rational number objects.

If @var{q} is a rational number objects, then @code{(integer? @var{q})} is true
if and only if @code{(= (denominator @var{q}) 1)} is true. If q is not a rational
number object, then @code{(integer? @var{q})} is #f.
}

@define[Function]{@name{real-valued?} @args{obj}}
@define[Function]{@name{rational-valued?} @args{obj}}
@define[Function]{@name{integer-valued?} @args{obj}}
@desc{[R6RS] These numerical type predicates can be applied to any kind of argument.
The @code{real-valued?} procedure returns #t if the object is a number object and
is equal in the sense of @code{=} to some real number object, or if the object is
a NaN, or a complex number object whose real part is a NaN and whose imaginary part
is zero in the sense of zero?. The @code{rational-valued?} and @code{integer-valued?}
procedures return #t if the object is a number object and is equal in the sense
of @code{=} to some object of the named type, and otherwise they return #f.
}

@define[Function]{@name{exact?} @args{obj}}
@define[Function]{@name{inexact?} @args{obj}}
@desc{[R6RS] These numerical predicates provide tests for the exactness of a
quantity. For any number object, precisely one of these predicates is true.
}

@subsubsection{ Generic conversion}

@define[Function]{@name{exact} @args{z}}
@define[Function]{@name{inexact} @args{z}}
@desc{[R6RS] The @code{inexact} procedure returns an inexact representation of
@var{z}. If inexact number objects of the appropriate type have bounded precision,
then the value returned is an inexact number object that is nearest to the argument.

The @code{exact} procedure returns an exact representation of @var{z}. The value
returned is the exact number object that is numerically closest to the argument;
in most cases, the result of this procedure should be numerically equal to its argument.
}

@subsubsection{Arithmetic operations}

@define[Function]{@name{=} @args{z1 z2 z3 @dots{}}}
@define[Function]{@name{>} @args{x1 x2 x3 @dots{}}}
@define[Function]{@name{<} @args{x1 x2 x3 @dots{}}}
@define[Function]{@name{>=} @args{x1 x2 x3 @dots{}}}
@define[Function]{@name{<=} @args{x1 x2 x3 @dots{}}}
@desc{[R6RS] These procedures return #t if their arguments are (respectively):
equal, monotonically increasing, monotonically decreasing, monotonically
nondecreasing, or monotonically nonincreasing, and #f otherwise.
}

@define[Function]{@name{zero?} @args{z}}
@define[Function]{@name{positive?} @args{z}}
@define[Function]{@name{negative?} @args{z}}
@define[Function]{@name{odd?} @args{z}}
@define[Function]{@name{even?} @args{z}}
@define[Function]{@name{finite?} @args{z}}
@define[Function]{@name{infinite?} @args{z}}
@define[Function]{@name{nan?} @args{z}}
@desc{[R6RS] These numerical predicates test a number object for a particular
property, returning #t or #f.

The @code{zero?} procedure tests if the number object is @code{=} to zero.

The @code{positive?} tests whether it is greater than zero.

The  @code{negative?} tests whether it is less than zero.

The @code{odd?} tests whether it is odd.

The @code{even?} tests whether it is even

The @code{finite?} tests whether it is not an infinity and not a NaN.

The @code{infinite?} tests whether it is an infinity.

The @code{nan?} tests whether it is a NaN.
}

@define[Function]{@name{max} @args{x1 x2 @dots{}}}
@define[Function]{@name{min} @args{x1 x2 @dots{}}}
@desc{[R6RS] These procedures return the maximum or minimum of their arguments.}

@define[Function]{@name{+} @args{z @dots{}}}
@define[Function]{@name{*} @args{z @dots{}}}
@desc{[R6RS] These procedures return the sum or product of their arguments.}

@define[Function]{@name{-} @args{z @dots{}}}
@define[Function]{@name{-} @args{z1 z2 @dots{}}}
@desc{[R6RS] With two or more arguments, this procedures returns the difference
of its arguments, associating to the left. With one argument, however, it returns
the additive inverse of its argument.

If this procedure is applied to mixed non-rational real and non-real complex
arguments, it returns an unspecified number object.
}

@define[Function]{@name{/} @args{z @dots{}}}
@define[Function]{@name{/} @args{z1 z2 @dots{}}}
@desc{[R6RS+] If all of the arguments are exact, then the divisors must all be
nonzero. With two or more arguments, this procedure returns the quotient of its
arguments, associating to the left. With one argument, however, it returns the
multiplicative inverse of its argument.

If this procedure is applied to mixed non-rational real and non-real complex
arguments, it returns an unspecified number object.

In R6RS, it requires to raise @code{&assertion} when the divisor was 0, on
Sagittarius, however, it returns NaN or infinite number when it is running with
compatible mode. In R6RS mode it raises @code{&assertion}.
}

@define[Function]{@name{abs} @args{x}}
@desc{[R6RS] Returns the absolute value of its argument.}

@define[Function]{@name{div-and-mod} @args{x1 x2}}
@define[Function]{@name{div} @args{x1 x2}}
@define[Function]{@name{mod} @args{x1 x2}}
@define[Function]{@name{div0-and-mod0} @args{x1 x2}}
@define[Function]{@name{div0} @args{x1 x2}}
@define[Function]{@name{mod0} @args{x1 x2}}
@desc{[R6RS] These procedures implement number-theoretic integer division and
return the results of the corresponding mathematical operations. In each case,
@var{x1} must be neither infinite nor a NaN, and @var{x2} must be nonzero;
otherwise, an exception with condition type @code{&assertion} is raised.
}

@define[Function]{@name{gcd} @args{n1 @dots{}}}
@define[Function]{@name{lcm} @args{n1 @dots{}}}
@desc{[R6RS] These procedures return the greatest common divisor or least common
multiple of their arguments. The result is always non-negative.
}

@define[Function]{@name{numerator} @args{q}}
@define[Function]{@name{denominator} @args{q}}
@desc{[R6RS] These procedures return the numerator or denominator of their
argument; the result is computed as if the argument was represented as a fraction
in lowest terms. The denominator is always positive. The denominator of 0 is
defined to be 1.
}

@define[Function]{@name{floor} @args{x}}
@define[Function]{@name{ceiling} @args{x}}
@define[Function]{@name{truncate} @args{x}}
@define[Function]{@name{round} @args{x}}
@desc{[R6RS] These procedures return inexact integer objects for inexact arguments
that are not infinities or NaNs, and exact integer objects for exact rational
arguments. For such arguments, @code{floor} returns the largest integer object
not larger than @var{x}. The @code{ceiling} procedure returns the smallest
integer object not smaller than @var{x}. The @code{truncate} procedure returns
the integer object closest to @var{x} whose absolute value is not larger than
the absolute value of @var{x}. The @code{round} procedure returns the closest
integer object to @var{x}, rounding to even when @var{x} represents a number
halfway between two integers.

Although infinities and NaNs are not integer objects, these procedures return
an infinity when given an infinity as an argument, and a NaN when given a NaN.
}

@define[Function]{@name{rationalize} @args{x1 x2}}
@desc{[R6RS] The @code{rationalize} procedure returns the a number object
representing the simplest rational number differing from @var{x1} by no more than
@var{x2}. A rational number @var{r1} is simpler than another rational number
@var{r2} if @var{r1} = @var{p1}/@var{q1} and @var{r2} = @var{p2}/@var{q2} (in
lowest terms) and |@var{p1}| ≤ |@var{p2}| and |@var{q1}| ≤ |@var{q2}|. Thus 3/5
is simpler than 4/7. Although not all rationals are comparable in this ordering
(consider 2/7 and 3/5) any interval contains a rational number that is simpler
than every other rational number in that interval (the simpler 2/5 lies between
2/7 and 3/5). Note that 0 = 0/1 is the simplest rational of all.
}

@define[Function]{@name{exp} @args{z}}
@define[Function]{@name{log} @args{z}}
@define[Function]{@name{log} @args{z1 z2}}
@define[Function]{@name{sin} @args{z}}
@define[Function]{@name{cos} @args{z}}
@define[Function]{@name{tan} @args{z}}
@define[Function]{@name{asin} @args{z}}
@define[Function]{@name{acos} @args{z}}
@define[Function]{@name{atan} @args{z}}
@define[Function]{@name{atan} @args{z1 z2}}
@desc{[R6RS] These procedures compute the usual transcendental functions. The
@code{exp} procedure computes the base-e exponential of @var{z}.

The @code{log} procedure with a single argument computes the natural logarithm
of @var{z} (not the base-ten logarithm); @code{(log @var{z1} @var{z2})} computes
the base-@var{z2} logarithm of @var{z1}.

The @code{asin}, @code{acos}, and @code{atan} procedures compute arcsine,
arccosine, and arctangent, respectively.

The two-argument variant of @code{atan} computes
@code{(angle (make-rectangular @var{x2} @var{x1}))}.
}

@define[Function]{@name{sqrt} @args{z}}
@desc{[R6RS] Returns the principal square root of @var{z}. For rational @var{z},
the result has either positive real part, or zero real part and non-negative imaginary part.

The @var{sqrt} procedure may return an inexact result even when given an exact argument.
}

@define[Function]{@name{expt} @args{z1 z2}}
@desc{[R6RS] Returns @var{z1} raised to the power @var{z2}. For nonzero @var{z1},
this is @math{e^{z2 log z1}}. 0.0z is 1.0 if z = 0.0, and 0.0 if @code{(real-part @var{z})} is
positive. For other cases in which the first argument is zero, an unspecified
number object(@code{+nan.0+nan.0i}) is returned.

For an exact real number object @var{z1} and an exact integer object @var{z2},
@code{(expt @var{z1} @var{z2})} must return an exact result. For all other values
of @var{z1} and @var{z2}, @code{(expt @var{z1} @var{z2})} may return an inexact
result, even when both @var{z1} and @var{z2} are exact.
}

@define[Function]{@name{make-rectangular} @args{x1 x2}}
@define[Function]{@name{make-polar} @args{x3 x4}}
@define[Function]{@name{real-part} @args{z}}
@define[Function]{@name{imag-part} @args{z}}
@define[Function]{@name{magnitude} @args{z}}
@define[Function]{@name{angle} @args{z}}
@desc{[R6RS] Suppose @var{a1}, @var{a2}, @var{a3}, and @var{a4} are real numbers,
and @var{c} is a complex number such that the following holds:

@math{c = a1 + a2 ^ i = a3 e ^ ia4}

Then, if @var{x1}, @var{x2}, @var{x3}, and @var{x4} are number objects representing
@var{a1}, @var{a2}, @var{a3}, and @var{a4}, respectively,
@code{(make-rectangular @var{x1} @var{x2})} returns @var{c}, and
@code{(make-polar @var{x3} @var{x4})} returns @var{c}.
}

@subsubsection{ Numerical input and output}

@define[Function]{@name{number->string} @args{z :optional radix precision}}
@desc{[R6RS+] @var{Radix} must be an exact integer object, between 2, and 32. If
omitted, @var{radix} defaults to 10. In Sagittarius @var{precision} will be
ignored. The @code{number->string} procedure takes a number object and a radix
and returns as a string an external representation of the given number object in
the given @var{radix} such that

@codeblock{
(let ((number @var{z}) (radix @var{radix}))
  (eqv? (string->number
          (number->string number radix)
          radix)
        number))
}

is true.
}

@define[Function]{@name{number->string} @args{string :optional radix}}
@desc{[R6RS+] Returns a number object with maximally precise representation
expressed by the given @var{string}. @var{Radix} must be an exact integer object,
between 2, and 32. If supplied, @var{radix} is a default radix that may be
overridden by an explicit radix prefix in @var{string} (e.g., "#o177"). If
@var{radix} is not supplied, then the default radix is 10. If @var{string} is
not a syntactically valid notation for a number object or a notation for a
rational number object with a zero denominator, then @code{string->number}
returns #f.

These @code{number->string} and @code{string->number}'s resolutions of radix are
taken from Gauche.
}

@subsubsection{Booleans}

@define[Function]{@name{not} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is #f, and returns #f otherwise.}

@define[Function]{@name{boolean?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is either #t or #f and returns #f otherwise.}

@define[Function]{@name{boolean=?} @args{bool1 bool2 bool3@dots{}}}
@desc{[R6RS] Returns #t if the booleans are the same.}

@subsubsection{Pairs and lists}

A @var{pair} is a compound structure with two fields called the car and cdr fields
(for historical reasons). Pairs are created by the procedure @code{cons}. The car
and cdr fields are accessed by the procedures @code{car} and @code{cdr}.

Pairs are used primarily to represent lists. A list can be defined recursively as
either the empty list or a pair whose cdr is a list. More precisely, the set of
lists is defined as the smallest set @var{X} such that

@itemlist[
@item{The empty list is in}
@item{If list is in @var{X}, then any pair whose cdr field contains @var{list}
is also in @var{X}.}
]

The objects in the car fields of successive pairs of a list are the elements of
the list. For example, a two-element list is a pair whose car is the first element
and whose cdr is a pair whose car is the second element and whose cdr is the empty
list. The length of a list is the number of elements, which is the same as the
number of pairs.

The empty listis a special object of its own type. It is not a pair. It has no
elements and its length is zero.

A chain of pairs not ending in the empty list is called an @var{improper list}.
Note that an improper list is not a list. The list and dotted notations can be
combined to represent improper lists:

@snipet{(a b c . d)}

is equivalent to

@snipet{(a . (b . (c . d)))}

Whether a given pair is a list depends upon what is stored in the cdr field.

@define[Function]{@name{pair?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a pair, and otherwise returns #f.}

@define[Function]{@name{cons} @args{obj1 obj2}}
@desc{[R6RS] Returns a newly allocated pair whose car is @var{obj1} and whose
cdr is @var{obj2}. The pair is guaranteed to be different (in the sense of 
@code{eqv?}) from every existing object.
}

@define[Function]{@name{car} @args{pair}}
@define[Function]{@name{cdr} @args{pair}}
@desc{[R6RS] Returns the contents of the car/cdr field of @var{pair}.}

@define[Function]{@name{caar} @args{pair}}
@define[Function]{@name{cadr} @args{pair}}
  :
@define[Function]{@name{cadddr} @args{pair}}
@define[Function]{@name{cddddr} @args{pair}}
@desc{[R6RS] These procedures are compositions of @code{car} and @code{cdr},
where for example @code{caddr} could be defined by

@snipet{(define caddr (lambda (x) (car (cdr (cdr x)))))}.

Arbitrary compositions, up to four deep, are provided. There are twenty-eight
of these procedures in all.
}

@define[Function]{@name{null?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is the empty list, #f otherwise.}

@define[Function]{@name{list?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a list, #f otherwise. By definition, all
lists are chains of pairs that have finite length and are terminated by the
empty list.
}

@define[Function]{@name{list} @args{obj @dots{}}}
@desc{[R6RS] Returns a newly allocated list of its arguments.}

@define[Function]{@name{length} @args{list}}
@desc{[R6RS+] Returns the length of @var{list}. If the @var{list} is improper
list, it returns -1 If the @var{list} is infinite list , it returns -2.
}

@define[Function]{@name{append} @args{list @dots{} obj}}
@desc{[R6RS] Returns a possibly improper list consisting of the elements of the
first @var{list} followed by the elements of the other @var{lists}, with @var{obj}
as the cdr of the final pair. An improper list results if @var{obj} is not a list.
}

@define[Function]{@name{reverse} @args{list}}
@desc{[R6RS] Returns a newly allocated list consisting of the elements of
@var{list} in reverse order.
}

@define[Function]{@name{list-tail} @args{list k :optaionl fallback}}
@desc{[R6RS+] The @code{list-tail} procedure returns the subchain of pairs of
@var{list} obtained by omitting the first @var{k} elements. If @var{fallback}
is given and @var{k} is out of range, it returns @var{fallback} otherwise
@code{&assertion} is raised.
}

@define[Function]{@name{list-ref} @args{list k :optaionl fallback}}
@desc{[R6RS+] The @code{list-ref} procedure returns the @var{k}th element of
@var{list}. If @var{fallback} is given and @var{k} is out of range, it returns
@var{fallback} otherwise @code{&assertion} is raised.
}

@define[Function]{@name{map} @args{proc list1 list2 @dots{}}}
@desc{[R6RS+ SRFI-1] The @code{map} procedure applies @var{proc} element-wise to
the elements of the @var{lists} and returns a list of the results, in order. The
order in which @var{proc} is applied to the elements of the lists is unspecified.
If multiple returns occur from @code{map}, the values returned by earlier returns
are not mutated. If the given @var{lists} are not the same length, when the
shortest list is processed the @code{map} will stop.
}

@define[Function]{@name{for-each} @args{proc list1 list2 @dots{}}}
@desc{[R6RS+ SRFI-1] The @code{for-each} procedure applies @var{proc} element-wise
to the elements of the @var{lists} for its side effects, in order from the first
elements to the last. The return values of @code{for-each} are unspecified. If
the given @var{lists} are not the same length, when the shortest list is
processed the @code{for-each} will stop.
}

@subsubsection{Symbols}

@define[Function]{@name{symbol?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a symbol, otherwise returns #f.}

@define[Function]{@name{symbol->string} @args{symbol}}
@desc{[R6RS] Returns the name of @var{symbol} as an immutable string.}

@define[Function]{@name{symbol=?} @args{symbol1 symbol2 symbol3 @dots{}}}
@desc{[R6RS] Returns #t if the @var{symbols} are the same, i.e., if their names
are spelled the same.}

@define[Function]{@name{string->symbol} @args{string}}
@desc{[R6RS] Returns the symbol whose name is @var{string}.}

@subsubsection{Characters}

@var{Characters} are objects that represent Unicode scalar values.

@define[Function]{@name{char?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a character, otherwise returns #f.}

@define[Function]{@name{char->integer} @args{char}}
@define[Function]{@name{integer->char} @args{sv}}
@desc{[R6RS] @var{Sv} must be a Unicode scalar value, i.e., a non-negative exact
integer object in [0, #xD7FF] ∪ [#xE000, #x10FFFF].

Given a character, @code{char->integer} returns its Unicode scalar value as an
exact integer object. For a Unicode scalar value @var{sv}, @code{integer->char}
returns its associated character.
}

@define[Function]{@name{char=?} @args{char1 char2 char3 @dots{}}}
@define[Function]{@name{char<?} @args{char1 char2 char3 @dots{}}}
@define[Function]{@name{char>?} @args{char1 char2 char3 @dots{}}}
@define[Function]{@name{char<=?} @args{char1 char2 char3 @dots{}}}
@define[Function]{@name{char>=?} @args{char1 char2 char3 @dots{}}}
@desc{[R6RS] These procedures impose a total ordering on the set of characters
according to their Unicode scalar values.
}

@subsubsection{Strings}

Strings are sequences of characters.
The @var{length} of a string is the number of characters that it contains. This
number is fixed when the string is created. The @var{valid indices} of a string
are the integers less than the length of the string. The first character of a
string has index 0, the second has index 1, and so on.

@define[Function]{@name{string?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a string, otherwise returns #f.}

@define[Function]{@name{make-string} @args{k :optional char}}
@desc{[R6RS] Returns a newly allocated string of length @var{k}. If @var{char}
is given, then all elements of the string are initialized to @var{char}, otherwise
the contents of the string are @code{#\space}.

These are equivalence:
@snipet[=> @code{(make-string 10 #\space)}]{(make-string 10)}
}

@define[Function]{@name{string} @args{char @dots{}}}
@desc{[R6RS] Returns a newly allocated string composed of the arguments.}

@define[Function]{@name{string-length} @args{string}}
@desc{[R6RS] Returns the number of characters in the given @var{string} as an
exact integer object.}

@define[Function]{@name{string-ref} @args{string k :optional fallback}}
@desc{[R6RS+] The @code{string-ref} procedure returns character. If a third
argument is given ant @var{k} is not a valid index, it returns @var{fallback},
otherwise raises @code{&assertion}.

@var{k} of string using zero-origin indexing.
}

@define[Function]{@name{string=?} @args{string1 string2 string3 @dots{}}}
@desc{[R6RS] Returns #t if the @var{strings} are the same length and contain the
same characters in the same positions. Otherwise, the @code{string=?} procedure
returns #f.
}

@define[Function]{@name{string<?} @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string>?} @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string<=?} @args{string1 string2 string3 @dots{}}}
@define[Function]{@name{string>=?} @args{string1 string2 string3 @dots{}}}
@desc{[R6RS] These procedures are the lexicographic extensions to strings of the
corresponding orderings on characters. For example, @code{string<?} is the
lexicographic ordering on strings induced by the ordering @code{char<?} on
characters. If two strings differ in length but are the same up to the length of
the shorter string, the shorter string is considered to be lexicographically less
than the longer string.
}

@define[Function]{@name{substring} @args{string start end}}
@desc{[R6RS] @var{String} must be a string, and @var{start} and @var{end} must
be exact integer objects satisfying

0 ≤ @var{start} ≤ @var{end} ≤ @code{(string-length @var{string})}.

The @code{substring} procedure returns a newly allocated string formed from the
characters of string beginning with index @var{start} (inclusive) and ending with
index @var{end} (exclusive).
}

@define[Function]{@name{string-append} @args{string @dots{}}}
@desc{[R6RS] Returns a newly allocated string whose characters form the
concatenation of the given @var{strings}.
}

@define[Function]{@name{string->list} @args{string} :optional start end}
@define[Function]{@name{list->string} @args{list} :optional start end}
@desc{[R6RS+] @var{List} must be a list of characters.

The @code{string->list} procedure returns a newly allocated list of the
characters that make up the given @var{string}.

The @code{list->string} procedure returns a newly allocated string formed from
the characters in @var{list}.

The @code{string->list} and @code{list->string} procedures are inverses so far
as @code{equal?} is concerned.

If optional argument @var{start} and @var{end} are given, it restrict the
conversion range. It convert from @var{start} (inclusive) to @var{end}
(exclusive).

If only @var{start} is given, then the @var{end} is the length of given string.
}

@define[Function]{@name{string-for-each} @args{proc string1 string2 @dots{}}}
@desc{[R6RS+] @var{Proc} should accept as many arguments as there are @var{strings}.
The @code{string-for-each} procedure applies @var{proc} element-wise to the
characters of the @var{strings} for its side effects, in order from the first
characters to the last. The return values of @code{string-for-each} are unspecified.

Analogous to for-each.
}

@define[Function]{@name{string-copy} @args{string :optional (start 0) (end -1)}}
@desc{[R6RS+] Returns a newly allocated copy of the given @var{string}.

If optional argument @var{start} was given, the procedure copies from the given
@var{start} index.

If optional argument @var{end} was given, the procedure copies to the given
@var{end} index (exclusive).
}

@subsubsection{Vectors}

Vectors are heterogeneous structures whose elements are indexed by integers. A
vector typically occupies less space than a list of the same length, and the
average time needed to access a randomly chosen element is typically less for the
vector than for the list.

The @var{length} of a vector is the number of elements that it contains. This
number is a non-negative integer that is fixed when the vector is created. The
@var{valid indices} of a vector are the exact non-negative integer objects less
than the length of the vector. The first element in a vector is indexed by zero,
and the last element is indexed by one less than the length of the vector.

@define[Function]{@name{vector?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a vector. Otherwise returns #f.}

@define[Function]{@name{make-vector} @args{k :optional fill}}
@desc{[R6RS] Returns a newly allocated vector of @var{k} elements. If a second
argument is given, then each element is initialized to @var{fill}. Otherwise the
initial contents of each element is unspecified.
}

@define[Function]{@name{vector} @args{obj @dots{}}}
@desc{[R6RS] Returns a newly allocated vector whose elements contain the given
arguments. Analogous to @code{list}.}

@define[Function]{@name{vector-length} @args{vector}}
@desc{[R6RS] Returns the number of elements in @var{vector} as an exact integer
object.}

@define[Function]{@name{vector-ref} @args{vector k :optional fallback}}
@desc{[R6RS+] The @code{vector-ref} procedure returns the contents of element
@var{k} of @var{vector}. If a third argument is given and @var{k} is not a valid
index, it returns @var{fallback}, otherwise raises @code{&assertion}.
}

@define[Function]{@name{vector-ref} @args{vector k obj}}
@desc{[R6RS] @var{K} must be a valid index of @var{vector}. The @code{vector-set!}
procedure stores @var{obj} in element @var{k} of @var{vector}, and returns
unspecified values.

It raises @code{&assertion} when it attempt to modify immutable vector on R6RS
mode.
}

@define[Function]{@name{vector->list} @args{vector :optional start end}}
@define[Function]{@name{list->vector} @args{list :optional start end}}
@desc{[R6RS+] The @code{vector->list} procedure returns a newly allocated list
of the objects contained in the elements of @var{vector}. The @code{list->vector}
procedure returns a newly created vector initialized to the elements of the list
@var{list}. The optional @var{start} and @var{end} arguments limit the range of
the source.

@snipet[=> (1 2 3 4 5)]{(vector->list '#(1 2 3 4 5))}
@snipet[=> #(1 2 3 4 5)]{(list->vector '(1 2 3 4 5))}
@snipet[=> (3 4)]{(vector->list '#(1 2 3 4 5) 2 4)}
@snipet[=> #(b c a b c)]{(list->vector (circular-list 'a 'b 'c) 1 6)}
}

@define[Function]{@name{vector-fill!} @args{vector :optional start end}}
@desc{[R6RS+] Stores @var{fill} in every element of @var{vector} and returns
unspecified values. Optional @var{start} and @var{end} limits the range of effect
between @var{start}-th index (inclusive) to @var{end}-th index (exclusive).
@var{Start} defaults to zero, and @var{end} defaults to the length of @var{vector}.
}

@define[Function]{@name{vector-map} @args{proc vector1 vactor2 @dots{}}}
@desc{[R6RS+] @var{Proc} should accept as many arguments as there are @var{vectors}.
The @code{vector-map} procedure applies @var{proc} element-wise to the elements
of the @var{vectors} and returns a vector of the results, in order. If multiple
returns occur from @code{vector-map}, the return values returned by earlier
returns are not mutated.

Analogous to @code{map}.
}

@define[Function]{@name{vector-for-each} @args{proc vector1 vactor2 @dots{}}}
@desc{[R6RS+] @var{Proc} should accept as many arguments as there are @var{vectors}.
The @code{vector-for-each} procedure applies @var{proc} element-wise to the
elements of the @var{vectors} for its side effects, in order from the first
elements to the last. The return values of @code{vector-for-each} are unspecified.

Analogous to @code{for-each}.
}

@subsubsection{Errors and violations}

@define[Function]{@name{error} @args{who message irritant @dots{}}}
@define[Function]{@name{assertion-violation} @args{who message irritant @dots{}}}
@desc{[R6RS] @var{Who} must be a string or a symbol or #f. @var{Message} must be
a string. The @var{irritants} are arbitrary objects.

These procedures raise an exception. The @code{error} procedure should be called
when an error has occurred, typically caused by something that has gone wrong in
the interaction of the program with the external world or the user. The
@code{assertion-violation} procedure should be called when an invalid call to a
procedure was made, either passing an invalid number of arguments, or passing an
argument that it is not specified to handle.

The @var{who} argument should describe the procedure or operation that detected
the exception. The @var{message} argument should describe the exceptional situation.
The @var{irritants} should be the arguments to the operation that detected the
operation.
}

@subsubsection{Control features}

@define[Function]{@name{apply} @args{proc arg1 @dots{} rest-args}}
@desc{[R6RS] @var{Rest-args} must be a list. @var{Proc} should accept n arguments,
where n is number of @var{args} plus the length of @var{rest-args}. The @code{apply}
procedure calls @var{proc} with the elements of the list
@code{(append (list @var{arg1} @dots{}) @var{rest-args})} as the actual arguments.

If a call to @code{apply} occurs in a tail context, the call to @code{proc} is
also in a tail context.
}

@define[Function]{@name{call-with-current-continuation} @args{proc}}
@define[Function]{@name{call/cc} @args{proc}}
@desc{[R6RS] @var{Proc} should accept one argument. The procedure 
@code{call-with-current-continuation} (which is the same as the procedure
@code{call/cc}) packages the current continuation as an "escape procedure" and
passes it as an argument to @var{proc}. The escape procedure is a Scheme procedure
that, if it is later called, will abandon whatever continuation is in effect a
that later time and will instead reinstate the continuation that was in effect
when the escape procedure was created. Calling the escape procedure may cause
the invocation of @var{before} and @var{after} procedures installed using
@code{dynamic-wind}.

The escape procedure accepts the same number of arguments as the continuation
of the original call to @code{call-with-current-continuation}.
}

@define[Function]{@name{values} @args{obj @dots{}}}
@desc{[R6RS] Returns @var{objs} as multiple values.
}

@define[Function]{@name{call-with-values} @args{producer consumer}}
@desc{[R6RS] @var{Producer} must be a procedure and should accept zero arguments.
@var{Consumer} must be a procedure and should accept as many values as
@var{producer} returns. The @code{call-with-values} procedure calls @var{producer}
with no arguments and a continuation that, when passed some values, calls the
@var{consumer} procedure with those values as arguments. The continuation for
the call to @var{consumer} is the continuation of the call to @code{call-with-values}.

If a call to @code{call-with-values} occurs in a tail context, the call to
@var{consumer} is also in a tail context.
}

@define[Function]{@name{dynamic-wind} @args{before thunk after}}
@desc{[R6RS] @var{Before}, @var{thunk}, and @var{after} must be procedures, and
each should accept zero arguments. These procedures may return any number of
values. The @code{dynamic-wind} procedure calls @var{thunk} without arguments,
returning the results of this call. Moreover, @code{dynamic-wind} calls @var{before}
without arguments whenever the dynamic extent of the call to @var{thunk} is
entered, and @var{after} without arguments whenever the dynamic extent of the
call to @var{thunk} is exited. Thus, in the absence of calls to escape procedures
created by @code{call-with-current-continuation}, @code{dynamic-wind} calls
@var{before}, @var{thunk}, and @var{after}, in that order.
}

@subsubsection{Iteration}

@define[Syntax]{@name{let} @args{variable bindings body @dots{}}}
@desc{[R6RS] “Named let” is a variant on the syntax of @code{let} that provides
a general looping construct and may also be used to express recursion. It has
the same syntax and semantics as ordinary @code{let} except that @var{variable}
is bound within @var{body} to a procedure whose parameters are the bound variables
and whose body is @var{body}. Thus the execution of @var{body} may be repeated
by invoking the procedure named by @var{variable}.
}

@subsubsection{Quasiquote}

@define[Syntax]{@name{quasiquote} @args{qq-template}}
@define["Auxiliary Syntax"]{@name{unquote} @args{qq-template}}
@define["Auxiliary Syntax"]{@name{unquote-splicing} @args{qq-template}}
@desc{[R6RS] "Quasiquote" expressions is useful for constructing a list or
vector structure when some but not all of the desired structure is known in
advance.

@var{Qq-template} should be as specified by the grammar at the end of this entry.

If no @code{unquote} or @code{unquote-splicing} forms appear within the
@var{qq-template}, the result of evaluating @code{(quasiquote @var{qq-template})}
is equivalent to the result of evaluating @code{(quote @var{qq-template})}.

If an @code{(unquote @var{expression} @dots{})} form appears inside a @var{qq-template},
however, the expressions are evaluated @code{("unquoted")} and their results are
inserted into the structure instead of the @code{unquote} form.

If an @code{(unquote-splicing @var{expression} @dots{})} form appears inside a
@var{qq-template}, then the expressions must evaluate to lists; the opening and
closing parentheses of the lists are then "stripped away" and the elements of
the lists are inserted in place of the @code{unquote-splicing} form.

Any @code{unquote-splicing} or multi-operand unquote form must appear only within
a list or vector @var{qq-template}.

Note: even though @code{unquote} and @code{unquote-splicing} are bounded, however
it does not work with import prefix nor renamed import. This may be fixed in future.
}

@subsubsection{Binding constructs for syntactic keywords}

The @code{let-syntax} and @code{letrec-syntax} forms bind keywords. On R6RS mode
it works like a @code{begin} form, a @code{let-syntax} or @code{letrec-syntax}
form may appear in a definition context, in which case it is treated as a
definition, and the forms in the body must also be definitions. A @code{let-syntax}
or @code{letrec-syntax} form may also appear in an expression context, in which
case the forms within their bodies must be expressions.

@define[Syntax]{@name{let-syntax} @args{bindings form @dots{}}}
@desc{[R6RS] @var{Bindings} must have the form

@snipet{((@var{keyword} @var{expression}) @dots{})}

Each @var{keyword} is an identifier, and each @var{expression} is an expression
that evaluates, at macro-expansion time, to a transformer. Transformers may be
created by @code{syntax-rules} or @code{identifier-syntax} or by one of the other
mechanisms described in library chapter on @secref["rnrs.syntax-case.6"]{"syntax-case"}.
It is a syntax violation for @var{keyword} to appear more than once in the list
of keywords being bound.

The @var{forms} are expanded in the syntactic environment obtained by extending
the syntactic environment of the @code{let-syntax} form with macros whose keywords
are the @var{keywords}, bound to the specified transformers. Each binding of a 
@var{keyword} has the @var{forms} as its region.
}

@define[Syntax]{@name{letrec-syntax} @args{bindings form @dots{}}}
@desc{[R6RS] @var{Bindings} must have the form

@snipet{((@var{keyword} @var{expression}) @dots{})}

Each @var{keyword} is an identifier, and each @var{expression} is an expression
that evaluates, at macro-expansion time, to a transformer. Transformers may be
created by @code{syntax-rules} or @code{identifier-syntax} or by one of the other
mechanisms described in library chapter on @secref["rnrs.syntax-case.6"]{"syntax-case"}.
It is a syntax violation for @var{keyword} to appear more than once in the list
of keywords being bound.

The @var{forms} are expanded in the syntactic environment obtained by extending
the syntactic environment of the @code{letrec-syntax} form with macros whose
keywords are the @var{keywords}, bound to the specified transformers. Each
binding of a @var{keyword} has the bindings as well as the @var{forms} within its
region, so the transformers can transcribe @var{forms} into uses of the macros
introduced by the @code{letrec-syntax} form.

Note: The @var{forms} of a @code{let-syntax} and a @code{letrec-syntax} form are
treated, whether in definition or expression context, as if wrapped in an implicit
@code{begin} on R6RS mode, it is, then, treated as if wrapped in an implicit
@code{let} on compatible mode. Thus on compatible mode, it creates a scope.
}

@subsubsection{Macro transformers}

In R6RS, it requires @code{'_'} @code{'@dots{}'} as bounded symbols but in Sagittarius
these are not bound. And if import clause has rename or prefix these auxiliary
syntax are not be renamed or prefixed. This behaivour may be fixed in future.

@define[Syntax]{@name{syntax-rules} @args{(literal @dots{}) rule @dots{}}}
@desc{[R6RS] Each @var{literal} must be an identifier. Each @var{rule} must have
the following form:

@snipet{(srpattern template)}

An @var{srpattern} is a restricted form of @var{pattern}, namely, a nonempty
@var{pattern} in one of four parenthesized forms below whose first subform is
an identifier or an underscore @code{_}. A @var{pattern} is an identifier,
constant, or one of the following.

@itemlist[
@item{(pattern @dots{})}
@item{(pattern pattern @dots{} . pattern)}
@item{(pattern @dots{} pattern ellipsis pattern @dots{})}
@item{(pattern @dots{} pattern ellipsis pattern @dots{} . pattern)}
@item{#(pattern @dots{})}
@item{#(pattern @dots{} pattern ellipsis pattern @dots{})}
]

An @var{ellipsis} is the identifier @code{"@dots{}"} (three periods).

A @var{template} is a pattern variable, an identifier that is not a pattern
variable, a pattern datum, or one of the following.

@itemlist[
@item{(subtemplate @dots{})}
@item{(subtemplate @dots{} . template)}
@item{#(subtemplate @dots{})}
]

A @var{subtemplate} is a @var{template} followed by zero or more ellipses.

An instance of @code{syntax-rules} evaluates, at macro-expansion time, to a new
macro transformer by specifying a sequence of hygienic rewrite rules. A use of a
macro whose keyword is associated with a transformer specified by @code{syntax-rules}
is matched against the patterns contained in the @var{rules}, beginning with the
leftmost @var{rule}. When a match is found, the macro use is transcribed hygienically
according to the @var{template}. It is a syntax violation when no match is found.
}

@define[Syntax]{@name{identifier-syntax} @args{template}}
@define[Syntax]{@name{identifier-syntax} @args{(id1 template1) (set! id2 pattern) template2}}
@desc{[R6RS] The @var{ids} must be identifiers. The @var{templates} must be as
for @code{syntax-rules}.

When a keyword is bound to a transformer produced by the first form of 
@code{identifier-syntax}, references to the keyword within the scope of the
binding are replaced by @var{template}.

@snipet{(define p (cons 4 5))}
@snipet{(define-syntax p.car (identifier-syntax (car p)))}
@snipet[=> 4]{p.car}
@snipet[=> "&syntax exception"]{(set! p.car 15)}

The second, more general, form of @code{identifier-syntax} permits the transformer
to determine what happens when @code{set!} is used. In this case, uses of the
identifier by itself are replaced by @var{template1}, and uses of @code{set!} with
the identifier are replaced by @var{template2}

@snipet{(define p (cons 4 5))}
@codeblock{
(define-syntax p.car
  (identifier-syntax
    (_ (car p))
    ((set! _ e) (set-car! p e))))
}
@snipet{(set! p.car 15)}
@snipet[=> 15]{p.car}
@snipet[=> (15 5)]{p}
}
