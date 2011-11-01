@; -*- mode: scribble; coding: utf-8; -*-

@subsection[:tag "rnrs.composite"]{Composite libraries}

@subsubsection[:tag "rnrs.eval.6"]{eval}

@define[Library]{@name{(rnrs eval (6))}}
@desc{[R6RS]The @code{(rnrs eval (6))} library allows a program to create Scheme
expressions as data at run time and evaluate them.
}

@define[Function]{@name{eval} @args{expression environment}}
@desc{[R6RS] Evaluates @var{expression} in the specified @var{environment} and
returns its value. @var{Expression} must be a syntactically valid Scheme
expression represented as a datum value.

R6RS requires @var{envionment} an envitonment which must be created by the
@code{environment} procedure. However on Sagittarius, @var{environment} can be
anything. This behaviour might be fixed in future.
}

@define[Function]{@name{environment} @args{import-spec @dots{}}}
@desc{[R6RS] @var{Import-spec} must be a datum representing an import spec. The
@code{environment} procedure returns an environment corresponding to @var{import-spec}.
}

@subsubsection[:tag "rnrs.mutable-pairs.6"]{Mutable pairs}

@define[Library]{@name{(rnrs mutable-pairs (6))}}
@desc{[R6RS] This library exports @code{set-car!} and @code{set-cdr!}.}

@define[Function]{@name{set-car!} @args{pair obj}}
@define[Function]{@name{set-cdr!} @args{pair obj}}
@desc{[R6RS] Store @var{obj} in the car/cdr field of @var{pair}. These procedures
return unspecified value.

On Sagittarius Scheme, these procedures can modify immutable pairs.
}

@subsubsection[:tag "rnrs.mutable-strings.6"]{Mutable strings}

@define[Library]{@name{(rnrs mutable-stringss (6))}}
@desc{[R6RS] This library exports @code{string-set!} and @code{string-fill!}.}

@define[Function]{@name{string-set!} @args{string k char}}
@desc{[R6RS] @var{K} must be a valid index of @var{string}.

The @code{string-set!} procedure stores @var{char} in element @var{k} of string
and returns unspecified values.
}

@define[Function]{@name{string-fill!}
 @args{string char :optional (start 0) (end @code{(string-length @var{string})})}}
@desc{[R6RS+] Stores @var{char} in every element of the given @var{string} and
returns unspecified values. Optional arguments @var{start} and @var{end} restrict
the ragne of filling.

Passing an immutable string to these procedures cause an exception with condition
type @code{&assertion} to be raised.
}

@subsubsection[:tag "rnrs.r5rs.6"]{R5RS compatibility}

@define[Library]{@name{(rnrs r5rs (6))}}
@desc{This library provides R5RS compatibility procedures such as
@code{exact->inexact}.}

@define[Function]{@name{exact->inexect} @args{z}}
@define[Function]{@name{inexact->exect} @args{z}}
@desc{[R6RS] These are the same as the @code{inexact} and @code{exact} procedures.}

@define[Function]{@name{quotient} @args{n1 n2}}
@define[Function]{@name{remainder} @args{n1 n2}}
@define[Function]{@name{modulo} @args{n1 n2}}
@desc{[R6RS] Returns the quotient, remainder and modulo of dividing an integer
@var{n1} by an integer @var{n2}, respectively. The result is an exact number only
if both @var{n1} and @var{n2} are exact numbers.
}

@define[Macro]{@name{delay} @args{expression}}
@desc{[R6RS] The @code{delay} construct is used together with the procedure
@code{force} to implement @var{lazy evaluation} or @var{call by need}.
@code{(delay @var{expression})} returns an object called a @var{promise} which
at some point in the future may be asked (by the @code{force} procedure) to
evaluate expression, and deliver the resulting value. The effect of expression
returning multiple values is unspecified.
}

@define[Function]{@name{force} @args{promise}}
@desc{[R6RS] @var{Promise} must be a promise.

The @code{force} procedure forces the value of @var{promise}. If no value has
been computed for the promise, then a value is computed and returned. The value
of the promise is cached (or "memoized") so that if it is forced a second time,
the previously computed value is returned.
}

@define[Function]{@name{null-environment} @args{n}}
@define[Function]{@name{scheme-report-environment} @args{n}}
@desc{[R6RS] @var{N} must be the exact integer object 5. These procedures return
an environment which is suitable to use with @code{eval}.
}
