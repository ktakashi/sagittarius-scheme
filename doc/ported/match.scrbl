@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "ported.match"]{(match) -- Pattern matching}

The @code{match} is originally from Alex Shin's `portable hygineic pattern matcher'
The documents below are derived from his source code.

@define[Library]{@name{(match)}}
@desc{This is a full superset of the popular@hyperlink[
:href "http://www.cs.indiana.edu/scheme-repository/code.match.html"]{match}
package by Andrew Wright, written in fully portable @code{syntax-rules}
and thus preserving hygiene.

The most notable extensions are the ability to use @var{non-linear}
patterns - patterns in which the same identifier occurs multiple
times, tail patterns after ellipsis, and the experimental tree patterns.
}

@subsubsection{Patterns}

Patterns are written to look like the printed representation of
the objects they match.  The basic usage is

@snipet{(match expr (pat body ...) ...)}

where the result of @var{expr} is matched against each pattern in
turn, and the corresponding body is evaluated for the first to
succeed.  Thus, a list of three elements matches a list of three
elements.

@snipet{(let ((ls (list 1 2 3))) (match ls ((1 2 3) #t)))}

If no patterns match an error is signalled.

Identifiers will match anything, and make the corresponding
binding available in the body.

@snipet{(match (list 1 2 3) ((a b c) b))}

If the same identifier occurs multiple times, the first instance
will match anything, but subsequent instances must match a value
which is @code{equal?} to the first.

@snipet{(match (list 1 2 1) ((a a b) 1) ((a b a) 2))}

The special identifier @code{_} matches anything, no matter how
many times it is used, and does not bind the result in the body.

@snipet{(match (list 1 2 1) ((_ _ b) 1) ((a b a) 2))}

To match a literal identifier (or list or any other literal), use
@code{quote}.

@snipet{(match 'a ('b 1) ('a 2))}

Analogous to its normal usage in scheme, @code{quasiquote} can
be used to quote a mostly literally matching object with selected
parts unquoted.

@snipet|{(match (list 1 2 3) (`(1 ,b ,c) (list b c)))}|

Often you want to match any number of a repeated pattern.  Inside
a list pattern you can append @code{...} after an element to
match zero or more of that pattern (like a regexp Kleene star).

@snipet{(match (list 1 2) ((1 2 3 ...) #t))}
@snipet{(match (list 1 2 3) ((1 2 3 ...) #t))}
@snipet{(match (list 1 2 3 3 3) ((1 2 3 ...) #t))}

Pattern variables matched inside the repeated pattern are bound to
a list of each matching instance in the body.

@snipet{(match (list 1 2) ((a b c ...) c))}
@snipet{(match (list 1 2 3) ((a b c ...) c))}
@snipet{(match (list 1 2 3 4 5) ((a b c ...) c))}

More than one @code{...} may not be used in the same list, since
this would require exponential backtracking in the general case.
However, @code{...} need not be the final element in the list,
and may be succeeded by a fixed number of patterns.

@snipet{(match (list 1 2 3 4) ((a b c ... d e) c))}
@snipet{(match (list 1 2 3 4 5) ((a b c ... d e) c))}
@snipet{(match (list 1 2 3 4 5 6 7) ((a b c ... d e) c))}

@code{___} is provided as an alias for @code{...} when it is
inconvenient to use the ellipsis (as in a @code{syntax-rules} template).

The @code{..1} syntax is exactly like the @code{...} except
that it matches one or more repetitions (like a regexp "+").

@snipet{(match (list 1 2) ((a b c ..1) c))}
@snipet{(match (list 1 2 3) ((a b c ..1) c))}

The boolean operators @code{and}, @code{or} and @code{not}
can be used to group and negate patterns analogously to their
Scheme counterparts.

The @code{and} operator ensures that all subpatterns match.
This operator is often used with the idiom @code{(and x pat)} to
bind @var{x} to the entire value that matches @var{pat}
(c.f. "as-patterns" in ML or Haskell).  Another common use is in
conjunction with @code{not} patterns to match a general case
with certain exceptions.

@snipet{(match 1 ((and) #t))}
@snipet{(match 1 ((and x) x))}
@snipet{(match 1 ((and x 1) x))}

The @code{or} operator ensures that at least one subpattern
matches.  If the same identifier occurs in different subpatterns,
it is matched independently.  All identifiers from all subpatterns
are bound if the @code{or} operator matches, but the binding is
only defined for identifiers from the subpattern which matched.

@snipet{(match 1 ((or) #t) (else #f))}
@snipet{(match 1 ((or x) x))}
@snipet{(match 1 ((or x 2) x))}

The @code{not} operator succeeds if the given pattern doesn't
match.  None of the identifiers used are available in the body.

@snipet{(match 1 ((not 2) #t))}

The more general operator @code{?} can be used to provide a
predicate.  The usage is @code{(? predicate pat ...)} where
@var{predicate} is a Scheme expression evaluating to a predicate
called on the value to match, and any optional patterns after the
predicate are then matched as in an @code{and} pattern.

@snipet{(match 1 ((? odd? x) x))}

The field operator @code{=} is used to extract an arbitrary
field and match against it.  It is useful for more complex or
conditional destructuring that can't be more directly expressed in
the pattern syntax.  The usage is @code{(= field pat)}, where
@var{field} can be any expression, and should result in a
procedure of one argument, which is applied to the value to match
to generate a new value to match against @var{pat}.

Thus the pattern @code{(and (= car x) (= cdr y))} is equivalent
to @code{(x . y)}, except it will result in an immediate error
if the value isn't a pair.

@snipet{(match '(1 . 2) ((= car x) x))}
@snipet{(match 4 ((= sqrt x) x))}

The record operator @code{$} is used as a concise way to match
records defined by SRFI-9 (or SRFI-99).  The usage is
@code{($ rtd field ...)}, where @var{rtd} should be the record
type descriptor specified as the first argument to
@code{define-record-type}, and each @var{field} is a subpattern
matched against the fields of the record in order.  Not all fields
must be present.

@codeblock{
(let ()
  (define-record-type employee
    (make-employee name title)
    employee?
    (name get-name)
    (title get-title))
  (match (make-employee "Bob" "Doctor")
    (($ employee n t) (list t n))))
}

The @code{set!} and @code{get!} operators are used to bind an
identifier to the setter and getter of a field, respectively.  The
setter is a procedure of one argument, which mutates the field to
that argument.  The getter is a procedure of no arguments which
returns the current value of the field.

@snipet{(let ((x (cons 1 2))) (match x ((1 . (set! s)) (s 3) x)))}
@snipet{(match '(1 . 2) ((1 . (get! g)) (g)))}

The new operator @code{***} can be used to search a tree for
subpatterns.  A pattern of the form @code{(x *** y)} represents
the subpattern @var{y} located somewhere in a tree where the path
from the current object to @var{y} can be seen as a list of the
form @code{(x ...)}.  @var{y} can immediately match the current
object in which case the path is the empty list.  In a sense it's
a 2-dimensional version of the @code{...} pattern.

As a common case the pattern @code{(_ *** y)} can be used to
search for @var{y} anywhere in a tree, regardless of the path
used.

@snipet{(match '(a (a (a b))) ((x *** 'b) x))}
@snipet{(match '(a (b) (c (d e) (f g))) ((x *** 'g) x))}

@subsubsection{Syntax}

@define[Macro]{@name{match} @args{expr (pattern . body) @dots{}}}
@define[Macro]{@name{match} @args{expr (pattern (=> failure) . body) @dots{}}}
@desc{
turn, according to the pattern rules described in the previous
section, until the the first @var{pattern} matches.  When a match is
found, the corresponding @var{body}s are evaluated in order,
and the result of the last expression is returned as the result
of the entire @scheme{match}.  If a @var{failure} is provided,
then it is bound to a procedure of no arguments which continues,
processing at the next @var{pattern}.  If no @var{pattern} matches,
an error is signalled.
}


@define[Macro]{@name{match-lambda} @args{clause @dots{}}}
@desc{
Shortcut for @code{lambda} + @code{match}.  Creates a
procedure of one argument, and matches that argument against each
clause.
}

@define[Macro]{@name{match-lambda*} @args{clause @dots{}}}
@desc{
Similar to @code{match-lambda}.  Creates a procedure of any
number of arguments, and matches the argument list against each
clause.
}

@define[Macro]{@name{match-let} @args{((pat expr) @dots{}) body-expr @dots{}}}
@define[Macro]{@name{match-let} @args{name ((pat expr) @dots{}) body-expr @dots{}}}
@desc{
Matches each var to the corresponding expression, and evaluates
the body with all match variables in scope.  Raises an error if
any of the expressions fail to match.  Syntax analogous to named
let can also be used for recursive functions which match on their
arguments as in @scheme{match-lambda*}.
}

@define[Macro]{@name{match-letrec} @args{((pat expr) @dots{}) body-expr @dots{}}}
@desc{
Similar to @code{match-let}, but analogously to @code{letrec}
matches and binds the variables with all match variables in scope.
}

@define[Macro]{@name{match-let*} @args{((pat expr) @dots{}) body-expr @dots{}}}
@desc{
Similar to @scheme{match-let}, but analogously to @scheme{let*}
matches and binds the variables in sequence, with preceding match
variables in scope.
}