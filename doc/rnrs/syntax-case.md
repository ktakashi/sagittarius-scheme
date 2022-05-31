[§2] Syntax-case {#rnrs.syntax-case.6}
-------------

###### [!Library] `(rnrs syntax-case (6))` 

[R6RS] The `(rnrs syntax-case (6))`library provides support for
writing low-level macros in a high-level style, with automatic syntax checking,
input destructuring, output restructuring, maintenance of lexical scoping and
referential transparency (hygiene), and support for controlled identifier capture.


###### [!Syntax] `syntax-case`  _expression_ _(literal_ _..._ _)_ _clause_ _..._

[R6RS] Each _literal_ must be an identifier. Each _clause_ must take
one of the following two forms.

``(_pattern_ _output-expression_)``

``(_pattern_ _fender_ _output-expression_)``

_Fender_ and _output-expression_ must be expressions.

_Pattern_ is the same as `syntax-rules`. See
[(rnrs base (6))](#rnrs.base.6) section.

A `syntax-case` expression first evaluates expression. It then attempts to
match the _pattern_ from the first _clause_ against the resulting value,
which is unwrapped as necessary to perform the match. If the _pattern_matches the value and no _fender_ is present, _output-expression_ is
evaluated and its value returned as the value of the `syntax-case` expression.
If the _pattern_ does not match the value, `syntax-case` tries the second
_clause_, then the third, and so on. It is a syntax violation if the value
does not match any of the _patterns_.

If the optional _fender_ is present, it serves as an additional constraint on
acceptance of a clause. If the _pattern_ of a given _clause_ matches the
input value, the corresponding _fender_ is evaluated. If _fender_ evaluates
to a true value, the _clause_ is accepted; otherwise, the _clause_ is
rejected as if the _pattern_ had failed to match the value. _Fenders_ are
logically a part of the matching process, i.e., they specify additional matching
constraints beyond the basic structure of the input.

Pattern variables contained within a clause's _pattern_ are bound to the
corresponding pieces of the input value within the clause's _fender_ (if present)
and _output-expression_. Pattern variables can be referenced only within syntax
expressions (see below). Pattern variables occupy the same name space as program
variables and keywords.

If the `syntax-case` form is in tail context, the `output-expressions`are also in tail position.


###### [!Syntax] `syntax`  _template_

[R6RS] A _template_ is a pattern variable, an identifier that is not a
pattern variable, a pattern datum, or one of the following.

``(_subtemplate_ ...)``

``(_subtemplate_ ... . _template_)``

``#(_subtemplate_ ...)``

A _subtemplate_ is a template followed by zero or more ellipses.

The value of a `syntax` form is a copy of _template_ in which the
pattern variables appearing within the _template_ are replaced with the input
_subforms_ to which they are bound. Pattern data and identifiers that are not
pattern variables or ellipses are copied directly into the output. A
_subtemplate_ followed by an ellipsis expands into zero or more occurrences
of the _subtemplate_. Pattern variables that occur in subpatterns followed
by one or more ellipses may occur only in _subtemplates_ that are followed by
(at least) as many ellipses. These pattern variables are replaced in the output
by the input subforms to which they are bound, distributed as specified. If a
pattern variable is followed by more ellipses in the _subtemplate_ than in
the associated subpattern, the input form is replicated as necessary. The
_subtemplate_ must contain at least one pattern variable from a subpattern
followed by an ellipsis, and for at least one such pattern variable, the
_subtemplate_ must be followed by exactly as many ellipses as the subpattern
in which the pattern variable appears.


###### [!Function] `identifier?`  _obj_

[R6RS] Returns #t if _obj_ is an identifier, i.e., a syntax object
representing an identifier, and #f otherwise.


###### [!Function] `bound-identifier=?`  _id1_ _id2_

[R6RS] _Id1_ and _id2_ must be identifiers. The procedure
`bound-identifier=?` returns #t if given arguments are exactly the same object.

The _bound-identifier=?_ procedure can be used for detecting duplicate
identifiers in a binding construct or for other preprocessing of a binding
construct that requires detecting instances of the bound identifiers.


###### [!Function] `free-identifier=?`  _id1_ _id2_

[R6RS] _Id1_ and _id2_ must be identifiers. The `free-identifier=?`procedure returns #t if given arguments are indicating the same bindings.


###### [!Function] `syntax->datum`  _syntax-object_

[R6RS] Strips all syntactic information from a syntax object and returns the
corresponding Scheme datum.


###### [!Function] `datum->syntax`  _template-id_ _datum_

[R6RS] _Template-id_ must be a template identifier and _datum_should be a datum value.

The `datum->syntax` procedure returns a syntax-object representation of
_datum_ that contains the same contextual information as _template-id_,
with the effect that the syntax object behaves as if it were introduced into the
code when _template-id_ was introduced.

The `datum->syntax` procedure allows a transformer to "bend" lexical scoping
rules by creating implicit identifiers that behave as if they were present in the
input form, thus permitting the definition of macros that introduce visible
bindings for or references to identifiers that do not appear explicitly in the
input form. For example, the following defines a `loop` expression that uses
this controlled form of identifier capture to bind the variable break to an escape
procedure within the loop body.

``````````scheme
(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax
           ([break (datum->syntax (syntax k) 'break)])
         (syntax 
	  (call-with-current-continuation
	   (lambda (break)
	     (let f () e ... (f))))))])))

(let ((n 3) (ls '()))
  (loop
    (if (= n 0) (break ls))
    (set! ls (cons 'a ls))
    (set! n (- n 1)))) 
``````````
=> ``(a a a)``



###### [!Function] `generate-temporaries`  _l_

[R6RS] _L_ must be a list or syntax object representing a list-structured
form. The number of temporaries generated is the number of elements in _l_.
Each temporary is guaranteed to be unique.

NOTE: If you want to create just one temporary symbol and do not think about
portability, it's better to use `gensym` in `(sagittarius)` library.


###### [!Macro] `with-syntax`  _((pattern_ _expression)_ _..._ _)_ _body_

[R6RS] The `with-syntax` form is used to bind pattern variables, just
as `let` is used to bind variables. This allows a transformer to construct
its output in separate pieces, then put the pieces together.

Each _pattern_ is identical in form to a `syntax-case` pattern. The
value of each _expression_ is computed and destructured according to the
corresponding _pattern_, and pattern variables within the _pattern_ are
bound as with `syntax-case` to the corresponding portions of the value
within body.


###### [!Macro] `quasisyntax`  _template_
###### [!Auxiliary Macro] `unsyntax` 
###### [!Auxiliary Macro] `unsyntax-splicing` 

[R6RS] The `quasisyntax` form is similar to `syntax`, but it
allows parts of the quoted text to be evaluated, in a manner similar to the
operation of `quasiquote`.

Within a `quasisyntax` template, subforms of `unsyntax` and
`unsyntax-splicing` forms are evaluated, and everything else is treated
as ordinary template material, as with `syntax`. The value of each
`unsyntax` subform is inserted into the output in place of the `unsyntax`form, while the value of each `unsyntax-splicing` subform is spliced into
the surrounding list or vector structure. Uses of `unsyntax` and
`unsyntax-splicing` are valid only within `quasisyntax` expressions.

A `quasisyntax` expression may be nested, with each `quasisyntax`introducing a new level of syntax quotation and each `unsyntax` or
`unsyntax-splicing` taking away a level of quotation. An expression nested
within n `quasisyntax` expressions must be within n `unsyntax` or
`unsyntax-splicing` expressions to be evaluated.


###### [!Function] `syntax-violation`  _who_ _message_ _form_ _:optional_ _subform_

[R6RS] _Who_ must be #f or a string or a symbol. _Message_ must be
a string. _Form_ must be a syntax object or a datum value. _Subform_ must
be a syntax object or a datum value.

The `syntax-violation` procedure raises an exception, reporting a syntax
violation. _Who_ should describe the macro transformer that detected the
exception. The _message_ argument should describe the violation. _Form_should be the erroneous source syntax object or a datum value representing a form.
The optional _subform_ argument should be a syntax object or datum value
representing a form that more precisely locates the violation.


