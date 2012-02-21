@; -*- mode: scribble; coding: utf-8; -*-

@subsection[:tag "rnrs.syntax-case.6"]{Syntax-case}

On Sagittarius Scheme @code{syntax-case} does not behave as R6RS required. This
is why it is @b{MOSTLY} R6RS implementation. As far as I know the part which is
not compromised with R6RS is following expression does not return correct answer:

@codeblock{
(define-syntax let/scope
  (lambda(x)
    (syntax-case x ()
      ((k scope-name body @dots{})
       (syntax
	(let-syntax
	    ((scope-name
	      (lambda(x)
		(syntax-case x ()
		  ((_ b (@dots{} @dots{}))
		  (quasisyntax
		   (begin
		      (unsyntax-splicing
		       (datum->syntax (syntax k)
				      (syntax->datum 
				       (syntax (b (@dots{} @dots{})))))))))))))
           body @dots{}))))))
(let ((x 1))
  (let/scope d1
    (let ((x 2))
      (let/scope d2
        (let ((x 3))
          (list (d1 x) (d2 x) x))))))
}

This should return @code{(1 2 3)} however on Sagittarius it returns @code{(1 1 3}).
If you want to write a portable program with @code{syntax-case}, it is better to
check in other implementation too.

@define[Library]{@name{(rnrs syntax-case (6))}}
@desc{[R6RS] The @code{(rnrs syntax-case (6))}library provides support for
writing low-level macros in a high-level style, with automatic syntax checking,
input destructuring, output restructuring, maintenance of lexical scoping and
referential transparency (hygiene), and support for controlled identifier capture.
}

@define[Syntax]{@name{syntax-case} @args{expression (literal @dots{}) clause @dots{}}}
@desc{[R6RS] Each @var{literal} must be an identifier. Each @var{clause} must take
one of the following two forms.

@snipet{(@var{pattern} @var{output-expression})}
@snipet{(@var{pattern} @var{fender} @var{output-expression})}

@var{Fender} and @var{output-expression} must be expressions.

@var{Pattern} is the same as @code{syntax-rules}. See
@secref["rnrs.base.6"]{(rnrs base (6))} section.

A @code{syntax-case} expression first evaluates expression. It then attempts to
match the @var{pattern} from the first @var{clause} against the resulting value,
which is unwrapped as necessary to perform the match. If the @var{pattern}
matches the value and no @var{fender} is present, @var{output-expression} is
evaluated and its value returned as the value of the @code{syntax-case} expression.
If the @var{pattern} does not match the value, @code{syntax-case} tries the second
@var{clause}, then the third, and so on. It is a syntax violation if the value
does not match any of the @var{patterns}.

If the optional @var{fender} is present, it serves as an additional constraint on
acceptance of a clause. If the @var{pattern} of a given @var{clause} matches the
input value, the corresponding @var{fender} is evaluated. If @var{fender} evaluates
to a true value, the @var{clause} is accepted; otherwise, the @var{clause} is
rejected as if the @var{pattern} had failed to match the value. @var{Fenders} are
logically a part of the matching process, i.e., they specify additional matching
constraints beyond the basic structure of the input.

Pattern variables contained within a clause's @var{pattern} are bound to the
corresponding pieces of the input value within the clause's @var{fender} (if present)
and @var{output-expression}. Pattern variables can be referenced only within syntax
expressions (see below). Pattern variables occupy the same name space as program
variables and keywords.

If the @code{syntax-case} form is in tail context, the @code{output-expressions}
are also in tail position.
}

@define[Syntax]{@name{syntax} @args{template}}
@desc{[R6RS] A @var{template} is a pattern variable, an identifier that is not a
pattern variable, a pattern datum, or one of the following.

@snipet{(@var{subtemplate} @dots{})}
@snipet{(@var{subtemplate} @dots{} . @var{template})}
@snipet{#(@var{subtemplate} @dots{})}

A @var{subtemplate} is a template followed by zero or more ellipses.

The value of a @code{syntax} form is a copy of @var{template} in which the
pattern variables appearing within the @var{template} are replaced with the input
@var{subforms} to which they are bound. Pattern data and identifiers that are not
pattern variables or ellipses are copied directly into the output. A
@var{subtemplate} followed by an ellipsis expands into zero or more occurrences
of the @var{subtemplate}. Pattern variables that occur in subpatterns followed
by one or more ellipses may occur only in @var{subtemplates} that are followed by
(at least) as many ellipses. These pattern variables are replaced in the output
by the input subforms to which they are bound, distributed as specified. If a
pattern variable is followed by more ellipses in the @var{subtemplate} than in
the associated subpattern, the input form is replicated as necessary. The
@var{subtemplate} must contain at least one pattern variable from a subpattern
followed by an ellipsis, and for at least one such pattern variable, the
@var{subtemplate} must be followed by exactly as many ellipses as the subpattern
in which the pattern variable appears.
}

@define[Function]{@name{identifier?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is an identifier, i.e., a syntax object
representing an identifier, and #f otherwise.
}

@define[Function]{@name{bound-identifier=?} @args{id1 id2}}
@desc{[R6RS] @var{Id1} and @var{id2} must be identifiers. The procedure
@code{bound-identifier=?} returns #t if given arguments are exactly the same object.

The @var{bound-identifier=?} procedure can be used for detecting duplicate
identifiers in a binding construct or for other preprocessing of a binding
construct that requires detecting instances of the bound identifiers.
}

@define[Function]{@name{free-identifier=?} @args{id1 id2}}
@desc{[R6RS] @var{Id1} and @var{id2} must be identifiers. The @code{free-identifier=?}
procedure returns #t if given arguments are indicating the same bindings.
}

@define[Function]{@name{syntax->datum} @args{syntax-object}}
@desc{[R6RS] Strips all syntactic information from a syntax object and returns the
corresponding Scheme datum.
}

@define[Function]{@name{datum->syntax} @args{template-id datum}}
@desc{[R6RS] @var{Template-id} must be a template identifier and @var{datum}
should be a datum value.

The @code{datum->syntax} procedure returns a syntax-object representation of
@var{datum} that contains the same contextual information as @var{template-id},
with the effect that the syntax object behaves as if it were introduced into the
code when @var{template-id} was introduced.

The @code{datum->syntax} procedure allows a transformer to "bend" lexical scoping
rules by creating implicit identifiers that behave as if they were present in the
input form, thus permitting the definition of macros that introduce visible
bindings for or references to identifiers that do not appear explicitly in the
input form. For example, the following defines a @code{loop} expression that uses
this controlled form of identifier capture to bind the variable break to an escape
procedure within the loop body.

@codeblock[=> (a a a)]{
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
}
}

@define[Function]{@name{generate-temporaries} @args{l}}
@desc{[R6RS] @var{L} must be a list or syntax object representing a list-structured
form. The number of temporaries generated is the number of elements in @var{l}.
Each temporary is guaranteed to be unique.

NOTE: If you want to create just one temporary symbol and do not think about
portability, it's better to use @code{gensym} in @code{(sagittarius)} library.
}

@define[Macro]{@name{with-syntax} @args{((pattern expression) @dots{}) body}}
@desc{[R6RS] The @code{with-syntax} form is used to bind pattern variables, just
as @cde{let} is used to bind variables. This allows a transformer to construct
its output in separate pieces, then put the pieces together.

Each @var{pattern} is identical in form to a @code{syntax-case} pattern. The
value of each @var{expression} is computed and destructured according to the
corresponding @var{pattern}, and pattern variables within the @var{pattern} are
bound as with @code{syntax-case} to the corresponding portions of the value
within body.
}

@define[Macro]{@name{quasisyntax} @args{template}}
@define["Auxiliary Macro"]{@name{unsyntax}}
@define["Auxiliary Macro"]{@name{unsyntax-splicing}}
@desc{[R6RS] The @code{quasisyntax} form is similar to @code{syntax}, but it
allows parts of the quoted text to be evaluated, in a manner similar to the
operation of @code{quasiquote}.

Within a @code{quasisyntax} template, subforms of @code{unsyntax} and
@code{unsyntax-splicing} forms are evaluated, and everything else is treated
as ordinary template material, as with @code{syntax}. The value of each
@code{unsyntax} subform is inserted into the output in place of the @code{unsyntax}
form, while the value of each @code{unsyntax-splicing} subform is spliced into
the surrounding list or vector structure. Uses of @code{unsyntax} and
@code{unsyntax-splicing} are valid only within @code{quasisyntax} expressions.

A @code{quasisyntax} expression may be nested, with each @code{quasisyntax}
introducing a new level of syntax quotation and each @code{unsyntax} or
@code{unsyntax-splicing} taking away a level of quotation. An expression nested
within n @code{quasisyntax} expressions must be within n @code{unsyntax} or
@code{unsyntax-splicing} expressions to be evaluated.
}

@define[Function]{@name{syntax-violation} @args{who message form :optional subform}}
@desc{[R6RS] @var{Who} must be #f or a string or a symbol. @var{Message} must be
a string. @var{Form} must be a syntax object or a datum value. @var{Subform} must
be a syntax object or a datum value.

The @code{syntax-violation} procedure raises an exception, reporting a syntax
violation. @var{Who} should describe the macro transformer that detected the
exception. The @var{message} argument should describe the violation. @var{Form}
should be the erroneous source syntax object or a datum value representing a form.
The optional @var{subform} argument should be a syntax object or datum value
representing a form that more precisely locates the violation.
}