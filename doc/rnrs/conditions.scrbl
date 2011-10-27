@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.conditions.6"]{Conditions}

@define[Library]{@name{(rnrs conditions (6))}}
@desc{The section describes @code{(rnrs conditions (6))}library for creating and
inspecting condition types and values. A condition value encapsulates information
about an exceptional situation.
}

@define[Function]{@name{condition} @args{condition @dots{}}}
@desc{[R6RS] The @code{condition} procedure returns a condition object with the
components of the @var{conditions} as its components, in the same order. The
returned condition is compound if the total number of components is zero or
greater than one. Otherwise, it may be compound or simple.
}

@define[Function]{@name{simple-condition} @args{condition}}
@desc{[R6RS] The @code{simple-conditions} procedure returns a list of the
components of @var{condition}, in the same order as they appeared in the
construction of condition.
}

@define[Function]{@name{condition?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a (simple or compound) condition,
otherwise returns #f.}

@define[Function]{@name{condition-predicate} @args{rtd}}
@desc{[R6RS] @var{Rtd} must be a record-type descriptor of a subtype of
@code{&condition}. The @code{condition-predicate} procedure returns a procedure
that takes one argument. This procedure returns #t if its argument is a condition
of the condition type represented by @var{rtd}, i.e., if it is either a simple
condition of that record type (or one of its subtypes) or a compound conditition
with such a simple condition as one of its components, and #f otherwise.
}

@define[Function]{@name{condition-accessor} @args{rtd proc}}
@desc{[R6RS] @var{Rtd} must be a record-type descriptor of a subtype of
@code{&condition}. @var{Proc} should accept one argument, a record of the
record type of @var{rtd}. The @code{condition-accessor} procedure returns a
procedure that accepts a single argument, which must be a condition of the type
represented by @var{rtd}. This procedure extracts the first component of the
condition of the type represented by @var{rtd}, and returns the result of
applying proc to that component.
}

@define[Macro]{@name{define-condition-type} @args{condition-type supertypes constructor predicate field-spec @dots{}}}
@desc{[R6RS] @var{Condition-type}, @var{supertypes}, @var{constructor}, and 
@var{predicate} must all be identifiers. Each @var{field-spec} must be of the form

@snipet{(@var{field} @var{accessor})}

where both @var{field} and @var{accessor} must be identifiers.

The @code{define-condition-type} form expands into a record-type definition for
a record type @var{condition-type}. The record type will be non-opaque, non-sealed,
and its fields will be immutable. It will have supertype has its parent type.
The remaining identifiers will be bound as follows:

@itemlist[
@item{@var{Constructor} is bound to a default constructor for the type : It accepts
one argument for each of the record type's complete set of fields (including parent
types, with the fields of the parent coming before those of the extension in the
arguments) and returns a condition object initialized to those arguments.}
@item{@var{Predicate} is bound to a predicate that identifies conditions of type
@var{condition-type} or any of its subtypes.}
@item{Each @var{accessor} is bound to a procedure that extracts the corresponding
field from a condition of type @var{condition-type}.}
]
}

@subsubsection{Standard condition types}

Hierarchy of standard condition types:

@codeblock{
+ &condition
    + &warning
    + &serious
          + &error
          + &violation
                + &assertion
                + &non-continuable
                + &implementation-restriction
                + &lexical
                + &syntax
                + &undefined
    + &message
    + &irritants
}

@define["Condition Type"]{@name{&message}}
@define[Function]{@name{make-message-condition} @args{message}}
@define[Function]{@name{message-condition?} @args{obj}}
@define[Function]{@name{condition-message} @args{condition}}
@desc{[R6RS] It carries a message further describing the nature of the condition
to humans.}

@define["Condition Type"]{@name{&warning}}
@define[Function]{@name{make-warning}}
@define[Function]{@name{warning?} @args{obj}}
@desc{[R6RS] This type describes conditions that do not, in principle, prohibit
immediate continued execution of the program, but may interfere with the program's
execution later.}

@define["Condition Type"]{@name{&serious}}
@define[Function]{@name{make-serious-condition}}
@define[Function]{@name{serious-condition?} @args{obj}}
@desc{[R6RS] This type describes conditions serious enough that they cannot safely
be ignored. This condition type is primarily intended as a supertype of other
condition types.}

@define["Condition Type"]{@name{&error}}
@define[Function]{@name{make-error}}
@define[Function]{@name{error?} @args{obj}}
@desc{[R6RS] This type describes errors, typically caused by something that has
gone wrong in the interaction of the program with the external world or the user.}

@define["Condition Type"]{@name{&violation}}
@define[Function]{@name{make-violation}}
@define[Function]{@name{violation?} @args{obj}}
@desc{[R6RS] This type describes violations of the language standard or a library
standard, typically caused by a programming error.}

@define["Condition Type"]{@name{&assertion}}
@define[Function]{@name{make-assertion-violation}}
@define[Function]{@name{assertion-violation?} @args{obj}}
@desc{[R6RS] This type describes an invalid call to a procedure, either passing
an invalid number of arguments, or passing an argument of the wrong type.}

@define["Condition Type"]{@name{&irritants}}
@define[Function]{@name{make-irritants-condition} @args{irritants}}
@define[Function]{@name{irritants-condition?} @args{obj}}
@define[Function]{@name{condition-irritants} @args{condition}}
@desc{[R6RS] @var{Irritants} should be a list of objects. This condition provides
additional information about a condition, typically the argument list of a
procedure that detected an exception. Conditions of this type are created by the
@code{error} and @code{assertion-violation} procedures.}

@define["Condition Type"]{@name{&who}}
@define[Function]{@name{make-who-condition} @args{who}}
@define[Function]{@name{who-condition?} @args{obj}}
@define[Function]{@name{condition-who} @args{condition}}
@desc{[R6RS] @var{Who} should be a symbol or string identifying the entity
reporting the exception. Conditions of this type are created by the @code{error}
and @code{assertion-violation} procedures, and the @code{syntax-violation}
procedure.}

@define["Condition Type"]{@name{&non-continuable}}
@define[Function]{@name{make-non-continuable-violation}}
@define[Function]{@name{non-continuable-violation?} @args{obj}}
@desc{[R6RS] This type indicates that an exception handler invoked via raise has
returned.}

@define["Condition Type"]{@name{&implementation-restriction}}
@define[Function]{@name{make-implementation-restriction-violation}}
@define[Function]{@name{implementation-restriction-violation?} @args{obj}}
@desc{[R6RS] This type describes a violation of an implementation restriction
allowed by the specification, such as the absence of representations for NaNs
and infinities.}

@define["Condition Type"]{@name{&lexical}}
@define[Function]{@name{make-lexical-violation}}
@define[Function]{@name{lexical-violation?} @args{obj}}
@desc{[R6RS] This type describes syntax violations at the level of the datum syntax.}

@define["Condition Type"]{@name{&syntax}}
@define[Function]{@name{make-syntax-violation} @args{form subform}}
@define[Function]{@name{syntax-violation?} @args{obj}}
@define[Function]{@name{syntax-violation-form} @args{condition}}
@define[Function]{@name{syntax-violation-subform} @args{condition}}
@desc{[R6RS] This type describes syntax violations. @var{Form} should be the
erroneous syntax object or a datum representing the code of the erroneous form.
@var{Subform} should be an optional syntax object or datum within the erroneous
form that more precisely locates the violation. It can be #f to indicate the
absence of more precise information.
}

@define["Condition Type"]{@name{&undefined}}
@define[Function]{@name{make-undefined-violation}}
@define[Function]{@name{undefined-violation?} @args{obj}}
@desc{[R6RS] This type describes unbound identifiers in the program.}
