@; -*- mode: scribble; coding: utf-8; -*-

@subsection[:tag "rnrs.enums.6"]{Enumerations}

This section describes the @code{(rnrs enums (6))}library for dealing with
enumerated values and sets of enumerated values. Enumerated values are represented
by ordinary symbols, while finite sets of enumerated values form a separate type,
known as the @var{enumeration sets}. The enumeration sets are further partitioned
into sets that share the same @var{universe} and @var{enumeration type}. These
universes and enumeration types are created by the @code{make-enumeration}
procedure. Each call to that procedure creates a new enumeration type.

@define[Library]{@name{(rnrs enums (6))}}
@desc{[R6RS] This library interprets each enumeration set with respect to its
specific universe of symbols and enumeration type. This facilitates efficient
implementation of enumeration sets and enables the complement operation.
}

In the descriptions of the following procedures, @var{enum-set} ranges over
the enumeration sets, which are defined as the subsets of the universes that
can be defined using make-enumeration.

@define[Function]{@name{make-enumeration} @args{symbol-list}}
@desc{[R6RS] @var{Symbol-list} must be a list of symbols.

The @code{make-enumeration} procedure creates a new enumeration type whose
universe consists of those symbols (in canonical order of their first appearance
in the list) and returns that universe as an enumeration set whose universe is
itself and whose enumeration type is the newly created enumeration type.
}

@define[Function]{@name{enum-set-universe} @args{enum-set}}
@desc{[R6RS] Returns the set of all symbols that comprise the universe of its
argument, as an enumeration set.
}

@define[Function]{@name{enum-set-indexer} @args{enum-set}}
@desc{[R6RS] Returns a unary procedure that, given a symbol that is in the
universe of @var{enum-set}, returns its 0-origin index within the canonical
ordering of the symbols in the universe; given a value not in the universe,
the unary procedure returns #f.
}

@define[Function]{@name{enum-set-constructor} @args{enum-set}}
@desc{[R6RS] Returns a unary procedure that, given a list of symbols that belong
to the universe of @var{enum-set}, returns a subset of that universe that contains
exactly the symbols in the list. The values in the list must all belong to the
universe.
}

@define[Function]{@name{enum-set->list} @args{enum-set}}
@desc{[R6RS] Returns a list of the symbols that belong to its argument, in the
canonical order of the universe of @var{enum-set}.
}

@define[Function]{@name{enum-set-member} @args{symbol enum-set}}
@define[Function]{@name{enum-set-subst?} @args{enum-set1 enum-set2}}
@define[Function]{@name{enum-set=?} @args{enum-set1 enum-set2}}
@desc{[R6RS] The @code{enum-set-member?} procedure returns #t if its first
argument is an element of its second argument, #f otherwise.

The @code{enum-set-subset?} procedure returns #t if the universe of
@var{enum-set1} is a subset of the universe of @var{enum-set2} (considered as
sets of symbols) and every element of @var{enum-set1} is a member of
@var{enum-set2}. It returns #f otherwise.

The @code{enum-set=?} procedure returns #t if @var{enum-set1} is a subset of
@var{enum-set2} and vice versa, as determined by the @code{enum-set-subset?}
procedure. This implies that the universes of the two sets are equal as sets of
symbols, but does not imply that they are equal as enumeration types. Otherwise,
#f is returned.
}

@define[Function]{@name{enum-set-union} @args{enum-set1 enum-set2}}
@define[Function]{@name{enum-set-intersection} @args{enum-set1 enum-set2}}
@define[Function]{@name{enum-set-difference} @args{enum-set1 enum-set2}}
@desc{[R6RS] @var{Enum-set1} and @var{enum-set2} must be enumeration sets that
have the same enumeration type.

The @code{enum-set-union} procedure returns the union of @var{enum-set1} and
@var{enum-set2}.

The @code{enum-set-intersection} procedure returns the intersection of
@var{enum-set1} and @var{enum-set2}.

The @code{enum-set-difference} procedure returns the difference of @var{enum-set1}
and @var{enum-set2}.
}

@define[Function]{@name{enum-set-complement} @args{enum-set}}
@desc{[R6RS] Returns @var{enum-set}'s complement with respect to its universe.}

@define[Function]{@name{enum-set-projection} @args{enum-set1 enum-set2}}
@desc{[R6RS] Projects @var{enum-set1} into the universe of @var{enum-set2},
dropping any elements of @var{enum-set1} that do not belong to the universe of
@var{enum-set2}. (If @var{enum-set1} is a subset of the universe of its second,
no elements are dropped, and the injection is returned.)
}

@define[Macro]{@name{define-enumeration}
 @args{type-name (symbol @dots{}) constructor-syntax}}
@desc{[R6RS] The @code{define-enumeration} form defines an enumeration type and
provides two macros for constructing its members and sets of its members.

A @code{define-enumeration} form is a definition and can appear anywhere any
other definition can appear.

@var{Type-name} is an identifier that is bound as a syntactic keyword; 
@var{symbol @dots{}} are the symbols that comprise the universe of the enumeration
(in order).

@code{(@var{type-name} @var{symbol})} checks at macro-expansion time whether the
name of @var{symbol} is in the universe associated with @var{type-name}. If it is, 
@code{(@var{type-name} @var{symbol})} is equivalent to symbol. It is a syntax
violation if it is not.

@var{Constructor-syntax} is an identifier that is bound to a macro that, given any
finite sequence of the symbols in the universe, possibly with duplicates, expands
into an expression that evaluates to the enumeration set of those symbols.

@code{(@var{constructor-syntax} @var{symbol @dots{}})} checks at macro-expansion
time whether every @var{symbol @dots{}} is in the universe associated with
@var{type-name}. It is a syntax violation if one or more is not. Otherwise

@snipet{(@var{constructor-syntax} @var{symbol @dots{}})}

is equivalent to

@codeblock{
((enum-set-constructor (@var{constructor-syntax}))
 '(@var{symbol @dots{}}))
}.
}