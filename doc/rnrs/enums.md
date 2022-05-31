[§2] Enumerations {#rnrs.enums.6}
-------------

This section describes the `(rnrs enums (6))`library for dealing with
enumerated values and sets of enumerated values. Enumerated values are represented
by ordinary symbols, while finite sets of enumerated values form a separate type,
known as the _enumeration sets_. The enumeration sets are further partitioned
into sets that share the same _universe_ and _enumeration type_. These
universes and enumeration types are created by the `make-enumeration`procedure. Each call to that procedure creates a new enumeration type.

###### [!Library] `(rnrs enums (6))` 

[R6RS] This library interprets each enumeration set with respect to its
specific universe of symbols and enumeration type. This facilitates efficient
implementation of enumeration sets and enables the complement operation.


In the descriptions of the following procedures, _enum-set_ ranges over
the enumeration sets, which are defined as the subsets of the universes that
can be defined using make-enumeration.

###### [!Function] `make-enumeration`  _symbol-list_

[R6RS] _Symbol-list_ must be a list of symbols.

The `make-enumeration` procedure creates a new enumeration type whose
universe consists of those symbols (in canonical order of their first appearance
in the list) and returns that universe as an enumeration set whose universe is
itself and whose enumeration type is the newly created enumeration type.


###### [!Function] `enum-set-universe`  _enum-set_

[R6RS] Returns the set of all symbols that comprise the universe of its
argument, as an enumeration set.


###### [!Function] `enum-set-indexer`  _enum-set_

[R6RS] Returns a unary procedure that, given a symbol that is in the
universe of _enum-set_, returns its 0-origin index within the canonical
ordering of the symbols in the universe; given a value not in the universe,
the unary procedure returns #f.


###### [!Function] `enum-set-constructor`  _enum-set_

[R6RS] Returns a unary procedure that, given a list of symbols that belong
to the universe of _enum-set_, returns a subset of that universe that contains
exactly the symbols in the list. The values in the list must all belong to the
universe.


###### [!Function] `enum-set->list`  _enum-set_

[R6RS] Returns a list of the symbols that belong to its argument, in the
canonical order of the universe of _enum-set_.


###### [!Function] `enum-set-member`  _symbol_ _enum-set_
###### [!Function] `enum-set-subst?`  _enum-set1_ _enum-set2_
###### [!Function] `enum-set=?`  _enum-set1_ _enum-set2_

[R6RS] The `enum-set-member?` procedure returns #t if its first
argument is an element of its second argument, #f otherwise.

The `enum-set-subset?` procedure returns #t if the universe of
_enum-set1_ is a subset of the universe of _enum-set2_ (considered as
sets of symbols) and every element of _enum-set1_ is a member of
_enum-set2_. It returns #f otherwise.

The `enum-set=?` procedure returns #t if _enum-set1_ is a subset of
_enum-set2_ and vice versa, as determined by the `enum-set-subset?`procedure. This implies that the universes of the two sets are equal as sets of
symbols, but does not imply that they are equal as enumeration types. Otherwise,
#f is returned.


###### [!Function] `enum-set-union`  _enum-set1_ _enum-set2_
###### [!Function] `enum-set-intersection`  _enum-set1_ _enum-set2_
###### [!Function] `enum-set-difference`  _enum-set1_ _enum-set2_

[R6RS] _Enum-set1_ and _enum-set2_ must be enumeration sets that
have the same enumeration type.

The `enum-set-union` procedure returns the union of _enum-set1_ and
_enum-set2_.

The `enum-set-intersection` procedure returns the intersection of
_enum-set1_ and _enum-set2_.

The `enum-set-difference` procedure returns the difference of _enum-set1_and _enum-set2_.


###### [!Function] `enum-set-complement`  _enum-set_

[R6RS] Returns _enum-set_'s complement with respect to its universe.

###### [!Function] `enum-set-projection`  _enum-set1_ _enum-set2_

[R6RS] Projects _enum-set1_ into the universe of _enum-set2_,
dropping any elements of _enum-set1_ that do not belong to the universe of
_enum-set2_. (If _enum-set1_ is a subset of the universe of its second,
no elements are dropped, and the injection is returned.)


###### [!Macro] `define-enumeration`  _type-name_ _(symbol_ _..._ _)_ _constructor-syntax_

[R6RS] The `define-enumeration` form defines an enumeration type and
provides two macros for constructing its members and sets of its members.

A `define-enumeration` form is a definition and can appear anywhere any
other definition can appear.

_Type-name_ is an identifier that is bound as a syntactic keyword; 
_symbol ..._ are the symbols that comprise the universe of the enumeration
(in order).

`(_type-name_ _symbol_)` checks at macro-expansion time whether the
name of _symbol_ is in the universe associated with _type-name_. If it is, 
`(_type-name_ _symbol_)` is equivalent to symbol. It is a syntax
violation if it is not.

_Constructor-syntax_ is an identifier that is bound to a macro that, given any
finite sequence of the symbols in the universe, possibly with duplicates, expands
into an expression that evaluates to the enumeration set of those symbols.

`(_constructor-syntax_ _symbol ..._)` checks at macro-expansion
time whether every _symbol ..._ is in the universe associated with
_type-name_. It is a syntax violation if one or more is not. Otherwise

``(_constructor-syntax_ _symbol ..._)``

is equivalent to

``````````scheme
((enum-set-constructor (_constructor-syntax_))
 '(_symbol ..._))
``````````

.


