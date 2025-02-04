[§2] (text sxml sxpath) - Functional XML parser {#ported.text.sxml.sxpath}
-------------

###### [!Library] `(text sxml sxpath)` 

This library is ported from Kirill Lisovsky's SXPath which is based on
Oleg Kiselyov's SXPath. The documents are from the original file.

SXPath is a query language for SXML, an instance of XML Information
set (Infoset) in the form of s-expressions. See SSAX.scm for the
definition of SXML and more details. SXPath is also a translation into
Scheme of an XML Path Language, XPath:

> http://www.w3.org/TR/xpath

XPath and SXPath describe means of selecting a set of Infoset's items
or their properties.

To facilitate queries, XPath maps the XML Infoset into an explicit
tree, and introduces important notions of a location path and a
current, context node. A location path denotes a selection of a set of
nodes relative to a context node. Any XPath tree has a distinguished,
root node -- which serves as the context node for absolute location
paths. Location path is recursively defined as a location step joined
with a location path. A location step is a simple query of the
database relative to a context node. A step may include expressions
that further filter the selected set. Each node in the resulting set
is used as a context node for the adjoining location path. The result
of the step is a union of the sets returned by the latter location
paths.

The SXML representation of the XML Infoset (see SSAX.scm) is rather
suitable for querying as it is. Bowing to the XPath specification,
we will refer to SXML information items as 'Nodes':

``````````scheme
	<Node> ::= <Element> | <attributes-coll> | <attrib>
		   | "text string" | <PI>
``````````

This production can also be described as

``````````scheme
	<Node> ::= (name . <Nodelist>) | "text string"
``````````

An (ordered) set of nodes is just a list of the constituent nodes:

``````````scheme
	<Nodelist> ::= (<Node> ...)
``````````

Nodelists, and Nodes other than text strings are both lists. A
\<Nodelist> however is either an empty list, or a list whose head is not
a symbol.  A symbol at the head of a node is either an XML name (in
which case it's a tag of an XML element), or an administrative name
such as '@'.  This uniform list representation makes processing rather
simple and elegant, while avoiding confusion. The multi-branch tree
structure formed by the mutually-recursive datatypes \<Node> and
\<Nodelist> lends itself well to processing by functional languages.

A location path is in fact a composite query over an XPath tree or
its branch. A singe step is a combination of a projection, selection
or a transitive closure. Multiple steps are combined via join and
union operations. This insight allows us to _elegantly_ implement
XPath as a sequence of projection and filtering primitives --
converters -- joined by _combinators_. Each converter takes a node
and returns a nodelist which is the result of the corresponding query
relative to that node. A converter can also be called on a set of
nodes. In that case it returns a union of the corresponding queries over
each node in the set. The union is easily implemented as a list
append operation as all nodes in a SXML tree are considered
distinct, by XPath conventions. We also preserve the order of the
members in the union. Query combinators are high-order functions:
they take converter(s) (which is a Node|Nodelist -> Nodelist function)
and compose or otherwise combine them. We will be concerned with
only relative location paths [XPath]: an absolute location path is a
relative path applied to the root node.

Similarly to XPath, SXPath defines full and abbreviated notations
for location paths. In both cases, the abbreviated notation can be
mechanically expanded into the full form by simple rewriting
rules. In case of SXPath the corresponding rules are given as
comments to a sxpath function, below. The regression test suite at
the end of this file shows a representative sample of SXPaths in
both notations, juxtaposed with the corresponding XPath
expressions. Most of the samples are borrowed literally from the
XPath specification, while the others are adjusted for our running
example, tree1.


### [§3] Basic converters and applicators

A converter is a function

``````````scheme
	type Converter = Node|Nodelist -> Nodelist
``````````

A converter can also play a role of a predicate: in that case, if a
converter, applied to a node or a nodelist, yields a non-empty
nodelist, the converter-predicate is deemed satisfied. Throughout
this file a nil nodelist is equivalent to #f in denoting a failure.

###### [!Function] `nodeset?`  _x_

Returns #t if given object is a nodelist

###### [!Function] `as-nodeset`  _x_

If x is a nodelist - returns it as is, otherwise wrap it in a list.

###### [!Function] `sxml:element?`  _x_

Predicate which returns #t if \<obj> is SXML element, otherwise returns #f.


###### [!Function] `ntype-names??`  _crit_

The function ntype-names?? takes a list of acceptable node names as a
criterion and returns a function, which, when applied to a node, 
will return #t if the node name is present in criterion list and #f
otherwise.

``````````scheme
	ntype-names?? :: ListOfNames -> Node -> Boolean
``````````



###### [!Function] `ntype-names??`  _crit_

The function ntype?? takes a type criterion and returns
a function, which, when applied to a node, will tell if the node satisfies
the test.

``````````scheme
	ntype?? :: Crit -> Node -> Boolean
``````````

The criterion 'crit' is 
one of the following symbols:

id
: tests if the Node has the right name (id)

@
: tests if the Node is an \<attributes-list>

\*
: tests if the Node is an \<Element>

\*text\*
: tests if the Node is a text node

\*data\*
: tests if the Node is a data node (text, number, boolean,
     		   etc., but not pair)

\*PI\*
: tests if the Node is a PI node

\*COMMENT\*
: tests if the Node is a COMMENT node

\*ENTITY\*
: tests if the Node is a ENTITY node

\*any\*
: #t for any type of Node



###### [!Function] `ntype-namespace-id??`  _ns-id_

This function takes a namespace-id, and returns a predicate
Node -> Boolean, which is #t for nodes with this very namespace-id.
_ns-id_ is a string

`(ntype-namespace-id?? #f)` will be #t for nodes with non-qualified names.


###### [!Function] `sxml:complement`  _pred_

This function takes a predicate and returns it complemented 
That is if the given predicate yelds #f or '() the complemented one  
yields the given node (#t) and vice versa.


###### [!Function] `node-eq?`  _other_
###### [!Function] `node-equal?`  _other_

Curried equivalence converter-predicates

###### [!Function] `mode-pos`  _n_

node-pos:: N -> Nodelist -> Nodelist, or

node-pos:: N -> Converter

Select the N'th element of a Nodelist and return as a singular Nodelist;
Return an empty nodelist if the Nth element does not exist.

((node-pos 1) Nodelist) selects the node at the head of the Nodelist,
if exists; ((node-pos 2) Nodelist) selects the Node after that, if
exists.

N can also be a negative number: in that case the node is picked from
the tail of the list.

((node-pos -1) Nodelist) selects the last node of a non-empty nodelist;
((node-pos -2) Nodelist) selects the last but one node, if exists.


###### [!Function] `sxml:filter`  _pred?_

filter:: Converter -> Converter

A filter applicator, which introduces a filtering context. The argument
converter is considered a predicate, with either #f or nil result meaning
failure.


###### [!Function] `take-until`  _pred?_

take-until:: Converter -> Converter, or

take-until:: Pred -> Node|Nodelist -> Nodelist

Given a converter-predicate and a nodelist, apply the predicate to
each element of the nodelist, until the predicate yields anything but #f or
nil. Return the elements of the input nodelist that have been processed
till that moment (that is, which fail the predicate).

take-until is a variation of the filter above: take-until passes
elements of an ordered input set till (but not including) the first
element that satisfies the predicate.

The nodelist returned by ((take-until (not pred)) nset) is a subset -- 
to be more precise, a prefix -- of the nodelist returned by
((filter pred) nset)


###### [!Function] `take-after`  _pred?_

take-after:: Converter -> Converter, or

take-after:: Pred -> Node|Nodelist -> Nodelist

Given a converter-predicate and a nodelist, apply the predicate to
each element of the nodelist, until the predicate yields anything but #f or
nil. Return the elements of the input nodelist that have not been processed:
that is, return the elements of the input nodelist that follow the first
element that satisfied the predicate.

take-after along with take-until partition an input nodelist into three
parts: the first element that satisfies a predicate, all preceding
elements and all following elements.


###### [!Function] `map-union`  _proc_ _lst_

Apply proc to each element of lst and return the list of results.
if proc returns a nodelist, splice it into the result

From another point of view, map-union is a function Converter->Converter,
which places an argument-converter in a joining context.


###### [!Function] `node-reverse`  _node-or-nodelist_

node-reverse :: Converter, or

node-reverse:: Node|Nodelist -> Nodelist

Reverses the order of nodes in the nodelist
This basic converter is needed to implement a reverse document order
(see the XPath Recommendation).


###### [!Function] `node-trace`  _title_

node-trace:: String -> Converter

(node-trace title) is an identity converter. In addition it prints out
a node or nodelist it is applied to, prefixed with the 'title'.
This converter is very useful for debugging.


### [§3] Converter combinators

Combinators are higher-order functions that transmogrify a converter
or glue a sequence of converters into a single, non-trivial
converter. The goal is to arrive at converters that correspond to
XPath location paths.

From a different point of view, a combinator is a fixed, named
_pattern_ of applying converters. Given below is a complete set of
such patterns that together implement XPath location path
specification. As it turns out, all these combinators can be built
from a small number of basic blocks: regular functional composition,
map-union and filter applicators, and the nodelist union.

###### [!Function] `select-kids`  _test-pred?_

select-kids:: Pred -> Node -> Nodelist

Given a Node, return an (ordered) subset its children that satisfy
the Pred (a converter, actually)

select-kids:: Pred -> Nodelist -> Nodelist

The same as above, but select among children of all the nodes in
the Nodelist

More succinctly, the signature of this function is
select-kids:: Converter -> Converter


###### [!Function] `node-self`  _pred?_

node-self:: Pred -> Node -> Nodelist, or

node-self:: Converter -> Converter

Similar to select-kids but apply to the Node itself rather
than to its children. The resulting Nodelist will contain either one
component, or will be empty (if the Node failed the Pred).


###### [!Function] `node-join`  _selectors_ _..._

node-join:: [LocPath] -> Node|Nodelist -> Nodelist, or

node-join:: [Converter] -> Converter

join the sequence of location steps or paths as described
in the title comments above.


###### [!Function] `node-reduce`  _converters_ _..._

node-reduce:: [LocPath] -> Node|Nodelist -> Nodelist, or

node-reduce:: [Converter] -> Converter

A regular functional composition of converters.
From a different point of view,
   ``((apply node-reduce converters) nodelist)``

is equivalent to
   ``(foldl apply nodelist converters)``

i.e., folding, or reducing, a list of converters with the nodelist
as a seed.


###### [!Function] `node-or`  _converters_ _..._

node-or:: [Converter] -> Converter

This combinator applies all converters to a given node and
produces the union of their results.

This combinator corresponds to a union, '|' operation for XPath
location paths.


###### [!Function] `node-closure`  _test-pred_

node-closure:: Converter -> Converter

Select all _descendants_ of a node that satisfy a converter-predicate.
This combinator is similar to select-kids but applies to
grand... children as well.
This combinator implements the "descendant::" XPath axis
Conceptually, this combinator can be expressed as

``````````scheme
(define (node-closure f)
     (node-or
       (select-kids f)
	 (node-reduce (select-kids (ntype?? '*)) (node-closure f))))
``````````

This definition, as written, looks somewhat like a fixpoint, and it
will run forever. It is obvious however that sooner or later
(select-kids (ntype?? '\*)) will return an empty nodelist. At
this point further iterations will no longer affect the result and
can be stopped.


### [§3] Extensions

###### [!Function] `sxml:node? node` 

According to XPath specification 2.3, this test is true for any
XPath node.

For SXML auxiliary lists and lists of attributes has to be excluded.


###### [!Function] `sxml:attr-list`  _obj_

Returns the list of attributes for a given SXML node
Empty list is returned if the given node os not an element,
or if it has no list of attributes


###### [!Function] `sxml:attribute`  _test-pred?_

Attribute axis

###### [!Function] `sxml:child`  _test-pred?_

Child axis

This function is similar to 'select-kids', but it returns an empty
child-list for PI, Comment and Entity nodes


###### [!Function] `sxml:parent`  _test-pred?_

Parent axis
Given a predicate, it returns a function

 RootNode -> Converter

which which yields a 

 node -> parent 

converter then applied to a rootnode.
Thus, such a converter may be constructed using
 ``((sxml:parent test-pred) rootnode)``

and returns a parent of a node it is applied to.
If applied to a nodelist, it returns the 
list of parents of nodes in the nodelist. The rootnode does not have
to be the root node of the whole SXML tree -- it may be a root node
of a branch of interest.
The parent:: axis can be used with any SXML node.


#### [§4] Popular short cuts 

###### [!Function] `sxml:child-nodes`  _test-pred?_

``(sxml:child sxml:node?)``



###### [!Function] `sxml:child-elements`  _test-pred?_

``(select-kids sxml:element)``



### [§3] Abbreviated SXPath

###### [!Function] `sxpath`  _path_ _:optional_ _ns-binding_

Evaluate an abbreviated SXPath

``````````scheme
	sxpath:: AbbrPath -> Converter, or
	sxpath:: AbbrPath -> Node|Nodeset -> Nodeset
``````````

AbbrPath is a list. It is translated to the full SXPath according
to the following rewriting rules

``````````scheme
(sxpath '()) -> (node-join)
(sxpath '(path-component ...)) ->
		(node-join (sxpath1 path-component) (sxpath '(...)))
(sxpath1 '//) -> (sxml:descendant-or-self sxml:node?)
(sxpath1 '(equal? x)) -> (select-kids (node-equal? x))
(sxpath1 '(eq? x))    -> (select-kids (node-eq? x))
(sxpath1 '(*or* ...))  -> (select-kids (ntype-names??
                                         (cdr '(*or* ...))))
(sxpath1 '(*not* ...)) -> (select-kids (sxml:complement 
                                        (ntype-names??
                                         (cdr '(*not* ...)))))
(sxpath1 '(ns-id:* x)) -> (select-kids 
                                     (ntype-namespace-id?? x))
(sxpath1 ?symbol)     -> (select-kids (ntype?? ?symbol))
(sxpath1 ?string)     -> (txpath ?string)
(sxpath1 procedure)   -> procedure
(sxpath1 '(?symbol ...)) -> (sxpath1 '((?symbol) ...))
(sxpath1 '(path reducer ...)) ->
		(node-reduce (sxpath path) (sxpathr reducer) ...)
(sxpathr number)      -> (node-pos number)
(sxpathr path-filter) -> (filter (sxpath path-filter))
``````````



#### [§4] Wrappers

###### [!Function] `if-sxpath`  _path_

sxpath always returns a list, which is #t in Scheme 
if-sxpath returns #f instead of empty list


###### [!Function] `if-car-sxpath`  _path_

Returns first node found, if any.
Otherwise returns #f.


###### [!Function] `car-sxpath`  _path_

Returns first node found, if any.
Otherwise returns empty list.


### [§3] lookup by a value of ID type attribute

###### [!Function] `sxml:id-alist`  _node_ _paths_ _..._

Built an index as a list of (ID_value . element) pairs for given
node. lpaths are location paths for attributes of type ID.


### [§3] SXML counterparts to W3C XPath Core Functions

###### [!Function] `sxml:string`  _object_

The counterpart to XPath 'string' function (section 4.2 XPath Rec.)
Converts a given object to a string

> NOTE:
>  1. When converting a nodeset - a document order is not preserved
>  2. number->string function returns the result in a form which is slightly

different from XPath Rec. specification


###### [!Function] `sxml:boolean`  _object_

The counterpart to XPath 'boolean' function (section 4.3 XPath Rec.)
Converts its argument to a boolean


###### [!Function] `sxml:number`  _object_

The counterpart to XPath 'number' function (section 4.4 XPath Rec.)
Converts its argument to a number

> NOTE: 
>  1. The argument is not optional (yet?)
>  2. string->number conversion is not IEEE 754 round-to-nearest
>  3. NaN is represented as 0



###### [!Function] `sxml:string-value`  _object_

Returns a string value for a given node in accordance to
XPath Rec. 5.1 - 5.7 


###### [!Function] `sxml:id`  _object_

Select SXML element by its unique IDs
XPath Rec. 4.1

> object - a nodeset or a datatype which can be converted to a string by means

of a 'string' function

> id-index = ( (id-value . element) (id-value . element) ... ) 

This index is used for selection of an element by its unique ID. 
The result is a nodeset


### [§3] Comparators for XPath objects 

#### [§4] Equality comparison

###### [!Function] `sxml:equality-cmp`  _bool-op_ _number-op_ _string-op_

A helper for XPath equality operations: = , !=
 'bool-op', 'number-op' and 'string-op' are comparison operations for 
a pair of booleans,  numbers and strings respectively


###### [!Function] `sxml:equal?`  _obj1_ _obj2_

(sxml:equality-cmp eq? = string=?)

Compares given _obj1_ and _obj2_.

###### [!Function] `sxml:not-equal?`  _obj1_ _obj2_

``````````scheme
(sxml:equality-cmp
   (lambda (bool1 bool2) (not (eq? bool1 bool2)))
   (lambda (num1 num2) (not (= num1 num2)))
   (lambda (str1 str2) (not (string=? str1 str2))))
``````````

Counterparts of `sxml:equal?`.


#### [§4] Relational comparison

###### [!Function] `sxml:relational-cmp`  _op_

Relational operation ( \< , > , \<= , >= ) for two XPath objects
 op is comparison procedure: \< , > , \<= or >=


### [§3] XPath axes. An order in resulting nodeset is preserved

###### [!Function] `sxml:ancestor`  _test-pred?_

Ancestor axis

###### [!Function] `sxml:ancestor-or-self`  _test-pred?_

Ancestor-or-self axis

###### [!Function] `sxml:descendant`  _test-pred?_

Descendant axis

It's similar to original 'node-closure' a resulting nodeset is 
in depth-first order rather than breadth-first
Fix: din't descend in non-element nodes!


###### [!Function] `sxml:descendant-or-self`  _test-pred?_

Descendant-or-self axis

###### [!Function] `sxml:following`  _test-pred?_

Following axis

###### [!Function] `sxml:following-sibling`  _test-pred?_

Following-sibling axis

###### [!Function] `sxml:namespace`  _test-pred?_

Namespace axis

###### [!Function] `sxml:preceding`  _test-pred?_

Preceding axis

###### [!Function] `sxml:preceding-sibling`  _test-pred?_

Preceding-sibling axis

