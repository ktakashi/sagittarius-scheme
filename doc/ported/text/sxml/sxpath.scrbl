@; -*- mode:scribble; coding: utf-8 -*-
@subsection[:tag "ported.text.sxml.sxpath"]{(text sxml sxpath) - Functional XML parser}

@define[Library]{@name{(text sxml sxpath)}}
@desc{This library is ported from Kirill Lisovsky's SXPath which is based on
Oleg Kiselyov's SXPath. The documents are from the original file.

SXPath is a query language for SXML, an instance of XML Information
set (Infoset) in the form of s-expressions. See SSAX.scm for the
definition of SXML and more details. SXPath is also a translation into
Scheme of an XML Path Language, XPath:
@blockquote{
	http://www.w3.org/TR/xpath
}
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
@codeblock{
	<Node> ::= <Element> | <attributes-coll> | <attrib>
		   | "text string" | <PI>
}
This production can also be described as
@codeblock{
	<Node> ::= (name . <Nodelist>) | "text string"
}
An (ordered) set of nodes is just a list of the constituent nodes:
@codeblock{
	<Nodelist> ::= (<Node> ...)
}
Nodelists, and Nodes other than text strings are both lists. A
<Nodelist> however is either an empty list, or a list whose head is not
a symbol.  A symbol at the head of a node is either an XML name (in
which case it's a tag of an XML element), or an administrative name
such as '@atmark{}'.  This uniform list representation makes processing rather
simple and elegant, while avoiding confusion. The multi-branch tree
structure formed by the mutually-recursive datatypes <Node> and
<Nodelist> lends itself well to processing by functional languages.

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
}

@subsubsection{Basic converters and applicators}

A converter is a function
@codeblock{
	type Converter = Node|Nodelist -> Nodelist
}
A converter can also play a role of a predicate: in that case, if a
converter, applied to a node or a nodelist, yields a non-empty
nodelist, the converter-predicate is deemed satisfied. Throughout
this file a nil nodelist is equivalent to #f in denoting a failure.

@define[Function]{@name{nodeset?} @args{x}}
@desc{Returns #t if given object is a nodelist}

@define[Function]{@name{as-nodeset} @args{x}}
@desc{If x is a nodelist - returns it as is, otherwise wrap it in a list.}

@define[Function]{@name{sxml:element?} @args{x}}
@desc{
Predicate which returns #t if <obj> is SXML element, otherwise returns #f.
}

@define[Function]{@name{ntype-names??} @args{crit}}
@desc{
The function ntype-names?? takes a list of acceptable node names as a
criterion and returns a function, which, when applied to a node, 
will return #t if the node name is present in criterion list and #f
otherwise.
@codeblock{
	ntype-names?? :: ListOfNames -> Node -> Boolean
}
}
@define[Function]{@name{ntype-names??} @args{crit}}
@desc{
The function ntype?? takes a type criterion and returns
a function, which, when applied to a node, will tell if the node satisfies
the test.
@codeblock{
	ntype?? :: Crit -> Node -> Boolean
}

The criterion 'crit' is 
one of the following symbols:
@dl-list{
  @dl-item["id"]{tests if the Node has the right name (id)}
  @dl-item["@"]{tests if the Node is an <attributes-list>}
  @dl-item["*"]{tests if the Node is an <Element>}
  @dl-item["*text*"]{tests if the Node is a text node}
  @dl-item["*data*"]{tests if the Node is a data node (text, number, boolean,
   		   etc., but not pair)}
  @dl-item["*PI*"]{tests if the Node is a PI node}
  @dl-item["*COMMENT*"]{tests if the Node is a COMMENT node}
  @dl-item["*ENTITY*"]{tests if the Node is a ENTITY node}
  @dl-item["*any*"]{#t for any type of Node}
}
}

@define[Function]{@name{ntype-namespace-id??} @args{ns-id}}
@desc{
This function takes a namespace-id, and returns a predicate
Node -> Boolean, which is #t for nodes with this very namespace-id.
@var{ns-id} is a string

@code{(ntype-namespace-id?? #f)} will be #t for nodes with non-qualified names.
}

@define[Function]{@name{sxml:complement} @args{pred}}
@desc{
This function takes a predicate and returns it complemented 
That is if the given predicate yelds #f or '() the complemented one  
yields the given node (#t) and vice versa.
}

@define[Function]{@name{node-eq?} @args{other}}
@define[Function]{@name{node-equal?} @args{other}}
@desc{Curried equivalence converter-predicates}

@define[Function]{@name{mode-pos} @args{n}}
@desc{
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
}

@define[Function]{@name{sxml:filter} @args{pred?}}
@desc{
filter:: Converter -> Converter

A filter applicator, which introduces a filtering context. The argument
converter is considered a predicate, with either #f or nil result meaning
failure.
}

@define[Function]{@name{take-until} @args{pred?}}
@desc{
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
}

@define[Function]{@name{take-after} @args{pred?}}
@desc{
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
}

@define[Function]{@name{map-union} @args{proc lst}}
@desc{
Apply proc to each element of lst and return the list of results.
if proc returns a nodelist, splice it into the result

From another point of view, map-union is a function Converter->Converter,
which places an argument-converter in a joining context.
}

@define[Function]{@name{node-reverse} @args{node-or-nodelist}}
@desc{
node-reverse :: Converter, or

node-reverse:: Node|Nodelist -> Nodelist

Reverses the order of nodes in the nodelist
This basic converter is needed to implement a reverse document order
(see the XPath Recommendation).
}

@define[Function]{@name{node-trace} @args{title}}
@desc{
node-trace:: String -> Converter

(node-trace title) is an identity converter. In addition it prints out
a node or nodelist it is applied to, prefixed with the 'title'.
This converter is very useful for debugging.
}

@subsubsection{Converter combinators}

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

@define[Function]{@name{select-kids} @args{test-pred?}}
@desc{
select-kids:: Pred -> Node -> Nodelist

Given a Node, return an (ordered) subset its children that satisfy
the Pred (a converter, actually)

select-kids:: Pred -> Nodelist -> Nodelist

The same as above, but select among children of all the nodes in
the Nodelist

More succinctly, the signature of this function is
select-kids:: Converter -> Converter
}

@define[Function]{@name{node-self} @args{pred?}}
@desc{
node-self:: Pred -> Node -> Nodelist, or

node-self:: Converter -> Converter

Similar to select-kids but apply to the Node itself rather
than to its children. The resulting Nodelist will contain either one
component, or will be empty (if the Node failed the Pred).
}

@define[Function]{@name{node-join} @args{selectors @dots{}}}
@desc{
node-join:: [LocPath] -> Node|Nodelist -> Nodelist, or

node-join:: [Converter] -> Converter

join the sequence of location steps or paths as described
in the title comments above.
}

@define[Function]{@name{node-reduce} @args{converters @dots{}}}
@desc{
node-reduce:: [LocPath] -> Node|Nodelist -> Nodelist, or

node-reduce:: [Converter] -> Converter

A regular functional composition of converters.
From a different point of view,
   @snipet{((apply node-reduce converters) nodelist)}
is equivalent to
   @snipet{(foldl apply nodelist converters)}
i.e., folding, or reducing, a list of converters with the nodelist
as a seed.
}

@define[Function]{@name{node-or} @args{converters @dots{}}}
@desc{
node-or:: [Converter] -> Converter

This combinator applies all converters to a given node and
produces the union of their results.

This combinator corresponds to a union, '|' operation for XPath
location paths.
}

@define[Function]{@name{node-closure} @args{test-pred}}
@desc{
node-closure:: Converter -> Converter

Select all _descendants_ of a node that satisfy a converter-predicate.
This combinator is similar to select-kids but applies to
grand... children as well.
This combinator implements the "descendant::" XPath axis
Conceptually, this combinator can be expressed as
@codeblock{
(define (node-closure f)
     (node-or
       (select-kids f)
	 (node-reduce (select-kids (ntype?? '*)) (node-closure f))))
}
This definition, as written, looks somewhat like a fixpoint, and it
will run forever. It is obvious however that sooner or later
(select-kids (ntype?? '*)) will return an empty nodelist. At
this point further iterations will no longer affect the result and
can be stopped.
}

@subsubsection{Extensions}

@define[Function]{@name{sxml:node? node}}
@desc{
According to XPath specification 2.3, this test is true for any
XPath node.

For SXML auxiliary lists and lists of attributes has to be excluded.
}

@define[Function]{@name{sxml:attr-list} @args{obj}}
@desc{
Returns the list of attributes for a given SXML node
Empty list is returned if the given node os not an element,
or if it has no list of attributes
}

@define[Function]{@name{sxml:attribute} @args{test-pred?}}
@desc{Attribute axis}

@define[Function]{@name{sxml:child} @args{test-pred?}}
@desc{
Child axis

This function is similar to 'select-kids', but it returns an empty
child-list for PI, Comment and Entity nodes
}

@define[Function]{@name{sxml:parent} @args{test-pred?}}
@desc{
Parent axis
Given a predicate, it returns a function

 RootNode -> Converter

which which yields a 

 node -> parent 

converter then applied to a rootnode.
Thus, such a converter may be constructed using
 @snipet{((sxml:parent test-pred) rootnode)}
and returns a parent of a node it is applied to.
If applied to a nodelist, it returns the 
list of parents of nodes in the nodelist. The rootnode does not have
to be the root node of the whole SXML tree -- it may be a root node
of a branch of interest.
The parent:: axis can be used with any SXML node.
}

@sub*section{Popular short cuts }

@define[Function]{@name{sxml:child-nodes} @args{test-pred?}}
@desc{@snipet{(sxml:child sxml:node?)}}

@define[Function]{@name{sxml:child-elements} @args{test-pred?}}
@desc{@snipet{(select-kids sxml:element)}}

@subsubsection{Abbreviated SXPath}

@define[Function]{@name{sxpath} @args{path :optional ns-binding}}
@desc{
Evaluate an abbreviated SXPath
@codeblock{
	sxpath:: AbbrPath -> Converter, or
	sxpath:: AbbrPath -> Node|Nodeset -> Nodeset
}
AbbrPath is a list. It is translated to the full SXPath according
to the following rewriting rules
@codeblock{
(sxpath '()) -> (node-join)
(sxpath '(path-component @dots{})) ->
		(node-join (sxpath1 path-component) (sxpath '(@dots{})))
(sxpath1 '//) -> (sxml:descendant-or-self sxml:node?)
(sxpath1 '(equal? x)) -> (select-kids (node-equal? x))
(sxpath1 '(eq? x))    -> (select-kids (node-eq? x))
(sxpath1 '(*or* @dots{}))  -> (select-kids (ntype-names??
                                         (cdr '(*or* @dots{}))))
(sxpath1 '(*not* @dots{})) -> (select-kids (sxml:complement 
                                        (ntype-names??
                                         (cdr '(*not* @dots{})))))
(sxpath1 '(ns-id:* x)) -> (select-kids 
                                     (ntype-namespace-id?? x))
(sxpath1 ?symbol)     -> (select-kids (ntype?? ?symbol))
(sxpath1 ?string)     -> (txpath ?string)
(sxpath1 procedure)   -> procedure
(sxpath1 '(?symbol @dots{})) -> (sxpath1 '((?symbol) @dots{}))
(sxpath1 '(path reducer @dots{})) ->
		(node-reduce (sxpath path) (sxpathr reducer) @dots{})
(sxpathr number)      -> (node-pos number)
(sxpathr path-filter) -> (filter (sxpath path-filter))
}
}

@sub*section{Wrappers}

@define[Function]{@name{if-sxpath} @args{path}}
@desc{
sxpath always returns a list, which is #t in Scheme 
if-sxpath returns #f instead of empty list
}

@define[Function]{@name{if-car-sxpath} @args{path}}
@desc{
Returns first node found, if any.
Otherwise returns #f.
}

@define[Function]{@name{car-sxpath} @args{path}}
@desc{
Returns first node found, if any.
Otherwise returns empty list.
}

@subsubsection{lookup by a value of ID type attribute}

@define[Function]{@name{sxml:id-alist} @args{node paths @dots{}}}
@desc{
Built an index as a list of (ID_value . element) pairs for given
node. lpaths are location paths for attributes of type ID.
}

@subsubsection{SXML counterparts to W3C XPath Core Functions}

@define[Functions]{@name{sxml:string} @args{object}}
@desc{
The counterpart to XPath 'string' function (section 4.2 XPath Rec.)
Converts a given object to a string
@blockquote{
NOTE:
 1. When converting a nodeset - a document order is not preserved
 2. number->string function returns the result in a form which is slightly
}
different from XPath Rec. specification
}

@define[Functions]{@name{sxml:boolean} @args{object}}
@desc{
The counterpart to XPath 'boolean' function (section 4.3 XPath Rec.)
Converts its argument to a boolean
}

@define[Functions]{@name{sxml:number} @args{object}}
@desc{
The counterpart to XPath 'number' function (section 4.4 XPath Rec.)
Converts its argument to a number
@blockquote{
NOTE: 
 1. The argument is not optional (yet?)
 2. string->number conversion is not IEEE 754 round-to-nearest
 3. NaN is represented as 0
}
}

@define[Functions]{@name{sxml:string-value} @args{object}}
@desc{
Returns a string value for a given node in accordance to
XPath Rec. 5.1 - 5.7 
}

@define[Functions]{@name{sxml:id} @args{object}}
@desc{
Select SXML element by its unique IDs
XPath Rec. 4.1
@blockquote{
 object - a nodeset or a datatype which can be converted to a string by means
}
of a 'string' function
@blockquote{
 id-index = ( (id-value . element) (id-value . element) @dots{} ) 
}
This index is used for selection of an element by its unique ID. 
The result is a nodeset
}

@subsubsection{Comparators for XPath objects }
@sub*section{Equality comparison}

@define[Function]{@name{sxml:equality-cmp} @args{bool-op number-op string-op}}
@desc{
A helper for XPath equality operations: = , !=
 'bool-op', 'number-op' and 'string-op' are comparison operations for 
a pair of booleans,  numbers and strings respectively
}

@define[Function]{@name{sxml:equal?} @args{obj1 obj2}}
@desc{(sxml:equality-cmp eq? = string=?)

Compares given @var{obj1} and @var{obj2}.}

@define[Function]{@name{sxml:not-equal?} @args{obj1 obj2}}
@desc{
@codeblock{
(sxml:equality-cmp
   (lambda (bool1 bool2) (not (eq? bool1 bool2)))
   (lambda (num1 num2) (not (= num1 num2)))
   (lambda (str1 str2) (not (string=? str1 str2))))
}

Counterparts of @code{sxml:equal?}.
}

@sub*section{Relational comparison}

@define[Function]{@name{sxml:relational-cmp} @args{op}}
@desc{
Relational operation ( < , > , <= , >= ) for two XPath objects
 op is comparison procedure: < , > , <= or >=
}

@subsubsection{XPath axes. An order in resulting nodeset is preserved}

@define[Function]{@name{sxml:ancestor} @args{test-pred?}}
@desc{Ancestor axis}

@define[Function]{@name{sxml:ancestor-or-self} @args{test-pred?}}
@desc{Ancestor-or-self axis}

@define[Function]{@name{sxml:descendant} @args{test-pred?}}
@desc{
Descendant axis

It's similar to original 'node-closure' a resulting nodeset is 
in depth-first order rather than breadth-first
Fix: din't descend in non-element nodes!
}

@define[Function]{@name{sxml:descendant-or-self} @args{test-pred?}}
@desc{Descendant-or-self axis}

@define[Function]{@name{sxml:following} @args{test-pred?}}
@desc{Following axis}

@define[Function]{@name{sxml:following-sibling} @args{test-pred?}}
@desc{Following-sibling axis}

@define[Function]{@name{sxml:namespace} @args{test-pred?}}
@desc{Namespace axis}

@define[Function]{@name{sxml:preceding} @args{test-pred?}}
@desc{Preceding axis}

@define[Function]{@name{sxml:preceding-sibling} @args{test-pred?}}
@desc{Preceding-sibling axis}

