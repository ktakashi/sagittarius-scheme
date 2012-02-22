@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "ported.text.sxml.ssax"]{(text sxml ssax) - Functional XML parser}

@define[Library]{@name{(text sxml ssax)}}
@desc{@code{(text sxml *)} libraries are the adaptation of Oleg Kiselyov's SXML
framework SSAX, which is based on S-expression representation of XML structure.

SSAX is a parser part of SXML framework.

This is a quote from SSAX webpage:
@blockquote{
The framework consists of a DOM/SXML parser, a SAX parser, and a supporting
library of lexing and parsing procedures. The procedures in the package can be
used separately to tokenize or parse various pieces of XML documents. The
framework supports XML Namespaces, character, internal and external parsed
entities, xml:space, attribute value normalization, processing instructions and
CDATA sections. The package includes a semi-validating SXML parser: a DOM-mode
parser that is an instantiation of a SAX parser (called SSAX).

The parsing framework offers support for XML validation, to the full or any
user-specified degree. Framework's procedures by themselves detect great many
validation errors and almost all well-formedness errors. Furthermore, a user is
given a chance to set his own handlers to capture the rest of the errors.
Content is validated given user-specified constraints, which the user can derive
from a DTD, from an XML schema, or from other competing doctype specification
formats.

SSAX is a full-featured, algorithmically optimal, pure-functional parser, which
can act as a stream processor. SSAX is an efficient SAX parser that is easy to
use. SSAX minimizes the amount of application-specific state that has to be
shared among user-supplied event handlers. SSAX makes the maintenance of an
application-specific element stack unnecessary, which eliminates several classes
of common bugs. SSAX is written in a pure-functional subset of Scheme. Therefore,
the event handlers are referentially transparent, which makes them easier for a
programmer to write and to reason about. The more expressive, reliable and easier
to use application interface for the event-driven XML parsing is the outcome of
implementing the parsing engine as an enhanced tree fold combinator, which fully
captures the control pattern of the depth-first tree traversal.
}
Sagittarius supports the latest version of SSAX 5.1.
}

All procedures and macros are described bottom up. So you might be interested
only in user level APIs. If so, 
see @secref["ssax.user.api"]{Highest-level parsers: XML to SXML}.


@subsubsection{Introduction}

I derived the content of this part of the manual from SSAX source code, just by
converting its comments into this manual format. The original text is by Oleg
Kiselyov.

This is a package of low-to-high level lexing and parsing procedures
that can be combined to yield a SAX, a DOM, a validating parsers, or
a parser intended for a particular document type. The procedures in
the package can be used separately to tokenize or parse various
pieces of XML documents. The package supports XML Namespaces,
internal and external parsed entities, user-controlled handling of
whitespace, and validation. This module therefore is intended to be
a framework, a set of "Lego blocks" you can use to build a parser
following any discipline and performing validation to any degree. As
an example of the parser construction, this file includes a
semi-validating SXML parser.

The present XML framework has a "sequential" feel of SAX yet a
"functional style" of DOM. Like a SAX parser, the framework scans
the document only once and permits incremental processing. An
application that handles document elements in order can run as
efficiently as possible. _Unlike_ a SAX parser, the framework does
not require an application register stateful callbacks and surrender
control to the parser. Rather, it is the application that can drive
the framework -- calling its functions to get the current lexical or
syntax element. These functions do not maintain or mutate any state
save the input port. Therefore, the framework permits parsing of XML
in a pure functional style, with the input port being a monad (or a
linear, read-once parameter).

Besides the PORT, there is another monad -- SEED. Most of the
middle- and high-level parsers are single-threaded through the
seed. The functions of this framework do not process or affect the
SEED in any way: they simply pass it around as an instance of an
opaque datatype.  User functions, on the other hand, can use the
seed to maintain user's state, to accumulate parsing results, etc. A
user can freely mix his own functions with those of the
framework. On the other hand, the user may wish to instantiate a
high-level parser: ssax:make-elem-parser or ssax:make-parser.  In
the latter case, the user must provide functions of specific
signatures, which are called at predictable moments during the
parsing: to handle character data, element data, or processing
instructions (PI). The functions are always given the SEED, among
other parameters, and must return the new SEED.

From a functional point of view, XML parsing is a combined
pre-post-order traversal of a "tree" that is the XML document
itself. This down-and-up traversal tells the user about an element
when its start tag is encountered. The user is notified about the
element once more, after all element's children have been
handled. The process of XML parsing therefore is a fold over the
raw XML document. Unlike a fold over trees defined in [1], the
parser is necessarily single-threaded -- obviously as elements
in a text XML document are laid down sequentially. The parser
therefore is a tree fold that has been transformed to accept an
accumulating parameter [1,2].

Formally, the denotational semantics of the parser can be expressed
as
@codeblock{
parser:: (Start-tag -> Seed -> Seed) ->
	 (Start-tag -> Seed -> Seed -> Seed) ->
	 (Char-Data -> Seed -> Seed) ->
	 XML-text-fragment -> Seed -> Seed

parser fdown fup fchar "<elem attrs> content </elem>" seed
 = fup "<elem attrs>" seed
	(parser fdown fup fchar "content" (fdown "<elem attrs>" seed))

parser fdown fup fchar "char-data content" seed
 = parser fdown fup fchar "content" (fchar "char-data" seed)

parser fdown fup fchar "elem-content content" seed
 = parser fdown fup fchar "content" (
	parser fdown fup fchar "elem-content" seed)
}
Compare the last two equations with the left fold
fold-left kons elem:list seed = fold-left kons list (kons elem seed)

The real parser created my ssax:make-parser is slightly more complicated,
to account for processing instructions, entity references, namespaces,
processing of document type declaration, etc.


The XML standard document referred to in this module is
@hyperlink[:href "http://www.w3.org/TR/1998/REC-xml-19980210.html"]{http://www.w3.org/TR/1998/REC-xml-19980210.html}

The present file also defines a procedure that parses the text of an
XML document or of a separate element into SXML, an
S-expression-based model of an XML Information Set. SXML is also an
Abstract Syntax Tree of an XML document. SXML is similar
but not identical to DOM; SXML is particularly suitable for
Scheme-based XML/HTML authoring, SXPath queries, and tree
transformations. See SXML.html for more details.
SXML is a term implementation of evaluation of the XML document [3].
The other implementation is context-passing.

The present frameworks fully supports the XML Namespaces Recommendation:
@hyperlink[:href "http://www.w3.org/TR/REC-xml-names/"]{http://www.w3.org/TR/REC-xml-names/}

Other links:

[1] Jeremy Gibbons, Geraint Jones, "The Under-appreciated Unfold,"
Proc. ICFP'98, 1998, pp. 273-279.

[2] Richard S. Bird, The promotion and accumulation strategies in
transformational programming, ACM Trans. Progr. Lang. Systems,
6(4):487-504, October 1984.

[3] Ralf Hinze, "Deriving Backtracking Monad Transformers,"
Functional Pearl. Proc ICFP'00, pp. 186-197.

@subsubsection{Data Types}

@dl-list[
@dl-item["TAG-KIND"]{
	a symbol @code{START}, @code{END}, @code{PI}, @code{DECL}, @code{COMMENT},
	@code{CDSECT} or @code{ENTITY-REF} that identifies a markup token
}
@dl-item["UNRES-NAME"]{
	a name (called GI in the XML Recommendation) as given in an xml
	document for a markup token: start-tag, PI target, attribute name.
	If a GI is an NCName, UNRES-NAME is this NCName converted into
	a Scheme symbol. If a GI is a QName, UNRES-NAME is a pair of
	symbols: (PREFIX . LOCALPART)
}
@dl-item["RES-NAME"]{
	An expanded name, a resolved version of an UNRES-NAME.
	For an element or an attribute name with a non-empty namespace URI,
	RES-NAME is a pair of symbols, (URI-SYMB . LOCALPART).
	Otherwise, it's a single symbol.
}
@dl-item["ELEM-CONTENT-MODEL"]{
	A symbol:
	@dl-list[
	@dl-item["ANY"]{anything goes, expect an END tag.}
	@dl-item["EMPTY-TAG"]{no content, and no END-tag is coming}
	@dl-item["EMPTY"]{no content, expect the END-tag as the next token}
	@dl-item["PCDATA"]{expect character data only, and no children elements}
	@dl-item["MIXED"]{}
	@dl-item["ELEM-CONTENT"]{}
	]
}
@dl-item["URI-SYMB"]{
	A symbol representing a namespace URI -- or other symbol chosen
	by the user to represent URI. In the former case,
	URI-SYMB is created by %-quoting of bad URI characters and
	converting the resulting string into a symbol.
}
@dl-item["NAMESPACES"]{
	A list representing namespaces in effect. An element of the list
	has one of the following forms:
	@dl-list[
	@dl-item[2 "(PREFIX URI-SYMB . URI-SYMB)" "(PREFIX USER-PREFIX . URI-SYMB)"]{
		USER-PREFIX is a symbol chosen by the user
		to represent the URI.
	}
	@dl-item["(#f USER-PREFIX . URI-SYMB)"]{
		Specification of the user-chosen prefix and a URI-SYMBOL.}
	@dl-item["(*DEFAULT* USER-PREFIX . URI-SYMB)"]{
		Declaration of the default namespace}
	@dl-item["(*DEFAULT* #f . #f)"]{
		Un-declaration of the default namespace. This notation
		represents overriding of the previous declaration}
	]
	A NAMESPACES list may contain several elements for the same PREFIX.
	The one closest to the beginning of the list takes effect.
}
@dl-item["ATTLIST"]{
	An ordered collection of (NAME . VALUE) pairs, where NAME is
	a RES-NAME or an UNRES-NAME. The collection is an ADT
}
@dl-item["STR-HANDLER"]{
	A procedure of three arguments: STRING1 STRING2 SEED
	returning a new SEED
	The procedure is supposed to handle a chunk of character data
	STRING1 followed by a chunk of character data STRING2.
	STRING2 is a short string, often "\n" and even ""
}
@dl-item["ENTITIES"]{
	An assoc list of pairs:
	   @snipet{(named-entity-name . named-entity-body)}
	where named-entity-name is a symbol under which the entity was
	declared, named-entity-body is either a string, or
	(for an external entity) a thunk that will return an
	input port (from which the entity can be read).
	named-entity-body may also be #f. This is an indication that a
	named-entity-name is currently being expanded. A reference to
	this named-entity-name will be an error: violation of the
	WFC nonrecursion.
}
@dl-item["XML-TOKEN"]{a record
This record represents a markup, which is, according to the XML
Recommendation, "takes the form of start-tags, end-tags, empty-element tags,
entity references, character references, comments, CDATA section delimiters,
document type declarations, and processing instructions."

@dl-list[
	@dl-item["kind"]{a TAG-KIND}
	@dl-item["head"]{an UNRES-NAME. For xml-tokens of kinds 'COMMENT and
		'CDSECT, the head is #f}
]
For example,
@codeblock{
	<P>  => kind='START, head='P
	</P> => kind='END, head='P
	<BR/> => kind='EMPTY-EL, head='BR
	<!DOCTYPE OMF ...> => kind='DECL, head='DOCTYPE
	<?xml version="1.0"?> => kind='PI, head='xml
	&my-ent; => kind = 'ENTITY-REF, head='my-ent
}
Character references are not represented by xml-tokens as these references
are transparently resolved into the corresponding characters.
}
@dl-item["XML-DECL"]{a record

The record represents a datatype of an XML document: the list of
declared elements and their attributes, declared notations, list of
replacement strings or loading procedures for parsed general
entities, etc. Normally an xml-decl record is created from a DTD or
an XML Schema, although it can be created and filled in in many other
ways (e.g., loaded from a file).


@dl-list[
@dl-item["elems"]{
	an (assoc) list of decl-elem or #f. The latter instructs
	the parser to do no validation of elements and attributes.
}
@dl-item["decl-elem"]{
	declaration of one element:
	@snipet{(@var{elem-name} @var{elem-content} @var{decl-attrs})}
	@var{elem-name} is an UNRES-NAME for the element.

	@var{elem-content} is an ELEM-CONTENT-MODEL.

	@var{decl-attrs} is an ATTLIST, of (ATTR-NAME . VALUE) associations

!!!This element can declare a user procedure to handle parsing of an
element (e.g., to do a custom validation, or to build a hash of
IDs as they're encountered).
}
@dl-item["decl-attr"]{
	an element of an ATTLIST, declaration of one attribute
	@snipet{(@var{attr-name} @var{content-type} @var{use-type} @var{default-value})}

	@var{attr-name} is an UNRES-NAME for the declared attribute

	@var{content-type} is a symbol: CDATA, NMTOKEN, NMTOKENS, @dots{}
		or a list of strings for the enumerated type.

	@var{use-type} is a symbol: REQUIRED, IMPLIED, FIXED

	@var{default-value} is a string for the default value, or #f if not given.
}
]
}
]

@define[Function]{@name{make-xml-token} @args{kind head}}
@define[Function]{@name{xml-token?} @args{kind head}}
@desc{A constructor and a predicate for a @var{XML-TOKEN}
 record.}

@define[Macro]{@name{xml-token-kind} @args{xml-token}}
@define[Macro]{@name{xml-token-head} @args{xml-token}}
@desc{Accessor macros of a @var{XML-TOKEN} record.}

@subsubsection{Lower-level parsers and scanners}

They deal with primitive lexical units (Names, whitespaces, tags)
and with pieces of more generic productions. Most of these parsers
must be called in appropriate context. For example, ssax:complete-start-tag
must be called only when the start-tag has been detected and its GI
has been read.

@sub*section{Low-level parsing code}

@define[Function]{@name{ssax:skip-S} @args{port}}
@desc{Skip the S (whitespace) production as defined by
@snipet{[3] S ::= (#x20 | #x9 | #xD | #xA)}
The procedure returns the first not-whitespace character it
encounters while scanning the @var{port}. This character is left
on the input stream.
}

@define[Function]{@name{ssax:ncname-starting-char?} @args{a-char}}
@desc{
Read a Name lexem and return it as string
@codeblock{
[4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
                 | CombiningChar | Extender
[5] Name ::= (Letter | '_' | ':') (NameChar)*
}
This code supports the XML Namespace Recommendation REC-xml-names,
which modifies the above productions as follows:
@codeblock{
[4] NCNameChar ::= Letter | Digit | '.' | '-' | '_'
                      | CombiningChar | Extender
[5] NCName ::= (Letter | '_') (NCNameChar)*
}
As the Rec-xml-names says,
"An XML document conforms to this specification if all other tokens
[other than element types and attribute names] in the document which
are required, for XML conformance, to match the XML production for
Name, match this specification's production for NCName."
Element types and attribute names must match the production QName,
defined below.
}

@define[Function]{@var{ssax:read-NCName} @args{port}}
@desc{
Read a NCName starting from the current position in the @var{port} and
preturn it as a symbol.
}

@define[Function]{@name{ssax:read-QName} @var{port}}
@desc{
Read a (namespace-) Qualified Name, QName, from the current
position in the @var{port}.
@codeblock{
From REC-xml-names:
	[6] QName ::= (Prefix ':')? LocalPart
	[7] Prefix ::= NCName
	[8] LocalPart ::= NCName
}
Return: an UNRES-NAME
}

@define[Variable]{@name{ssax:Prefix-XML}}
@desc{The prefix of the pre-defined XML namespace}

@define[Function]{@name{name-compare} @args{name1 name2}}
@desc{
Compare one RES-NAME or an UNRES-NAME with the other.
Return a symbol '<, '>, or '= depending on the result of
the comparison.
Names without PREFIX are always smaller than those with the PREFIX.
}

@define[Variable]{@name{ssax:largest-unres-name}}
@desc{
An UNRES-NAME that is postulated to be larger than anything that can occur in
a well-formed XML document.
name-compare enforces this postulate.
}

@define[Function]{@name{ssax:read-markup-token} @args{port}}
@desc{
This procedure starts parsing of a markup token. The current position
in the stream must be #\<. This procedure scans enough of the input stream
to figure out what kind of a markup token it is seeing. The procedure returns
an xml-token structure describing the token. Note, generally reading
of the current markup is not finished! In particular, no attributes of
the start-tag token are scanned.

Here's a detailed break out of the return values and the position in the @var{port}
when that particular value is returned:
@dl-list[
	@dl-item["PI-token"]{
			only PI-target is read.
			To finish the Processing Instruction and disregard it,
			call ssax:skip-pi. ssax:read-attributes may be useful
			as well (for PIs whose content is attribute-value
			pairs)
	}
	@dl-item["END-token"]{
			The end tag is read completely; the current position
			is right after the terminating #\> character.
	}
	@dl-item["COMMENT"]{
			is read and skipped completely. The current position
			is right after "-->" that terminates the comment.
	}
	@dl-item["CDSECT"]{
			The current position is right after "<!CDATA[" @;]
			Use ssax:read-cdata-body to read the rest.
	}
	@dl-item["DECL"]{
			We have read the keyword (the one that follows "<!")
			identifying this declaration markup. The current
			position is after the keyword (usually a
			whitespace character)
	}
	@dl-item["START-token"]{
			We have read the keyword (GI) of this start tag.
			No attributes are scanned yet. We don't know if this
			tag has an empty content either.
			Use ssax:complete-start-tag to finish parsing of
			the token.
	}
]
}

@define[Function]{@name{ssax:skip-pi} @args{port}}
@desc{The current position is inside a PI. Skip till the rest of the PI}

@define[Function]{@name{ssax:read-pi-body-as-string} @args{port}}
@desc{
The current position is right after reading the PITarget. We read the
body of PI and return is as a string. The @var{port} will point to the
character right after '?>' combination that terminates PI.
@snipet{[16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'}
}

@define[Function]{@Name{ssax:skip-internal-dtd} @args{port}}
@desc{
The current pos in the port is inside an internal DTD subset
(e.g., after reading #\[ that begins an internal DTD subset)
Skip until the "]>" combination that terminates this DTD
}

@define[Function]{@name{ssax:read-cdata-body} @args{port str-handler seed}}
@desc{
This procedure must be called after we have read a string "<![CDATA["
that begins a CDATA section. The current position must be the first
position of the CDATA body. This function reads _lines_ of the CDATA
body and passes them to a STR-HANDLER, a character data consumer.

The @var{str-handler} is a STR-HANDLER, a procedure STRING1 STRING2 SEED.
The first STRING1 argument to STR-HANDLER never contains a newline.
The second STRING2 argument often will. On the first invocation of
the STR-HANDLER, the @var{seed} is the one passed to @code{ssax:read-cdata-body}
as the third argument. The result of this first invocation will be
passed as the @var{seed} argument to the second invocation of the line
consumer, and so on. The result of the last invocation of the
STR-HANDLER is returned by the ssax:read-cdata-body.  Note a
similarity to the fundamental 'fold' iterator.

Within a CDATA section all characters are taken at their face value,
with only three exceptions:
@itemlist[
	@item{CR, LF, and CRLF are treated as line delimiters, and passed}
	@item{as a single #\newline to the STR-HANDLER}
	@item{"]]>" combination is the end of the CDATA section.}
	@item{&gt; is treated as an embedded #\> character}
]
Note, &lt; and &amp; are not specially recognized (and are not expanded)!
}

@define[Function]{@name{ssax:read-char-ref} @args{port}}
@desc{
@codeblock{
[66]  CharRef ::=  '&#' [0-9]+ ';' 
                 | '&#x' [0-9a-fA-F]+ ';'
}
This procedure must be called after we we have read "&#" 
that introduces a char reference.
The procedure reads this reference and returns the corresponding char
The current position in @var{port} will be after ";" that terminates
the char reference
Faults detected:
	@dl-list[@dl-item["WFC"]{XML-Spec.html#wf-Legalchar}]

According to Section "4.1 Character and Entity References"
of the XML Recommendation:
@blockquote{
 "[Definition: A character reference refers to a specific character
  in the ISO/IEC 10646 character set, for example one not directly
  accessible from available input devices.]"
}
Therefore, we use a ucscode->char function to convert a character
code into the character -- @b{*regardless*} of the current character
encoding of the input stream.
}

@define[Function]{@name{ssax:handle-parsed-entity}
 @args{port name entities comment-handler str-handler seed}}
@desc{
Expand and handle a parsed-entity reference

@var{port} - a PORT

@var{name} - the name of the parsed entity to expand, a symbol

@var{entities} - see ENTITIES

@var{content-handler} -- procedure PORT ENTITIES SEED
	that is supposed to return a SEED

@var{str-handler} - a STR-HANDLER. It is called if the entity in question
turns out to be a pre-declared entity

The result is the one returned by CONTENT-HANDLER or STR-HANDLER
Faults detected:
	@dl-list[
	@dl-item["WFC"]{XML-Spec.html#wf-entdeclared}
	@dl-item["WFC"]{XML-Spec.html#norecursion}
	]
}

@define[Function]{@name{make-empty-attlist}}
@define[Function]{@name{attlist-add} @args{attlist name-value}}
@define[Function]{@name{attlist-null?} @args{attlist}}
@define[Function]{@name{attlist-remove-top} @args{attlist}}
@define[Function]{@name{attlist->alist} @args{attlist}}
@define[Function]{@name{attlist-fold} @args{kons knil attlst}}
@desc{Utility procedures to deal with attribute list, which keeps name-value association.}

@define[Function]{@name{ssax:read-attributes} @args{port entities}}
@desc{
This procedure reads and parses a production Attribute*
@codeblock{
[41] Attribute ::= Name Eq AttValue
[10] AttValue ::=  '"' ([^<&"] | Reference)* '"' 
                | "'" ([^<&'] | Reference)* "'"
[25] Eq ::= S? '=' S?
}
The procedure returns an ATTLIST, of Name (as UNRES-NAME), Value (as string)
pairs. The current character on the PORT is a non-whitespace character
that is not an ncname-starting character.

Note the following rules to keep in mind when reading an 'AttValue'
"Before the value of an attribute is passed to the application
or checked for validity, the XML processor must normalize it as follows: 
@itemlist[
@item{a character reference is processed by appending the referenced
  character to the attribute value}
@item{an entity reference is processed by recursively processing the
  replacement text of the entity [see ENTITIES]
  [named entities amp lt gt quot apos are assumed pre-declared]
}
@item{a whitespace character (#x20, #xD, #xA, #x9) is processed by appending #x20
  to the normalized value, except that only a single #x20 is appended for a
  "#xD#xA" sequence that is part of an external parsed entity or the
  literal entity value of an internal parsed entity}
@item{other characters are processed by appending them to the normalized value}]
"

Faults detected:
	@dl-list[
	@dl-item["WFC"]{XML-Spec.html#CleanAttrVals}
	@dl-item["WFC"]{XML-Spec.html#uniqattspec}
	]
}

@define[Function]{@name{ssax:resolve-name} @args{port ures-name namespace apply-default-us?}}
@desc{
Convert an UNRES-NAME to a RES-NAME given the appropriate NAMESPACES
declarations.
the last parameter @var{apply-default-ns?} determines if the default
namespace applies (for instance, it does not for attribute names)

Per REC-xml-names/#nsc-NSDeclared, "xml" prefix is considered pre-declared
and bound to the namespace name "http://www.w3.org/XML/1998/namespace".

This procedure tests for the namespace constraints:
@hyperlink[:href "http://www.w3.org/TR/REC-xml-names/#nsc-NSDeclared"]{http://www.w3.org/TR/REC-xml-names/#nsc-NSDeclared}

}

@define[Function]{@name{ssax:uri-string->symbol} @args{uri-str}}
@desc{Convert a URI-STR to an appropriate symbol}

@define[Function]{@name{ssax:complete-start-tag @args{tag port elems entities namespaces}}}
@desc{
This procedure is to complete parsing of a start-tag markup. The
procedure must be called after the start tag token has been
read. TAG is an UNRES-NAME. ELEMS is an instance of xml-decl::elems;
it can be #f to tell the function to do _no_ validation of elements
and their attributes.

This procedure returns several values:
@dl-list[
 @dl-item["ELEM-GI"]{a RES-NAME.}
 @dl-item["ATTRIBUTES"]{element's attributes, an ATTLIST of (RES-NAME . STRING)
	pairs. The list does NOT include xmlns attributes.}
 @dl-item["NAMESPACES"]{the input list of namespaces amended with namespace
	(re-)declarations contained within the start-tag under parsing}
 @dl-item["ELEM-CONTENT-MODEL"]{}
]
On exit, the current position in PORT will be the first character after
#\> that terminates the start-tag markup.

Faults detected:
@dl-list[
	@dl-item["VC"]{XML-Spec.html#enum}
	@dl-item["VC"]{XML-Spec.html#RequiredAttr}
	@dl-item["VC"]{XML-Spec.html#FixedAttr}
	@dl-item["VC"]{XML-Spec.html#ValueType}
	@dl-item["WFC"]{XML-Spec.html#uniqattspec (after namespaces prefixes are resolved)}
	@dl-item["VC"]{XML-Spec.html#elementvalid}
	@dl-item["WFC"]{REC-xml-names/#dt-NSName}
]
Note, although XML Recommendation does not explicitly say it,
xmlns and xmlns: attributes don't have to be declared (although they
can be declared, to specify their default value)
}

@define[Function]{@name{ssax:read-external-id} @args{port}}
@desc{
This procedure parses an ExternalID production:
@codeblock{
[75] ExternalID ::= 'SYSTEM' S SystemLiteral
		| 'PUBLIC' S PubidLiteral S SystemLiteral
[11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'") 
[12] PubidLiteral ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
[13] PubidChar ::=  #x20 | #xD | #xA | [a-zA-Z0-9]
                         | [-'()+,./:=?;!*#@atmark{}$_%]
}
This procedure is supposed to be called when an ExternalID is expected;
that is, the current character must be either #\S or #\P that start
correspondingly a SYSTEM or PUBLIC token. This procedure returns the
SystemLiteral as a string. A PubidLiteral is disregarded if present.
}

@sub*section{Higher-level parsers and scanners}

They parse productions corresponding to the whole (document) entity
or its higher-level pieces (prolog, root element, etc).

@define[Function]{@name{ssax:scan-Misc} @args{port}}
@desc{
Scan the Misc production in the context
@codeblock{
[1]  document ::=  prolog element Misc*
[22] prolog ::= XMLDecl? Misc* (doctypedec l Misc*)?
[27] Misc ::= Comment | PI |  S
}

The following function should be called in the prolog or epilog contexts.
In these contexts, whitespaces are completely ignored.
The return value from ssax:scan-Misc is either a PI-token,
a DECL-token, a START token, or EOF.
Comments are ignored and not reported.
}

@define[Function]{@name{ssax:read-char-data} @args{port expected-eof? str-handler seed}}
@desc{
This procedure is to read the character content of an XML document
or an XML element.
@codeblock{
[43] content ::= 
	(element | CharData | Reference | CDSect | PI
	| Comment)*
}
To be more precise, the procedure reads CharData, expands CDSect
and character entities, and skips comments. The procedure stops
at a named reference, EOF, at the beginning of a PI or a start/end tag.

@dl-list[
@dl-item["port"]{
	a PORT to read}
@dl-item["expect-eof?"]{
	a boolean indicating if EOF is normal, i.e., the character
	data may be terminated by the EOF. EOF is normal
	while processing a parsed entity.}
@dl-item["str-handler"]{
	a STR-HANDLER}
@dl-item["seed"]{
	an argument passed to the first invocation of STR-HANDLER.}
]
The procedure returns two results: SEED and TOKEN.
The SEED is the result of the last invocation of STR-HANDLER, or the
original seed if STR-HANDLER was never called.

TOKEN can be either an eof-object (this can happen only if
expect-eof? was #t), or:
@itemlist[
    @item{an xml-token describing a START tag or an END-tag;
	For a start token, the caller has to finish reading it.}
    @item{an xml-token describing the beginning of a PI. It's up to an
	application to read or skip through the rest of this PI;}
    @item{an xml-token describing a named entity reference.}
]
CDATA sections and character references are expanded inline and
never returned. Comments are silently disregarded.

As the XML Recommendation requires, all whitespace in character data
must be preserved. However, a CR character (#xD) must be disregarded
if it appears before a LF character (#xA), or replaced by a #xA character
otherwise. See Secs. 2.10 and 2.11 of the XML Recommendation. See also
the canonical XML Recommendation.
}

@define[Function]{@name{ssax:assert-token} @args{token kind gi}}
@desc{
Make sure that TOKEN is of anticipated KIND and has anticipated GI
Note GI argument may actually be a pair of two symbols, Namespace
URI or the prefix, and of the localname.
If the assertion fails, error-cont is evaluated by passing it
three arguments: token kind gi. The result of error-cont is returned.
}

@subsubsection{Highest-level parsers: XML to SXML}

These parsers are a set of syntactic forms to instantiate a SSAX parser.
A user can instantiate the parser to do the full validation, or
no validation, or any particular validation. The user specifies
which PI he wants to be notified about. The user tells what to do
with the parsed character and element data. The latter handlers
determine if the parsing follows a SAX or a DOM model.

@define[Macro]{@name{ssax:make-pi-parser} @args{my-pi-handlers}}
@desc{
Create a parser to parse and process one Processing Element (PI).
@dl-list[
@dl-item[@var{my-pi-handlers}]{
	An assoc list of pairs (PI-TAG . PI-HANDLER)
	where PI-TAG is an NCName symbol, the PI target, and
	PI-HANDLER is a procedure PORT PI-TAG SEED
	where PORT points to the first symbol after the PI target.
	The handler should read the rest of the PI up to and including
	the combination '?>' that terminates the PI. The handler should
	return a new seed.
	One of the PI-TAGs may be the symbol *DEFAULT*. The corresponding
	handler will handle PIs that no other handler will. If the
	*DEFAULT* PI-TAG is not specified, ssax:make-pi-parser will assume
	the default handler that skips the body of the PI}
]
The output of the @code{ssax:make-pi-parser} is a

	@snipet{procedure @var{PORT} @var{PI-TAG} @var{SEED}}

that will parse the current PI according to the user-specified handlers.
}

@define[Macro]{@name{ssax:make-elem-parser}
 @args{my-new-level-seed my-finish-element my-char-data-handler my-pi-handlers}}
@desc{
Create a parser to parse and process one element, including its
character content or children elements. The parser is typically
applied to the root element of a document.

@dl-list[
@dl-item[@var{my-new-level-seed}]{
	procedure ELEM-GI ATTRIBUTES NAMESPACES EXPECTED-CONTENT SEED
		where ELEM-GI is a RES-NAME of the element
		about to be processed.
	This procedure is to generate the seed to be passed
	to handlers that process the content of the element.
	This is the function identified as 'fdown' in the denotational
	semantics of the XML parser given in the title comments to this
	file.
}
@dl-item[@var{my-finish-element}]{
	procedure ELEM-GI ATTRIBUTES NAMESPACES PARENT-SEED SEED
	This procedure is called when parsing of ELEM-GI is finished.
	The SEED is the result from the last content parser (or
	from my-new-level-seed if the element has the empty content).
	PARENT-SEED is the same seed as was passed to my-new-level-seed.
	The procedure is to generate a seed that will be the result
	of the element parser.
	This is the function identified as 'fup' in the denotational
	semantics of the XML parser given in the title comments to this
	file.
}
@dl-item[@var{my-char-data-handler}]{
	A STR-HANDLER
}
@dl-item[@var{my-pi-handlers}]{
	See ssax:make-pi-handler above
}
]

The generated parser is a
@snipet{procedure @var{START-TAG-HEAD} @var{PORT} @var{ELEMS} @var{ENTITIES}
	 @var{NAMESPACES} @var{PRESERVE-WS?} @var{SEED}}
The procedure must be called after the start tag token has been
read. START-TAG-HEAD is an UNRES-NAME from the start-element tag.
ELEMS is an instance of xml-decl::elems.
See @code{ssax:complete-start-tag::preserve-ws?}

Faults detected:
@dl-list[
	@dl-item["VC"]{XML-Spec.html#elementvalid}
	@dl-item["WFC"]{XML-Spec.html#GIMatch}
]
}

@define[Macro]{@name{ssax:make-parser} @args{user-handler-tag user-handler-proc @dots{}}}
@desc{
Create an XML parser, an instance of the XML parsing framework.
This will be a SAX, a DOM, or a specialized parser depending
on the supplied user-handlers.

user-handler-tag is a symbol that identifies a procedural expression
that follows the tag. Given below are tags and signatures of the
corresponding procedures. Not all tags have to be specified. If some
are omitted, reasonable defaults will apply.

@dl-list[
@dl-itemx[2 "tag: DOCTYPE"
"handler-procedure: PORT DOCNAME SYSTEMID INTERNAL-SUBSET? SEED"]{
If internal-subset? is #t, the current position in the port
is right after we have read #\[ that begins the internal DTD subset.
We must finish reading of this subset before we return
(or must call skip-internal-subset if we aren't interested in reading it).
The port at exit must be at the first symbol after the whole
DOCTYPE declaration.
The @var{handler-procedure} must generate four values:
	ELEMS ENTITIES NAMESPACES SEED

See xml-decl::elems for ELEMS. It may be #f to switch off the validation.
NAMESPACES will typically contain USER-PREFIXes for selected URI-SYMBs.
The default handler-procedure skips the internal subset,
if any, and returns (values #f '() '() seed)
}
@dl-itemx[2 "tag: UNDECL-ROOT"
"handler-procedure ELEM-GI SEED"]{
where ELEM-GI is an UNRES-NAME of the root element. This procedure
is called when an XML document under parsing contains _no_ DOCTYPE
declaration.
The @var{handler-procedure}, as a DOCTYPE handler procedure above,
must generate four values:
	ELEMS ENTITIES NAMESPACES SEED

The default handler-procedure returns (values #f '() '() seed)
}
@dl-itemx[2 "tag: DECL-ROOT"
"handler-procedure ELEM-GI SEED"]{
where ELEM-GI is an UNRES-NAME of the root element. This procedure
is called when an XML document under parsing does contains the DOCTYPE
declaration.
The handler-procedure must generate a new SEED (and verify
that the name of the root element matches the doctype, if the handler
so wishes). 
The default @var{handler-procedure} is the identity function.
}
@dl-itemx[2 "tag: NEW-LEVEL-SEED"
"handler-procedure"]{see @code{ssax:make-elem-parser}, my-new-level-seed
}
@dl-itemx[2 "tag: FINISH-ELEMENT"
"handler-procedure"]{see @code{ssax:make-elem-parser}, my-finish-element
}
@dl-itemx[2 "tag: CHAR-DATA-HANDLER"
"handler-procedure"]{see @code{ssax:make-elem-parser}, my-char-data-handler
}
@dl-itemx[2 "tag: PI"
"handler-procedure"]{see @code{ssax:make-pi-parser}
The default value is '()

The generated parser is a
	procedure PORT SEED

This procedure parses the document prolog and then exits to
an element parser (created by @code{ssax:make-elem-parser}) to handle
the rest.
}
]
@codeblock{
[1]  document ::=  prolog element Misc*
[22] prolog ::= XMLDecl? Misc* (doctypedec | Misc*)?
[27] Misc ::= Comment | PI |  S

[28] doctypedecl ::=  '<!DOCTYPE' S Name (S ExternalID)? S? 
			('[' (markupdecl | PEReference | S)* ']' S?)? '>'
[29] markupdecl ::= elementdecl | AttlistDecl
                     | EntityDecl
                     | NotationDecl | PI
                     | Comment 
}
}

@subsubsection[:tag "ssax.user.api"]{Highest-level parsers: XML to SXML}

First, a few utility procedures that turned out useful

@define[Function]{@name{ssax:reverse-collect-str} @args{list-of-frags}}
@desc{
given the list of fragments (some of which are text strings)
reverse the list and concatenate adjacent text strings.
We can prove from the general case below that if LIST-OF-FRAGS
has zero or one element, the result of the procedure is equal?
to its argument. This fact justifies the shortcut evaluation below.
}

@define[Function]{@name{ssax:reverse-collect-str-drop-ws} @args{list-of-frags}}
@desc{
given the list of fragments (some of which are text strings)
reverse the list and concatenate adjacent text strings.
We also drop "unsignificant" whitespace, that is, whitespace
in front, behind and between elements. The whitespace that
is included in character data is not affected.
We use this procedure to "intelligently" drop "insignificant"
whitespace in the parsed SXML. If the strict compliance with
the XML Recommendation regarding the whitespace is desired, please
use the @code{ssax:reverse-collect-str} procedure instead.
}

@define[Function]{@name{ssax:xml->sxml} @args{port namespaces}}
@desc{
This is an instance of a SSAX parser above that returns an SXML
representation of the XML document to be read from PORT.
NAMESPACE-PREFIX-ASSIG is a list of (USER-PREFIX . URI-STRING)
that assigns USER-PREFIXes to certain namespaces identified by
particular URI-STRINGs. It may be an empty list.
The procedure returns an SXML tree. The port points out to the
first character after the root element.
}
