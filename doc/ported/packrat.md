[§2] (packrat) -- Packrat parser library {#ported.packrat}
-------------

###### [!Library] `(packrat)` 

This library is ported from Chicken Scheme
[packrat](http://wiki.call-cc.org/eggref/4/packrat). The
documentation is from the PDF file located on the website and formatted 
Sagittarius document format.

Packrat parsing is a memorizing, backtracking recursive-descent parsing
technique that runs in time and space linear in the size of the input test. The
technique was originally discovered by Alexander Birman in 1970 [1], and Bryan
Ford took up the idea for his master's thesis in 2002 [4, 3, 2]. For detailed
information on the technique, please see Bryan Ford's web pate at

["http://pdos.csail.mit.edu/~baford/packrat/"](http://pdos.csail.mit.edu/~baford/packrat/)This document describes an R5RS Scheme library of parsing combinators
implemented using the packrat parsing algorithm. The main interfaces are the
`packrat-parse` macro and the combinators into into which it expands, the
`base-generator->results` function, and the accessors for
`parse-result` records.

### [§3] Data Structures

This section describes the data structures that make up the core of the packrat
parsing algorithm, and some of the low-level procedures that operate on them.

#### [§4] parse-result

A parse-result record describes the results of an attempt at a parse at a
particular position in the input stream. It can either record a successful
parse, in which case it contains an associated semantic-value, or a failed
parse, in which case it contains a parse-error structure.

###### [!Function] `parse-result?`  _object_

This is a predicate which answers #t if and only if its argument is a
parse-result record.

###### [!Function] `parse-result-successful?`  _parse-result_

This predicate returns #t if its argument represents a successful parse,
or #f if it represents a failed parse.

###### [!Function] `parse-result-semantic-value`  _parse-result_

If the argument represents a successful parse, this function returns the
associated semantic-value; otherwise, it will return #f.

###### [!Function] `parse-result-next`  _parse-result_

If the argument represents a successful parse, this function returns a
parse-results record representing the parsed input stream starting immediately
after the parse this parse-results represents. For instance, given an input
stream [a, b, c, d, e], if the parse-result given to `parse-result-next`had completed successfully, consuming the [a, b, c] prefix of the input stream
and producing some semantic value, then the parse-result returned from
`parse-result-next` would represent all possible parses starting from the
[d, e] suffix of the input stream.

###### [!Function] `parse-result-error`  _parse-result_

If the argument represents a failed parse, this function returns a
parse-error structure; otherwise, it may return a parse-error structure for
internal implementation reasons (to do with propagating errors upwards for
improved error-reporting), or it may return #f

###### [!Function] `make-result`  _semantic-value_ _next-parse-results_

This function constructs an instance of parse-result representing a
successful parse. The first argument is used as the semantic value to include
with the new parse-result, and the second argument should be a parse-results
structure representing the location in the input stream from which continue
parsing.

###### [!Function] `make-expected-result`  _parse-position_ _object_

This function constructs an instance of parse-result representing a failed
parse. The parse-position in the first argument and the value in the second
argument are used to construct a variant of a parse-error record for inclusion
in the parse-result that reports that a particular kind of value was expected at
the given parse-position.

###### [!Function] `make-message-result`  _parse-position_ _string_

This function constructs an instance of parse-result representing a failed
parse. The parse-position in the first argument and the string in the second
argument are used to construct a variant of a parse-error record for inclusion
in the parse-result that reports a general error message at the given parse
position.

###### [!Function] `merge-result-errors`  _parse-result_ _parse-error_

This function propagates error information through a particular parse
result. The parse-error contained in the first argument is combined with the
parse-error from the second argument, and the resulting parse-result structure
is returned embedded in the error field of a copy of the first argument.

#### [§4] parse-results

A parse-results record notionally describes all possible parses that can be
attempted from a particular point in an input stream, and the results of those
parses. It contains a parse-position record, which corresponds to the position
in the input stream that this parse-results represents, and a map associating
"key objects" with instance of parse-result.

Atomic input objects (known as "base values"; usually either characters or token
/ semantic-value pairs) are represented specially in the parse-results data
structure, as an optimisation: the two fields `base` and code{next}
represent the implicit successful parse of a base value at the current position.
The `base` field contains a pair of a toke-class-identifier and a semantic
value unless the parse-results data structure as a whole is representing the of
the input stream, in which case it will contain #f.

###### [!Function] `parse-results?`  _object_

This is a predicate which answer #t if and only if its argument is a
parse-results record.

###### [!Function] `parse-results-position`  _parse-results_

Returns the parse-position corresponding to the argument. An unknown
position is represented by #f.

###### [!Function] `parse-results-base`  _parse-results_

If the argument corresponds to the end of the input stream, this function
returns #f; otherwise, it returns a pair, where the car is to be interpreted as
a base lexical token class identifier (for instance, "symbol", "string",
"number") and the cdr is to be interpreted as the semantic value of the data.

###### [!Function] `parse-results-token-kind`  _parse-results_

This function returns the car (the token class identifier) of the result 
of `parse-results-base`, if that result is a pair; otherwise it returns
#f.

###### [!Function] `parse-results-token-kind`  _parse-results_

This function returns the car (the token class identifier) of the result 
of `parse-results-base`, if that result is a pair; otherwise it returns
#f.

###### [!Function] `parse-results-token-value`  _parse-results_

This function returns the cdr (the token value) of the result of
`parse-results-base`, if that result is a pair; otherwise it returns #f.

###### [!Function] `parse-results-next`  _parse-results_

This function returns the parse-results record representing the position
in the input stream immediately after the argument's base token. For instance,
if the base tokens used represented characters, then this function would return
the parse-results representing the next character position; or, if the base
tokens represented lexemes, then this function would return a representation of
the results obtainable starting from the next lexeme position. The value #f is
returned if there is no next position (that is, if the argument represents the
final possible position before the end-of-stream).

###### [!Function] `base-generator->results`  _generator-function_

This function is used to set up an initial input stream of base tokens.
The argument is to be nullary function returning multiple-values, the first of
which is to be a parse-position record or #f, and the second of which is to be a
base token, that is a pair of a token class identifier and a semantic value. The
argument is called every time the parser needs to read a fresh base token from 
the input stream.

###### [!Function] `prepend-base`  _parse-position_ _base-value_ _parse-results_

This function effectively prepends a base token to particular
parse-results. This can be useful when implementing extensible parsers: using
this function in a suitable loop, it is possible to splice together two streams
of input.

For instance, if `r` is a parse-results representing parse over the input
token stream `'((b . 2) (c . 3))`, then the result of the call

``(prepend-base #f '(a . 1) r)``

is a new parse-results representing parse over the input stream
`'((a . 1) (b . 2) (c . 3))`.

The first argument to prepend-base, the parse-position, should be either a
parse-position representing the location the base token being prepended, or #f
if the input position of the base token is unknown.

###### [!Function] `prepend-semantic-value`  _parse-position_ _key-object_ _semantic-value_ _parse-results_

This function is similar to prepend-base, but prepends an already-computed
semantic value to a parse-results, again primarily for use in implementing
extensible parsers. The resulting parse-results is assigned the given
parse-position, and has an entry in its result map associating the given
key-object with the given semantic-value and input parse-results.

###### [!Function] `results->result`  _parse-results_ _key-object_ _result-thunk_

This function is the central function that drives the parsing process. It
examines the result in the parse-results given to it, searching for an entry
matching the given key-object. If such an entry is found, the parse-result
structure associated with the key is returned; otherwise, the nullary
result-thunk is called, and the resulting parse-result is both stored into the
result map and returned to the caller of `results->result`.

#### [§4] parse-error

Parse-error structure represent collected error information from attempted
parses. They contain two kinds of error report, following [3]: a collection of
"expected token" messages, and a collection of free-format message strings.

###### [!Function] `parse-error?`  _object_

This is a predicate which answers #t if and only if its argument is a
parse-error record.

###### [!Function] `parse-error-position`  _parse-error_

Retrieves the parse-position in the input stream that this parse-error is
describing. A #f result indicates an unknown position.

###### [!Function] `parse-error-expected`  _parse-error_

Retrieves the set (represented as a list) of token class identifiers that
could have allowed the parse to continue from this point.

###### [!Function] `parse-error-message`  _parse-error_

Retrieves the list of error messages associated with this parser-error.

###### [!Function] `make-error-expected`  _object_

Constructs an "expected token" parse-error record from its arguments.
Called by `make-expected-result`.

###### [!Function] `make-error-message`  _string_

Constructs an "general error message" parse-error record from its
arguments. Called by `make-message-result`.

###### [!Function] `parse-error-empty`  _parse-error_

Returns #f if its argument contains no expected tokens, and no general
error messages; otherwise returns #f. Used internally by
`merge-result-errors`.

###### [!Function] `merge-result-errors`  _parse-error_ _parse-error_

Merges two parse-error records, following [3]. If one record represents a
position earlier in the input stream than the other, then that record is
returned; if they both represent the same position, the "expected token" sets
are unioned and the general message lists are appended to form a new
parse-error record at the same position. The standard parsing combinators call
this function as appropriate to propagate error information through the parse.

#### [§4] parse-position

A parse-position record represents a character location in an input stream.

###### [!Function] `make-parse-position`  _filename_ _linenumber_ _columnnumber_

Constructs a parse-position record from its arguments. The given filename
may be #f if the filename is unknown or not appropriate for the input stream
the parse-position is indexing into.

###### [!Function] `parse-position?`  _object_

This is a predicate which answer #t if any only if its argument is
parse-position record.

###### [!Function] `parse-position-file`  _parse-position_

Retrieves the file name associated with a parse-position record. Returns
#f if the filename is absent or not appropriate for this input stream.

###### [!Function] `parse-position-line`  _parse-position_

Retrieves the line number this parse-position represents. Line numbers
begin at 1; that is all characters on the very first line in a file will have
line number 1.

###### [!Function] `parse-position-column`  _parse-position_

Retrieves the column number within a line that parse-position represents.
Column numbers begin at 0; that is, the very first character of the very first
line in a file will have line number 1 and column number 0.

###### [!Function] `top-parse-position`  _string_

Constructs a parse-position representing the very beginning of an input
stream. The argument is passed into `make-parse-position` as the "filename"
parameter, and so may be either a string or #f.

###### [!Function] `update-parse-position`  _parse-position_ _character_

Given a position, and the character occurring at that position, returns
the position of the next character in the input stream. Most characters simply
increment the column number. Exceptions to this rule are: `#\return`, which
resets the column number to zero; `#\newline`, which both resets the column
number to zero and increments the line number; and `#\tab`, which
increments the column number to the nearest multiple of eight, just as terminal 
with an eight-column tab stop setting might do.

###### [!Function] `parse-position->string`  _parse-position_

Converts a parse-position record into an emacs-compatible display format.
If the filename in the parse-position is unknown, the string "\<??>" is used in
its place. The result is of the form

``filename:linenumber:columnnumber``

for example,

``main.c:33:7``



###### [!Function] `parse-position>?`  _parse-position_ _parse-position_

Returns #t if the first parse-position is more than advanced in the input
stream than the second parse-position. Either or both positions may be #f,
representing unknown positions; an unknown position is considered to be less
advanced in the input stream than any known position. Note that the filename
associated with each parse-position is completely ignored. It is the caller's
responsibility to ensure the two positions are associated with the same input
stream.

### [§3] Parsing Combinators

Parsing combinators are functions taking a parse-results structure and retruning
a parse-result structure. Each combinator attempts to parse the input stream in
some manner, and the result of the combinator is either a successful parse with
an associated semantic value, or a failed parse with an associated error record.

This section describes the procedures that produce the mid-level parsing
combinators provided as part of the library.

The type of a parser combinator, written in ML-like notation, would be

``parse-results -> parse-result``

###### [!Function] `packrat-check-base`  _kind-object_ _semantic-value-acceptor_

Returns a combinator which, if the next base token has token class
identifier equal to the first argument ("kind-object"), calls the second
argument ("semantic-value-acceptor") with the semantic value of the next base
token. The result of this cal should be another parser combinator, which is
applied to the parse-results representing the remainder of the input stream.

The type of the semantic value acceptor, written in ML-like notation, would be

``semanticValue -> parserCombinator``

or more fully expanded,

``semanticValue -> parse-results -> parse-result``

These types recall the types of functions that work with monads.

###### [!Function] `packrat-check`  _combinator_ _semantic-value-acceptor_

Returns a combinator which attempts to parse using the first argument, and
if the parse is successful, hands the resulting semantic value to the
semantic-value-acceptor (which has the same type as the semantic-value-acceptor
passed to `packrat-check-base` ) and continues parsing using the resulting
combinator.

###### [!Function] `packrat-or`  _combinator_ _combinator_

Returns a combinator which attempts to parse using the first argument,
only trying the second argument if the first argument fails to parse the input.
This is the basic combinator used to implement a choice among several
alternative means of parsing an input stream.

###### [!Function] `packrat-unless`  _string_ _combinator_ _combinator_

The combinator returned from this function first tries the first
combinator given. If it fails, the second is tried; otherwise, an error message
containing the given string is returned as the result. This can be used to
assert that a particular sequence of tokens does not occur at the current
position before continuing on. (This is the "not-followed-by" matcher).

### [§3] The parckrat-parser macro

###### [!Macro] `packrat-parser`  _result-expr_ _nonterminal-definition_ _..._

The `packrat-parse` macro provides syntactic sugar for building complex
parser combinators from simpler combinators. The general form of the macro, in
an EBNF-like language, is:

``(packrat-parser <result-expr> <nonterminal-definition>*)``

where

``````````scheme
<nonterminal-definition> :==
  (<nonterminal-id> (<sequence> <body-expr>+)*)
<sequence> :== (<part>*)
<part> :== (! <part>*)
       |   (/ <sequence>*)
       |   <var> <- '<kind-object>
       |   <var> <- 

       |   <var> <- <nonterminal-id>
       |   '<kind-object>
       |   <nonterminal-id>
``````````

Each nonterminal-definition expands into a parser-combinator. The collection of
defined nonterminal parser-combinators expands to a `(begin)` containing an
internal definition for each nonterminal.

The result of the whole `packrat-parser` form is the `<result-expr>`immediately following the `packrat-parser` keyword. Since `(begin)`within `(begin)` forms are flattened out in Scheme, the
`<result-expr>` can be used to introduce handwritten parser combinators
which can call, and can be called by, the nonterminal definitions built in the
rest of the parser definition.

Each nonterminal definition expands into:

``````````scheme
(define (<nonterminal-id> results)
  (results->result results 'nonterminal-id
    (lambda ()
      (<...> results))))
``````````

where `<...>` is the expanded definition-of-sequences combinator formed
form the body of the nonterminal definition.

An alternation (either implicit in the main body of a nonterminal definition, or
introduced via a `<part>` of the form `(/ <sequence> ...)`)
expands to

`(packrat-or <expansion-of-first-alternative>
            (packrat-or <expansion-of-second-alternative>
                        ...))    
`This causes each alternative to be tried in turn, in left-to-right order of
occurrence.

Wherever a `<part>` of the form `"<var> <- ..."` occurs, a
variable binding for `<var>` is made available in the `<body-expr>`s
that make up each arm of a nonterminal definition. The variable will be bound to
the semantic value resulting form parsing according to the parser definition to
the right of the arrow (the `"..."` above).

The `(! <part> ...)` syntax expands into an invocation of
`packrat-unless`.

The `"@"` syntax in `"<var> <- @"` causes `<var>`to be bound to the parse-position at that point in the input stream. This can be
used for annotating abstract syntax trees with location information.

`<part>`s of the form `'<kind-object>` expand into invocations of
`packrat-check-base`; those of the form `<nonterminal-id>` expand
into invocations of `packrat-check`, with the procedure associated with
the named nonterminal passed in as the combinator argument.


### [§3] References

[1] Alexander Birman and Jeffrey D. Ullman. Parsing algorithms with backtrack.
_Information and Control,_ 23(1):1 34, August 1973

[2] Bryan Ford. Parsing expression grammars: A recognition-based syntactic
foundation.

[3] Bryan Ford. Packrat parsing: a practical linear-time algorithm with
backtracking. Master's thesis. Massachusetts Institute of Technology, Sep 2002.

[4] Bryan Ford. Packrat parsing: Simple, powerful, lazy, linear time. In
_Proceedings of the 2002 International Conference on Functional
Programming_. Oct 2002.

