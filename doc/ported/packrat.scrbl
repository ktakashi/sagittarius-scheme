@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "ported.packrat"]{(packrat) -- Packrat parser library}

@define[Library]{@name{(packrat)}}
@desc{This library is ported from Chicken Scheme
@hyperlink[:href "http://wiki.call-cc.org/eggref/4/packrat"]{packrat}. The
documentation is from the PDF file located on the website and formatted 
Sagittarius document format.}

Packrat parsing is a memorizing, backtracking recursive-descent parsing
technique that runs in time and space linear in the size of the input test. The
technique was originally discovered by Alexander Birman in 1970 [1], and Bryan
Ford took up the idea for his master's thesis in 2002 [4, 3, 2]. For detailed
information on the technique, please see Bryan Ford's web pate at

@hyperlink[:href "http://pdos.csail.mit.edu/~baford/packrat/"]{"http://pdos.csail.mit.edu/~baford/packrat/"}

This document describes an R5RS Scheme library of parsing combinators
implemented using the packrat parsing algorithm. The main interfaces are the
@code{packrat-parse} macro and the combinators into into which it expands, the
@code{base-generator->results} function, and the accessors for
@code{parse-result} records.

@subsubsection{Data Structures}

This section describes the data structures that make up the core of the packrat
parsing algorithm, and some of the low-level procedures that operate on them.

@sub*section{parse-result}

A parse-result record describes the results of an attempt at a parse at a
particular position in the input stream. It can either record a successful
parse, in which case it contains an associated semantic-value, or a failed
parse, in which case it contains a parse-error structure.

@define[Function]{@name{parse-result?} @args{object}}
@desc{This is a predicate which answers #t if and only if its argument is a
parse-result record.}

@define[Function]{@name{parse-result-successful?} @args{parse-result}}
@desc{This predicate returns #t if its argument represents a successful parse,
or #f if it represents a failed parse.}

@define[Function]{@name{parse-result-semantic-value} @args{parse-result}}
@desc{If the argument represents a successful parse, this function returns the
associated semantic-value; otherwise, it will return #f.}

@define[Function]{@name{parse-result-next} @args{parse-result}}
@desc{If the argument represents a successful parse, this function returns a
parse-results record representing the parsed input stream starting immediately
after the parse this parse-results represents. For instance, given an input
stream [a, b, c, d, e], if the parse-result given to @code{parse-result-next}
had completed successfully, consuming the [a, b, c] prefix of the input stream
and producing some semantic value, then the parse-result returned from
@code{parse-result-next} would represent all possible parses starting from the
[d, e] suffix of the input stream.}

@define[Function]{@name{parse-result-error} @args{parse-result}}
@desc{If the argument represents a failed parse, this function returns a
parse-error structure; otherwise, it may return a parse-error structure for
internal implementation reasons (to do with propagating errors upwards for
improved error-reporting), or it may return #f}

@define[Function]{@name{make-result} @args{semantic-value next-parse-results}}
@desc{This function constructs an instance of parse-result representing a
successful parse. The first argument is used as the semantic value to include
with the new parse-result, and the second argument should be a parse-results
structure representing the location in the input stream from which continue
parsing.}

@define[Function]{@name{make-expected-result} @args{parse-position object}}
@desc{This function constructs an instance of parse-result representing a failed
parse. The parse-position in the first argument and the value in the second
argument are used to construct a variant of a parse-error record for inclusion
in the parse-result that reports that a particular kind of value was expected at
the given parse-position.}

@define[Function]{@name{make-message-result} @args{parse-position string}}
@desc{This function constructs an instance of parse-result representing a failed
parse. The parse-position in the first argument and the string in the second
argument are used to construct a variant of a parse-error record for inclusion
in the parse-result that reports a general error message at the given parse
position.}

@define[Function]{@name{merge-result-errors} @args{parse-result parse-error}}
@desc{This function propagates error information through a particular parse
result. The parse-error contained in the first argument is combined with the
parse-error from the second argument, and the resulting parse-result structure
is returned embedded in the error field of a copy of the first argument.}

@sub*section{parse-results}

A parse-results record notionally describes all possible parses that can be
attempted from a particular point in an input stream, and the results of those
parses. It contains a parse-position record, which corresponds to the position
in the input stream that this parse-results represents, and a map associating
"key objects" with instance of parse-result.

Atomic input objects (known as "base values"; usually either characters or token
/ semantic-value pairs) are represented specially in the parse-results data
structure, as an optimisation: the two fields @code{base} and code{next}
represent the implicit successful parse of a base value at the current position.
The @code{base} field contains a pair of a toke-class-identifier and a semantic
value unless the parse-results data structure as a whole is representing the of
the input stream, in which case it will contain #f.

@define[Function]{@name{parse-results?} @args{object}}
@desc{This is a predicate which answer #t if and only if its argument is a
parse-results record.}

@define[Function]{@name{parse-results-position} @args{parse-results}}
@desc{Returns the parse-position corresponding to the argument. An unknown
position is represented by #f.}

@define[Function]{@name{parse-results-base} @args{parse-results}}
@desc{If the argument corresponds to the end of the input stream, this function
returns #f; otherwise, it returns a pair, where the car is to be interpreted as
a base lexical token class identifier (for instance, "symbol", "string",
"number") and the cdr is to be interpreted as the semantic value of the data.}

@define[Function]{@name{parse-results-token-kind} @args{parse-results}}
@desc{This function returns the car (the token class identifier) of the result 
of @code{parse-results-base}, if that result is a pair; otherwise it returns
#f.}

@define[Function]{@name{parse-results-token-kind} @args{parse-results}}
@desc{This function returns the car (the token class identifier) of the result 
of @code{parse-results-base}, if that result is a pair; otherwise it returns
#f.}

@define[Function]{@name{parse-results-token-value} @args{parse-results}}
@desc{This function returns the cdr (the token value) of the result of
@code{parse-results-base}, if that result is a pair; otherwise it returns #f.}

@define[Function]{@name{parse-results-next} @args{parse-results}}
@desc{This function returns the parse-results record representing the position
in the input stream immediately after the argument's base token. For instance,
if the base tokens used represented characters, then this function would return
the parse-results representing the next character position; or, if the base
tokens represented lexemes, then this function would return a representation of
the results obtainable starting from the next lexeme position. The value #f is
returned if there is no next position (that is, if the argument represents the
final possible position before the end-of-stream).}

@define[Function]{@name{base-generator->results} @args{generator-function}}
@desc{This function is used to set up an initial input stream of base tokens.
The argument is to be nullary function returning multiple-values, the first of
which is to be a parse-position record or #f, and the second of which is to be a
base token, that is a pair of a token class identifier and a semantic value. The
argument is called every time the parser needs to read a fresh base token from 
the input stream.}

@define[Function]{@name{prepend-base}
 @args{parse-position base-value parse-results}}
@desc{This function effectively prepends a base token to particular
parse-results. This can be useful when implementing extensible parsers: using
this function in a suitable loop, it is possible to splice together two streams
of input.

For instance, if @code{r} is a parse-results representing parse over the input
token stream @code{'((b . 2) (c . 3))}, then the result of the call

@snipet{(prepend-base #f '(a . 1) r)}

is a new parse-results representing parse over the input stream
@code{'((a . 1) (b . 2) (c . 3))}.

The first argument to prepend-base, the parse-position, should be either a
parse-position representing the location the base token being prepended, or #f
if the input position of the base token is unknown.}

@define[Function]{@name{prepend-semantic-value}
 @args{parse-position key-object semantic-value parse-results}}
@desc{This function is similar to prepend-base, but prepends an already-computed
semantic value to a parse-results, again primarily for use in implementing
extensible parsers. The resulting parse-results is assigned the given
parse-position, and has an entry in its result map associating the given
key-object with the given semantic-value and input parse-results.}

@define[Function]{@name{results->result}
 @args{parse-results key-object result-thunk}}
@desc{This function is the central function that drives the parsing process. It
examines the result in the parse-results given to it, searching for an entry
matching the given key-object. If such an entry is found, the parse-result
structure associated with the key is returned; otherwise, the nullary
result-thunk is called, and the resulting parse-result is both stored into the
result map and returned to the caller of @code{results->result}.}

@sub*section{parse-error}

Parse-error structure represent collected error information from attempted
parses. They contain two kinds of error report, following [3]: a collection of
"expected token" messages, and a collection of free-format message strings.

@define[Function]{@name{parse-error?} @args{object}}
@desc{This is a predicate which answers #t if and only if its argument is a
parse-error record.}

@define[Function]{@name{parse-error-position} @args{parse-error}}
@desc{Retrieves the parse-position in the input stream that this parse-error is
describing. A #f result indicates an unknown position.}

@define[Function]{@name{parse-error-expected} @args{parse-error}}
@desc{Retrieves the set (represented as a list) of token class identifiers that
could have allowed the parse to continue from this point.}

@define[Function]{@name{parse-error-message} @args{parse-error}}
@desc{Retrieves the list of error messages associated with this parser-error.}

@define[Function]{@name{make-error-expected} @args{object}}
@desc{Constructs an "expected token" parse-error record from its arguments.
Called by @code{make-expected-result}.}

@define[Function]{@name{make-error-message} @args{string}}
@desc{Constructs an "general error message" parse-error record from its
arguments. Called by @code{make-message-result}.}

@define[Function]{@name{parse-error-empty} @args{parse-error}}
@desc{Returns #f if its argument contains no expected tokens, and no general
error messages; otherwise returns #f. Used internally by
@code{merge-result-errors}.}

@define[Function]{@name{merge-result-errors} @args{parse-error parse-error}}
@desc{Merges two parse-error records, following [3]. If one record represents a
position earlier in the input stream than the other, then that record is
returned; if they both represent the same position, the "expected token" sets
are unioned and the general message lists are appended to form a new
parse-error record at the same position. The standard parsing combinators call
this function as appropriate to propagate error information through the parse.}

@sub*section{parse-position}

A parse-position record represents a character location in an input stream.

@define[Function]{@name{make-parse-position}
 @args{filename linenumber columnnumber}}
@desc{Constructs a parse-position record from its arguments. The given filename
may be #f if the filename is unknown or not appropriate for the input stream
the parse-position is indexing into.}

@define[Function]{@name{parse-position?} @args{object}}
@desc{This is a predicate which answer #t if any only if its argument is
parse-position record.}

@define[Function]{@name{parse-position-file} @args{parse-position}}
@desc{Retrieves the file name associated with a parse-position record. Returns
#f if the filename is absent or not appropriate for this input stream.}

@define[Function]{@name{parse-position-line} @args{parse-position}}
@desc{Retrieves the line number this parse-position represents. Line numbers
begin at 1; that is all characters on the very first line in a file will have
line number 1.}

@define[Function]{@name{parse-position-column} @args{parse-position}}
@desc{Retrieves the column number within a line that parse-position represents.
Column numbers begin at 0; that is, the very first character of the very first
line in a file will have line number 1 and column number 0.}

@define[Function]{@name{top-parse-position} @args{string}}
@desc{Constructs a parse-position representing the very beginning of an input
stream. The argument is passed into @code{make-parse-position} as the "filename"
parameter, and so may be either a string or #f.}

@define[Function]{@name{update-parse-position} @args{parse-position character}}
@desc{Given a position, and the character occurring at that position, returns
the position of the next character in the input stream. Most characters simply
increment the column number. Exceptions to this rule are: @code{#\return}, which
resets the column number to zero; @code{#\newline}, which both resets the column
number to zero and increments the line number; and @code{#\tab}, which
increments the column number to the nearest multiple of eight, just as terminal 
with an eight-column tab stop setting might do.}

@define[Function]{@name{parse-position->string} @args{parse-position}}
@desc{Converts a parse-position record into an emacs-compatible display format.
If the filename in the parse-position is unknown, the string "<??>" is used in
its place. The result is of the form

@snipet{filename:linenumber:columnnumber}

for example,

@snipet{main.c:33:7}}

@define[Function]{@name{parse-position>?} @args{parse-position parse-position}}
@desc{Returns #t if the first parse-position is more than advanced in the input
stream than the second parse-position. Either or both positions may be #f,
representing unknown positions; an unknown position is considered to be less
advanced in the input stream than any known position. Note that the filename
associated with each parse-position is completely ignored. It is the caller's
responsibility to ensure the two positions are associated with the same input
stream.}

@subsubsection{Parsing Combinators}

Parsing combinators are functions taking a parse-results structure and retruning
a parse-result structure. Each combinator attempts to parse the input stream in
some manner, and the result of the combinator is either a successful parse with
an associated semantic value, or a failed parse with an associated error record.

This section describes the procedures that produce the mid-level parsing
combinators provided as part of the library.

The type of a parser combinator, written in ML-like notation, would be

@snipet{parse-results -> parse-result}

@define[Function]{@name{packrat-check-base}
 @args{kind-object semantic-value-acceptor}}
@desc{Returns a combinator which, if the next base token has token class
identifier equal to the first argument ("kind-object"), calls the second
argument ("semantic-value-acceptor") with the semantic value of the next base
token. The result of this cal should be another parser combinator, which is
applied to the parse-results representing the remainder of the input stream.

The type of the semantic value acceptor, written in ML-like notation, would be

@snipet{semanticValue -> parserCombinator}

or more fully expanded,

@snipet{semanticValue -> parse-results -> parse-result}

These types recall the types of functions that work with monads.}

@define[Function]{@name{packrat-check}
 @args{combinator semantic-value-acceptor}}
@desc{Returns a combinator which attempts to parse using the first argument, and
if the parse is successful, hands the resulting semantic value to the
semantic-value-acceptor (which has the same type as the semantic-value-acceptor
passed to @code{packrat-check-base} ) and continues parsing using the resulting
combinator.}

@define[Function]{@name{packrat-or} @args{combinator combinator}}
@desc{Returns a combinator which attempts to parse using the first argument,
only trying the second argument if the first argument fails to parse the input.
This is the basic combinator used to implement a choice among several
alternative means of parsing an input stream.}

@define[Function]{@name{packrat-unless} @args{string combinator combinator}}
@desc{The combinator returned from this function first tries the first
combinator given. If it fails, the second is tried; otherwise, an error message
containing the given string is returned as the result. This can be used to
assert that a particular sequence of tokens does not occur at the current
position before continuing on. (This is the "not-followed-by" matcher).}

@subsubsection{The parckrat-parser macro}

@define[Macro]{@name{packrat-parser}
 @args{result-expr nonterminal-definition @dots{}}
@desc{
The @code{packrat-parse} macro provides syntactic sugar for building complex
parser combinators from simpler combinators. The general form of the macro, in
an EBNF-like language, is:

@snipet{(packrat-parser <result-expr> <nonterminal-definition>*)}

where

@codeblock{
<nonterminal-definition> :==
  (<nonterminal-id> (<sequence> <body-expr>+)*)
<sequence> :== (<part>*)
<part> :== (! <part>*)
       |   (/ <sequence>*)
       |   <var> <- '<kind-object>
       |   <var> <- @
       |   <var> <- <nonterminal-id>
       |   '<kind-object>
       |   <nonterminal-id>
}

Each nonterminal-definition expands into a parser-combinator. The collection of
defined nonterminal parser-combinators expands to a @code{(begin)} containing an
internal definition for each nonterminal.

The result of the whole @code{packrat-parser} form is the @code{<result-expr>}
immediately following the @code{packrat-parser} keyword. Since @code{(begin)}
within @code{(begin)} forms are flattened out in Scheme, the
@code{<result-expr>} can be used to introduce handwritten parser combinators
which can call, and can be called by, the nonterminal definitions built in the
rest of the parser definition.

Each nonterminal definition expands into:

@codeblock{
(define (<nonterminal-id> results)
  (results->result results 'nonterminal-id
    (lambda ()
      (<@dots{}> results))))
}

where @code{<@dots{}>} is the expanded definition-of-sequences combinator formed
form the body of the nonterminal definition.

An alternation (either implicit in the main body of a nonterminal definition, or
introduced via a @code{<part>} of the form @code{(/ <sequence> @dots{})})
expands to

@code{
(packrat-or <expansion-of-first-alternative>
            (packrat-or <expansion-of-second-alternative>
                        @dots{}))    
}

This causes each alternative to be tried in turn, in left-to-right order of
occurrence.

Wherever a @code{<part>} of the form @code{"<var> <- @dots{}"} occurs, a
variable binding for @code{<var>} is made available in the @code{<body-expr>}s
that make up each arm of a nonterminal definition. The variable will be bound to
the semantic value resulting form parsing according to the parser definition to
the right of the arrow (the @code{"@dots{}"} above).

The @code{(! <part> @dots{})} syntax expands into an invocation of
@code{packrat-unless}.

The @code{"@atmark{}"} syntax in @code{"<var> <- @atmark{}"} causes @code{<var>}
to be bound to the parse-position at that point in the input stream. This can be
used for annotating abstract syntax trees with location information.

@code{<part>}s of the form @code{'<kind-object>} expand into invocations of
@code{packrat-check-base}; those of the form @code{<nonterminal-id>} expand
into invocations of @code{packrat-check}, with the procedure associated with
the named nonterminal passed in as the combinator argument.
}

@subsubsection{References}

[1] Alexander Birman and Jeffrey D. Ullman. Parsing algorithms with backtrack.
@italic{Information and Control,} 23(1):1 34, August 1973

[2] Bryan Ford. Parsing expression grammars: A recognition-based syntactic
foundation.

[3] Bryan Ford. Packrat parsing: a practical linear-time algorithm with
backtracking. Master's thesis. Massachusetts Institute of Technology, Sep 2002.

[4] Bryan Ford. Packrat parsing: Simple, powerful, lazy, linear time. In
@italic{Proceedings of the 2002 International Conference on Functional
Programming}. Oct 2002.

