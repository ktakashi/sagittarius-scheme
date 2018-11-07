@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.5322"]{(rfc :5322) - Internet message format library}

@define[Library]{@name{(rfc :5322)}}
@desc{This library provides the procedures for internet message format defined
in RFC5322(@hyperlink[:href "http://tools.ietf.org/html/rfc5322"]{RFC 5322}).
}

@subsubsection{Parsing message headers}

@define[Function]{@name{rfc5322-read-headers}
 @args{input-port :key (strict? #f) reader}}
@desc{@var{input-port} must be input port and will be passed to @var{reader}.

Reads RFC5322 format message from the given port until it reaches the end of the
message header and returns a list of the following format;

@snipet{((name body) @dots{})}

@var{name @dots{}} are the field names and @var{body @dots{}} are the
correspoinding field body. Both are as string. Field names are converted to
lower-case characters.

The keyword argument @var{strict?} switches the behaviour when the procedure
encounter EOF. If it's #f, then it simply ignore the field and return the
composed list. And if it's #t, it raises @code{&rfc5322-parse-error}.

The keyword argument @var{reader} reads a line from given port. The default
is @code{rfc5322-line-reader} and it treats both LF and CRLF eof style. If you
want to read from binary port, you need to pass own reader.
}

@define[Function]{@name{rfc5322-line-reader} @args{input-port}}
@desc{@var{input-port} must be textual input port.

Reads a line from given port. If the last character is CR chop it off.
}

@define[Function]{@name{rfc5322-header-ref}
 @args{header-list field-name . maybe-default}}
@desc{An utility procedure to get a specific field from the parsed header list,
which is returned by @code{rfc5322-read-headers}.

If the field with given @var{field-name} is in @var{header-list}, the
procedure returns its value in a string.  Otherwise, if default is
given, it is returned, and if not, #f is returned.
}

@define[Function]{@name{rfc5322-header-ref*} @args{header-list field-name}}
@desc{An utility procedure to get a specific field from the parsed header list,
which is returned by @code{rfc5322-read-headers}.

This procedure collects all the given @var{field-name} from the
@var{header-list}. If there's no header named @var{field-name}, then the
procedure returns @code{()}.
}


@subsubsection{Basic field parsers}

@define[Function]{@name{rfc5322-next-token}
 @args{input-port :optional tokenizer-spec}}
@desc{@var{input-port} must be textual input port.

A basic tokenizer. First it skips whitespaces and/or comments (CFWS) from 
@var{input-port}, if any. Then reads one token according to 
var{tokenizer-specs}. If @var{input-port} reaches EOF before any token is read,
EOF is returned.

@var{tokenizer-specs} is a list of tokenizer spec. which is a cons of a char-set
and a procedure.

After skipping CFWS, the procedure peeks a character at the head of
@var{input-port}, and checks it against the char-sets in @var{tokenizer-specs}
one by one. If a char-set that contains the character belongs to is found, then
a token is retrieved with calling the procedure with @var{input-port} to read a
token.

If the head character doesn’t match any char-sets, the character is taken from
@var{input-port} and returned.

The default @var{tokenizer-specs} is as follows:
 	
@snipet{
(list (cons (string->char-set ":") rfc5322-quoted-string)
      (cons *rfc5322-atext-chars* rfc5322-dot-atom))
}
}

@define[Function]{@name{rfc5322-field->tokens}
 @args{field :optional tokenizer-spec}}
@desc{A convenience procedure. Creates a string input port from given
@var{field} and calls @code{rfc5322-next-token} repeatedly on it until it
consumes all input, and returns a list of tokens.
}

@define[Function]{@name{rfc5322-skip-cfws} @args{input-port}}
@desc{Consumes whitespace characters and/or any comments from @var{input-port}
and returns a non comment and whitespace character. The returned character
remains in @var{input-port}.
}

@define[Variable]{@name{*rfc5322-atext-chars*}}
@desc{A character set which is defined RFC 5322 section 3.2.3 Atom.}

@define[Variable]{@name{*rfc5322-standard-tokenizers*}}
@desc{Default tokenizer.}

@define[Function]{@name{rfc5322-dot-atom} @args{input-port}}
@define[Function]{@name{rfc5322-quoted-string} @args{input-port}}
@desc{Tokenizers for @code{dot-atom} and @code{quoted-string} respectively.}

@subsubsection{Specific field parsers}

@define[Function]{@name{rfc5322-parse-date} @args{string}}
@desc{Takes RFC-5322 type date string and returns eight values:

@code{year, month, day-of-month, hour, minute, second, time-zone, day-of-week.}

@var{time-zone} is an offset from UT in minutes. @var{day-of-week} is a day from
sunday, and may be #f if that information is not available. @var{month} is an
integer between 1 and 12, inclusive. If the @var{string} is not parsable, all
the elements are #f. 
}


@subsubsection{Message constructors}

@define[Function]{@name{rfc5322-write-headers}
 @args{headers :key (output (current-output-port))
 (check :error) (continue #f)}}
@desc{Writes the given @var{header} to the port @var{output}.
}

@define[Function]{@name{date->rfc5322-date} @args{date}}
@desc{@var{date} must be SRFI-19 date.

Returns RFC 5322 date formatted string.
}
