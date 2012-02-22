@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "ported.text.parse"]{(text parse) - Parsing input stream}

@define[Library]{@name{(text parse)}}
@desc{The @code{(text parse)} library is inspired and compatible with Oleg
Kiselyov's input parsing library. You can use this library in place of his
@code{'input-parse.scm'} and @code{'look-for-str.scm'}.}

@define[Function]{@name{find-string-from-port?}
 @args{str in-port :optional max-no-char}}
@desc{Looks for a string @var{str} from the input port @var{in-port}. The
optional argument @var{max-no-char} limits the maxmum number of characters to be
read from the port; the search span is until EOF.

If @var{str} is found, the function returns the number of characters it has read
from the port, and the port is set to read the first char after that (that is,
after the @var{str}). If @var{str} is not found, the function returns #f.

Note: Although this procedure has @code{`?'} in its name, it may return
non-boolean value, contrary to the Scheme convention.
}

In the following functions, @var{char-list} refers to one of the following.
@itemlist[
@item{A character set which defined in SRFI-14.}
@item{A list of characters, character sets and/or symbol @code{*eof*}.}
]
That denotes a set of characters. If a symbol @code{*eof*} is included, the EOF
condition is also included.  Without @code{*eof*}, the EOF condition is regarded
as an error.

@define[Functions]{@name{assert-curr-char}
 @args{char-list string :optional port}}
@desc{Reads a character from the @var{port} and looks it up in the
@var{char-list} of expected characters. If the read character was found among
the expected, it is returned. Otherwise, the procedure writes a nasty message
using @var{string} as a comment, and quits.
}

@define[Functions]{@name{skip-until} @args{char-list/number :optional port}}
@desc{@var{Char-list/number} must be either char-list or number.

If it is a number; skips the specified number of characters from the port and
returns #f.

If it is a char-list; reads and skips characters from the port until one of the
break characters is encountered. This break character is returned. The break
characters are specified as the char-list. This list may include EOF, which is
to be coded as a symbol @code{*eof*}.
}

@define[Functions]{@name{skip-while} @args{char-list :optional port}}
@desc{Advances the @var{port} to the first character that is not a member of the
@var{char-list} -- or till the EOF, whichever occurs sooner. This character or
the EOF object is returned. This character is left on the stream.
}

@define[Functions]{@name{peek-next-char} @args{:optional port}}
@desc{Advances to the next character in the port and peeks at it. This function
is useful when parsing LR(1)-type languages.
}

@define[Functions]{@name{next-token}
 @args{prefix-char-list break-char-list :optional comment port}}
@desc{Skips any number of characters in @var{prefix-char-list}, then collects the
characters until it sees @var{break-char-list}. The collected characters are
returned as a string. The break character remains in the @var{port}.

If the function encounters EOF and @code{*eof*} is not included in
@var{break-char-list}, an error is signalled with @var{comment} is included in the
message.
}

@define[Functions]{@name{next-token-of} @args{char-list/pred :optional port}
@desc{Reads and collects the characters as far as it belongs to @var{char-list/pred},
then returns them as a string. The first character that doesn't belong to
@var{char-list/pred} remains on the @var{port}.

@var{Char-list/pred} may be a char-list or a predicate that takes a character.
If it is a predicate, each character is passed to it, and the character is
regarded to ``belong to'' @var{char-list/pred} when it returns a true value.
}

@define[Functions]{@name{read-string} @args{n :optional port}
@desc{Reads up to @var{n} characters, collects them into a string, and returns
it. If the input stream contains less characters, the returns string contains
as many characters available.

This function is similar with @code{get-string-n}.
}