@; -*- coding: utf-8 -*-
@subsection[:tag "text.html-parser"]{(text sxml html-parser) - Flxible HTML parser}


@define[Library]{@name{(text sxml html-parser)}}
@desc{This library provides HTML parser and printer procedures.

The provided procedures are compatible with HTMLPrag.
}

@subsubsection{High level APIs}

@define[Function]{@name{html->sxml-0nf} @args{in}}
@define[Function]{@name{html->sxml-1nf} @args{in}}
@define[Function]{@name{html->sxml-2nf} @args{in}}
@define[Function]{@name{html->sxml} @args{in}}
@define[Function]{@name{html->shtml} @args{in}}
@desc{Parse given input port to SHTML. The @var{in} must be a textual input
port.

The @code{html->sxml-2nf} procedure normalizes attributes if it doesn't have
value. Others don't. Normalization in this context means adding attribute name
as its value.
}

@define[Function]{@name{write-shtml-as-html}
 @args{shtml :optional out error-filter}}
@desc{Writes given @var{shtml} to @var{out}.

If optional argument @var{out} is given, it must be a textual output port,
then the procedure writes to given output port. Otherwise 
@code{current-output-port} is used.

Optional argument @var{error-filter} is used when unexpected SHTML entity is
met. It must be a procedure takes 2 arguments, unexpected entity and boolean
flag indicates if the entity is attribute or not. By default, it raises an
error.
}

@define[Function]{@name{shtml->html} @args{shtml}}
@desc{Converts @var{shtml} to string represented HTML.}

@subsubsection{Low level APIs}

@define[Variable]{@name{shtml-comment-symbol}}
@define[Variable]{@name{shtml-decl-symbol}}
@define[Variable]{@name{shtml-empty-symbol}}
@define[Variable]{@name{shtml-end-symbol}}
@define[Variable]{@name{shtml-entity-symbol}}
@define[Variable]{@name{shtml-pi-symbol}}
@define[Variable]{@name{shtml-start-symbol}}
@define[Variable]{@name{shtml-text-symbol}}
@define[Variable]{@name{shtml-top-symbol}}
@desc{SXML entity symbols: @code{*COMMENT*}, @code{*DECL*}, @code{*EMPTY*},
@code{*END*}, @code{*ENTITY*}, @code{*PI*}, @code{*START*}, @code{*TEXT*} and
@code{*TOP*} respectively.
}

@define[Variable]{@name{shtml-named-char-id}}
@define[Variable]{@name{shtml-numeric-char-id}}
@desc{These variables are used in @code{*ENTITY*} as public identifier.
}

@define[Function]{@name{make-shtml-entity} @args{value}}
@desc{Makes SHTML character entity. The @var{value} must be a symbol, string
or integer.

@snipet[=> "(& 151)"]{(make-shtml-entity 151)}
}

@define[Function]{@name{shtml-entity-value}}
@desc{Retrieve SHTML entity value.
}

@; TBD
@; @define[Function]{@name{make-html-tokenizer}}
@; @define[Function]{@name{tokenize-html}}
@; @define[Function]{@name{shtml-token-kind}}
@; @define[Function]{@name{parse-html/tokenizer}}
