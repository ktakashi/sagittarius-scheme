@; -*- coding: utf-8 -*-

@subsection[:tag "text.tree"]{(text tree) - Lightweight text generation}

@define[Library]{@name{(text tree)}}
@desc{Defines simple but commonly used functions for a text construction.

This library is ported from Gauche.
}

@define[Generic]{@name{write-tree} @args{tree :optional out}}
@desc{Write out an @var{tree} as a tree of text, to the output port @var{out}.
If the @var{out} is omitted, then current output port is used.
}

@define[Function]{@name{tree->string} @args{tree}}
@desc{Just calls the @code{write-tree} method for @var{tree] using an output
string port, and returns the result string.
}