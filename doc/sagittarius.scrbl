@; -*- mode:scribble; coding: utf-8 -*-

@section[:tag "ext.sagittarius"]{Sagittarius extensions}

Sagittarius has its own extension libraries because even R6RS is huge however I
know it is not sufficient to write practical program. To support to write it,
Sagittarius provides some useful libraries.

@subsection[:tag "lib.sagittarius"]{(sagittarius) - builtin library}

@define[Library]{@name{(sagittarius)}}
@desc{This library has Sagittarius specific functions which are not supported
in R6RS such as extra file system functions and so.
}

@subsubsection{File system operations}

@define[Function]{@name{file-size-in-bytes} @args{filename}}
@desc{Returns file size of filename in bytes. If filename does not exist, it
raises @code{&assertion} condition.
}

@define[Function]{@name{file-regular?} @args{filename}}
@define[Function]{@name{file-directory?} @args{filename}}
@define[Function]{@name{file-symbolic-link?} @args{filename}}
@define[Function]{@name{file-readable?} @args{filename}}
@define[Function]{@name{file-writable?} @args{filename}}
@define[Function]{@name{file-executable?} @args{filename}}
@desc{Returns file type or permission of given @var{filename}.}

@define[Function]{@name{file-stat-ctime} @args{filename}}
@define[Function]{@name{file-stat-mtime} @args{filename}}
@define[Function]{@name{file-stat-atime} @args{filename}}
@desc{Returns file statistics time in nano sec.

The @code{file-stat-ctime} procedure returns last change time of @var{filename}.

The @code{file-stat-mtime} returns last modified time of @var{filename}.

The @code{file-stat-atime} returns last accesse time of @var{filename}.
}

@define[Function]{@name{create-symbolic-link} @args{old-filename new-filename}}
@desc{Creates symbolic link of @var{old-filename} as @var{new-filename}.}

@define[Function]{@name{rename-file} @args{old-filename new-filename}}
@desc{Renames given @var{old-filename} to @var{new-filename}.

If @var{old-filename} does not exist, it raises @code{&assertion}.

If @var{new-filename} exists, it overwrite the existing file.
}

@define[Function]{@name{create-directory} @args{path}}
@define[Function]{@name{delete-directory} @args{path}}
@desc{Creates/deletes given directory. If it fails, it raises condition
@code{&assertion}.
}

@define[Function]{@name{read-directory} @args{path}}
@desc{Reads directory and returns contents as a string list. If @var{path} does
not exist, it returns #f.
}

@define[Function]{@name{current-directory}}
@desc{Returns current working directory.}

@define[Function]{@name{set-current-directory} @{path}}
@desc{Sets current working directory to @var{path}.}

@subsubsection{Hashtables}

@define[Function]{@name{make-equal-hashtable} @args{:optional k}}
@define[Function]{@name{make-string-hashtable} @args{:optional k}}
@desc{Creates a hashtable. The same as @code{make-eq-hashtable} and
@code{make-eqv-hashtable}. It uses @code{equal?} or @code{string=?} as comparing
procedure, respectively.
}

@define[Function]{@name{hashtable-values} @args{hashtable}}
@desc{Returns all @var{hashtable}'s values. This procedure is for consistancy of
@code{hashtable-keys}.}

@define[Function]{@name{hashtable-keys-list} @args{hashtable}}
@define[Function]{@name{hashtable-values-list} @args{hashtable}}
@desc{Returns given @var{hashtable}'s @var{keys} or @var{values}, respectively.

The R6RS required procedure @code{hashtable-keys} and @code{hashtable-values}
are implemented with these procedures.
}

@define[Function]{@name{hashtable-type} @args{hashtable}}
@desc{Returns @var{hashtable}'s hash type as a symbol. The possible return values
are @code{eq}, @code{eqv}, @code{equal}, @code{string} and @code{general}.
}

@subsubsection{I/O}

@define[Function]{@name{port-closed?} @args{port}}
@desc{Returns #t if given @var{port} is closed, otherwise #f.}

@define[Function]{@name{read/ss}
 @args{:optional (port @code{(current-input-port)})}}
@define[Function]{@name{write/ss}
 @args{obj :optional (port @code{(current-output-port)})}}
@desc{[SRFI-38] The @code{read/ss} reads a datum from given @var{port}.

The @code{write/ss} writes @var{obj} to given @var{port}.

These are the same as @code{read} and @code{write} procedure, but it can handle
circular list.
}

@define[Function]{@name{format} @args{port string arg @dots{}}}
@define[Function]{@name{format} @args{string arg @dots{}}}
@desc{[SRFI-28+] Formats @var{arg} accoding to @var{string}. @var{Port} specifies
the destination; if it is an output port, the formatted result is written to it;
if it is #t, the result is written to current output port; if it is #f, the
formatted result is returned as a string. @var{Port} can be omitted and the result
is the same as when #f is given.

@var{String} is a string that contains format directives. A format directive is a
character sequence begins with tilda @code{'~'}, and ends with some specific
characters. A format directive takes the corresponding arg and formats it. The
rest of string is copied to the output as is.

@snipet[=> "the anser is 48"]{(format #f “the answer is ~a” 48)}

The format directive can take one or more parameters, separated by comma characters.
A parameter may be an integer or a character; if it is a character, it should be
preceded by a quote character. Parameter can be omitted, in such case the system
default value is used. The interpretation of the parameters depends on the format
directive.

Furthermore, a format directive can take two additional flags: atmark @code{'@atmark{}'} and
colon @code{':'}. One or both of them may modify the behavior of the format directive.
Those flags must be placed immediately before the directive character.

The following complete list of the supported directives. Either upper case or lower
case character can be used for the format directive; usually they have no distinction,
except noted.

@dl-list[
@dl-item[@string{~@var{mincol},@var{colinc},@var{minpad},@var{padchar},@var{maxcol}@b{A}}]{
Ascii output. The corresponding argument is printed by @code{display}. If an integer
@var{mincol} is given, it specifies the minimum number of characters to be output;
if the formatted result is shorter than @var{mincol}, a whitespace is padded to
the right (i.e. the result is left justified).

The @var{colinc}, @var{minpad} and @var{padchar} parameters control, if given,
further padding. A character padchar replaces the padding character for the
whitespace. If an integer @var{minpad} is given and greater than 0, at least
@var{minpad} padding character is used, regardless of the resulting width. If an
integer @var{colinc} is given, the padding character is added (after minpad) in
chunk of @var{colinc} characters, until the entire width exceeds @var{mincol}.

If atmark-flag is given, the format result is right justified, i.e. padding is
added to the left.

The @var{maxcol} parameter, if given, limits the maximum number of characters to
be written. If the length of formatted string exceeds @var{maxcol}, only
@var{maxcol} characters are written. If colon-flag is given as well and the length
of formatted string exceeds @var{maxcol}, @var{maxcol} - 4 characters are written
and a string @code{" @dots{}"} is attached after it.

@snipet[=> "|oops|"       ]{(format #f "|~a|" "oops")}
@snipet[=> "|oops      |" ]{(format #f "|~10a|" "oops")}
@snipet[=> "|      oops|" ]{(format #f "|~10@a|" "oops")}
@snipet[=> "|******oops|" ]{(format #f "|~10,,,'*@a|" "oops")}
@snipet[=> "|abc def gh|" ]{(format #f "|~,,,,10a|" '(abc def ghi jkl))}
@snipet[=> "|abc de ...|" ]{(format #f "|~,,,,10:a|" '(abc def ghi jkl))}
}
@dl-item[@string{~@var{mincol},@var{colinc},@var{minpad},@var{padchar},@var{maxcol}@b{S}}]{
S-expression output. The corresponding argument is printed by @code{write}. The
semantics of parameters and flags are the same as @b{~A} directive.

@snipet[=> "|\"oops\"|"	   ]{(format #f "|~s|" "oops")}
@snipet[=> "|\"oops\"    |"]{(format #f "|~10s|" "oops")}
@snipet[=> "|    \"oops\"|"]{(format #f "|~10@s|" "oops")}
@snipet[=> "|****\"oops\"|"]{(format #f "|~10,,,'*@s|" "oops")}
}

@dl-item[@string{~@var{mincol},@var{padchar},@var{commachar},@var{interval}@b{D}}]{
Decimal output. The argument is formatted as an decimal integer. If the argument
is not an integer, all parameters are ignored and it is formatted by @b{~A}
directive.

If an integer parameter @var{mincol} is given, it specifies minimum width of the
formatted result; if the result is shorter than it, @var{padchar} is padded on
the left (i.e. the result is right justified). The default of padchar is a whitespace.

@snipet[=> "|12345|"     ]{(format #f "|~d|" 12345)}
@snipet[=> "|     12345|"]{(format #f "|~10d|" 12345)}
@snipet[=> "|0000012345|"]{(format #f "|~10,'0d|" 12345)}

If atmark-flag is given, the sign @code{'+'} is printed for the positive argument.

If colon-flag is given, every @var{interval}th digit of the result is grouped and
commachar is inserted between them. The default of @var{commachar} is @code{','},
and the default of @var{interval} is 3.

@snipet[=> "|12,345|"    ]{(format #f "|~:d|" 12345)}
@snipet[=> "|-1234_5678|"]{(format #f "|~,,'_,4:d|" -12345678)}
}
@dl-item[@string{~@var{mincol},@var{padchar},@var{commachar},@var{interval}@b{B}}]{
Binary output. The argument is formatted as a binary integer. The semantics of
parameters and flags are the same as the @b{~D} directive.
}
@dl-item[@string{~@var{mincol},@var{padchar},@var{commachar},@var{interval}@b{O}}]{
Octet output. The argument is formatted as a octal integer. The semantics of
parameters and flags are the same as the @b{~D} directive.
}
@dl-itemx[2
@string{~@var{mincol},@var{padchar},@var{commachar},@var{interval}@b{X}}
@string{~@var{mincol},@var{padchar},@var{commachar},@var{interval}@b{x}}]{
Hexadecimal output. The argument is formatted as a hexadecimal integer. If
@code{'X'} is used, upper case alphabets are used for the digits larger than 10.
If @code{'x'} is used, lower case alphabets are used. The semantics of parameters
and flags are the same as the @b{~D} directive.
@snipet[=> "0f7cf5a8"]{(format #f "~8,'0x" 259847592)}
@snipet[=> "0F7CF5A8"]{(format #f "~8,'0X" 259847592)}
}
]
Note: The format procedure's implementation and most of documentation is quoted
from Gauche.
}

@define[Function]{@name{make-codec} @args{symbol getc putc data}}
@desc{Creates a custom codec. @var{Symbol} is the name of the codec. @var{Getc}
and @var{putc} must be procedures. @var{Data} is an user data used in @var{getc}
and @var{putc}.

@var{Getc} must take 4 arguments, @var{input-port}, @var{error-handling-mode},
@var{check-bom?} and @var{userdata}. @var{Input-port} is binary input port.
@var{Error-handling-mode} is symbol can be @code{ignore}, @code{raise} or
@code{replace} depending on a transcoder which uses this custom codec.
@var{Check-bom?} is boolean, if @var{getc} is being called first time, it is #t,
otherwise #f. @var{Userdata} is user defined data which is given when the codec
is created.

The basic process of @var{getc} is reading binary data from @var{input-port} and
converting the data to UCS4. Returning UCS4 must be integer and does not have to
be 4 byte.

@var{Putc} must take 4 arguments, @var{output-port}, @var{char}, 
@var{error-handling-mode} and @var{userdata}. @var{Output-port} is binary output
port. @var{Char} is character object which needs to be converted from UCS4.
@var{Error-handling-mode} is symbol can be @code{ignore}, @code{raise} or
@code{replace} depending on a transcoder which uses this custom codec. @var{Userdata}
is user defined data which is given when the codec is created.

The basic process of @var{putc} is converting given UCS4 charactner to target
encoding data and putting it to @var{output-port} as binary data.

For sample implementation, see sitelib/encoding directory. You can find some
custom codecs.
}

@subsubsection{Keywords}

Sagittarius has keyword objects which starts with @code{':'}. It has almost the
same feature as symbol, however it can not be bounded with any values. It can be
used when variable is bounded by @code{define-with-key} (see
@secref["sagittarius.control"]{@code{(sagittarius control)}}library).

@define[Function]{@name{make-keyword} @args{symbol}}
@desc{Creates a new keyword from @var{symbol}.}

@define[Function]{@name{keyword?} @args{obj}}
@desc{Returns #t if @var{obj} is keyword, otherwise #f.
}

@subsubsection{Bytevector operations}

@define[Function]{@name{bytevector->integer} @args{bytevector :optional start end}}
@desc{Converts given bytevector @var{bytevector} to exact integer. If optional
argument @var{start} is given, conversion starts with index @var{start}. If
optional argument @var{end} is given, convertion ends by index @var{end}.
}

@define[Function]{@name{integer->bytevector} @args{ei}}
@desc{@var{Ei} must be exact integer. Converts exact integer @var{ei} to bytevector.}

@subsubsection{List operations}

@define[Function]{@name{circular-list?} @args{list}}
@define[Function]{@name{dotted-list?} @args{list}}
@desc{[SRFI-1] Returns #t if @var{list} is circular or dotted list, respectively. 
Otherwise #f.}

@define[Function]{@name{acons} @args{obj1 obj2 obj3}}
@desc{Returns @code{(cons (cons @var{obj1} @var{obj2}) @var{obj3})}. Useful to
put an entry at the head of an associative list.
}

@define[Function]{@name{append!} @args{list @dots{}}}
@desc{[SRFI-1] Returns a list consisting of the elements of the first @var{list}
followed by the elements of the other @var{lists}. The cells in the lists except
the last one may be reused to construct the result. The last argument may be any
object.
}

@define[Function]{@name{reverse!} @args{list @dots{}}}
@desc{[SRFI-1] Returns a list consisting of the elements of @var{list} in reverse
order. The cells of list may be reused to construct the returned list.
}

@subsubsection{Vector operations}

@define[Function]{@name{vector-copy} @args{vector :optional start end fill}}
@desc{Copies a vector @var{vector}. Optional @var{start} and @var{end} arguments
can be used to limit the range of @var{vector} to be copied. If the range
specified by @var{start} and @var{end} falls outside of the original vector, the
@var{fill} value is used to fill the result vector.
}

@include-section["sagittarius/regex.scrbl"]
@include-section["sagittarius/socket.scrbl"]
@include-section["sagittarius/control.scrbl"]
