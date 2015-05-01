@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "text.markdown"]{(text markdown) -- Markdown parser}

@define[Library]{@name{(text markdown)}}
@desc{This library provides markdown parser.

The library consists 3 parts, first one is the parser, second one is the
converter which converts markdown sexp to SHTML. And the last one is the
user APIs.
}


@subsubsection{High level APIs}

@define[Function]{@name{markdown-read}
 @args{in :key (as 'sxml) :allow-other-keys opt}}
@desc{Reads markdown from given input port @var{in}.

The keyword argument @var{as} specifies the return value. Following 3 are
supported:

@dl-list{
  @dl-item[@code{sxml}]{Returns SHTML. The procedure uses
   @code{markdown-sexp->sxml} to convert raw markdown S-expression.}
  @dl-item[@code{html}]{Returns HTML string.  The procedure uses
   @code{markdown-sexp->string} to convert raw markdown S-expression.}
  @dl-item[@code{sexp}]{Returns S-expression representation of markdown.}
}

Parsing markdown is done by @code{parse-markdown}. The rest argument @var{opt}
is passed to both @code{parse-markdown} and underlying convertion procedure.
So if you want to read a markdown document as an HTML string without
indentation, then the procedure should be called like the following:

@codeblock{
(markdown-read in :as 'html :no-indent #t)
}
}

@define[Function]{@name{string->markdown} @args{string :rest opt}}
@desc{Reads markdown from given @var{string}.

This procedure is thin wrapper of @code{markdown-read}. It opens string
input port of @var{string} and call the @code{markdown-read}.

The rest argument @var{opt} is passed to @code{markdown-read}.
}

@subsubsection{Supporting syntaxes}

This section describes which syntax is supported on this library. Markdown
has variety of dialect and it is rather impossible to provide all of them.
So this library only provides most common features.

@dl-list{
@; comments for emacs colouring.
@; atx heading
@dl-item["Atx heading"]{Starting @code{#} up to 6 of them is supported.

@codeblock[=> "<h1>header</h1>"]{# header\n}

The line can contain inline syntaxes and must end with line break. If it
doesn't have line break, then the line is considered plain text.
}

@; setext heading
@dl-item["Setext heading"]{A line which have @code{=} or @code{-} in its
next line is supported. The @code{=} or @code{-} can be more than one.

@codeblock[=> "<h1>header</h1>"]{
header
======
}

The fist line can contain inline syntaxes. The second line must end with
line break.
}

@; blockquote
@dl-item["Blockquote"]{A section which starts with @code{>} is supported.

@codeblock[=> "<blockquote>sentence1\\nsentence2</blockquote>"]{
> sentence1
> sentence2
}

Nested blockquote is not supported.
}

@; verbatim
@dl-item["Verbatim"]{A section which starts with @code{"    "} 
(4 spaces or one tab) is supported.

@codeblock[=> "<pre>thie\\n  is\\n    the sentence</pre>"]{
    this
      is
        the sentence
}

}

@; code block
@dl-item["Code block"]{A section which starts with @code{"```\n"} and end with
@code{"```\n"} is supported.

@codeblock[=> "<pre>(define (print . args)\\n  (for-each display args) (newline))</pre>"]{
```
(define (print . args)
  (for-each display args) (newline))
```
}

}

@; note
@dl-item["Note"]{A line which start with @code{"[^@var{$name}]: "} is 
supported. To refer this note, use @code{"[^@var{$name}]"}. @var{$name}
can be alphanumeric.

@codeblock[=> "<div id=\"notes\"><ol><li>note1</li></div>"]{
[^1]: note1
}

To refer:

@codeblock[=> "<p>Note to refer <sup><a href=\"#id\">1</a></sup>."]{
Note to refer [^1].
}

The generated HTML also contains @code{title} attribute with the value of
note.
}

@; reference
@dl-item["Reference"]{A line which start with @code{"[@var{$name}]: "} is 
supported. To refer this note, use @code{"[^@var{$name}]"}. @var{$name}
can be alphanumeric. To refer the reference, use @code{"[label][ref]"}.

@codeblock[=> "<div id=\"references\"><div>[Ref]: http://foo 'title'</div></div>"]{
[Ref]: http://foo (title)

or

[Ref]: http://foo 'title'

or

[Ref]: http://foo "title"
}

To refer:

@codeblock[=> "<a href=\"http://foo\">label</a>"]{
[label][Ref]
}

}

@; TODO
@; line
@; ordered-list
@; bullet-list
@; paragraph

@; TODO inlines
@; code
@; link
@; image
@; emph
@; strong

}


@subsubsection{Conversion APIs}

@define[Function]{@name{markdown-sexp->sxml}
 @args{sexp :key (no-reference #t) (no-notes #f) :allow-other-keys}}
@desc{Converts given markdown S-expression @var{sexp} to SXML.

The keyword arguments @var{no-reference} and @var{no-notes} control the
result SXML to have reference section and note section. If the values are
true values, then returning SXML doesn't have reference or note section.
}

@define[Function]{@name{markdown-sexp->string}
 @args{sexp :key (no-indent #f) :allow-other-keys opts}}
@desc{Converts given markdown S-expression @var{sexp} to HTML string.

The procedure first calls @code{markdown-sexp->sxml} passing @var{sexp} and
@var{opts} then converts the returned value to HTML string.

The keyword argument @var{no-indent} controls if the returning string
has indentation or not. If the value is true value, then returning string
doesn't have indentation.
}

@subsubsection{Parser APIs}

@define[Function]{@name{markdown-parser-error?} @args{obj}}
@desc{Returns #t if the given object is a markdown parser error condition,
otherwise #f.
}

@define[Function]{@name{markdown-parser-position} @args{condition}}
@define[Function]{@name{markdown-parser-expected} @args{condition}}
@desc{Returns @code{position} and @code{expexted} slot of the given
markdown parser error condition, respectively.
}

@define[Function]{@name{parse-markdown} @args{in :allow-other-keys}}
@desc{Parses given input port @var{in} and returns markdown S-expression.

The returning value is S-expression represented AST of markdown. The
structure is @strong{not} specified in this document yet, thus it might be
changed in future.
}
