@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "text.yaml"]{(text yaml) -- YAML parser}

@define[Library]{@name{(text yaml)}}
@desc{This library provides YAML parser and writer.}

@define[Function]{@name{yaml-read} @args{in}}
@desc{Reads YAML documents from the given textual input port @var{in} and
returns a list of S-expression represented YAML documents.

The returning YAML documents are the compatible with vector JSON
format described in @secref["text.json"]{(text json)}.

@codeblock[=> '(#(("foo" . "bar") ("boo" 1 2 3 4)))]{
(yaml-read (open-string-input-port "
%YAML 1.2
---
foo: bar
boo:
- 1
- 2
- 3
- 4"))
}
}

@define[Function]{@name{yaml-write} @args{yaml}}
@define[Function]{@name{yaml-write} @args{yaml out}}
@desc{Writes the given list of S-expression represented YAML documents
@var{yaml} to the @var{out}.

If the first form is used, then it writes to @code{(current-output-port)}.

@codeblock{
(yaml-write '(#(("foo" . "bar") ("boo" 1 2 3 4))))
;; Prints the following
#|
%YAML 1.2
---
foo: bar
boo:
- 1
- 2
- 3
- 4
...
|#
}
}

@; TBD custom builder and serializer
