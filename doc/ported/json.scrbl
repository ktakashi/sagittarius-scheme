@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "ported.json"]{(json) -- JSON parser library}

@define[Library]{@name{(json)}}
@desc{This library is ported from Chicken Scheme json module and provides
JSON reader and writer.}

@define[Function]{@name{json-read} @args{:optional (port (current-input-port))}}
@desc{Reads JSON from given @var{port} and returns representing S-expression.

Conversion rules:
@codeblock{
JSON array   <-> list
JSON table   <-> vector
JSON boolean <-> boolean
JSON null    <-> symbol @code{null}
}

The procedure does not support @code{u} escape.
}

@define[Function]{@name{json-write}
 @args{json :optional (port (current-output-port))}}
@desc{Writes the given S-expression JSON representing object to given
@var{post}.
}
