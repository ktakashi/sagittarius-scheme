@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "ported.json"]{(json) -- JSON parser library}

@define[Library]{@name{(json)}}
@desc{This library is compatible with Chicken Scheme json module and provides
JSON reader and writer. The library is a thin wrapper of 
@secref["text.json"]{(text json)} library.
}

@define[Function]{@name{json-read} @args{:optional (port (current-input-port))}}
@desc{Reads JSON from given @var{port} and returns representing S-expression.

Conversion rules:
@codeblock{
JSON array   <-> list
JSON table   <-> vector
JSON boolean <-> boolean
JSON null    <-> symbol @code{null}
}

Read and write procedure always use above conversion rules even if 
@code{*json-map-type*} is set to @code{'alist}.

}

@define[Function]{@name{json-write}
 @args{json :optional (port (current-output-port))}}
@desc{Writes the given S-expression JSON representing object to given
@var{port}.
}
