@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "text.json"]{(text json) -- JSON parser}

@define[Library]{@name{(text json)}}
@desc{This library provides JSON parser and writer. 
}

@define[Parameter]@{@name{*json-map-type*}}
@desc{Controls mapping of JSON map and array. The value must be either
@code{'vector} or @code{'alist}. The first one is compatible with Chicken
Scheme's json module, the latter one is compatible with Gauche's 
@code{rfc.json} module. By default, it's set to @code{vector} for backward
compatibility.

Conversion rules for @code{vector}:
@codeblock{
JSON array   <-> list
JSON map     <-> vector
JSON boolean <-> boolean
JSON null    <-> symbol @code{null}
}

Conversion rules for @code{alist}:
@codeblock{
JSON array   <-> vector
JSON map     <-> alist
JSON boolean <-> boolean
JSON null    <-> symbol @code{null}
}

This parameter affects the read and write procedures.
}

@define[Function]{@name{json-read} @args{:optional (port (current-input-port))}}
@desc{Reads JSON from given @var{port} and returns representing S-expression.}

@define[Function]{@name{json-write}
 @args{json :optional (port (current-output-port))}}
@desc{Writes the given S-expression JSON representing object to given
@var{port}.
}
