@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.uri-template"]{(rfc uri-template) - URI template}

@define[Library]{@name{(rfc uri-template)}}
@desc{This library provides RFC6570 'URI Template' procedures.}

@define[Function]{@name{parse-uri-template} @args{in}}
@desc{Parses the given URI template into S-expression.

The given @var{in} must be either a textual input port or a string.

The parsed form consists with the following rules:

@itemlist[
@item{uri-template ::= @code{(template-unit)}}
@item{template-unit ::= @code{string}}
@item{template-unit ::= @code{(template)}}
@item{template ::= @code{(operator varaible)}}
@item{operator ::= @code{character}}
@item{variable ::= @code{string}}
@item{variable ::= @code{(string modifier)}}
@item{modifier ::= @code{*}}
@item{modifier ::= @code{positive exact integer}}
]

@var{operator} must be a valid URI template operator

@snipet[=> ("http://" (("domain" *)) "/" (#\/ "path"))]{
(parse-uri-template "http://{domain*}/{/path}")
}
}

@define[Function]{@name{expand-uri-template} @args{uri-template parameter}}
@desc{Expands the given @var{uri-template} and returns a string.

The @var{parameter} must be a vector of pair of string and @var{value}.

The @var{value} must be one of the followings:
@itemlist[
@item{string}
@item{list of string}
@item{vector of pair of string}
]

@snipet[=> "?semi=%3B&dot=.&comma=%2C"]{
(expand-uri-template '((#\? ("keys" *))) '#(("keys" . #(("semi" . ";") ("dot" . ".") ("comma" . ",")))))
}
}
