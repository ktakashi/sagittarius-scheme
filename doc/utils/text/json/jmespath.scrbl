@; -*- coding: utf-8 -*-
@subsection[:tag "text.json.jmespath"]{(text json jmespath) - JMESPath}

@define[Library]{@name{(text json jmespath)}}
@desc{This library provides JMESPath procedures. JMESPath is defined on
@hyperlink[:href "http://jmespath.org/specification.html"]{JMESPath}.
}

The following example shows how to use the library:
@codeblock[=> "foo"]{
(import (rnrs) (text json jmespath))

((jmespath "a") '#(("a" . "foo") ("b" . "bar") ("c" . "baz")))
}

@subsubsection{Scheme APIs}

@define[Function]{@name{jmespath} @args{path}}
@desc{Returns a procedure takes one argument, which must be a vector
representad JSON.

The given @var{path} must be a string which is a valid JMESPath, otherwise
raises @code{&jmespath}.
}

@; TBD sexp JMESPath

@sub*section{Conditions}

This section describes conditions might be raised by the @code{jmespath}
procedure or the procedure returned by the @code{jmespath} procedure.

The library doesn't export the condition type itself. (e.g. @code{&jmespath}
isn't exported from the library). However for the comprehensivity, we
also describe the hierarchy of the conditions here:

@codeblock{
+ &error (standard R6RS error)
  + &jmespath
    + &jmespath:parse
    + &jmespath:expression
        - expression
        - argument
      + &jmespath:compile
      + &jmespath:runtime
}

The @code{&jmespath} is the root condition. This condition itself won't be
raised.

The @code{&jmespath:parse} is the condition raised by the parser. This means
either the given expression is lexically incorrect or grammartically incorrect.

The @code{&jmespath:expression} is the base condition of both
@code{&jmespath:compile} and @code{&jmespath:runtime}. This condition itself
won't be raised.

The @code{&jmespath:compile} is the condition raised by the compiler. This means
the parsed expression is syntatically incorrect.

The @code{&jmespath:runtime} is the condition raised by the returned procedure.
This means evaluation error. For example, a string is passed to the @code{avg}
function.

@define[Function]{@name{jmespath-error?} @args{obj}}
@desc{
Returns #t if the given @var{obj} is an instance of @code{&jmespath},
otherwise #f.
}

@define[Function]{@name{jmespath-parse-error?} @args{obj}}
@desc{
Returns #t if the given @var{obj} is an instance of @code{&jmespath:parse},
otherwise #f.

The @code{&jmespath:parse} is a sub condition of @code{&jmespath}.
}

@define[Function]{@name{jmespath-error-expression} @args{jmespath-error}}
@desc{Returns @code{expression} field of the given @var{jmespath-error}.

The the given @var{jmespath-error} must be a sub condition of
@code{&jmespath:expression}.
}

@define[Function]{@name{jmespath-error-arguments} @args{jmespath-error}}
@desc{Returns @code{arguments} field of the given @var{jmespath-error}.

The the given @var{jmespath-error} must be a sub condition of
@code{&jmespath:expression}.
}

@define[Function]{@name{jmespath-compile-error?} @args{obj}}
@desc{
Returns #t if the given @var{obj} is an instance of @code{&jmespath:compile},
otherwise #f.

The @code{&jmespath:compile} is a sub condition of @code{&jmespath:expression}.
}

@define[Function]{@name{jmespath-runtime-error?} @args{obj}}
@desc{
Returns #t if the given @var{obj} is an instance of @code{&jmespath:runtime},
otherwise #f.

The @code{&jmespath:runtime} is a sub condition of @code{&jmespath:expression}.
}

@subsubsection{Extra functions}

This library provides extra functions for usability.

@define["JMESPath Function"]{@name{parent} @args{node}}
@desc{Returns parent node of the given @var{node}. This function can be
used like this:

@codeblock[=> (#(("bar" . 1)))]{
((jmespath "*.bar.parent(@)") '#(("foo" . #(("bar" . 1)))))
}

A literal doesn't have a parent so returns @code{null}.
@codeblock[=> 'null]{
((jmespath "parent(`{}`)") '#(("foo" . #(("bar" . 1)))))
}
}

@define["JMESPath Function"]{@name{unique} @args{array}}
@desc{Returns unique elements of the given @var{array}. This function can be
used like this:

@codeblock[=> (1 2 3)]{
((jmespath "unique(@)") '(1 2 1 2 3))
}

It raises a @code{&jmespath:runtime} if the give @var{array} is not an array.
}

@define["JMESPath Function"]{@name{odd} @args{number}}
@desc{Returns #t if the given @var{number} is an odd number.
This function can be used like this:

@snipet[=> #t]{((jmespath "is_odd(@)") '5)}

It raises a @code{&jmespath:runtime} if the give @var{number} is not a number.
}
@define["JMESPath Function"]{@name{even} @args{number}}
@desc{Returns #t if the given @var{number} is an even number.
This function can be used like this:

@snipet[=> #t]{((jmespath "is_even(@)") '5)}

It raises a @code{&jmespath:runtime} if the give @var{number} is not a number.
}
