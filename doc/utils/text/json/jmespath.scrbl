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

@codeblock[=> '(#(("bar" . 1)))]{
((jmespath "*.bar.parent(@atmark{})") '#(("foo" . #(("bar" . 1)))))
}

A literal doesn't have a parent so returns @code{null}.
@codeblock[=> 'null]{
((jmespath "parent(`{}`)") '#(("foo" . #(("bar" . 1)))))
}
}

@define["JMESPath Function"]{@name{unique} @args{array}}
@desc{Returns unique elements of the given @var{array}. This function can be
used like this:

@codeblock[=> '(1 2 3)]{
((jmespath "unique(@atmark{})") '(1 2 1 2 3))
}

It raises a @code{&jmespath:runtime} if the give @var{array} is not an array.
}

@define["JMESPath Function"]{@name{odd} @args{number}}
@desc{Returns #t if the given @var{number} is an odd number.
This function can be used like this:

@snipet[=> #t]{((jmespath "is_odd(@atmark{})") '5)}

It raises a @code{&jmespath:runtime} if the give @var{number} is not a number.
}

@define["JMESPath Function"]{@name{even} @args{number}}
@desc{Returns #t if the given @var{number} is an even number.
This function can be used like this:

@snipet[=> #t]{((jmespath "is_even(@atmark{})") '5)}

It raises a @code{&jmespath:runtime} if the give @var{number} is not a number.
}

@define["JMESPath Function"]{@name{remove} @args{array/object expr}}
@desc{Removes element from the given @var{array/object} if the @var{expr}
returns true value.

The @var{array/object} must be an array or object.

The @var{expr} must be an expression reference. 

The @var{expr} is executed in the context of the elements of @var{array/object}.
Means if the @code{@atmark{}} is passed to the @var{expr}, then the receiving
value is one of the elements of the @var{array/object}.

This function can be used like this:

@snipet[=> '(1 3 5)]{((jmespath "remove(@atmark{}, &odd(@atmark{}))") '(1 2 3 4 5))}

It raises a @code{&jmespath:runtime} if the give @var{array/object} is not
either an array or object, or if the given @var{expr} is not a function
reference.
}

@define["JMESPath Function"]{@name{remove_entry} @args{object array/expr}}
@desc{Removes entries from the given @var{object} either
if @var{array/expr} is an array of string and it contains the key of the entry
or if @var{array/expr} is a function expression and returns true value.

The @var{object} must be an object.

This function can be used like this:

@codeblock[=> '#(("key" . 1))]{
((jmespath "remove_entry(@atmark{}, `[\"key2\"]`)") '#(("key" . 1)))
}
@codeblock[=> '#(("key" . 1))]{
((jmespath "remove_entry(@atmark{}, &contains(`[\"key2\"]`, @atmark{}))")
 '#(("key" . 1) ("key2" . 2)))
}

It raises a @code{&jmespath:runtime} if the give @var{object} is not an object,
or if the given @var{array/expr} is not an array of string or function
reference.
}
