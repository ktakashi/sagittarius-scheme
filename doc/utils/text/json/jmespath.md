[§2] (text json jmespath) - JMESPath {#text.json.jmespath}
-------------

###### [!Library] `(text json jmespath)` 

This library provides JMESPath procedures. JMESPath is defined on
[JMESPath](http://jmespath.org/specification.html).


The following example shows how to use the library:

``````````scheme
(import (rnrs) (text json jmespath))

((jmespath "a") '#(("a" . "foo") ("b" . "bar") ("c" . "baz")))
``````````
=> ``foo``

### [§3] Scheme APIs

###### [!Function] `jmespath`  _path_

Returns a procedure takes one argument, which must be a vector
representad JSON.

The given _path_ must be a string which is a valid JMESPath, otherwise
raises `&jmespath`.


#### [§4] Conditions

This section describes conditions might be raised by the `jmespath`procedure or the procedure returned by the `jmespath` procedure.

The library doesn't export the condition type itself. (e.g. `&jmespath`isn't exported from the library). However for the comprehensivity, we
also describe the hierarchy of the conditions here:

``````````scheme
+ &error (standard R6RS error)
  + &jmespath
    + &jmespath:parse
    + &jmespath:expression
        - expression
        - argument
      + &jmespath:compile
      + &jmespath:runtime
``````````

The `&jmespath` is the root condition. This condition itself won't be
raised.

The `&jmespath:parse` is the condition raised by the parser. This means
either the given expression is lexically incorrect or grammartically incorrect.

The `&jmespath:expression` is the base condition of both
`&jmespath:compile` and `&jmespath:runtime`. This condition itself
won't be raised.

The `&jmespath:compile` is the condition raised by the compiler. This means
the parsed expression is syntatically incorrect.

The `&jmespath:runtime` is the condition raised by the returned procedure.
This means evaluation error. For example, a string is passed to the `avg`function.

###### [!Function] `jmespath-error?`  _obj_

Returns #t if the given _obj_ is an instance of `&jmespath`,
otherwise #f.


###### [!Function] `jmespath-parse-error?`  _obj_

Returns #t if the given _obj_ is an instance of `&jmespath:parse`,
otherwise #f.

The `&jmespath:parse` is a sub condition of `&jmespath`.


###### [!Function] `jmespath-error-expression`  _jmespath-error_

Returns `expression` field of the given _jmespath-error_.

The the given _jmespath-error_ must be a sub condition of
`&jmespath:expression`.


###### [!Function] `jmespath-error-arguments`  _jmespath-error_

Returns `arguments` field of the given _jmespath-error_.

The the given _jmespath-error_ must be a sub condition of
`&jmespath:expression`.


###### [!Function] `jmespath-compile-error?`  _obj_

Returns #t if the given _obj_ is an instance of `&jmespath:compile`,
otherwise #f.

The `&jmespath:compile` is a sub condition of `&jmespath:expression`.


###### [!Function] `jmespath-runtime-error?`  _obj_

Returns #t if the given _obj_ is an instance of `&jmespath:runtime`,
otherwise #f.

The `&jmespath:runtime` is a sub condition of `&jmespath:expression`.


### [§3] Extra functions

This library provides extra functions for usability.

###### [!JMESPath Function] `parent`  _node_

Returns parent node of the given _node_. This function can be
used like this:

``````````scheme
((jmespath "*.bar.parent(@)") '#(("foo" . #(("bar" . 1)))))
``````````
=> ``'(#((bar . 1)))``

A literal doesn't have a parent so returns `null`.

``````````scheme
((jmespath "parent(`{}`)") '#(("foo" . #(("bar" . 1)))))
``````````
=> ``'null``



###### [!JMESPath Function] `unique`  _array_

Returns unique elements of the given _array_. This function can be
used like this:

``````````scheme
((jmespath "unique(@)") '(1 2 1 2 3))
``````````
=> ``'(1 2 3)``

It raises a `&jmespath:runtime` if the give _array_ is not an array.


###### [!JMESPath Function] `is_odd`  _number_

Returns #t if the given _number_ is an odd number.
This function can be used like this:

``((jmespath "is_odd(@)") '5)`` => ``#t``

It raises a `&jmespath:runtime` if the give _number_ is not a number.


###### [!JMESPath Function] `is_even`  _number_

Returns #t if the given _number_ is an even number.
This function can be used like this:

``((jmespath "is_even(@)") '5)`` => ``#t``

It raises a `&jmespath:runtime` if the give _number_ is not a number.


###### [!JMESPath Function] `remove`  _array/object_ _expr_

Removes element from the given _array/object_ if the _expr_returns true value.

The _array/object_ must be an array or object.

The _expr_ must be an expression reference. 

The _expr_ is executed in the context of the elements of _array/object_.
Means if the `@` is passed to the _expr_, then the receiving
value is one of the elements of the _array/object_.

This function can be used like this:

``((jmespath "remove(@, &odd(@))") '(1 2 3 4 5))`` => ``'(1 3 5)``

It raises a `&jmespath:runtime` if the give _array/object_ is not
either an array or object, or if the given _expr_ is not a function
reference.


###### [!JMESPath Function] `remove_entry`  _object_ _array/expr_

Removes entries from the given _object_ either
if _array/expr_ is an array of string and it contains the key of the entry
or if _array/expr_ is a function expression and returns true value.

The _object_ must be an object.

This function can be used like this:

``````````scheme
((jmespath "remove_entry(@, `[\"key2\"]`)") '#(("key" . 1)))
``````````
=> ``'#((key . 1))``

``````````scheme
((jmespath "remove_entry(@, &contains(`[\"key2\"]`, @))")
 '#(("key" . 1) ("key2" . 2)))
``````````
=> ``'#((key . 1))``

It raises a `&jmespath:runtime` if the give _object_ is not an object,
or if the given _array/expr_ is not an array of string or function
reference.


###### [!JMESPath Function] `array_of`  _any_ _..._

Returns array of given arguments _any_.

``((jmespath "array_of(`1`, `2`, `3`)") 'null)`` => ``'(1 2 3)``



