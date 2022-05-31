[§2] (peg) - PEG library {#peg}
-------------

###### [!Library] `(peg)` 

A parser combinators library.

This library is named PEG (Paring Expression Grammar), howerver it
only provides parser combinator not syntactical layer, yet.


The following example shows parsing simple arithmetic expressions.

The first part of the example shows how to define the parser. The parser
is defined with the name of `calc:expr`. This parser accepts one
argument which is a lazy sequence provided by SRFI-127 `(srfi :127)`,
and returns 3 values, parse state, parser value and next input.

NOTE: A lazy sequence is a pair, whose `cdr` part might be a generator.
A normal pair can also be a lazy sequence.

``````````scheme
(import (rnrs)
	(peg)
	(peg chars)
	(srfi :14 char-sets)
	(srfi :121 generators)
	(srfi :127 lseqs))

(define ascii-digits (char-set-intersection char-set:digit char-set:ascii))
(define calc:num
  ($do (s ($optional ($or ($eqv? #\+) ($eqv? #\-)) #\+))
       (n ($many ($char-set-contains? ascii-digits) 1))
       ($return (string->number (apply string s n)))))
(define (calc:op c)
  ($seq ($many ($satisfy char-whitespace?))
	($eqv? c)
	($many ($satisfy char-whitespace?))))
(define calc:+ (calc:op #\+))
(define calc:* (calc:op #\*))
(define calc:open (calc:op #\())
(define calc:close (calc:op #\)))

(define calc:simple
  ($or ($do calc:open (a ($lazy calc:expr)) calc:close ($return a))
       calc:num))
(define calc:mulexp
  ($or ($do (a calc:simple) calc:* (b calc:simple) ($return (* a b)))
       calc:simple))
(define calc:expr
  ($or ($do (a calc:mulexp) calc:+ (b calc:mulexp) ($return (+ a b)))
       calc:mulexp))
``````````

The parser can be called like this:

``````````scheme
(calc:expr (generator->lseq (string->generator "1 + 2")))
``````````
=> ``#\<parse-succcess> 3 '()``

The parser doesn't check if the input is consumed entirely. So this also
returns success:

``````````scheme
(calc:expr (generator->lseq (string->generator "1 - 2")))
``````````
=> ``#\<parse-succcess> 1 '(#space #- #space #2)``

If you want to make sure that the entire input is consumed, then you need to
check if the next input is `()` or not.

NOTE: An input of parsers, which is lazy sequence, doesn't necessarily be a
character sequence. So users can implmenet own lexer.

NOTE2: The above example of the parser usage can also be like this:

``````````scheme
(calc:expr (string->list "1 + 2"))
``````````
=> ``#\<parse-succcess> 3 '()``

In this document, we always convert to a lazy sequence.

### [§3] Predefined parsers

###### [!Function] `$return`  _v_
###### [!Function] `$return`  _v_ _state_
###### [!Function] `$return`  _v_ _state_ _l_

Returns a parser which returns given _v_ as its value.

The second form specifies the state of parser result, which must be
one of the followings:

- `+parse-success+` - Indicating successful parse result
- `+parse-fail+` - Indicating failure parse result
- `+parse-expect+` - Indicating different from expected input
- `+parse-unexpect+` - Indicating unexpectedly matched input

The third form specifies the state described above and the next input.
The next input must be a lazy sequence.


###### [!Function] `$fail`  _message_

Returns a parser whose returning state is `+parse-fail+` and
return value is given _message_.


###### [!Function] `$expect`  _message_

Returns a parser whose returning state is `+parse-expect+` and
return value is given _message_.


###### [!Function] `$eof`  _input_

A parser which check if the _input_ is exhausted or not.

It returns success if the _input_ is exhausted otherwise
`+parse-expect+` as its state.


###### [!Function] `$any`  _input_

A parser which consume the first element of _input_ and returns
success.


###### [!Function] `$empty`  _v_

Returns a parser which returns success and given _v_ as its value
without consuming input.

In the context of BNF term, this is called epsilon.


###### [!Function] `$satisfy`  _pred_
###### [!Function] `$satisfy`  _pred_ _message_

Returns a parser which check if the first element of the input
of the parser satisfies the given _pred_ or not.

If the _pred_ returns true value, then it return success and the
first element. Otherwise `+parse-expect+`.

If the second form is used, then the _message_ is returned when
the _pred_ returned `#f`.


###### [!Function] `$not`  _parser_

Returns a parser which uses the given _parser_ as its parser
and check if the returning state is not successful or not.

If the state is successful, then returns `+parse-unexpect+`,
otherwise return successful and `#f` as its returning value.


###### [!Function] `$seq`  _parser_ _..._

Returns a parser which uses the given _parser_s as its parser.

It returns successful if all the given _parser_s return successful.
The returning value is the last parser's value. Otherwise
`+parse-expect+` is returned.


###### [!Function] `$or`  _parser_ _..._

Returns a parser which uses the given _parser_s as its parser.

It returns successful if one of the given _parser_s return successful.
The returning value is the first successful parser's value. Otherwise
`+parse-expect+` is returned.

If one of the parser returns `+parse-fail+`, then the entire parser
returned by this procedure fails. For example, the parser after the
`$fail` won't be evaluated.

``````````scheme
($or ($satisfy char-whitespace?)
     ($fail "Boom!")
     ($satisfy char-lower-case?))
``````````



###### [!Function] `$many`  _parser_
###### [!Function] `$many`  _parser_ _at-least_
###### [!Function] `$many`  _parser_ _at-least_ _at-most_

Returns a parser which uses the given _parser_ as its parser.

The parser parses the input as long as the given _parser_ returns
successful state. The returning value is a list of the values returned by
the given _parser_.

If the second or third form is used, then it limits the number of
trial.

_at-least_ specifies the number of minimum parse. If the given
_parser_ returned non successful state before this number, then
the parser return `+parse-expect+`.

_at-most_ specifies the number of maximum parse. If the parser
parsed _at-most_ input, then it returns successful even the
rest of the input contains the valid input of the given _parser_.


###### [!Function] `$peek`  _parser_

Returns a parser which uses the given _parser_ as its parser.

The parser returns successful if the given _parser_ returns successful.
The returning next input will not be consumed by the parser.


###### [!Function] `$eqv?`  _obj_

Returns a parser which compares the first element of the input
and the given _obj_ using `eqv?`.

If the comparison result is `#t`, then it returns successful.

This procedure is equivalent with the following:

``````````scheme
(define ($eqv? v) ($satisfy (lambda (e) (eqv? e v))))
``````````



###### [!Function] `$optional`  _parser_
###### [!Function] `$optional`  _parser_ _fallback_

Returns a parser which uses the given _parser_ as its parser.

The parser returns always successful state. The returning value is the
value of the given _parser_ if the _parser_ returns successful.
Otherwise `#f`.

If the second form is used, then _fallback_ is returned when the
_parser_ returned non successful.


###### [!Function] `$repeat`  _parser_ _n_

Returns a parser which uses the given _parser_ as its parser.

The parser parses input exactly _n_ times and returns a list of
result value if the given _parser_ returns _n_ times successful.

This is equivalent with the following code:

``````````scheme
(define ($repeat parser n) ($many parser n n))
``````````



###### [!Function] `$bind`  _parser_ _f_

Returns a parser which uses the given _parser_ as its parser.

The _f_ must be a procedure which takes one argument and returns a parser.

The parser returns the result of the parser created by _f_. The _f_ is
called when the given _parser_ returned successful state, passed the
returning value of the given _parser_.

The `$bind` is useful to take the result of the parsers.

This procedure is used to implement `$do` described below.


###### [!Macro] `$do`  _clause_ _._ _body_

A macro which creates a parser.

The `$do` macro makes users easier to bind variables. The syntax is
the following:

``````````scheme
$do    ::= ($do clause ... parser)
clause ::= (var parser)
         | (parser)
	 | parser
``````````

The following example shows how to bind a variable returned by the `$many`procedure.

``````````scheme
($do (c* ($many ($satisfy char-numeric?)))
     ($return (string->number (list->string c*))))
``````````

The above parser parses given numeric character sequence and returns a number.
The _c\*_ is the result of the _$many_.


###### [!Macro] `$lazy`  _parser_

A macro which creates a parser.

The `$lazy` macro delays the creation of _parser_. This macro is
useful to handle cross reference. For example:

``````````scheme
(define a ($do (c b) ($return c)))
(define b ($do (c a) ($return c)))
``````````

The above code causes unbound variable in runtime when the definition of
`a` is evaluated. To avoid this, users can use the `$lazy` like this:

``````````scheme
(define a ($do (c ($lazy b)) ($return c)))
(define b ($do (c a) ($return c)))
``````````



###### [!Macro] `$if`  _pred_ _then_ _else_

A macro which creates a parser.

The returning parser calls either _then_ or _else_ parser depending
on the result of _pred_.

This is the simple example.

``````````scheme
($if #t ($eqv? #\t) ($eqv? #\f))
;; ($eqv? #\t) will ba called
``````````

The `$if` can be used to dispatch parsers by the results like this:

``````````scheme
($do (c ($optional ($eqv? #\t)))
     ($if c
          ($return c)
	  ($return 'something)))
``````````



###### [!Macro] `$cond`  _clause_ _..._
###### [!Macro] `$cond`  _clause_ _..._ _(else_ _parser)_

A macro which creates a parser.

The _clause_ must be the following form:

``````````scheme
clause ::= (pred parser)
``````````

The parser returned by this macro is similar with the one from `$if`,
the difference is this macro can handle multiple predicates.


###### [!Macro] `$when`  _pred_ _parser_
###### [!Macro] `$unless`  _pred_ _parser_

A macro which creates a parser.

The `$when` macro returns a parser which calls the given _parser_only if the _pred_ returned true value.

The `$unless` macro returns a parser which calls the given _parser_only if the _pred_ returned `#f`.

They are defined like this:

``````````scheme
(define-syntax $when
  (syntax-rules ()
    ((_ pred body)
     ($if pred body ($fail 'pred)))))

(define-syntax $unless
  (syntax-rules ()
    ((_ pred body)
     ($when (not pred) body))))
``````````



###### [!Macro] `$parameterize`  _bindings_ _parser_

A macro which creates a parser.

The `$parameterize` macro returns a parser which calls the given
_parser_ in the extended dynamic extend with the given _bindings_.

The _bindings_ must be the following form:

``````````scheme
bindings ::= ((parameter value) ...)
``````````



###### [!Macro] `$guard`  _guard-clause_ _parser_

A macro which creates a parser.

The `$guard` macro returns a parser which is wrapped by the `guard`.
When the given _parser_ raises an error during parsing, then the
_guard-clause_ will be executed.

The _guard-clause_ must be the following form:
`guard-clause ::= (variable (pred clause) ...)
`The _pred_ must be a expression which checks if the raised condition
should be handled by the coupled _clause_ or not. If the _pred_is evaluated to true value, then the coupled _clause_ is called with
the input of the given _parser_.


### [§3] Character specific parsers

The procedures provided by the `(peg)` can be used for all kinds of
input. This section describes parsers which can only be used for character
input.

###### [!Library] `(peg chars)` 

Character specific PEG parser library.

###### [!Function] `$char-set-contains?`  _char-set_

Returns a parser which returns successful if the given
_char-set_ contains the input of the parser.

This procedure is defined like this:

``````````scheme
(define ($char-set-contains? s) 
  ($satisfy (lambda (c) (char-set-contains? s c)) s))
``````````



###### [!Function] `$token`  _string_

Returns a parser which returns successful if the input of
the parser matches with the given _string_.

This procedure is defined like this:

``````````scheme
(define ($token s) (apply $seq (map $eqv? (string->list s))))
``````````



