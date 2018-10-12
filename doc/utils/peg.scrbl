@; -*- coding: utf-8 -*-
@subsection[:tag "peg"]{(peg) - PEG library}

@define[Library]{@name{(peg)}}
@desc{A parser combinators library.

This library is named PEG (Paring Expression Grammar), howerver it
only provides parser combinator not syntactical layer, yet.
}

The following example shows parsing simple arithmetic expressions.

The first part of the example shows how to define the parser. The parser
is defined with the name of @code{calc:expr}. This parser accepts one
argument which is a lazy sequence provided by SRFI-127 @code{(srfi :127)},
and returns 3 values, parse state, parser value and next input.

NOTE: A lazy sequence is a pair, whose @code{cdr} part might be a generator.
A normal pair can also be a lazy sequence.

@codeblock{
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
}

The parser can be called like this:

@codeblock[=> "#<parse-succcess> 3 '()"]{
(calc:expr (generator->lseq (string->generator "1 + 2")))
}

The parser doesn't check if the input is consumed entirely. So this also
returns success:

@codeblock[=> "#<parse-succcess> 1 '(#\space #\- #\space #\2)"]{
(calc:expr (generator->lseq (string->generator "1 - 2")))
}

If you want to make sure that the entire input is consumed, then you need to
check if the next input is @code{()} or not.

NOTE: An input of parsers, which is lazy sequence, doesn't necessarily be a
character sequence. So users can implmenet own lexer.

NOTE2: The above example of the parser usage can also be like this:
@codeblock[=> "#<parse-succcess> 3 '()"]{
(calc:expr (string->list "1 + 2"))
}
In this document, we always convert to a lazy sequence.

@subsubsection{Predefined parsers}

@define[Function]{@name{$return} @args{v}}
@define[Function]{@name{$return} @args{v state}}
@define[Function]{@name{$return} @args{v state l}}
@desc{Returns a parser which returns given @var{v} as its value.

The second form specifies the state of parser result, which must be
one of the followings:
@itemlist{
@item{@code{+parse-success+} - Indicating successful parse result}
@item{@code{+parse-fail+} - Indicating failure parse result}
@item{@code{+parse-expect+} - Indicating different from expected input}
@item{@code{+parse-unexpect+} - Indicating unexpectedly matched input}
}

The third form specifies the state described above and the next input.
The next input must be a lazy sequence.
}

@define[Function]{@name{$fail} @args{message}}
@desc{Returns a parser whose returning state is @code{+parse-fail+} and
return value is given @var{message}.
}

@define[Function]{@name{$expect} @args{parser message}}
@desc{Returns a parser which uses the given @var{parser} to parse its input,
and if the result state is not success, then return @code{+parse-expect+}
with the given @var{message} as its return value.

This is useful to provide detail message of the failure.
}

@define[Function]{@name{$eof} @args{input}}
@desc{A parser which check if the @var{input} is exhausted or not.

It returns success if the @var{input} is exhausted otherwise
@code{+parse-expect+} as its state.
}

@define[Function]{@name{$any} @args{input}}
@desc{A parser which consume the first element of @var{input} and returns
success.
}

@define[Function]{@name{$empty} @args{v}}
@desc{Returns a parser which returns success and given @var{v} as its value
without consuming input.

In the context of BNF term, this is called epsilon.
}

@define[Function]{@name{$satisfy} @args{pred}}
@define[Function]{@name{$satisfy} @args{pred message}}
@desc{Returns a parser which check if the first element of the input
of the parser satisfies the given @var{pred} or not.

If the @var{pred} returns true value, then it return success and the
first element. Otherwise @code{+parse-expect+}.

If the second form is used, then the @var{message} is returned when
the @var{pred} returned @code{#f}.
}

@define[Function]{@name{$not} @args{parser}}
@desc{Returns a parser which uses the given @var{parser} as its parser
and check if the returning state is not successful or not.

If the state is successful, then returns @code{+parse-unexpect+},
otherwise return successful and @code{#f} as its returning value.
}

@define[Function]{@name{$seq} @args{parser @dots{}}}
@desc{Returns a parser which uses the given @var{parser}s as its parser.

It returns successful if all the given @var{parser}s return successful.
The returning value is the last parser's value. Otherwise
@code{+parse-expect+} is returned.
}

@define[Function]{@name{$or} @args{parser @dots{}}}
@desc{Returns a parser which uses the given @var{parser}s as its parser.

It returns successful if one of the given @var{parser}s return successful.
The returning value is the first successful parser's value. Otherwise
@code{+parse-expect+} is returned.

If one of the parser returns @code{+parse-fail+}, then the entire parser
returned by this procedure fails. For example, the parser after the
@code{$fail} won't be evaluated.
@codeblock{
($or ($satisfy char-whitespace?)
     ($fail "Boom!")
     ($satisfy char-lower-case?))
}
}

@define[Function]{@name{$many} @args{parser}}
@define[Function]{@name{$many} @args{parser at-least}}
@define[Function]{@name{$many} @args{parser at-least at-most}}
@desc{Returns a parser which uses the given @var{parser} as its parser.

The parser parses the input as long as the given @var{parser} returns
successful state. The returning value is a list of the values returned by
the given @var{parser}.

If the second or third form is used, then it limits the number of
trial.

@var{at-least} specifies the number of minimum parse. If the given
@var{parser} returned non successful state before this number, then
the parser return @code{+parse-expect+}.

@var{at-most} specifies the number of maximum parse. If the parser
parsed @var{at-most} input, then it returns successful even the
rest of the input contains the valid input of the given @var{parser}.
}

@define[Function]{@name{$peek} @args{parser}}
@desc{Returns a parser which uses the given @var{parser} as its parser.

The parser returns successful if the given @var{parser} returns successful.
The returning next input will not be consumed by the parser.
}

@; derived
@define[Function]{@name{$eqv?} @args{obj}}
@desc{Returns a parser which compares the first element of the input
and the given @var{obj} using @code{eqv?}.

If the comparison result is @code{#t}, then it returns successful.

This procedure is equivalent with the following:
@codeblogk{
(define ($eqv? v) ($satisfy (lambda (e) (eqv? e v))))
}
}

@define[Function]{@name{$optional} @args{parser}}
@define[Function]{@name{$optional} @args{parser fallback}}
@desc{Returns a parser which uses the given @var{parser} as its parser.

The parser returns always successful state. The returning value is the
value of the given @var{parser} if the @var{parser} returns successful.
Otherwise @code{#f}.

If the second form is used, then @var{fallback} is returned when the
@var{parser} returned non successful.
}

@define[Function]{@name{$repeat} @args{parser n}}
@desc{Returns a parser which uses the given @var{parser} as its parser.

The parser parses input exactly @var{n} times and returns a list of
result value if the given @var{parser} returns @var{n} times successful.

This is equivalent with the following code:
@codeblogk{
(define ($repeat parser n) ($many parser n n))
}
}

@define[Function]{@name{$bind} @args{parser f}}
@desc{Returns a parser which uses the given @var{parser} as its parser.

The @var{f} must be a procedure which takes one argument and returns a parser.

The parser returns the result of the parser created by @var{f}. The @var{f} is
called when the given @var{parser} returned successful state, passed the
returning value of the given @var{parser}.

The @code{$bind} is useful to take the result of the parsers.

This procedure is used to implement @code{$do} described below.
}

@define[Macro]{@name{$do} @args{clause @dot{} body}}
@desc{A macro which creates a parser.

The @code{$do} macro makes users easier to bind variables. The syntax is
the following:
@codeblock{
$do    ::= ($do clause ... parser)
clause ::= (var parser)
         | (parser)
	 | parser
}
The following example shows how to bind a variable returned by the @code{$many}
procedure.

@codeblock{
($do (c* ($many ($satisfy char-numeric?)))
     ($return (string->number (list->string c*))))
}
The above parser parses given numeric character sequence and returns a number.
The @var{c*} is the result of the @var{$many}.
}

@define[Macro]{@name{$lazy} @args{parser}}
@desc{A macro which creates a parser.

The @code{$lazy} macro delays the creation of @var{parser}. This macro is
useful to handle cross reference. For example:
@codeblock{
(define a ($do (c b) ($return c)))
(define b ($do (c a) ($return c)))
}
The above code causes unbound variable in runtime when the definition of
@code{a} is evaluated. To avoid this, users can use the @code{$lazy} like this:
@codeblock{
(define a ($do (c ($lazy b)) ($return c)))
(define b ($do (c a) ($return c)))
}
}

@define[Macro]{@name{$if} @args{pred then else}}
@desc{A macro which creates a parser.

The returning parser calls either @var{then} or @var{else} parser depending
on the result of @var{pred}.

This is the simple example.
@codeblock{
($if #t ($eqv? #\t) ($eqv? #\f))
;; ($eqv? #\t) will ba called
}

The @code{$if} can be used to dispatch parsers by the results like this:
@codeblock{
($do (c ($optional ($eqv? #\t)))
     ($if c
          ($return c)
	  ($return 'something)))
}
}

@define[Macro]{@name{$cond} @args{clause @dots{}}}
@define[Macro]{@name{$cond} @args{clause @dots{} (else parser)}}
@desc{A macro which creates a parser.

The @var{clause} must be the following form:
@codeblock{
clause ::= (pred parser)
}

The parser returned by this macro is similar with the one from @code{$if},
the difference is this macro can handle multiple predicates.
}

@define[Macro]{@name{$when} @args{pred parser}}
@define[Macro]{@name{$unless} @args{pred parser}}
@desc{A macro which creates a parser.

The @code{$when} macro returns a parser which calls the given @var{parser}
only if the @var{pred} returned true value.

The @code{$unless} macro returns a parser which calls the given @var{parser}
only if the @var{pred} returned @code{#f}.

They are defined like this:
@codeblock{
(define-syntax $when
  (syntax-rules ()
    ((_ pred body)
     ($if pred body ($fail 'pred)))))

(define-syntax $unless
  (syntax-rules ()
    ((_ pred body)
     ($when (not pred) body))))
}
}
