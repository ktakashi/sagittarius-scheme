[§2] (sagittarius debug) - Debugging support {#lib.sagittarius.debug}
-------------

###### [!Library] `(sagittarius debug)` 

This library provides debugging support reader macro.

###### [!Reader Macro] `#?=`  _expr_

This reader macro reads the next expression as followings;

``(debug-print _expr_)``

`debug-print` is an internal macro of this library which prints the
read expression and its result.

Following example shows how to enable this;

``````````scheme
#!read-macro=sagittarius/debug
#!debug
(let ((a (+ 1 2)))
  #?=(expt a 2))

#|
#?=(expt a 2)
#?-    9
|#
``````````

`#!debug` enables the debug print.


###### [!Function] `macroexpand`  _expr_

Expands given _expr_. The returning value may or may not be used
as proper Scheme expression.


###### [!Function] `macroexpand-1`  _expr_
###### [!Function] `macroexpand-n`  _expr_ _n_

Expands given _expr_ _n_ times. The first form's _n_ is 1.

This procedure expands only globally defined macro and the result of expansion
is other macro such as next rule of `syntax-rules`. It doesn't consider
locally bound macros.

The returning value may or may not be used as proper Scheme expression.


Above 2 procedures are no more than debug aid. Depending on the result of
expansion is not guaranteed to work.
