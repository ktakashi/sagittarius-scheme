[§2] (getopt) - Parsing command-line options {#getopt}
-------------

###### [!Library] `(getopt)` 

This library defines a convenient way to parse command-line options.

The library exports a thin wrapper of [SRFI-37: args-fold](#srfi).


###### [!Macro] `with-args`  _args_ _(bind-spec_ _..._ _[._ _rest])_ _body_ _..._

This macro wraps `args-fold` provided by SRFI-37. It takes a list
of arguments, _args_, and scans it to find Unix-style command-line
options and binds their values to local variables according to _bind-spec_,
then executes _body_.

Following is the example how to use;

``````````scheme
(define (usage args) ...)

(define (main args)
  (with-args args
     ((verbose (#\v "verbose") #f #f)
      (file    (#\f "file") #t (usage args))
      (debug-level   (#\d "debug-level") #t "0")
      . rest)
    ...))
``````````

The _bind-spec_ must be one of the following forms;


- _(var (short long) value-required? default)_
- _(var (short long) `*` default)_

_var_ is variable name. _short_ must be a character represents the
short argument name. _long_ must be a string represents the long argument
name. _value-required?_ specifies whether or not the optional argument
has the value. _default_ is the default form of the variable so it will
be evaluated when the input _args_ didn't have the specified option.

If the second form is used, the `*` is syntactic keyword and the
passed value will be packed to list. Thus the script can take more than one
the same options.

If _rest_ is not presented and _args_ contains non listed argument,
then it raises an `&assertion`. If it is, then it will pack the rest of
non listed argument as a list.



