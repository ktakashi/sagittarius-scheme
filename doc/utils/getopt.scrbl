@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "getopt"]{(getopt) - Parsing command-line options}

@define[Library]{@name{(getopt)}}
@desc{This library defines a convenient way to parse command-line options.

The library exports a thin wrapper of @secref["srfi"]{SRFI-37: args-fold}.
}

@define[Macro]{@name{with-args}
 @args{args (bind-spec @dots{} [. rest]) body @dots{}}}
@desc{This macro wraps @code{args-fold} provided by SRFI-37. It takes a list
of arguments, @var{args}, and scans it to find Unix-style command-line
options and binds their values to local variables according to @var{bind-spec},
then executes @var{body}.

Following is the example how to use;

@codeblock{
(define (usage args) ...)

(define (main args)
  (with-args args
     ((verbose (#\v "verbose") #f #f)
      (file    (#\f "file") #t (usage args))
      (debug-level   (#\d "debug-level") #t "0")
      . rest)
    ...))
}

The @var{bind-spec} must be one of the following forms;

@itemlist[
 @item{@var{(var (short long) value-required? default)}}
 @item{@var{(var (short long) @code{*} default)}}
]

@var{var} is variable name. @var{short} must be a character represents the
short argument name. @var{long} must be a string represents the long argument
name. @var{value-required?} specifies whether or not the optional argument
has the value. @var{default} is the default form of the variable so it will
be evaluated when the input @var{args} didn't have the specified option.

If the second form is used, the @code{*} is syntactic keyword and the
passed value will be packed to list. Thus the script can take more than one
the same options.

If @var{rest} is not presented and @var{args} contains non listed argument,
then it raises an @code{&assertion}. If it is, then it will pack the rest of
non listed argument as a list.

}
