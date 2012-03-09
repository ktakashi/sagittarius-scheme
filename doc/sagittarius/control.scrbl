@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "lib.sagittarius.control"]{(sagittarius control) - control library}

@define[Library]{@name{(sagittarius control)}}
@desc{This library provides some useful macros using Sagittarius specific
functions.}

@define[Macro]{@name{define-macro} @args{name procedure}}
@define[Macro]{@name{define-macro} @args{(name . formals) body @dots{}}}
@desc{Defines @var{name} to be a macro whose transformer is @var{procedure}.
The second form is a shorthand notation of the following form:

@snipet{(define-macro @var{name} (lambda @var{formals} @var{body} @dots{}))}
}

@define[Macro]{@name{let-optionals*}
 @args{restargs (var-spec @dots{}) body @dots{}}}
@define[Macro]{@name{let-optionals*}
 @args{restargs (var-spec @dots{} . restvar) body @dots{}}}
@desc{Given a list of values @var{restargs}, binds variables according to
@var{var-spec}, then evaluates @var{body}.

@var{Var-spec} can be either a symbol, or a list of two elements and its car is
a symbol. The symbol is the bound variable name. The values in @var{restargs}
are bound to the symbol in order. If there are not as many values in restargs as
@var{var-spec}, the rest of symbols are bound to the default values, determined
as follows:

If @var{var-spec} is just a symbol, the default value is undefined.

If @var{var-spec} is a list, the default value is the result of evaluation of
the second element of the list.

In the latter case the second element is only evaluated when there are not
enough arguments. The binding proceeds in the order of @var{var-spec}, so the 
second element may refer to the bindings of previous @var{var-spec}.

In the second form, @var{restvar} must be a symbol and bound to the list of
values whatever left from @var{restargs} after binding to @var{var-spec}.

It is not an error if @var{restarg} has more values than @var{var-specs}. The
extra values are simply ignored in the first form. 
}

@define[Macro]{@name{get-optionals} @args{restargs default}}
@define[Macro]{@name{get-optionals} @args{restargs default test}}
@desc{This is a short version of @code{let-optionals*} where you have only one
optional argument. Given the optional argument list @var{restargs}, this macro
returns the value of optional argument if one is given, or the result of
@var{default} otherwise. 

If latter form is used, @var{test} must be procedure which takes one argument
and it will be called to test the given argument. If the test failed, it raises
@code{&error} condition.

@var{Default} is not evaluated unless restargs is an empty list.
}

@define[Macro]{@name{let-keywords}
 @args{restargs (var-spec @dots{}) body @dots{}}}
@define[Macro]{@name{let-keywords}
 @args{restargs (var-spec @dots{} . restvar) body @dots{}}}
@desc{This macro is for keyword arguments. @var{Var-spec} can be one of the
following forms:

@snipet{(@var{symbol} @var{expr})}

If the @var{restrag} contains keyword which has the same name as @var{symbol},
binds symbol to the corresponding value. If such a keyword doesn't appear in 
@var{restarg}, binds symbol to the result of @var{expr}.

@snipet{(@var{symbol} @var{keyword} @var{expr})}

If the @var{restarg} contains keyword @var{keyword}, binds symbol to the
corresponding value. If such a keyword doesn't appear in restarg, binds symbol
to the result of @var{expr}. 

The default value @var{expr} is only evaluated when the keyword is not given to
the @var{restarg}.

If you use the first form, @code{let-keyword} raises @code{&error} condition
when @var{restarg} contains a keyword argument that is not listed in
@var{var-specs}. When you want to allow keyword arguments other than listed in
@var{var-specs}, use the second form.

In the second form, @var{restvar} must be either a symbol or #f. If it is a
symbol, it is bound to a list of keyword arguments that are not processed by
@var{var-specs}. If it is #f, such keyword arguments are just ignored.

@codeblock{
(define (proc x . options)
  (let-keywords options ((a 'a)
                         (b :beta 'b)
                         (c 'c)
                         . rest)
    (list x a b c rest)))
}

@snipet[=> (0 a b c ())]{(proc 0)}
@snipet[=> (0 1 b c ())]{(proc 0 :a 1)}
@snipet[=> (0 a 1 c ())]{(proc 0 :beta 1)}
@snipet[=> (0 a 1 3 (:unknown 4))]{(proc 0 :beta 1 :c 3 :unknown 4)}
}

@define[Macro]{@name{let-keywords*}
 @args{restargs (var-spec @dots{}) body @dots{}}}
@define[Macro]{@name{let-keywords*}
 @args{restargs (var-spec @dots{} . restvar) body @dots{}}}
@desc{Like @code{let-keywords}, but the binding is done in the order of
@var{var-specs}. So each @var{expr} can refer to the variables bound by
preceding @var{var-specs}.

These let-keywords and let-keywords* are originally from Gauche. 
}

@define[Macro]{@name{define-with-key} @args{variable expression}}
@define[Macro]{@name{define-with-key} @args{variable}}
@define[Macro]{@name{define-with-key} @args{(variable formals) body @dots{}}}
@define[Macro]{@name{define-with-key} @args{(variable . formals) body @dots{}}}
@desc{The @code{define-with-key} is synonym of @code{define}.

See more detail
@secref["rnrs.base.6.variable.definitions"]{Variable definitions}.
}

@define[Macro]{@name{begin0} @args{exp0 exp1 @dots{}}}
@desc{Evaluate @var{exp0}, @var{exp1}, @dots{}, then returns the result(s) of
@var{exp0}.
}

@define[Macro]{@name{let1} @args{var expr body @dots{}}}
@desc{A convenient macro when you have only one variable. Expanded as follows:
@snipet{(let ((@var{var} @var{expr})) @var{body} @dots{})}
}

@define[Macro]{@name{dotimes} @args{(variable limit [result]) body @dots{}}}
@define[Macro]{@name{dolist} @args{(variable lexpr [result]) body @dots{}}}
@desc{Imported from Common List. These are equivalent to the following forms,
respectively.

@codeblock{(dotimes (variable limit result) body @dots{})}
=>
@codeblock{
(do ((tlimit limit)
     (variable 0 (+ variable 1)))
    ((>= variable tlimit) result)
  body @dots{})
}

@codeblock{(dolist (variable lexpr result) body @dots{})}
=>
@codeblock{
(begin
  (for-each (lambda (variable) body @dots{}) lexpr)
  (let ((variable '())) result))
}

}

@define[Macro]{@name{with-library} @args{library variable}}
@desc{@var{library} must be a library name. ex. (srfi :1 lists)

@var{variable} must be a symbol.

Retrieve a value which is bounded with @var{variable} in the @var{library}.

This should not be used casually however you want to use some procedures or
variables which are not exported, such as a procedure written in C but not
exported or non exported record accessor. For thoese purpose, this might be a
quick solution.
}