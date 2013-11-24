@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.binary.data"]{(binary data) - Binary data read/write}

@define[Library]{@name{(binary data)}}
@desc{This library provides yet another binary data structure read/write. The
difference between @secref["util.binary.pack"]{(binary pack)} and this is the
level of abstraction. This library provides higher abstraction layer of the 
way how to handle binary data.
}

@define[Macro]{@name{define-simple-datum-define} @args{name reader writer}}
@define[Macro]{@name{define-composite-data-define} @args{name reader writer}}
@desc{Defines a macro named @var{name} to read binary data by generic method
@var{reader} and write binary data by @var{writer}.

To use defining macros effectively, both forms @var{reader} and @var{writer}
should be the same name.

The first form defines a macro which takes 5 required arguments and optional
keyword arguments. Let's say the @var{name} is @code{define-simple}, the
@var{reader} is @code{simple-read} and the @var{write} is @code{simple-write},
Then the definition of defined macro would be like this;
@define[Macro]{@name{define-simple}
 @args{name parents slots read-proc write-proc :key (parent-metaclass <class>)}}
@desc{Defines @var{name} class whose parent classes are @var{parents} and slots
are @var{slots}.

@var{read-proc} must be a procedure which takes one argument, a input port and
return number of slots values.

@var{write-proc} must be a procedure which takes number of slots plus 1
arguments. The first one is an output port and the rest are the value of the
slots of the defined class' instance.

The keyword argument @var{parent-metaclass} is a parent class of the metaclass
of this class.

The @var{slots} form must be a list of slot definitions which must be 
followings;
@itemlist[
 @item{@var{name}}
 @item{@var{(name)}}
 @item{@var{(name default)}}
]
The first and second form define a slot which name is @var{name} and its
initial value is #f. The third form defines a slot which name is @var{name}
and its initial is @var{default}.

Note that current implemenation does not handle parent classes slots. This is
only for seamless operations with other CLOS class.
}

The second form defines a macro which takes 3 required arguments and optional
keyword arguments. Let's say the @var{name} is @code{define-composite}, the
@var{reader} is @code{simple-read} and the @var{write} is @code{simple-write},
Then the definition of defined macro would be like this;
@define[Macro]{@name{define-simple}
 @args{name parents slots :key (parent-metaclass <class>)}}
@desc{Defines a composite data class named @var{name} whose parent classes are
@var{parents} and slots are @var{slots}.

}

}

Following is the simple example to show how to use the macros above.

@codeblock{
;; use the same name of reader and writer
(define-simple-datum-define   define-simple    sample-read sample-write)
(define-composite-data-define define-composite sample-read sample-write)

(define-simple <simple> ()
  (a b (c 0))
  (lambda (in) (values (get-u8 in) (get-u8 in) (get-u8 in)))
  (lambda (out a b c) (put-u8 out a) (put-u8 out b) (put-u8 out c)))

}
