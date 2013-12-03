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

It is similar form with @code{define-class} however @var{slots} must be a list
of one of the followings.
@itemlist[
 @item{@var{(name type)}}
 @item{@var{(name type default)}}
 @item{@var{(name (type count))}}
 @item{@var{(name (type count) default)}}
]
@var{name} must be a symbol which represents the slot name.

@var{type} can be class name or @code{eqv?} comparable datum. e.g. keyword.

@var{default} can be any object.

@var{count} must be a non negative exact integer.

The first form is equivalent with the following form;
@code{(name type #f)}.
And the third form is equivalent with the following form;
@code{(name (type count) #f)}.

The first 2 forms defines a datum slot which the datum is read by @var{reader}
passing @var{type} and written by @var{writer}.

The rest forms defines an array data represented by a vector.

If the @var{type} is not defined by neither of the definition forms, then
it is users responsibility to define a method which handles the @var{type}.
}

}

Following is the simple example to show how to use the macros above.

@codeblock{
(import (clos user) (binary data))

;; use the same name of reader and writer
(define-simple-datum-define   define-simple    sample-read sample-write)
(define-composite-data-define define-composite sample-read sample-write)

(define-simple <simple> ()
  (a b (c 0))
  (lambda (in) (values (get-u8 in) (get-u8 in) (get-u8 in)))
  (lambda (out a b c) (put-u8 out a) (put-u8 out b) (put-u8 out c)))

(define-composite <composite> ()
  ((d :byte 1)
   (e (:byte 4) #vu8(1 2 3 4))
   (f <simple>)))

;; :byte reader and writer
(define-method sample-read ((o (eql :byte)) in array-size?)
  (if array-size?
      (get-bytevector-n in array-size?)
      (get-u8 in)))

(define-method sample-write ((type (eql :byte)) o out array-size?)
  (if array-size?
     (put-bytevector out o)
     (put-u8 out o)))
}

How to use the defined data structure.
@codeblock{
;; read as a <composite> object
;; "deeeeabc" in ascii
(define bv #vu8(#x64 #x65 #x65 #x65 #x65 #x61 #x62 #x63))
(call-with-port (open-bytevector-input-port bv)
  (lambda (in)
    (let ((s (sample-read <composite> in)))
      (slot-ref s 'd) ;; => #\d
      (slot-ref s 'f) ;; => <simple>
      )))

;; write <composite> object
(call-with-bytevector-output-port
  (lambda (out)
    (let* ((s (make <simple> :a 10 :b 20))
           (c (make <composite> :f s)))
      ;; this can be written like this as well (sample-write o out)
      (sample-write <composite> c out))))
;; => #vu8(1 1 2 3 4 10 20 0)
}

@subsubsection{Utilities}

@define[Function]}{@name{put-u16} @args{out v endian}}
@define[Function]}{@name{put-s16} @args{out v endian}}
@define[Function]}{@name{put-u32} @args{out v endian}}
@define[Function]}{@name{put-s32} @args{out v endian}}
@define[Function]}{@name{put-u64} @args{out v endian}}
@define[Function]}{@name{put-s64} @args{out v endian}}
@define[Function]}{@name{put-f32} @args{out v endian}}
@define[Function]}{@name{put-f64} @args{out v endian}}
@desc{@var{out} must be binary output port. @var{endian} must be a value
returned from @code{endianness} macro.

Write @var{v} to @var{out} as unsigned/signed 16/32/64 bit integer or
32/64 bit floating number.
}

@define[Function]}{@name{get-u16} @args{in endian}}
@define[Function]}{@name{get-s16} @args{in endian}}
@define[Function]}{@name{get-u32} @args{in endian}}
@define[Function]}{@name{get-s32} @args{in endian}}
@define[Function]}{@name{get-u64} @args{in endian}}
@define[Function]}{@name{get-s64} @args{in endian}}
@define[Function]}{@name{get-f32} @args{in endian}}
@define[Function]}{@name{get-f64} @args{in endian}}
@desc{@var{in} must be binary input port. @var{endian} must be a value
returned from @code{endianness} macro.

Read a number from @var{in} as unsigned/signed 16/32/64 bit integer or
32/64 bit floating number.
}
