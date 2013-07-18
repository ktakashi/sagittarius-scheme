@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.bytevector"]{(util bytevector) - Bytevector utility library}

@define[Library]{@name{(util bytevector)}}
@desc{This library provides bytevector utilities which are not provided as
builtin procedures such as @code{bytevector->integer}.

All procedures take bytevector as its arguments.
}

@define[Function]{@name{bytevector-xor} @args{bv1 bv2 @dots{}}}
@define[Function]{@name{bytevector-xor!} @args{out bv1 bv2 @dots{}}}
@define[Function]{@name{bytevector-ior} @args{bv1 bv2 @dots{}}}
@define[Function]{@name{bytevector-ior!} @args{out bv1 bv2 @dots{}}}
@define[Function]{@name{bytevector-and} @args{bv1 bv2 @dots{}}}
@define[Function]{@name{bytevector-and!} @args{out bv1 bv2 @dots{}}}
@desc{Compute exclusive or, logical or and logical and for each given
bytevectors, respectively.

The procedures without @code{!} freshly allocate a new bytevector as it's return
value. If the given bytevectors are not the same sized, then the smallest 
size will be allocated.

The procedures with @code{!} takes first argument as the storage of the result
and return it.
}

@define[Function]{@name{bytevector-slices} @args{bv k :key (padding #f)}}
@desc{Slices the given bytevector @var{bv} into @var{k} size and returns
a list of bytevectors.

The keyword argument @var{padding} is given and it must be a procedure accept
one argument, then it will be called when the last chunk of bytevector is not
size of @var{k}. The procedure should return padded bytevector and it doesn't
check the returned value nor it's size so it is caller's responsibility to
make sure the returned value is a bytevector and the size is @var{k}.

@snipet[=> (#vu8(1 2 3) #vu8(4 5 6))]{(bytevector-slices #vu8(1 2 3 4 5 6) 3)}
@snipet[=> (#vu8(1 2 3) #vu8(4))]{(bytevector-slices #vu8(1 2 3 4) 3)}
@codeblock[=> (#vu8(1 2 3) #vu8(4 5 6))]{
;; the given bytevector bv is #vu8(4)
(bytevector-slices #vu8(1 2 3 4) 3 :padding (lambda (bv) #vu8(4 5 6)))
}
@codeblock[=> (#vu8(1 2 3) #f)]{
;; this is valid as well so that bytevector-slices doesn't check the 
;; return value
(bytevector-slices #vu8(1 2 3 4) 3 :padding (lambda (bv) #f))
}
}

@define[Function]{@name{bytevector-split-at*} @args{bv k :key (padding #f)}}w
@desc{Splits bytevector into 2 bytevectors and returns 2 values of bytevectors.

The first returned bytevector size will be @var{k} and its content is given
bytevector's value starting from 0 to @var{k} - 1. The second returned value
is the rest of values of @var{bv}.

If size of the given bytevector @var{bv} is less than @var{k} then the second
value of returned bytevector will be empty bytevector.

The keyword argument @var{padding} is given and it must be a procedure accept
one argument, then it will be called when given bytevector's size is less than
@var{k} and first returned value will the result of @var{padding}.

@snipet[=> "#vu8(1 2 3) and #vu8(4 5)"]{
(bytevector-split-at* #vu8(1 2 3 4 5) 3)}
@codeblock[=> "#vu8(1 2 3) and #vu8()"]{
(bytevector-split-at* #vu8(1 2) 3 :padding (lambda (bv) #vu8(1 2 3)))
}
@codeblock[=> "#f and #vu8()"]{
(bytevector-split-at* #vu8(1 2) 3 :padding (lambda (bv) #f))
}
}

@define[Function]{@name{->odd-parity}
 @args{bv :optional (start 0) (end (bytevector-length bv))}}
@define[Function]{@name{->odd-parity!}
 @args{bv :optional (start 0) (end (bytevector-length bv))}}
@desc{Compute odd parity of the given bytevector @var{bv} and return the
result of bytevector.

If the second procedure is used, then @var{bv} will be modified.
}
