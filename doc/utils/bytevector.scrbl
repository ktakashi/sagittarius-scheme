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
value.

The procedures with @code{!} takes first argument as the storage of the result
and return it.
}
