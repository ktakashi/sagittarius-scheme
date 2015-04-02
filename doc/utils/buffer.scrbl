@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.buffer"]{(util buffer) - Buffer utilities}

@define[Library]{@name{(util buffer)}}
@desc{This library provides buffer utitlities. Currently, it only provides
pre-allocated buffer and its procedures.
}

@subsubsection{Pre-allocated buffer}

@define["Record Type"]{@name{<pre-allocated-buffer>}}
@desc{Parent type of pre-allocated buffer. This library doesn't expose
the record constructor for this type.

This type contains @code{buffer} and @code{size} fields.
}

@define[Function]{@name{pre-allocated-buffer?} @args{obj}}
@desc{Returns #t if given @var{obj} is a pre-allocated buffer. Otherwise #f.}

@define[Function]{@name{pre-allocated-buffer-buffer} @args{buffer}}
@desc{Returns value of @code{buffer} field of given @var{buffer}.

The type of @code{buffer} field is implementation specific.
}

@define[Function]{@name{pre-allocated-buffer-size} @args{buffer}}
@desc{Returns value of @code{size} field of given @var{buffer}.

The returning value shall represents how much buffer of the given @var{buffer}
is consumed.
}

@define[Function]{@name{pre-allocated-buffer-reset!} @args{buffer}}
@desc{Sets 0 to the @code{size} field of given @code{buffer}.}


@subsubsection{Conditions}

@define[Condition]{@name{&pre-allocated-buffer-overflow}}
@desc{Buffer overflow condition. This is raised when an operation tries
to put data exceed the buffer size.

This condition have @code{data} field which contains overflowing data.
}
@define[Function]{@name{pre-allocated-buffer-overflow?} @args{obj}}
@desc{Returns #t if given @var{obj} is @code{&pre-allocated-buffer-overflow}
condition, otherwise #f.
}
@define[Function]{@name{pre-allocated-buffer-overflow-data} @args{condition}}
@desc{Retrieves @code{data} field value of @var{condition}.

The @var{condition} must be a @code{&pre-allocated-buffer-overflow} condition.
}

@subsubsection{Binary pre-allocated buffer}

Binary pre-allocated buffer can be used when users don't want to allocate
bytevector number of times. This buffer can be used like a bytevector.

@define[Function]{@name{make-binary-pre-allocated-buffer} @args{bv}}
@desc{Creates pre-allocated buffer with given bytevector @var{bv}.

The returning value is a subtype of @code{<pre-allocated-buffer>}.
}
@define[Function]{@name{binary-pre-allocated-buffer?} @args{obj}}
@desc{Returns #t if given @var{obj} is binary pre-allocated buffer, 
otherwise #f.}

@sub*section{Operations}

@define[Function]{@name{binary-pre-allocated-buffer-put-u8!} 
 @args{binary-buffer u8}}
@define[Function]{@name{binary-pre-allocated-buffer-put-u16!}
 @args{binary-buffer u16 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-put-u32!}
 @args{binary-buffer u32 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-put-u64!}
 @args{binary-buffer u64 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-put-s8!} 
 @args{binary-buffer s8}}	  
@define[Function]{@name{binary-pre-allocated-buffer-put-s16!}
 @args{binary-buffer s16 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-put-s32!}
 @args{binary-buffer s32 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-put-s64!}
 @args{binary-buffer s64 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-put-f32!}
 @args{binary-buffer f32 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-put-f64!}
 @args{binary-buffer f64 endianness}}
@desc{Appending given integer or flonum to @var{binary-buffer}. Setting
given values uses the following procedures, respectively:

@code{bytevector-u8-set!}
@code{bytevector-u16-set!}
@code{bytevector-u32-set!}
@code{bytevector-u64-set!}
@code{bytevector-s8-set!}
@code{bytevector-s16-set!}
@code{bytevector-s32-set!}
@code{bytevector-s64-set!}
@code{bytevector-ieee-single-set!}
@code{bytevector-ieee-double-set!}

The @var{endianness} is passed to the above procedures if required.

This procedure also updates the @code{size} field of @var{binary-buffer}.
}

@define[Function]{@name{binary-pre-allocated-buffer-put-bytevector!}
 @args{binary-buffer bv :optional start count}}
@desc{Appending given bytevector @var{bv} to @var{binary-buffer}.

Optional arguments @var{start} and @var{count} specifies from where of
@var{bv} and how much bytes need to be put. By default, @var{start} is 0
and @var{count} is @code{(- (bytevector-length @var{bv}) @var{start})}.


This procedure also updates the @code{size} field of @var{binary-buffer}.
}

@define[Function]{@name{binary-pre-allocated-buffer-set-u8!} 
 @args{binary-buffer index u8}}
@define[Function]{@name{binary-pre-allocated-buffer-set-u16!}
 @args{binary-buffer index u16 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-set-u32!}
 @args{binary-buffer index u32 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-set-u64!}
 @args{binary-buffer index u64 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-set-s8!} 
 @args{binary-buffer index s8}}
@define[Function]{@name{binary-pre-allocated-buffer-set-s16!}
 @args{binary-buffer index s16 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-set-s32!}
 @args{binary-buffer index s32 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-set-s64!}
 @args{binary-buffer index s64 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-set-f32!}
 @args{binary-buffer index f32 endianness}}
@define[Function]{@name{binary-pre-allocated-buffer-set-f64!}
 @args{binary-buffer index f64 endianness}}
@desc{Setting given integer/flonum to @var{binary-buffer}. These procedures
are anology of the following procedures, respectively:

@code{bytevector-u8-set!}
@code{bytevector-s8-set!}
@code{bytevector-u16-set!}
@code{bytevector-u32-set!}
@code{bytevector-u64-set!}
@code{bytevector-s16-set!}
@code{bytevector-s32-set!}
@code{bytevector-s64-set!}
@code{bytevector-ieee-single-set!}
@code{bytevector-ieee-double-set!}

The @var{endianness} is passed to the above procedures if required.

This procedure updates the @code{size} field of @var{binary-buffer} if
sum of given @var{index} and number of bytes set in the buffer exceeds
the size of the buffer.
}

@define[Function]{@name{binary-pre-allocated-buffer-set-bytevector!}
 @args{binary-buffer index bv :optional start count}}
@desc{Sets the given @var{bv} to @var{binary-buffer} at position of 
@var{index}.

Optional arguments @var{start} and @var{count} specifies from where of
@var{bv} and how much bytes need to be put. By default, @var{start} is 0
and @var{count} is @code{(- (bytevector-length @var{bv}) @var{start})}.

This procedure updates the @code{size} field of @var{binary-buffer} if
sum of given @var{index} and number of bytes set in the buffer exceeds
the size of the buffer.
}

All above operations may raises an @code{&pre-allocated-buffer-overflow},
when it tries to exceed the pre-allocated buffer.

@define[Function]{@name{binary-pre-allocated-buffer-can-store?}
 @args{binary-buffer count :optional position}}
@desc{Returns #t if @var{binary-buffer} can store @var{count} bytes.

If optional argument @var{position} is given, then the procedure check
from the @var{position}.
}

@define[Function]{@name{binary-pre-allocated-buffer-swap!}
 @args{binary-buffer new-buf new-size}}
@desc{Swaps the buffer of @var{binary-buffer} with @var{new-buf}
and @var{new-size}.
}

@define[Function]{@name{binary-pre-allocated-buffer-get-bytevector-n!}
 @args{binary-buffer input-port n :optional position}}
@desc{Reads @var{n} bytes from given @var{input-port} and store it to
@var{binary-buffer}.

If optional argument @var{position} is given, then the procedure stores
from the @var{position}.
}

@define[Function]{@name{crop-binary-buffer} @args{binary-buffer}}
@desc{Returns newly allocated bytevector which contains range of
0 to @code{size} field value of @code{buffer} field.
}

@; port conversion
@define[Function]{@name{->binary-pre-allocated-buffer-output-port}
 @args{binary-buffer}}
@desc{Converts the given @var{binary-buffer} to binary output port.

If port operations try to exceed the pre-allocated buffer, then
it raises @code{&pre-allocated-buffer-overflow}.
}
