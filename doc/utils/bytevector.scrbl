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

@define[Function]{@name{bytevector<?} @args{bv1 bv2 rest @dots{}}}
@define[Function]{@name{bytevector>?} @args{bv1 bv2 rest @dots{}}}
@define[Function]{@name{bytevector<=?} @args{bv1 bv2 rest @dots{}}}
@define[Function]{@name{bytevector>=?} @args{bv1 bv2 rest @dots{}}}
@desc{Comparing given bytevectors.

The comparison is done by comparing the elements of bytevectors from
index @code{0}. The comparison procedures are @code{<}, @code{>}, @code{<=}
and @code{>=}, respectively.
}

@define[Function]{@name{bytevector->hex-string} @args{bv :key (upper? #t)}}
@desc{Converts given bytevector @var{bv} to hex string.

The keyword argument @var{upper?} is specified with true value, then the
procedures converts to upper case hex values, otherwise lower case.
}

@define[Function]{@name{hex-string->bytevector} @args{string}}
@desc{Converts given hex string @var{string} to bytevector.}

@define[Function]{@name{bytevector-reverse!}
 @args{bv :optional (start 0) (end (bytevector-length bv))}}
@define[Function]{@name{bytevector-reverse}
 @args{bv :optional (start 0) (end (bytevector-length bv))}}
@desc{Reverse the given bytevector @var{bv}.

Optional arguments @var{start} and @var{end} controls from and until where
the procedure reverses the bytevector. @var{end} is exclusive.

The @code{bytevector-reverse!} reverses destructively.
}

@subsubsection{SRFI-13 convension APIs}

@sub*section{U8 sets}

U8 set is a list of integers which range is in between @code{0 <= n <= 255}.
This is useful to handle bytevectors as if they are ASCII strings.

@define[Function]{@name{u8?} @args{o}}
@desc{Returns #t if given @var{o} is an integer in range of 
@code{0 <= @var{o} <= 255}, otherwise #f.}

@define[Function]{@name{u8-set?} @args{o}}
@desc{Returns #t if given @var{o} is a list and its all elements satisfy
@code{u8?}. Otherwise #f.
}

@define[Function]{@name{u8-set-contains?} @args{u8-set u8}}
@desc{@var{u8-set} must satisfy @code{u8-set?}. @var{u8} should satisfy
@code{u8}. The procedure doesn't check if arguments satify this.

Returns #t if given @var{u8-set} contains @var{u8}.
}

@define[Function]{@name{string->u8-set} @args{string}}
@desc{Converts given @var{string} to list of integers. Given @var{string} should
only contains in range of ASCII characters but the procedure doesn't check. 
Thus the procedure may return a list doesn't satify @code{u8-set?}.
It is users' responsibility to pass ASCII string.
}

@define[Function]{@name{char-set->u8-set} @args{cset}}
@desc{Converts given char-set @var{cset} to u8 set. This procedure returns
a list that satify @code{u8-set?} by dropping outside of ASCII characters.
}

@sub*section{Bytevectors as ASCII strings}

@define[Function]{@name{bytevector-fold}
 @args{kons knil bv :optional start end}}
@define[Function]{@name{bytevector-fold-right}
 @args{kons knil bv :optional start end}}
@desc{Iterate given @var{bv} from @var{start} until @var{end}. @var{kons} is
called by each element with result of the @var{kons}. The inital value is
@var{knil}.

This is analogy of @var{fold-left} and @var{fold-right}.
}

@define[Function]{@name{bytevector-take} @args{bv n}}
@define[Function]{@name{bytevector-take-right} @args{bv n}}
@desc{Subtract bytevector @var{bv} until index @var{n} (exclusive).

The @code{bytevector-take} takes from left and the @code{bytevector-take-right}
takes from right.
}

@define[Function]{@name{bytevector-drop} @args{bv n}}
@define[Function]{@name{bytevector-drop-right} @args{bv n}}
@desc{Drops given @var{bv} until index @var{n} (exclusive).

The @code{bytevector-drop} drops from left and the @code{bytevector-drop-right}
drops from right.
}

@define[Function]{@name{bytevector-trim}
 @args{bv :optional criterion start end}}
@define[Function]{@name{bytevector-trim-right}
 @args{bv :optional criterion start end}}
@define[Function]{@name{bytevector-trim-both}
 @args{bv :optional criterion start end}}
@desc{Trims given bytevector @var{bv} from left, right and both, respectively.

The optional argument @var{criterion} specifies how to trim. By default, it
uses whitespaces. @code{" \r\f\v\n\t"}.

The optional arguments @var{start} and @var{end} specify from and until where
the procedure trims. The default value is 0 for @var{start} and the length
of given bytevector for @var{end}.
}

@define[Function]{@name{bytevector-pad} @args{bv n :optional (u8 0) start end}}
@define[Function]{@name{bytevector-pad-right} 
 @args{bv n :optional (u8 0) start end}}
@desc{Pads given bytevector @var{bv} with @var{n} elements of @var{u8}.
The @code{bytevector-pad} pads left side of given @var{bv}. The 
@code{bytevector-pad-right} pads right side of given @var{bv}.

The optional arguments @var{start} and @var{end} specify from and until where
the procedure pads. The default value is 0 for @var{start} and the length
of given bytevector for @var{end}.
}

@define[Function]{@name{bytevector-prefix-length}
 @args{bv1 bv2 :optional start1 end1 start2 end2}}
@define[Function]{@name{bytevector-suffix-length}
 @args{bv1 bv2 :optional start1 end1 start2 end2}}
@desc{Return the length of the longest common prefix/suffix of the two 
bytevectors.

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of @var{bv1} and @var{bv2}.
}

@define[Function]{@name{bytevector-prefix?}
 @args{bv1 bv2 :optional start1 end1 start2 end2}}
@define[Function]{@name{bytevector-suffix?}
 @args{bv1 bv2 :optional start1 end1 start2 end2}}
@desc{Returns #t if @var{bv1} is a prefix/suffix of @var{bv2}. Otherwise #f. 

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of @var{bv1} and @var{bv2}.
}

@define[Function]{@name{bytevector-index}
 @args{bv criterion :optional start end}}
@define[Function]{@name{bytevector-index-right}
 @args{bv criterion :optional start end}}
@desc{Searches through the given bytevector @var{bv} from the left (right), 
returning the index of the first occurrence of an element which satisfies
the @var{criterion}.

@var{criterion} can be a u8 value, a u8 set or a procedure.

If the procedure doesn't find any element satisfies @var{criterion}, then
returns #f.
}

@define[Function]{@name{bytevector-skip}
 @args{bv criterion :optional start end}}
@define[Function]{@name{bytevector-skip-right}
 @args{bv criterion :optional start end}}
@desc{Search through the given bytevector @var{bv} from the left (right), 
returning the index of the first occurrence of an element which does not
satisfy the @var{criterion}.

@var{criterion} can be a u8 value, a u8 set or a procedure.

If the procedure doesn't find any element which does not satisfy 
@var{criterion}, then returns #f.
}


@define[Function]{@name{bytevector-contains}
 @args{bv1 bv2 :optional start1 end1 start2 end2}}
@desc{Returns index of @var{bv1} where @var{bv2} is found. If @var{bv1} doesn't
contain @var{bv2} then returns #f.

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of @var{bv1} and @var{bv2}.
}

@define[Function]{@name{bytevector-replace}
 @args{bv1 bv2 start1 end2 :optional start2 end2}}
@desc{Returns
@codeblock{
(bytevector-append (bytevector-copy s1 0 start1)
                   (bytevector-copy s2 start2 end2)
                   (bytevector-copy s1 end1 (string-length s1)))
}
}

@define[Function]{@name{bytevector-tokenize}
 @args{bv :optional token-set start end}}
@desc{Split the given bytevector @var{bv} into a list of sub bytevectors,
where each sub bytevector is a maximal non-empty contigunous sequence of
elements from the u8 set @var{token-set}.

Optional argument @var{token-set} must be a u8 set. By default, it's a
list of bytes of ASCII graphical characters.

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of @var{bv}.
}

@define[Function]{@name{bytevector-filter}
 @args{criterion bv :optional start end}}
@define[Function]{@name{bytevector-delete}
 @args{criterion bv :optional start end}}
@desc{Filter the bytevector @var{bv}, retaining only those elements that 
satisfy / do not satisfy the @var{criterion} argument.

@var{criterion} can be a u8 value, a u8 set or a procedure.

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of @var{bv}.
}
