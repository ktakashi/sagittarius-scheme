@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.list"]{(util list) - Extra list utility library}

@define[Library]{@name{(util list)}}
@desc{This library provides extra list utility procedures.
}

@define[Function]{@name{for-each-with-index} @args{proc list1 list2 @dots{}}}
@define[Function]{@name{map-with-index} @args{proc list1 list2 @dots{}}}
@desc{Like @code{for-each} and @code{map}, expect @var{proc} receives the index
as the first argument.

@snipet[=> ((0 a e) (1 b f) (2 c g))]{(map-with-index list '(a b c) '(e f g))}
}

@define[Function]{@name{intersperse} @args{item list}}
@desc{Inserts @var{item} between elements in the @var{list}.

@snipet[=> (1 + 2 + 3)]{(intersperse '+ '(1 2 3))}
@snipet[=> (1)]{(intersperse '+ '(1))}
@snipet[=> ()]{(intersperse '+ '())}
}

@define[Function]{@name{slices} @args{list k :optional fill? padding}}
@desc{Splits @var{list} into the sublists (slices) where the length of each
slice is @var{k}. If the length of @var{list} is not multiple of @var{k}, the
last slice is dealt in the same way as @code{take*}; this is, it is shorter than
@var{k} by default, or added @var{padding} if @var{fill?} is true.

@snipet[=> ((a b c) (d e f) (g))]{(slices '(a b c d e f g) 3)}
@snipet[=> ((a b c) (d e f) (g z z))]{(slices '(a b c d e f g) 3 #t 'z)}
}

@define[Function]{@name{split-at*}
 @args{list k :optional (fill? #t) (padding #f)}}
@desc{Splits the list @var{list} at index @var{k}. This is more tolerant version
of @code{split-at} defined in SRFI-1 library. Returns the results of
@code{take*} and @code{drop*}.

@snipet[=> "(a b c d z z) and ()"]{(split-at* '(a b c d) 6 #t 'z)}
}


@define[Function]{@name{take*} @args{list k :optional (fill? #f) (padding #f)}}
@define[Function]{@name{drop*} @args{list k}}
@desc{More tolerant version of @code{take} and @code{drop} defined in SRFI-1
library. These won't raise an error when @var{k} is larger than the size of the
given list.

If the list is shorter than @var{k} elements, @code{take*} returns a copy of
@var{list} by default. If @var{fill?} is true, @var{padding} is added to the
result to make its length @var{k}.

On the other hand, @code{drop*} just returns as empty list when the input list
is shorter than @var{k} elements.

@snipet[=> (a b c)]{(take* '(a b c d) 3)}
@snipet[=> (a b c d)]{(take* '(a b c d) 6)}
@snipet[=> (a b c d #f #f)]{(take* '(a b c d) 6 #t)}
@snipet[=> (a b c d z z)]{(take* '(a b c d) 6 #t 'z)}
@snipet[=> (d)]{(drop* '(a b c d) 3)}
@snipet[=> ()]{(drop* '(a b c d) 5)}

}

@define[Macro]{@name{cond-list} @args{clause @dots{}}}
@desc{Construct a list by conditionally adding entries. Each @var{clause} must
have a test and expressions. When its test yields true, then result of
associated expression is used to construct the resulting list. When the test 
yields false, nothing is inserted.

@var{Clause} must either one of the following form:

@dl-list{
@dl-item[@var{(test expr @dots{})}]{
	@var{Test} is evaluated, and when it is true, @var{expr @dots{}} are
	evaluated, and the return value becomes a part of the result. If no
	@var{expr} is given, the result of test is used if it is not false.
}
@dl-item[@var{(test => proc)}]{
	@var{Test} is evaluated, and if it is true, @var{proc} is called with
	the value, and the return value is used to construct the result
}
@dl-item[@var{(test @ expr @dots{})}]{
	Like @code{(test expr @dots{})}, except that the result of the last
	@var{expr} must be a list, and it is spliced into the resulting list,
	like unquote-splicing.
}
@dl-item[@var{(test => @ proc)}]{
	Like @code{(test => proc)}, except that the result of the last
	@var{proc} must be a list, and it is spliced into the resulting list,
	like unquote-splicing.
}
}

@codeblock[=> (have-x 6)]{
(let ((alist '((x 3) (y -1) (z 6))))
 (cond-list ((assoc 'x alist) 'have-x)
            ((assoc 'w alist) 'have-w)
            ((assoc 'z alist) => cadr)))
}

@codeblock[=> "(:x 2 :z 5)"]{
(let ((x 2) (y #f) (z 5))
  (cond-list (x @ `(:x ,x))
             (y @ `(:y ,y))
             (z @ `(:z ,z))))
}
}