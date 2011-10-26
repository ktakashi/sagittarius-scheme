@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.lists.6"]{List utilities}

@define[Library]{@name{(rnrs lists (6))}}
@desc{[R6RS] The @code{(rnrs lists (6))}library, which contains various useful
procedures that operate on lists.
}

@define[Function]{@name{find} @args{proc list}}
@desc{[R6RS] @var{Proc} should accept one argument and return a single value.
@var{Proc} should not mutate list. The @code{find} procedure applies @var{proc}
to the elements of @var{list} in order. If @var{proc} returns a true value for
an element, find immediately returns that element. If @var{proc} returns #f for
all elements of the @var{list}, find returns #f.
}

@define[Function]{@name{for-all} @args{pred list1 list2 ...}}
@desc{[R6RS+] Applies @var{pred} across each element of @var{lists}, and returns
#f as soon as @var{pred} returns #f. If all application of @var{pred} return a
non-false value, @code{for-all} returns the last result of the applications.
}

@define[Function]{@name{exists} @args{pred list1 list2 ...}}
@desc{[R6RS+] Applies @var{pred} across each element of @var{lists}, and returns
as soon as @var{pred} returns a non-false value. The return value of any is the
non-false value @var{pred} returned. If lists are exhausted before @var{pred}
returns a non-false value, #f is returned.

Note: R6RS requires the same length list for @code{for-all} and @code{exists}.
On Sagittarius, however, these can accept different length list and it will
finish to process when the shortest list is finish to process.
}

@define[Function]{@name{filter} @args{proc list}}
@define[Function]{@name{partition} @args{proc list}}
@desc{[R6RS] @var{Proc} should accept one argument and return a single value.

The @code{filter} procedure applies @var{proc} to each element of @var{list} and
returns a list of the elements of @var{list} for which @var{proc} returned a true
value. The @code{partition} procedure also applies @var{proc} to each element of
@var{list}, but returns two values, the first one a list of the elements of @var{list}
for which @var{proc} returned a true value, and the second a list of the elements
of @var{list} for which @var{proc} returned #f. In both cases, the elements of the
result list(s) are in the same order as they appear in the input list. If multiple
returns occur from @code{filter} or @code{partitions}, the return values returned
by earlier returns are not mutated.
}

@define[Function]{@name{fold-left} @args{combine nil list1 list2 ...}}
@desc{[R6RS+] @var{Combine} must be a procedure. It should accept one more argument
than there are @var{lists} and return a single value. It should not mutate the
@var{list} arguments. The @code{fold-left} procedure iterates the @var{combine}
procedure over an accumulator value and the elements of the @var{lists} from left
to right, starting with an accumulator value of @var{nil}. More specifically,
@code{fold-left} returns @var{nil} if the @var{lists} are empty. If they are not
empty, @var{combine} is first applied to @var{nil} and the respective first
elements of the @var{lists} in order. The result becomes the new accumulator
value, and @var{combine} is applied to the new accumulator value and the respective
next elements of the @var{list}. This step is repeated until the end of the
@var{list} is reached; then the accumulator value is returned.
}

@define[Function]{@name{fold-right} @args{combine nil list1 list2 ...}}
@desc{[R6RS+] @var{Combine} must be a procedure. It should accept one more argument
than there are @var{lists} and return a single value. @var{Combine} should not
mutate the @var{list} arguments. The @code{fold-right} procedure iterates the
@var{combine} procedure over the elements of the @var{lists} from right to left
and an accumulator value, starting with an accumulator value of @var{nil}. More
specifically, @code{fold-right} returns @var{nil} if the lists are empty. If they
are not empty, @var{combine} is first applied to the respective last elements of
the @var{lists} in order and @var{nil}. The result becomes the new accumulator
value, and @var{combine} is applied to the respective previous elements of the
@var{lists} and the new accumulator value. This step is repeated until the beginning
of the @var{list} is reached; then the accumulator value is returned.

Note: R6RS requires the same length list for @code{fold-left} and @code{fold-right}.
On Sagittarius, however, these can accept different length list and it will finish
to process when the shortest list is finish to process.
}

@define[Function]{@name{remp} @args{proc list}}
@define[Function]{@name{remove} @args{obj list}}
@define[Function]{@name{remv} @args{obj list}}
@define[Function]{@name{remq} @args{obj list}}
@desc{[R6RS] Proc should accept one argument and return a single value. @var{Proc}
should not mutate @var{list}.

Each of these procedures returns a list of the elements of @var{list} that do not
satisfy a given condition. The @code{remp} procedure applies @var{proc} to each
element of @var{list} and returns a list of the elements of @var{list} for which
@var{proc} returned #f. The @code{remove}, @cpde{remv}, and @code{remq} procedures
return a list of the elements that are not @var{obj}. The @code{remq} procedure
uses @code{eq?} to compare @var{obj} with the elements of @var{list}, while
@code{remv} uses @code{eqv?} and @code{remove} uses @code{equal?}. The elements
of the result list are in the same order as they appear in the input list. If
multiple returns occur from @code{remp}, the return values returned by earlier
returns are not mutated.
}

@define[Function]{@name{memp} @args{proc list}}
@define[Function]{@name{member} @args{obj list :optional =}}
@define[Function]{@name{memv} @args{obj list}}
@define[Function]{@name{memq} @args{obj list}}
@desc{[R6RS+] @var{Proc} should accept one argument and return a single value.
@var{Proc} should not mutate @var{list}.

These procedures return the first sublist of @var{list} whose car satisfies a
given condition, where the sublists of @var{lists} are the lists returned by
@code{(list-tail @var{list} @var{k})} for @var{k} less than the length of @var{list}.
The @code{memp} procedure applies @var{proc} to the cars of the sublists of 
@var{list} until it finds one for which @var{proc} returns a true value. The
@code{member}, @code{memv}, and @code{memq} procedures look for the first
occurrence of @var{obj}. If list does not contain an element satisfying the
condition, then #f (not the empty list) is returned. The @code{member} procedure
uses @code{equal?} or if @var{=} is given use it to compare @var{obj} with the
elements of @var{list}, while @code{memv} uses @code{eqv?} and @code{memq} uses
@code{eq?}.
}

@define[Function]{@name{assp} @args{proc alist}}
@define[Function]{@name{assc} @args{obj alist :optional =}}
@define[Function]{@name{assv} @args{obj alist}}
@define[Function]{@name{assq} @args{obj alist}}
@desc{[R6RS+] @var{Alist} (for “association list”) should be a list of pairs.
@var{Proc} should accept one argument and return a single value. @var{Proc}
should not mutate @var{alist}.

These procedures find the first pair in @var{alist} whose car field satisfies
a given condition, and returns that pair without traversing @var{alist} further.
If no pair in @var{alist} satisfies the condition, then #f is returned. The 
@code{assp} procedure successively applies @var{proc} to the car fields of
@var{alist} and looks for a pair for which it returns a true value. The
@code{assoc}, @code{assv}, and @code{assq} procedures look for a pair that has
@var{obj} as its car. The @code{assoc} procedure uses @code{equal?} or if @var{=}
is given use it to compare @var{obj} with the car fields of the pairs in @var{alist},
while @code{assv} uses @code{eqv?} and @code{assq} uses @cpde{eq?}.

Note: @code{member} and @code{assoc} procedures are the same behaviour as SRFI-1.
}

@define[Function]{@name{cons*} @args{obj1 obj2 ...}}
@desc{[R6RS] Like @code{list}, but the last argument provides the tail of the
constructed list.}
