@; -*- coding: utf-8 -*-
@subsection[:tag "util.heap"]{(util heap) - Heap}

@define[Library]{@name{(util heap)}}
@desc{This library provides heap data structure and its operations. The
implementation of heap is Fibonacci heap. Running time of heap operations
are followings;

@itemlist[
  @item{Insert(key, data): O(1)}
  @item{FindMin(): O(1)}
  @item{DeleteMin(): Amortized O(log n)}
  @item{Delete(node): Amortized O(log n)}
  @item{DecreaseKey(node): Amortized O(1)}
  @item{Merge(heap1, heap2): O(1)}
  @item{Search(key): O(n)}
]

}

@subsection{Constructors, predicates and accessors}

@define[Class]{@name{<heap>}}
@desc{A class of Fibonacci heap.}

@define[Function]{@name{heap?} @args{obj}}
@desc{Return #t if @var{obj} is a heap, otherwise #f.}

@define[Function]{@name{make-heap} @args{compare}}
@desc{@var{compare} must be a procedure which takes 2 argument and
returns an exact integer indicates the order of given arguments.

Creates a heap.
}

@define[Function]{@name{copy-heap} @args{heap}}
@desc{Creates copy of @var{heap}.}

@define[Function]{@name{alist->heap} @args{compare alist}}
@desc{@var{compare} must be a procedure which takes 2 argument and
returns an exact integer indicates the order of given arguments.

Creates a heap whose keys and values are car/cdr part of @var{alist}.
}

@define[Function]{@name{heap-empty?} @args{heap}}
@desc{Returns #t if @var{heap} is empty, otherwise #f.}

@define[Function]{@name{heap-size} @args{heap}}
@desc{Returns the number of entries of @var{heap}.}


@define[Function]{@name{heap-compare} @args{heap}}
@desc{Returns comparison procedure of @var{heap}.}

@define[Function]{@name{heap-entry?} @args{obj}}
@desc{Returns #t if @var{obj} is a heap entry, otherwise #f.}

@define[Function]{@name{heap-entry-key} @args{entry}}
@define[Function]{@name{heap-entry-value} @args{entry}}
@desc{Returns key and value of @var{entry}, respectively.}

@define[Function]{@name{heap-entry-value-set!} @args{entry value}}
@desc{Sets @var{value} as @var{entry}'s value.}

@subsubsection{Heap operations}

@define[Function]{@name{heap-min} @args{heap}}
@desc{Returns the smallest entry of @var{heap}. If @var{heap} is empty,
then #f is returned.

To get the key and value from the returning entry, use @code{heap-entry-key}
and @code{heap-entry-value} procedures, respectively.
}

@define[Function]{@name{heap-set!} @args{heap key value}}
@desc{Inserts an entry whose key is @var{key}, value is @var{value} and
returns the entry.
}

@define[Function]{@name{heap-extract-min!} @args{heap}}
@desc{Removes smallest entry and returns it. It is an error if @var{heap}
is empty.
}

@define[Function]{@name{heap-delete!} @args{heap entry/key}}
@desc{Removes target @var{entry/key} from @var{heap} and returns the
removed entry.

If @var{entry/key} is an entry then this operation is done in amortized
O(log n). If not, then O(n).

NOTE: If @var{entry/key} is an entry, then it must exist in @var{heap}.
However the procedure won't check so it is user's responsibility to
make sure.
}

@define[Function]{@name{heap-delete!} @args{heap entry/key new-key}}
@desc{Change the key value of given @var{entry/key} to @var{new-key}. The
@var{new-key} must be smaller than current key in sense of the returning
value of @code{heap-compare}, otherwise it's an error.

If @var{entry/key} is an entry then this operation is done in amortized
O(log n). If not, then O(n).

NOTE: If @var{entry/key} is an entry, then it must exist in @var{heap}.
However the procedure won't check so it is user's responsibility to
make sure.
}

@define[Function]{@name{heap-clear!} @args{heap}}
@desc{Clears all entry of @var{heap} and returns @var{heap}.}

@define[Function]{@name{heap-merge!} @args{heap1 heap2}}
@desc{Merge @var{heap2} into @var{heap1} and empty it. Then returns @var{heap1}.

This procedure changes both heaps destructively, if you want to kepp
@var{heap2} intact, use @code{merge-heaps} or @code{merge-heaps!} instead.
}

@define[Function]{@name{merge-heaps} @args{heap1 heap2 more @dots{}}}
@define[Function]{@name{merge-heaps!} @args{heap1 heap2 more @dots{}}}
@desc{Merges heap and returns merged heap.

If the first procedure is used then the returning heap is freshly created
according to @var{heap1}. The second form merged the rest of heaps to
@var{heap1}.

The running time of these procedures is O(nm) where m is number of heaps to
merge.
}

@define[Function]{@name{heap-ref} @args{heap key :optional (fallback #f)}}
@desc{Returns entry value associated with @var{key} in @var{heap}. If there
is no entry, then @var{fallback} is returned.

The running time of this procedures is O(n).
}

@define[Function]{@name{heap-update!} @args{heap key proc default}}
@desc{@var{proc} must be a procedure accepts one argument.

Updates the entry value associated to @var{key} with the returned value of 
@var{proc}. If the entry doesn't exist then @var{default} will be passed to
the @var{proc}.

The running time of this procedures is O(n).
}

@define[Function]{@name{heap-search} @args{heap key :optional finish}}
@desc{Searches the entry associated to @var{key} and returns it. If there is
no entry then #f is returned.

If optional argument @var{finish} given then it must be a procedure which
takes one argument. The procedure is called when search process is finished.
The given argument is either an entry of #f.
}