@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "lib.sagittarius.generators"]{(sagittarius generators) - Generators}

@define[Library]{@name{(sagittarius generators)}}
@desc{This library provides procedures for generator.

@; From SRFI-121 Abstract
A generator is simply a procedure with no arguments that works as a source
of a series of values. Every time it is called, it yields a value. Generators
may be finite or infinite; a finite generator returns an EOF object to 
indicate that it is exhausted. For example, read-char is a generator that
generates characters from the current input port. Generators provide 
lightweight laziness. 
}


@subsubsection{Generator constructors}

The following procedures creates a generator. Except @code{null-generator},
all procedures have prefix @code{'g'}. Arguments named @var{generator}
indicates a generator.

@define[Function]{@name{null-generator}}
@desc{Returns a generator which always returns EOF object.}

@define[Function]{@name{giota}
 @args{:optional (count +inf.0) (start 0) (step 1)}}
@desc{Returns a generator which returns @var{count} number of numbers.
The returning numbers start with @var{start} and increased by @var{step}.

@snipet[=> (0 1 2 3 4)]{(generator->list (giota 5))}
@snipet[=> (10 11 12 13 14)]{(generator->list (giota 5 10))}
@snipet[=> (10 12 14 16 18)]{(generator->list (giota 5 10 2))}

If @var{count} is not given, then the generator is inifinte.
}

@define[Function]{@name{grange}
 @args{:optional (start 0) (end +inf.0) (step 1)}}
@desc{Returns a generator which returns numbers in range of @var{start} and
@var{end}. The returning numbers are increased by @var{step}.
}

@define[Function]{@name{gunfold}
 @args{stop? mapper successor seed :optional (tail-gen #f)}}
@desc{A generator constructor similar to @code{unfold}.

@; TBD

@codeblock[=> (0 2 4 6 8 10)]{
(generator->list (gunfold
                      (lambda (s) (> s 5))
                      (lambda (s) (* s 2))
                      (lambda (s) (+ s 1))
                      0))
}
}

@define[Function]{@name{list->generator} @args{list}}
@define[Function]{@name{vector->generator} @args{vector}}
@define[Function]{@name{reverse-vector->generator} @args{vector}}
@define[Function]{@name{string->generator} @args{string}}
@define[Function]{@name{bytevector->generator} @args{bytevector}}
@desc{Generator constructors. The returning generator returns the items
taken from given argument from the beginning of the given sequence to
the end. Except @code{reverse-vector->generator} which return end to beginning.
}

@define[Function]{@name{port->char-generator} @args{port}}
@define[Function]{@name{port->byte-generator} @args{port}}
@desc{Generator constructors. The returning generator returns the items read
from the given @var{port}. The @code{port->char-generator} uses @code{get-char}
to read the port. The @code{port->byte-generator} uses @code{get-u8}.
}

@define[Generic]{@name{->generator}}
@desc{Generic constructor of generators. By default, the following methods are
defined and dispatched above generator constrocturs.

@code{<list>}, @code{<vector>}, @code{<string>}, @code{<bytevector>} and
@code{<port>}.

If the given argument is type of  @code{<vector>}, then @code{vector->generator}
is used. If the given argument is type of @code{<port>}, then it checks if
it's binary or textual and dispatches apropriate procedure.
}

@subsubsection{Generator operations}

@define[Function]{@name{gcons*} @args{object ... generator}}
@desc{Returns a generator which adds @var{object}s in front of @var{generator}.
}

@define[Function]{@name{gappend} @args{generator ...}}
@desc{Returns a generator which yields values from the first @var{generator}
and when it's exhausted continues to next.
}

@define[Function]{@name{gcombine} @args{proc seed generator generators ...}}
@desc{Returns a generator for mapping with state. It yields a sequence of 
sub-folds over @var{proc}. 

The @var{proc} argument is a procedure which takes as many arguments as 
the input generators plus one. It is called as 
@code{(@var{proc} @var{v1} @var{v2} ... @var{seed})}, where 
@var{v1}, @var{v2},@code{...} are the values yielded from the input 
generators, and @var{seed} is the current seed value. It must return two
values, the yielding value and the next seed. 
}

@define[Function]{@name{gfilter} @args{pred generator}}
@define[Function]{@name{gremove} @args{pred generator}}
@desc{Return generators which yield the items from the source generator, 
except those on which @var{pred} returns false or true respectively. 
}

@define[Function]{@name{gtake} @args{generator k :optional padding}}
@define[Function]{@name{gdrop} @args{generator k}}
@desc{Return generators which take or drop @var{k} items from @var{generator},
respectively. Returning generators won't raise errors when it's exhausted
before reaching @var{k}.

Optional argument @var{padding} for @code{gtake} is passed, then the value
is filled until the procedure reaches @var{k}.

These procedures are analogues of SRFI-1 @code{take} and @code{drop}.
}

@define[Function]{@name{gtake-while} @args{generator pred}}
@define[Function]{@name{gdrop-while} @args{generator pred}}
@desc{Return generators which take or drop until procedure @var{pred} returns
false value respectively.

These procedures are analogues of SRFI-1 @code{take-while} 
and @code{drop-while}.
}

@define[Function]{@name{gdelete} @args{item generator :optional (= equal?)}}
@desc{Returns a generator which returns items @var{generator} returns, except
items which are the same as @var{item} in sense of @var{=}.
}

@define[Function]{@name{gdelete-neighbor-dups}
 @args{generator :optional (= equal?)}}
@desc{Returns a generator which returns items @var{generator} returns, except
items which are the same as the proceeding item in sense of @var{=}.
}

@define[Function]{@name{gselect} @args{value-generator index-generator}}
@desc{Returns a generator which returns the items generated by
@var{value-generator} of specified by the indice generated by 
@var{index-generator}. The indice must be non negative integer and
increased strictly. Otherwise an error is raised.

@codeblock[=> (a c e)]{
(generator->list (gindex (list->generator '(a b c d e f))
                 (list->generator '(0 2 4))))}
}

@define[Function]{@name{gselect} @args{value-generator truth-generator}}
@desc{Returns a generator which returns the items generated by
@var{value-generator} that correspond to the items generated by 
@var{truth-generator}. If @var{truth-generator} returns true value,
then the current value of @var{value-generator} is returned. Otherwise not.

@codeblock[=> (a d e)]{
(generator->list (gselect (list->generator '(a b c d e f))
                          (list->generator '(#t #f #f #t #t #f))))}
}

@define[Function]{@name{gconcatenate} @args{generator}}
@desc{@var{generator} must be an generator generates generators.

Returns a generator which returns the items generated by the generators
generated by given @var{generator}. It is similar to the following:
@codeblock{
(apply gappend (generator->list generator))
}
The difference is that this procedure can handle infinite generator.
}


@define[Function]{@name{gflatten} @args{generator}}
@desc{@var{generator} must be a generator which returns lists as its items.

Returns a generator which flatten the items generated by given @var{generator}.
@codeblock[=> (1 2 3 4 a b c d A B C D)]{
(generator->list (gflatten (list->generator (list '(1 2 3 4)
                                                  '(a b c d)
                                                  '(A B C D)))))
}

If the @var{generator} returns non list item, then it is ignored.
@codeblock[=> (a b c d A B C D)]{
(generator->list (gflatten (list->generator (list 'ignored
                                                  '(a b c d)
                                                  'ignored
                                                  '(A B C D)))))
}
This behaviour is an error behaviour so it might be changed in future.
So users should not depend on this.
}

@define[Function]{@name{gmerge} @args{compare generator1 generator2 @dots{}}}
@desc{@var{compare} must be a procedure which accepts 2 arguments and returns
boolean value. The procedure should compare the given 2 arguments and return
true value if the first argument is smaller than the second argument.

Returns a generator which returns the ordered items determined by @var{compare}
procedure. If the @code{gmerge} procedure is called only one argument, then 
it simply returns a generator (if @var{generator1} isn't a generator then
it is coerced).

@codeblock[=> (0 1 2 3 4 5)]{
(generator->list (gmerge < (list->generator '(1 4 5)) (list->generator '(0 2 3))))
}
}

@define[Function]{@name{gmap} @args{proc generator1 generator2 @dots{}}}
@desc{Returns a generator which returns the items returned by @code{proc}.

The @code{proc} is called with the items returned by @var{generator1} and
@var{generator2} if it's given.

The @var{gmap} procedure accepts uneven length of generators however one
of the generator must be finite length, otherwise it won't be exhausted.

It is an analogy of @code{map}.
}

@define[Function]{@name{gfilter-map} @args{proc generator1 generator2 @dots{}}}
@desc{Returns a generator which returns the items returnd by @code{proc}.

This procedure is similar with @code{gmap}. The difference is that the 
returning item is filtered if the returning value of @var{proc} is #f.

It is an analogy of @code{filter-map}.
}

