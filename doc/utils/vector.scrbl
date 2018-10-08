@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.vector"]{(util vector) - Extra vector utility library}

@define[Library]{@name{(util vector)}}
@desc{This library provides extra vector utility procedures which is not
provided neither @code{(srfi :43 vectors)} nor @code{(srfi :133 vectors)}.
}

@define[Function]{@name{vector-filter} @args{pred vec}}
@define[Function]{@name{vector-remove} @args{pred vec}}
@desc{Returns newly allocated vector which contains the elements from the
given @var{vec} satisfied the given @var{pref}.

The @code{vector-filter} uses the elements which @var{pred} returns true value.

The @code{vector-remove} removes the elements which @var{pred} returns
true value.
}

@define[Function]{@name{vector-find} @args{pred vec}}
@desc{Returns the element of the given @var{vec} satisfies the @var{pred}
or #f.}
