@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "lib.sagittarius.record"]{(sagittarius record) - Extra record inspection library}

@define[Library]{@name{(sagittarius record)}}
@desc{This library provides extra record operations.}

@define[Function]{@name{is-a?} @args{record record-type}}
@desc{@var{Record} must be a record which must return #t wich @code{record?}.
@var{Record-type} must be the name of the record which specified when record
is defined. Moreover @code{define-record-type}'s @var{record-name}. For more
detail see @secref["rnrs.records.syntactic.6"]{Records syntactic layer}.

The @code{is-a?} procedure returns #f if @var{record} is an instance of
@var{record-type}, otherwise #f.
}

@define[Function]{@name{slot-ref} @args{record-type record k}}
@desc{@var{Record-type} must be the name of the record which specified when record
is defined. @var{Record} must be a record which must return #t wich @code{record?}.
@var{K} must be non negative exact integer.

The @code{slot-ref} procedure return @var{record}'s @var{k}th value. The
@var{record-type} is ignored.
}

These procedures are actually for Alex Shin's pattern matching library. See
@secref["ported.match"]{(match) -- Pattern matching}

@define[Function]{@name{record-type?} @args{obj}}
@desc{Returns #t if @var{obj} is record type, otherwise #f.}

@define[Function]{@name{make-record-type} @args{name rtd rcd}}
@desc{@var{Name} must be symbol. @var{Rtd} must be record type descriptor.
@var{Rcd} must be record constructor descriptor.

Returns fresh record type.
}

@define[Function]{@name{record-type-rtd} @args{record-type}}
@desc{@var{Record-type} must be record type.

Returns associated rtd from @var{record-type}.
}

@define[Function]{@name{record-type-rtd} @args{record-type}}
@desc{@var{Record-type} must be record type.

Returns associated rcd from @var{record-type}.
}

Note: These procedures are not for using casually.