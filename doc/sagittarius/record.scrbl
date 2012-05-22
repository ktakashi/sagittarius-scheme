@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "lib.sagittarius.record"]{(sagittarius record) - Extra record inspection library}

@define[Library]{@name{(sagittarius record)}}
@desc{This library provides extra record operations.}

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