@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.records.inspection.6"]{Records inspection}

@define[Library]{@name{(rnrs records syntactic (6))}}
@desc{The @code{(rnrs records inspection (6))}library provides procedures for
inspecting records and their record-type descriptors. These procedures are designed
to allow the writing of portable printers and inspectors.
}

@define[Function]{@name{record?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a record, and its record type is not
opaque, and returns #f otherwise.
}

@define[Function]{@name{record-rtd} @args{record}}
@desc{[R6RS] Returns the rtd representing the type of @var{record} if the type
is not opaque. The rtd of the most precise type is returned; that is, the type
@var{t} such that @var{record} is of type @var{t} but not of any type that
extends @var{t}. If the type is opaque, an exception is raised with condition
type @code{&assertion}.
}

@define[Function]{@name{record-type-name} @args{rtd}}
@define[Function]{@name{record-type-parent} @args{rtd}}
@define[Function]{@name{record-type-uid} @args{rtd}}
@desc{[R6RS] Returns the name/parent/uid of the record-type descriptor @var{rtd}.
}

@define[Function]{@name{record-type-generative?} @args{rtd}}
@define[Function]{@name{record-type-sealed?} @args{rtd}}
@define[Function]{@name{record-type-opaque?} @args{rtd}}
@desc{[R6RS] Returns #t if the record-type descriptor is generative/sealed/opaque,
and #f if not.
}

@define[Function]{@name{record-type-field-names} @args{rtd}}
@desc{[R6RS] Returns a vector of symbols naming the fields of the type represented
by @var{rtd} (not including the fields of parent types) where the fields are
ordered as described under @code{make-record-type-descriptor}.
}

@define[Function]{@name{record-type-mutable?} @args{rtd k}}
@desc{[R6RS] Returns #t if the field specified by @var{k} of the type represented
by @var{rtd} is mutable, and #f if not. @var{K} is as in @code{record-accessor}.
}
