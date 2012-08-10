@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.uuid"]{(rfc uuid) - UUID generate library}

@define[Library]{@name{(rfc uuid)}}
@desc{This library provides RFC 4122 a Universally Unique IDentifier (UUID)
URN Namespace procedures.}


@define[Class]{@name{<uuid>}}
@desc{The class representing UUID.}

@subsubsection{Predicates}

@define[Function]{@name{uuid?} @args{obj}}
@desc{Returns #t if the @var{obj} is an instance of @code{<uuid>}, otherwise
 #f.}

@define[Function]{@name{uuid=?} @args{uuid1 uuid2}}
@desc{Compares given 2 uuids and return #t if both have the same values,
otherwise #f.}

@subsubsection{Constructors}

@define[Function]{@name{make-null-uuid}}
@desc{Creates a null (empty) uuid.

The returning object will be represented like this UUID;

@code{00000000-0000-0000-0000-000000000000}.
}

@define[Function]{@name{make-v1-uuid}}
@define[Function]{@name{make-v3-uuid} @args{namespace name}}
@define[Function]{@name{make-v4-uuid}}
@define[Function]{@name{make-v5-uuid} @args{namespace name}}
@desc{Creates version 1, 3, 4 and 5 UUIDs respectively.

For version 3 and 5, procedures need to take 2 arguments, @var{namespace}
and @var{name}. @var{namespace} must be a UUID object, and @var{name} must be a
string.
}

@subsubsection{Predefined namespaces}

@define[Constant]{@name{+namespace-dns+}}
@define[Constant]{@name{+namespace-url+}}
@define[Constant]{@name{+namespace-oid+}}
@define[Constant]{@name{+namespace-x500+}}
@desc{Constant predefined namespace of UUIDs.}

@subsubsection{Converters}

@define[Function]{@name{uuid->bytevector} @args{uuid}}
@desc{Returns bytevector converted from given @var{uuid}.

@snipet[=> "#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)"]{(uuid->bytevector (make-null-uuid))}
}

@define[Function]{@name{uuid->string} @args{uuid}}
@desc{Returns string represented uuid converted from given @var{uuid}.

@snipet[=> "00000000-0000-0000-0000-000000000000"]{(uuid->string (make-null-uuid))}
}

@define[Function]{@name{bytevector->uuid} @args{bv}}
@desc{Returns uuid object generated from @var{bv}.

Given bytevector must have length at least 16.
}

@define[Function]{@name{string->uuid} @args{string}}
@desc{Returns uuid object generated from @var{string}.

Given @var{string} must be proper format of UUID defined in RFC 4122.}

@define[Function]{@name{uuid->urn-format} @args{uuid}}
@desc{Returns URN formatted string of given @var{uuid}.
}