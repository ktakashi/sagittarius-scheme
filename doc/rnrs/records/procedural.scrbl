@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.records.procedural.6"]{Records procedural layer}

@define[Library]{@name{(rnrs records procedural (6))}}
@desc{The procedural layer is provided by the @code{(rnrs records procedural (6))}
library.
}

@define[Function]{@name{make-record-type-descriptor} @args{name parent uid sealed? opaque? fields}}
@desc{[R6RS] Returns a record-type descriptor (rtd).

The @var{name} argument must be a symbol. It names the record type, and is intended
purely for informational purposes.

The @var{parent} argument must be either #f or an rtd. If it is an rtd, the returned
record type, t, extends the record type @var{p} represented by @var{parent}. An
exception with condition type @code{&assertion} is raised if parent is sealed
(see below).

The @var{uid} argument must be either #f or a symbol. If @var{uid{ is a symbol,
the record-creation operation is nongenerative i.e., a new record type is created
only if no previous call to @code{make-record-type-descriptor} was made with the
@var{uid}. If @var{uid} is #f, the record-creation operation is generative, 
.e., a new record type is created even if a previous call to 
@code{make-record-type-descriptor} was made with the same arguments.

If @code{make-record-type-descriptor} is called twice with the same @var{uid}
symbol, the @var{parent} arguments in the two calls must be @code{eqv?}, the
@var{fields} arguments @code{equal?}, the @var{sealed?} arguments boolean-equivalent
(both #f or both true), and the @var{opaque?} arguments boolean-equivalent. If
these conditions are not met, an exception with condition type @code{&assertion}
is raised when the second call occurs. If they are met, the second call returns,
without creating a new record type, the same record-type descriptor (in the
sense of @code{eqv?}) as the first call.

The @var{sealed?} flag must be a boolean. If true, the returned record type is
sealed, i.e., it cannot be extended.

The @var{opaque?} flag must be a boolean. If true, the record type is opaque. If
passed an instance of the record type, @var{record?} returns #f. Moreover, if
@code{record-rtd} (see @secref["rnrs.records.inspection.6"]{(rnrs records inspection (6))})
is called with an instance of the record type, an exception with condition
type @code{&assertion} is raised. The record type is also opaque if an opaque
parent is supplied. If @var{opaque?} is #f and an opaque parent is not supplied,
the record is not opaque.

The @var{fields} argument must be a vector of field specifiers. Each field specifier
must be a list of the form @code{(mutable @var{name})} or a list of the form
@code{(immutable @var{name})}. Each name must be a symbol and names the corresponding
field of the record type; the names need not be distinct. A field identified as
mutable may be modified, whereas, when a program attempts to obtain a mutator for
a field identified as immutable, an exception with condition type @code{&assertion}
is raised. Where field order is relevant, e.g., for record construction and field
access, the fields are considered to be ordered as specified, although no particular
order is required for the actual representation of a record instance.

The specified fields are added to the parent fields, if any, to determine the
complete set of fields of the returned record type. If fields is modified after
@code{make-record-type-descriptor} has been called, the effect on the returned
rtd is unspecified.

A generative record-type descriptor created by a call to
@code{make-record-type-descriptor} is not @code{eqv?} to any record-type descriptor
(generative or nongenerative) created by another call to
@code{make-record-type-descriptor}. A generative record-type descriptor is @code{eqv?}
only to itself, i.e., (eqv? rtd1 rtd2) iff (eq? rtd1 rtd2). Also, two nongenerative
record-type descriptors are @code{eqv?} if they were created by calls to
@code{make-record-type-descriptor} with the same uid arguments.
}

@define[Function]{@name{record-type-descriptor?} @args{obj}}
@desc{[R6RS] Returns #t if the argument is a record-type descriptor, #f otherwise.}

@define[Function]{@name{make-record-constructor-descriptor} @args{rtd parent-constructor-descriptor protocol}}
@desc{[R6RS] Returns a @var{record-constructor descriptor} (or 
var{constructor descriptor} for short) that specifies a @var{record constructor}
(or @var{constructor} for short), that can be used to construct record values of
the type specified by @var{rtd}, and which can be obtained via @code{record-constructor}.
A constructor descriptor can also be used to create other constructor descriptors
for subtypes of its own record type. @var{Rtd} must be a record-type descriptor.
@var{Protocol{ must be a procedure or #f. If it is #f, a default protocol procedure
is supplied.

If @var{protocol} is a procedure, it is handled analogously to the protocol
expression in a @code{define-record-type} form.

If @var{rtd} is a base record type and protocol is a procedure, 
@var{parent-constructor-descriptor} must be #f. In this case, @var{protocol}
is called by @code{record-constructor} with a single argument @var{p}. @var{P}
is a procedure that expects one argument for every field of @var{rtd} and returns
a record with the fields of @var{rtd} initialized to these arguments. The
procedure returned by protocol should call @var{p} once with the number of
arguments @var{p} expects and return the resulting record as shown in the
simple example below:

@codeblock{
(lambda (p)
  (lambda (v1 v2 v3)
    (p v1 v2 v3)))
}

Here, the call to @var{p} returns a record whose fields are initialized with
the values of @code{v1}, @code{v2}, and @code{v3}. The expression above is
equivalent to @code{(lambda (p) p)}. Note that the procedure returned by protocol
is otherwise unconstrained; specifically, it can take any number of arguments.

If @var{rtd} is an extension of another record type @var{parent-rtd} and
@var{protocol} is a procedure, @var{parent-constructor-descriptor} must be a
constructor descriptor of @var{parent-rtd} or #f. If 
@var{parent-constructor-descriptor} is a constructor descriptor, @var{protocol}
it is called by @var{record-constructor} with a single argument @var{n}, which
is a procedure that accepts the same number of arguments as the constructor of
@var{parent-constructor-descriptor} and returns a procedure @var{p} that, when
called, constructs the record itself. The @var{p} procedure expects one argument
for every field of @var{rtd} (not including parent fields) and returns a record
with the fields of @var{rtd} initialized to these arguments, and the fields of
@var{parent-rtd} and its parents initialized as specified by
@var{parent-constructor-descriptor}.

The procedure returned by @var{protocol} should call @var{n} once with the number
of arguments @var{n} expects, call the procedure @var{p} it returns once with
the number of arguments @var{p} expects and return the resulting record. A
simple @var{protocol} in this case might be written as follows:

@codeblock{
(lambda (n)
  (lambda (v1 v2 v3 x1 x2 x3 x4)
    (let ((p (n v1 v2 v3)))
      (p x1 x2 x3 x4))))
}

This passes arguments @code{v1}, @code{v2}, @code{v3} to n for
@var{parent-constructor-descriptor} and calls @var{p} with @code{x1}, @dots{},
@code{x4} to initialize the fields of @var{rtd} itself.

Thus, the constructor descriptors for a record type form a sequence of protocols
parallel to the sequence of record-type parents. Each constructor descriptor in
the chain determines the field values for the associated record type. Child record
constructors need not know the number or contents of parent fields, only the number
of arguments accepted by the parent constructor.

@var{Protocol} may be #f, specifying a default constructor that accepts one
argument for each field of @var{rtd} (including the fields of its parent type,
if any). Specifically, if @var{rtd} is a base type, the default protocol procedure
behaves as if it were @code{(lambda (p) p)}. If @var{rtd} is an extension of
another type, then @var{parent-constructor-descriptor} must be either #f or
itself specify a default constructor, and the default protocol procedure behaves
as if it were:

@codeblock{
(lambda (n)
  (lambda (v1 ... vj x1 ... xk)
    (let ((p (n v1 ... vj)))
      (p x1 ... xk))))
}

The resulting constructor accepts one argument for each of the record type's complete
set of fields (including those of the parent record type, the parent's parent record
type, etc.) and returns a record with the fields initialized to those arguments,
with the field values for the parent coming before those of the extension in the
argument list. (In the example, @var{j} is the complete number of fields of the
parent type, and @var{k} is the number of fields of rtd itself.)

If @var{rtd} is an extension of another record type, and @var{parent-constructor-descriptor}
or the @var{protocol} of @var{parent-constructor-descriptor} is #f, protocol must
also be #f, and a default constructor descriptor as described above is also assumed.
}

@define[Function]{@name{record-constructor} @args{constructor-descriptor}}
@desc{[R6RS] Calls the @var{protocol} of @var{constructor-descriptor} (as described
for @code{make-record-constructor-descriptor}) and returns the resulting constructor
constructor for records of the record type associated with 
@var{constructor-descriptor}.
}

@define[Function]{@name{record-predicate} @args{rtd}}
@desc{[R6RS] Returns a procedure that, given an object @var{obj}, returns #t if
@var{obj} is a record of the type represented by @var{rtd}, and #f otherwise.
}

@define[Function]{@name{record-accessor} @args{rtd k}}
@desc{[R6RS] @var{K} must be a valid field index of @var{rtd}. The
@code{record-accessor} procedure returns a one-argument procedure whose argument
must be a record of the type represented by @var{rtd}. This procedure returns
the value of the selected field of that record.

The field selected corresponds to the @var{k}th element (0-based) of the fields
argument to the invocation of @code{make-record-type-descriptor} that created
@var{rtd}. Note that @var{k} cannot be used to specify a field of any type 
@var{rtd} extends.
}

@define[Function]{@name{record-mutator} @args{rtd k}}
@desc{[R6RS] @var{K} must be a valid field index of @var{rtd}. The
@code{record-mutator} procedure returns a two-argument procedure whose arguments
must be a record record @var{r} of the type represented by @var{rtd} and an
object @var{obj}. This procedure stores @var{obj} within the field of @var{r}
specified by @var{k}. The @var{k} argument is as in @code{record-accessor}. If
@var{k} specifies an immutable field, an exception with condition type
@code{&assertion} is raised. The mutator returns unspecified values.
}
