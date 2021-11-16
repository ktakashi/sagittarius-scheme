@subsection[:tag "record.builder"]{(record builder) - Record builder}

@define[Library]{@name{(record builder)}}
@desc{Record builder library. This library provides utilities to
construct a record instance easily.
}

The following example shows how to use the @code{make-record-builder}
macro.

@codeblock[=> "#<base a='a b=#f c=\"c\">"]{
(define-record-type base
  (fields a b c))
(define-syntax base-builder
  (make-record-builder base ((a 'a) (c 'c symbol->string))))

(base-builder)
}

@codeblock[=> "#<child a='b b=#f c=#f d=#f e=#f f=#f>"]{
(define-record-type child
  (parent base)
  (fields d e f))
(define-syntax child-builder
  (make-record-builder child ((a 'b))))

(child-builder)
}

@define[Macro]{@name{make-record-builder} @args{rtd (default @dots{})}}
@desc{Make a record builder macro transformer.

@var{Default} must be one of the following form:

@snipet{(@var{field-name} @var{value})}
@snipet{(@var{field-name} @var{value} @var{converter})}

@var{field-name} must be a field name of the @var{rtd}. @var{value} is a
default in case the builder didn't receive the field. The @var{converter}
of the second form must be a procedure which accepts one argument. The
result of this procedure will be used to instantisate the record. It
will also apply the @var{value}.


The result of the macro transformer accepts the following forms:

@snipet{(@var{field-name} @var{value}) @dots{}}
@snipet{(@code{from} @var{record-instance}) (@var{field} @var{value}) @dots{}}

The @code{from} is an auxiliary macro bound in the @code{(record builder)}.
The value of @code{from}, @var{record-instance}, will be copied to the
creating record.
}
