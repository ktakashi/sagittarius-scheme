@subsection[:tag "record.builderr"]{(record builder) - Record builder}

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

@define[Macro]{@name{make-record-builder} @args{rtd (defaults @dots{})}}
@desc{Make a record builder macro.

}
