@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "io.condition.types"]{I/O condition types}

The condition types and corresponding predicates and accessors are exported by
both the @code{(rnrs io ports (6))} and @code{(rnrs io simple (6))} libraries.
They are also exported by the @code{(rnrs files (6))} library.

@define["Condition Type"]{@name{&i/o}}
@define[Function]{@name{make-i/o-error}}
@define[Function]{@name{i/o-error?} @args{obj}}
@desc{[R6RS] This is a supertype for a set of more specific I/O errors.}

@define["Condition Type"]{@name{&i/o-read}}
@define[Function]{@name{make-i/o-read-error}}
@define[Function]{@name{i/o-read-error?} @args{obj}}
@desc{[R6RS] This condition type describes read errors that occurred during an
I/O operation.}

@define["Condition Type"]{@name{&i/o-write}}
@define[Function]{@name{make-i/o-write-error}}
@define[Function]{@name{i/o-write-error?} @args{obj}}
@desc{[R6RS] This condition type describes write errors that occurred during an
I/O operation.}

@define["Condition Type"]{@name{&i/o-invalid-position}}
@define[Function]{@name{make-i/o-invalid-position-error} @args{position}}
@define[Function]{@name{i/o-invalid-position-error?} @args{obj}}
@define[Function]{@name{i/o-error-position} @args{condition}}
@desc{[R6RS] This condition type describes attempts to set the file position to
an invalid position. @var{Position} should be the file position that the program
intended to set. This condition describes a range error, but not an assertion
violation.}

@define["Condition Type"]{@name{&i/o-filename}}
@define[Function]{@name{make-i/o-filename-error} @args{filename}}
@define[Function]{@name{i/o-filename-error?} @args{obj}}
@define[Function]{@name{i/o-error-filename} @args{condition}}
@desc{[R6RS] This condition type describes an I/O error that occurred during an
operation on a named file. @var{Filename} should be the name of the file.}

@define["Condition Type"]{@name{&i/o-file-protection}}
@define[Function]{@name{make-i/o-file-protection-error} @args{filename}}
@define[Function]{@name{i/o-file-protection-error?} @args{obj}}
@desc{[R6RS] A condition of this type specifies that an operation tried to
operate on a named file with insufficient access rights.}

@define["Condition Type"]{@name{&i/o-file-is-read-only}}
@define[Function]{@name{make-i/o-file-is-read-only-error} @args{filename}}
@define[Function]{@name{i/o-file-is-read-only-error?} @args{obj}}
@desc{[R6RS] A condition of this type specifies that an operation tried to
operate on a named read-only file under the assumption that it is writeable.}

@define["Condition Type"]{@name{&i/o-file-already-exists}}
@define[Function]{@name{make-i/o-file-already-exists-error} @args{filename}}
@define[Function]{@name{i/o-file-already-exists-error?} @args{obj}}
@desc{[R6RS] A condition of this type specifies that an operation tried to
operate on an existing named file under the assumption that it did not exist.}

@define["Condition Type"]{@name{&i/o-file-does-not-exist}}
@define[Function]{@name{make-i/o-file-does-not-exist-error} @args{filename}}
@define[Function]{@name{i/o-file-does-not-exist-error?} @args{obj}}
@desc{[R6RS] A condition of this type specifies that an operation tried to
operate on an non-existent named file under the assumption that it existed.}

@define["Condition Type"]{@name{&i/o-port}}
@define[Function]{@name{make-i/o-port-error} @args{port}}
@define[Function]{@name{i/o-port-error?} @args{obj}}
@define[Function]{@name{i/o-error-port} @args{condition}}
@desc{[R6RS] This condition type specifies the port with which an I/O error
is associated. @var{Port} should be the port. Conditions raised by procedures
accepting a port as an argument should include an @code{&i/o-port-error}
condition.}

Here I describe the conditions hierarchy.

@codeblock{
+ &error(@secref["rnrs.conditions.6"]{See (rnrs conditions (6))})
    + &i/o
          + &i/o-read
          + &i/o-write
          + &i/o-invalid-position
          + &i/o-filename
                + &i/o-file-protection
                      + &i/o-file-is-read-only
                + &i/o-file-already-exists
                + &i/o-file-does-not-exist
          + &i/o-port-error
}

@include-section["rnrs/io/ports.scrbl"]
@include-section["rnrs/io/simple.scrbl"]