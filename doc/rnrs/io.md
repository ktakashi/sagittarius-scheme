[§2] I/O condition types {#io.condition.types}
-------------

The condition types and corresponding predicates and accessors are exported by
both the `(rnrs io ports (6))` and `(rnrs io simple (6))` libraries.
They are also exported by the `(rnrs files (6))` library.

###### [!Condition Type] `&i/o` 
###### [!Function] `make-i/o-error` 
###### [!Function] `i/o-error?`  _obj_

[R6RS] This is a supertype for a set of more specific I/O errors.

###### [!Condition Type] `&i/o-read` 
###### [!Function] `make-i/o-read-error` 
###### [!Function] `i/o-read-error?`  _obj_

[R6RS] This condition type describes read errors that occurred during an
I/O operation.

###### [!Condition Type] `&i/o-write` 
###### [!Function] `make-i/o-write-error` 
###### [!Function] `i/o-write-error?`  _obj_

[R6RS] This condition type describes write errors that occurred during an
I/O operation.

###### [!Condition Type] `&i/o-invalid-position` 
###### [!Function] `make-i/o-invalid-position-error`  _position_
###### [!Function] `i/o-invalid-position-error?`  _obj_
###### [!Function] `i/o-error-position`  _condition_

[R6RS] This condition type describes attempts to set the file position to
an invalid position. _Position_ should be the file position that the program
intended to set. This condition describes a range error, but not an assertion
violation.

###### [!Condition Type] `&i/o-filename` 
###### [!Function] `make-i/o-filename-error`  _filename_
###### [!Function] `i/o-filename-error?`  _obj_
###### [!Function] `i/o-error-filename`  _condition_

[R6RS] This condition type describes an I/O error that occurred during an
operation on a named file. _Filename_ should be the name of the file.

###### [!Condition Type] `&i/o-file-protection` 
###### [!Function] `make-i/o-file-protection-error`  _filename_
###### [!Function] `i/o-file-protection-error?`  _obj_

[R6RS] A condition of this type specifies that an operation tried to
operate on a named file with insufficient access rights.

###### [!Condition Type] `&i/o-file-is-read-only` 
###### [!Function] `make-i/o-file-is-read-only-error`  _filename_
###### [!Function] `i/o-file-is-read-only-error?`  _obj_

[R6RS] A condition of this type specifies that an operation tried to
operate on a named read-only file under the assumption that it is writeable.

###### [!Condition Type] `&i/o-file-already-exists` 
###### [!Function] `make-i/o-file-already-exists-error`  _filename_
###### [!Function] `i/o-file-already-exists-error?`  _obj_

[R6RS] A condition of this type specifies that an operation tried to
operate on an existing named file under the assumption that it did not exist.

###### [!Condition Type] `&i/o-file-does-not-exist` 
###### [!Function] `make-i/o-file-does-not-exist-error`  _filename_
###### [!Function] `i/o-file-does-not-exist-error?`  _obj_

[R6RS] A condition of this type specifies that an operation tried to
operate on an non-existent named file under the assumption that it existed.

###### [!Condition Type] `&i/o-port` 
###### [!Function] `make-i/o-port-error`  _port_
###### [!Function] `i/o-port-error?`  _obj_
###### [!Function] `i/o-error-port`  _condition_

[R6RS] This condition type specifies the port with which an I/O error
is associated. _Port_ should be the port. Conditions raised by procedures
accepting a port as an argument should include an `&i/o-port-error`condition.

Here I describe the conditions hierarchy.

``````````scheme
+ &error([See (rnrs conditions (6))](#rnrs.conditions.6))
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
``````````

* @[[rnrs/io/ports.md](io/ports.md)]
* @[[rnrs/io/simple.md](io/simple.md)]
