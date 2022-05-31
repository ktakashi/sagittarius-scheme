[§2] (archive) - Generic archive interface {#archive}
-------------

###### [!Library] `archive` 

This library provides generic interface to access archive libraries.
Sagittarius supports `tar` and `zip`.


Following code describes a typical use of the library;

``````````scheme
(import (rnrs) (archive))

;; extract file "bar.txt" from "foo.zip"
(call-with-input-archive-file 'zip "foo.zip"
  (lambda (zip-in)
    (do-entry (e zip-in)
      (when (string=? (archive-entry-name e) "bar.txt")
        (call-with-output-file "bar.txt"
          (lambda (out) (extract-entry e out))
           :transcoder #f)))))

;; archive "bar.txt" into foo.tar
(call-with-output-archive-file 'tar "foo.tar"
  (lambda (tar-out)
    (append-entry! tar-out (create-entry tar-out "bar.txt"))))

``````````

Following sections use _type_ as a supported archive type. More precisely,
if it's a supported archive type then there must be a library named
`(archive _type_)`.

### [§3] Archive input

###### [!Function] `make-input-archive`  _type_ _input-port_

_type_ must be a symbol and supported archive type.
_input-port_ must be a binary input port.

Creates an archive input which represents the specified type of archive.


###### [!Method] `next-entry!`  _archive-input_

Retrieves next entry of the given archive input. If there is no entry,
then it returns #f.


###### [!Macro] `do-entry`  _(entry_ _archive-input)_ _body_ _..._
###### [!Macro] `do-entry`  _(entry_ _archive-input_ _result)_ _body_ _..._

Convenient macro. Iterates the given _archive-input_'s entries.

The macro is expanded like this;

``````````scheme
(do ((_entry_ (next-entry! _archive-input_) (next-entry! _archive-input_)))
    ((not _entry_) _result_)
  _body_ ...)
``````````

If the first form is used, then _result_ is #t.


###### [!Method] `extract-entry`  _entry_ _output-port_

Extract the given archive entry _entry_ to binary output port
_output-port_.


###### [!Function] `extract-all-entries`  _archive-input_ _:key_ _(destinator_ _archive-entry-name)_ _(overwrite_ _#f)_

Convenient function. Extracts all entries in the given
_archive-input_ to the file specified by _destinator_.

The keyword argument _destinator_ must be a procedure which accepts
one argument, archive entry, and return a string represents the 
file/directory path.

The keyword argument _overwrite_ is #t, then it overwrites the file.
If it is #f and there is a file, then it raises an error.


###### [!Method] `finish!`  _archive-input_

Finalize the given archive input.

###### [!Function] `call-with-input-archive`  _archive-input_ _proc_

_archive-input_ must be an archive input.
_proc_ must be a procedure which accepts one argument.

Call the _proc_ with archive input and returns the result of the
_proc_.

The _archive-input_ is finalized by `finish!`.


###### [!Function] `call-with-input-archive-port`  _type_ _input-port_ _proc_

Creates an archive input with _type_ and _input-port_, then
call `call-with-input-archive`.


###### [!Function] `call-with-input-archive-file`  _type_ _file_ _proc_

Open file binary input port with given _file_ and call
`call-with-input-archive-port`.


### [§3] Archive output

###### [!Function] `make-output-archive`  _type_ _output-port_

_type_ must be a symbol.
_output-port_ must be a output port.

Creates an archive output which represents the specified type of archive.


###### [!Method] `create-entry`  _archive-output_ _file_

Creates an archive entry from the given _file_.

For implementing user defined archive;

This method is defined like following on the interface library:

``````````scheme
(define-method create-entry ((out <archive-output>) file)
  (create-entry out file file))
``````````

So as long as it doesn't have to be distinguished, users don't have to
implement this method.


###### [!Method] `create-entry`  _archive-output_ _entry-name_ _file_

Creates an archive entry from the given _file_. The entry's name
is _entry-name_. This is useful when users want to append entry with
different name from file name.

###### [!Method] `append-entry!`  _archive-output_ _entry_

Appends the given _entry_ to _archive-output_.

###### [!Method] `finish!`  _archive-output_

Finalize the given archive output.

###### [!Function] `call-with-output-archive`  _archive-output_ _proc_

_archive-output_ must be an archive output.
_proc_ must be a procedure which accepts one argument.

Call the _proc_ with archive input and returns the result of the 
_proc_.

The _archive-output_ is finalized by `finish!`.


###### [!Function] `call-with-output-archive-port`  _type_ _output-port_ _proc_

Creates an archive output with _type_ and _output-port_, then
call `call-with-output-archive`.


###### [!Function] `call-with-output-archive-file`  _type_ _file_ _proc_

Open file binary output port with given _file_ and call
`call-with-output-archive-port`.


### [§3] Entry accessor

###### [!Function] `archive-entry-name`  _entry_

Returns the name of _entry_.

###### [!Function] `archive-entry-type`  _entry_

Returns the type of _entry_. It is either `file` or 
`directory`.


### [§3] Implementing archive implementation library

To support other archive such as RAR, then you need to create a implementation
library.

###### [!Library] `(archive interface` 

The library defines all abstract class and method for the generic
archive access.


To support _foo_ archive, then the library name must be 
code{(archive _foo_)} and it must import `(archive interface)`.
So the library code should look like this;

``````````scheme
(library (archive foo)
  (export) ;; no export procedure is needed
  (import (rnrs)
          (close user)
          (archive interface)
          ;; so on
          ...)
  ;; class and method definitions
  ...)
``````````

For archiving, the implementation needs to implement following methods and
extends following classes;

``````````scheme
make-archive-input, next-entry, extract-entry
``````````

``````````scheme
<archive-input> <archive-entry>
``````````

For extracting, the implementation needs to implement following methods and
extends following classes;

``````````scheme
make-archive-output, create-entry, append-entry!, finish!
``````````

``````````scheme
<archive-output> <archive-entry>
``````````

NOTE: `<archive-entry>` may be shared between archiving and extracting.

###### [!Class] `<archive-input>` 

Abstract class of the archive input. This class has the following
slot;

source
: Source of the archive. For compatibility of other archive, this should be
  a binary input port.



###### [!Class] `<archive-output>` 

Abstract class of the archive output. This class has the following
slot;

sink
: Destination of the archive. For compatibility of other archive, this
  should be a binary output port.



###### [!Class] `<archive-entry>` 

Abstract class of the archive entry. This class has the following
slots;

name
: Entry name. 

type
: Entry type. For compatibility of other archive, this must be `file` or
  `directory`.



###### [!Method] `make-archive-input`  _type_ _(source_ _<port>)_
###### [!Method] `make-archive-output`  _type_ _(sink_ _<port>)_

Creates an archive input or output. _type_ specifies the
archive type. It is recommended to use `eql` specializer to specify.


###### [!Method] `finish!`  _(in_ _<archive-input>)_

The `finish!` method for archive input has a default
implementation and it does nothing.

Users can specialize the method for own archive input.


The other methods must be implemented as it's described in above section.