[§2] (util file) - File operation utility library {#util.file}
-------------

###### [!Library] `(util file)` 

This library provides file operation utilities which is not exported from
`(sagittarius)` library.


### [§3] File to Scheme object operations

###### [!Function] `file->list`  _reader_ _path_ _:key_ _(transcoder_ _(native-transcoder))_

_reader_ must be a procedure accept one argument which is a port.
_path_ must be a string indicating existing file.

Returns a list which elements are read by _reader_ from the given _path_file. _reader_ reads the file contents until it reads EOF object.

The keyword argument _transcoder_ can specify which transcoder will be used
to open a port. It must be either #f or transcoder. The default value is the
value `(native-transcoder)` returns.


###### [!Function] `file->sexp-list`  _path_
###### [!Function] `file->string-list`  _path_

Thin wrapper of `file->list`. The procedures defined as following;

``(file->list `read/ss` _path_)``

``(file->list `get-line` _path_)``

respectively.


###### [!Function] `file->string`  _path_

Reads all file contents indicated _path_ as a string and return it.

###### [!Function] `file->bytevector`  _path_

Reads all file contents indicated _path_ as a bytevector and return
 it.

### [§3] Temporary directory operations

###### [!Function] `temporary-directory`  _:optional_ _path_

Returns operating system specific temporary directory path.

If optional argument _path_ is given, the procedure replace the temporary 
directory path.

###### [!Function] `make-temporary-file`  _:optional_ _prefix_

Creates temporary file and returns 2 values. One is the port, the other
one is created file path.

The created temporary file won't be deleted automatically so it is users
responsibility to delete.


### [§3] Path operations

###### [!Function] `decompose-path`  _path_

_path_ must be a string.

Decompose the given _path_ to directory path, base name, and extension then
returns these decomposed elements as 3 values.

The returning value can be #f if the path does not contains corresponding
element.


###### [!Function] `path-extension`  _path_

Returns the extension of given _path_. If it does not contains
extension then the procedure returns #f.

###### [!Function] `path-sans-extension`  _path_

Removes extension from given _path_ and return it.

###### [!Function] `path-basename`  _path_

Returns the basename of given _path_. If it does not contains
basename then the procedure returns #f.

###### [!Function] `find-files`  _target_ _:key_ _(pattern_ _#f)_ _(all_ _#t)_ _(sort_ _string<=?)_ _(recursive_ _#t)_

_target_ must be a string indicating existing file system path.

Find files from _target_ if it's a directory and return the found files as
a list.

Keyword arguments:

pattern
: Specifies the file pattern to be returned. This can be
  either string or regular expression pattern object.

all
: When this keyword argument is #t, then returns all files
  including hidden files which file name starting with `.`. If this is #f,
  the procedure excludes hidden files.

physical
: If this is #t, then the _proc_ is only given either
  `directory` or `file`. No symbolic likes are given.

sort
: This specifies how to sort the result list. If the value is #f,
  then the result order is unspecified.

recursive
: If this keyword argument is #t then the procedure finds
  files recursively, otherwise only the first _target_ directory.



###### [!Function] `path-for-each`  _path_ _proc_ _:key_ _(physical_ _#t)_ _(file-only_ _#f)_ _(absolute-path_ _#t)_ _
_ _(stop-on-file_ _#f)_ _(all_ _#t)_ _(recursive_ _#t)_

_path_ must be a string indicating existing file path. _proc_must be a procedure accepts 2 arguments, a path string and a symbol. The given
symbol can be `directory`, `symbolic-link` or `file`.

Apply given _proc_ to the found paths starting from _path_ and returns
unspecified value. The second argument of _proc_ indicates the file type
with following meaning;

`directory`
: The path is a directory.

`symbolic-link`
: The path is a symbolic link.

`file`
: The path is a file

The keyword arguments:

file-only
: If this is #t, then the _proc_ is only given a file.
  Otherwise all file types.

absolute-path
: If this is #t, then the _proc_ is given absolute
  path. Otherwise only the filename.

stop-on-false
: If this is #t, then when _proc_ returns #f the
  process will stop. Otherwise it continues the process.

The rest of the keyword arguments are the same as `find-files`.


###### [!Function] `path-map`  _path_ _proc_ _:key_ _(physical_ _#t)_ _(file-only_ _#f)_ _(absolute-path_ _#t)_ _(all_ _#t)_ _
_ _(recursive_ _#t)_

_path_ must be a string indicating existing file path. _proc_must be a procedure accepts 2 arguments, a path string and a symbol. The given
symbol can be `directory`, `symbolic-link` or `file`.

Process the path string starting from _path_ with given _proc_ and
returns a list which elements are the result value of _proc_.

The keyword arguments are the same as `path-for-each`.


###### [!Function] `build-path`  _paths_ _..._

Re-exported procedure from `(sagittarius)` for convenience. See
[Sagittarius extensions](#ext.sagittarius).


###### [!Function] `build-path*`  _paths_ _..._

_paths_ must be list of strings.

Compose given list to platform specific path. This procedure doesn't put path
separator at the end of composed string.


### [§3] Directory operations

###### [!Function] `create-directory`  _path_
###### [!Function] `delete-directory`  _path_

Re-exported procedures from `(sagittarius)` for convenience. See
[Sagittarius extensions](#ext.sagittarius).


###### [!Function] `create-directory*`  _path_
###### [!Function] `delete-directory*`  _path_

Convenient procedures to create or delete directories.

These are the same as UNIX command `mkdir -p` and `rm -rf`,
respectively.


###### [!Function] `copy-directory`  _src_ _dst_ _:key_ _(excludes_ _'())_ _options_ _..._

_src_ and _dst_ must be string and _src_ must indicates an
existing path.

Copies _src_ directory to _dst_. Keyword argument _excludes_ must be
a list of string and the procedure won't copy files which contain the
_excludes_ string(s).

The _options_ is passed to `path-for-each`.


