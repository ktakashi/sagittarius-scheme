[§2] File system {#rnrs.files.6}
-------------

###### [!Library] `(rnrs files (6))` 

This library, in addition to the procedures described here, also exports
the I/O condition types described in [I/O condition types](#io.condition.types).


###### [!Function] `file-exists?`  _filename_

[R6RS] _Filename_ must be a file name (see [Port I/O](#rnrs.io.ports.6)).
The `file-exists?` procedure returns #t if the named file exists at the time
the procedure is called, #f otherwise. 


###### [!Function] `delete-file`  _filename_

[R6RS] _Filename_ must be a file name (see [Port I/O](#rnrs.io.ports.6)).
The `delete-file` procedure deletes the named file if it exists and can be
deleted, and returns unspecified values. If the file does not exist or cannot be
deleted, an exception with condition type `&i/o-filename` is raised. 


