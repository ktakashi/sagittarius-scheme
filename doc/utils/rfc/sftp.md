[§2] (rfc sftp) - SFTP library {#rfc.sftp}
-------------

###### [!Library] `rfc sftp` 

This library provides SFTP programmatic operations.

Following example code describes how to use in high level.

```scheme
(import (rfc sftp) (pp) (srfi :26))

(call-with-sftp-connection 
  "localhost"    ;; hostname
  "23"           ;; port number
  (lambda (conn)
    ;; read directory
    (pp (sftp-readdir conn "."))
    ;; only short names
    (pp (sftp-readdir-as-filenames conn "."))
    ;; only long names (usually POSIX 'ls -l' format)
    (pp (sftp-readdir-as-longnames conn "."))

    ;; retrieve a file as a bytevector
    (print (utf8->string (sftp-read! conn "reading/file" 
                                    (sftp-binary-receiver))))
    ;; store a file to local file directly
    (sftp-read conn "/a/file/path"
	(sftp-file-receiver "where/to/store" :options (file-options no-fail)))

    ;; upload a file
    (let ((handle (sftp-open conn "boo" 
                    (bitwise-ior +ssh-fxf-creat+ +ssh-fxf-write+))))
     (call-with-input-file "a/local/file" (cut sftp-write! conn handle <>)
                           :transcoder #f))

    ;; rename a file
    (sftp-rename! conn "boo" "foo")
    ;; remove a file
    (sftp-remove! conn "foo")
    ;; create a directory
    (sftp-mkdir! conn "boo")
    ;; remove a directory
    (sftp-rmdir! conn "boo")

    ;; create a symbolic link
    (pp (sftp-symlink! conn "/tmp" "tmp"))
    ;; get a actual path of symbolic link
    (pp (sftp-readlink conn "tmp"))
    ;; get a real path. (usually an absolute path)
    (pp (sftp-realpath conn "../")))
  :username "username" :password "password")
```

### [§3] High level APIs

###### [!Function] `call-with-sftp-connection`  _server_ _port_ _proc_ _._ _opts_

_server_ and _port_ must be string, indicating hostname,
port number/service name respectively.
_proc_ must accept one argument.

Creates a SFTP connection, executes given _proc_ with the connection
and closes it after the execution.

The _opts_ will be passed to `make-client-sftp-connection`.


###### [!Function] `sftp-open`  _conn_ _filename_ _pflags_

_conn_ must be a SFTP connection. 
_filename_ must be a string.
_pflags_ must be an exact integer which value must be the result of
inclusive or of following values;

###### [!Constant] `+ssh-fxf-read+` 

Open the given _filename_ read mode.

###### [!Constant] `+ssh-fxf-write+` 

Open the given _filename_ write mode.

###### [!Constant] `+ssh-fxf-append+` 

Open the given _filename_ append mode.

###### [!Constant] `+ssh-fxf-creat+` 

Creates the given _filename_ if it doesn't exist.

###### [!Constant] `+ssh-fxf-trunc+` 

Truncate the given _filename_ if it exists. `+ssh-fxf-creat+` 
      must be specified as well.

###### [!Constant] `+ssh-fxf-excl+` 

Raises and error _filename_ if it exists. `+ssh-fxf-creat+` 
      must be specified as well.

If _filename_ opened successfully, the procedure will return a handle.


###### [!Function] `sftp-close`  _conn_ _handle_

_conn_ must be a SFTP connection. 

Closes given _handle_ created by `sftp-open`.


###### [!Function] `sftp-read`  _conn_ _handle/filename_ _receiver_ _:key_ _(offset_ _0)_ _buffer-size_

_conn_ must be a SFTP connection.
_handle/filename_ must be either a handle or string.
_receiver_ must be a procedure accepts 2 arguments, _offset_ which
is an integer and _data_ which is a binary input port, respectively.

Reads the given _handle/filename_ content from the server and
call _receiver_ with the returned value. When it reaches the end of file,
then it will pass -1 as offset and eof value as data.

The keyword argument _offset_ specifies starting where to read. It
is useful to read only diff.

The keyword argument _buffer-size_ specifies how many bytes it tries to
read in one read call. The default value is 1048576 (1MB). This value is only
an indication so that server can decide actual data length to send.

The return value of this procedure is the result value of _receiver_.

Receiver must return 2 values, one if processed octet count and the other
one is result value. Following is the _sftp-binary-receiver_ definition;

``````````scheme
(define (sftp-binary-receiver)
  (let-values (((out extract) (open-bytevector-output-port)))
    (lambda (offset data)
      (if (< offset 0)
          (values -1 (extract))
          (values (copy-binary-port out data) #f)))))
``````````



###### [!Function] `sftp-binary-receiver` 

Creates a in memory receiver for `sftp-read`.

###### [!Function] `sftp-file-receiver`  _filename_ _:key_ _options_

Creates a file receiver for `sftp-read`.

The keyword argument _option_ must be created by `file-options`.
By default no option.


###### [!Function] `sftp-oport-receiver`  _output-port_

_output-port_ must be a binary output port.

Creates a output port receiver for `sftp-read`.


###### [!Function] `sftp-write!`  _conn_ _handle_ _input-port_ _:key_ _(offset_ _0)_ _buffer-size_

_conn_ must be a SFTP connection.
_handle_ must be a handle.
_input-port_ must be a binary input port.

Reads the content from given _input-port_ and writes it to given
_handle_.

The keyword argument _offset_ specifies where to write. This is useful
to write a file separately or simply append.

The keyword argument _buffer-size_ specifies how many bytes of data
the procedure sends in one packet. The default value is 131072 (128KB).
The RFC defines the minimum value 32768 (3KB) so it is safe to use the
value. However too small buffer size makes writing extremely slow so
we use a bit larger value to make performance better. If you meet a
problem with writing a file, consider to change this size.


###### [!Function] `sftp-exists?`  _conn_ _filename_

_conn_ must be a SFTP connection.
_filename_ must be a string indicating existing filename.

Checks if the given _filename_ exists.


###### [!Function] `sftp-remove!`  _conn_ _filename_

_conn_ must be a SFTP connection.
_filename_ must be a string indicating existing filename.

Removes the given _filename_. It is an error if the file doesn't exist
or user doesn't have proper permission to remove.

The result value is raw SFTP status object.


###### [!Function] `sftp-rename!`  _conn_ _oldpath_ _newpath_

_conn_ must be a SFTP connection.
_oldpath_ and _newpath_  must be strings indicating existing path.

Renames the given _oldpath_ to _newpath_. It is an error if the 
file doesn't exist or user doesn't have proper permission to rename.

The result value is raw SFTP status object.


###### [!Function] `sftp-mkdir!`  _conn_ _path_

_conn_ must be a SFTP connection.
_path_ must be a string.

Creates the given _path_ directory. It is an error if the 
directory exists or user doesn't have proper permission to create.

The result value is raw SFTP status object.


###### [!Function] `sftp-rmdir!`  _conn_ _path_

_conn_ must be a SFTP connection.
_path_ must be a string.

Removes the given _path_ directory. It is an error if the 
directory doesn't exists, user doesn't have proper permission to create or
the directory isn't empty.

The result value is raw SFTP status object.


###### [!Function] `sftp-opendir`  _conn_ _path_

_conn_ must be a SFTP connection.
_path_ must be a string indicating existing path.

Opens the given _path_ directory and returns its handle.


###### [!Function] `sftp-readdir`  _conn_ _handle/path_

_conn_ must be a SFTP connection.
_handle/path_ must be either a handle opened by `sftp-opendir` or
a string indicating existing path.

Reads the given _handle/path_ and returns the list of contents. The content
is an object of `<sftp-name>` which has `filename`, `longname`and `attribute` slots.


###### [!Function] `sftp-readdir-as-filenames`  _conn_ _handle/path_
###### [!Function] `sftp-readdir-as-longnames`  _conn_ _handle/path_

_conn_ must be a SFTP connection.
_handle/path_ must be either a handle opened by `sftp-opendir` or
a string indicating existing path.

Calls `sftp-readdir` and strips out the result object to string by
calling `(slot-ref _c_ 'filename)` or 
`(slot-ref _c_ 'longname)` respectively.


### [§3] Mid level APIs

Mid level APIs are changed since 0.7.8. The changes are not backward
compatible, so if you are using 0.7.7 API and move to 0.7.8, please be aware.

###### [!Function] `make-client-sftp-connection`  _server_ _port_

Creates a SFTP connection object.

###### [!Function] `open-client-sftp-connection!`  _sftp-connection_ _:key_ _(authenticate_ _#f_

Opens the SFTP connection.

If the keyword argument `authenticate` is specified, then it must be a
procedure which accepts one argument.

###### [!Function] `sftp-password-authentication` _username_ _password_

Provides a procedure which is suitable for username and password
authentication by using the given _username_ and _password_.

NOTE: a lot of SSH server may not support this by default.

###### [!Function] `sftp-public-key-authentication` _username_ _private-key_ _public-key_

Provides a procedure which is suitable for public key authentication.
The _public-key_ must be listed on the server's authorized key.

###### [!Function] `sftp-keyboard-interactive-authentication` _username_ 

Provides a procedure which is suitable for keyboard interactive authentication.

