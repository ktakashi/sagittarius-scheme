@; -*- coding:utf-8; -*-

@subsection[:tag "rfc.sftp"]{(rfc sftp) - SFTP library}

@define[Library]{@name{rfc sftp}}
@desc{This library provides SFTP programmatic operations.}

Following example code describes how to use in high level.
@codeblock{
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
    (sftp-read! conn "/a/file/path"
	(sftp-file-receiver "where/to/store" :options (file-options no-fail)))

    ;; upload a file
    (let ((handle (sftp-open conn "boo" 
                    (bitwise-ior +ssh-fxf-creat+ +ssh-fxf-write+))))
     (call-with-input-file "a/local/file" (cut sftp-write! conn handle <>)))

    ;; rename a file
    (sftp-rename! conn "boo" "foo")
    ;; remove a file
    (sftp-remove! conn "foo")
    ;; create a directory
    (sftp-mkdir! conn "boo")
    ;; remove a directory
    (sftp-rmdir! conn "boo")

    ;; create a symbolic link
    (print (sftp-symlink! conn "/tmp" "tmp"))
    ;; get a actual path of symbolic link
    (print (sftp-readlink conn "tmp"))
    ;; get a real path. (usually an absolute path)
    (print (sftp-realpath conn "../")))
  :username "username" :password "password")
}

@subsubsection{High level APIs}

@define[Function]{@name{call-with-sftp-connection}
 @args{server port proc . opts}}
@desc{@var{server} and @var{port} must be string, indicating hostname,
port number/service name respectively.
@var{proc} must accept one argument.

Creates a SFTP connection, executes given @var{proc} with the connection
and closes it after the execution.

The @var{opts} will be passed to @code{make-client-sftp-connection}.
}

@define[Function]{@name{sftp-open} @args{conn filename pflags}}
@desc{@var{conn} must be a SFTP connection. 
@var{filename} must be a string.
@var{pflags} must be an exact integer which value must be the result of
inclusive or of following values;

@define[Constant]{@name{+ssh-fxf-read+}}
@desc{Open the given @var{filename} read mode.}
@define[Constant]{@name{+ssh-fxf-write+}}
@desc{Open the given @var{filename} write mode.}
@define[Constant]{@name{+ssh-fxf-append+}}
@desc{Open the given @var{filename} append mode.}
@define[Constant]{@name{+ssh-fxf-creat+}}
@desc{Creates the given @var{filename} if it doesn't exist.}
@define[Constant]{@name{+ssh-fxf-trunc+}}
@desc{Truncate the given @var{filename} if it exists. @code{+ssh-fxf-creat+} 
      must be specified as well.}
@define[Constant]{@name{+ssh-fxf-excl+}}
@desc{Raises and error @var{filename} if it exists. @code{+ssh-fxf-creat+} 
      must be specified as well.}

If @var{filename} opened successfully, the procedure will return a handle.
}

@define[Function]{@name{sftp-close} @args{conn handle}}
@desc{@var{conn} must be a SFTP connection. 

Closes given @var{handle} created by @code{sftp-open}.
}

@define[Function]{@name{sftp-read} @args{conn handle/filename receiver}}
@desc{@var{conn} must be a SFTP connection.
@var{handle/filename} must be either a handle or string.
@var{receiver} must be a procedure accepts 2 arguments, offset and data,
respectively.

Reads the given @var{handle/filename} content from the server and
call @var{receiver} with the returned value. When it reaches the end of file,
then it will pass -1 as offset and eof value as data.
}

@define[Function]{@name{sftp-binary-receiver}}
@desc{Creates a in memory receiver for @code{sftp-read}.}
@define[Function]{@name{sftp-file-receiver} @args{filename :key options}}
@desc{Creates a file receiver for @code{sftp-read}.

The keyword argument @var{option} must be created by @code{file-options}.
By default no option.
}

@define[Function]{@name{sftp-write!} @args{conn handle input-port}}
@desc{@var{conn} must be a SFTP connection.
@var{handle} must be a handle.
@var{input-port} must be a binary input port.

Reads the content from given @var{input-port} and writes it to given
@var{handle}.
}

@define[Function]{@name{sftp-remove!} @args{conn filename}}
@desc{@var{conn} must be a SFTP connection.
@var{filename} must be a string indicating existing filename.

Removes the given @var{filename}. It is an error if the file doesn't exist
or user doesn't have proper permission to remove.

The result value is raw SFTP status object.
}

@define[Function]{@name{sftp-rename!} @args{conn oldpath newpath}}
@desc{@var{conn} must be a SFTP connection.
@var{oldpath} and @var{newpath}  must be strings indicating existing path.

Renames the given @var{oldpath} to @var{newpath}. It is an error if the 
file doesn't exist or user doesn't have proper permission to rename.

The result value is raw SFTP status object.
}

@; TODO add keyword arguments for attrs but I don't know if we are 
@; constructing properly so for now not documented.
@define[Function]{@name{sftp-mkdir!} @args{conn path}}
@desc{@var{conn} must be a SFTP connection.
@var{path} must be a string.

Creates the given @var{path} directory. It is an error if the 
directory exists or user doesn't have proper permission to create.

The result value is raw SFTP status object.
}

@define[Function]{@name{sftp-rmdir!} @args{conn path}}
@desc{@var{conn} must be a SFTP connection.
@var{path} must be a string.

Removes the given @var{path} directory. It is an error if the 
directory doesn't exists, user doesn't have proper permission to create or
the directory isn't empty.

The result value is raw SFTP status object.
}

@define[Function]{@name{sftp-opendir} @args{conn path}}
@desc{@var{conn} must be a SFTP connection.
@var{path} must be a string indicating existing path.

Opens the given @var{path} directory and returns its handle.
}

@define[Function]{@name{sftp-readdir} @args{conn handle/path}}
@desc{@var{conn} must be a SFTP connection.
@var{handle/path} must be either a handle opened by @code{sftp-opendir} or
a string indicating existing path.

Reads the given @var{handle/path} and returns the list of contents. The content
is an object of @code{<sftp-name>} which has @code{filename}, @code{longname}
and @code{attribute} slots.
}

@define[Function]{@name{sftp-readdir-as-filenames} @args{conn handle/path}}
@define[Function]{@name{sftp-readdir-as-longnames} @args{conn handle/path}}
@desc{@var{conn} must be a SFTP connection.
@var{handle/path} must be either a handle opened by @code{sftp-opendir} or
a string indicating existing path.

Calls @code{sftp-readdir} and strips out the result object to string by
calling @code{(slot-ref @var{c} 'filename)} or 
@code{(slot-ref @var{c} 'longname)} respectively.
}

@; status things needs be documented but for now this is enough


@subsubsection{Mid level APIs}

@define[Function]{@name{make-client-sftp-connection}
 @args{server port :key (username #f) (password #f)}}
@desc{Creates a SFTP connection object.}

@define[Function]{@name{sftp-close-connection} @args{connection}}
@desc{Closes the given SFTP connection object.}
