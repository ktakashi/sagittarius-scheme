@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.file"]{(util file) - File operation utility library}

@define[Library]{@name{(util file)}}
@desc{This library provides file operation utilities which is not exported from
@code{(sagittarius)} library.
}

@subsubsection{File to Scheme object operations}

@define[Function]{@name{file->list}
 @args{reader path :key (transcoder (native-transcoder))}}
@desc{@var{reader} must be a procedure accept one argument which is a port.
@var{path} must be a string indicating existing file.

Returns a list which elements are read by @var{reader} from the given @var{path}
file. @var{reader} reads the file contents until it reads EOF object.

The keyword argument @var{transcoder} can specify which transcoder will be used
to open a port. It must be either #f or transcoder. The default value is the
value @code{(native-transcoder)} returns.
}

@define[Function]{@name{file->sexp-list} @args{path}}
@define[Function]{@name{file->string-list} @args{path}}
@desc{Thin wrapper of @code{file->list}. The procedures defined as following;

@snipet{(file->list @code{read/ss} @var{path})}
@snipet{(file->list @code{get-line} @var{path})}

respectively.
}

@define[Function]{@name{file->string} @args{path}}
@desc{Reads all file contents indicated @var{path} as a string and return it.}

@subsubsection{Temporary directory operations}

@define[Function]{@name{temporary-directory} @args{:optional path}}
@desc{Returns operating system specific temporary directory path.

If optional argument @var{path} is given, the procedure replace the temporary 
directory path.}

@define[Function]{@name{make-temporary-file} @args{:optional prefix}}
@desc{Creates temporary file and returns 2 values. One is the port, the other
one is created file path.

The created temporary file won't be deleted automatically so it is users
responsibility to delete.
}

@subsubsection{Path operations}

@define[Function]{@name{decompose-path} @args{path}}
@desc{@var{path} must be a string.

Decompose the given @var{path} to directory path, base name, and extension then
returns these decomposed elements as 3 values.

The returning value can be #f if the path does not contains corresponding
element.
}

@define[Function]{@name{path-extension} @args{path}}
@desc{Returns the extension of given @var{path}. If it does not contains
extension then the procedure returns #f.}

@define[Function]{@name{path-sans-extension} @args{path}}
@desc{Removes extension from given @var{path} and return it.}

@define[Function]{@name{find-files}
 @args{target :key (pattern #f) (all #t) (sort string<=?) (recursive #t)}}
@desc{@var{target} must be a string indicating existing file system path.

Find files from @var{target} if it's a directory and return the found files as
a list.

Keyword arguments:
@dl-list{
@dl-item["pattern"]{Specifies the file pattern to be returned. This can be
either string or regular expression pattern object.}
@dl-item["all"]{When this keyword argument is #t, then returns all files
including hidden files which file name starting with @code{.}. If this is #f,
the procedure excludes hidden files.}
@dl-item["physical"]{If this is #t, then the @var{proc} is only given either
@code{directory} or @code{file}. No symbolic likes are given.}
@dl-item["sort"]{This specifies how to sort the result list. If the value is #f,
then the result order is unspecified.}
@dl-item["recursive"]{If this keyword argument is #t then the procedure finds
files recursively, otherwise only the first @var{target} directory.}
}
}

@define[Function]{@name{path-for-each}
 @args{path proc :key (physical #t) (file-only #f) (absolute-path #t)
 (stop-on-file #f) (all #t) (recursive #t)}}
@desc{@var{path} must be a string indicating existing file path. @var{proc}
must be a procedure accepts 2 arguments, a path string and a symbol. The given
symbol can be @code{directory}, @code{symbolic-link} or @code{file}.

Apply given @var{proc} to the found paths starting from @var{path} and returns
unspecified value. The second argument of @var{proc} indicates the file type
with following meaning;

@dl-list{
@dl-item[@code{directory}]{The path is a directory.}
@dl-item[@code{symbolic-link}]{The path is a symbolic link.}
@dl-item[@code{file}]{The path is a file}
}

The keyword arguments:
@dl-list{
@dl-item["file-only"]{If this is #t, then the @var{proc} is only given a file.
Otherwise all file types.}p
@dl-item["absolute-path"]{If this is #t, then the @var{proc} is given absolute
path. Otherwise only the filename.}
@dl-item["stop-on-false"]{If this is #t, then when @var{proc} returns #f the
process will stop. Otherwise it continues the process.}
}
The rest of the keyword arguments are the same as @code{find-files}.
}

@define[Function]{@name{path-map}
 @args{path proc :key (physical #t) (file-only #f) (absolute-path #t) (all #t)
  (recursive #t)}}
@desc{@var{path} must be a string indicating existing file path. @var{proc}
must be a procedure accepts 2 arguments, a path string and a symbol. The given
symbol can be @code{directory}, @code{symbolic-link} or @code{file}.

Process the path string starting from @var{path} with given @var{proc} and
returns a list which elements are the result value of @var{proc}.

The keyword arguments are the same as @code{path-for-each}.
}

@subsubsection{Create and delete utility}

@define[Function]{@name{create-directory*} @args{path}}
@define[Function]{@name{delete-directory*} @args{path}}
@desc{Convenient procedures to create or delete directories.

These are the same as UNIX command @code{mkdir -p} and @code{rm -rf},
respectively.
}