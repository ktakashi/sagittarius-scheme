@; -*- coding: utf-8 -*-
@subsection[:tag "archive"]{(archive) - Generic archive interface}

@define[Library]{@name{archive}}
@desc{This library provides generic interface to access archive libraries.
Sagittarius supports @code{tar} and @code{zip}.
}

Following code describes a typical use of the library;

@codeblock{
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

}

Following sections use @var{type} as a supported archive type. More precisely,
if it's a supported archive type then there must be a library named
@code{(archive @var{type})}.

@subsubsection{Archive input}

@define[Function]{@name{make-input-archive} @args{type input-port}}
@desc{@var{type} must be a symbol and supported archive type.
@var{input-port} must be a binary input port.

Creates an archive input which represents the specified type of archive.
}

@define[Method]{@name{next-entry!} @args{archive-input}}
@desc{Retrieves next entry of the given archive input. If there is no entry,
then it returns #f.
}

@define[Macro]{@name{do-entry} @args{(entry archive-input) body @dots{}}
@define[Macro]{@name{do-entry} @args{(entry archive-input result) body @dots{}}
@desc{Convenient macro. Iterates the given @var{archive-input}'s entries.

The macro is expanded like this;

@codeblock{
(do ((@var{entry} (next-entry! @var{archive-input}) (next-entry! @var{archive-input})))
    ((not @var{entry}) @var{result})
  @var{body} @dots{})
}

If the first form is used, then @var{result} is #t.
}

@define[Method]{@name{extract-entry} @args{entry output-port}}
@desc{Extract the given archive entry @var{entry} to binary output port
@var{output-port}.
}

@define[Function]{@name{extract-all-entries}
 @args{archive-input :key (destinator archive-entry-name) (overwrite #f)}}
@desc{Convenient function. Extracts all entries in the given
@var{archive-input} to the file specified by @var{destinator}.

The keyword argument @var{destinator} must be a procedure which accepts
one argument, archive entry, and return a string represents the 
file/directory path.

The keyword argument @var{overwrite} is #t, then it overwrites the file.
If it is #f and there is a file, then it raises an error.
}

@define[Method]{@name{finish!} @args{archive-input}}
@desc{Finalize the given archive input.}

@define[Function]{@name{call-with-input-archive} @args{archive-input proc}}
@desc{@var{archive-input} must be an archive input.
@var{proc} must be a procedure which accepts one argument.

Call the @var{proc} with archive input and returns the result of the
@var{proc}.

The @var{archive-input} is finalized by @code{finish!}.
}

@define[Function]{@name{call-with-input-archive-port}
 @args{type input-port proc}}
@desc{Creates an archive input with @var{type} and @var{input-port}, then
call @code{call-with-input-archive}.
}

@define[Function]{@name{call-with-input-archive-file}
 @args{type file proc}}
@desc{Open file binary input port with given @var{file} and call
@code{call-with-input-archive-port}.
}

@subsubsection{Archive output}

@define[Function]{@name{make-output-archive} @args{type output-port}}
@desc{@var{type} must be a symbol.
@var{output-port} must be a output port.

Creates an archive output which represents the specified type of archive.
}

@define[Method]{@name{create-entry} @args{archive-output file}}
@desc{Creates an archive entry from the given @var{file}.}

@define[Method]{@name{append-entry} @args{archive-output entry}}
@desc{Appends the given @var{entry} to @var{archive-output}.}

@define[Method]{@name{finish!} @args{archive-output}}
@desc{Finalize the given archive output.}

@define[Function]{@name{call-with-output-archive} @args{archive-output proc}}
@desc{@var{archive-output} must be an archive output.
@var{proc} must be a procedure which accepts one argument.

Call the @var{proc} with archive input and returns the result of the 
@var{proc}.

The @var{archive-output} is finalized by @code{finish!}.
}

@define[Function]{@name{call-with-output-archive-port}
 @args{type output-port proc}}
@desc{Creates an archive output with @var{type} and @var{output-port}, then
call @code{call-with-output-archive}.
}

@define[Function]{@name{call-with-output-archive-file}
 @args{type file proc}}
@desc{Open file binary output port with given @var{file} and call
@code{call-with-output-archive-port}.
}

@subsubsection{Entry accessor}

@define[Function]{@name{archive-entry-name} @args{entry}}
@desc{Returns the name of @var{entry}.}

@define[Function]{@name{archive-entry-type} @args{entry}}
@desc{Returns the type of @var{entry}. It is either @code{file} or 
@code{directory}.
}

@subsubsection{Implementing archive implementation library}

To support other archive such as RAR, then you need to create a implementation
library.

@define[Library]{@name{(archive interface}}
@desc{The library defines all abstract class and method for the generic
archive access.
}

To support @var{foo} archive, then the library name must be 
code{(archive @var{foo})} and it must import @code{(archive interface)}.
So the library code should look like this;

@codeblock{
(library (archive foo)
  (export) ;; no export procedure is needed
  (import (rnrs)
          (close user)
          (archive interface)
          ;; so on
          @dots{})
  ;; class and method definitions
  @dots{}
)
}


For archiving, the implementation needs to implement following methods and
extends following classes;
@codeblock{make-archive-input, next-entry, extract-entry}
@codeblock{<archive-input> <archive-entry>}

For extracting, the implementation needs to implement following methods and
extends following classes;
@codeblock{make-archive-output, create-entry, append-entry!, finish!}
@codeblock{<archive-output> <archive-entry>}

NOTE: @code{<archive-entry>} may be shared between archiving and extracting.

@define[Class]{@name{<archive-input>}}
@desc{Abstract class of the archive input. This class has the following
slot;

@dl-list{
  @dl-item["source"]{
   Source of the archive. For compatibility of other archive, this should be
   a binary input port.
  }
}
}

@define[Class]{@name{<archive-output>}}
@desc{Abstract class of the archive output. This class has the following
slot;

@dl-list{
  @dl-item["sink"]{
   Destination of the archive. For compatibility of other archive, this
   should be a binary output port.
  }
}
}

@define[Class]{@name{<archive-entry>}}
@desc{Abstract class of the archive entry. This class has the following
slots;

@dl-list{
  @dl-item["name"]{ Entry name. }
  @dl-item["type"]{
   Entry type. For compatibility of other archive, this must be @code{file} or
   @code{directory}.
  }
}
}

@define[Method]{@name{make-archive-input} @args{type (source <port>)}}
@define[Method]{@name{make-archive-output} @args{type (sink <port>)}}
@desc{Creates an archive input or output. @var{type} specifies the
archive type. It is recommended to use @code{eql} specializer to specify.
}

@define[Method]{@name{finish!} @args{(in <archive-input>)}}
@desc{The @code{finish!} method for archive input has a default
implementation and it does nothing.

Users can specialize the method for own archive input.
}

The other methods must implemented as it's described in above section.