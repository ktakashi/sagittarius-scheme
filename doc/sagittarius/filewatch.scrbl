@; -*- coding:utf-8; -*-

@subsection[:tag "lib.sagittarius.filewatch"]{(sagittarius filewatch) - Monitoring filesystem}

@define[Library]{@name{(sagittarius filewatch)}} 
@desc{Monitoring filesystem cannot be done efficiently without support of
underlying operating system. This library provides unified interface of the
mechanism.
}

The following simple @code{tail (1)} like script shows how it works:
@codeblock{
(import (rnrs) (getopt) (sagittarius filewatch) (prefix (binary io) binary:))

(define (tail file offset)
  (define watcher (make-filesystem-watcher))
  (define in (open-file-input-port file))
  ;; dump contents to stdout
  (define (dump)
    (let loop ()
      (let ((line (binary:get-line in)))
        (unless (eof-object? line) 
          (put-bytevector (standard-output-port) line)
          (put-bytevector (standard-output-port) #vu8(10))
          (loop)))))
  (define size (file-size-in-bytes file))
  ;; move port position if the size if more than offset
  (when (> size offset) (set-port-position! in (- size offset)))
  ;; dump first
  (dump)
  ;; add path to file watcher
  (filesystem-watcher-add-path! watcher file '(modify) 
                                (lambda (path event) (dump)))
  ;; monitor on foreground.
  (filesystem-watcher-start-monitoring! watcher :background #f))

;; this tail is not line oriented
;; it shows tail of the file from the given offset.
(define (main args)
  (with-args (cdr args)
      ((offset (#\o "offset") #t "1024")
       . rest)
    (tail (car rest) (string->number offset))))
}

@define[Function]{@name{make-filesystem-watcher} @args{:key error-handler}}
@desc{Creates and returns filesystem watcher object.

The keyword argument @var{error-handler} is specified, which must be a
procedure accepts one argument, then it is called with a condition 
when monitoring handler raised an error.}

@define[Function]{@name{release-filesystem-watcher!} @args{watcher}}
@desc{Releasing the @var{watcher}.

Released filesystem watcher can not be reused.
}

@define[Function]{@name{filesystem-watcher?} @args{o}}
@desc{Returns #t if the given @var{o} is a filesystem watcher object, otherwise
#f.}

@define[Function]{@name{filesystem-watcher-add-path!}
 @args{watcher path flags monitoring-handler}}
@desc{Adds monitoring targets to the @var{watcher}.

The @var{path} must be a string and indicating existing path.

The @var{flags} must be one of the following symbols or list of the symbols:
@dl-list{
  @dl-item[@code{access}]{Checks if the @var{path} is accessed.}
  @dl-item[@code{modify}]{Checks if the @var{path} is modified.}
  @dl-item[@code{delete}]{Checks if the @var{path} is deleted.}
  @dl-item[@code{move}]{Checks if the @var{path} is moved.}
  @dl-item[@code{attribute}]{Checks if the @var{path}'s attribute is changed.}
}
NOTE: The flags might not be supported depending on the platform. See 
@secref["filewatch.limit"]{implementation limitation} section for more details.

The @var{monitoring-handler} must be a procedure accepts 2 arguments. The
procedure is called if the @var{path} gets an event specified @var{flags}.
When the @var{monitoring-handler} is invoked, then the path and a symbol 
of the invoking event are passed respectively. The possible event symbols 
are the followings:
@dl-list{
  @dl-item[@code{accessed}]{Checks if the @var{path} is accessed.}
  @dl-item[@code{modified}]{Checks if the @var{path} is modified.}
  @dl-item[@code{deleted}]{Checks if the @var{path} is deleted.}
  @dl-item[@code{moved}]{Checks if the @var{path} is moved.}
  @dl-item[@code{attribute}]{Checks if the @var{path}'s attribute is changed.}
}

The procedure @var{filesystem-watcher-add-path!} returns the @var{watcher}.

If the @var{watcher} started monitoring, then the procedure raises
@code{&assertion}.
}

@define[Function]{@name{filesystem-watcher-remove-path!}
 @args{watcher path}}
@desc{Removes given @var{path} from the @var{watcher}. And returns 
@var{watcher},

If the @var{watcher} started monitoring, then the procedure raises
@code{&assertion}.
}

@define[Function]{@name{filesystem-watcher-start-monitoring!}
 @args{watcher :key (background #t)}}
@desc{Starts monitoring filesystem on given @var{watcher}.

If the keyword argument @var{background} is true value, then the procedure
creates a thread and let the thread monitor the filesystem. (So the procedure
returns after the thread invocation.) Otherwise, the procedure blocks and
wait until other thread calls @code{filesystem-watcher-stop-monitoring!}.
}

@define[Function]{@name{filesystem-watcher-stop-monitoring!} @args{watcher}}
@desc{Stops monitoring of given @var{watcher}.

If the @var{watcher} is started on background, then the monitoring thread
may not stop immediately.
}

@subsubsection[:tag "filewatch.limit"]{Implementation limitation}

Even the library provides unified APIs however users still should know the
limitations per operating system to avoid unexpected behaviours. The following
sections describes the known limitations.

@sub*section{Linux}

On Linux, the library is constructed on top of @code{inotify (7)} and
@code{poll (2)}. If users add too many paths, then it may reach the
maximum number of watch descriptor.

The @code{IN_MOVED_FROM} and @code{IN_MOVED_TO} flags are passed as
@code{moved}. So it is users responsibility to detect which file is
@var{moved from} and which file is @var{moved to}.

@sub*section[:tag "bsd.limitation"]{BSD Unix}

On BSD Unix, the library is constructed on top of @code{kqueue (2)}. This
implementation contains 3 major issues. Possibility of number of file
descriptor explosion, not @code{access} flag support, and no support of
directory monitoring.

The @code{kqueue} requires file descriptor per monitoring path. Thus if
the number of paths is large, then it reaches the maxinum number of file
descriptors. (NB: @code{kern.maxfiles} on FreeBSD).

@code{kqueue} doesn't support path access monitoring (e.g. @code{IN_ACCESS} 
on @code{inotify}). So it is impossible to monitor file access.

Current implementation of @code{(sagittarius filewatch)} using @code{kqueue}
doesn't allow users to monitor directory. This is because by default,
@code{kqueue} doesn't provide facility to detect which file is added.
To do it, we need manual management. To keep our code as simple as possible,
we decided not to do it for now. This decision may be changed if there's
enough demands.

@sub*section{OS X}

On OS X, the library is constructed on top of @code{kqueue}, thus the
same limitation as @secref["bsd.limitation"]{BSD Unix} is applied.

@; On OS X, the library is constructed on top of @code{FSEvents}. This
@; implementation does not support @code{acces} due to the limitation of OS X
@; which doesn't have facility to detect file access.

@; The implementations uses @code{kFSEventStreamCreateFlagFileEvents} flag 
@; which is supported after OS X v10.7 (Lion). If your system is older than
@; this version, then build script would fallback to @code{kqueue (2)}
@; implementation. Thus the limitation of @secref["bsd.limitation"]{BSD Unix}
@; is applied.

@sub*section{Windows}

Staring Windows Vista, Microsoft decided not to change timestamp just accessing
the file or directory by default. So @code{access} flag may or may not work on
Windows depending on the configuration of the platform.

Due to the lack of deletion detect, @code{delete} and @code{move} work the
same. Thus the monitoring handler may get both @code{deleted} and @code{moved}
even though it's only specified @code{delete} or @code{move}.
