@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.logging"]{(util logging) - Logging utilities}

@define[Library]{@name{(util logging)}}
@desc{This library provides logging utilities.

@code{(util logger)} provides logger and appender style logging utilility.
Loggers determine loging level and appenders determin how to write logs.
This example shows the concept.
@codeblock{
(import (rnrs) (util logging))

;; Appenders
(define console-appender (make-appender "[~w5] ~l ~m"))
(define file-appender
  (make-file-appender "[~w5] ~l ~m" "useful.log"))

;; Logger
(define logger (make-logger +debug-level+ console-appender file-appender))

;; This won't be logged since the logger level is debug.
(trace-log logger "trace log message")
(debug-log logger "debug log message")

;; If the logging requires heavy process, then it's better to
;; check the level
(when (logger-info? logger)
  (let ((message (construct-message)))
    (info-log logger message)))

;; stop logging
(terminate-logger! logger)
}
}

@subsubsection{Loggers}

Loggers contains collection of appenders and level of logging. Once a logger
is created, appenders and threshold can't be modified.

@define["Record Type"]{@name{<logger>}}
@define[Function]{@name{logger?} @args{obj}}
@define[Function]{@name{make-logger} @args{threshold appenders @dots{}}}
@desc{Basic logger.

Record type @code{<logger>} is a base type of loggers.

@code{logger?} returns #t if @var{obj} is a logger otherwise #f.

@code{make-logger} creates a logger whose threshold is @var{threshold} and
appenders are @var{appenders}. The @var{threshold} must be one of the
followings:

@define[Constant]{@name{+trace-level+}}
@define[Constant]{@name{+debug-level+}}
@define[Constant]{@name{+info-level+}}
@define[Constant]{@name{+warn-level+}}
@define[Constant]{@name{+error-level+}}
@define[Constant]{@name{+fatal-level+}}
@desc{Logging level constants.

The @code{+trace-level+} is the lowest level (@code{0}) and
@code{+fatal-level+} is the highest level (@code{5}). The values are integers
so users can extend the level for both side.
}

}

@define["Record Type"]{@name{<async-logger>}}
@define[Function]{@name{async-logger?} @args{obj}}
@define[Function]{@name{make-async-logger} @args{threshold appenders @dots{}}}
@desc{Asynchronous logger.

Record type @code{<async-logger>} is a type of asynchronous loggers. It inherits
the @code{<logger>}.

@code{async-logger?} returns #t if @var{obj} is an asynchronous logger
otherwise #f.

@code{make-async-logger} creates an asynchronous logger. The arguments are
passed to parent protocol.

Asynchronous logger logs message asynchronously. Means it creates a background
thread and lets it log. It is useful if a logger has a lot of appenders and
logging process may take a lot of time.

To stop background thread, @code{terminate-logger!} needs to be called. It
is users responsibility to do it.
}

@define[Function]{@name{trace-log} @args{logger message}}
@define[Function]{@name{logger-trace?} @args{logger}}
@define[Function]{@name{debug-log} @args{logger message}}
@define[Function]{@name{logger-debug?} @args{logger}}
@define[Function]{@name{info-log} @args{logger message}}
@define[Function]{@name{logger-info?} @args{logger}}
@define[Function]{@name{warn-log} @args{logger message}}
@define[Function]{@name{logger-warn?} @args{logger}}
@define[Function]{@name{error-log} @args{logger message}}
@define[Function]{@name{logger-error?} @args{logger}}
@define[Function]{@name{fatal-log} @args{logger message}}
@define[Function]{@name{logger-fatal?} @args{logger}}
@desc{Logging APIs.

@code{@var{level}-log} procedures log @var{message} on @var{logger} if
@var{logger} has its threshold lower than the @var{level}.

@code{logger-@var{level}?} procedures check if the @var{logger} has
threshold lower than @var{level}.
}

@define["Generic Function"]{@name{terminate-logger!}}
@define[Method]{@name{terminate-logger} @args{(logger <logger>)}}
@define[Method]{@name{terminate-logger} @args{(logger <async-logger>)}}
@desc{Terminates logging of the given @var{logger}.

The method calls @code{appender-finish} for all appenders of give @var{logger}.

If the @var{logger} is an asynchronous logger, then it also stops background
thread.
}

@subsubsection{Appenders}

Appenders are actual logging mechanisms. Each appender must be responsible
how to write a log message and resource management such as log file.

@define["Record Type"}{@name{<appender>}}
@define[Function}{@name{appender?} @args{obj}}
@define[Function}{@name{make-appender} @args{log-format}}
@desc{Base appender. This appender emits log messages into
@code{current-output-port}.

@code{appender?} returns #f if @var{obj} is an appender otherwise #f.

@code{make-appender} creates an appender. The @var{log-format} argument
must be a string and specifying the format of the log line.

The @var{log-format} can contains place holders stating with the character
@code{#\~}. The followings are the defined place holders:

@dl-list[]{
  @dl-item[@code{#\w@var{date-format}}]{
    Puts logging date on this location. The @var{date-format} specifies
    format of the log. It must be a character or string which is surrounded
    by @code{#\{ #\}}. The format is passed to the @code{date->string}
    procedure defined in SRFI-19.
  }
  @dl-item[@code{#\l}]{
    Puts logging level on this location.
  }
  @dl-item[@code{#\m}]{
    Puts log message on this location.
  }
}

The following example shows when @var{log-format} is @code{"[~w5] ~l ~m"}
and logging with info level.
@codeblock{
(define logger (make-logger +info-level+ (make-appender "[~w5] ~l ~m")))
(info-log logger "message of the log")
;; [2016-09-06T12:32:06] info message of the log
}

}

@define["Record Type"}{@name{<file-appender>}}
@define[Function}{@name{file-appender?} @args{obj}}
@define[Function}{@name{make-file-appender} @args{log-format filename}}
@desc{File appender. This is a subtype of @code{<appender>}. This appender
emits log messages to the file named @var{filename}.

@code{file-appender?} returns #f if @var{obj} is a file appender otherwise #f.

@code{make-file-appender} creates a file appender. The @var{log-format} is
passed to parent protocol. The file creation is done with file options of
@code{no-fail}, @code{no-truncate} and @code{append}. Thus if the file exists
then it would append the log line.

The given @var{filename} will be converted to absolute path so changing
directory will not affect the log file location.
}

@define["Generic Function"}{@name{file-appender-filename}}
@define[Method}{@name{file-appender-filename}
 @args{(file-appender <file-appender>)}}
@desc{Returns log file name.}

@define["Record Type"}{@name{<rolling-file-appender>}}
@define[Function}{@name{make-rolling-file-appender}
 @args{log-format filename :optional (rolling-size 10485760)}}
@define[Function}{@name{rolling-file-appender?} @args{obj}}
@desc{Rolling file appender. This is a subtype of @code{<file-appender>}.
This appender emits log message to the file named @var{filename} and if
the file size is more than @var{rolling-size}, then it renames the old file
to indexed file and new log file named @var{filename} is created.

@code{rolling-file-appender?} returns #f if @var{obj} is a rolling file
appender otherwise #f.
}

@define["Record Type"}{<daily-rolling-file-appender>}
@define[Function}{@name{make-daily-rolling-file-appender}
 @args{log-format filename :optional (date-pattern "~Y-~m-~d")}}
@define[Function}{@name{daily-rolling-file-appender?} @args{obj}}
@desc{Daily rolling file appender. This is a subtype of @code{<file-appender>}.
This appender emits log message to the file named @var{filename} and if
the date string of last file modified time formatted to @var{date-pattern}
is differ from log time, then the appender rolls the old log file to
a backup file. The backup file is concatenation of @var{filename} and
last modified date.

@code{daily-rolling-file-appender?} returns #f if @var{obj} is a daily rolling
file appender otherwise #f.
}

@sub*section{Creating appenders}

Users would soon face that predefined appenders are not enough or don't satisfy
the requirement. For that case, appenders can easily be created.

The following example shows how to create an Email appender.
@codeblock{
(import (rnrs)
        (rfc smtp)
        (rfc smtp authentications)
        (util logging)
        (clos user))

(define-record-type (<smtp-appender> make-smtp-appender smtp-appender?)
  (parent <appender>)
  (fields (immutable connection smtp-appender-connection)
          (immutable username smtp-appender-username)
          (immutable password smtp-appender-password))
  (protocol (lambda (p)
              (lambda (format host port username password)
                ((p format)
                 (make-smtp-connection host port) username password)))))

(define-method append-log ((appender <smtp-appender>) log)
  (let ((message (format-log appender log))
        (conn (smtp-appender-connection appender)))
    (guard (e (else (report-error e)))
      (smtp-connect! conn)
      (when (smtp-authentication-required? conn)
        (let* ((methods (smtp-connection-authentication-methods conn))
               (username (smtp-appender-username appender))
               (password (smtp-appender-password appender))
               (method (cond ((memq 'PLAIN methods)
                              (list (smtp-plain-authentication username password)))
                             ((memq 'PASSWORD methods)
                              (let-values ((init&next (smtp-login-authentication username password)))
                                init&next))
                             (else #f))))
          (if methods
              (apply smtp-authenticate! conn method)
              (begin
                (smtp-disconnect! conn)
                (error 'append-log "not supported")))))
      (smtp-send! conn (smtp:mail
                        (smtp:from "Takashi Kato" "ktakashi@ymail.com")
                        (smtp:to "Takashi Kato" "ktakashi@ymail.com")
                        (smtp:subject "Logging with email")
                        message))
      (smtp-disconnect! conn))))
}
The example is not really useful since it sends only the fixed recipient with
fixed format. If you need to use it, you need to modify.

Only what users need to do to create an appender is the followings:
@itemlist{
  @item{Creates a record type inherits @code{<appender>}}.
  @item{Specialising @code{append-log} method with the above record}.
}

@define["Generic Function"]{@name{append-log} @args{appender log}}
@desc{Core process of appending logs.

@var{log} is an object of @code{<log>} or its subtype. This method should
emit logs.
}

@define["Generic Function"]{@name{appender-finish} @args{appender}}
@desc{Finishing appender if necessary. This method is called when
@code{terminate-logger} is called.

Implementation should release resource of the @var{appender}.
}

@define["Generic Function"]{@name{format-log} @args{appender log}}
@desc{Formats log object. @var{log} is an object of @code{<log>} or its subtype.

The method must return a string representation of given @var{log}.

The default implementation handles the log format described in the
@code{make-logger} description.
}

@define[Function]{@name{appender-log-format} @args{appender}}
@desc{Returns log format of the given @var{appender}. }

@define["Record Type"]{@name{<log>}}
@define[Function]{@name{log?} @args{obj}}
@define[Function]{@name{make-log} @args{when level message}}
@define[Function]{@name{log-when} @args{log}}
@define[Function]{@name{log-level} @args{log}}
@define[Function]{@name{log-message} @args{log}}
@desc{Default log object.

@code{<log>} has 3 fields @code{when}, @code{level} and @code{message}.
By default creation, they are UTC time object, symbol of log level and
log message, respectively.
}

If you want to create own logger which handles log object differently,
then you need to specialise the following generic function with the
logger.

@define["Generic Function"]{@name{push-log} @args{logger log}}
@desc{Pushes @var{log} to appenders of the @var{logger}.}

@;@subsubsection{Singleton logger}
@;TBD