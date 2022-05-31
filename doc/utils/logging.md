[§2] (util logging) - Logging utilities {#util.logging}
-------------

###### [!Library] `(util logging)` 

This library provides logging utilities.

`(util logger)` provides logger and appender style logging utilility.
Loggers determine loging level and appenders determin how to write logs.
This example shows the concept.

``````````scheme
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
``````````



### [§3] Loggers

Loggers contains collection of appenders and level of logging. Once a logger
is created, appenders and threshold can't be modified.

###### [!Record Type] `<logger>` 
###### [!Function] `logger?`  _obj_
###### [!Function] `make-logger`  _threshold_ _appenders_ _..._

Basic logger.

Record type `<logger>` is a base type of loggers.

`logger?` returns #t if _obj_ is a logger otherwise #f.

`make-logger` creates a logger whose threshold is _threshold_ and
appenders are _appenders_. The _threshold_ must be one of the
followings:

###### [!Constant] `+trace-level+` 
###### [!Constant] `+debug-level+` 
###### [!Constant] `+info-level+` 
###### [!Constant] `+warn-level+` 
###### [!Constant] `+error-level+` 
###### [!Constant] `+fatal-level+` 

Logging level constants.

The `+trace-level+` is the lowest level (`0`) and
`+fatal-level+` is the highest level (`5`). The values are integers
so users can extend the level for both side.




###### [!Record Type] `<async-logger>` 
###### [!Function] `async-logger?`  _obj_
###### [!Function] `make-async-logger`  _threshold_ _appenders_ _..._

Asynchronous logger.

Record type `<async-logger>` is a type of asynchronous loggers. It inherits
the `<logger>`.

`async-logger?` returns #t if _obj_ is an asynchronous logger
otherwise #f.

`make-async-logger` creates an asynchronous logger. The arguments are
passed to parent protocol.

Asynchronous logger logs message asynchronously. Means it creates a background
thread and lets it log. It is useful if a logger has a lot of appenders and
logging process may take a lot of time.

To stop background thread, `terminate-logger!` needs to be called. It
is users responsibility to do it.


###### [!Function] `trace-log`  _logger_ _message_ _._ _arguments_
###### [!Function] `logger-trace?`  _logger_
###### [!Function] `debug-log`  _logger_ _message_ _._ _arguments_
###### [!Function] `logger-debug?`  _logger_
###### [!Function] `info-log`  _logger_ _message_ _._ _arguments_
###### [!Function] `logger-info?`  _logger_
###### [!Function] `warn-log`  _logger_ _message_ _._ _arguments_
###### [!Function] `logger-warn?`  _logger_
###### [!Function] `error-log`  _logger_ _message_ _._ _arguments_
###### [!Function] `logger-error?`  _logger_
###### [!Function] `fatal-log`  _logger_ _message_ _._ _arguments_
###### [!Function] `logger-fatal?`  _logger_

Logging APIs.

`_level_-log` procedures log _message_ on _logger_ if
_logger_ has its threshold lower than the _level_.

`logger-_level_?` procedures check if the _logger_ has
threshold lower than _level_.


###### [!Generic Function] `terminate-logger!` 
###### [!Method] `terminate-logger`  _(logger_ _<logger>)_
###### [!Method] `terminate-logger`  _(logger_ _<async-logger>)_

Terminates logging of the given _logger_.

The method calls `appender-finish` for all appenders of give _logger_.

If the _logger_ is an asynchronous logger, then it also stops background
thread.


### [§3] Appenders

Appenders are actual logging mechanisms. Each appender must be responsible
how to write a log message and resource management such as log file.

###### [!Record Type] `<appender>` 
###### [!Function] `appender?`  _obj_
###### [!Function] `make-appender`  _log-format_

Base appender. This appender emits log messages into
`current-output-port`.

`appender?` returns #f if _obj_ is an appender otherwise #f.

`make-appender` creates an appender. The _log-format_ argument
must be a string and specifying the format of the log line.

The _log-format_ can contains place holders stating with the character
`#\~`. The followings are the defined place holders:

`#\w_date-format_`
: Puts logging date on this location. The _date-format_ specifies
  format of the log. It must be a character or string which is surrounded
  by `#\{ #\}`. The format is passed to the `date->string`    procedure defined in SRFI-19.

`#\l`
: Puts logging level on this location.

`#\m`
: Puts log message on this location.

`#\a[n]`
: Puts _n_th log argument on this location.

`#\a`
: Puts all log arguments on this location.

The following example shows when _log-format_ is `"[~w5] ~l ~m"`and logging with info level.

``````````scheme
(define logger (make-logger +info-level+ (make-appender "[~w5] ~l ~m")))
(info-log logger "message of the log")
;; [2016-09-06T12:32:06] info message of the log
``````````



###### [!Record Type] `<file-appender>` 
###### [!Function] `file-appender?`  _obj_
###### [!Function] `make-file-appender`  _log-format_ _filename_

File appender. This is a subtype of `<appender>`. This appender
emits log messages to the file named _filename_.

`file-appender?` returns #f if _obj_ is a file appender otherwise #f.

`make-file-appender` creates a file appender. The _log-format_ is
passed to parent protocol. The file creation is done with file options of
`no-fail`, `no-truncate` and `append`. Thus if the file exists
then it would append the log line.

The given _filename_ will be converted to absolute path so changing
directory will not affect the log file location.


###### [!Generic Function] `file-appender-filename` 
###### [!Method] `file-appender-filename`  _(file-appender_ _<file-appender>)_

Returns log file name.

###### [!Record Type] `<rolling-file-appender>` 
###### [!Function] `make-rolling-file-appender`  _log-format_ _filename_ _:optional_ _(rolling-size_ _10485760)_
###### [!Function] `rolling-file-appender?`  _obj_

Rolling file appender. This is a subtype of `<file-appender>`.
This appender emits log message to the file named _filename_ and if
the file size is more than _rolling-size_, then it renames the old file
to indexed file and new log file named _filename_ is created.

`rolling-file-appender?` returns #f if _obj_ is a rolling file
appender otherwise #f.


###### [!Record Type] `<daily-rolling-file-appender>` 
###### [!Function] `make-daily-rolling-file-appender`  _log-format_ _filename_ _:optional_ _(date-pattern_ _"~Y-~m-~d")_
###### [!Function] `daily-rolling-file-appender?`  _obj_

Daily rolling file appender. This is a subtype of `<file-appender>`.
This appender emits log message to the file named _filename_ and if
the date string of last file modified time formatted to _date-pattern_is differ from log time, then the appender rolls the old log file to
a backup file. The backup file is concatenation of _filename_ and
last modified date.

`daily-rolling-file-appender?` returns #f if _obj_ is a daily rolling
file appender otherwise #f.


#### [§4] Creating appenders

Users would soon face that predefined appenders are not enough or don't satisfy
the requirement. For that case, appenders can easily be created.

The following example shows how to create an Email appender.

``````````scheme
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
``````````

The example is not really useful since it sends only the fixed recipient with
fixed format. If you need to use it, you need to modify.

Only what users need to do to create an appender is the followings:

- Creates a record type inherits `<appender>`.
- Specialising `append-log` method with the above record.

###### [!Generic Function] `append-log`  _appender_ _log_

Core process of appending logs.

_log_ is an object of `<log>` or its subtype. This method should
emit logs.


###### [!Generic Function] `appender-finish`  _appender_

Finishing appender if necessary. This method is called when
`terminate-logger` is called.

Implementation should release resource of the _appender_.


###### [!Generic Function] `format-log`  _appender_ _log_

Formats log object. _log_ is an object of `<log>` or its subtype.

The method must return a string representation of given _log_.

The default implementation handles the log format described in the
`make-logger` description.


###### [!Function] `appender-log-format`  _appender_

Returns log format of the given _appender_. 

###### [!Record Type] `<log>` 
###### [!Function] `log?`  _obj_
###### [!Function] `make-log`  _when_ _level_ _message_ _._ _arguments_
###### [!Function] `log-when`  _log_
###### [!Function] `log-level`  _log_
###### [!Function] `log-message`  _log_
###### [!Function] `log-arguments`  _log_

Default log object.

`<log>` has 4 fields `when`, `level`, `message` and
`arguments`. By the default creation, they are UTC time object, symbol
of log level, log message and a vector of extra logging arguments, respectively.


If you want to create own logger which handles log object differently,
then you need to specialise the following generic function with the
logger.

###### [!Generic Function] `push-log`  _logger_ _log_

Pushes _log_ to appenders of the _logger_.

### [§3] Singleton logger

Asynchronous logger or other loggers should sometimes be singleton. This type
of thing might need to be done in users responsibility however I think it's
common idiom. So the very simple one is added to the library.

###### [!Macro] `define-logger-storage`  _lookup_ _clause*_ _..._
###### [!Macro] `define-logger-storage`  _(lookup_ _register)_ _clause*_ _..._

This macro defines a logger lookup procedure. If the second form is used
then, a registration procedure.

The generated procedures have the following signatures

(@var{lookup} @var{name})
: Looks up the logger named _name_

(@var{register} @var{name} @var{logger})
: Registers the given logger with name, _name_

The registration procedure stores the given logger to hidden storage. If 
the procedure called the same name argument twice, then the latter logger
overwrites the previous one.

`clause*` must the following form:

- `(loggers (_logger-name_ _make-logger_) ...)`

`loggers` is an auxiliary syntax. If the `loggers`clause is specified, then the macro stores the logger _logger-name_ which
is created by _make-logger_ thunk as its predefined loggers.


