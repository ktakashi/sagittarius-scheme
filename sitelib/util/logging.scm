;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; util/logging.scm - Logging utilities
;;;
;;;   Copyright (c) 2010-2019  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

#!nounbound
(library (util logging)
    (export ;; Loggers
	    make-logger        logger?       
	    (rename (logger <logger>))
	    make-async-logger  async-logger? 
	    (rename (async-logger <async-logger>))

	    logger-threshold
	    logger-threshold-set!

	    ;; Logger APIs
	    +trace-level+ trace-log logger-trace?
	    +debug-level+ debug-log logger-debug?
	    +info-level+  info-log  logger-info?
	    +warn-level+  warn-log  logger-warn?
	    +error-level+ error-log logger-error?
	    +fatal-level+ fatal-log logger-fatal?
	    +logging-off+
	    terminate-logger!

	    ;; Appenders
	    make-appender appender? (rename (appender <appender>))
	    make-file-appender file-appender?
	    (rename (file-appender <file-appender>))
	    file-appender-filename
	    make-rolling-file-appender rolling-file-appender?
	    (rename (rolling-file-appender <rolling-file-appender>))
	    make-daily-rolling-file-appender daily-rolling-file-appender?
	    (rename (daily-rolling-file-appender <daily-rolling-file-appender>))

	    ;; For extension
	    push-log
	    append-log
	    appender-finish

	    format-log (rename (appender-format appender-log-format))
	    (rename (log <log>))
	    make-log log? ;; for push-log
	    log-when log-level log-message log-arguments log-thread-name

	    ;; logger storage
	    define-logger-storage loggers
	    )
    (import (except (rnrs) log)
	    (sagittarius)
	    (sagittarius control)
	    (util concurrent)
	    (clos user)
	    (srfi :1)
	    (srfi :18)
	    (srfi :19))

;; Log object.
(define-record-type log
  (fields when
	  level
	  message
	  arguments
	  thread-name)
  (protocol (lambda (p)
	      (lambda (wh level msg . rest)
		(p wh level msg (list->vector rest)
		   (thread-name (current-thread)))))))

;; Log formatter. It handles log object
(define (builtin-format-log log log-format)
  (define when (time-utc->date (log-when log)))
  (define level (log-level log))
  (define message (log-message log))
  (define arguments (log-arguments log))
  (define thread-name (log-thread-name log))
  (define in (open-string-input-port log-format))
  (define (read-date-format in)
    (get-char in)
    (let-values (((out extract) (open-string-output-port)))
      (let loop ()
	(let ((c (get-char in)))
	  (cond ((eof-object? c) (extract))
		((char=? c #\})  (extract))
		(else (put-char out c) (loop)))))))
  (define (get-argument in)
    (define (read-number in)
      (let loop ((r '()))
	(let ((c (get-char in)))
	  (cond ((eof-object? c) (values #f (list->string (reverse! r))))
		((char-numeric? c) (loop (cons c r)))
		((char=? c #\])
		 (values #t (string->number (list->string (reverse! r)))))
		(else (values #f (list->string (reverse! (cons c r)))))))))
    (get-char in)
    (let-values (((num? v) (read-number in)))
      (if num?
	  (if (< v (vector-length arguments))
	      (vector-ref arguments v)
	      #f)
	  (string-append "a[" v))))
  (define (put out msg)
    (cond ((string? msg) (put-string out msg))
	  ((condition? msg) #f) ;; skip
	  (else (put-datum out msg))))
  (define (strip-arguments fmt args)
    (define (count-replacement fmt)
      (define len (string-length fmt))
      (let loop ((i 0) (c 0))
	(cond ((= i len) c)
	      ((eqv? (string-ref fmt i) #\~)
	       (cond ((= (+ i 1) len) c)
		     ((eqv? (string-ref fmt (+ i 1)) #\%) (loop (+ i 2) c))
		     ;; Okay, let's hope this is a valid marker
		     (else (loop (+ i 2) (+ c 1)))))
	      (else (loop (+ i 1) c)))))
    (let ((replecement (count-replacement fmt)))
      (take args replecement)))
      
  (let-values (((out extract) (open-string-output-port)))
    (do ((c (get-char in) (get-char in)))
	((eof-object? c) (extract))
      (case c
	((#\~)
	 (case (get-char in)
	   ((#\w)
	    (let ((c2 (lookahead-char in)))
	      (case c2
		((#\{)
		 (put-string out (date->string when (read-date-format in))))
		(else
		 (put-string out (date->string when (string #\~ c2)))))))
	   ((#\l) (put-string out (symbol->string level)))
	   ((#\t) (put-string out thread-name))
	   ((#\m)
	    (if (zero? (vector-length arguments))
		(put out message)
		(guard (e (else (put out message)))
		  (let ((args (vector->list arguments)))
		    (put out (apply format message
				    (strip-arguments message args)))))))
	   ((#\a)
	    (let ((c2 (lookahead-char in)))
	      (case c2
		((#\[) (cond ((get-argument in) => (lambda (v) (put out v)))))
		(else
		 (let loop ((i 0) (written? #f))
		   (unless (= i (vector-length arguments))
		     (and written? (put-char out #\space))
		     (loop (+ i 1) (put out (vector-ref arguments i)))))))))
	   ((#\e)
	    (let ((c2 (lookahead-char in)))
	      (case c2
		((#\0)
		 (get-char in)
		 (vector-for-each
		  (lambda (a)
		    (and (condition? a)
			 (put-char out #\newline)
			 (put-string out (describe-condition a)))) arguments))
		(else
		 (vector-for-each
		  (lambda (a)
		    (and (condition? a) (report-error a))) arguments)))))
	   (else => (lambda (c2)
		      (put-char out #\~)
		      (put-char out c2)))))
	 (else (put-char out c))))))

;; Appender APIs
(define-generic append-log)
(define-generic appender-finish)
(define-generic format-log)

;; abstract appender
;; all appenders must inherit <appender>
(define-record-type appender
  (fields (immutable log-format appender-format)))

(define-method format-log ((a appender) log)
  (builtin-format-log log (appender-format a)))

;; but you can use it for trial
;; default just print
(define-method append-log ((a appender) log)
  ;; use format to lock the port
  (format #t "~a~%" (format-log a log)))
(define-method appender-finish ((a appender)) #t) ;; do nothing


;; file appender
(define-generic file-appender-filename)
(define-record-type file-appender
  (fields path)
  (parent appender)
  (protocol (lambda (p)
	      (lambda (format filename)
		;; get absolute path of given file so that
		;; changing current directory won't affect
		;; the file
		((p format) (absolute-path filename))))))
(define-method file-appender-filename ((appender file-appender))
  (file-appender-path appender))

(define-method append-log ((appender file-appender) log)
  (call-with-port (open-file-output-port
		   (file-appender-filename appender)
		   (file-options no-fail no-truncate append)
		   (buffer-mode block)
		   (native-transcoder))
    (lambda (out)
      (format out "~a~%" (format-log appender log)))))

(define-record-type rolling-file-appender
  (fields rolling-size
	  (mutable current-backup-index))
  (parent file-appender)
  (protocol (lambda (p)
	      (lambda (format filename :optional (rolling-size 10485760))
		((p format filename) rolling-size 0)))))
(define-method file-appender-filename ((a rolling-file-appender))
  (let ((file (call-next-method))
	(rolling-size (rolling-file-appender-rolling-size a)))
    (when (and (file-exists? file) (>= (file-size-in-bytes file) rolling-size))
      (let* ((index (rolling-file-appender-current-backup-index a))
	     (backup (format "~a.~a" file index)))
	(rolling-file-appender-current-backup-index-set! a (+ index 1))
	(rename-file file backup)))
    file))

(define-record-type daily-rolling-file-appender
  (fields date-pattern)
  (parent file-appender)
  (protocol (lambda (p)
	      (lambda (format filename :optional (date-pattern "~Y-~m-~d"))
		((p format filename) date-pattern)))))
(define-method file-appender-filename ((a daily-rolling-file-appender))
  (define (check-timestamp a file)
    (define pattern (daily-rolling-file-appender-date-pattern a))
    (define (mtime->date file)
      (time-utc->date (make-time time-utc (file-stat-mtime file) 0)))
    (let ((now (date->string (current-date) pattern))
	  (this (date->string (mtime->date file) pattern)))
      (and (not (string=? now this)) this)))
  (let ((file (call-next-method)))
    (cond ((and (file-exists? file) (check-timestamp a file)) =>
	   (lambda (backup-date)
	     (rename-file file (format "~a.~a" file backup-date)))))
    file))

;; loggers
(define-generic push-log)
(define-generic terminate-logger!)
(define-record-type logger
  (fields (mutable threshold)
	  appenders)
  (protocol (lambda (p)
	      (lambda (threshold . appenders)
		(unless (for-all appender? appenders)
		  (assertion-violation 'make-logger "appender required"
				       appenders))
		(p threshold appenders)))))
(define-method push-log ((l logger) log)
  (for-each (lambda (appender) (append-log appender log))
	    (logger-appenders l)))
(define-method terminate-logger! ((l logger))
  (for-each appender-finish (logger-appenders l)))

(define (make-logger-deamon logger sq)
  (define (deamon-task)
    (define appenders (logger-appenders logger))
    (define (do-append log)
      (for-each (lambda (appender) (append-log appender log)) appenders))
    (define (do-finish) (for-each appender-finish appenders))
    (let loop ()
      (let ((log (shared-queue-get! sq)))
	(cond ((log? log) (do-append log) (loop))
	      (else (do-finish))))))
  (thread-start! (make-thread deamon-task)))

(define-record-type async-logger
  (fields (immutable buffer logger-buffer)
	  (mutable   deamon logger-deamon logger-deamon-set!))
  (parent logger)
  (protocol (lambda (p)
	      (lambda args
		(let* ((sq (make-shared-queue))
		       (l ((apply p args) sq #f)))
		  (logger-deamon-set! l (make-logger-deamon l sq))
		  l)))))
(define-method push-log ((l async-logger) log)
  (shared-queue-put! (logger-buffer l) log))
;; maybe logger should not raise an error, but for my convenience
(define-method terminate-logger! ((l async-logger))
  (shared-queue-put! (logger-buffer l) #f)
  (thread-join! (logger-deamon l)))

(define-constant +trace-level+ 0)
(define-constant +debug-level+ 1)
(define-constant +info-level+  2)
(define-constant +warn-level+  3)
(define-constant +error-level+ 4)
(define-constant +fatal-level+ 5)
(define-constant +logging-off+ +inf.0)

(define-syntax define-logging-api
  (lambda (x)
    (define ->s datum->syntax)
    (define (->level-constant k level)
      (->s k (string->symbol (format "+~a-level+" (syntax->datum level)))))
    (define (make-names k level)
      (let ((n (syntax->datum level)))
	(->s k (list (string->symbol (format "logger-~a?" n))
		     (string->symbol (format "~a-log" n))
		     (string->symbol (format "%~a-log" n))))))
    (syntax-case x ()
      ((k level)
       (with-syntax ((c (->level-constant #'k #'level))
		     ((check logging proc-name) (make-names #'k #'level)))

	 #'(begin
	     (define (check logger) (>= c (logger-threshold logger)))
	     (define (proc-name logger msg . args)
	       (when (check logger)
		 (push-log logger
			   (apply make-log (current-time) 'level msg args))))
	     (define-syntax logging
	       (lambda (xx)
		 (syntax-case xx ()
		   ((_ logger msg rest (... ...))
		    #'(when (check logger)
			(push-log logger
				  (make-log (current-time) 'level msg
					    rest (... ...)))))
		   (me (identifier? #'me) #'proc-name))))))))))
;; per level APIs
(define-logging-api trace)
(define-logging-api debug)
(define-logging-api info)
(define-logging-api warn)
(define-logging-api error)
(define-logging-api fatal)

;; In some cases, the same loggers mustn't be created (e.g. asyn logger).
;; The following might be better to be handled by user code but it seems
;; sort of common use case so why not.
(define-syntax loggers (syntax-rules ()))
(define-syntax define-logger-storage
  (lambda (x)
    (define (parse-clause clause*)
      (syntax-case clause* (loggers)
	(((loggers (logger-name make-logger) ...))
	 #'((logger-name make-logger) ...))))
    (define (parse-name k name)
      (syntax-case name ()
	((lookup register) (list #'lookup #'register))
	;; TODO should we make implicit register name instead hiding?
	(_ (list name #'register))))
    (syntax-case x ()
      ((k name clause* ...)
       (with-syntax ((((logger-name make-logger) ...)
		      (parse-clause #'(clause* ...)))
		     ((lookup register) (parse-name #'k #'name)))
	 #'(define-values (lookup register)
	     (let ((storage (make-eq-hashtable))
		   (lock (make-mutex)))
	       (define (lookup n) (hashtable-ref storage n))
	       (define (register n logger)
		 (mutex-lock! lock)
		 (unless (hashtable-ref storage n)
		   (hashtable-set! storage n logger))
		 (mutex-unlock! lock))
	       (hashtable-set! storage 'logger-name (make-logger)) ...
	       (values lookup register))))))))
)
