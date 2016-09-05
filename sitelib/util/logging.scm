;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; util/logging.scm - Logging utilities
;;;  
;;;   Copyright (c) 2010-2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (util logging)
    (export ;; Loggers
	    make-logger        logger?
	    make-async-logger  async-logger?
	    ;; Logger APIs
	    +trace-level+ trace-log logger-trace?
	    +debug-level+ debug-log logger-debug?
	    +info-level+  info-log  logger-info?
	    v	  +warn-level+  warn-log  logger-warn?
	    +error-level+ error-log logger-error?
	    +fatal-level+ fatal-log logger-fatal?

	    ;; Appenders
	    <appender> make-appender appender?
	    <file-appender> make-file-appender file-appender? 
	    file-appender-filename
	    <rolling-file-appender> make-rolling-file-appender 
	    rolling-file-appender?
	    <daily-rolling-file-appender> make-daily-rolling-file-appender
	    daily-rolling-file-appender?

	    ;; For extension
	    push-log
	    terminate-logger!
	    append-log
	    appender-finish
	    
	    format-log
	    <log> make-log log? ;; for push-log

	    ;; logger storage
	    define-logger-storage loggers
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (util concurrent)
	    (clos user)
	    (srfi :18)
	    (srfi :19))

;; Log object.
(define-record-type (<log> make-log log?)
  (fields (immutable when log-when)
	  (immutable level log-level)
	  (immutable message log-message)))

;; Log formatter. It handles log object
(define (builtin-format-log log format)
  (define when (time-utc->date (log-when log)))
  (define level (log-level log))
  (define message (log-message log))
  (define in (open-string-input-port format))
  (define (read-date-format in)
    (let-values (((out extract) (open-string-output-port)))
      (let loop ()
	(let ((c (get-char in)))
	  (cond ((eof-object? c) (extract))
		((char=? c #\})  (extract))
		(else (put-char out c) (loop)))))))

  (let-values (((out extract) (open-string-output-port)))
    (do ((c (get-char in) (get-char in)))
	((eof-object? c) (extract))
      (case c
	((#\~)
	 (case (get-char in)
	   ((#\w)
	    (let ((c2 (get-char in)))
	      (case c2
		((#\{) 
		 (put-string out (date->string when (read-date-format in))))
		(else
		 (put-string out (date->string when (string #\~ c2)))))))
	   ((#\l)
	    (put-string out (symbol->string level)))
	   ((#\m)
	    (put-string out message))
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
(define-record-type (<appender> make-appender appender?)
  (fields (immutable log-format appender-format)))

(define-method format-log ((a <appender>) log)
  (builtin-format-log log (appender-format a)))

;; but you can use it for traial
;; default just print
(define-method append-log ((appender <appender>) log)
  (display (format-log appender log)) (newline))
(define-method appender-finish ((appender <appender>)) #t) ;; do nothing


;; file appender
(define-generic file-appender-filename)
(define-record-type (<file-appender> make-file-appender file-appender?)
  (fields (immutable path file-appender-path))
  (parent <appender>)
  (protocol (lambda (p)
	      (lambda (format filename)
		;; get absolute path of given file so that
		;; changing current directory won't affect
		;; the file
		((p format) (absolute-path filename))))))
(define-method file-appender-filename ((appender <file-appender>))
  (file-appender-path appender))

(define-method append-log ((appender <file-appender>) log)
  (call-with-port (open-file-output-port 
		   (file-appender-filename appender)
		   (file-options no-fail no-truncate append)
		   (buffer-mode block)
		   (native-transcoder))
    (lambda (out)
      (display (format-log appender log) out)
      (newline out))))

(define-record-type (<rolling-file-appender> make-rolling-file-appender
					     rolling-file-appender?)
  (fields (immutable rolling-size rolling-file-appender-rolling-size)
	  (mutable current-backup-index
		   rolling-file-appender-current-backup-index
		   rolling-file-appender-current-backup-index-set!))
  (parent <file-appender>)
  (protocol (lambda (p)
	      (lambda (format filename :optional (rolling-size 10485760))
		((p format filename) rolling-size 0)))))
(define-method file-appender-filename ((a <rolling-file-appender>))
  (let ((file (call-next-method))
	(rolling-size (rolling-file-appender-rolling-size a)))
    (when (and (file-exists? file) (>= (file-size-in-bytes file) rolling-size))
      (let* ((index (rolling-file-appender-current-backup-index a))
	     (backup (format "~a.~a" file index)))
	(rolling-file-appender-current-backup-index-set! a (+ index 1))
	(rename-file file backup)))
    file))

(define-record-type (<daily-rolling-file-appender> 
		     make-daily-rolling-file-appender
		     daily-rolling-file-appender?)
  (fields (immutable date-pattern daily-rolling-file-appender-date-pattern))
  (parent <file-appender>)
  (protocol (lambda (p)
	      (lambda (format filename :optional (date-pattern "~Y-~m-~d"))
		((p format filename) date-pattern)))))
(define-method file-appender-filename ((a <daily-rolling-file-appender>))
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
(define-record-type (<logger> make-logger logger?)
  (fields (immutable threashold logger-threashold)
	  (immutable appenders  logger-appenders))
  (protocol (lambda (p)
	      (lambda (threashold . appenders)
		(unless (for-all appender? appenders)
		  (assertion-violation 'make-logger "appender required"
				       appenders))
		(p threashold appenders)))))
(define-method push-log ((l <logger>) log)
  (for-each (lambda (appender) (append-log appender log))
	    (logger-appenders l)))
(define-method terminate-logger! ((l <logger>))
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

(define-record-type (<async-logger> make-async-logger async-logger?)
  (fields (immutable buffer logger-buffer)
	  (mutable   deamon logger-deamon logger-deamon-set!))
  (parent <logger>)
  (protocol (lambda (p)
	      (lambda args
		(let* ((sq (make-shared-queue))
		       (l ((apply p args) sq #f)))
		  (logger-deamon-set! l (make-logger-deamon l sq))
		  l)))))
(define-method push-log ((l <async-logger>) log)
  (shared-queue-put! (logger-buffer l) log))
;; maybe logger should not raise an error, but for my convenience
(define-method terminate-logger! ((l <async-logger>))
  (shared-queue-put! (logger-buffer l) #f)
  (thread-join! (logger-deamon l)))

(define-constant +trace-level+ 0)
(define-constant +debug-level+ 1)
(define-constant +info-level+  2)
(define-constant +warn-level+  3)
(define-constant +error-level+ 4)
(define-constant +fatal-level+ 5)

(define-syntax define-logging-api
  (lambda (x)
    (define ->s datum->syntax)
    (define (->level-constant k level)
      (->s k (string->symbol (format "+~a-level+" (syntax->datum level)))))
    (define (make-names k level)
      (let ((n (syntax->datum level)))
	(->s k (list (string->symbol (format "logger-~a" n))
		     (string->symbol (format "~a-log" n))))))
    (syntax-case x ()
      ((k level)
       (with-syntax ((c (->level-constant #'k #'level))
		     ((check logging) (make-names #'k #'level)))
		      
	 #'(begin
	     (define (check logger) (>= c (logger-threashold logger)))
	     (define (logging logger msg)
	       (when (check logger)
		 (push-log logger (make-log (current-time) 'level msg))))))))))
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
