;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; dbi.scm - Common database interface
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

(library (dbi)
    (export &dbi-error make-dbi-error dbi-error?
	    &dbi-driver-not-exist make-dbi-driver-not-exist
	    dbi-driver-not-exist? condition-driver-name
	    &dbi-unsupported make-dbi-unsupported dbi-unsupported?
	    &dbi-parameter-error make-dbi-parameter-error dbi-parameter-error?
	    raise-dbi-error
	    ;; dbi objects
	    ;; These are for DBD APIs
	    dbi-driver dbi-driver? make-dbi-driver
	    dbi-connection dbi-connection? make-dbi-connection
	    dbi-query dbi-query? make-dbi-query dbi-query-prepared
	    ;; User level APIs
	    dbi-connect
	    dbi-prepare
	    dbi-bind-parameter
	    dbi-execute!
	    dbi-fetch!
	    dbi-fetch-all!
	    dbi-columns
	    dbi-open?
	    dbi-close

	    ;; Low level APIs
	    dbi-parse-dsn
	    )
    (import (rnrs)
	    (rnrs eval)
	    (srfi :13 strings)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius control))

  ;;--------------------------
  ;; DBI conditions

  ;; Root of DBI related condition
  (define-condition-type &dbi-error &error
    make-dbi-error dbi-error?)
  ;; Failed to load the specified driver
  (define-condition-type &dbi-driver-not-exist &dbi-error
    make-dbi-driver-not-exist dbi-driver-not-exist?
    (driver-name condition-driver-name))
  ;; Feature not supported
  (define-condition-type &dbi-unsupported &dbi-error
    make-dbi-unsupported dbi-unsupported?)
  ;; Parameter mismatch between a prepared query and its execution
  (define-condition-type &dbi-parameter-error &dbi-error
    make-dbi-parameter-error dbi-parameter-error?)

  (define (raise-dbi-error type who msg . irritants)
    (raise (apply condition
		  (filter values
			  (list type
				(and who (make-who-condition who))
				(make-message-condition msg)
				(make-irritants-condition irritants))))))

  ;;--------------------------
  ;; DBI object definitions

  ;; DBI driver.
  ;; Since Sagittarius does not support generic method, it must have some driver
  ;; specific method in it. And DBD library must have make-<driver-name>-driver
  ;; procedure.
  (define-record-type dbi-driver
    (fields driver-name
	    ;; driver interface
	    make-connection)
    (protocol
     (lambda (p)
       (lambda (name . args)
	 (let-keywords args ((make-connection 
			      (lambda args (raise-dbi-error (make-dbi-unsupported)
							    'dbi-make-connection
							    "method is not supported")))
			     
			     )
	   (p name make-connection))))))
  ;; connection class
  (define-record-type dbi-connection
    (fields open?
	    close
	    prepare
	    escape-sql
	    commit
	    rollback)
    (protocol
     (lambda (p)
       (lambda args
	 (let-keywords args ((open? (lambda (obj) #t))
			     (close (lambda (obj) (undefined)))
			     (prepare (lambda (c sql . args)
					(raise-dbi-error (make-dbi-unsupported)
							 'dbi-prepare
							 "method is not supported")))
			     
			     (escape-sql (lambda (c str)
					   ;; default implementation will come later
					   (raise-dbi-error (make-dbi-unsupported)
							    'dbi-escape-sql
							    "method is not supported")))
			     (commit (lambda (c)
					   ;; default implementation will come later
					   (raise-dbi-error (make-dbi-unsupported)
							    'dbi-commit
							    "method is not supported")))
			     (rollback (lambda (c)
					   ;; default implementation will come later
					 (raise-dbi-error (make-dbi-unsupported)
							  'dbi-rollback
							  "method is not supported"))))
	   (p open? close prepare escape-sql commit rollback))))))
 
  ;; prepared query
  (define-record-type dbi-query
    (fields connection
	    prepared
	    execute
	    bind-parameter
	    fetch
	    fetch-all
	    commit
	    rollback
	    columns
	    )
    (protocol
     (lambda (p)
       (lambda (connection prepared . args)
	 (let-keywords args ((bind-parameter (lambda (q index value)
					       (raise-dbi-error (make-dbi-unsupported)
								'dbi-bind-parameter
								"method is not supported")))
			     (execute (lambda (obj . params)
					(raise-dbi-error (make-dbi-unsupported)
							 'dbi-execute
							 "method is not supported")))
			     (fetch (lambda (q)
				      (raise-dbi-error (make-dbi-unsupported)
							 'dbi-execute
							 "method is not supported")))
			     (fetch-all (lambda (q)
					  (raise-dbi-error (make-dbi-unsupported)
							   'dbi-execute
							   "method is not supported")))
			     (commit (lambda (c)
					   ;; default implementation will come later
					   (raise-dbi-error (make-dbi-unsupported)
							    'dbi-commit
							    "method is not supported")))
			     (rollback (lambda (c)
					   ;; default implementation will come later
					 (raise-dbi-error (make-dbi-unsupported)
							  'dbi-rollback
							  "method is not supported")))
			     (columns (lambda (q)
					   ;; default implementation will come later
					 (raise-dbi-error (make-dbi-unsupported)
							  'dbi-columns
							  "method is not supported"))))
	   (p connection prepared execute bind-parameter fetch fetch-all
	      commit rollback columns))))))
  
  ;;--------------------------
  ;; User level APIs

  ;; Establish a connection to the data source specified by DSN,
  ;; and returns a connection object.
  ;; DSN is the data source name, which can have the following syntax.
  ;;   "dbi:driver-type"
  ;;   "dbi:driver-type:connection-options"
  ;; Connection-options is like "name1=value1;name2=value2;...".
  (define (dbi-connect dsn . args)
    (let-values (((driver-name options option-alist) (dbi-parse-dsn dsn)))
      (let ((driver (dbi-make-driver driver-name)))
	(apply (dbi-driver-make-connection driver)
	       driver options option-alist args))))

  ;; connection object interface
  (define (dbi-open? con)
    (or (dbi-connection? con)
	(assertion-violation 'dbi-open?
			     (format "dbi-connection required, but got ~s" con)
			     con))
    ((dbi-connection-open? con) con))

  (define (dbi-close con)
    (or (dbi-connection? con)
	(assertion-violation 'dbi-close
			     (format "dbi-connection required, but got ~s" con)
			     con))
    ((dbi-connection-close con) con))

  ;; make prepared statement.
  (define (dbi-prepare con sql . args)
    (or (dbi-connection? con)
	(assertion-violation 'dbi-prepare
			     (format "dbi-connection required, but got ~s" con)
			     con sql args))
    (apply (dbi-connection-prepare con) con sql args))

  (define (dbi-bind-parameter query index value . args)
    (or (dbi-query? query)
	(assertion-violation 'dbi-execute
			     (format "dbi-query required, but got ~s" query)
			     query sql args))
    (apply (dbi-query-bind-parameter query)
	   query index value args))
  
  ;; execute query
  (define (dbi-execute! query . params)
    (or (dbi-query? query)
	(assertion-violation 'dbi-execute
			     (format "dbi-query required, but got ~s" query)
			     query sql args))
    (apply (dbi-query-execute query) query params))

  (define (dbi-fetch! query)
    (or (dbi-query? query)
	(assertion-violation 'dbi-fetch!
			     (format "dbi-query required, but got ~s" query)
			     query))
    ((dbi-query-fetch query) query))

  (define (dbi-fetch-all! query)
    (or (dbi-query? query)
	(assertion-violation 'dbi-fetch-all!
			     (format "dbi-query required, but got ~s" query)
			     query))
    ((dbi-query-fetch-all query) query))

  (define (dbi-columns query)
    (or (dbi-query? query)
	(assertion-violation 'dbi-columns
			     (format "dbi-query required, but got ~s" query)
			     query))
    ((dbi-query-columns query) query))

  ;;--------------------------
  ;; Low level APIs
  (define *dsn-regex* (regex "^dbi:([\\w-]+)(?::(.*))?$"))
  (define (dbi-parse-dsn dsn)
    (cond ((looking-at *dsn-regex* dsn)
	   => (lambda (m)
		(let ((driver (m 1))
		      (options (m 2)))
		  (if (and options (not (string-null? options)))
		      ;; TODO maybe I need to make string-split like Gauche.
		      (let1 alist (let loop ((options options)
					     (r '()))
				    (let ((index (string-scan options #\; 'index)))
				      (cond (index
					     (let*-values (((nv rest) (string-scan options #\; 'both))
							   ((n v) (string-scan nv "=" 'both)))
					       (if n 
						   (loop rest (acons n v r))
						   (loop rest (acons nv #t r)))))
					     ;; option is either null-string or a=b form
					     ((string-null? options) r)
					     (else
					      (let-values (((n v) (string-scan options "=" 'both)))
						(if n
						    (acons n v r)
						    (acons options #t r)))))))
			(values driver options alist))
		      (values driver "" '())))))
	  (else
	   (raise-dbi-error (make-dbi-error)
			    'dbi-parse-dsn "bad data source name" dsn))))

  ;; load driver library and execute driver constructor.
  ;; NB: driver library must be on load path.
  (define (dbi-make-driver driver-name)
    (let ((lib `(dbd ,(string->symbol driver-name)))
	  (ctr `(,(string->symbol (format "make-~a-driver" driver-name)))))
      (guard (e (else
		 (raise-dbi-error (make-dbi-driver-not-exist driver-name)
				  'dbi-make-driver
				  (format "could not load driver ~a or it does not have procedure make-~a-driver"
					  lib driver-name)
				  e)))
	(eval ctr (environment lib)))))
  )