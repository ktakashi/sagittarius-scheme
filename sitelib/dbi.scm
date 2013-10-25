;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; dbi.scm - Common database interface
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/regex
(library (dbi)
    (export &dbi-error make-dbi-error dbi-error?
	    &dbi-driver-not-exist make-dbi-driver-not-exist
	    dbi-driver-not-exist? condition-driver-name
	    &dbi-unsupported make-dbi-unsupported dbi-unsupported?
	    &dbi-parameter-error make-dbi-parameter-error dbi-parameter-error?
	    raise-dbi-error
	    ;; dbi objects
	    ;; These are for DBD APIs
	    <dbi-driver>
	    <dbi-connection>
	    <dbi-query>  dbi-query-prepared dbi-query-connection
	    ;; User level APIs
	    dbi-connect
	    dbi-prepare
	    dbi-bind-parameter!
	    dbi-execute!
	    dbi-execute-query!
	    dbi-execute-using-connection!
	    dbi-execute-query-using-connection!
	    dbi-fetch!
	    dbi-fetch-all!
	    dbi-columns
	    dbi-open?
	    dbi-close
	    dbi-commit!
	    dbi-rollback!

	    <dbi-table> dbi-tables
	    <dbi-column> dbi-table-columns

	    ;; utility
	    dbi-do-fetch!
	    dbi-query-fold
	    dbi-query-map
	    dbi-query-for-each

	    ;; DBD level APIs
	    dbi-make-connection

	    ;; Low level APIs
	    dbi-parse-dsn
	    )
    (import (rnrs)
	    (rnrs eval)
	    (clos user)
	    (srfi :13 strings)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius control)
	    (sagittarius object))

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

  (define-class <dbi-driver> ()
    ((driver-name :init-keyword :driver-name)))

  (define-class <dbi-connection> () ())

  (define-class <dbi-query> ()
    ((connection :init-keyword :connection :reader dbi-query-connection)
     (prepared   :init-keyword :prepared :reader dbi-query-prepared)))

  ;; table information
  (define-class <dbi-table> ()
    ((schema :init-keyword :schema :init-value #f)
     (name   :init-keyword :name   :init-value #f)
     ;; table, view or synonym (DBD can extend but must be a symbol)
     (type   :init-keyword :type   :init-value #f)))

  (define-method write-object ((t <dbi-table>) port)
    (format port "#<dbi-table ~a ~a ~a>" (~ t 'schema) (~ t 'name) (~ t 'type)))

  ;; column information
  (define-class <dbi-column> ()
    ((name  :init-keyword :name)
     (table :init-keyword :table)
     (column-type :init-keyword :column-type :init-value #f)
     (nullable? :init-keyword :nullable? :init-value #t)))

  (define-method write-object ((t <dbi-column>) port)
    (format port "#<dbi-column ~a ~a ~a ~a>" (~ t 'table 'name) (~ t 'name) 
	    (~ t 'column-type) (~ t 'nullable?)))

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
      (apply dbi-make-connection
	     (dbi-make-driver driver-name) options option-alist args)))

  ;; generic methods
  ;; We do not provide any default method. So if no body implements DBD,
  ;; this will raise no method condition.
  (define-generic dbi-prepare)
  (define-generic dbi-execute!)
  ;; make some room for DBD extension...
  (define-generic dbi-execute-query!)
  (define-method dbi-execute-query! ((stmt <dbi-query>) . option)
    (apply dbi-execute! stmt option)
    stmt)

  (define-generic dbi-execute-using-connection!)
  (define-generic dbi-execute-query-using-connection!)

  (define (%execute-using-connection c sql . args)
    (let1 q (dbi-prepare c sql)
      ;; assume dbi-bind-parameter! can handle the integer
      (unless (null? args)
	(do ((i 1 (+ i 1)) (args args (cdr args)))
	    ((null? args))
	  (dbi-bind-parameter! q i (car args))))
      (values (dbi-execute! q) q)))
  ;; simple implementation
  (define-method dbi-execute-using-connection! ((c <dbi-connection>) sql . args)
    (receive (count stmt) (apply %execute-using-connection c sql args)
      (dbi-close stmt)
      count))

  ;; simple implementation
  (define-method dbi-execute-query-using-connection! 
    ((c <dbi-connection>) sql . args)
    (receive (count stmt) (apply %execute-using-connection c sql args)
      stmt))
  ;; fetch must return #f if no result available
  (define-generic dbi-fetch!)
  (define-generic dbi-fetch-all!)
  ;; dbi-fetch-all! can be naive implementation like this
  (define-method dbi-fetch-all! ((q <dbi-query>))
    (let loop ((v (dbi-fetch! q))
	       (r '()))
      (if v
	  (loop (dbi-fetch! q) (cons v r))
	  (reverse! r))))

  ;; utility macro
  (define-syntax dbi-do-fetch!
    (syntax-rules ()
      ((_ (t q) body ...)
       (dbi-do-fetch! (t q #t) body ...))
      ((_ (t query r) body ...)
       (let ((q query))
	 (do ((t (dbi-fetch! q) (dbi-fetch! q)))
	     ((not t) r)
	   body ...)))))

  ;; simply one, sub library can extend
  (define-method dbi-query-fold ((q <dbi-query>) proc knil)
    (let loop ((r (dbi-fetch! q)) (knil knil))
      (if r
	  (loop (dbi-fetch! q) (proc r knil))
	  knil)))

  (define-method dbi-query-map ((q <dbi-query>) proc)
    (reverse!
     (dbi-query-fold q (lambda (record seed) (cons (proc record) seed)) '())))
  (define-method dbi-query-for-each ((q <dbi-query>) proc)
    (dbi-query-fold q (lambda (record seed) (proc record)) '()))

  (define-generic dbi-columns)
  (define-generic dbi-bind-parameter!)
  ;;(define-generic dbi-do)
  (define-generic dbi-escape-sql)
  (define-generic dbi-commit!)
  (define-generic dbi-rollback!)

  ;; DBD level APIs
  (define-generic dbi-make-connection)
  (define-generic dbi-open?)
  (define-generic dbi-close)

  ;; default just return empty list
  (define-method dbi-tables ((conn <dbi-connection>) . args) '())
  (define-method dbi-table-columns ((t <dbi-table>) . args) '())

  ;;--------------------------
  ;; Low level APIs
  (define (dbi-parse-dsn dsn)
    (cond ((#/^dbi:([\w-]+)(?::(.*))?$/ dsn)
	   => (lambda (m)
		(let ((driver (m 1))
		      (options (m 2)))
		  (if (and options (not (string-null? options)))
		      (let1 alist (map (lambda (nv)
					 (receive (n v)
					     (string-scan nv "=" 'both)
					   (if n (cons n v) (cons nv #t))))
				       (string-split options #\;))
			(values driver options alist))
		      (values driver "" '())))))
	  (else
	   (raise-dbi-error (make-dbi-error)
			    'dbi-parse-dsn "bad data source name" dsn))))

  ;; load driver library and execute driver constructor.
  ;; NB: driver library must be on load path.

  ;; TODO make this thread safe
  (define *driver-pool* (make-eq-hashtable))

  (define (dbi-make-driver driver-name)
    (let* ((driver-name (string->symbol driver-name))
	   (lib `(dbd ,driver-name))
	   (ctr `(,(string->symbol (format "make-~a-driver" driver-name)))))
      (cond ((~ *driver-pool* driver-name))
	    (else
	     (guard (e (else
			(raise-dbi-error 
			 (make-dbi-driver-not-exist driver-name)
			 'dbi-make-driver
			 (if (message-condition? e)
			     (condition-message e)
			     (format "could not load driver ~a or it does not have procedure make-~a-driver"
				     lib driver-name))
			 e)))
	       (let1 driver (eval ctr (environment lib))
		 (set! (~ *driver-pool* driver-name) driver)
		 driver))))))
  )