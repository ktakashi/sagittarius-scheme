;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; odbc.scm - DBD for ODBC
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

(library (dbd odbc)
    (export make-odbc-driver)
    (import (rnrs)
	    (odbc)
	    (dbi)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (sagittarius control))

  (define-class <dbi-odbc-driver> (<dbi-driver>)
    ((env :init-keyword :env :accessor odbc-driver-env)))

  (define-class <dbi-odbc-connection> (<dbi-connection>)
    ((hbc :init-keyword :hbc :accessor odbc-connection-odbc-hbc)))

  ;; to make method more specific
  (define-class <dbi-odbc-query> (<dbi-query>)
    ()) ;; no slot

  (define-class <dbi-odbc-table> (<dbi-table>)
    ((catalog :init-keyword :catalog :init-value #f)
     (remarks :init-keyword :remarks :init-value "")
     (dbc     :init-keyword :dbc)))

  (define-class <dbi-odbc-column> (<dbi-column>)
    ((column-size :init-keyword :column-size :init-value #f)))

  (define-method dbi-make-connection ((driver <dbi-odbc-driver>)
				      (options <string>)
				      (option-alist <list>) . auth)
    (define (get-option name :optional (default #f))
      (or (and-let* ((v (assoc name option-alist))) (cdr v))
	  default))
    (define (->boolean s)
      (if (string? s) (not (string=? s "false")) s))
    (let ((env (odbc-driver-env driver))
	  (server   (get-option "server"))
	  (username (get-option "username" ""))
	  (password (get-option "password" ""))
	  (auto-commit (get-option "auto-commit" #t)))
      (unless server
	(assertion-violation 'make-odbc-connection
			     "server option is required"))
      (let-keywords auth ((username username)
			  (password password)
			  (auto-commit (->boolean auto-commit)))
	(make <dbi-odbc-connection>
	  :hbc (connect! env server username password auto-commit)))))

  (define-method dbi-open? ((conn <dbi-odbc-connection>))
    (connection-open? (odbc-connection-odbc-hbc conn)))
  (define-method dbi-close ((conn <dbi-odbc-connection>))
    (disconnect! (odbc-connection-odbc-hbc conn))
    (free-handle! (odbc-connection-odbc-hbc conn)))
  (define-method dbi-open? ((q <dbi-odbc-query>))
    (statement-open? (dbi-query-prepared q)))
  (define-method dbi-close ((q <dbi-odbc-query>))
    (free-handle! (dbi-query-prepared q)))

  (define-method dbi-prepare ((conn <dbi-odbc-connection>)
			      (sql <string>) . args)
    (let* ((c (odbc-connection-odbc-hbc conn))
	   (stmt (prepare c sql)))
      (unless (null? args)
	;; bind
	(do ((i 1 (+ i 1))
	     (params args (cdr params)))
	    ((null? params) #t)
	  (bind-parameter! stmt i (car params))))
      (make <dbi-odbc-query>
	:connection c
	:prepared stmt)))

  (define-method dbi-commit! ((conn <dbi-odbc-connection>))
    (let ((c (odbc-connection-odbc-hbc conn)))
      (commit! c)))

  (define-method dbi-rollback! ((conn <dbi-odbc-connection>))
    (let ((c (odbc-connection-odbc-hbc conn)))
      (rollback! c)))

  (define-method dbi-bind-parameter! ((query <dbi-odbc-query>)
				     (index <integer>) value . args)
    (let ((stmt (dbi-query-prepared query)))
      (bind-parameter! stmt index value)))

  (define-method dbi-execute! ((query <dbi-odbc-query>) . args)
    (let ((stmt (dbi-query-prepared query)))
      (unless (null? args)
	;; bind
	(do ((i 1 (+ i 1))
	     (params args (cdr params)))
	    ((null? params) #t)
	  (bind-parameter! stmt i (car params))))
      (execute! stmt)
      (row-count stmt)))

  (define-method dbi-fetch! ((query <dbi-odbc-query>))
    (let* ((stmt (dbi-query-prepared query))
	   (next? (fetch! stmt)))
      (if next?
	  (let* ((count (column-count stmt))
		 (ret (make-vector count)))
	    (do ((i 1 (+ i 1)))
		((= i (+ count 1)) ret)
	      (let ((data (get-data stmt i)))
		(cond ((odbc-date? data)
		       (vector-set! ret (- i 1) (odbc-date->date data)))
		      ((odbc-time? data)
		       (vector-set! ret (- i 1) (odbc-time->time data)))
		      ((odbc-timestamp? data)
		       (vector-set! ret (- i 1) (odbc-timestamp->date data)))
		      (else
		       (vector-set! ret (- i 1) data))))))
	  #f)))

  (define-method dbi-fetch-all! ((query <dbi-odbc-query>))
    (let loop ((v (dbi-fetch! query))
	       (r '()))
      (if v
	  (loop (dbi-fetch! query) (cons v r))
	  (reverse! r))))

  (define-method dbi-commit! ((query <dbi-odbc-query>))
    (let ((c (dbi-query-connection query)))
      (commit! c)))

  (define-method dbi-rollback! ((query <dbi-odbc-query>))
    (let ((c (dbi-query-connection query)))
      (rollback! c)))

  (define-method dbi-columns ((query <dbi-odbc-query>))
    (let ((c (dbi-query-prepared query)))
      (result-columns c)))

  (define (make-odbc-driver)
    (make <dbi-odbc-driver> :env (create-odbc-env)))

  (define-method dbi-tables ((conn <dbi-odbc-connection>)
			     :key (schema "") (table "") (types '()))
    (let* ((dbc (odbc-connection-odbc-hbc conn))
	   (v (tables dbc schema table types)))
      (map (lambda (l)
	     (let* ((schema (car l))
		    (name   (cadr l))
		    (type   (string->symbol 
			     (string-downcase (caddr l)))))
	       (make <dbi-odbc-table>
		 :schema schema :name name :type type 
		 :remarks (cadddr l) :dbc dbc)))
	   v)))

  (define-method dbi-table-columns ((table <dbi-odbc-table>)
				    :key (column ""))
    (let ((cs (columns (slot-ref table 'dbc)
		       (slot-ref table 'schema)
		       (slot-ref table 'name)
		       column)))
      (map (lambda (c)
	     (make <dbi-odbc-column>
	       :name (list-ref c 2)
	       :table table
	       :column-type (list-ref c 3)
	       :column-size (list-ref c 4)
	       :nullable? (list-ref c 5))) cs)))

  (define-method write-object ((i <dbi-odbc-table>) port)
    (format port "#<dbi-odbc-table ~a ~a ~a>"
	    (slot-ref i 'schema)
	    (slot-ref i 'name)
	    (slot-ref i 'type)))

  (define-method write-object ((c <dbi-odbc-column>) port)
    (format port "#<dbi-odbc-column ~a ~a ~a ~a>"
	    (slot-ref (slot-ref c 'table) 'name)
	    (slot-ref c 'name)
	    (slot-ref c 'column-type)
	    (slot-ref c 'nullable?)))
)
