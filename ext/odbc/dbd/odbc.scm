;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; odbc.scm - DBD for ODBC
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

  (define-method dbi-make-connection ((driver <dbi-odbc-driver>)
				      (options <string>)
				      (option-alist <list>) . auth)
    (let ((env (odbc-driver-env driver))
	  (server (assoc "server" option-alist)))
      (unless server
	(assertion-violation 'make-odbc-connection
			     "server option is required"))
      (let-keywords auth ((username "")
			  (password "")
			  (auto-commit? #t))
	(make <dbi-odbc-connection>
	  :hbc (connect! env (cdr server) username password auto-commit?)))))

  (define-method dbi-open? ((conn <dbi-odbc-connection>))
    (let ((c (odbc-connection-odbc-hbc conn)))
      (connection-open? c)))

  (define-method dbi-close ((conn <dbi-odbc-connection>))
    (let ((c (odbc-connection-odbc-hbc conn)))
      (disconnect! c)))

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
)
