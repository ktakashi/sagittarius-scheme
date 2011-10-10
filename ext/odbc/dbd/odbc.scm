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
	    (sagittarius)
	    (sagittarius control))

  (define-record-type dbi-odbc-driver
    (parent dbi-driver)
    (fields odbc-env)
    (protocol
     (lambda (p)
       (lambda (e)
	 (let ((c (p 'odbc-driver
		     :make-connection make-odbc-connection)))
	   (c e))))))

  (define (make-odbc-connection driver options option-alist . auth)
    (let ((env (dbi-odbc-driver-odbc-env driver))
	  (server (assoc "server" option-alist)))
      (unless server
	(assertion-violation 'make-odbc-connection
			     "server option is required"))
      (let-keywords auth ((username "")
			  (password "")
			  (auto-commit? #t))
	(make-dbi-odbc-connection (connect! env (cdr server) username password auto-commit?)))))

  (define-record-type dbi-odbc-connection
    (parent dbi-connection)
    (fields odbc-hbc)
    (protocol
     (lambda (p)
       (lambda (conn)
	 (let ((c (p :open? odbc-open?
		     :close odbc-close
		     :prepare odbc-prepare
		     :commit odbc-commit
		     :rollback odbc-rollback)))
	   (c conn))))))
  
  (define (odbc-open? conn)
    (let ((c (dbi-odbc-connection-odbc-hbc conn)))
      (connection-open? c)))

  (define (odbc-close conn)
    (let ((c (dbi-odbc-connection-odbc-hbc conn)))
      (disconnect! c)))

  (define (odbc-prepare conn sql . args)
    (let* ((c (dbi-odbc-connection-odbc-hbc conn))
	   (stmt (prepare c sql)))
      (unless (null? args)
	;; bind
	(do ((i 1 (+ i 1))
	     (params args (cdr params)))
	    ((null? params) #t)
	  (bind-parameter! stmt i (car params))))
      (make-dbi-query c stmt :bind-parameter odbc-bind-parameter
		             :execute odbc-execute
			     :fetch odbc-fetch
			     :fetch-all odbc-fetch-all
			     :commit odbc-stmt-commit
			     :rollback odbc-stmt-rollback
			     :columns odbc-columns)))

  (define (odbc-commit conn)
    (let ((c (dbi-odbc-connection-odbc-hbc conn)))
      (commit! c)))

  (define (odbc-rollback conn)
    (let ((c (dbi-odbc-connection-odbc-hbc conn)))
      (rollback! c)))

  (define (odbc-bind-parameter query index value . args)
    (let ((stmt (dbi-query-prepared query)))
      (bind-parameter! stmt index value)))

  (define (odbc-execute query . args)
    (let ((stmt (dbi-query-prepared query)))
      (unless (null? args)
	;; bind
	(do ((i 1 (+ i 1))
	     (params args (cdr params)))
	    ((null? params) #t)
	  (bind-parameter! stmt i (car params))))
      (execute! stmt)))

  (define (odbc-fetch query)
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

  (define (odbc-fetch-all query)
    (let loop ((v (odbc-fetch query))
	       (r '()))
      (if v
	  (loop (odbc-fetch query) (cons v r))
	  (reverse! r)))
    )

  (define (odbc-stmt-commit query)
    (let ((c (dbi-query-prepared query)))
      (commit! c)))

  (define (odbc-stmt-rollback query)
    (let ((c (dbi-query-prepared query)))
      (rollback! c)))

  (define (odbc-columns query)
    (let ((c (dbi-query-prepared query)))
      (result-columns c)))

  (define (make-odbc-driver)
    (make-dbi-odbc-driver (create-odbc-env)))
)