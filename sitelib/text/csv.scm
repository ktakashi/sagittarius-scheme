;;; -*- Scheme -*-
;;;
;;; csv.scm - CSV parser
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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


(library (text csv)
    (export csv->list
	    read-csv
	    write-csv

	    ;; CLOS
	    <csv>
	    ;; low level APIs
	    csv-header
	    csv-records
	    ;; construct 
	    add-records!
	    add-record!
	    set-header!
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (packrat)
	    (sagittarius control)
	    (srfi :1 lists)
	    (srfi :14 char-set)
	    (srfi :26 cut)
	    (clos user))

  (define *text-set* 
    (ucs-range->char-set! #x2d #x7e #f
	(ucs-range->char-set! #x23 #x2b #f
	    (ucs-range->char-set #x20 #x21))))

  (define *any-set*
    (char-set-union *text-set*
		    (string->char-set ",\r\n")))

  (define (read-text results pred)
    (let loop ((acc '()) (results results))
      (let ((ch (parse-results-token-value results)))
	;; it's just testing
	(cond ((and ch (pred ch results)) =>
	       (lambda (ch)
		 (loop (cons (car ch) acc)
		       (parse-results-next (cdr ch)))))
	      (else (make-result (list->string (reverse! acc)) results))))))

  (define (any results)
    (read-text results (lambda (ch results)
			 (case ch
			   ((#\")
			    (let ((ch (parse-results-token-value
				       (parse-results-next results))))
			      (and ch (char=? ch #\")
				   (cons ch (parse-results-next results)))))
			   (else
			    (and (char-set-contains? *any-set* ch)
				 (cons ch results)))))))
  (define (textdata results)
    (read-text results (lambda (ch results)
			 (and (char-set-contains? *text-set* ch)
			      (cons ch results)))))

  (define parser
    (packrat-parser 
     (begin 
       (define (token str)
	 (lambda (starting-results)
	   (let loop ((pos 0) (results starting-results))
	     (if (= pos (string-length str))
		 (make-result str results)
		 (let ((ch (parse-results-token-value results)))
		   (if (and ch (char=? ch (string-ref str pos)))
		       (loop (+ pos 1) (parse-results-next results))
		       (make-expected-result
			(parse-results-position starting-results) str)))))))
       (define (comment results)
	 (let loop ((acc '()) (results results))
	   (let ((ch (parse-results-token-value results)))
	     (case ch
	       ((#\linefeed)
		(make-result (list->string (reverse! acc)) results))
	       ((#\return)
		(let ((ch (parse-results-token-value 
			   (parse-results-next results))))
		  (if (and ch (char=? ch #\linefeed))
		      (make-result (list->string (reverse! acc)) results)
		      (loop (cons ch acc) (parse-results-next results)))))
	       (else (loop (cons ch acc)
			   (parse-results-next results)))))))
       file)

     (file     ((h <- header crlf r <- records) (cons h r))
	       ((h <- header) (list h)))
     ;; supports both crlf and lf
     (crlf     (((/ ((token "\r\n")) ((token "\n")))) "\r\n"))
     (header   ((comments n <- names) (cons :header n)))
     (records  ((comments f <- fields crlf r <- records)
		(cons (cons :record f) r))
	       ((comments f <- fields) (list (cons :record f))))
     (comments (('#\# comment crlf) '())
	       (() '()))
     (fields   ((f <- field-entry '#\, fs <- fields) (cons f fs))
	       ((f <- field-entry) (list f)))
     (names    ((n <- field-entry '#\, ns <- fields) (cons n ns))
	       ((n <- field-entry) (list n)))
     (field-entry (('#\" e <- any '#\") e)
		  ((e <- textdata) e))))

  (define (generator p)
    (let ((ateof #f)
	  (pos (top-parse-position "<?>")))
      (lambda ()
	(if ateof
	    (values pos #f)
	    (let ((x (read-char p)))
	      (if (eof-object? x)
		  (begin
		    (set! ateof #t)
		    (values pos #f))
		  (let ((old-pos pos))
		    (set! pos (update-parse-position pos x))
		    (values old-pos (cons x x)))))))))

  (define (csv->list p :optional (first-line-is-header? #f))
    (let ((result (parser (base-generator->results (generator p)))))
      (if (parse-result-successful? result)
	  (let ((csv-list (parse-result-semantic-value result)))
	    (unless first-line-is-header?
	      (cond ((assq :header csv-list)
		     => (lambda (slot) (set-car! slot :record)))))
	    csv-list)
	  (apply assertion-violation
		 (let ((e (parse-result-error result)))
		   (list 'parse-csv
			 (parse-error-messages e)
			 (parse-position->string (parse-error-position e))
			 (parse-error-expected e)))))))

  ;; CLOS
  (define-class <csv> ()
    ((header  :init-keyword :header   :init-value '()
	      :accessor csv-header)
     (records :init-keyword :records  :init-value '()
	      :accessor csv-records)))

  (define (process-parsed-csv lst)
    (let loop ((lst lst) (header '()) (records '()))
      (cond ((null? lst) (values (if (null? header) '()
				     (car (reverse! header)))
				 (reverse! records)))
	    ((eq? :header (caar lst))
	     (loop (cdr lst) (cons (cdar lst) header) records))
	    ((eq? :record (caar lst))
	     (loop (cdr lst)  header (cons (cdar lst) records)))
	    (else
	     (assertion-violation 'process-parsed-csv
				  "invalid csv list" lst)))))

  (define-method read-csv ((p <port>) . opt)
    (let ((csv-list (apply csv->list p opt)))
      ;; get two lists, header and records
      (let-values (((header records) (process-parsed-csv csv-list)))
	(make <csv> :header header :records records))))
  ;; assume this is csv string
  (define-method read-csv ((s <string>) . opt)
    (apply read-csv (open-string-input-port s) opt))

  (define-method write-csv ((csv <csv>) . opt)
    (let ((out (if (null? opt) (current-output-port) (car opt))))
      (let ((header (csv-header csv))
	    (records (csv-records csv)))
	(define (write-value v out)
	  (unless (zero? (string-length v))
	    (display #\" out)
	    (string-for-each (lambda (c)
			       (case c
				 ((#\")
				  (display #\" out)
				  (display #\" out))
				 (else (display c out)))) v)
	    (display #\" out)))
	(define (write-line lst out)
	  (unless (null? lst)
	    (write-value (car lst) out)
	    (dolist (v (cdr lst))
	      (display #\, out)
	      (write-value v out))
	    (display "\r\n" out)))

	(write-line header out)
	(for-each (cut write-line <> out) records))))

  ;; Should we define this? 
  #;(define-method write-object ((csv <csv>) (p <port>))
    (write-csv csv p))

  ;; User level APIs
  ;; given string must be a valid csv line and it will be parsed by reader
  (define-method add-records! ((csv <csv>) (s <string>))
    (let ((parsed (csv->list (open-string-input-port s) #f)))
      (add-records! csv parsed)))

  (define-method add-records! ((csv <csv>) (l <list>))
    (dolist (v l)
      (add-record! csv v)))

  (define-method add-record! ((csv <csv>) (l <list>))
    ;; validity check
    (unless (eq? :record (car l))
      (assertion-violation 'add-record!
			   "csv record list must have :record at its car part"
			   l))
    (csv-records csv (append (csv-records csv) (list (cdr l)))))

  (define-method set-header! ((csv <csv>) (s <string>))
    (let ((parsed (csv->list (open-string-input-port s) #t)))
      (set-header! csv parsed)))

  (define-method set-header! ((csv <csv>) (l <list>))
    (cond 
     ;; try alist
     ((assq :header l)
      => (lambda (slot)
	   (csv-header csv (cdr slot))))
     ;; maybe a list?
     ((memq :header l)
      ;; TODO should we add check?
      => (lambda (l) (csv-header csv (cdr l))))
     (else
      (assertion-violation 'set-header!
			   "csv header list must have :header at its car part"
			   l))))

)