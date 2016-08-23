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
	    csv-read
	    csv-write

	    ;; CLOS
	    <csv> csv?
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
	    (sagittarius control)
	    (except (srfi :1 lists) any)
	    (util port)
	    (srfi :14 char-sets)
	    (srfi :26 cut)
	    (clos user))

  (define *text-set* 
    ;; range %x20-21 / %x23-2B / %x2D-7E
    ;; but ucs-range->char-set! doesn't add the end range
    ;; so increment 1
    (ucs-range->char-set! #x2d #x7f #f
	(ucs-range->char-set! #x23 #x2c #f
	    (ucs-range->char-set #x20 #x22))))

  (define *any-set* char-set:full)

  (define (csv-read1 p)
    (define (read-entry p)
      (define (finish cs eol?)
	(values (list->string (reverse! cs)) eol?))
      (define (crlf? c p)
	(case c
	  ((#\return)
	   (when (eqv? (lookahead-char p) #\newline)
	     (get-char p))
	   #t)
	  ((#\newline) #t)
	  (else #f)))
      ;; reads until #\,
      (let loop ((cs '()) (q? #f))
	(let ((c (get-char p)))
	  (cond ((eof-object? c) (finish cs #t)) ;; EOF end 
		((and (not q?) (eqv? c #\,)) (finish cs #f)) ;; end
		((eqv? c #\") ;; double quote
		 (if q?
		     (case (lookahead-char p)
		       ((#\") (get-char p) (loop (cons c cs) q?))
		       (else (loop cs #f)))
		     (loop cs #t)))
		((and (not q?) (crlf? c p)) (finish cs #t))
		(else (loop (cons c cs) q?))))))
    (let loop ((r '()))
      (let ((c (lookahead-char p)))
	(cond ((eof-object? c) c)
	      ((eqv? c #\#) (get-line p) (loop r))
	      (else
	       (let-values (((e eol?) (read-entry p)))
		 (if eol?
		     (reverse! (cons e r))
		     (loop (cons e r)))))))))
	 

  ;; if optional argument first-line-is-header? is #f
  (define (csv->list p :optional (first-line-is-header? #f))
    (let ((ch (lookahead-char p)))
      (if (eof-object? ch)
	  (list :records "") ;; empty
	  (let* ((header (if first-line-is-header?
			     (cons :header (csv-read1 p))
			     #f))
		 (records (port-map (lambda (r) (cons :record r))
				    (lambda () (csv-read1 p)))))
	    (if header
		(cons header records)
		records)))))

  ;; CLOS
  (define-class <csv> ()
    ((header  :init-keyword :header   :init-value '()
	      :accessor csv-header)
     (records :init-keyword :records  :init-value '()
	      :accessor csv-records)))
  (define (csv? o) (is-a? o <csv>))

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

  (define-generic csv-read)
  (define-method csv-read ((p <port>) . opt)
    (let ((csv-list (apply csv->list p opt)))
      ;; get two lists, header and records
      (let-values (((header records) (process-parsed-csv csv-list)))
	(make <csv> :header header :records records))))
  ;; assume this is csv string
  (define-method csv-read ((s <string>) . opt)
    (apply csv-read (open-string-input-port s) opt))

  (define-generic csv-write)
  (define-method csv-write ((csv <csv>) . opt)
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
    (csv-write csv p))

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
