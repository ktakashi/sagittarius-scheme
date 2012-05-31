;;; -*- Scheme -*-
;;;
;;; cgi.scm - Common Gateway Interface support
;;;  
;;;   Copyright (c) 2009-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; the implementation based on Python's cgi.py
;; multipart parsing part is based on Gauche's cgi.scm
#!compatible
#< (sagittarius regex) >
(library (www cgi)
    (export cgi-parse
	    split-query-string
	    cgi-add-temporary-file
	    cgi-temporary-files
	    with-cgi-parameters
	    cgi-get-parameter
	    cgi-header
	    &cgi-error)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (sagittarius)
	    (sagittarius regex)
	    (rfc mime)
	    (rfc uri)
	    (rfc :5322)
	    (shorten)
	    (util file)
	    (util list)
	    (text html-lite)
	    (text tree)
	    (srfi :1  lists)
	    (srfi :13 strings)
	    (srfi :14 char-set)
	    (srfi :26 cut)
	    (srfi :39 parameters))

  ;; error condition
  (define-condition-type &cgi-error &error
    make-cgi-error cgi-error?)

  (define (cgi-error who msg . irr)
    (raise (apply condition
		  (filter values
			  (list (make-cgi-error)
				(and who (make-who-condition who))
				(make-message-condition msg)
				(make-irritants-condition irr))))))

  ;; the list of temporary files
  (define cgi-temporary-files (make-parameter '()))
  (define (cgi-add-temporary-file path)
    (cgi-temporary-files (cons path (cgi-temporary-files))))

  ;; utility
  (define (get-env name environment)
    (or (and-let* ((p (assoc name environment)))
	  (cdr p))
	#f))

  ;; default reads bytevector from current-input-port. so we need to
  ;; use reckless flag. can this be (standard-input-port) ?
  (define (default-contnt-reader :optional (size #f))
    (if size
	(get-bytevector-n (current-input-port) size #t)
	(get-bytevector-all (current-input-port) #t)))

  (define (split-query-string input)
    (fold-right (lambda (elt params)
		  (let* ((ss (string-split elt #\=))
			 (n  (uri-decode-string (car ss) :cgi-decode #t))
			 (p  (assoc n params))
			 (v (if (null? (cdr ss))
				#t
				(uri-decode-string (string-join (cdr ss) "=")
						   :cgi-decode #t))))
		    (cond (p
			   (set-cdr! p (cons v (cdr p))) params)
			  (else (cons (list n v) params)))))
		'()
		(string-split input #/&|\;/))) ;; | dummy for emacs )))

  ;; Parse  a query in the environment.
  ;; 
  ;; keyword arguments:
  ;;  environment    : target environment alist. default (getenv-alist)
  ;;  content-reader : reader for form content. must be a procedure
  ;;                   accepts 0 or one argiument size and returns bytevector.
  ;;                   this is for fastcgi. see fastcgi.scm and its POST-READER.
  ;;  strict?        : flag indicating what to do with parsing errors.
  ;;                   default #f (ignored)
  ;;  transcoder     : how to convert the parsed bytevector. default
  ;;                   utf-8, eol-style: none. if this is #f it returns raw
  ;;                   bytevector
  ;; 
  (define (cgi-parse :key 
		     (environment (getenv-alist))
		     (content-reader default-contnt-reader)
		     (strict? #f)
		     (part-handlers '())
		     (transcoder (make-transcoder (utf-8-codec) 'none)))

    (let ((method (cond ((get-env "REQUEST_METHOD" environment)
			 => (lambda (val)
			      (string->symbol (string-upcase val))))
			(else 'GET)))
	  (qs (get-env "QUERY_STRING" environment)))
      (case method
	((GET HEAD)
	 (if qs
	     (split-query-string qs)
	     '()))
	((POST)
	 ;; check form type
	 (let* ((ctype (mime-parse-content-type
			(get-env "CONTENT_TYPE" environment)))
		(typesig (and ctype (list (car ctype) (cadr ctype))))
		(method (get-env "REQUEST_METHOD" environment)))
	   (unless (or (not ctype)
		       (member typesig
			       '(("application" "x-www-form-urlencoded")
				 ("multipart" "form-data"))))
	     (cgi-error 'cgi-parse
			"unsupported CONTENT_TYPE" ctype))
	   (unless method
	     (cgi-error 'cgi-parse
			"REQUEST_METHOD is missing"))
	   (if (equal? typesig '("multipart" "form-data"))
	       (parse-multipart part-handlers content-reader environment)
	       (let* ((cqs 
		       (or (and-let* ((len (get-env "CONTENT_LENGTH"
						    environment))
				      (n (string->number len)))
			     (utf8->string (content-reader n)))
			   ""))
		      (qs (get-env "QUERY_STRING" environment)))
		 (split-query-string (if qs
					 (string-append cqs "&" qs)
					 cqs))))))
	(else (cgi-error 'cgi-parse
			 "unknown REQUEST_METHOD" method)))))

  ;; internal
  (define *quote-set* (string->char-set "\""))
  (define *backslash-quote-set* (string->char-set "\\\""))

  (define (parse-multipart part-handlers reader env)
    (define (part-ref info name)
      (rfc5322-header-ref (mime-part-headers info) name))

    (define (make-file-handler prefix keep-original? mode)
      (lambda (name filename part-info in)
	(receive (out tmpfile) (make-temporary-file prefix)
	  (cgi-add-temporary-file tmpfile)
	  (mime-retrieve-body part-info in out)
	  (close-output-port out)
	  (when mode (change-file-mode tmpfile mode))
	  (if keep-original?
	      (list tmpfile filename)
	      tmpfile))))

    (define (string-handler name filename part-info in)
      (mime-body->string part-info in))

    (define (ignore-handler name filename part-info in)
      (let loop ((ignore (get-line in)))
	(if (eof-object? ignore) #f (loop (get-line in)))))

    (define (get-action&opts name)
      (let ((clause (find (lambda (entry)
			    (or (eq? (car entry) #t)
				(and (regex-pattern? (car entry))
				     ((car entry) name))
				(and (list? (car entry))
				     (member name (map (^n (format "~a" n))
						       (car entry))))
				(string=? (format "~a" (car entry)) name)))
			  part-handlers)))
	(if (or (not clause) (not (pair? (cdr clause))))
	    (list string-handler) ;; default
	    (cdr clause))))

    (define (get-handler action . opts)
      (cond ((not action) string-handler)
	    ((memq action '(file file+name))
	     (make-file-handler (get-keyword :prefix opts
					     (build-path (temporary-directory)
							 "sagittarius-cgi-"))
				(eq? action 'file+name)
				(get-keyword :mode opts #f)))
	    ((eq? action 'ignore) ignore-handler)
	    (else action)))

    (define (content-disposition-string input)
      (get-char input)
      (call-with-string-output-port
       (lambda (out)
	 (let loop ((c (get-char input)))
	   (cond ((eof-object? c))
		 ((char=? c #\"))
		 ((char=? c #\\)
		  (let ((c (get-char input)))
		    (cond ((eof-object? c))
			  (else
			   (unless (char-set-contains? *backslash-quote-set* c)
			     (put-char out #\\))
			   (put-char out c)
			   (loop (get-char input))))))
		 (else (put-char out c) (loop (get-char input))))))))

    (define (parse-content-disposition field)
      (if field
	  (rfc5322-field->tokens 
	   field
	   `((,*quote-set* . ,content-disposition-string)
	     (,*rfc5322-atext-chars* . ,rfc5322-dot-atom)))
	  '()))
    (define (get-option name regex opts)
      (cond ((member name opts) => cadr)
	    ((exists (^t (and (string? t) (regex t))) opts)
	     => (lambda (m) (m 'after)))
	    (else #f)))
    (define (handle-part part-info in)
      (let* ((cd (part-ref part-info "content-disposition"))
	     (opts (parse-content-disposition cd))
	     (name (get-option "name=" #/^name=/ opts))
	     (filename (get-option "filename=" #/^filename=/ opts)))
	(cond
	 ((not name)
	  (ignore-handler name filename part-info in))
	 ((not filename)
	  (list name (string-handler name filename part-info in)))
	 ((string-null? filename)
	  (list name (ignore-handler name filename part-info in)))
	 (else
	  (let* ((action&opts (get-action&opts name))
		 (handler (apply get-handler action&opts))
		 (result (handler name filename part-info in)))
	    (list name result))))))

    (let* ((len (string->number (get-env "CONTENT_LENGTH" env)))
	   (ctype (get-env "CONTENT_TYPE" env))
	   (in  (open-bytevector-input-port (reader len)))
	   (result (mime-parse-message in `(("content-type" ,ctype))
				       handle-part)))
      (filter-map (cut mime-part-content <>) (mime-part-content result))))

  (define (default-converter x) x)
  (define (cgi-get-parameter name params :key
			     ((:list lis) #f)
			     (default (if lis '() #f))
			     ((:convert cv) default-converter))
    (cond ((assoc name params) => (^p (if lis (map cv (cdr p)) (cv (cadr p)))))
	  (else default)))

  (define (cgi-header :key
		      (content-type #f)
		      (location #f)
		      (status #f)
		      (cookies '())
		      :allow-other-keys rest)
    (let-syntax ((push-string! (syntax-rules ()
				 ((_ l fmt n v)
				  (set! l (cons (format fmt n v) l)))
				 ((_ l fmt v)
				  (set! l (cons (format fmt v) l)))
				 ((_ l v)
				  (set! l (cons v l))))))
      (let ((ct (or content-type (and (not location) "text/html")))
	    (r '()))
	(when status (push-string! r "Status: ~a\r\n" status))
	(when ct (push-string! r "Content-type: ~a\r\n" ct))
	(when location (push-string! r "Location: ~a\r\n" location))
	(for-each (lambda (cookie)
		    (push-string! r "Set-cookie: ~a\r\n" cookie))
		  cookies)
	(for-each (lambda (p)
		    (when (pair? (cdr p))
		      (push-string! r "~a: ~a\r\n" (car p) (cadr p))))
		  (slices rest 2))
	(push-string! r "\r\n")
	(reverse! r))))

  (define (with-cgi-parameters proc :key
			       (on-error cgi-default-error-proc)
			       (output-proc cgi-default-output)
			       :allow-other-keys opts)
    (guard (e (#t (output-proc (on-error e))))
      (let ((params (apply cgi-parse opts)))
	(output-proc (proc params))))
    (for-each delete-file (cgi-temporary-files))
    0)

  (define (cgi-default-error-proc e)
    `(,(cgi-header)
      ,(html-doctype)
      ,(html:html
	(html:head (html:title "Error"))
	(html:body (html:h1 "Error")
		   (html:p (html-escape-string 
			    (cond ((message-condition? e)
				   (condition-message e))
				  ((string? e) e)
				  (else (format "~a" e)))))))))

  (define (cgi-default-output tree)
    (write-tree tree))
)
