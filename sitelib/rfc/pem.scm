;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; pem.scm - RFC 1421 PEM format parser library.
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

;; NOTE: this library handle only RFC 1421 format for now.
;;  http://tools.ietf.org/html/rfc1421
#!read-macro=sagittarius/regex
(library (rfc pem)
    (export parse-pem
	    parse-pem-file
	    parse-pem-string)
    (import (rnrs)
	    (asn.1)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius regex)
	    (srfi :6 basic-string-ports)
	    (srfi :8 receive)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (rfc :5322)
	    (rfc base64))

  (define-condition-type &pem-error &error
    make-pem-error pem-error?)

  (define-condition-type &invalid-pem-format &pem-error
    make-invalid-pem-format invalid-pem-format?)

  (define (raise-pem-error ctr who msg . irr)
    (raise (apply condition
		  (filter values
			  (list (ctr)
				(and who (make-who-condition who))
				(make-message-condition msg)
				(make-irritants-condition irr))))))

  (define (parse-pem in :key (multiple #f) (asn1 #f)
		     (builder #f))
    (define (parse-header header-string)
      (rfc5322-read-headers (open-string-input-port header-string)))
    (define (read-content p type)
      (let ((header-out (open-output-string))
	    (content-out (open-output-string)))
	(let loop ((line (get-line p))
		   (content? #f))
	  (if (eof-object? line)
	      (raise-pem-error make-invalid-pem-format
			       'parse-pem "unexpected EOF")
	      (cond ((#/-----END (.+?)-----/ line)
		     => (^m 
			 (unless (string=? type (m 1))
			   (raise-pem-error make-invalid-pem-format 'parse-pem
					    "invalid END clause found"
					    `(expected ,type) `(got ,(m 1))))
			 (values (parse-header (get-output-string header-out))
				 (get-output-string content-out))))
		    ((and (not content?) (string-scan line #\:))
		     ;; As far as I know, this parameters are RFC 822 format
		     (put-string header-out line)
		     (put-string header-out "\r\n");; for rfc5322-read-headers
		     (loop (get-line p) content?))
		    (else
		     (put-string content-out line)
		     (loop (get-line p) #t)))))))
    (define (rec in)
      (let loop ((line (get-line in)))
	(cond ((eof-object? line)
	       (raise-pem-error make-invalid-pem-format 'parse-pem
				"unexpected EOF appeared"))
	      ((#/-----BEGIN (.+?)-----/ line)
	       => (^m 
		   (receive (params content) (read-content in (m 1))
		     (let1 base64 (base64-decode-string content :transcoder #f)
		       (values params
			       (cond (builder (builder base64))
				     (asn1
				      (read-asn.1-object 
				       (open-bytevector-input-port base64)))
				     (else base64)))))))
	      (else (loop (get-line in))))))

    (let loop ((r '()))
      (receive (params content) (rec in)
	(cond ((and multiple (not (eof-object? (peek-char in))))
	       (loop (cons (cons params content) r)))
	      (multiple (reverse! (cons (cons params content) r)))
	      (else (values params content))))))

  ;; for convenience
  (define (parse-pem-file file . opts)
    (call-with-input-file file (cut apply parse-pem <> opts)))

  (define (parse-pem-string str . opts)
    (apply parse-pem (open-string-input-port str) opts))

)