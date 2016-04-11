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
	    (except (binary io) get-line)
	    (srfi :6 basic-string-ports)
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

#|
   Encapsulated Message

       Pre-Encapsulation Boundary (Pre-EB)
           -----BEGIN PRIVACY-ENHANCED MESSAGE-----

       Encapsulated Header Portion
           (Contains encryption control fields inserted in plaintext.
           Examples include "DEK-Info:" and "Key-Info:".
           Note that, although these control fields have line-oriented
           representations similar to RFC 822 header fields, the set
           of fields valid in this context is disjoint from those used
           in RFC 822 processing.)

       Blank Line
           (Separates Encapsulated Header from subsequent
           Encapsulated Text Portion)

       Encapsulated Text Portion
           (Contains message data encoded as specified in Section 4.3.)

       Post-Encapsulation Boundary (Post-EB)
           -----END PRIVACY-ENHANCED MESSAGE-----

   Bounary rule:
   The pre-EB is the string The post-EB is either (1) another pre-EB
   indicating that another encapsulated PEM message follows, or (2) 
   the string "-----END PRIVACY-ENHANCED MESSAGE-----" indicating
   that any text that immediately follows is non-PEM text.
|#
  (define (default-pem-decoder content)
    (base64-decode-string content :transcoder #f))
  (define pem-transcoder
    (make-transcoder (utf-8-codec) (eol-style none)))
  (define (parse-pem in :key (multiple #f) (asn1 #f)
		     (builder #f) (decoder default-pem-decoder))
    (define (open-input/output-string-port)
      (transcoded-port (open-chunked-binary-input/output-port)
		       pem-transcoder))
    (define (parse-header header-string)
      (rfc5322-read-headers (open-string-input-port header-string)))
    (define (read-content p type)
      (define (ret content-out type)
	(set-port-position! content-out 0)
	(let ((headers (rfc5322-read-headers content-out)))
	  ;; it doesn't have header so revert the position
	  (when (null? headers) (set-port-position! content-out 0))
	  (values headers (get-string-all content-out) type)))
      (define (put-line out line)
	(put-string out line)
	(let ((l (string-length line)))
	  ;; NB: R6RS get-line doesn't skip \r
	  (cond ((zero? l) (put-string out "\r\n"))
		((char=? #\return (string-ref line (- l 1)))
		 (put-string out "\n"))
		(else (put-string out "\r\n")))))

      (let ((content-out (open-input/output-string-port)))
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
			 (ret content-out #f)))
		    ((#/-----BEGIN (.+?)-----/ line)
		     => (^m
			 (unless multiple 
			   (raise-pem-error make-pem-error 'parse-mem
					    "multiple content without Post-Encapsulation Boundary requires :multiple #t"))
			 (ret content-out (m 1))))
		    (else
		     (put-line content-out line)
		     (loop (get-line p) #t)))))))
    (define (rec in type)
      (define (next type)
	(let-values (((params content type) (read-content in type)))
	  ;; NB: PEM content must be US ASCII.
	  (let1 base64 (if decoder (decoder content) (string->utf8 content))
	    (values params
		    (cond (builder (builder base64))
			  (asn1
			   (read-asn.1-object 
			    (open-bytevector-input-port base64)))
			  (else base64))
		    type))))
      (if type
	  (next type)
	  (let loop ((line (get-line in)))
	    (cond ((eof-object? line)
		   (raise-pem-error make-invalid-pem-format 'parse-pem
				    "unexpected EOF appeared"))
		  ((#/-----BEGIN (.+?)-----/ line) => (^m (next (m 1))))
		  (else (loop (get-line in)))))))

    (let loop ((r '()) (type #f))
      (let-values (((params content type) (rec in type)))
	(cond ((and multiple (not (eof-object? (peek-char in))))
	       (loop (cons (cons params content) r) type))
	      (multiple (reverse! (cons (cons params content) r)))
	      (else (values params content))))))

  ;; for convenience
  (define (parse-pem-file file . opts)
    (call-with-input-file file (cut apply parse-pem <> opts)))

  (define (parse-pem-string str . opts)
    (apply parse-pem (open-string-input-port str) opts))

)
