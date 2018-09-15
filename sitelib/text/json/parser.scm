;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/json/parser.scm - JSON parser
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

;; peg parser of JSON
;; reference
;; RFC8259: https://tools.ietf.org/html/rfc8259
#!nounbound
(library (text json parser)
    (export parse-json
	    json:parser)
    (import (rnrs)
	    (peg)
	    (sagittarius generators)
	    (srfi :14 char-sets)
	    (srfi :127 lseqs))
(define-condition-type &json &error
  make-json-error json-error?)
(define-condition-type &json-parse &json
  make-json-parse-error json-parse-error)

(define (json-parse-error message . irr)
  (define c (condition (make-json-parse-error)
		       (make-who-condition 'parse-json)
		       (make-message-condition message)))
  (if (null? irr)
      (raise c)
      (raise (condition c (make-irritants-condition irr)))))
(define ($error message)
  (lambda (in)
    (if (null? in)
	(json-parse-error "Unexpected EOF")
	(json-parse-error message (lseq-car in)))))

(define ($cs s) ($satisfy (lambda (c) (char-set-contains? s c))))
(define ws ($many ($cs (char-set #\space #\tab #\newline #\return))))

(define digit ($cs (string->char-set "012345789")))

(define begin-array ($seq ws ($eqv? #\[) ws))
(define begin-object ($seq ws ($eqv? #\{) ws))
(define end-array ($seq ws ($eqv? #\]) ws))
(define end-object ($seq ws ($eqv? #\}) ws))
(define name-separactor ($seq ws ($eqv? #\:) ws))
(define value-separactor ($seq ws ($eqv? #\,) ws))

(define (token s) (apply $seq (map $eqv? (string->list s))))

(define json:true  ($do ((token "true")) ($return #t)))
(define json:false ($do ((token "false")) ($return #f)))
(define json:null  ($do ((token "null")) ($return 'null)))

(define json:int
  ($or ($do (($eqv? #\0)) ($return "0"))
       ($do (c ($cs (string->char-set "12345789")))
	    (c* ($many digit))
	    ($return (apply string c c*)))))
(define json:frac
  ($do (($eqv? #\.))
       (c* ($many digit 1))
       ($return (list->string c*))))
(define json:exp
  ($do (e ($or ($eqv? #\e) ($eqv? #\E)))
       (s ($or ($eqv? #\-) ($eqv? #\+)))
       (c* ($many digit 1))
       ($return (apply string e s c*))))

(define json:number
  ($do (sig ($optional ($do (($eqv? #\-)) ($return "-")) "+"))
       (int json:int)
       (frac ($optional json:frac ""))
       (exp  ($optional json:exp ""))
       ($return (let* ((s (string-append sig int frac exp))
		       (v (string->number s)))
		  (or v
		      (json-parse-error "Invalid JSON number" s))))))
(define json:unescaped
  ($cs (char-set-union
	(ucs-range->char-set #x20 (+ #x21 1))
	(ucs-range->char-set #x23 (+ #x5B 1))
	(ucs-range->char-set #x5D (+ #x10FFFF 1)))))

(define json:code-point
  ($do (c* ($repeat ($cs char-set:hex-digit) 4))
       ($return (string->number (list->string c*) 16))))
(define json:escaped
  ($seq ($eqv? #\\)
	($or ($do (($eqv? #\")) ($return #\"))
	     ($do (($eqv? #\\)) ($return #\\))
	     ($do (($eqv? #\/)) ($return #\/))
	     ($do (($eqv? #\b)) ($return #\backspace))
	     ($do (($eqv? #\f)) ($return #\page))
	     ($do (($eqv? #\n)) ($return #\linefeed))
	     ($do (($eqv? #\r)) ($return #\return))
	     ($do (($eqv? #\t)) ($return #\tab))
	     ($do (($eqv? #\u))
		  (cp json:code-point)
		  ($if (<= #xd800 cp #xdbff)
		       ($do (($eqv? #\\)) (($eqv? #\u))
			    (cp2 json:code-point)
			    ($if (<= #xdc00 cp2 #xdfff)
				 ($return (integer->char
					   (+ #x10000
					      (* (- cp #xd800) #x400)
					      (- cp2 #xdc00))))
				 ($error "Invalid unicode code point")))
		       ($return (integer->char cp)))))))

(define json:char
  ($or json:unescaped
       json:escaped))
(define json:string
  ($do (($eqv? #\"))
       (c* ($many json:char))
       (($eqv? #\"))
       ($return (list->string c*))))

(define json:array
  ($do begin-array
       (v* ($optional ($do (v1 (json:value))
			   (v* ($many ($seq value-separactor (json:value))))
			   ($return (cons v1 v*)))
		      '()))
       (($or end-array ($error "Invalid JSON array")))
       ($return v*)))

(define json:member
  ($do (k json:string) name-separactor (v (json:value)) ($return (cons k v))))
(define json:object
  ($do begin-object
       (v* ($optional ($do (v1 json:member)
			   (v* ($many ($seq value-separactor json:member)))
			   ($return (cons v1 v*)))
		      '()))
       (($or end-object ($error "Invalid JSON object")))
       ($return (list->vector v*))))

(define (json:value)
  ($or json:true
       json:null
       json:false
       json:object
       json:array
       json:string
       json:number
       ($error "Invalid JSON character")))

(define json:parser ($do ws (v (json:value)) ws ($return v)))

(define (parse-json in)
  (define lseq (generator->lseq (port->char-generator in)))
  (let-values (((s v nl) (json:parser lseq)))
    (if (parse-success? s)
	v
	(json-parse-error "Failed to parse JSON"))))

)
