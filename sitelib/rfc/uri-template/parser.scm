;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/uri-template/parser.scm - URI template parser
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

;; Reference:
;; RFC6570 URI Template
;; https://tools.ietf.org/html/rfc6570
#!nounbound
(library (rfc uri-template parser)
    (export parse-uri-template
	    &uri-template-parse
	    uri-template-parse-error? uri-template-error-parsing-template
	    ;; these char sets can be used during expansion
	    (rename (*reserved* *uri-template:reserved-set*)
		    (*unreserved* *uri-template:unreserved-set*))
	    )
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (sagittarius generators)
	    (rfc uri-template conditions)
	    (srfi :14 char-sets)
	    (srfi :127 lseqs))

(define $cs $char-set-contains?)

;;; 1.5.  Notational Conventions
;; ALPHA       =  %x41-5A / %x61-7A   ; A-Z / a-z
;; DIGIT       =  %x30-39             ; 0-9
;; HEXDIG      =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
;;                ; case-insensitive
(define *alpha* (char-set-intersection char-set:letter char-set:ascii))
(define *digit* (char-set-intersection char-set:digit char-set:ascii))
(define *hexdig* char-set:hex-digit)


;; unreserved  =  ALPHA / DIGIT / "-" / "." / "_" / "~"
;; reserved    =  gen-delims / sub-delims
;; gen-delims  =  ":" / "/" / "?" / "#" / "[" / "]" / "@"
;; sub-delims  =  "!" / "$" / "&" / "'" / "(" / ")"
;;             /  "*" / "+" / "," / ";" / "="
(define *unreserved* (char-set-union *alpha* *digit* (string->char-set "-._~")))
(define *gen-delims* (string->char-set ":/?#[]@"))
(define *sub-delims* (string->char-set "!$&'()*+,;="))
(define *reserved* (char-set-union *gen-delims* *sub-delims*))

;; ucschar     =  %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
;;             /  %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
;;             /  %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
;;             /  %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
;;             /  %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
;;             /  %xD0000-DFFFD / %xE1000-EFFFD
;; ucs-range->char-set excludes the end range
(define *ucschar* (char-set-union
		   (ucs-range->char-set #xA0    (+ #xD7FF 1))
		   (ucs-range->char-set #xF900  (+ #xFDCF 1))
		   (ucs-range->char-set #xFDF0  (+ #xFFEF 1))
		   (ucs-range->char-set #x10000 (+ #x1FFFD 1))
		   (ucs-range->char-set #x20000 (+ #x2FFFD 1))
		   (ucs-range->char-set #x30000 (+ #x3FFFD 1))
		   (ucs-range->char-set #x40000 (+ #x4FFFD 1))
		   (ucs-range->char-set #x50000 (+ #x5FFFD 1))
		   (ucs-range->char-set #x60000 (+ #x6FFFD 1))
		   (ucs-range->char-set #x70000 (+ #x7FFFD 1))
		   (ucs-range->char-set #x80000 (+ #x8FFFD 1))
		   (ucs-range->char-set #x90000 (+ #x9FFFD 1))
		   (ucs-range->char-set #xA0000 (+ #xAFFFD 1))
		   (ucs-range->char-set #xB0000 (+ #xBFFFD 1))
		   (ucs-range->char-set #xC0000 (+ #xCFFFD 1))
		   (ucs-range->char-set #xD0000 (+ #xDFFFD 1))
		   (ucs-range->char-set #xE1000 (+ #xEFFFD 1))))
;; iprivate    =  %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
(define *iprivate* (char-set-union
		    (ucs-range->char-set #xE000   (+ #xF8FF))
		    (ucs-range->char-set #xF0000  (+ #xFFFFD))
		    (ucs-range->char-set #x100000 (+ #x10FFFD))))

;; pct-encoded =  "%" HEXDIG HEXDIG
(define pct-encoded
 ($do (($eqv? #\%)) (c1 ($cs *hexdig*)) (c2 ($cs *hexdig*))
      ($return (integer->char (string->number (string c1 c2))))))

;;; 2.1 Literals
;; literals =  %x21 / %x23-24 / %x26 / %x28-3B / %x3D / %x3F-5B
;;             /  %x5D / %x5F / %x61-7A / %x7E / ucschar / iprivate
;;             /  pct-encoded
;;                  ; any Unicode character except: CTL, SP,
;;                  ;  DQUOTE, "'", "%" (aside from pct-encoded),
;;                  ;  "<", ">", "\", "^", "`", "{", "|", "}"
(define *literal-char* (char-set-union
			(string->char-set "!#$&=]_~")
			(ucs-range->char-set #x28 (+ #x3b 1))
			(ucs-range->char-set #x3f (+ #x5b 1))
			(ucs-range->char-set #x61 (+ #x7a 1))
			*ucschar* *iprivate*))
;; we merge literals into chunk of string, so we need to read
;; as many as possible
(define literals
  ($do (c* ($many ($or ($cs *literal-char*) pct-encoded) 1))
       ($return (list->string c*))))

;;; 2.4.1 Prefix Values
;; prefix        =  ":" max-length
;; max-length    =  %x31-39 0*3DIGIT   ; positive integer < 10000
(define prefix ($do (($eqv? #\:))
		    (d1 ($cs (string->char-set "123456789")))
		    (d* ($many ($cs *digit*) 0 3))
		    ($return (string->number (apply string d1 d*)))))
;;; 2.4.2 Composite Values
;; explode       =  "*"
(define explode ($do (($eqv? #\*)) ($return '*)))
;;; 2.4 Value Modifiers
;; modifier-level4 =  prefix / explode
(define modifier-level4 ($or prefix explode))

;;; 2.3 Variables
;; varchar       =  ALPHA / DIGIT / "_" / pct-encoded
(define varchar ($or ($cs *alpha*) ($cs *digit*) ($eqv? #\_) pct-encoded))
;; varname       =  varchar *( ["."] varchar )
(define varname ($do (c varchar)
		     (c* ($many ($do (d ($optional ($eqv? #\.)))
				     (v varchar)
				     ($return (if d (string d v) (string v))))))
		     ($return (apply string-append (string c) c*))))
;; varspec       =  varname [ modifier-level4 ]
(define varspec ($do (n varname)
		     (m ($optional modifier-level4))
		     ($return (if m (list n m) n))))
;; variable-list =  varspec *( "," varspec )
(define variable-list ($do (v varspec)
			   (v* ($many ($seq ($eqv? #\,) varspec)))
			   ($return (cons v v*))))

;;; 2.2 Expressions
;; op-reserve = "=" / "," / "!" / "@" / "|"
(define op-reserve ($cs (string->char-set "=,!@|")))
;; op-level3  = "." / "/" / ";" / "?" / "&"
(define op-level3 ($cs (string->char-set "./;?&")))
;; op-level2  = "+" / "#"
(define op-level2 ($cs (string->char-set "+#")))
;; operator   = op-level2 / op-level3 / op-reserve
(define operator ($or op-level2 op-level3 op-reserve))
;; expression = "{" [ operator ] variable-list "}"
(define expression ($do (($eqv? #\{))
			(o ($optional operator))
			(v* variable-list)
			(($eqv? #\}))
			($return (if o (cons o v*) v*))))

;;; 2 Syntax
;; URI-template = *( literals /expression)
(define uri-template ($many ($or literals expression)))

(define-condition-type &uri-template-parse &uri-template
  make-uri-template-parse-error uri-template-parse-error?
  (template uri-template-error-parsing-template))

(define (parse-uri-template in)
  (define lseq (generator->lseq
		(if (string? in)
		    (string->generator in)
		    (port->char-generator in))))
  (let-values (((s v nl) (uri-template lseq)))
    (if (and (parse-success? s) (null? nl))
	v
	;; should be okay like this...
	(raise
	 (condition
	  (make-uri-template-parse-error (list->string (lseq-realize lseq)))
	  (make-who-condition 'parse-uri-template)
	  (make-message-condition "Failed to parse")
	  (make-irritants-condition (list->string nl)))))))

)
