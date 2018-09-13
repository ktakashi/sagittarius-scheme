;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/jmespath/parser.scm - JMESPath parser
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

;; Reference
;; http://jmespath.org/specification.html

;;; Left recursion elimiated version of expression
;; # expression
;; expression ::= top-expression
;;              | index-expression
;;              | comparator-expression
;;              | sub-expression
;;              | or-expression
;;              | and-expression
;;              | pipe-expression
;; # can be toplevel
;; top-expression ::= "@" # current-node
;;     	            | "*" # wild card
;;     	            | "!" expression # not-expression
;;     	            | "(" expression ")" # paren-expression
;;                    # multi-select-list
;;     	            | "[" (expression *( "," expression) ) "]"
;;                    # multi-select-hash
;;     	            | "{" (keyval-expr *( "," keyval-expr) ) "}"
;;                    # function-expression
;;     	            | unquoted-string ( no-args / one-or-more-args )
;;     	            | "`" json-value "`"  # literal
;;     	            | "'" *raw-string-char "'" # raw-string
;;     	            | identifier
;;                  | bracket-specifier # part of index-expression
;; index-expression ::= top-expression bracket-specifier
;; comparator-expression ::= top-expression comparator expression
;; pipe-expression ::= top-expression "|" expression
;; or-expression ::= top-expression "||" expression
;; and-expression ::= top-expression "&&" expression
;; sub-expression ::= top-expression "." ( identifier
;;                                       | multi-select-list
;;                                       | multi-select-hash function-expression
;;                                       | "*" )
#!nounbound
(library (text json jmespath parser)
    (export ;;parse-jmespath

	    jmespath:unquoted-string
	    jmespath:quoted-string
	    jmespath:identifier
	    jmespath:top-expression)
    (import (rnrs)
	    (peg)
	    (srfi :14))
'dummy
;; TODO maybe 
(define ($cs s) ($satisfy (lambda (c) (char-set-contains? s c))))
(define jmespath:unquoted-string
  ($do (c ($cs (char-set-union (ucs-range->char-set #x41 (+ #x5A 1))
			       (ucs-range->char-set #x61 (+ #x7A 1))
			       (char-set #\_))))
       (c* ($many ($cs (char-set-union
			(char-set-intersection char-set:ascii
					       char-set:letter+digit)
			(char-set #\_)))))
       ($return (apply string c c*))))
(define jmespath:unescaped-char
  ($cs (char-set-union
	(ucs-range->char-set #x20 (+ #x21 1))
	(ucs-range->char-set #x23 (+ #x5B 1))
	(ucs-range->char-set #x5D (+ #x10FFFF 1)))))
(define jmespath:escaped-char
  ($seq ($eqv? #\\)
	($or ($eqv? #\")
	     ($eqv? #\\)
	     ($eqv? #\/)
	     ($do (($eqv? #\b)) ($return #\backspace))
	     ($do (($eqv? #\f)) ($return #\page))
	     ($do (($eqv? #\n)) ($return #\newline))
	     ($do (($eqv? #\r)) ($return #\return))
	     ($do (($eqv? #\t)) ($return #\tab))
	     ($do (($eqv? #\x))
		  (c* ($repeat ($cs char-set:hex-digit) 4))
		  ($return (integer->char
			    (string->number (list->string c*) 16)))))))
(define jmespath:quoted-string
  ($do (($eqv? #\"))
       (c* ($many ($or jmespath:unescaped-char jmespath:escaped-char) 1))
       (($eqv? #\"))
       ($return (list->string c*))))

(define jmespath:identifier
  ($or jmespath:unquoted-string
       jmespath:quoted-string))

(define jmespath:top-expression
  ($or ($eqv? #\*)
       ($eqv? #\@)
       jmespath:identifier
       ;;jmespath:not-expression
       ;;jmespath:paren-expression
       ;;jmespath:multi-select-list
       ;;jmespath:multi-select-hash
       ;;jmespath:literal
       ;;jmespath:function-expression
       ;;jmespath:raw-string
       ))

(define (jmespath:pipe-expression)
  ($do (e jmespath:top-expression)
       (($eqv? #\|))
       (e2 jmespath:expression)
       ($return `(pipe ,e ,e2))))
(define (jmespath:or-expression)
  ($do (e jmespath:top-expression)
       (($eqv? #\|)) (($eqv? #\|))
       (e2 jmespath:expression)
       ($return `(or ,e ,e2))))
(define (jmespath:and-expression)
  ($do (e jmespath:top-expression)
       (($eqv? #\&)) (($eqv? #\&))
       (e2 jmespath:expression)
       ($return `(and ,e ,e2))))

(define (jmespath:expression)
  ($or ;;jmespath:sub-expression
       ;;jmespath:index-expression
       ;;jmespath:comparator-expression
       (jmespath:pipe-expression) ;; pipe must be before the or
       (jmespath:or-expression)
       (jmespath:and-expression)
       jmespath:top-expression ;; this must be the last
       ))
)
