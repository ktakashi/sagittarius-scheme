;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/escaping - Utilities
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (text markdown parser escaping)
    (export escaping:normalize-label
	    escaping:unescape
	    escaping:resolve-entity)
    (import (rnrs)
	    (srfi :13 strings)
	    (srfi :115 regexp)
	    (text markdown parser parsing)
	    (text xml entities))

(define (escaping:normalize-label label)
  (let ((s (string-upcase (string-downcase (string-trim-both label)))))
    (regexp-replace-all (rx (+ space)) s " ")))

(define entity/escape
  (rx (w/nocase (or ($ #\\ any)
		    ($ #\& (or (: "#x" (** 1 6 hex-digit))
			       (w/ascii (** 1 7 num))
			       (w/ascii (** 1 31 alnum)))
		       #\;)))))
(define (escaping:unescape str)
  (define (replace-all str)
    (regexp-replace-all entity/escape str
     (lambda (m)
       (let ((s (regexp-match-submatch m 0)))
	 (cond ((and (= (string-length s) 2) (eq? (string-ref s 0) #\\))
		(let ((c (string-ref s 1)))
		  (if (parsing:escapable? c)
		      (string c)
		      s)))
	       (else (escaping:resolve-entity s)))))))
  (cond ((string-any (lambda (c) (or (eqv? c #\&) (eqv? c #\\))) str)
	 (replace-all str))
	(else str)))

(define (escaping:resolve-entity str)
  (if (and (string-prefix? "&" str) (string-suffix? ";" str))
      (let ((v (substring str 1 (- (string-length str) 1))))
	(cond ((string-prefix? "#" v)
	       (let ((n (if (memv (string-ref v 1) '(#\x #\X))
			    (string->number v)
			    (let ((v (substring v 1 (string-length v))))
			      (string->number v)))))
		 (or (and n
			  (if (zero? n)
			      "\xFFFD;"
			      (string (integer->char n))))
		     "\xFFFD;")))
	      ((xml-entity-name->char v) => list->string)
	      (else str)))
      str))

)
