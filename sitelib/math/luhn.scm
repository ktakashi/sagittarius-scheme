;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; math/luhn.scm - Luhn checksum.
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

;; https://en.wikipedia.org/wiki/Luhn_algorithm
(library (math luhn)
    (export luhn-valid?
	    luhn-calculate
	    *luhn-converter*)
    (import (rnrs)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))

  (define identifier-char-set
    (string->char-set "0123456789ABCDEFGHIJKLMNOPQRSTUVYWXZ_"))
  (define *luhn-converter* 
    (make-parameter 
     ;; we accept identifiers as well like the following link
     ;; https://wiki.openmrs.org/display/docs/Check+Digit+Algorithm
     (lambda (s/i) 
       (map (lambda (c) 
	      (unless (char-set-contains? identifier-char-set c)
		(assertion-violation 'luhn-checksum "invalid character" c))
	      (- (char->integer c) 48))
	    (string->list 
	     (if (string? s/i) 
		 (string-upcase s/i)
		 (number->string s/i)))))))

  ;; we don't export this
  (define (luhn-checksum s calc?)
    (define (checksum n*)
      (define (sum-them n*)
	(let loop ((i 0) (n* n*) (acc 0))
	  (if (null? n*)
	      acc
	      (let ((n (car n*)))
		(loop (+ i 1)
		      (cdr n*)
		      (+ acc (if (even? i) (- (* n 2) (* (div n 5) 9)) n)))))))
      (let ((sum (+ (abs (sum-them n*)) 10)))
	(mod (- 10 (mod sum 10)) 10)))
    ;; loop through the converted digits from right to left
    (let ((n* (reverse ((*luhn-converter*) s))))
      ;; if it's not calculation, then we need to skip the last char
      (values (if calc? #f (car n*))
	      (checksum (if calc? n* (cdr n*))))))

  (define (luhn-valid? s/i)
    (let-values (((checksum calculated) (luhn-checksum s/i #f)))
      (= checksum calculated)))

  (define (luhn-calculate partial-num) 
    (let-values (((ignore checksum) (luhn-checksum partial-num #t)))
      checksum))

  )
