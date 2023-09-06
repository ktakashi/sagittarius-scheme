;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/password.scm - Password policy
;;;
;;;   Copyright (c) 2023  Takashi Kato  <ktakashi@ymail.com>
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
(library (security password)
    (export password-policy? password-policies
	    password-policy-compliant?

	    length-policy? make-length-policy password-policy-length
	    character-policy? make-character-policy password-policy-char-set
	    ;; predefined policy creators
	    make-lower-case-policy
	    make-upper-case-policy
	    make-symbol-policy
	    make-digit-policy

	    password-policy->generator
	    generate-password
	    *password-policy:default-length*
	    *password-policy:default-char-set*

	    password-policy->predicate
	    password-policy-entropy
	    )
    (import (rnrs)
	    (security password policy)
	    (security password checker)
	    (security password generator)
	    (srfi :14 char-sets))

(define (->ascii-char-set cs) (char-set-intersection char-set:ascii cs))
(define char-set:lower-case/ascii (->ascii-char-set char-set:lower-case))
(define char-set:upper-case/ascii (->ascii-char-set char-set:upper-case))
(define char-set:symbol/ascii (->ascii-char-set char-set:symbol))
(define char-set:digit/ascii (->ascii-char-set char-set:digit))

(define (make-lower-case-policy at-least)
  (make-character-policy char-set:lower-case/ascii at-least))
(define (make-upper-case-policy at-least)
  (make-character-policy char-set:upper-case/ascii at-least))
(define (make-symbol-policy at-least)
  (make-character-policy char-set:symbol/ascii at-least))
(define (make-digit-policy at-least)
  (make-character-policy char-set:digit/ascii at-least))

(define (password-policy-entropy policy)
  (if (single-password-policy? policy)
      (password-policy-entropy (password-policies policy))
      (let ((l (password-policy-length policy))
	    (cs (password-policy-char-set policy)))
	(log (expt (char-set-size cs) l) 2))))

;; utility
(define (password-policy->predicate policy)
  (lambda (p) (password-policy-compliant? policy p)))

)
