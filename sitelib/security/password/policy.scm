;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/password/policy.scm - Password policy
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
(library (security password policy)
    (export password-policy?

	    single-password-policy?
	    (rename (single-password-policy <single-password-policy>))
	    single-password-policy-rule

	    (rename (compound-password-policy? password-policies?))
	    password-policies
	    ;; internal use
	    compound-password-policy-policies

	    length-policy? make-length-policy
	    length-policy-length

	    character-policy? make-character-policy
	    character-policy-at-least character-policy-char-set)
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets))

(define-record-type password-policy)

(define-record-type single-password-policy
  (parent password-policy)
  ;; rule: () -> values { accumlator, result-retriever }
  (fields rule))


(define-record-type compound-password-policy
  (parent password-policy)
  (fields policies))
(define (password-policies . policies)
  (define (normalize policy)
    (if (single-password-policy? policy)
	(list policy)
	(compound-password-policy-policies policy)))
  (unless (for-all password-policy? policies)
    (assertion-violation 'password-policies "Password policies are required"))
  (make-compound-password-policy (append-map normalize policies)))

;; Built-in policies
;; Password length
(define ((make-length-policy-rule len))
  (let ((n 0))
    (define (length-accumlator s) (set! n (+ n 1)))
    (values length-accumlator (lambda () (>= n len)))))

(define-record-type length-policy
  (parent single-password-policy)
  (fields length)
  (protocol (lambda (p)
	      (lambda (n)
		((p (make-length-policy-rule n)) n)))))

;; Character
(define ((make-character-policy-rule cs at-least))
  (let ((c 0))
    (define (char-set-accumulator s)
      ;; The given `s` is a string contains one character however, one
      ;; character doesn't mean one code point in Unicode world. Say,
      ;; an emoji may contain multiple code points but must be
      ;; considered a character.
      ;; Should we use string-any here?
      (when (string-every (lambda (c) (char-set-contains? cs c)) s)
	(set! c (+ c 1))))
    (values char-set-accumulator (lambda () (>= c at-least)))))
    
(define-record-type character-policy
  (parent single-password-policy)
  (fields char-set at-least)
  (protocol (lambda (p)
	      (lambda (cs at-least)
		((p (make-character-policy-rule cs at-least))
		 cs at-least)))))

)
