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
    (export password-state? (rename (password-state <password-state>))
	    password-policy? (rename (password-policy <password-policy>))

	    single-password-policy?
	    (rename (single-password-policy <single-password-policy>))
	    single-password-policy-rule single-password-policy-state

	    (rename (compound-password-policy? password-policies?))
	    password-policies
	    ;; internal use
	    compound-password-policy-policies

	    mimimum-length-policy? make-mimimum-length-policy)
	    
    (import (rnrs)
	    (srfi :1 lists))

(define-record-type password-state)

(define-enumeration password-compliancy
  (compliant awaiting uncompliant)
  password-compliancies)
(define (merge-password-compliancy pc0 pc1)
  (cond ((and (eq? pc0 'compliant) (eq? pc1 compliant)) 'compliant)
	((or (eq? pc0 'uncompliant) (eq? pc1 uncompliant)) 'uncompliant)
	(else 'awaiting)))

(define-record-type password-policy)

(define-record-type single-password-policy
  (parent password-policy)
  ;; rule = procedure
  ;; state = record type descriptior, sub record of password-state
  (fields rule state))


(define-record-type compound-password-policy
  (parent password-policy)
  (fields policies))
(define (password-policies . policies)
  (define (normalize policy)
    (if (single-poassword-policy? policy)
	(list policy)
	(compound-password-policy-policies policy)))
  (unless (for-all password-policy? policies)
    (assertion-violation 'password-policies "Password policies are required"))
  (make-compound-password-policy (append-map normalize policies)))

;; Built-in policies
;; Minimum length
(define-record-type mimimum-length-state
  (parent password-state)
  (fields (mutable length))
  (protocol (lambda (p) (lambda () ((p) 0)))))
(define ((make-mimimum-length-policy-rule n) char state)
  (let ((cn (mimimum-length-state-length state)))
    (if (>= cn n)
	(password-compliancy compliant)
	(let ((nn (+ cn 1)))
	  (mimimum-length-state-length-set! state nn)
	  (password-compliancy awaiting)))))

(define-record-type mimimum-length-policy
  (parent single-poassword-policy)
  (protocol (lambda (p)
	      (lambda (n)
		((p (make-mimimum-length-policy-rule n)
		    mimimum-length-state))))))

)
