;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/smtp/format.scm - SMTP format check
;;;  
;;;   Copyright (c) 2010-2017  Takashi Kato  <ktakashi@ymail.com>
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

;; references
;;   - https://tools.ietf.org/html/rfc5321
;;   - https://tools.ietf.org/html/rfc5322
(library (rfc smtp format)
    (export smtp-valid-address?)
    (import (rnrs)
	    (srfi :2)
	    (srfi :13)
	    (srfi :14)
	    (rfc :5322))

;; https://tools.ietf.org/html/rfc5321#section-2.3.11
;; local-part@domain
;; this is a bit too naive but for now
(define (smtp-valid-address? s)
  (define in (if (input-port? s) s (open-string-input-port s)))
  (let ((local (rfc5322-next-token in)))
    (and (eqv? #\@ (get-char in))
	 ;; this doesn't handle domain-literal properly...
	 (let ((domain (rfc5322-next-token in)))
	   (and (string? domain)
		(check-length local domain)
		(eof-object? (get-char in)))))))

;; TODO add mailbox format
;; mailbox format is more strict than address
;; though, not sure if we need it...
;; (define (smtp-valid-mailbox? s))

(define (check-length local-part domain)
  (define llen (string-length local-part))
  (define dlen (string-length domain))
  (and (<= llen 64)
       (<= dlen 255)
       ;; exclude '@'
       (< (+ llen dlen) 253)))
)
