;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ftp/messages.scm - FTP protocol messages.
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

;; to use let-syntax without creating scope
;; should i make compiler accept this without #!r6rs?
;; i'm tired of this restriction...
#!r6rs
(library (rfc ftp messages)
    (export *response-messages*
	    ;; predicates for client
	    positive-preliminary?
	    positive-completion?
	    positive-intermediate?
	    transient-negative-completion?
	    permanent-negative-completion?
	    syntax-message?
	    information-message?
	    connection-message?
	    authentication-message?
	    unspecified-message?
	    file-system-message?
	    status->message)
    (import (rnrs) (sagittarius))

  (let-syntax ((define-message-predicate
		 (lambda (x)
		   (syntax-case x ()
		     ((k name x pos)
		      (let* ((xx (syntax->datum #'x))
			     (p  (syntax->datum #'pos))
			     (n  (string-ref (number->string xx) 0)))
			(with-syntax ((n (datum->syntax #'k n)))
			  #'(define (name response)
			      (char=? n (string-ref response pos))))))))))
    (define-message-predicate positive-preliminary? 1 0)
    (define-message-predicate positive-completion? 2 0)
    (define-message-predicate positive-intermediate? 3 0)
    (define-message-predicate transient-negative-completion? 4 0)
    (define-message-predicate permanent-negative-completion? 5 0)
    (define-message-predicate syntax-message? 0 1)
    (define-message-predicate information-message? 1 1)
    (define-message-predicate connection-message? 2 1)
    (define-message-predicate authentication-message? 3 1)
    (define-message-predicate unspecified-message? 4 1)
    (define-message-predicate file-system-message? 5 1))

  (define (status->message status)
    (let ((n (string->number status)))
      (cond ((assv n *response-messages*) => cadr)
	    (else (format "unknown status ~a" status)))))

  ;; NOTE for future server support, 2xx message contains ~s for
  ;; format procedure
  (define-constant *response-messages*
    '((110 "Restart marker reply.")
      (120 "Service ready in ~a minutes.")
      (125 "Data connection already open; transfer starting.")
      (150 "File status okay; about to open data connection.")
      (200 "Command okay.")
      (202 "Command not implemented, superfluous at this site.")
      (211 "System status, or system help reply.")
      (212 "Directory status.")
      (213 "File status.")
      (214 "Help message.")
      (215 "~a system type.")
      (220 "Service ready for new user.")
      (221 "Service closing control connection.")
      (232 "User logged in, authorized by security data exchange.")
      (234 "Security data exchange complete.")
      (225 "Data connection open; no transfer in progress.")
      (226 "Closing data connection.")
      (227 "Entering Passive Mode (h1,h2,h3,h4,p1,p2).")
      (230 "User logged in, proceed.")
      (250 "Requested file action okay, completed.")
      (257 "~s created.")
      (331 "User name okay, need password.")
      (332 "Need account for login.")
      (350 "Requested file action pending further information.")
      (421 "Service not available, closing control connection.")
      (425 "Can't open data connection.")
      (426 "Connection closed; transfer aborted.")
      (450 "Requested file action not taken. File unavailable (e.g., file busy).")
      (451 "Requested action aborted: local error in processing.")
      (452 "Requested action not taken. Insufficient storage space in system.")
      (500 "Syntax error, command unrecognized.")
      (501 "Syntax error in parameters or arguments.")
      (502 "Command not implemented.")
      (503 "Bad sequence of commands.")
      (504 "Command not implemented for that parameter.")
      (530 "Not logged in.")
      (532 "Need account for storing files.")
      (550 "Requested action not taken. File unavailable (e.g., file not found, no access).")
      (551 "Requested action aborted: page type unknown.")
      (552 "Requested file action aborted. Exceeded storage allocation.")
      (553 "Requested action not taken. File name not allowed.")
      ))

  
)