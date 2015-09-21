;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/smtp/commands.scm - SMTP commands
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

;; reference https://tools.ietf.org/html/rfc5321#section-4
(library (rfc smtp commands)
    (export smtp-helo
	    smtp-ehlo
	    smtp-mail
	    smtp-rcpt
	    smtp-data
	    smtp-rset
	    smtp-vrfy
	    smtp-expn
	    smtp-help
	    smtp-noop
	    smtp-quit

	    smtp-send-data

	    smtp-recv
	    )
    (import (rnrs)
	    (rfc smtp conditions)
	    (srfi :1)
	    (srfi :13))

;; port must have utf-8-codec or latin-1-codec to make this work
;; in binary world.
;; NB: the port must be associated with 'lf/none transcoder.
;;     See R6RS standard libraries section 8.4.2 Transcoders
(define (send-command port com . param)
  (put-string port com)
  (for-each (lambda (p) (put-char port #\space) (put-string port p)) param)
  ;; for output port, lf and none are the same, thus this will emit
  ;; 0x0d 0x0a on utf-8 codec
  (put-string port "\r\n")
  (flush-output-port port))

;; HELO and EHLO
(define (smtp-helo port domain) (send-command port "HELO" domain))
(define (smtp-ehlo port domain) (send-command port "EHLO" domain))

;; MAIL
(define (smtp-mail port reserved-path . parameter)
  (apply send-command port "MAIL FROM:" reserved-path parameter))

;; RCPT
(define (smtp-rcpt port reserved-path . parameter)
  (apply send-command port "RCPT TO:" reserved-path parameter))

;; DATA
(define (smtp-data port) (send-command port "DATA"))
;; Should we get end mark from the response of DATA command?
;; though, that's kinda silly isn't it?
(define (smtp-send-data port in)
  (let loop ((line (get-line in)))
    (unless (eof-object? line)
      ;; OK we only use \r\n as line separator no matter what.
      ;; may sounds a bit too brute but hey.
      (let ((line (string-trim-right line)))
	(when (string=? line ".") (put-char port #\.))
	(put-string port line)
	(put-string port "\r\n")
	(loop (get-line in)))))
  (put-string port "\r\n.\r\n"))

;; RSET
(define (smtp-rset port) (send-command port "RSET"))

;; VRFY
(define (smtp-vrfy port string) (send-command port "VRFY" string))
;; EXPN
(define (smtp-expn port string) (send-command port "EXPN" string))
;; HELP
(define (smtp-help port . string) (apply send-command port "HELP" string))
;; NOOP
(define (smtp-noop port . string) (apply send-command port "NOOP" string))
;; QUIT
(define (smtp-quit port) (send-command port "QUIT"))

;; receive server response
(define (smtp-recv in)
  (define (parse-line line)
    (let ((continue? (and (> (string-length line) 3)
			  (eqv? (string-ref line 3) #\-))))
      (values (string->number (substring line 0 3))
	      continue?
	      ;; skip the 3rd one
	      (or (and (> (string-length line) 3)
		       (substring line 4 (string-length line)))
		  ""))))
  (let loop ((line (string-trim-right (get-line in))) (saved #f)  (r '()))
    (let-values (((status continue? content) (parse-line line)))
      (if continue?
	  (if (and saved (not (= status saved)))
	      (raise (condition
		      (list (make-smtp-invalid-response-error)
			    (make-who-condition 'smtp-recv)
			    (make-message-condition "Invalid response")
			    (make-irritants-condition saved status))))
	      (loop (string-trim-right (get-line in)) 
		    status 
		    (cons* "\n" content r)))
	  (values status (string-concatenate-reverse (cons content r)))))))

)
