;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport/version.scm - SSH2 protocol version exchange
;;;  
;;;   Copyright (c) 2010-2025  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/regex
#!nounbound
(library (rfc ssh transport version)
    (export ssh-version-exchange
	    *ssh-version-string*)
    (import (rnrs)
	    (srfi :39 parameters)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius object)
	    (sagittarius socket))

(define-constant +default-version-string+ 
  (string-append "SSH-2.0-Sagittarius_" (sagittarius-version)))
(define *ssh-version-string* (make-parameter +default-version-string+))


;; RFC 4253 defines identification string like the followings;
;;  SSH-protoversion-softwareversion SP comments CR LF
;; Thus 'comments' must be a part of identification string
;; to be used as a part of MAC.
(define (ssh-version-exchange transport)
  (define in/out (~ transport 'socket))
  ;; first send
  (let ((version (*ssh-version-string*)))
    (set! (~ transport 'host-version) version)
    (write-line in/out version))
  (let loop ((vs (read-ascii-line transport)))
    (cond ((zero? (string-length vs))
	   (socket-shutdown in/out SHUT_RDWR)
	   (socket-close in/out)
	   (error 'version-exchange "no version string"))
	  ((#/(SSH-2.0-[\w.-]+)\s*/ vs)
	   (set! (~ transport 'peer-version) vs))
	  (else (loop (read-ascii-line transport))))))

;; utility
(define (read-ascii-line transport)
  (define in (~ transport 'socket))
  (define buf (~ transport 'read-buffer))
  (let-values (((out e) (open-string-output-port)))
    (let loop ((r (socket-recv! in buf 0 1)))
      (unless (zero? r)
	(case (bytevector-u8-ref buf 0)
	  ;; version string must end CR LF however
	  ;; OpenSSH 4.3 returns only LF...
	  ((#x0D) (socket-recv! in buf 0 1))
	  ((#x0A))
	  (else => (lambda (u8)
		     (put-char out (integer->char u8))
		     (loop (socket-recv! in buf 0 1)))))))
    (e)))

;; write line with CR LF
(define (write-line out str)
  (socket-send out (string->utf8 str))
  (socket-send out #vu8(#x0D #x0A)))
)

