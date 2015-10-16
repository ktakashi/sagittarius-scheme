;;; -*- mode:scheme; coding: utf-8 -*-
;;;
;;; sagittarius/stty.scm - STTY
;;;
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

;; the API names are taken from Chibi Scheme
(library (sagittarius stty)
    (export stty with-stty with-raw-io)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius dynamic-module))
  (load-dynamic-module "sagittarius--stty")

(define-constant +mode-map+
  `((icanon . ,SG_CONSOLE_CANON)
    (echo   . ,SG_CONSOLE_ECHO)))

(define (stty port setting)
  (let ((base (get-console-mode port)))
    (if base
	;; might be a bit too naive
	(let loop ((attr base) (setting setting) (remove #f))
	  (cond ((null? setting) (set-console-mode! port attr))
		((and (pair? (car setting))
		      (eq? (cadr setting) 'not))
		 (loop (loop attr (cddr setting) #t) (cdr setting) remove))
		((eq? (car setting) 'not) (loop attr (cdr setting) #t))
		((assq (car setting) +mode-map+) =>
		 (lambda (slot)
		   (let* ((name (cdr slot))
			  (flag (%get-console-mode-flag name)))
		     (loop (if remove
			       (or (and (not (zero? (bitwise-and attr)))
					(bitwise-xor attr flag))
				   flag)
			       (bitwise-ior attr flag))
			   (cdr setting) remove))))
		(else (loop attr (cdr setting) remove))))
	#f)))

(define (with-stty setting thunk :optional (port (current-input-port)))
  (cond ((get-console-mode port) =>
	 (lambda (attr)
	   (dynamic-wind
	       (lambda () (stty port setting))
	       thunk
	       (lambda () (set-console-mode! port attr)))))
	(else (thunk))))

(define (with-raw-io port thunk)
  (with-stty '(not icanon echo) thunk port))

)
  