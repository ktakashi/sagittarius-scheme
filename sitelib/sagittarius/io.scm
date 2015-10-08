;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/io - Extra I/O
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

(library (sagittarius io)
    (export call-with-input-string
	    call-with-output-string
	    with-input-from-string
	    with-input-from-port
	    with-output-to-string
	    with-output-to-port
	    with-error-to-port

	    <custom-binary-input-port>
	    <custom-textual-input-port>
	    <custom-binary-output-port>
	    <custom-textual-output-port>
	    <custom-binary-input/output-port>
	    <custom-textual-input/output-port>
	    )
    (import (rnrs)
	    (sagittarius)
	    (clos user)
	    (srfi :39 parameters)
	    ;; for internal port classes
	    (sagittarius clos))

  (define (call-with-input-string str proc)
    (proc (open-string-input-port str)))

  (define (call-with-output-string proc)
    (let ((port (open-output-string)))
      (proc port)
      (get-output-string port)))

  (define (with-input-from-string str thunk)
    (with-input-from-port (open-input-string str) thunk))

  (define (with-input-from-port port thunk)
    (parameterize ((current-input-port port))
      (thunk)))

  (define (with-output-to-string thunk)
    (let ((port (open-output-string)))
      (with-output-to-port port thunk)
      (get-output-string port)))

  (define (with-output-to-port port thunk)
    (parameterize ((current-output-port port))
      (thunk)))

  (define (with-error-to-port port thunk)
    (parameterize ((current-error-port port))
      (thunk)))

  ;; interface
  (define-class <input/output-port> (<input-port> <output-port>) ())

  (define-syntax define-ports
    (lambda (x)
      (define (ports k name)
	(let* ((s (symbol->string (syntax->datum name)))
	       (b (string->symbol (format "<custom-binary-~a-port>" s)))
	       (t (string->symbol (format "<custom-textual-~a-port>" s)))
	       (i (string->symbol (format "<~a-port>" s))))
	  (datum->syntax k (list b t i))))
      (syntax-case x ()
	((k type)
	 (with-syntax (((b t i) (ports #'k #'type)))
	   #'(begin
	       (define-class b (<custom-binary-port> i) ())
	       (define-class t (<custom-textual-port> i) ())))))))
  (define-ports input)
  (define-ports output)
  (define-ports input/output)

)
