;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; core/program.scm - starting poinrt of Sagittarius kernel 
;;;  
;;;   Copyright (c) 2010-2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (core program)
    (export start command-line)
    (import (core)
	    (core base)
	    (sagittarius)
	    (sagittarius vm))

  (define command-line (make-core-parameter '()))

  (define *r6rs-read-context* (make-read-context-for-load))
  (define (program-r6rs file/port)
    (let ((port (cond ((port? file/port) file/port)
		      ((string? file/port)
		       (open-file-input-port file/port #f 'block
					     (*current-load-transcoder*)))
		      (else
		       (error 'progam "Strict R6RS mode doesn't have REPL")))))
      (apply-directive! port 'r6rs *r6rs-read-context*)
      (let loop ((e (read port)) (r '()))
	(if (eof-object? e)
	    (eval `(program . ,(reverse! r)) (environment '(r6rs-script)))
	    (loop (read port) (cons e r))))))
  
  (define (script file/port prompt opt)
    (define (import-it lib) (eval `(import ,lib) (current-library)))
    (define (repl)
      (define interactive (find-library '(sagittarius interactive) #f))  
      (if interactive
	  (case prompt
	    ((r7rs)
	     (eval '(*import-libraries* '((scheme base))) interactive)
	     (eval '(current-prompter (lambda () (display "sash[r7rs]> ")))
		   interactive)
	     (eval `(read-eval-print-loop) interactive))
	    (else (eval `(read-eval-print-loop) interactive)))
	  (begin
	    (display "(sagittarius interactive) is not located on the \
                      loadpath. Add -L option to indicate it"
		     (current-error-port))
	    (newline (current-error-port))
	    (exit -1))))
    (define (exit/repl exit-code)
      (if (assq :interactive? opt)
	  (repl)
	  (exit exit-code)))
    
    (define (load-r7rs file/port)
      (define (ensure-port file/port)
	(if (port? file/port)
	    file/port
	    (open-file-input-port file/port
				  #f 'block (*current-load-transcoder*))))
      (let ((in (ensure-port file/port)))
	(apply-directive! in 'r7rs *r6rs-read-context*)
	(load-from-port in)))
    
    (cond ((assq :preimports opt) =>
	   (lambda (preimports) (for-each import-it (cdr preimports)))))
    (cond ((assq :expressions opt) =>
	   (lambda (expr)
	     (call-with-port (open-string-input-port (cdr expr))
	       load-from-port))))
      
    (if file/port
	(let ((exit-code (cond ((eq? prompt 'r7rs) (load-r7rs file/port))
			       ((port? file/port) (load-from-port file/port))
			       (else (load file/port)))))
	  (or (and-let* ((main? (assq :main? opt))
			 ( (cdr main?) )
			 (main (find-binding (current-library) 'main #f))
			 (r (eval `(main ',(command-line))
				  (current-library))))
		(if (fixnum? r) (exit/repl r) (exit/repl exit-code)))
	      (exit/repl exit-code)))
	(repl)))
  
  (define (start file/port args opt)
    (command-line args)
    (case (cond ((assq :standard opt) => cdr) (else #f))
      ((6) (program-r6rs file/port))
      ((7) (script file/port 'r7rs opt))
      (else (script file/port #f opt))))
)
