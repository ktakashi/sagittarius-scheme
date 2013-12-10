;;; -*- Scheme -*-
;;;
;;; process.scm - process library
;;;  
;;;   Copyright (c) 2010-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; process library
;; exported methods:
;;  make-process -- creates a process
;;  process?     -- predicate
;;  process-run  -- run process. this waits for the process
;;  process-call -- run process. this does not wait for the process.
;;  process-wait -- wait for the process.
;;  run          -- convenient method
;;  create-process -- ditto
(library (sagittarius process)
    (export make-process
	    process?
	    process-input-port
	    process-output-port
	    process-error-port
	    process-run
	    process-call
	    process-wait
	    <process>

	    ;; user level APIs
	    run
	    call
	    create-process

	    ;; extra
	    getpid
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius dynamic-module)
	    (sagittarius threads))
  (load-dynamic-module "sagittarius--process")

  (define-class <process> ()
    ((name :init-keyword :name)
     (args :init-keyword :args)
     (input  :init-keyword :input  :reader process-input-port)
     (output :init-keyword :output :reader process-output-port)
     (error  :init-keyword :error  :reader process-error-port)
     (pid    :init-keyword :pid)))

  (define (process? o) (is-a? o <process>))
  (define (make-process name args)
    (make <process> :name name :args args))
  (define (process-call p)
    (let-values (((pid input output error)
		  (sys-process-call (slot-ref p 'name) (slot-ref p 'args))))
      (slot-set! p 'input  input)
      (slot-set! p 'output output)
      (slot-set! p 'error  error)
      (slot-set! p 'pid pid)
      pid))
  (define (process-wait p)
    (sys-process-wait (slot-ref p 'pid)))
  (define (process-run p)
    (process-call p)
    (process-wait p))

  (define (create-process name args :key (stdout #f)
			                 (stderr #f)
					 (call? #t)
					 (reader async-process-read)
					 (transcoder #f))
    (when (and (not call?) (not (output-port? stdout)))
      (assertion-violation 
       'create-process
       "keyword argument :stdout must be output port, when :call? is #f"
       stdout call?))
    (let-values (((pid input output error) (sys-process-call name args)))
      (let ((process (make <process> :name name :args args
			   :input input :output output :error error
			   :pid pid)))
	(cond ((and call? stdout)
	       (reader process stdout (if stderr stderr stdout) transcoder)
	       process)
	      (call? process)
	      (else
	       (reader process stdout (if stderr stderr stdout) transcoder)
	       (sys-process-wait pid)
	       (let loop ()
		 (when (port-ready? output) (sys-nanosleep 1000) (loop))))))))

  ;; handle both stdout and stderr
  (define (async-process-read process stdout stderr transcoder)
    (define (pipe-read in out reader converter)
      (let loop ()
	(let ((r (reader in)))
	  (cond ((eof-object? r) (close-input-port in))
		(else
		 (display (converter r) out)
		 (loop))))))
    (let ((out-thread (make-thread
		       (lambda ()
			 (let ((in (process-output-port process)))
			   (if transcoder
			       (pipe-read (transcoded-port in transcoder)
					  stdout
					  get-char
					  (lambda (x) x))
			       (pipe-read in stdout get-u8 integer->char))))))
	  (err-thread (make-thread
		       (lambda ()
			 (let ((in (process-error-port process)))
			   (if transcoder
			       (pipe-read (transcoded-port in transcoder)
					  stderr
					  get-char
					  (lambda (x) x))
			       (pipe-read in stderr get-u8 integer->char)))))))
      (thread-start! out-thread)
      (thread-start! err-thread)))

  (define (run name . args)
    (create-process name
		    args
		    :stdout (current-output-port)
		    :stderr (current-error-port)
		    :transcoder (native-transcoder)
		    :call? #f))

  (define (call name . args)
    (create-process name
		    args
		    :stdout (current-output-port)
		    :stderr (current-error-port)
		    :transcoder (native-transcoder)))

)
