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
    (import (core)
	    (sagittarius)
	    (sagittarius threads))
  (load-dynamic-library "sagittarius--process")

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
    (let ((process (make-process name args)))
      (cond ((and call? stdout)
	     (process-call process)
	     ;; well user might not want to create threads.
	     (reader process stdout (if stderr stderr stdout) transcoder)
	     process)
	    (call?
	     (process-call process)
	     process)
	    (else
	     ;; on POSIX envirionment, pipe is not created yet, so we need to
	     ;; emulate process-run like this.
	     (process-call process)
	     (reader process stdout (if stderr stderr stdout) transcoder)
	     (process-wait process)))))

  ;; handle both stdout and stderr
  (define (async-process-read process stdout stderr transcoder)
    (define (pipe-read in out reader converter)
      (let loop ((r (reader in)))
	(unless (eof-object? r)
	  (display (converter r) out)
	  (loop (reader in)))))
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
