;;; -*- Scheme -*-
;;;
;;; process.scm - process library
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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
(load-dynamic-library "sagittarius--process")
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
	    )
    (import (core)
	    (sagittarius threads)
	    (sagittarius process impl))

  (define (create-process name args :key (stdout #f)
			                 (stderr #f)
					 (call? #t)
					 (reader async-process-read))
    (when (and (not call?) (not (output-port? stdout)))
      (assertion-violation 
       'create-process
       "keyword argument :stdout must be output port, when :call? is #f"
       stdout call?))
    (let ((process (make-process name args)))
      (cond ((and call? stdout)
	     (reader process stdout (if stderr stderr stdout))
	     (process-call process))
	    (call?
	     (process-call process))
	    (else
	     (reader process stdout (if stderr stderr stdout))
	     (process-run process)))
      process))

  ;; handle both output and 
  (define (async-process-read process stdout stderr)
    (let ((out-thread (make-thread
		       (lambda ()
			 (let ((in (process-output-port process)))
			   (let loop ((r (get-u8 in)))
			     (unless (eof-object? r)
			       (display (integer->char r) stdout)
			       (loop (get-u8 in))))))))
	  (err-thread (make-thread
		       (lambda ()
			 (let ((in (process-error-port process)))
			   (let loop ((r (get-u8 in)))
			     (unless (eof-object? r)
			       (display (integer->char r) stderr)
			       (loop (get-u8 in)))))))))
      (thread-start! out-thread)
      (thread-start! err-thread)))

  (define (run name . args)
    (create-process name
		    args
		    :stdout (current-output-port)
		    :stderr (current-error-port)
		    :call? #f))

  (define (call name . args)
    (create-process name
		    args
		    :stdout (current-output-port)
		    :stderr (current-error-port)))

)
