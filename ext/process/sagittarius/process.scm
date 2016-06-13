;;; -*- Scheme -*-
;;;
;;; process.scm - process library
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
	    process-kill
	    process-active?
	    <process>

	    ;; user level APIs
	    run
	    call
	    create-process

	    ;; extra
	    getpid pid->process

	    ;; IPC
	    shared-memory?
	    open-shared-memory
	    close-shared-memory
	    shared-memory->bytevector

	    ;; reader
	    sync-process-read
	    async-process-read
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
     (directory :init-keyword :directory)
     (pid    :init-keyword :pid)))

  (define (process? o) (is-a? o <process>))
  (define (make-process name args)
    (make <process> :name name :args args))
  (define (process-call p . opts)
    (unless (slot-ref p 'name)
      (assertion-violation 'process-call "attached process can't be called"
			   p))
    (let-values (((pid input output error)
		  (apply sys-process-call (slot-ref p 'name) (slot-ref p 'args)
			 opts)))
      (slot-set! p 'input  input)
      (slot-set! p 'output output)
      (slot-set! p 'error  error)
      (slot-set! p 'pid pid)
      pid))
  (define (process-wait p :key (timeout #f))
    (if (slot-bound? p 'pid)
	(sys-process-wait (slot-ref p 'pid) :timeout timeout)
	(error 'process-wait "pid is not set" p)))
  (define (process-run p . opts)
    (apply process-call p opts)
    (process-wait p))
  (define (process-kill p :key (children? #f))
    (sys-process-kill (slot-ref p 'pid) :children? children?))
  (define (process-active? p)
    (sys-process-active? (slot-ref p 'pid)))

  (define (pid->process pid)
    ;; attached process.
    (make <process> :name #f :args '() :pid (%pid->sys-process pid)
	  :input #f :output #f :error #f :directory #f))

  ;; FIXME
  ;; This API is getting inconsistent. The decision if the process should
  ;; be waited or not is done by reader. 
  (define (create-process name args :key (stdout #f)
			                 (stderr #f)
					 (call? #t)
					 (reader #f)
					 (transcoder #f)
					 (directory #f)
					 (detach? #f))
    ;; Some of my script uses create-process with :call? #f
    ;; in that case it expects to wait process with reading all
    ;; output from process. so just switch it here.
    (define %reader
      (cond ((procedure? reader) reader)
	    (call? async-process-read)
	    (else sync-process-read)))
    (let-values (((pid input output error)
		  (sys-process-call name args
				    :directory directory
				    :detach? detach?)))
      (let ((process (make <process> :name name :args args
			   :input input :output output :error error
			   :pid pid :directory (or directory
						   (current-directory)))))
	(if (and (output-port? stdout) (procedure? %reader))
	    (%reader process stdout (if stderr stderr stdout) transcoder)
	    process))))
	    

  ;; handle both stdout and stderr
  (define (async-process-read process stdout stderr transcoder)
    (define (pipe-read in out reader writer)
      (let loop ()
	(let ((r (reader in)))
	  (cond ((eof-object? r) (flush-output-port out) (close-input-port in))
		(else (writer out r) (loop))))))
    (define (make-task in)
      (lambda ()
	(if transcoder
	    (pipe-read (transcoded-port in transcoder)
		       stdout
		       get-char
		       put-char)
	    (pipe-read in stdout get-u8 put-u8))))
    (thread-start! (make-thread (make-task (process-output-port process))))
    (thread-start! (make-thread (make-task (process-error-port process))))
    process)

  ;; handle both stdout and stderr
  ;; this reader blocks
  ;; FIXME I want to make sure this procedure reads all outputs and error
  ;;       outputs, but this may truncate some of the outputs.
  (define (sync-process-read process stdout stderr transcoder)
    (process-wait (async-process-read process stdout stderr transcoder)))

  (define (run name . args)
    (create-process name
		    args
		    :reader sync-process-read
		    :stdout (current-output-port)
		    :stderr (current-error-port)
		    :transcoder (native-transcoder)))

  (define (call name . args)
    (create-process name
		    args
		    :stdout (current-output-port)
		    :stderr (current-error-port)
		    :transcoder (native-transcoder)))

)
