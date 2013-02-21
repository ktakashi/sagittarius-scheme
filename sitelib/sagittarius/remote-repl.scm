;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/remote-repl.scm - remote REPL library.
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

(library (sagittarius remote-repl)
    (export connect-remote-repl
	    make-remote-repl
	    make-username&password-authenticate)
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (sagittarius interactive)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius socket)
	    (sagittarius vm)
	    (rfc tls)
	    (srfi :18 multithreading)
	    (srfi :26 cut)
	    (srfi :39 parameters)
	    (math)
	    (pp))
  (define interaction-environment (find-library 'user #t))
  ;; to keep connection we need to do some low level operations
  (define-constant +chunk-size+ 512)

  (define (send-datum socket datum)
    (let1 bv (string->utf8 (format "~a" datum))
      (if (tls-socket? socket)
	  (tls-socket-send socket bv)
	  (socket-send socket bv))))
  (define (recv-datum socket)
    (define (recv-bv)
      (call-with-bytevector-output-port
       (lambda (out)
	 (let loop ()
	   (let1 bv (if (tls-socket? socket)
			(tls-socket-recv socket +chunk-size+)
			(socket-recv socket +chunk-size+))
	     (put-bytevector out bv)
	     (unless (< (bytevector-length bv) +chunk-size+)
	       (loop)))))))
    (let1 bv (recv-bv)
      (open-string-input-port (utf8->string bv))))

  ;; remote repl protocol
  ;; client
  ;;   send datum: :datum <datum>
  ;;   exit      : :exit
  ;; server
  ;;   on auth   : :request-authenticate for authenticate
  ;;               :response-authenticate <result>
  ;;   on success: :values <n> <value 1> ... <value n>
  ;;   on error  : :error <expression> <message>
  ;;   on exit   : :exit
  ;;   other i/o : {no-tags just strings}

  (define (pack-data tag . data)
    (call-with-string-output-port
     (^o (write tag o)
	 (for-each (^d (display #\space o) (write d o)) data))))

  (define-syntax send-packed-data
    (syntax-rules ()
      ((_ socket tag data ...)
       (send-datum socket (pack-data tag data ...)))))

  (define (connect-remote-repl node service :key (secure #f))
    (define (make-evaluator socket)
      (lambda (form env)
	(send-packed-data socket :datum form)))
    (define (make-printer socket)
      (lambda args
	(define (handle-values n port)
	  (dotimes (i n)
	    (display (read/ss port)) (newline)))
	(let loop ()
	  (let* ((port (recv-datum socket))
		 (tag   (read/ss port)))
	    (unless (eof-object? tag)
	      (case tag
		((:values) (handle-values (read/ss port) port))
		((:error)
		 (format #t "error in: ~s~%" (read/ss port))
		 (display (read/ss port)) (newline))
		((:exit) (display ";; finished") (newline) (exit 0))
		(else 
		 ;; other I/O
		 (display tag) (loop))))))))
    (define (authenticate socket)
      (define (wait-response)
	(let loop ()
	  (let1 port (recv-datum socket)
	    (case (read/ss port)
	      ((:response-authenticate) (read/ss port))
	      ((:alert) (display (read/ss port)) (newline) (loop))
	      ((:prompt)
	       (display (read/ss port))
	       (flush-output-port)
	       (send-packed-data socket :user-input (read/ss))
	       (loop))))))
      (case (read/ss (recv-datum socket))
	((:request-authenticate) (wait-response))
	((:no-authenticate) #t)))
    (format #t "~%connect: ~a:~a (enter ^D to exit) ~%~%" node service)
    (call-with-socket (if secure
			  (make-client-tls-socket node service)
			  (make-client-socket node service))
      (lambda (socket)
	(when (authenticate socket)
	  (parameterize ((current-evaluator (make-evaluator socket))
			 (current-printer   (make-printer socket))
			 (current-exit (lambda ()
					 (send-packed-data socket :exit))))
	    (read-eval-print-loop #f)))))
    (format #t "~%[exit]~%")
    (flush-output-port))

  ;; TODO what should we get?
  (define (make-username&password-authenticate username password
					       :key (digest SHA-1)
					       (max-try 3))
    
    (let* ((bv (string->utf8 (string-append username "&" password)))
	   (credential (hash digest bv)))
      (lambda (socket)
	(define (read-response socket)
	  (let1 port (recv-datum socket)
	    (case (read/ss port)
	      ((:user-input) (read/ss port))
	      (else ;; how should we handle?
	       => (^r
		   (error 'username&password-authenticate
			  "invalid response" r))))))
	(let loop ((count 1))
	  (send-packed-data socket :prompt "username> ")
	  (let1 input-username (read-response socket)
	    (send-packed-data socket :prompt "password> ")
	    (let* ((input-password (read-response socket))
		   (input (string->utf8 
			   (string-append (->string input-username) "&"
					  (->string input-password))))
		   (h (hash digest input)))
	      (cond ((bytevector=? credential h))
		    ((= count max-try)
		     ;; TODO log it
		     (send-packed-data socket :alert "login failed, disconnect")
		     #f)
		    (else (loop (+ count 1))))))))))
		    
  (define (make-remote-repl service :key (secure #f)
			    (authenticate #f)
			    (certificates '())
			    (private-key #f)
			    (log (current-output-port)))
    (define (detach server socket)
      (define (main-loop in out err)
	(define (set-ports! p)
	  (current-input-port p)
	  (current-output-port p)
	  (current-error-port p))
	(define (restor-ports!)
	  (current-input-port in)
	  (current-output-port out)
	  (current-error-port err))
	(let ((stop? #f) (current-expression #f))
	  (format log "remote-repl: connect ~s~%" socket)
	  (let loop ()
	    (call/cc
	     (lambda (continue)
	       (with-exception-handler
		(lambda (c)
		  (restor-ports!)
		  (format log "~%error in ~a: ~a~%" server
			  (if (message-condition? c)
			      (condition-message c)
			      c))
		  ;; socket connection
		  (let1 msg (describe-condition c)
		    (send-packed-data socket :error current-expression msg))
		  (and (serious-condition? c) (continue)))
		(lambda ()
		  (let1 in (recv-datum socket)
		    (define (data->string r) (map (^e (format "~s" e)) r))
		    (case (read/ss in)
		      ((:datum)
		       (let ((e (read/ss in))
			     (p (transcoded-port 
				 (if (tls-socket? socket)
				     (tls-socket-port socket)
				     (socket-port socket))
				 (native-transcoder))))
			 (set-ports! p)
			 (set! current-expression e)
			 (if (eof-object? e)
			     (set! stop? #t)
			     (receive r (eval e interaction-environment)
			       (send-datum 
				socket
				(apply pack-data :values (length r)
				       (data->string r)))))))
		      ((:exit)
		       (send-packed-data socket :exit)
		       (set! stop? #t))
		      (else 
		       => (^t 
			   ;; invalid protocol
			   (send-packed-data 
			    socket :error (format "unknown tag ~s" t))))))))))
	    (restor-ports!)
	    (unless stop? (loop)))))
      (define (do-authenticate socket)
	(if authenticate
	    ;; send authenticate request
	    (begin
	      (send-packed-data socket :request-authenticate)
	      (let1 r (authenticate socket)
		(send-packed-data socket :response-authenticate r)
		r))
	    (send-packed-data socket :no-authenticate)))
	
      (lambda ()
	(call-with-socket socket
	  (lambda (socket)
	    (let ((in    (current-input-port))
		  (out   (current-output-port))
		  (err   (current-error-port)))
	      (when (do-authenticate socket)
		(main-loop in out err)))))))
    (let1 server (if secure
		     (make-server-tls-socket service certificates
					     :private-key private-key)
		     (make-server-socket service))
      (format log "~%remote-repl: ~a~%~%" server)
      (lambda ()
	(let loop ()
	  (let* ((socket (if (tls-socket? server)
			     (tls-socket-accept server)
			     (socket-accept server)))
		 (thread (make-thread (detach server socket))))
	    (thread-start! thread)
	    (loop))))))

)