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
	    (srfi :18)
	    (srfi :26)
	    (srfi :39)
	    (math)
	    (pp))
  (define interaction-environment (find-library 'user #t))
  ;; to keep connection we need to do some low level operations
  (define-constant +chunk-size+ 512)

  (define (send-datum socket datum)
    (let1 bv (string->utf8 (format "~a" datum))
      (socket-send socket bv)))
  (define (recv-datum socket)
    (define (recv-bv)
      (call-with-bytevector-output-port
       (lambda (out)
	 (let loop ((bv (socket-recv socket +chunk-size+)))
	   (put-bytevector out bv)
	   (unless (< (bytevector-length bv) +chunk-size+)
	     (loop (socket-recv socket +chunk-size+)))))))
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
  ;;   on error  : :error  <message>
  ;;   on exit   : :exit
  ;;   other i/o : {no-tags just strings}

  (define (pack-data tag . data)
    (call-with-string-output-port
     (^o (write tag o)
	 (for-each (^d (display #\space o) (write d o)) data))))

  (define (connect-remote-repl node service :optional (secure #f))
    (define (make-evaluator socket)
      (lambda (form env)
	(send-datum socket (pack-data :datum form))))
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
		((:error)  (display (read/ss port)) (newline))
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
	       (send-datum socket (pack-data :user-input (read/ss)))
	       (loop))))))
      (case (read/ss (recv-datum socket))
	((:request-authenticate) (wait-response))
	((:no-authenticate) #t)))
    (format #t "~%connect: ~a:~a (enter ^D to exit) ~%~%" node service)
    (call-with-socket (make-client-socket node service)
      (lambda (socket)
	(when (authenticate socket)
	  (parameterize ((current-evaluator (make-evaluator socket))
			 (current-printer   (make-printer socket))
			 (current-exit (lambda ()
					 (send-datum socket 
						     (pack-data :exit))))))
	    (read-eval-print-loop))))
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
	  (send-datum socket (pack-data :prompt "username> "))
	  (let1 input-username (read-response socket)
	    (send-datum socket (pack-data :prompt "password> "))
	    (let* ((input-password (read-response socket))
		   (input (string->utf8 
			   (string-append (->string input-username) "&"
					  (->string input-password))))
		   (h (hash digest input)))
	      (cond ((bytevector=? credential h))
		    ((= count max-try)
		     ;; TODO log it
		     (send-datum socket 
				 (pack-data :alert "login failed, disconnect"))
		     #f)
		    (else (loop (+ count 1))))))))))
		    
  (define (make-remote-repl service :key (secure #f)
			    (authenticate #f))
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
	(let1 stop? #f
	  (let loop ()
	    (call/cc
	     (lambda (continue)
	       (with-exception-handler
		(lambda (c)
		  (restor-ports!)
		  (format #t "~%error in ~a: ~a~%" server c)
		  ;; socket connection
		  (send-datum socket 
			      (pack-data :error
					 (call-with-string-output-port
					  (lambda (err) (report-error c err)))))
		  (and (serious-condition? c) (continue)))
		(lambda ()
		  (format #t "remote-repl: connect ~s~%" socket)
		  (let1 in (recv-datum socket)
		    (define (data->string r) (map (^e (format "~s" e)) r))
		    (case (read/ss in)
		      ((:datum)
		       (let ((e (read/ss in))
			     (p (make-remote-in/out-port socket)))
			 (set-ports! p)
			 (if (eof-object? e)
			     (set! stop? #t)
			     (receive r (eval e interaction-environment)
			       (send-datum 
				socket
				(apply pack-data :values (length r)
				       (data->string r)))))))
		      ((:exit)
		       (send-datum socket (pack-data :exit))
		       (set! stop? #t))
		      (else 
		       => (^t 
			   ;; invalid protocol
			   (send-datum 
			    socket 
			    (pack-data :error 
				       (format "unknown tag ~s" t)))))))))))
	    (restor-ports!)
	    (unless stop? (loop)))))
      (define (do-authenticate socket)
	(if authenticate
	    ;; send authenticate request
	    (begin
	      (send-datum socket (pack-data :request-authenticate))
	      (let1 r (authenticate socket)
		(send-datum socket (pack-data :response-authenticate r))
		r))
	    (send-datum socket (pack-data :no-authenticate))))
	
      (lambda ()
	(call-with-socket socket
	  (lambda (socket)
	    (let ((in    (current-input-port))
		  (out   (current-output-port))
		  (err   (current-error-port)))
	      (when (do-authenticate socket)
		(main-loop in out err)))))))
    (let1 server (make-server-socket service)
      (format #t "~%remote-repl: ~a~%~%" server)
      (lambda ()
	(let loop ()
	  (let* ((socket (socket-accept server))
		 (thread (make-thread (detach server socket))))
	    (thread-start! thread)
	    (loop))))))
  ;; why do i need this?
  (define (make-remote-in/out-port socket)
    (define (read! bv start count)
      (let1 r (socket-recv socket count)
	(bytevector-copy! r 0 bv start (bytevector-length r))
	(bytevector-length r)))
    (define (write! bv start count)
      (socket-send socket (bytevector-copy bv start (+ start count)))
      count)
    (transcoded-port
     (make-custom-binary-input/output-port "remote-port" read! write! #f #f #f)
     (native-transcoder)))
)