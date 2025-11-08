#!read-macro=sagittarius/bv-string
(import (rnrs)
	(rnrs eval)
	(clos user)
	(sagittarius pam)
	(sagittarius socket)
	(sagittarius crypto keys)
	(rfc ssh) ;; for types and client
	(rfc ssh server)
	(srfi :18))

(define-method ssh-authenticate-user ((m (equal +ssh-connection+))
				      (cred <ssh-interactive-credential>))
  (define ((make-conversation cred) prompt)
    ((ssh-credential-prompt-sender cred)
     (vector-map (lambda (p)
		   (make-ssh-interactive-prompt (cdr p) (eq? 'echo-on (car p))))
		 prompt))
    ((ssh-credential-response-receiver cred)))
  (define conv (make-conversation cred))
  (cond ((pam-authenticate "login" (ssh-credential-username cred) conv) =>
	 (lambda (token)
	   (pam-invalidate-token! token)
	   (make <ssh-auth-ticket>)))
	(else #f)))

(define-method ssh-handle-channel-request
  ((m (equal "subsystem")) channel ignore bv)
  (define (make-data-handler)
    (let ((env (environment '(rnrs) '(sagittarius))))
      (lambda (c data start count)
	;; #f means EOF
	(when data
	  (let* ((in (open-bytevector-input-port data (native-transcoder)
						 start (+ count start)))
		 (v (eval (get-datum in) env)))
	    (let-values (((out e) (open-bytevector-output-port
				   (native-transcoder))))
	      (put-string out v)
	      (ssh-deliver-channel-data c (e))))))))
  (let ((msg (bytevector->ssh-message <ssh-msg-channel-subsystem-request> bv)))
    (and (string=? (slot-ref msg 'subsystem-name) "scheme")
	 (make-data-handler))))


(define ed25519-host-key (generate-key-pair *key:ed25519*))
(define server-socket (make-server-socket "0"))
(define server-port (socket-info-port (socket-info server-socket)))
(define (server-thread)
  (define (event-loop transport)
    (let loop ((packet (ssh-read-packet transport)))
      (cond ((ssh-server-handle-packet transport packet)
	     (unless (ssh-packet-msg-disconnect? packet)
	       (loop (ssh-read-packet transport))))
	    (else
	     (close-server-ssh-transport!
	      transport +ssh-disconnect-service-not-available+)))))
  (let loop ((socket (socket-accept server-socket)))
    (let ((transport (socket->server-ssh-transport
		      socket (list (key-pair->ssh-host-key ed25519-host-key)))))
      (guard (e (else (close-server-ssh-transport! transport)
		      (report-error e)))
	(event-loop transport))
      (loop (socket-accept server-socket)))))
(define thread (thread-start! (make-thread server-thread)))


(define (from-console prompt)
  (display prompt) (flush-output-port (current-output-port))
  (get-line (current-input-port)))

(let ((socket (make-client-socket "localhost" (number->string server-port))))
  (guard (e (else (socket-close socket)))
    (let ((transport (socket->client-ssh-transport socket)))
      (open-client-ssh-transport! transport)
      (when (ssh-authenticate transport +ssh-auth-method-keyboard-interactive+
			      (from-console "Username: "))
	(call-with-ssh-channel (open-client-ssh-session-channel transport)
	  (lambda (c)
	    ;; request scheme subsystem
	    ;; Can be interactive session, but for this example we
	    ;; send only one expression.
	    (ssh-request-subsystem c "scheme")
	    (ssh-send-channel-data c #*"(sagittarius-version)")
	    (print (utf8->string (ssh-recv-channel-data c)))))
	(close-client-ssh-transport! transport)))))

(socket-close server-socket)
