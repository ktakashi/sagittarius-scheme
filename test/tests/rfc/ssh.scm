#!read-macro=sagittarius/bv-string
(import (rnrs)
	(rnrs eval)
	(rfc ssh)
	(rfc ssh connection) ;; for test
	(rfc ssh server)
	(rfc ssh transport kex hybrid)
	(rfc ssh server types) ;; for test
	(sagittarius socket)
	(sagittarius crypto digests)
	(sagittarius crypto keys)
	(util concurrent)
	(clos user)
	(srfi :18)
	(srfi :39)
	(srfi :64))

(define ed25519-host-key (generate-key-pair *key:ed25519*))
(define ed448-host-key (generate-key-pair *key:ed448*))
(define ecdsa-p256-host-key (generate-key-pair *key:ecdsa*))
(define ecdsa-p384-host-key (generate-key-pair *key:ecdsa* :ec-parameter *ec-parameter:p384*))
(define ecdsa-p521-host-key (generate-key-pair *key:ecdsa* :ec-parameter *ec-parameter:p521*))
(define rsa-host-key (generate-key-pair *key:rsa*))
(define dsa-host-key (generate-key-pair *key:dsa*))

(define username "username")
(define password "password")

(define-method ssh-authenticate-user ((m (equal +ssh-connection+))
				      (cred <ssh-interactive-credential>))
  ((ssh-credential-prompt-sender cred)
   (vector (make-ssh-interactive-prompt "Password:" #f)))
  (let ((resp ((ssh-credential-response-receiver cred))))
    (and (string=? username (ssh-credential-username cred))
	 (string=? password (vector-ref resp 0))
	 (make <ssh-auth-ticket>))))

(define-method ssh-authenticate-user ((m (equal +ssh-connection+))
				      (cred <ssh-pubic-key-credential>))
  ;; we trust keys, it's test anyway :)
  (and (string=? username (ssh-credential-username cred))
       (make <ssh-auth-ticket>)))

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
  (define msg (bytevector->ssh-message <ssh-msg-channel-subsystem-request> bv))
  (and (string=? (slot-ref msg 'subsystem-name) "scheme")
       (make-data-handler)))

(define host-keys
  (map key-pair->ssh-host-key (list ed25519-host-key
				    ed448-host-key
				    ecdsa-p256-host-key
				    ecdsa-p384-host-key
				    ecdsa-p521-host-key
				    rsa-host-key
				    dsa-host-key)))
(define server-socket (make-server-socket "0"))
(define server-port (socket-info-port (socket-info server-socket)))
(define (custom-prompt-handler prompts) (list password))
(define u8 bytevector-u8-ref)

(define (print-auth-error service username method e)
  (report-error e))

(test-begin "SSH")

;; Hybrid KEX wire format and secret combination
(test-group "hybrid KEX encoding"
  (let* ((C-INIT (make-bytevector (+ 1184 32) #x11))
	 (init (make <ssh-msg-kex-hybrid-init> :C-INIT C-INIT))
	 (packet (ssh-message->bytevector init))
	 (decoded (bytevector->ssh-message <ssh-msg-kex-hybrid-init> packet)))
    (test-equal "hybrid init message number"
      +ssh-msg-kex-hybrid-init+ (bytevector-u8-ref packet 0))
    (test-equal "hybrid init one string length"
      (+ 1 4 1184 32) (bytevector-length packet))
    (test-assert "hybrid init payload roundtrip"
      (bytevector=? C-INIT (slot-ref decoded 'C-INIT)))
    (test-assert "hybrid init packet canonical"
      (ssh-hybrid-valid-packet? packet decoded +ssh-msg-kex-hybrid-init+))
    (test-assert "hybrid init rejects trailing bytes"
      (not (ssh-hybrid-valid-packet?
	    (bytevector-append packet #vu8(0)) decoded
	    +ssh-msg-kex-hybrid-init+))))
  (test-error "hybrid init rejects incorrect component lengths" condition?
    (ssh-hybrid-client-init-components #vu8(0)))
  (let* ((S-REPLY (make-bytevector (+ 1088 32) #x22))
	 (reply (make <ssh-msg-kex-hybrid-reply>
		  :K-S #vu8(1 2)
		  :S-REPLY S-REPLY
		  :signature #vu8(3 4)))
	 (packet (ssh-message->bytevector reply))
	 (decoded (bytevector->ssh-message <ssh-msg-kex-hybrid-reply> packet)))
    (test-equal "hybrid reply message number"
      +ssh-msg-kex-hybrid-reply+ (bytevector-u8-ref packet 0))
    (test-assert "hybrid reply payload roundtrip"
      (bytevector=? S-REPLY (slot-ref decoded 'S-REPLY)))
    (test-assert "hybrid reply packet canonical"
      (ssh-hybrid-valid-packet? packet decoded +ssh-msg-kex-hybrid-reply+)))
  (test-error "hybrid reply rejects incorrect component lengths" condition?
    (ssh-hybrid-server-reply-components #vu8(0)))
  (test-equal "hybrid K is SHA-256 of both fixed-length secrets"
    #vu8(248 24 175 211 122 109 195 188 146 251 68 115 16 17 39 112
	 6 219 78 250 110 144 35 205 116 104 192 35 53 210 42 77)
    (ssh-hybrid-combine-secrets (make-bytevector 32 1)
				(make-bytevector 32 2)))
  (test-error "hybrid K rejects non-32-byte inputs" condition?
    (ssh-hybrid-combine-secrets (make-bytevector 31 1)
				(make-bytevector 32 2)))
  (let ((hash-message
	 (make <SSH-HYBRID-H>
	   :V-C "SSH-2.0-client"
	   :V-S "SSH-2.0-server"
	   :I-C #vu8(20 30)
	   :I-S #vu8(20 31)
	   :K-S #vu8(1 2)
	   :C-INIT (make-bytevector (+ 1184 32) #x11)
	   :S-REPLY (make-bytevector (+ 1088 32) #x22)
	   :K (make-bytevector 32 #x33))))
    (test-equal "RFC 10042 hybrid exchange-hash vector"
      #vu8(241 127 162 27 30 116 81 120 14 252 160 157 134 164 99 135
	   188 131 209 244 123 244 79 67 222 223 49 225 168 163 197 134)
      (digest-message
       (ssh-kex-digest +kex-mlkem768x25519-sha256+)
       (ssh-message->bytevector hash-message)))))

;; KEX and other algorithm testing
(let ()
  (define client-queue (make-shared-queue))
  (define client-result-queue (make-shared-queue))
  (define client-authenticate #f)
  (define (keyboard-authenticate transport)
    (ssh-authenticate transport
		      +ssh-auth-method-keyboard-interactive+ username
		      :prompt-handler custom-prompt-handler))
  (define ((public-key-authenticate kp) transport)
    (ssh-authenticate transport
		      +ssh-auth-method-public-key+ username
		      (key-pair-private kp)
		      (key-pair-public kp)))
  (define client-kex-list (*ssh-client-kex-list*))
  (define client-public-key-list (*ssh-client-public-key-list*))
  (define client-mac-list (*ssh-client-mac-list*))
  (define client-enc-list (*ssh-client-encryption-list*))
  ;; TODO public key authentication
  (define (client-thread)
    (let loop ()
      (when (shared-queue-get! client-queue)
	(let ((socket (make-client-socket "localhost"
					  (number->string server-port))))
	  (guard (e (else (print e)
			  (socket-close socket)
			  (shared-queue-put! client-result-queue
					     "unexpected error")
			  (loop)))
	    (let ((transport (socket->client-ssh-transport socket)))
	      (parameterize ((*ssh-client-kex-list* client-kex-list)
			     (*ssh-client-public-key-list* client-public-key-list)
			     (*ssh-client-mac-list* client-mac-list)
			     (*ssh-client-encryption-list* client-enc-list))
		(open-client-ssh-transport! transport)
		(if (client-authenticate transport)
		    (let ((c (open-client-ssh-session-channel transport)))
		      (ssh-request-subsystem c "scheme")
		      (ssh-send-channel-data c #*"(sagittarius-version)")
		      (let ((v (ssh-recv-channel-data c)))
			(shared-queue-put! client-result-queue (utf8->string v)))
		      (close-ssh-channel c))
		    (shared-queue-put! client-result-queue "auth failed")))
	      (close-client-ssh-transport! transport))
	    (loop))))))

  (define (test-ssh-server host-keys :key (kex #f) (mac #f) (enc #f))
    (define (wait-packet transport)
      (let loop ((packet (ssh-read-packet transport)))
	(when (ssh-server-handle-packet transport packet)
	  ;; extra packet handling for test :)
	  (when (= (bytevector-u8-ref packet 0) +ssh-msg-service-request+)
	    (let ((name (utf8->string packet 5)))
	      (test-equal "service connection" +ssh-userauth+ name)))
	  (unless (ssh-packet-msg-disconnect? packet)
	    (loop (ssh-read-packet transport))))))
    (define socket (socket-accept server-socket))

    (guard (e (else (print e)
		    (test-assert "unexpected error" #f)
		    (socket-shutdown socket SHUT_RDWR)
		    (socket-close socket)
		    (if kex
			(test-assert kex #f)
			(test-assert client-public-key-list #f))))
      (parameterize ((*ssh-server-kex-list* (or kex (*ssh-server-kex-list*)))
		     (*ssh-server-mac-list* (or mac (*ssh-server-mac-list*)))
		     (*ssh-server-encryption-list* (or enc (*ssh-server-encryption-list*)))
		     (*ssh-authentication-error-handler* print-auth-error))
	(let ((transport (socket->server-ssh-transport socket host-keys)))
	  (wait-packet transport)
	  (when (and kex (= (length kex) 1))
	    (test-equal "negotiated KEX" (car kex) (slot-ref transport 'kex)))
	  (unless kex
	    (test-equal "preferred default KEX"
	      +kex-mlkem768x25519-sha256+ (slot-ref transport 'kex)))
	  (when kex (test-assert kex #t))
	  (when mac (test-assert mac #t))
	  (when enc (test-assert enc #t))
	  (close-server-ssh-transport! transport
				       +ssh-disconnect-by-application+)
	  (let ((v (shared-queue-get! client-result-queue)))
	    (test-equal "result" (sagittarius-version) v))))))

  (thread-start! (make-thread client-thread))

  (test-group "keyboard interactive"
   (set! client-authenticate keyboard-authenticate)
   (test-group "generic case"
     (shared-queue-put! client-queue #t)
     (test-ssh-server host-keys))
   (test-group "test cryptographic algorithms"
    (test-group "test KEX"
      (let ((save client-kex-list))
	(print "---- KEX test ----")
	(for-each (lambda (kex)
		    (print kex)
		    (set! client-kex-list (list kex))
		    (shared-queue-put! client-queue #t)
		    (test-ssh-server host-keys :kex (list kex)))
		  (list +kex-diffie-hellman-group-exchange-sha256+
			+kex-diffie-hellman-group-exchange-sha1+
			+kex-diffie-hellman-group14-sha1+
			+kex-diffie-hellman-group1-sha1+
			+kex-diffie-hellman-group14-sha256+
			+kex-diffie-hellman-group15-sha512+
			+kex-diffie-hellman-group16-sha512+
			+kex-diffie-hellman-group17-sha512+
			+kex-diffie-hellman-group18-sha512+
			+kex-ecdh-sha2-nistp256+
			+kex-ecdh-sha2-nistp384+
			+kex-ecdh-sha2-nistp521+
			+kex-ecdh-sha2-nistk163+
			+kex-ecdh-sha2-nistp192+
			+kex-ecdh-sha2-nistp224+
			+kex-ecdh-sha2-nistk233+
			+kex-ecdh-sha2-nistb233+
			+kex-ecdh-sha2-nistk283+
			+kex-ecdh-sha2-nistk409+
			+kex-ecdh-sha2-nistb409+
			+kex-ecdh-sha2-nistt571+
			+kex-mlkem768x25519-sha256+
			+kex-curve25519-sha256+
			+kex-curve448-sha512+))
	(set! client-kex-list save)
	(set! client-kex-list
	      (list +kex-mlkem768x25519-sha256+ +kex-curve25519-sha256+))
	(shared-queue-put! client-queue #t)
	(test-ssh-server host-keys :kex (list +kex-curve25519-sha256+))
	(set! client-kex-list save)))
    (test-group "test public key algorithms"
      (let ((save client-public-key-list))
	(define (->ssh-host-key kp pkey)
	  (make-ssh-host-key pkey (key-pair-private kp) (key-pair-public kp)))
	(print "---- Public key algorithm test ----")
	(for-each (lambda (host-key pkey)
		    (print pkey)
		    (set! client-public-key-list (list pkey))
		    (shared-queue-put! client-queue #t)
		    (test-ssh-server (list (->ssh-host-key host-key pkey))))
		  (list ed25519-host-key
			ed448-host-key
			ecdsa-p256-host-key
			ecdsa-p384-host-key
			ecdsa-p521-host-key
			rsa-host-key
			rsa-host-key
			rsa-host-key
			dsa-host-key)
		  (list +public-key-ssh-ed25519+
			+public-key-ssh-ed448+
			+public-key-ecdsa-sha2-nistp256+
			+public-key-ecdsa-sha2-nistp384+
			+public-key-ecdsa-sha2-nistp521+
			+public-key-rsa-sha2-256+
			+public-key-rsa-sha2-512+
			+public-key-ssh-rsa+
			+public-key-ssh-dss+))
	(print "---- Public key algorithm test recommended curves ----")
	;; recommended EC parameters
	(for-each
	 (lambda (name)
	   (let* ((param
		   (ssh-ecdsa-identifier->ec-parameter (string->keyword name)))
		  (ssh-name (string-append "ecdsa-sha2-" name))
		  (kp (generate-key-pair *key:ecdsa* :ec-parameter param)))
	     (print ssh-name)
	     (set! client-public-key-list (list ssh-name))
	     (shared-queue-put! client-queue #t)
	     (test-ssh-server (list (->ssh-host-key kp ssh-name)))))
	 '("nistk163"
	   "nistp192"
	   "nistp224"
	   "nistk233"
	   "nistb233"
	   "nistk283"
	   "nistk409"
	   "nistb409"
	   "nistt571"
	   ))
	(set! client-public-key-list save)))
    (test-group "test MAC algorithm"
      (let ((save client-mac-list))
	(print "---- MAC algorithm test ----")
	(for-each
	 (lambda (name)
	   (print name)
	   (set! client-mac-list (list name))
	   (shared-queue-put! client-queue #t)
	   (test-ssh-server host-keys :mac (list name)))
	 (list +mac-hmac-sha2-256+ +mac-hmac-sha1+))

	(set! client-mac-list save)))

    (test-group "test Encryption algorithm"
      (let ((save client-enc-list))
	(print "---- Encryption algorithm test ----")
	(for-each
	 (lambda (name)
	   (print name)
	   (set! client-enc-list (list name))
	   (shared-queue-put! client-queue #t)
	   (test-ssh-server host-keys :enc (list name)))
	 (list +enc-aes256-ctr+  
	       +enc-aes192-ctr+  
	       +enc-aes128-ctr+  
	       +enc-3des-ctr+    
	       +enc-blowfish-ctr+
	       +enc-aes256-cbc+  
	       +enc-aes128-cbc+  
	       +enc-3des-cbc+    
	       +enc-blowfish-cbc+))

	(set! client-enc-list save)))
    ))
  (test-group "public-key authentication"
   (print "==== public key auth ====")
   (for-each (lambda (kp)
	       (set! client-authenticate (public-key-authenticate kp))
	       (shared-queue-put! client-queue #t)
	       (test-ssh-server host-keys))
	     ;; layziness
	     (list ed25519-host-key
		   ed448-host-key
		   ecdsa-p521-host-key
		   ecdsa-p384-host-key
		   ecdsa-p256-host-key
		   rsa-host-key
		   dsa-host-key)))
  ;; end client thread
  (shared-queue-put! client-queue #f))
(socket-close server-socket)

(test-end)
