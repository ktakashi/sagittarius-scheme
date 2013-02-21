;;; -*- Scheme -*-
;;;
;;; tls.scm - TLS 1.0 - 1.2 protocol library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; Caution this library is not well tested and not secure yet.
(library (rfc tls socket)
    (export make-client-tls-socket
	    make-server-tls-socket

	    tls-socket?
	    tls-socket-send
	    tls-socket-recv
	    tls-socket-close
	    tls-socket-closed?
	    tls-socket-accept

	    ;; for the user who wants to specify TSL version
	    *tls-version-1.2*
	    *tls-version-1.1*
	    *tls-version-1.0*

	    socket-close
	    )
    (import (rnrs)
	    (core errors)
	    (sagittarius)
	    (sagittarius socket)
	    (sagittarius control)
	    (sagittarius object)
	    (rfc tls types)
	    (rfc tls constant)
	    (rfc x.509)
	    (rfc hmac)
	    (util bytevector)
	    (except (math) lookup-hash)
	    (crypto)
	    (clos user)
	    (srfi :19 time)
	    (srfi :39 parameters))

  (define-class <session-key> ()
    ((write-mac-secret :init-keyword :write-mac-secret)
     (write-key  :init-keyword :write-key)
     (write-iv   :init-keyword :write-iv)
     (final-wirte-key :init-value #f)
     (read-mac-secret :init-keyword :read-mac-secret)
     (read-key  :init-keyword :read-key)
     (read-iv   :init-keyword :read-iv)
     (final-read-key :init-value #f)))

  (define-class <dh-params> ()
    ;; The following url was comprehensive
    ;; http://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange
    ((g :init-keyword :g)
     (p :init-keyword :p)
     (Ys :init-keyword :Ys)   ;; A
     (Yc :init-keyword :Yc))) ;; B

  (define-class <tls-session> ()
    ((session-id :init-keyword :session-id)
     ;; negotiated version
     (version :init-keyword :version :init-value #f)
     ;; for server-certificate
     (need-certificate? :init-value #f)
     ;; random data
     (client-random     :init-value #f)
     (server-random     :init-value #f)
     ;; compression methods 
     ;; XXX for now we don't check any where
     (methods     :init-keyword :methods)
     ;; this must be defined by server
     (cipher-suite :init-value #f)
     ;; public key. can be #f if server does not have certificate
     (public-key :init-value #f)
     ;; computed master secret
     (master-secret :init-value #f)
     ;; sequence number
     (read-sequence :init-value 0)
     (write-sequence :init-value 0)
     ;; session key (to avoid unnecessary caluculation)
     (session-key    :init-value #f)
     ;; RSA or DH parameters
     (params         :init-value #f)
     (closed?        :init-value #f)
     (session-encrypted? :init-value #f)
     ;; all handshake messages without record layer...
     (messages :init-form (open-output-bytevector))))

  (define-class <tls-server-session> (<tls-session>)
    ;; for DHE
    ((a :init-value #f)
     ;; ugly ...
     (client-finished)))

  (define-class <tls-socket> ()
    ((raw-socket :init-keyword :raw-socket)
     ;; should this be in session?
     (version    :init-keyword :version)
     (prng       :init-keyword :prng)
     (session    :init-keyword :session)
     ;; for tls-socket-recv, we need to store application data
     ;; in this buffer to be able to take size argument.
     (buffer     :init-value #f)))

  (define-class <tls-server-socket> (<tls-socket>)
    (;; for server socket
     (certificates :init-value '() :init-keyword :certificates)
     (private-key  :init-value #f  :init-keyword :private-key)))

  (define-method write-object ((o <tls-socket>) out)
    (let1 session (~ o 'session)
      (format out "#<~a ~x~a~a>" 
	      (if (is-a? o <tls-server-socket>)
		  "tls-server-socket" "tls-socket") 
	      (~ session 'version)
	      (if (~ session 'closed?)
		  " session-closed"
		  "")
	      (if (~ o 'raw-socket)
		  ""
		  " closed"))))
  (define (tls-socket? o) (is-a? o <tls-socket>))
  (define (tls-socket-closed? socket) (~ socket 'session 'closed?))

  (define (negotiated-version socket)
    (let1 session (~ socket 'session)
      (or (~ session 'version)
	  (~ socket 'version))))

  (define (make-cipher-suites)
    (let* ((size (length *cipher-suites*))
	   (bv (make-bytevector (* size 2))))
      (do ((i 0 (+ i 2))
	   (ciphers *cipher-suites* (cdr ciphers)))
	  ((null? ciphers))
	(bytevector-u16-set! bv i (caar ciphers) 'big))
      bv))

  (define (make-client-hello socket :optional (extension #f) :rest ignore)
    (let* ((version (negotiated-version socket))
	   (session (~ socket 'session))
	   (random-bytes (read-random-bytes (~ socket 'prng) 28))
	   (random (make-tls-random random-bytes))
	   (session-id (make-variable-vector 1 (~ session 'session-id)))
	   (cipher-suite (make-variable-vector 2 (make-cipher-suites)))
	   (compression-methods (make-variable-vector 1 (~ session 'methods)))
	   (hello (apply make-tls-client-hello
			 :version version
			 :random random
			 :session-id session-id
			 :cipher-suites cipher-suite
			 :compression-methods compression-methods
			 (if extension
			     (list :extensions extension)
			     '()))))
      (set! (~ session 'client-random) random)
      (make-tls-handshake *client-hello* hello)))

  (define (make-initial-session prng :optional (class <tls-session>))
    (make class
      :session-id (read-random-bytes prng 28)
      :methods #vu8(0)))

  (define *dh-primi-size* (make-parameter 1024))
  ;; is 2 ok?
  (define-constant +dh-g+ 2)

  (define (tls-packet->bytevector p)
    (call-with-bytevector-output-port (^o (write-tls-packet p o))))

  (define (make-server-tls-socket port certificates
				  :key (prng (secure-random RC4))
				  (private-key #f)
				  (version *tls-version-1.2*)
				  :allow-other-keys opt)
    (let1 raw-socket (apply make-server-socket port opt)
      (make <tls-server-socket> :raw-socket raw-socket
	    :version version :prng prng
	    ;; so far we don't need this but for later
	    :private-key private-key
	    :certificates certificates
	    :session (make-initial-session prng <tls-server-session>))))

  (define (tls-socket-accept socket)
    (let* ((raw-socket (socket-accept (~ socket 'raw-socket)))
	   (new-socket (make <tls-server-socket> :raw-socket raw-socket
			     :version (~ socket 'version)
			     :prng (~ socket 'prng)
			     :session (make-initial-session 
				       (~ socket 'prng)
				       <tls-server-session>)
			     :private-key (~ socket 'private-key)
			     :certificates (~ socket 'certificates))))
      (tls-server-handshake new-socket)
      new-socket))

  (define (verify-mac socket finish label restore-message?)
    (let* ((session (~ socket 'session))
	   (messages (get-output-bytevector (~ session 'messages)))
	   (session-verify (tls-finished-data finish))
	   (verify (finish-message-hash session label messages)))
      ;; verify message again
      (when restore-message? 
	(put-bytevector (~ session 'messages) messages))
      (or (bytevector=? session-verify verify)
	  (error 'tls-handshake "MAC verification failed"))))

  (define (tls-server-handshake socket)
    (define (process-client-hello! hello)
      (unless (<= *tls-version-1.0*  (~ hello 'version) *tls-version-1.2*)
	(assertion-violation 'tls-server-handshake
			     "non supported TLS version" (~ hello 'version)))
      ;; for now only supports max TLS1.1
      (if (> (~ hello 'version) *tls-version-1.1*)
	  (set! (~ socket 'session 'version) *tls-version-1.1*)
	  (set! (~ socket 'session 'version) (~ hello 'version)))
      (set! (~ socket 'session 'client-random) (~ hello 'random))
      (let* ((vv (~ hello 'cipher-suites))
	     (bv (~ vv 'value))
	     (len (bytevector-length bv))
	     (have-key? (~ socket 'private-key))
	     (suppoting-suites *cipher-suites*))
	(let loop ((i 0))
	  (cond ((>= i len) (error 'tls-server-handshake "no cipher"))
		((and-let* ((spec (bytevector-u16-ref bv i 'big))
			    ( (or have-key? 
				  (memv spec *dh-key-exchange-algorithms*)) ))
		   (assv spec suppoting-suites))
		 => (^s
		     (set! (~ socket 'session 'cipher-suite) (car s))))
		(else (loop (+ i 2)))))))
    (define (send-server-hello)
      (let1 random (make-tls-random (read-random-bytes (~ socket 'prng) 28))
	(set! (~ socket 'session 'server-random) random)
	(tls-socket-send-inner socket
	 (make-tls-handshake *server-hello*
	  (make-tls-server-hello
	   :version (~ socket 'session 'version)
	   :random random
	   :session-id (make-variable-vector 1 (~ socket 'session 'session-id))
	   :cipher-suite (~ socket 'session 'cipher-suite)
	   ;; no compression
	   :compression-method 0))
	 0 *handshake* #f)))
    (define (send-certificate)
      (tls-socket-send-inner socket
       (make-tls-handshake *certificate*
	(make-tls-certificate (~ socket 'certificates)))
       0 *handshake* #f))
    (define (send-server-key-exchange)
      (let* ((prime  (random-prime (div (*dh-primi-size*) 8) 
				   :prng (~ socket 'prng)))
	     (a (bytevector->integer
		 (read-random-bytes (~ socket 'prng)
				    (- (div (bitwise-length prime) 8) 1))))
	     (params (if (is-dh? (~ socket 'session))
			 (make-tls-server-dh-params 
			  ;; dh-g 2 is ok?
			  (integer->bytevector prime)
			  (integer->bytevector +dh-g+)
			  ;; A = g^a mod p
			  (integer->bytevector (mod-expt +dh-g+ a prime)))
			 (implementation-restriction-violation 
			  'send-server-key-exchange 
			  "DH_RSA is not supported yet"))
		     )
	     (signature (hash (lookup-hash (~ socket 'session)) 
			      (tls-packet->bytevector params))))
	(set! (~ socket 'session 'params) params)
	(set! (~ socket 'session 'a) a)
	(tls-socket-send-inner socket
	 (make-tls-handshake *server-key-echange*
	  (make-tls-server-key-exchange params signature))
	 0 *handshake* #f)))
    (define (send-server-hello-done)
      (tls-socket-send-inner socket
       (make-tls-handshake *server-hello-done*
	(make-tls-server-hello-done))
       0 *handshake* #f))
    (define (process-client-key-exchange socket o)
      (let ((session (~ socket 'session))
	    (dh (~ o 'exchange-keys)))
	(if (is-dh? session)
	    ;; Diffie-Hellman key exchange
	    ;; Ka = B^a mod p
	    ;; calculate client Yc
	    (let* ((Yc (bytevector->integer (~ dh 'dh-public 'value)))
		   (a  (~ socket 'session 'a))
		   (p  (bytevector->integer (~ session 'params 'dh-p))))
	      (let1 K (mod-expt Yc a p)
		(set! (~ session 'master-secret) 
		      (compute-master-secret session (integer->bytevector K)))))
	    (let* ((encrypted-pre-master-secret 
		    (~ dh 'pre-master-secret 'value))
		   (rsa-cipher (cipher RSA (~ socket 'private-key)))
		   (pre-master-secret 
		    (decrypt rsa-cipher encrypted-pre-master-secret)))
	      (set! (~ session 'master-secret)
		    (compute-master-secret session pre-master-secret))))))
    (define (send-server-finished)
      ;; send change cipher spec first
      (tls-socket-send-inner socket (make-tls-change-cipher-spec #x01)
			     0 *change-cipher-spec* #f)
      (let* ((session (~ socket 'session))
	     (out (~ session 'messages))
	     (handshake-messages (get-output-bytevector out))
	     (client-finished (~ session 'client-finished)))
	(set! (~ session 'client-finished) #f) ;; for GC
	(tls-socket-send-inner socket
	 (make-tls-handshake *finished*
	  (make-tls-finished 
	   (finish-message-hash session
	    *server-finished-label* 
	    (bytevector-concat handshake-messages client-finished))))
	 0 *handshake* #t)))
    (let ((hello (read-record socket 0)))
      (unless (tls-client-hello? hello)
	(assertion-violation 'tls-server-handshake
			     "unexpected packet was sent!" hello))
      (process-client-hello! hello)
      (send-server-hello)
      (send-certificate)
      (when (is-dh? (~ socket 'session))
	(send-server-key-exchange))

      ;; TODO send DH param if we need.
      (send-server-hello-done)
      ;; wait for client
      ;; TODO verify certificate if the client sends.
      (let loop ((had-spec? #f))
	(let1 o (read-record socket 0)
	  (if (tls-finished? o)
	      (and (verify-mac socket o *client-finished-label* #t)
		   (send-server-finished))
	      (cond ((tls-change-cipher-spec? o)
		     (calculate-session-key socket)
		     (set! (~ socket 'session 'session-encrypted?) #t)
		     (loop #t))
		    ((tls-client-key-exchange? o)
		     (process-client-key-exchange socket o) 
		     (loop had-spec?))
		    (else
		     (assertion-violation 'tls-handshake
					  "unexpected object" o))))))))

  (define (make-client-tls-socket server service
				  :key (prng (secure-random RC4))
				  (version *tls-version-1.2*)
				  (session #f)
				  :allow-other-keys opt)
    (let* ((raw-socket (apply make-client-socket server service opt))
	   (socket (make <tls-socket> :raw-socket raw-socket
			 :version version :prng prng
			 :session (if session
				      session
				      (make-initial-session prng)))))
      (tls-client-handshake socket)
      socket))


  (define (finish-message-hash session label handshake-messages)
    (PRF session 12 ;; For 1.2 check cipher
	 (~ session 'master-secret) label
	 (if (< (~ session 'version) *tls-version-1.2*)
	     ;; use md5 + sha1
	     (bytevector-concat (hash MD5 handshake-messages)
				(hash SHA-1 handshake-messages))
	     (let1 algo (session-signature-algorithm session #t)
	       ;; TODO I'm not sure this is correct or not
	       (hash algo handshake-messages)))))

  (define (tls-client-handshake socket)
    (define (process-server-hello socket sh)
      (let1 session (~ socket 'session)
	(set! (~ session 'session-id) (~ sh 'session-id))
	(let1 version (~ sh 'version)
	  (when (or (< version *tls-version-1.0*)
		    (> version *tls-version-1.2*))
	    (assertion-violation 'tls-handshake
				 "non supported TLS version" version))
	  (set! (~ session 'version) version))
	(set! (~ session 'server-random) (~ sh 'random))
	(set! (~ session 'cipher-suite) (~ sh 'cipher-suite))
	(set! (~ session 'methods) (~ sh 'compression-method))))

    (define (process-server-key-exchange socket ske)
      ;; TODO check if client certificate has suitable key.
      (let1 session (~ socket 'session)
	(if (is-dh? session)
	    ;; Diffie-Hellman key exchange
	    ;; A = g^a mod p
	    ;; B = g^b mod p
	    ;; Ka = B^a mod p
	    ;; Kb = A^b mod p
	    ;; calculate client Yc
	    (let* ((dh (~ ske 'params))
		   (g (bytevector->integer (~ dh 'dh-g 'value)))
		   (p (bytevector->integer (~ dh 'dh-p 'value)))
		   ;; b must be 0 <= b <= p-2
		   ;; so let me take (- (div (bitwise-length p) 8) 1)
		   ;; ex) if 1024 bit p, then b will be 1016
		   (b (bytevector->integer
		       (read-random-bytes (~ socket 'prng) 
					  (- (div (bitwise-length p) 8) 1))))
		   ;; A
		   (Ys (bytevector->integer (~ dh 'dh-Ys 'value)))
		   ;; B
		   (Yc (mod-expt g b p)))
	      (set! (~ session 'params) 
		    (make <dh-params> :p p :g g :Ys Ys :Yc Yc))
	      ;; compute master secret
	      (let1 K (mod-expt Ys b p)
		(set! (~ session 'master-secret) 
		      (compute-master-secret session (integer->bytevector K)))))
	    ;; TODO RSA?
	    )))

    (define (process-certificate socke tls-certs)
      ;; the first certificate is the server certificate and the rest 
      ;; are chain.
      ;; TODO verify certificates and root CA
      (let ((session (~ socket 'session))
	    (cert (car (~ tls-certs 'certificates))))
	(set! (~ session 'public-key) (x509-certificate-get-public-key cert))))

    (define (make-rsa-key-exchange socket)
      (let* ((session (~ socket 'session))
	     (pre-master-secret (make-bytevector 48))
	     (key (~ session 'public-key)))
	(bytevector-u16-set! pre-master-secret 0 
			     ;; we need to use offered version
			     (~ socket 'version) 'big)
	(bytevector-copy! (read-random-bytes (~ socket 'prng) 46) 0
			  pre-master-secret 2 46)
	(let* ((rsa-cipher (cipher RSA key))
	       (encrypted (encrypt rsa-cipher pre-master-secret)))
	  ;; calculate client master_secret here to avoid unnecessary allocation
	  (set! (~ session 'master-secret) 
		(compute-master-secret session pre-master-secret))
	  (make-tls-handshake *client-key-exchange*
	   (make-tls-client-key-exchange 
	    (make-tls-encrypted-pre-master-secret 
	     (make-variable-vector 2 encrypted)))))))

    (define (make-dh-key-exchange socket)
      (let1 Yc (~ socket 'session 'params 'Yc)
	(make-tls-handshake *client-key-exchange*
	 (make-tls-client-key-exchange
	  (make-tls-client-diffie-hellman-public 
	   (make-variable-vector 2 (integer->bytevector Yc)))))))

    (define (make-client-verify socket)
      (let* ((session (~ socket 'session))
	     (in (~ session 'messages))
	     (position (port-position in))
	     (message (get-output-bytevector in)))
	;; restore
	(put-bytevector in message)
	(if (< (~ session 'version) *tls-version-1.2*)
	    (make-tls-client-verify
	     ;; TODO get it from cipher-suite
	     (let1 rsa-cipher (cipher RSA (~ session 'public-key)
				      ;; block type 1
				      :block-type PKCS-1-EMSA)
	       (sign rsa-cipher
		(bytevector-concat (hash MD5 message) (hash SHA-1 message)))))
	    (make-tls-client-verify
	     (let1 rsa-cipher (cipher RSA (~ session 'public-key)
				      ;; block type 1 (correct?)
				      :block-type PKCS-1-EMSA)
	       ;; TODO 1.2 hash
	       (sign rsa-cipher message))))))

    (define (wait-and-process-server socket)
      (let1 session (~ socket 'session)
	(let loop ((o (read-record socket 0)))
	  ;; finished or server-hello-done is the marker
	  (cond ((tls-server-hello-done? o) #t)
		((tls-finished? o)
		 (verify-mac socket o *server-finished-label* #f))
		(else
		 (cond ((tls-server-hello? o)
			(process-server-hello socket o))
		       ((tls-certificate? o)
			(process-certificate socket o))
		       ((tls-server-key-exchange? o)
			(process-server-key-exchange socket o))
		       ((tls-change-cipher-spec? o)
			(set! (~ session 'session-encrypted?) #t))
		       (else
			(assertion-violation 'tls-handshake
					     "unexpected object" o)))
		 (loop (read-record socket 0)))))))
    
    (let ((hello (make-client-hello socket))
	  (session (~ socket 'session)))
      (tls-socket-send-inner socket hello 0 *handshake* #f)
      (wait-and-process-server socket)
      ;; If server send CertificateRequest
      (when (~ session 'need-certificate?)
	;; TODO send certificate
	(assertion-violation 'tls-handshake
			     "client certificate is not supported yet"))
      ;; client key exchange message
      (tls-socket-send-inner socket 
			     (if (is-dh? session)
				 (make-dh-key-exchange socket)
				 (make-rsa-key-exchange socket))
			     0 *handshake* #f)

      ;; send certificate verify.
      (when (~ session 'need-certificate?)
	(tls-socket-send-inner socket (make-client-verify socket)
			       0 *handshake* #f))

      ;; Change cipher spec
      ;; we need to change session state to *change-cipher-spec*
      (tls-socket-send-inner socket (make-tls-change-cipher-spec #x01)
			     0 *change-cipher-spec* #f)

      ;; finish
      (let* ((out (~ session 'messages))
	     (handshake-messages (get-output-bytevector out)))
	;; add message again
	(put-bytevector out handshake-messages)
	(tls-socket-send-inner 
	 socket
	 (make-tls-handshake *finished*
	  (make-tls-finished (finish-message-hash session
						  *client-finished-label*
						  handshake-messages)))
	 0 *handshake* #t))
      (wait-and-process-server socket)))

  (define (dump-all-handshake-messages msg dh?)
    (let1 in (open-bytevector-input-port msg)
      (let loop ((o (read-handshake in dh?)))
	(when o
	  (display o) (newline)
	  (loop (read-handshake in dh?))))))

  ;; internals
  (define-constant *master-secret-label* (string->utf8 "master secret"))
  (define-constant *client-finished-label* (string->utf8 "client finished"))
  (define-constant *server-finished-label* (string->utf8 "server finished"))
  (define-constant *key-expansion-label* (string->utf8 "key expansion"))

  (define (session-signature-algorithm session consider-version?)
    (let ((version (~ session 'version))
	  (cipher-suite (~ session 'cipher-suite)))
      (or (and-let* ((algorithm (lookup-hash session)))
	    (if consider-version?
		;; we need to use SHA-256 if the version is 1.2
		(if (= version *tls-version-1.2*)
		    SHA-256
		    algorithm)
		algorithm))
	  (assertion-violation 'session-signature-algorithm
			       "non supported cipher suite detected"
			       cipher-suite))))

  (define (bytevector-concat bv1 bv2)
    (let* ((len1 (bytevector-length bv1))
	   (len2 (bytevector-length bv2))
	   (r (make-bytevector (+ len1 len2))))
      (bytevector-copy! bv1 0 r 0 len1)
      (bytevector-copy! bv2 0 r len1 len2)
      r))
  (define (PRF session len secret label seed)
    (define (p-hash hmac seed)
      (let* ((r (make-bytevector len))
	     (size (hash-size hmac))
	     (buf  (make-bytevector size))
	     (count (ceiling (/ len size))))
	(let loop ((i 1) (offset 0) (A1 (hash hmac seed)))
	  (hash! hmac buf (bytevector-concat A1 seed))
	  (cond ((= i count)
		 (bytevector-copy! buf 0 r offset (- len offset))
		 r)
		(else
		 (bytevector-copy! buf 0 r offset size)
		 (loop (+ i 1) (+ offset size) (hash hmac A1)))))))

    (if (< (~ session 'version) *tls-version-1.2*)
	;; TODO for odd, must be one longer (S2)
	(let* ((len (ceiling (/ (bytevector-length secret) 2)))
	       (S1 (bytevector-copy secret 0 len))
	       (S2 (bytevector-copy secret len)))
	  (bytevector-xor
	   (p-hash (hash-algorithm HMAC :key S1 :hash MD5)
		   (bytevector-concat label seed))
	   (p-hash (hash-algorithm HMAC :key S2 :hash SHA-1)
		   (bytevector-concat label seed))))
	(let1 algo (session-signature-algorithm session #t)
	  (p-hash (hash-algorithm HMAC :key secret :hash algo)
		  (bytevector-concat label seed)))))
  
  ;; this can be used by both client and server, if we support server socket...
  (define (compute-master-secret session pre-master-secret)
    (PRF session
	 48
	 pre-master-secret
	 *master-secret-label*
	 (random->bytevector (~ session 'client-random)
			     (~ session 'server-random))))

  (define (read-record socket flags)
    (define (get-decrypt-cipher session)
      (let* ((cipher&keysize (lookup-cipher&keysize session))
	     (session-key (~ session 'session-key))
	     (read-key (generate-secret-key (car cipher&keysize)
					    (~ session-key 'read-key)))
	     (iv (~ session-key 'read-iv))
	     (c (cipher (car cipher&keysize) read-key
			;; we can not use pkcs5padding
			:padder #f
			:iv iv :mode MODE_CBC)))
	;;(slot-set! session 'decrypt-cipher c)
	c))
    (define (decrypt-data session em type)
      (let* ((decrypt-cipher (get-decrypt-cipher session))
	     (message (decrypt decrypt-cipher em))
	     (algo (lookup-hash session))
	     (size (hash-size algo))
	     (len  (bytevector-length message))
	     ;; this must have data, MAC and padding
	     (pad-len (bytevector-u8-ref message (- len 1)))
	     (mac-offset (- len pad-len size 1))
	     (mac  (bytevector-copy message mac-offset (- len pad-len 1)))
	     (data-offset (if (>= (~ session 'version) *tls-version-1.1*)
			      (cipher-blocksize decrypt-cipher)
			      0))
	     (data (bytevector-copy message data-offset mac-offset)))
	(set! (~ session 'session-key 'read-iv) (cipher-iv decrypt-cipher))
	;; verify HMAC from server
	(unless (bytevector=? mac (calculate-read-mac
				   socket type
				   (~ session 'version) data))
	  ;; TODO should we send alert to the server?
	  (assertion-violation 'decrypt-data "MAC verification failed"))
	;; TODO what should we do with iv?
	data))

    (define (recv-n size raw-socket)
      (call-with-bytevector-output-port
       (lambda (p)
	 (let loop ((read-length 0)
		    (diff size))
	   (unless (= read-length size)
	     (let* ((buf (socket-recv raw-socket diff flags))
		    (len (bytevector-length buf)))
	       (put-bytevector p buf)
	       (loop (+ read-length len) (- diff len))))))))

    (let* ((raw-socket (~ socket 'raw-socket))
	   ;; the first 5 octets must be record header
	   (buf (socket-recv raw-socket 5 flags)))
      (unless (= (bytevector-length buf) 5)
	(assertion-violation 'read-record "invalid record header" buf))
      (or
       (and-let* ((type (bytevector-u8-ref buf 0))
		  (version (bytevector-u16-ref buf 1 'big))
		  (size-bv (bytevector-copy buf 3))
		  (size    (bytevector->integer size-bv))
		  (message (recv-n size raw-socket))
		  (session (~ socket 'session)))
	 (unless (= size (bytevector-length message))
	   (assertion-violation 'read-record
				"given size and actual data size is different"
				size (bytevector-length message)))
	 (when (~ session 'session-encrypted?)
	   ;; okey now we are in the secured session
	   (set! message (decrypt-data session message type))
	   ;; we finally read all message from the server now is the time to
	   ;; maintain read sequence number
	   (set! (~ session 'read-sequence) (+ (~ session 'read-sequence) 1)))
	 (cond ((= type *handshake*)
		;; for Finish message
		(let1 type (bytevector-u8-ref message 0)
		  (when (and (not (= type *finished*))
			     (not (= type *hello-request*)))
		    (put-bytevector (~ session 'messages) message))
		  ;; save client finished
		  ;; FIXME this is really ugly ...
		  (when (and (is-a? session <tls-server-session>)
			     (= type *finished*))
		    (set! (~ session 'client-finished) message)))
		(read-handshake (open-bytevector-input-port message)
				(is-dh? session)))
	       ((= type *alert*)
		(let1 alert (read-alert (open-bytevector-input-port message))
		  (cond ((= *close-notify* (~ alert 'description))
			 ;; session closed
			 (set! (~ session 'closed?) #t)
			 ;; user wants application data
			 #vu8())
			(else
			 alert))))
	       ((= type *change-cipher-spec*)
		(read-change-cipher-spec (open-bytevector-input-port message)))
	       ((= type *application-data*)
		;; we can simply return the message
		message)
	       (else
		(assertion-violation 'read-record
				     "not supported yet" type))))
       #vu8())))

  (define (read-variable-vector in n)
    (let* ((size (bytevector->integer (get-bytevector-n in n)))
	   (body (get-bytevector-n in size)))
      (make-variable-vector n (if (eof-object? body) #vu8() body))))

  (define (read-change-cipher-spec in)
    (let1 type (get-u8 in)
      (unless (= type 1)
	(assertion-violation 'read-change-cipher-spec
			     "invalid change cipher spec"))
      (make-tls-change-cipher-spec type)))

  (define (read-alert in)
    (let ((level (get-u8 in))
	  (description (get-u8 in)))
      (make-tls-alert level description)))

  ;; for now
  (define (check-key-exchange-algorithm suite set) (memv suite set))
  (define (is-dh? session)
    (check-key-exchange-algorithm (~ session 'cipher-suite)
				  *dh-key-exchange-algorithms*))
  (define (read-handshake in dh?)

    (define (read-random in)
      (let ((time (bytevector->integer (get-bytevector-n in 4)))
	    (bytes (get-bytevector-n in 28)))
	(make-tls-random bytes time)))

    (define (read-extensions in)
      (define (read-extension in)
	(let ((type (bytevector->integer (get-bytevector-n in 2)))
	      (data (read-variable-vector in 2)))
	  (make-tls-extension type data)))
      (and-let* ((size (get-bytevector-n in 2))
		 ( (bytevector? size) )
		 (len (bytevector->integer size)))
	(do ((i 0 (+ i 1)) (exts '() (cons (read-extension in) exts)))
	    ((= i len)) (make-tls-extensions (reverse! exts)))))

    (define (read-client-hello in)
      (let* ((version (bytevector->integer (get-bytevector-n in 2)))
	     (random  (read-random in))
	     (session-id (read-variable-vector in 1))
	     (cipher-suites (read-variable-vector in 2))
	     (compression-methods (read-variable-vector in 1))
	     (extensions (read-extensions in)))
	(unless (eof-object? (lookahead-u8 in))
	  (assertion-violation 'read-client-hello
			       "could not read client-hello properly."))
	(make-tls-client-hello :version version
			       :random random
			       :session-id session-id
			       :cipher-suites cipher-suites
			       :compression-methods compression-methods
			       :extensions extensions)))

    (define (read-server-key-exchange in)
      ;; check key algorithm
      (if dh?
	  (let ((p (read-variable-vector in 2))
		(g (read-variable-vector in 2))
		(ys (read-variable-vector in 2))
		;; the rest must be signature
		(signature (get-bytevector-all in)))
	    ;; TODO check signature if server sent certificate
	    (make-tls-server-key-exchange
	     (make-tls-server-dh-params p g ys)
	     signature))
	  ;; must be RSA
      	  (let ((rsa-modulus (read-variable-vector in 2))
		(rsa-exponent (read-variable-vector in 2))
		;; the rest must be signature ...
		(signature (get-bytevector-all in)))
	    (make-tls-server-key-exchange
	     (make-tls-server-rsa-params rsa-modulus rsa-exponent)
	     signature))))

    (define (read-server-hello in)
      (let* ((version (bytevector->integer (get-bytevector-n in 2)))
	     (random  (read-random in))
	     (session-id (read-variable-vector in 1))
	     (cipher-suite (bytevector->integer (get-bytevector-n in 2)))
	     (compression-method (get-u8 in))
	     (extensions (read-extensions in)))
	(unless (eof-object? (lookahead-u8 in))
	  (assertion-violation 'read-server-hello
			       "could not read server-hello properly."))
	(make-tls-server-hello :version version
			       :random random
			       :session-id session-id
			       :cipher-suite cipher-suite
			       :compression-method compression-method
			       :extensions extensions)))
    (define (read-client-key-exchange in)
      (let* ((body (get-bytevector-all in)))
	(make-tls-client-key-exchange
	 ((if dh? 
	      make-tls-client-diffie-hellman-public
	      make-tls-encrypted-pre-master-secret)
	  (read-variable-vector (open-bytevector-input-port body) 2)))))

    (define (read-certificate in)
      ;; we don't check the length, i trust you...
      (let1 total-length (bytevector->integer (get-bytevector-n in 3))
	(let loop ((i 0) (certs '()) (read-size 0))
	  (if (eof-object? (lookahead-u8 in))
	      (make-tls-certificate (reverse! certs))
	      (let* ((size (bytevector->integer (get-bytevector-n in 3)))
		     (body (get-bytevector-n in size))
		     (cert (make-x509-certificate body)))
		(loop (+ i 1) (cons cert certs) (+ read-size size)))))))

    (define (read-finished in) (make-tls-finished (get-bytevector-all in)))

    (and-let* ((type (get-u8 in))
	       ( (integer? type) )
	       (size (bytevector->integer (get-bytevector-n in 3)))
	       (body (get-bytevector-n in size)))
      (unless (or (zero? size) (= size (bytevector-length body)))
	(assertion-violation 'read-handshake
			     "given size and actual data size is different"
			     size))
      (cond ((= type *hello-request*) (make-tls-hello-request))
	    ((= type *client-hello*)
	     (read-client-hello (open-bytevector-input-port body)))
	    ((= type *server-hello*)
	     (read-server-hello (open-bytevector-input-port body)))
	    ((= type *certificate*)
	     (read-certificate (open-bytevector-input-port body)))
	    ((= type *server-key-echange*)
	     (read-server-key-exchange (open-bytevector-input-port body)))
	    ((= type *server-hello-done*)
	     (make-tls-server-hello-done))
	    ((= type *client-key-exchange*)
	     (read-client-key-exchange (open-bytevector-input-port body)))
	    ((= type *finished*)
	     (read-finished (open-bytevector-input-port body)))
	    (else
	     (assertion-violation 'read-handshake
				  "not supported" type)))))

  (define (lookup-cipher&keysize session)
    (and-let* ((suite (assv (~ session 'cipher-suite) *cipher-suites*)))
      (caddr suite)))
  (define (lookup-hash session)
    (and-let* ((suite (assv (~ session 'cipher-suite) *cipher-suites*)))
      (cadddr suite)))

  (define (random->bytevector random1 random2)
    (bytevector-concat
     (call-with-bytevector-output-port 
      (lambda (p) (write-tls-packet random1 p)))
     (call-with-bytevector-output-port 
      (lambda (p) (write-tls-packet random2 p)))))
  ;; dummy
  (define (exportable? session) #f)

  (define (dump-hex bv :optional (title #f))
    (when title (display title)(newline))
    (let1 len (bytevector-length bv)
      (format #t "length ~d~%" len)
      (dotimes (i len)
	(format #t "~2,'0X " (bytevector-u8-ref bv i))
	(when (zero? (mod (+ i 1) 16))
	  (newline)))
      (newline)))

  (define (calculate-session-key socket)
    (define (process-key-block! key-block session-key
				keysize blocksize hashsize
				client?)
      #|
       client_write_MAC_secret[SecurityParameters.hash_size]
       server_write_MAC_secret[SecurityParameters.hash_size]
       client_write_key[SecurityParameters.key_material_length]
       server_write_key[SecurityParameters.key_material_length]
       client_write_IV[SecurityParameters.IV_size]
       server_write_IV[SecurityParameters.IV_size]
      |#
      (let1 slot-set (if client?
			 `((write-mac-secret . ,hashsize)
			   (read-mac-secret . ,hashsize)
			   (write-key . ,keysize)
			   (read-key . ,keysize)
			   (write-iv . ,blocksize)
			   (read-iv . ,blocksize))
			 `((read-mac-secret . ,hashsize)
			   (write-mac-secret . ,hashsize)
			   (read-key . ,keysize)
			   (write-key . ,keysize)
			   (read-iv . ,blocksize)
			   (write-iv . ,blocksize)))
	(let loop ((offset 0) (set slot-set))
	  (unless (null? set)
	    (let1 end (+ offset (cdar set))
	      (set! (~ session-key (caar set))
		    (bytevector-copy key-block offset end))
	      (loop end (cdr set)))))))

    (let* ((session        (~ socket 'session))
	   (cipher&keysize (lookup-cipher&keysize session))
	   (keysize        (cdr cipher&keysize))
	   (dummy (cipher (car cipher&keysize)
			  (generate-secret-key
			   (car cipher&keysize)
			   (make-bytevector keysize))))
	   (blocksize      (cipher-blocksize dummy)) ;; iv-size
	   (hash           (lookup-hash session))
	   (hashsize       (hash-size hash))
	   (block-size     (* 2 (+ keysize hashsize blocksize)))
	   (key-block      (PRF session block-size
				(~ session 'master-secret)
				*key-expansion-label*
				(random->bytevector
				 (~ session 'server-random)
				 (~ session 'client-random))))
	   (session-key    (make <session-key>)))
      (process-key-block! key-block session-key keysize blocksize hashsize
			  (not (is-a? socket <tls-server-socket>)))
      (set! (~ session 'session-key) session-key)
      session-key))

  (define (calculate-mac session type version seq secret-deriver body)
    (let* ((algo (hash-algorithm HMAC :key (secret-deriver session)
				 :hash (lookup-hash session)))
	   (bv (make-bytevector (hash-size algo))))
      (hash! algo bv 
	     (let1 data
		 (call-with-bytevector-output-port
		  (lambda (p)
		    (put-u64 p (~ session seq))
		    (put-u8 p type)
		    (put-u16 p version)
		    (put-u16 p (bytevector-length body))
		    (put-bytevector p body)))
	       data))
      bv))

  (define (calculate-write-mac socket type version body)
    (define (derive-write-mac-secret session)
      (let1 session-key (~ session 'session-key)
	(unless session-key
	  (calculate-session-key socket)
	  (set! session-key (~ session 'session-key)))
	(~ session-key 'write-mac-secret)))
    (calculate-mac (~ socket 'session) type version 'write-sequence
		   derive-write-mac-secret body))

  (define (calculate-read-mac socket type version body)
    (define (derive-read-mac-secret session)
      (let1 session-key (~ session 'session-key)
	;; if we reach here, means we must have session key
	(~ session-key 'read-mac-secret)))
    (calculate-mac (~ socket 'session) type version 'read-sequence
		   derive-read-mac-secret body))

  ;; until here the key-block must be calculated
  (define (derive-final-write-key session label)
    (let1 session-key (~ session 'session-key)
      (if (exportable? session)
	  (or (and-let* ((key (~ session-key 'final-wirte-key)))
		key)
	      (let* ((keysize (lookup-cipher&keysize session))
		     (key (PRF session (cdr keysize)
			       (~ session-key 'write-key)
			       label 
			       (random->bytevector
				(~ session 'client-random)
				(~ session 'server-random)))))
		(set! (~ session-key 'final-wirte-key) key)
		key)))))

  (define (derive-final-read-key session label)
    (let1 session-key (~ session 'session-key)
      (if (exportable? session)
	  (or (and-let* ((key (~ session-key 'final-read-key)))
		key)
	      (let* ((keysize (lookup-cipher&keysize session))
		     (key (PRF session (cdr keysize)
			       (~ session-key 'read-key)
			       label 
			       (random->bytevector
				(~ session 'client-random)
				(~ session 'server-random)))))
		(set! (~ session-key 'final-read-key) key)
		key))
	  (~ session-key 'read-key))))
  
  ;; SSL/TLS send packet on record layer protocl
  (define (tls-socket-send socket data :optional (flags 0))
    (when (tls-socket-closed? socket)
      (assertion-violation 'tls-socket-send
			   "tls socket is alresy closed"))
    (tls-socket-send-inner socket data flags *application-data* #t))

  (define (tls-socket-send-inner socket data flags type encrypt?)
    (define (calculate-padding cipher len)
      (let* ((block-size (cipher-blocksize cipher))
	     (size (- block-size (mod len block-size))))
	(make-bytevector size size)))

    (define (encrypt-data session version data)
      (define (get-encrypt-cipher)
	(let* ((cipher&keysize (lookup-cipher&keysize session))
	       (session-key (~ session 'session-key))
	       (write-key (generate-secret-key (car cipher&keysize)
					       (~ session-key 'write-key)))
	       (iv (~ session-key 'write-iv))
	       (c (cipher (car cipher&keysize) write-key
			  ;; we need to pad by our self ... hmm
			  :padder #f
			  :iv iv :mode MODE_CBC)))
	  ;;(slot-set! session 'encrypt-cipher c)
	  c))
      ;; all toplevel data structures have the same slots.
      (let* ((body (if (= type *application-data*)
		       data
		       (call-with-bytevector-output-port
			(lambda (p)
			  (write-tls-packet data p)))))
	     (version (~ session 'version))
	     (hash-algo (lookup-hash session))
	     (mac (calculate-write-mac socket type version body))
	     (encrypt-cipher (get-encrypt-cipher))
	     (padding (calculate-padding encrypt-cipher
					 (+ (bytevector-length body)
					    (bytevector-length mac) 1)))
	     (em (encrypt encrypt-cipher
			  (call-with-bytevector-output-port
			   (lambda (p)
			     (when (>= (~ session 'version) *tls-version-1.1*)
			       ;; add IV
			       (put-bytevector p (cipher-iv encrypt-cipher)))
			     (put-bytevector p body)
			     (put-bytevector p mac)
			     (put-bytevector p padding)
			     (put-u8 p (bytevector-length padding)))))))
	(set! (~ session 'session-key 'write-iv) (cipher-iv encrypt-cipher))
	(make-tls-ciphered-data em)))
    
    (let* ((session (~ socket 'session))
	   (version (negotiated-version socket))
	   (record (make-tls-record-layer type version
					  (if encrypt?
					      (encrypt-data session version
							    data)
					      data)))
	   (packet (call-with-bytevector-output-port
		    (lambda (p) (write-tls-packet record p)))))
      (when encrypt?
	(set! (~ session 'write-sequence) (+ (~ session 'write-sequence) 1)))
      (when (and (tls-handshake? data)
		 (not (tls-hello-request? (tls-handshake-body data))))
	(write-tls-packet data (~ session 'messages)))
      (let1 raw-socket (~ socket 'raw-socket)
	(socket-send raw-socket packet flags))))

  ;; this is only used from out side of the world, means the received message
  ;; is always application data.
  (define (tls-socket-recv socket size :optional (flags 0))
    (or (and-let* ((in (~ socket 'buffer))
		   (buf (get-bytevector-n in size))
		   ( (not (eof-object? buf)) ))
	  ;; if the actual read size was equal or less than the requires size
	  ;; the buffer is now empty so, we need to set the slot #f
	  (when (< (bytevector-length buf) size)
	    (set! (~ socket 'buffer) #f))
	  buf)
	(and-let* ((record (read-record socket flags))
		   ( (bytevector? record) )
		   (in (open-bytevector-input-port record)))
	  (set! (~ socket 'buffer) in)
	  (get-bytevector-n in size))
	;; something is wrong
	(assertion-violation 'tls-socket-recv "invalid socket state")))

  (define (send-alert socket level description)
    (let1 alert (make-tls-alert level description)
      (tls-socket-send-inner socket alert 0 *alert*
			     (~ socket 'session 'session-encrypted?))))

  (define (tls-socket-close socket)
    (send-alert socket *warning* *close-notify*)
    (socket-close (~ socket 'raw-socket))
    ;; if we don't have any socket, we can't reconnect
    (set! (~ socket 'raw-socket) #f))

  ;; to make call-with-socket available for tls-socket
  (define-method socket-close ((o <tls-socket>))
    (tls-socket-close o))

  )