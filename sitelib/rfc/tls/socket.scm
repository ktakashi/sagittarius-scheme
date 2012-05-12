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
	    tls-socket-send
	    tls-socket-recv
	    tls-socket-close
	    tls-socket-closed?
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius socket)
	    (sagittarius control)
	    (rfc tls types)
	    (rfc tls constant)
	    (rfc x.509)
	    (rfc hmac)
	    (util bytevector)
	    (math)
	    (crypto)
	    (clos user)
	    (srfi :19 time))

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

  (define-class <tls-socket> ()
    ((raw-socket :init-keyword :raw-socket)
     ;; should this be in session?
     (version    :init-keyword :version :init-value *ssl-version*)
     (prng       :init-keyword :prng)
     (session    :init-keyword :session)
     (socket-type :init-keyword :socket-type)
     ;; for tls-socket-recv, we need to store application data
     ;; in this buffer to be able to take size argument.
     (buffer     :init-value #f)))

  (define-method write-object ((o <tls-socket>) out)
    (let1 session (slot-ref o 'session)
      (format out "#<tls-socket ~x~a~a>" 
	      (slot-ref session 'version)
	      (if (slot-ref session 'closed?)
		  " session-closed"
		  "")
	      (if (slot-ref o 'raw-socket)
		  ""
		  " closed"))))
  (define (tls-socket-closed? socket)
    (let1 session (slot-ref socket 'session)
      (slot-ref session 'closed?)))

  (define (negotiated-version socket)
    (let1 session (slot-ref socket 'session)
      (or (and-let* ((version (slot-ref session 'version)))
	    version)
	  (slot-ref socket 'version))))

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
	   (session (slot-ref socket 'session))
	   (random-bytes (read-random-bytes (slot-ref socket 'prng) 28))
	   (random (make-tls-random random-bytes))
	   (session-id (make-variable-vector 1 (slot-ref session 'session-id)))
	   (cipher-suite (make-variable-vector 2 (make-cipher-suites)))
	   (compression-methods (make-variable-vector 
				 1 (slot-ref session 'methods)))
	   (hello (apply make-tls-client-hello
			 :version version
			 :random random
			 :session-id session-id
			 :cipher-suites cipher-suite
			 :compression-methods compression-methods
			 (if extension
			     (list :extensions extension)
			     '()))))
      (slot-set! session 'client-random random)
      (make-tls-handshake *client-hello* hello)))

  (define (make-initial-session prng)
    (make <tls-session>
      :session-id (read-random-bytes prng 28)
      :methods #vu8(0)))

  (define (make-client-tls-socket server service
				  :key (prng (secure-random RC4))
				  (version *ssl-version*)
				  (session #f)
				  :allow-other-keys opt)
    (let* ((raw-socket (apply make-client-socket server service opt))
	   (socket (make <tls-socket> :raw-socket raw-socket
			 :version version :prng prng
			 :socket-type 'client
			 :session (if session
				      session
				      (make-initial-session prng)))))
      (tls-handshake socket)
      socket))

  (define (tls-handshake socket)
    (define (process-server-hello socket sh)
      (let1 session (slot-ref socket 'session)
	(slot-set! session 'session-id (slot-ref sh 'session-id))
	(slot-set! session 'version (slot-ref sh 'version))
	(slot-set! session 'server-random (slot-ref sh 'random))
	(slot-set! session 'cipher-suite (slot-ref sh 'cipher-suite))
	(slot-set! session 'methods (slot-ref sh 'compression-method))))

    (define (process-server-key-exchange socket ske)
      ;; TODO check if client certificate has suitable key.
      (let1 session (slot-ref socket 'session)
	(if (is-dh? session)
	    ;; Diffie-Hellman key exchange
	    ;; A = g^a mod p
	    ;; B = g^b mod p
	    ;; Ka = B^a mod p
	    ;; Kb = A^b mod p
	    ;; calculate client Yc
	    (let* ((dh (slot-ref ske 'params))
		   (g (bytevector->integer 
		       (slot-ref (slot-ref dh 'dh-g) 'value)))
		   (p (bytevector->integer
		       (slot-ref (slot-ref dh 'dh-p) 'value)))
		   ;; b must be 0 <= b <= p-2
		   ;; so let me take (- (div (bitwise-length p) 8) 1)
		   ;; ex) if 1024 bit p, then b will be 1016
		   (b (bytevector->integer
		       (read-random-bytes (slot-ref socket 'prng) 
					  (- (div (bitwise-length p) 8) 1))))
		   ;; A
		   (Ys (bytevector->integer
			(slot-ref (slot-ref dh 'dh-Ys) 'value)))
		   ;; B
		   (Yc (mod-expt g b p)))
	      ;; these values must be stored somewhere, but where?
	      (slot-set! session 'params (make <dh-params> :p p :g g
						:Ys Ys :Yc Yc))
	      ;; compute master secret
	      (let1 K (mod-expt Ys b p)
		(slot-set! session 'master-secret 
			   (compute-master-secret session
						  (integer->bytevector K)))))
	    
	    ;; TODO RSA?
	    )))
	  

    (define (process-certificate socke tls-certs)
      ;; the first certificate is the server certificate and the rest 
      ;; are chain.
      ;; TODO verify certificates and root CA
      (let ((session (slot-ref socket 'session))
	    (cert (car (slot-ref tls-certs 'certificates))))
	(slot-set! session 'public-key (x509-certificate-get-public-key cert))))

    (define (make-rsa-key-exchange socket)
      (let* ((session (slot-ref socket 'session))
	     (pre-master-secret (make-bytevector 48))
	     (key (slot-ref session 'public-key)))
	(bytevector-u16-set! pre-master-secret 0 
			     ;; we need to use offered version
			     (slot-ref socket 'version) 'big)
	(bytevector-copy! (read-random-bytes (slot-ref socket 'prng) 46) 0
			  pre-master-secret 2 46)
	(let* ((rsa-cipher (cipher RSA key))
	       (encrypted (encrypt rsa-cipher pre-master-secret)))
	  ;; calculate client master_secret here to avoid unnecessary allocation
	  (slot-set! session 'master-secret 
		     (compute-master-secret session pre-master-secret))
	  (make-tls-handshake *client-key-exchange*
	   (make-tls-client-key-exchange 
	    (make-tls-encrypted-pre-master-secret encrypted))))))

    (define (make-dh-key-exchange socket)
      (let* ((session (slot-ref socket 'session))
	     (Yc (slot-ref (slot-ref session 'params) 'Yc)))
	(make-tls-handshake *client-key-exchange*
	 (make-tls-client-key-exchange
	  (make-tls-client-diffie-hellman-public 
	   (make-variable-vector 2 (integer->bytevector Yc)))))))


    (define (make-client-verify socket)
      (let* ((session (slot-ref socket 'session))
	     (in (slot-ref session 'messages))
	     (position (port-position in))
	     (message (get-output-bytevector in)))
	;; restore
	(put-bytevector in message)
	(if (< (slot-ref session 'version) #x0303)
	    (make-tls-client-verify
	     ;; TODO get it from cipher-suite
	     (let1 rsa-cipher (cipher RSA (slot-ref session 'public-key)
				      ;; block type 1
				      :block-type PKCS-1-EMSA)
	       (sign rsa-cipher
		(bytevector-concat (hash MD5 message) (hash SHA-1 message)))))
	    (make-tls-client-verify
	     (let1 rsa-cipher (cipher RSA (slot-ref session 'public-key)
				      ;; block type 1 (correct?)
				      :block-type PKCS-1-EMSA)
	       (sign rsa-cipher message))))))

    (define (wait-and-process-server socket)
      (let loop ((o (read-record socket 0)))
	;; finished or server-hello-done is the marker
	(cond ((tls-server-hello-done? o) #t)
	      ((tls-finished? o)
	       ;; TODO verify
	       )
	      (else
	       (cond ((tls-server-hello? o)
		      (process-server-hello socket o))
		     ((tls-certificate? o)
		      (process-certificate socket o))
		     ((tls-server-key-exchange? o)
		      (process-server-key-exchange socket o))
		     ((tls-change-cipher-spec? o)
		      (slot-set! (slot-ref socket 'session)
				 'session-encrypted?
				 #t))
		     (else
		      (assertion-violation 'tls-handshake
					   "unexpected object" o)))
	       (loop (read-record socket 0))))))
    
    (let ((hello (make-client-hello socket))
	  (session (slot-ref socket 'session)))
      (tls-socket-send-inner socket hello 0 *handshake* #f)
      (wait-and-process-server socket)

      ;; If server send CertificateRequest
      (when (slot-ref session 'need-certificate?)
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
      (when (slot-ref session 'need-certificate?)
	(tls-socket-send-inner socket (make-client-verify socket)
			       0 *handshake* #f))

      ;; Change cipher spec
      ;; we need to change session state to *change-cipher-spec*
      (tls-socket-send-inner socket (make-tls-change-cipher-spec #x01)
			     0 *change-cipher-spec* #f)

      ;; finish
      (let1 handshake-messages 
	  (get-output-bytevector (slot-ref session 'messages)) 
	(define (hash-message handshake-messages)
	  (if (< (slot-ref session 'version) #x0303)
	      ;; use md5 + sha1
	      (bytevector-concat (hash MD5 handshake-messages)
				 (hash SHA-1 handshake-messages))
	      (let1 algo (session-signature-algorithm session #t)
		;; TODO I'm not sure this is correct or not
		(hash algo handshake-messages))))

	(tls-socket-send-inner 
	 socket
	 (make-tls-handshake *finished*
	  (make-tls-finished (PRF session
				  12 ;; TODO check cipher
				  (slot-ref session 'master-secret)
				  *finished-label*
				  (hash-message handshake-messages))))
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
  (define-constant *finished-label* (string->utf8 "client finished"))
  (define-constant *key-expansion-label* (string->utf8 "key expansion"))

  (define (session-signature-algorithm session consider-version?)
    (let ((version (slot-ref session 'version))
	  (cipher-suite (slot-ref session 'cipher-suite)))
      (or (and-let* ((algorithm (lookup-hash session)))
	    (if consider-version?
		;; we need to use SHA-256 if the version is 1.2
		(if (= version #x0303)
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

    (if (< (slot-ref session 'version) #x0303)
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
	 (random->bytevector (slot-ref session 'client-random)
			     (slot-ref session 'server-random))))

  (define (read-record socket flags)
    (define (get-decrypt-cipher session)
      (let* ((cipher&keysize (lookup-cipher&keysize session))
		 (session-key (slot-ref session 'session-key))
		 (read-key (generate-secret-key 
			     (car cipher&keysize)
			     (slot-ref session-key 'read-key)))
		 (iv (slot-ref session-key 'read-iv))
		 (c (cipher (car cipher&keysize) read-key
			    ;; we can not use pkcs5padding
			    :padder #f
			    :iv iv :mode MODE_CBC)))
	;;(slot-set! session 'decrypt-cipher c)
	c))
    (define (decrypt-data session em)
      (let* ((decrypt-cipher (get-decrypt-cipher session))
	     (message (decrypt decrypt-cipher em))
	     (algo (lookup-hash session))
	     (size (hash-size algo))
	     (len  (bytevector-length message))
	     ;; this must have data, MAC and padding
	     (pad-len (bytevector-u8-ref message (- len 1)))
	     (mac-offset (- len pad-len size 1))
	     (mac  (bytevector-copy message mac-offset
				    (- len pad-len 1)))
	     (data-offset (if (>= (slot-ref session 'version) #x0302)
			      (cipher-blocksize decrypt-cipher)
			      0))
	     (data (bytevector-copy message data-offset mac-offset)))
	(slot-set! (slot-ref session 'session-key) 'read-iv
		   (cipher-iv decrypt-cipher))
	;; TODO verify
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

    (let* ((raw-socket (slot-ref socket 'raw-socket))
	   ;; the first 5 octets must be record header
	   (buf (socket-recv raw-socket 5 flags)))
      (unless (= (bytevector-length buf) 5)
	(assertion-violation 'read-record
			     "invalid record header" buf))
      (or
       (and-let* ((type (bytevector-u8-ref buf 0))
		  (version (bytevector->integer 
			    (bytevector-copy buf 1 3)))
		  (size-bv (bytevector-copy buf 3))
		  (size    (bytevector->integer size-bv))
		  (message (recv-n size raw-socket))
		  (session (slot-ref socket 'session)))
	 (unless (= size (bytevector-length message))
	   (assertion-violation 'read-record
				"given size and actual data size is different"
				size (bytevector-length message)))
	 ;; we finally read all message from the server now is the time to
	 ;; maintain read sequence number
	 (slot-set! session 'read-sequence 
		    (+ (slot-ref session 'read-sequence)1))
	 (when (slot-ref session 'session-encrypted?)
	   ;; okey now we are in the secured session
	   (set! message (decrypt-data session message)))
	 (cond ((= type *handshake*)
		;; for Finish message
		(when (not (= (bytevector-u8-ref message 0) *hello-request*))
		  (let1 out (slot-ref session 'messages)
		    (put-bytevector out message)))
		(read-handshake (open-bytevector-input-port message)
				(is-dh? session)))
	       ((= type *alert*)
		(let1 alert (read-alert (open-bytevector-input-port message))
		  (cond ((= *close-notify* (slot-ref alert 'description))
			 ;; session closed
			 (slot-set! session 'closed? #t)
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
    (check-key-exchange-algorithm (slot-ref session 'cipher-suite)
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
      (let* ((size (bytevector->integer (get-bytevector-n in 3)))
	     (body (get-bytevector-n in size)))
	(make-tls-client-key-exchange
	 (make-tls-encrypted-pre-master-secret
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
		(loop (+ i 1)
		      (cons cert certs)
		      (+ read-size size)))))))

    (define (read-finished in)
      (make-tls-finished (get-bytevector-all in)))

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
    (and-let* ((suite (assv (slot-ref session 'cipher-suite) *cipher-suites*)))
      (caddr suite)))
  (define (lookup-hash session)
    (and-let* ((suite (assv (slot-ref session 'cipher-suite) *cipher-suites*)))
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
	      (slot-set! session-key (caar set)
			 (bytevector-copy key-block offset end))
	      (loop end (cdr set)))))))

    (let* ((session        (slot-ref socket 'session))
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
				(slot-ref session 'master-secret)
				*key-expansion-label*
				(random->bytevector
				 (slot-ref session 'server-random)
				 (slot-ref session 'client-random))))
	   (session-key    (make <session-key>)))
      (process-key-block! key-block session-key keysize blocksize hashsize
			  (eq? (slot-ref socket 'socket-type) 'client))
      (slot-set! session 'session-key session-key)
      session-key))

  (define (calculate-write-mac socket type version body)
    (define (derive-write-mac-secret session)
      (let1 session-key (slot-ref session 'session-key)
	(unless session-key
	  (calculate-session-key socket)
	  (set! session-key (slot-ref session 'session-key)))
	(slot-ref session-key 'write-mac-secret)))
    (let* ((session (slot-ref socket 'session))
	   (write-secret (derive-write-mac-secret session))
	   (algo (hash-algorithm HMAC :key write-secret
				 :hash (lookup-hash session)))
	   (bv (make-bytevector (hash-size algo))))
      (hash! algo bv 
	     (let1 data
		 (call-with-bytevector-output-port
		  (lambda (p)
		    (put-u64 p (slot-ref session 'write-sequence))
		    (put-u8 p type)
		    (put-u16 p version)
		    (put-u16 p (bytevector-length body))
		    (put-bytevector p body)))
	       data))
      bv))

  ;; until here the key-block must be calculated
  (define (derive-final-write-key session label)
    (let1 session-key (slot-ref session 'session-key)
      (if (exportable? session)
	  (or (and-let* ((key (slot-ref session-key 'final-wirte-key)))
		key)
	      (let* ((keysize (lookup-cipher&keysize session))
		     (key (PRF session (cdr keysize)
			       (slot-ref session-key 'write-key)
			       label 
			       (random->bytevector
				(slot-ref session 'client-random)
				(slot-ref session 'server-random)))))
		(slot-set! session-key 'final-wirte-key key)
		key))
	  )))
  (define (derive-final-read-key session label)
    (let1 session-key (slot-ref session 'session-key)
      (if (exportable? session)
	  (or (and-let* ((key (slot-ref session-key 'final-read-key)))
		key)
	      (let* ((keysize (lookup-cipher&keysize session))
		     (key (PRF session (cdr keysize)
			       (slot-ref session-key 'read-key)
			       label 
			       (random->bytevector
				(slot-ref session 'client-random)
				(slot-ref session 'server-random)))))
		(slot-set! session-key 'final-read-key key)
		key))
	  (slot-ref session-key 'read-key))))
  
  ;; SSL/TLS send packet on record layer protocl
  (define (tls-socket-send socket data flags)
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
		   (session-key (slot-ref session 'session-key))
		   (write-key (generate-secret-key 
			       (car cipher&keysize)
			       (slot-ref session-key 'write-key)))
		   (iv (slot-ref session-key 'write-iv))
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
	     (version (slot-ref session 'version))
	     (hash-algo (lookup-hash session))
	     (mac (calculate-write-mac socket type version body))
	     (encrypt-cipher (get-encrypt-cipher))
	     (padding (calculate-padding encrypt-cipher
					 (+ (bytevector-length body)
					    (bytevector-length mac) 1)))
	     (em (encrypt encrypt-cipher
			  (call-with-bytevector-output-port
			   (lambda (p)
			     (when (>= (slot-ref session 'version) #x0302)
			       ;; add IV
			       (put-bytevector p (cipher-iv encrypt-cipher)))
			     (put-bytevector p body)
			     (put-bytevector p mac)
			     (put-bytevector p padding)
			     (put-u8 p (bytevector-length padding)))))))
	(slot-set! (slot-ref session 'session-key) 'write-iv
		   (cipher-iv encrypt-cipher))
	(make-tls-ciphered-data em)))
    
    (let* ((session (slot-ref socket 'session))
	   (version (negotiated-version socket))
	   (record (make-tls-record-layer type version
					  (if encrypt?
					      (encrypt-data session version
							    data)
					      data)))
	   (packet (call-with-bytevector-output-port
		    (lambda (p) (write-tls-packet record p)))))
      (when encrypt?
	(slot-set! session 'write-sequence
		   (+ (slot-ref session 'write-sequence) 1)))
      (when (and (tls-handshake? data)
		 (not (tls-hello-request? (tls-handshake-body data)))
		 (not (tls-finished? (tls-handshake-body data))))
	(write-tls-packet data (slot-ref session 'messages)))
      (let1 raw-socket (slot-ref socket 'raw-socket)
	(socket-send raw-socket packet flags))))

  ;; this is only used from out side of the world, means the received message
  ;; is always application data.
  (define (tls-socket-recv socket size flags)
    (or (and-let* ((in (slot-ref socket 'buffer))
		   (buf (get-bytevector-n in size))
		   ( (not (eof-object? buf)) ))
	  ;; if the actual read size was equal or less than the requires size
	  ;; the buffer is now empty so, we need to set the slot #f
	  (when (< (bytevector-length buf) size)
	    (slot-set! socket 'buffer #f))
	  buf)
	(and-let* ((record (read-record socket flags))
		   ( (bytevector? record) )
		   (in (open-bytevector-input-port record)))
	  (slot-set! socket 'buffer in)
	  (get-bytevector-n in size))
	;; something is wrong
	(assertion-violation 'tls-socket-recv
			     "invalid socket state")))

  (define (send-alert socket level description)
    (let1 alert (make-tls-alert level description)
      (tls-socket-send-inner socket alert 0 *alert*
			     (slot-ref (slot-ref socket 'session)
				       'session-encrypted?))))

  (define (tls-socket-close socket)
    (send-alert socket *warning* *close-notify*)
    (socket-close (slot-ref socket 'raw-socket))
    ;; if we don't have any socket, we can't reconnect
    (slot-set! socket 'raw-socket #f))

  )