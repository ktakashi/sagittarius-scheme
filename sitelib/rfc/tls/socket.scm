;;; -*- Scheme -*-
;;;
;;; rfc/tls/socket.scm - TLS 1.0 - 1.2 protocol library.
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

;; Caution this library is not well tested and not well secure yet.
#!nounbound
(library (rfc tls socket)
    (export make-client-tls-socket
	    make-server-tls-socket make-server-tls-socket*

	    tls-socket?
	    tls-socket-send tls-socket-send/range
	    tls-socket-recv
	    tls-socket-recv!
	    tls-socket-shutdown
	    tls-socket-close
	    tls-socket-closed?
	    tls-socket-accept
	    tls-socket-peer
	    tls-socket-name
	    tls-socket-node
	    tls-socket-service
	    tls-socket-info
	    tls-socket-info-values
	    call-with-tls-socket
	    tls-socket-peer-certificate
	    tls-socket-selected-alpn
	    <tls-socket>
	    ;; blocking
	    tls-socket-nonblocking!
	    tls-socket-blocking!
	    tls-socket-set-read-timeout!
	    tls-socket-get-read-timeout
	    nonblocking-tls-socket?
	    tls-socket-client-certificate-callback-set!

	    ;; for the user who wants to specify TSL version
	    *tls-version-1.2*
	    *tls-version-1.1*
	    *tls-version-1.0*

	    socket-close
	    socket-closed?
	    socket-shutdown
	    socket-send socket-send/range
	    socket-recv
	    socket-recv!
	    socket-accept
	    call-with-socket
	    socket-peer
	    socket-name
	    socket-node
	    socket-service
	    socket-info
	    socket-info-values
	    socket-nonblocking!
	    socket-blocking!
	    nonblocking-socket?
	    socket-set-read-timeout!
	    socket-read-select
	    socket-write-select
	    socket-error-select
	    
	    ;; to send handshake explicitly
	    tls-server-handshake
	    tls-client-handshake

	    ;; socket conversion
	    socket->tls-socket

	    ;; hello extension helper
	    make-hello-extension
	    make-server-name-indication
	    make-protocol-name-list

	    tls-socket-raw-socket ;; don't use this casually
	    )
    (import (rnrs)
	    (rfc tls constant)
	    (rfc tls types)
	    (sagittarius)
	    (rename (sagittarius tls-socket)
		    (socket->tls-socket tls:socket->tls-socket)
		    (tls-server-socket-handshake tls-server-handshake)
		    (tls-socket-peer-certificate %tls-socket-peer-certificate)
		    (tls-socket-client-certificate-callback-set!
		     tls:tls-socket-client-certificate-callback-set!))
	    (sagittarius socket)
	    (prefix (only (sagittarius socket)
			  socket-read-select
			  socket-write-select
			  socket-error-select
			  socket-accept) socket:)
	    (sagittarius control)
	    (sagittarius object)
	    (clos user)
	    (rename (sagittarius crypto keys)
		    (*key:rsa* RSA)
		    (key-pair-private keypair-private))
	    (sagittarius crypto random)
	    (srfi :1)
	    (srfi :19)
	    (rfc x509))


(define (make-server-tls-socket port certificates :key
      			  (private-key #f)
      			  (authorities '()) ;; trust store i guess.
      			  (client-certificate-required? #f)
      			  (certificate-verifier #f)
      			  :allow-other-keys opt)
  (let ((s (apply make-server-socket port opt)))
    (socket->tls-socket s 
     :certificates certificates
     :private-key private-key
     :client-socket #f
     :authorities authorities
     :peer-certificate-required? client-certificate-required?
     :certificate-verifier certificate-verifier)))

(define (make-server-tls-socket* port certificates :key
      				 (private-key #f)
      				 (authorities '()) ;; trust store i guess.
      				 (client-certificate-required? #f)
      				 (certificate-verifier #f)
      				 :allow-other-keys opt)
  (map (lambda (s)
         (socket->tls-socket s
	  :certificates certificates
      	  :private-key private-key
      	  :client-socket #f
      	  :authorities authorities
      	  :peer-certificate-required? client-certificate-required?
      	  :certificate-verifier certificate-verifier))
       (apply make-server-socket* port opt)))

(define (make-client-tls-socket server service :key
      				(handshake #t)
      				(certificates '())
      				(private-key #f)
      				(hello-extensions '())
      				(certificate-verifier #f)
      				:allow-other-keys opt)
  (let ((s (apply make-client-socket server service opt)))
    (socket->tls-socket s :certificates certificates
      			:private-key private-key
      			:handshake handshake
      			:client-socket #t
      			:hello-extensions hello-extensions
      			:peer-certificate-required? #t
      			:certificate-verifier certificate-verifier)))

(define (make-hello-extension type data)
  (make-tls-extension type (make-variable-vector 2 data)))

(define (make-server-name-indication names)
  (let1 names (map (^n (make-tls-server-name *host-name* n)) names)
    (make-tls-extension *server-name* (make-tls-server-name-list names))))
(define (make-protocol-name-list names)
  (let1 names (map (^n (make-tls-protocol-name n)) names)
    (make-tls-extension *application-layer-protocol-negotiation*
      			(make-tls-protocol-name-list names))))

(define 1year (make-time time-duration 0 (* 1 60 60 24 365)))
(define system-prng (pseudo-random-generator *prng:system*))
(define (read-sys-random bits)
  (let ((size (div (+ bits 7) 8)))
    (random-generator-read-random-bytes system-prng size)))
(define (socket->tls-socket socket
      			    :key (client-socket #t)
      			    (handshake #t)
      			    (hello-extensions '())
      			    (certificates '())
      			    (private-key #f)
      			    (authorities '())
      			    (peer-certificate-required? #t)
      			    (certificate-verifier #f)
      			    :allow-other-keys opt)
  (define (certificates->bytevector certificates)
    (map x509-certificate->bytevector certificates))
  (define (backward-compatiblity pkey certs)
    (if (or pkey client-socket)
        (values pkey certs)
        ;; once upon a time, I misunderstood the requirement of the
        ;; certificates and private key. thus, some of the TLS server
        ;; doesn't have private key. such TLS servers (must) never be
        ;; in real world other than testing, so we generates self signed
        ;; certificates here
        (let* ((ks (generate-key-pair RSA))
      	       (cert (make-x509-basic-certificate ks
		      (bytevector->uinteger (read-sys-random 32))
      		      (make-x509-issuer '((C . "NL")))
		      (make-validity (current-date)
      				     (time-utc->date
      				      (add-duration! (current-time) 1year)))
		      (make-x509-issuer '((C . "NL"))))))
          ;; TODO show warning message
          (values (keypair-private ks) (list cert)))))
  (let-values (((pkey certs)
      		(backward-compatiblity private-key certificates)))
    (let ((r (tls:socket->tls-socket socket
	      :certificates (certificates->bytevector certs)
      	      :private-key (and pkey (export-private-key pkey)))))
      (tls-socket-authorities-set! r
	(map x509-certificate->bytevector authorities))
      (tls-socket-peer-certificate-verifier-set! r
	peer-certificate-required? certificate-verifier)
      (if (and client-socket handshake)
          (tls-client-handshake r :hello-extensions hello-extensions)
          r))))

(define (tls-client-handshake socket :key (hello-extensions '()))
  (define (parse-extension hello-extensions)
    (define (retrieve-sni snil)
      ;; supports only one server for now...
      (let ((lis (filter-map (lambda (s)
      			       (and (eqv? *host-name* (slot-ref s 'name-type))
      				    (utf8->string
      				     (slot-ref (slot-ref s 'name) 'value))))
      			     (slot-ref snil 'server-name-list))))
        (car lis)))
    (define (retrieve-alpn alpn)
      (let-values (((out e) (open-bytevector-output-port)))
        (write-tls-packet alpn out)
        (e)))
    (let loop ((extensions hello-extensions)
      	       (sni #f) (alpn #f))
      (if (null? extensions)
          `(,@(if sni `(:domain-name ,sni) '())
            ,@(if alpn `(:alpn ,alpn) '()))
          (let* ((e (car extensions))
      		 (t (slot-ref e 'type))
      		 (d (slot-ref e 'data)))
            (cond ((eqv? *server-name* t)
      		   (loop (cdr extensions) (retrieve-sni d) alpn))
      		  ((eqv? *application-layer-protocol-negotiation* t)
      		   (loop (cdr extensions) sni (retrieve-alpn d)))
      		  (else (loop (cdr extensions) sni alpn)))))))
  (let ((opts (parse-extension hello-extensions)))
    (and (apply tls-socket-connect! socket opts)
         socket)))

(define (tls-socket-peer-certificate socket)
  (let ((bv (%tls-socket-peer-certificate socket)))
    (and bv (make-x509-certificate bv))))

(define (tls-socket-client-certificate-callback-set! socket callback)
  (tls:tls-socket-client-certificate-callback-set! socket
   (lambda (socket)
     (let ((r (callback socket)))
       (and (pair? r) (private-key? (car r))
	    (pair? (cdr r)) (for-all x509-certificate? (cdr r))
	    (cons (export-private-key (car r))
      		  (map x509-certificate->bytevector (cdr r))))))))

(define (call-with-tls-socket socket proc)
  (let-values ((args (proc socket)))
    (tls-socket-close socket)
    (apply values args)))

(define (tls-socket-peer socket)
  (socket-peer (~ socket 'raw-socket)))
(define (tls-socket-name socket)
  (socket-name (~ socket 'raw-socket)))
(define (tls-socket-node socket)
  (socket-node (~ socket 'raw-socket)))
(define (tls-socket-service socket)
  (socket-service (~ socket 'raw-socket)))
(define (tls-socket-info socket)
  (socket-info (~ socket 'raw-socket)))
(define (tls-socket-info-values socket :key (type 'peer))
  (socket-info-values (~ socket 'raw-socket) :type type))

(define (tls-socket-nonblocking! socket)
  (socket-nonblocking! (~ socket 'raw-socket)))
(define (tls-socket-blocking! socket)
  (socket-blocking! (~ socket 'raw-socket)))
(define (nonblocking-tls-socket? socket)
  (nonblocking-socket? (~ socket 'raw-socket)))
(define (tls-socket-set-read-timeout! tls-socket timeout)
  (socket-set-read-timeout! (slot-ref tls-socket 'raw-socket) timeout))
(define (tls-socket-get-read-timeout tls-socket)
  (socket-get-read-timeout (slot-ref tls-socket 'raw-socket)))

;; to make call-with-socket available for tls-socket
(define-method socket-close ((o <tls-socket>))
  (tls-socket-close o))
(define-method socket-closed? ((o <tls-socket>))
  (tls-socket-closed? o))
(define-method socket-shutdown ((o <tls-socket>) how)
  (tls-socket-shutdown o how))
(define-method socket-send ((o <tls-socket>) data :optional (flags 0))
  (tls-socket-send o data flags))
(define-method socket-send/range ((o <tls-socket>) data . args)
  (apply tls-socket-send/range o data args))
(define-method socket-recv ((o <tls-socket>) size :optional (flags 0))
  (tls-socket-recv o size flags))
(define-method socket-recv! ((o <tls-socket>) bv start len
      			     :optional (flags 0))
  (tls-socket-recv! o bv start len flags))
(define-method socket-accept ((o <tls-socket>) . opt)
  (apply tls-socket-accept o opt))
;; To avoid no-next-method error
(define-method socket-accept ((o <socket>) (key <keyword>) . dummy)
  (socket:socket-accept o))

(define-method call-with-socket ((o <tls-socket>) proc)
  (call-with-tls-socket o proc))

(define-method socket-peer ((o <tls-socket>))
  (tls-socket-peer o))
(define-method socket-name ((o <tls-socket>))
  (tls-socket-name o))
(define-method socket-node ((o <tls-socket>))
  (tls-socket-node o))
(define-method socket-service ((o <tls-socket>))
  (tls-socket-service o))
(define-method socket-info ((o <tls-socket>))
  (tls-socket-info o))
(define-method socket-info-values ((o <tls-socket>) . opt)
  (apply tls-socket-info-values o opt))

(define-method socket-nonblocking! ((o <tls-socket>))
  (tls-socket-nonblocking! o))
(define-method socket-blocking! ((o <tls-socket>))
  (tls-socket-blocking! o))
(define-method nonblocking-socket? ((o <tls-socket>))
  (nonblocking-tls-socket? o))
(define-method socket-set-read-timeout! ((o <tls-socket>) timeout)
  (tls-socket-set-read-timeout! o timeout))
(define-method socket-get-read-timeout ((o <tls-socket>))
  (tls-socket-get-read-timeout o))

(define (select-sockets selector timeout sockets)
  (define mapping (make-eq-hashtable))
  (for-each (lambda (s)
      	      (hashtable-set! mapping
	       (if (tls-socket? s) (slot-ref s 'raw-socket) s) s)) sockets)
  (let ((raw-sockets (apply selector timeout
      			    (hashtable-keys-list mapping))))
    (filter-map (lambda (s) (hashtable-ref mapping s #f)) raw-sockets)))
(define-method socket-read-select (timeout . rest)
  (select-sockets socket:socket-read-select timeout rest))
(define-method socket-write-select (timeout . rest)
  (select-sockets socket:socket-write-select timeout rest))
(define-method socket-error-select (timeout . rest)
  (select-sockets socket:socket-error-select timeout rest))
)
