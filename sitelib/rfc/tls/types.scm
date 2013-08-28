;;; -*- Scheme -*-
;;;
;;; rfc/tls/types.scm - TLS 1.0 - 1.2 protocol library.
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

;; Caution this library is not well tested and not secure yet.
(library (rfc tls types)
    (export write-tls-packet
	    make-tls-record-layer
	    ;; helper
	    make-variable-vector
	    ;; handshake 
	    make-tls-handshake
	    ;; handshake message types
	    make-tls-change-cipher-spec
	    make-tls-alert
	    make-tls-hello-request
	    make-tls-client-hello
	    make-tls-random
	    make-tls-server-hello
	    make-tls-extensions
	    make-tls-extension
	    make-tls-certificate
	    make-tls-server-rsa-params
	    make-tls-server-dh-params
	    make-tls-server-key-exchange
	    make-tls-server-hello-done
	    make-tls-client-key-exchange
	    make-tls-client-verify
	    make-tls-signature
	    make-tls-signature-with-algorhtm
	    make-tls-encrypted-pre-master-secret
	    make-tls-client-diffie-hellman-public
	    make-tls-certificate-request
	    make-tls-finished
	    make-tls-ciphered-data

	    ;; extensions
	    make-tls-server-name
	    make-tls-server-name-list
	    ;; predicate
	    tls-handshake?
	    tls-hello-request?
	    tls-client-hello?
	    tls-server-hello?
	    tls-certificate?
	    tls-server-key-exchange?
	    tls-server-hello-done?
	    tls-change-cipher-spec?
	    tls-client-key-exchange?
	    tls-certificate-request?
	    tls-client-veify?
	    tls-finished?
	    ;; handshake accessor
	    tls-handshake-body
	    tls-finished-data
	    ;; utility
	    put-u16
	    put-u32
	    put-u64 ;; for sequence number
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius control)
	    (clos user)
	    (rfc x.509)
	    (srfi :19 time)
	    (srfi :39 parameters))

  ;; for display parameter
  (define *start* (make-parameter #t))
  (define *indent* (make-parameter 0))
  (define (write-indent off out)
    (dotimes (i (+ (*indent*) off))
      (display #\space out)))

  ;; helper (always put as big endian)
  (define (put-un out v n)
    (let ((count (/ n 8)) (diff  (- n 8)))
      (do ((end (+ count 1))
	   (i 1 (+ i 1))
	   (v v (bitwise-arithmetic-shift v 8)))
	  ((= i end))
	(let1 u8 (bitwise-and (bitwise-arithmetic-shift-right v diff) #xFF)
	  (put-u8 out u8)))))
  ;; for convenience
  (define (put-u16 out u16) (put-un out u16 16))
  (define (put-u24 out u24) (put-un out u24 24))
  (define (put-u32 out u32) (put-un out u32 32))
  (define (put-u64 out u64) (put-un out u64 64))

  ;; base class
  (define-class <tls-packet-component> () ())
  (define-method write-tls-packet ((p <tls-packet-component>) out)
    (assertion-violation 'write-tls-packet
			 "subclass must implement this method"))

  ;; Record Layer
  (define-class <tls-record-layer> (<tls-packet-component>)
    ((type    :init-keyword :type)
     (version :init-keyword :version)
     (message :init-keyword :message)))
  (define-method write-tls-packet ((o <tls-record-layer>) out)
    (put-u8 out (slot-ref o 'type))
    (put-u16 out (slot-ref o 'version))
    (let1 m (call-with-bytevector-output-port
	     (lambda (p)
	       (write-tls-packet (slot-ref o 'message) p)))
      (put-u16 out (bytevector-length m))
      (put-bytevector out m)))
  (define (make-tls-record-layer type version message)
    (make <tls-record-layer> :type type :version version :message message))

  ;; The direct message in record layer.
  ;; for encryption we must have the same slots.
  (define-class <tls-toplevel> (<tls-packet-component>)
    ((type   :init-keyword :type)
     (body   :init-keyword :body)))
  (define-method write-tls-packet ((o <tls-toplevel>) out)
    (put-u8 out (slot-ref o 'type))
    (let* ((body (slot-ref o 'body))
	   ;; if the body is encrypted, then it is a bytevecotor
	   (bv (if (bytevector? body)
		   body
		   (call-with-bytevector-output-port 
		    (lambda (p)
		      (write-tls-packet body p)))))
	   (len (bytevector-length bv)))
      (put-u24 out len)
      (put-bytevector out bv)))

  ;; Handshake
  (define-class <tls-handshake> (<tls-toplevel>) ())
  (define (make-tls-handshake type body)
    (make <tls-handshake> :type type :body body))
  (define (tls-handshake? o) (is-a? o <tls-handshake>))
  (define (tls-handshake-body o) (slot-ref o 'body))

  ;; helper
  (define-class <variable-vector> (<tls-packet-component>)
    ((length-size :init-keyword :length-size)
     (value       :init-keyword :value)))
  (define-method write-tls-packet ((o <variable-vector>) out)
    (let ((bv (slot-ref o 'value))
	  (size (slot-ref o 'length-size)))
      (put-un out (bytevector-length bv) (* size 8))
      (put-bytevector out bv)))
  (define (make-variable-vector size bv)
    (make <variable-vector> :length-size size :value bv))
  (define-method write-object ((o <variable-vector>) out)
    (when (*start*) (display "#<variable-vector "))
    (display (~ o 'value) out)
    (when (*start*) (display "> ")))

  ;; Change Cipher Spec
  (define-class <tls-change-cipher-spec> (<tls-packet-component>)
    ((type :init-keyword :type)))
  (define-method write-tls-packet ((o <tls-change-cipher-spec>) out)
    (put-u8 out (slot-ref o 'type)))
  (define (make-tls-change-cipher-spec type)
    (make <tls-change-cipher-spec> :type type))
  (define (tls-change-cipher-spec? o) (is-a? o <tls-change-cipher-spec>))

  ;; Alert
  (define-class <tls-alert> (<tls-packet-component>)
    ((level :init-keyword :level)
     (description :init-keyword :description)))
  (define-method write-tls-packet ((o <tls-alert>) out)
    (put-u8 out (slot-ref o 'level))
    (put-u8 out (slot-ref o 'description)))
  (define (make-tls-alert level description)
    (make <tls-alert> :level level :description description))
  (define-method write-object ((o <tls-alert>) out)
    (format out "#<tls-alert ~d ~a>"
	    (slot-ref o 'level)
	    (slot-ref o 'description)))

  ;; Hello Request
  (define-class <tls-hello-request> (<tls-packet-component>) ())
  ;; hello request must not contain any data
  (define-method write-tls-packet ((o <tls-hello-request>) out))
  (define (make-tls-hello-request) (make <tls-hello-request>))
  (define (tls-hello-request? o) (is-a? o <tls-hello-request>))

  ;; Client Hello
  ;; Random
  (define-class <tls-random> (<tls-packet-component>)
    ((unix-time :init-keyword :unix-time)
     (random-bytes :init-keyword :random-bytes)))
  (define-method write-tls-packet ((o <tls-random>) out)
    (put-u32 out (slot-ref o 'unix-time))
    (put-bytevector out (slot-ref o 'random-bytes)))
  (define (make-tls-random bytes :optional (time (time-second (current-time))))
    (make <tls-random> :unix-time time :random-bytes bytes))
  (define-method write-object ((o <tls-random>) out)
    (write-indent 0 out)
    (when (*start*) (display "#<" out))
    (display "tls-random" out) (newline out)
    (write-indent 2 out)
    (format out "time ~a~%" (slot-ref o 'unix-time))
    (write-indent 2 out)
    (format out "bytes ~a" (slot-ref o 'random-bytes))
    (when (*start*) (display ">" out)))

  (define-class <tls-client-hello> (<tls-packet-component>)
    ((version    	  :init-keyword :version)
     (random     	  :init-keyword :random)
     (session-id 	  :init-keyword :session-id)
     (cipher-suites       :init-keyword :cipher-suites)
     ;; use null method
     (compression-methods :init-keyword :compression-methods)
     (extensions          :init-keyword :extensions :init-value '())))
  (define-method write-tls-packet ((o <tls-client-hello>) out)
    (put-u16 out (~ o 'version))
    (write-tls-packet (~ o 'random) out)
    (write-tls-packet (~ o 'session-id) out)
    (write-tls-packet (~ o 'cipher-suites) out)
    (write-tls-packet (~ o 'compression-methods) out)
    (let1 ext (~ o 'extensions)
      (unless (null? ext)
	(let1 bv (call-with-bytevector-output-port
		  (^o (for-each (^d (write-tls-packet d o)) ext)))
	  (write-tls-packet (make-variable-vector 2 bv) out)))))
  (define (make-tls-client-hello . opt) (apply make <tls-client-hello> opt))
  (define (tls-client-hello? o) (is-a? o <tls-client-hello>))

  ;; Server Hello
  (define-class <tls-server-hello> (<tls-packet-component>)
    ((version             :init-keyword :version)
     (random              :init-keyword :random)
     (session-id          :init-keyword :session-id)
     (cipher-suite        :init-keyword :cipher-suite)
     (compression-method  :init-keyword :compression-method)
     (extensions          :init-keyword :extensions :init-value '())))
  (define-method write-tls-packet ((o <tls-server-hello>) out)
    (put-u16 out (slot-ref o 'version))
    (write-tls-packet (slot-ref o 'random) out)
    (write-tls-packet (slot-ref o 'session-id) out)
    ;; cipher-suite is uint8[2]
    (put-u16 out (slot-ref o 'cipher-suite))
    (put-u8 out (slot-ref o 'compression-method))
    (let1 ext (~ o 'extensions)
      (unless (null? ext)
	(let1 bv (call-with-bytevector-output-port
		  (^o (for-each (^d (write-tls-packet d o)) ext)))
	  (write-tls-packet (make-variable-vector 2 bv) out)))))
  (define (make-tls-server-hello . opt)
    (apply make <tls-server-hello> opt))
  (define-method write-object ((o <tls-server-hello>) out)
    (write-indent 0 out)
    (when (*start*) (display "#<" out))
    (parameterize ((*start* #f)
		   (*indent* (+ (*indent*) 2)))
      (format out "server-hello(~x)~%" (slot-ref o 'version))
      (display (slot-ref o 'random) out) (newline out)
      (write-indent 0 out)
      (format out "session-id ~a~%" (slot-ref o 'session-id))
      (write-indent 0 out)
      (format out "cipher-suite ~4,'0x~%" (slot-ref o 'cipher-suite))
      (write-indent 0 out)
      (format out "compression-method ~a~%" (slot-ref o 'compression-method))
      (and-let* ((ext (slot-ref o 'extensions)))
	(for-each (lambda (e) (display e out) (newline out)) ext)))
    (when (*start*) (display ">" out)))
  (define (tls-server-hello? o) (is-a? o <tls-server-hello>))


  ;; Extension
  (define-class <tls-extensions> (<tls-packet-component>)
    ((extensions :init-keyword :extensions)))
  ;; TODO write-tls-packet
  (define (make-tls-extensions extensions)
    (make <tls-extensions> :extensions extensions))
  (define-class <tls-extension> (<tls-packet-component>)
    ((type :init-keyword :type)
     (data :init-keyword :data)))
  (define (make-tls-extension type data)
    (make <tls-extension> :type type :data data))
  (define-method write-tls-packet ((o <tls-extension>) out)
    (put-u16 out (~ o 'type))
    (write-tls-packet (~ o 'data) out))

  ;; RFC6066 extensions
  (define-class <server-name> (<tls-packet-component>)
    ((name-type :init-keyword :name-type)
     (name      :init-keyword :name)))
  (define (make-tls-server-name type name)
    (make <server-name>
      :name-type type
      :name (make-variable-vector 2 (string->utf8 name))))
  (define-method write-tls-packet ((o <server-name>) out)
    ;; we don't check type assume only host name for now.
    (let1 bv (call-with-bytevector-output-port
	      (lambda (out)
		(put-u8 out (~ o 'name-type))
		(write-tls-packet (~ o 'name) out)))
      (put-u16 out (bytevector-length bv))
      (put-bytevector out bv)))
  (define-class <server-name-list> (<tls-packet-component>)
    ((server-name-list :init-keyword :server-name-list :init-value '())))
  (define (make-tls-server-name-list names)
    (let1 bv (call-with-bytevector-output-port
	      (^o (for-each (^n (write-tls-packet n o)) names)))
      (make <server-name-list> :server-name-list (make-variable-vector 2 bv))))
  (define-method write-tls-packet ((o <server-name-list>) out)
    (write-tls-packet (~ o 'server-name-list) out))

  ;; Server Certificate
  (define-class <tls-certificate> (<tls-packet-component>)
    ((certificates :init-keyword :certificates)))
  (define-method write-tls-packet ((o <tls-certificate>) out)
    (let1 certs (~ o 'certificates)
      (write-tls-packet 
       (make-variable-vector 3
	(call-with-bytevector-output-port
	 (^o
	  (for-each (^c (let1 body (x509-certificate->bytevector c)
			  (write-tls-packet (make-variable-vector 3 body) o)))
		    certs))))
       out)))
  (define (make-tls-certificate certificates)
    (make <tls-certificate> :certificates certificates))
  (define-method write-object ((o <tls-certificate>) out)
    (write-indent 0 out)
    (when (*start*) (display "#<" out))
    (display "tls-certificate" out) (newline)
    (display (~ o 'certificates) out) (newline)
    (when (*start*) (display ">" out)))
  (define (tls-certificate? o) (is-a? o <tls-certificate>))

  ;; Server RSA Params
  (define-class <tls-server-rsa-params> (<tls-packet-component>)
    ((modulus :init-keyword :modulus)
     (exponent :init-keyword :exponent)))
  (define (make-tls-server-rsa-params modulus exponent)
    (make <tls-server-rsa-params> :modulus modulus :exponent exponent))

  ;; Server DH Params
  (define-class <tls-server-dh-params> (<tls-packet-component>)
    ((dh-p :init-keyword :dh-p)
     (dh-g :init-keyword :dh-g)
     (dh-Ys :init-keyword :dh-Ys)))
  (define (make-tls-server-dh-params p g ys)
    (make <tls-server-dh-params> :dh-p p :dh-g g :dh-Ys ys))
  (define-method write-tls-packet ((o <tls-server-dh-params>) out)
    (write-tls-packet (make-variable-vector 2 (~ o 'dh-p)) out)
    (write-tls-packet (make-variable-vector 2 (~ o 'dh-g)) out)
    (write-tls-packet (make-variable-vector 2 (~ o 'dh-Ys)) out))

  ;; Server Key Exchange
  (define-class <tls-server-key-exchange> (<tls-packet-component>)
    ((params :init-keyword :params)
     (signed-params :init-keyword :signed-params)))
  (define (make-tls-server-key-exchange params signed-params)
    (make <tls-server-key-exchange> :params params 
	  :signed-params signed-params))
  (define-method write-tls-packet ((o <tls-server-key-exchange>) out)
    (write-tls-packet (~ o 'params) out)
    (put-bytevector out (~ o 'signed-params)))
  (define (tls-server-key-exchange? o) (is-a? o <tls-server-key-exchange>))

  ;; Server Hello done
  (define-class <tls-server-hello-done> (<tls-packet-component>) ())
  (define-method write-tls-packet ((o <tls-server-hello-done>) out))
  (define (make-tls-server-hello-done) (make <tls-server-hello-done>))
  (define (tls-server-hello-done? o) (is-a? o <tls-server-hello-done>))

  ;; Client Key Exchange
  (define-class <tls-client-key-exchange> (<tls-packet-component>)
    ((exchange-keys :init-keyword :exchange-keys)))
  (define-method write-tls-packet ((o <tls-client-key-exchange>) out)
    (write-tls-packet (slot-ref o 'exchange-keys) out))
  (define (make-tls-client-key-exchange keys)
    (make <tls-client-key-exchange> :exchange-keys keys))
  (define (tls-client-key-exchange? o) (is-a? o <tls-client-key-exchange>))

  ;; Client Verify
  ;; since TLS 1.2 verify just contains signed handshake messages,
  ;; but TLS 1.0 and 1.1 contains md5 and sha-1 hash. hmmm...
  ;; We just contain bytevector and let processer decide what contain.
  (define-class <tls-client-verify> (<tls-packet-component>)
    ((signature :init-keyword :signature)))
  (define-method write-tls-packet ((o <tls-client-verify>) out)
    (write-tls-packet (~ o 'signature) out))
  (define (make-tls-client-verify signature)
    (make <tls-client-verify> :signature signature))
  (define (tls-client-veify? o) (is-a? o <tls-client-verify>))

  (define-class <signature> (<tls-packet-component>)
    ((signature :init-keyword :signature)))
  (define-method write-tls-packet ((o <signature>) out)
    (write-tls-packet (make-variable-vector 2 (~ o 'signature)) out))
  (define (make-tls-signature signature)
    (make <signature> :signature signature))

  (define-class <signature-with-algorithm> (<signature>)
    ((hash :init-keyword :hash)
     (algorithm :init-keyword :algorithm)))
  (define-method write-tls-packet ((o <signature-with-algorithm>) out)
    (put-u8 out (~ o 'hash))
    (put-u8 out (~ o 'algorithm))
    (call-next-method))
  (define (make-tls-signature-with-algorhtm hash algorithm signature)
    (make <signature-with-algorithm>
      :hash hash
      :algorithm algorithm
      :signature signature))

  ;; PreMasterSecret
  ;; the value will be immediately encrypted so we don't need this.
  ;;  (define-class <tls-pre-master-secret> (<tls-packet-component>)
  ;;    ((version :init-keyword :version)
  ;;     (random  :init-keyword :random)))
  (define-class <tls-encrypted-pre-master-secret> (<tls-packet-component>)
    ((pre-master-secret :init-keyword :pre-master-secret)))
  (define-method write-tls-packet ((o <tls-encrypted-pre-master-secret>) out)
    (write-tls-packet (slot-ref o 'pre-master-secret) out))
  (define (make-tls-encrypted-pre-master-secret bv)
    ;; RFC 5246 page 59 Implementation note.
    (make <tls-encrypted-pre-master-secret> :pre-master-secret bv))

  ;; ClientDiffieHellmanPublic
  (define-class <tls-client-diffie-hellman-public> (<tls-packet-component>)
    ((dh-public :init-keyword :dh-public)))
  (define-method write-tls-packet ((o <tls-client-diffie-hellman-public>) out)
    (let1 public (~ o 'dh-public)
      ;; if implicit it does not have any value
      (when public
	(write-tls-packet public out))))
  (define (make-tls-client-diffie-hellman-public public)
    (make <tls-client-diffie-hellman-public> :dh-public public))

  ;; CertificateRequest
  (define-class <tls-certificate-request> (<tls-packet-component>)
    ((certificate-types :init-keyword :certificate-types)
     (supported-signature-algorithms 
      :init-keyword :supported-signature-algorithms
      :init-value #f)
     (certificate-authorities :init-keyword :certificate-authorities)))
  (define (tls-certificate-request? o) (is-a? o <tls-certificate-request>))
  (define-method write-tls-packet ((o <tls-certificate-request>) out)
    ;; I haven't decided how we should hold this
    (write-tls-packet (~ o 'certificate-types) out)
    (when (~ o 'supported-signature-algorithms)
      (write-tls-packet (~ o 'supported-signature-algorithms) out))
    (write-tls-packet (~ o 'certificate-authorities) out))
  (define make-tls-certificate-request
    (case-lambda
     ((type name)
      ;; TLS 1.0 to 1.1
      (make <tls-certificate-request>
	:certificate-types type
	:certificate-authorities name))
     ((type algorithms name)
      ;; TLS 1.2 or later (maybe)
      (make <tls-certificate-request>
	:certificate-types type
	:supported-signature-algorithms algorithms
	:certificate-authorities name))))

  ;; Finished
  (define-class <tls-finished> (<tls-packet-component>)
    ((verify-data :init-keyword :verify-data)))
  (define-method write-tls-packet ((o <tls-finished>) out)
    (put-bytevector out (slot-ref o 'verify-data)))
  (define (make-tls-finished data) (make <tls-finished> :verify-data data))
  (define (tls-finished? o) (is-a? o <tls-finished>))
  (define (tls-finished-data o) (slot-ref o 'verify-data))

  ;; ciphered data
  ;; this class is just for convenience
  (define-class <tls-ciphered-data> (<tls-packet-component>)
    ((data :init-keyword :data)))
  (define-method write-tls-packet ((o <tls-ciphered-data>) out)
    (put-bytevector out (slot-ref o 'data)))
  (define (make-tls-ciphered-data data)
    (make <tls-ciphered-data> :data data))

  )