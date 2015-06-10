;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/mqtt/broker.scm - MQTT v3.1.1 simple broker
;;;
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (net mq mqtt broker)
    (export make-mqtt-broker
	    make-mqtt-broker-config
	    mqtt-broker-start!
	    mqtt-broker-stop!
	    <mqtt-broker>
	    <mqtt-broker-config>)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius socket)
	    (net mq mqtt packet)
	    (net mq mqtt broker api)
	    (net server)
	    ;; for generic socket-close
	    (rfc tls))
  (define-class <mqtt-broker> (<simple-server> <mqtt-broker-context>)
    ;; private slot
    ((handlers :init-keyword :handlers)))
  (define-class <mqtt-broker-config> (<server-config>)
    ())

  ;; at least make some threads otherwise sort of useless...
  (define (make-mqtt-broker-config :key (max-thread 10) :allow-other-keys opt)
    (apply make <mqtt-broker-config> :max-thread max-thread
	   :non-blocking? #t  opt))

  (define (make-mqtt-broker port :key (config (make-mqtt-broker-config))
			    :allow-other-keys rest)
    (define (setup-handlers)
      (let ((ht (make-eqv-hashtable)))
	(hashtable-set! ht +publish+ mqtt-broker-publish)
	(hashtable-set! ht +puback+ mqtt-broker-puback)
	(hashtable-set! ht +pubrec+ mqtt-broker-pubrec)
	(hashtable-set! ht +pubrel+ mqtt-broker-pubrel)
	(hashtable-set! ht +pubcomp+ mqtt-broker-pubcomp)
	(hashtable-set! ht +subscribe+ mqtt-broker-subscribe)
	(hashtable-set! ht +unsubscribe+ mqtt-broker-unsubscribe)
	(hashtable-set! ht +pingreq+ mqtt-broker-pingreq)
	(hashtable-set! ht +disconnect+ mqtt-broker-disconnect!)
	ht))
    (define socket-table (make-eq-hashtable))
    (define (mqtt-handler server socket)
      (let ((in/out (socket-port socket #f)))
	(guard (e (else (socket-close socket)
			;; this must be internal or client packet error
			;; should we also broad case will message?
			(raise e)))
	  (let-values (((type flags len) (read-fixed-header in/out)))
	    ;; when connection is down, we don't invalidate the
	    ;; session for recovery purpose
	    (cond ((not type) 
		   (and-let* ((session (hashtable-ref socket-table socket #f)))
		     (mqtt-broker-will-message session))
		   (socket-close socket))
		  ((hashtable-ref (~ server 'handlers) type) =>
		   (lambda (handler)
		     (let ((session (hashtable-ref socket-table socket #f)))
		       (if (and session (mqtt-session-alive? session))
			   (handler session type flags len in/out)
			   (begin
			     (hashtable-delete! socket-table socket)
			     (socket-close socket))))))
		  ((= type +connect+)
		   (let ((session (mqtt-broker-connect! server 
							type flags len in/out)))
		     (if session
			 (hashtable-set! socket-table socket session)
			 (begin
			   (socket-close socket)
			   (error 'mqtt-connect "failed to connect")))))
		  (else
		   (error 'mqtt-handler "unknown packet type"
			  type)))))))
    (apply make-simple-server port mqtt-handler
	   :server-class <mqtt-broker>
	   :config config
	   :handlers (setup-handlers)
	   rest))

  ;; just forwarding :)
  (define mqtt-broker-start! server-start!)
  (define mqtt-broker-stop! server-stop!)
  (define-method on-server-stop! ((broker <mqtt-broker>) . ignore)
    (mqtt-session-cleaner-stop! broker))

)
