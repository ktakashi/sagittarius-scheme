;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/mqtt/broker.scm - MQTT v3.1.1 simple broker
;;;
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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
	    (net server))
  (define-class <mqtt-broker> (<simple-server> <mqtt-broker-context>)
    ;; private slot
    ((handlers :init-keyword :handlers)))
  (define-class <mqtt-broker-config> (<server-config>)
    ())

  ;; at least make some threads otherwise sort of useless...
  (define (make-mqtt-broker-config :key (max-thread 10) :allow-other-keys opt)
    (apply make <mqtt-broker-config> :max-thread max-thread opt))

  (define-condition-type &connection &error make-connection-error
    connection-error?
    (socket connection-socket))
  (define (connection-error socket)
    (raise (condition (make-connection-error socket)
		      (make-who-condition 'mqtt-broker)
		      (make-message-condition "Connection is broken"))))

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
    (define (mqtt-handler server socket)
      (define (unsupported)
	(error 'mqtt-handler "not supported yet"))
      (let ((in/out (socket-port socket)))
	(and-let* ((session (mqtt-broker-connect! server in/out)))
	  (guard (e ((connection-error? e)
		     (mqtt-broker-will-message session)
		     (raise e))
		    (else 
		     ;; this must be internal or client packet error
		     ;; should we also broad case will message?
		     (raise e)))
	    (let loop ()
	      ;; as long as session is alive.
	      (when (mqtt-session-alive? session)
		(let-values (((type flags len) (read-fixed-header in/out)))
		  ;; when connection is down, we don't invalidate the
		  ;; session for recovery purpose
		  (cond ((not type) (connection-error socket)) 
			((hashtable-ref (~ server 'handlers) type) =>
			 (lambda (handler)
			   (handler session type flags len in/out)
			   (loop)))
			(else
			 (error 'mqtt-handler "unknown packet type"
				type))))))))))
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
