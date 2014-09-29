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
	    <mqtt-broker-server>
	    <mqtt-broker-server-config>)
    (import (rnrs)
	    (clos user)
	    (sagittarius object)
	    (sagittarius socket)
	    (net mq mqtt packet)
	    (net mq mqtt broker api)
	    (net server))
  (define-class <mqtt-broker> (<simple-server>)
    (context))
  (define-class <mqtt-broker-config> (<server-config>)
    ())

  (define (make-mqtt-broker-config . opt)
    (apply make <mqtt-broker-config> opt))

  (define (make-mqtt-broker port :optional (config (make-mqtt-broker-config)))
    (define (mqtt-handler server socket)
      (let ((in/out (socket-port socket)))
	(mqtt-broker-connect! (~ server 'context) in/out)
	(let loop ()
	  (let-values (((type flags len) (read-fixed-header in/out)))
	    (error 'mqtt-handler "not supported yet" type)))))

    (let ((server (make-simple-server port mqtt-handler
				      :server-class <mqtt-broker>
				      config)))
      (set! (~ server 'context) (make-mqtt-broker-context))
      server))

)
