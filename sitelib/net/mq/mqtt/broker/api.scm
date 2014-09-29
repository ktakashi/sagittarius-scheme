;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/mqtt/broker/api.scm - MQTT v3.1.1 broker APIs
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

;; reference
;;  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/mqtt-v3.1.1.html

;; Broker requires a server however our (net server) is only for
;; simple use and isn't meant to be scalable. So define set of
;; APIs so that it can be flexible for future.
;; Most of the APIs take MQTT context, type, flag length of payload
;; and input/output port. We expect the port is bidirectional however
;; try not to assume.
(library (net mq mqtt broker api)
    (export make-mqtt-broker-context
	    mqtt-broker-connect!)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (net mq mqtt packet)
	    (binary pack)
	    (srfi :19))

  (define-class <mqtt-broker-context> ()
    ;; holds client ID and session object
    ((sessions    :init-form (make-string-hashtable))
     (authentication-handler :init-keyword :authentication-handler
			     :init-value #f)
     (authorities :init-keyword :authorities :init-value '())))

  (define (make-mqtt-broker-context . opt)
    (apply make <mqtt-broker-context> opt))

  ;; TODO
  (define-class <mqtt-session> ()
    ((version :init-keyword :version)
     alive-until ;; time object
     ;; these are will topic thing
     (topic   :init-keyword :topic)
     (message :init-keyword :message)
     (retain? :init-keyword :retain?)))

  ;; This is the entry when server got a connection.
  (define-constant +name-level+
    '(("MQTT" . 4)
      ("MQIsdp" . 3)))

  ;; reading variable header and payload
  (define (read-utf8-string in)
    (let ((len (get-unpack in "!S"))) (utf8->string (get-bytevector-n in len))))
  (define (read-application-data in)
    (let ((len (get-unpack in "!S"))) (get-bytevector-n in len)))

  (define (mqtt-broker-connect! context in/out)
    (define clean-session-bit 1)
    (define will-flag-bit 2)
    (define will-qos-mask #x0C) ;; bit 3 and 4
    (define will-retain-bit 5)
    (define password-bit 6)
    (define username-bit 7)
    (define (parse-rest flag in)
      (let-values (((topic message) (if (bitwise-bit-set? flag will-flag-bit)
					(let ((t (read-utf8-string in)))
					  (values t (read-application-data in)))
					(values #f #f))))
	(let* ((username (and (bitwise-bit-set? flag username-bit)
			      (read-utf8-string in)))
	       (password (and (bitwise-bit-set? flag password-bit)
			      (read-application-data in))))
	  (values topic message username password))))
    (define (send-conack code session-present? r)
      (write-fixed-header in/out +connack+ 0 2)
      (put-u8 in/out (if session-present? 1 0))
      (put-u8 in/out code)
      r)
    (define (check-verion vh)
      (cond ((assoc (car vh) +name-level+) =>
	     (lambda (slot)
	       (or (= (cdr slot) (cadr vh))
		   (send-conack #x01 #f #f))))
	    (else (send-conack #x01 #f #f))))
    (define (authenticate user password)
      (if (~ context 'authentication-handler)
	  (guard (e (else (send-conack #x04 #f #f)))
	    ((~ context 'authentication-handler) context user password))
	  #t))
    (define (compute-period keep-alive)
      (add-duration! (current-time) (make-time time-duration 0 keep-alive)))
    (let-values (((type flags len) (read-fixed-header in/out)))
      (unless (= type +connect+)
	(error 'mqtt-broker-connect! "expected CONNECT packet" type))
      (let*-values (((vh payload) (read-variable-header&payload 
				   ;; name, level, flag, keep-alive
				   in/out len :utf8 :u8 :u8 :u16))
		    ;; parse all payload as a simple verification.
		    ((client-id) (read-utf8-string payload))
		    ((topic message user password)
		     (parse-rest (caddr vh) payload)))
	(define (make/retrieve-session client-id flag)
	  (cond ((and (not (bitwise-bit-set? flag clean-session-bit))
		      (hashtable-ref (~ context 'sessions) client-id #f))
		 => (lambda (s) (values #f s)))
		(else
		 (values #t 
			 (make <mqtt-session>
			   :topic topic :message message
			   :retain (bitwise-bit-set? flag will-retain-bit)
			   :qos (bitwise-arithmetic-shift-right
				 (bitwise-and flag will-qos-mask) 2))))))
	;; not check for client-id should we?
	(and-let* ((flag (caddr vh))
		   ( (check-verion vh) )
		   ( (authenticate user password) ))
	  (let-values (((created? session) 
			(make/retrieve-session client-id flag)))
	    (set! (~ session 'alive-until) (compute-period (cadddr vh)))
	    (hashtable-set! (~ context 'sessions) client-id session)
	    (send-conack 0 (not created?) session))))))
)
		
