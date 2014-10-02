;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/mqtt/client.scm - MQTT v3.1.1 client library
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

(library (net mq mqtt client)
    (export mqtt-connection?
	    open-mqtt-connection
	    port->mqtt-connection
	    close-mqtt-connection!
	    
	    mqtt-subscribe
	    mqtt-receive-message

	    ;; publish
	    mqtt-publish

	    ;; unsubscribe
	    mqtt-unsubscribe

	    mqtt-ping

	    +mqtt-3.1+
	    +mqtt-3.1.1+
	    +qos-at-most-once+
	    +qos-at-least-once+
	    +qos-exactly-once+
	    )
    (import (rnrs)
	    (clos user)
	    ;; (rfc uuid)
	    (sagittarius)
	    (sagittarius socket)
	    (sagittarius object)
	    (net mq mqtt packet)
	    (net mq mqtt topic)
	    (binary io)
	    (srfi :1)
	    (srfi :13)
	    (srfi :19)
	    (srfi :26))

  (define-class <mqtt-connection> ()
    ((port  :init-keyword :port)
     (packet-identifier :init-value 1)
     (callbacks :init-value '())))
     
  (define (open-mqtt-connection host port . opt)
    (let ((socket (make-client-socket host port)))
      (apply port->mqtt-connection (socket-port socket) opt)))

  ;; version number, protocol name and connection variable header length
  (define-constant +mqtt-3.1+   '(3 "MQIsdp" 12))
  (define-constant +mqtt-3.1.1+ '(4 "MQTT"   10))

  (define (generate-client-id)
    (string-append "Sagittarius-MQTT" 
		   (number->string (time-second (current-time)) 32)))
  (define (port->mqtt-connection in/out 
				 :key (client-id #f)
				      (username #f)
				      (password #f)
				      (topic #f)
				      (message #f)
				      (retain? #f)
				      (keep-alive 10) ;; 10 sec?
				      (version +mqtt-3.1.1+)
				      ;; TODO
				      (qos +qos-at-most-once+))
    (define (construct-flag)
      ;; we don't set clean session flag.
      (bitwise-ior (if username #x80 #x00)
		   (if password #x40 #x00)
		   (if (and topic message retain?) #x20 #x00)
		   (bitwise-arithmetic-shift qos 3)
		   (if (and topic message) #x04 #x00)
		   (if (and client-id (not (string-null? client-id)))
		       #x00 #x02)))
    (define (compute-length)
      ;; make this a bit flexible
      (define (->utf8 v) 
	(and v 
	     (or (and (string? v) (string->utf8 v))
		 (and (bytevector? v) v))))
      (define (length v) (if v (+ (bytevector-length v) 2) 0))
      (let ((u8-id (string->utf8 (or client-id (generate-client-id))))
	    (u8-topic (->utf8 topic))
	    (u8-message message) ;; message must be an bytevector
	    (u8-user (->utf8 username))
	    (u8-pass (->utf8 password)))
	(values (+ (length u8-id) (length u8-topic)
		   (length u8-message) (length u8-user)
		   (length u8-pass))
		(filter-map values (list u8-id
					 u8-topic
					 u8-message
					 u8-user
					 u8-pass)))))
    ;; check consistency
    (unless (or (and topic message)
		(and (not topic) (not message)))
      (error 'port->mqtt-connection 
	     "topic and message must be either both #f or both passed"
	     topic message))
    ;; now we need to calculate total length of this packet
    (let-values (((payload-length payloads) (compute-length)))
      ;; we know variable header is 10 bytes
      ;; TODO should we?
      (write-fixed-header in/out +connect+ 0 (+ payload-length (caddr version)))
      ;; FIXED value
      (write-utf8 in/out (string->utf8 (cadr version)))
      (put-u8 in/out (car version))
      ;; (format #t "~b~%" (construct-flag))
      (put-u8 in/out (construct-flag))
      (put-u16 in/out keep-alive (endianness big))
      (for-each (cut write-utf8 in/out <>) payloads)
      ;; now receive
      (let-values (((type flag remaining-length) (read-fixed-header in/out)))
	(unless (= type +connack+)
	  (error 'port->mqtt-connection 
		 "Server respond non CONACK message" type))
	(let-values (((vheader payload) (read-variable-header&payload 
					 in/out remaining-length
					 :u8 :u8)))
	  (unless (zero? (cadr vheader))
	    (error 'port->mqtt-connection
		   (case (cadr vheader)
		     ((1) "Connection Refused, unacceptable protocol version")
		     ((2) "Connection Refused, identifier rejected")
		     ((3) "Connection Refused, Server unavailable")
		     ((4) "Connection Refused, bad user name or password")
		     ((5) "Connection Refused, not authorized")
		     (else "Unknown error"))))
	  ;; todo maybe restore session?
	  (make <mqtt-connection> :port in/out)))))

  (define (close-mqtt-connection! conn)
    (write-fixed-header (~ conn 'port) +disconnect+ 0 0)
    (close-port (~ conn 'port)))

  ;; FIXME this overflows. check max value
  (define (allocate-packet-identifier conn)
    (let ((pi (~ conn 'packet-identifier)))
      (set! (~ conn 'packet-identifier) (+ pi 1))
      pi))

  ;;; subscribe
  ;; we don't support multiple subscribe
  (define (mqtt-subscribe conn topic qos callback)
    (unless (mqtt-valid-topic? topic)
      (error 'mqtt-subscribe "not a valid topic" topic))
    (let ((pi (allocate-packet-identifier conn))
	  (in/out (~ conn 'port))
	  (u8-topic (string->utf8 topic)))
      ;; MQTT 3.1.1 spec says flag is 2 but reserved???
      (write-fixed-header in/out +subscribe+ 2
			  ;; 5 = pi + utf8-prefix + qos
			  (+ 5 (bytevector-length u8-topic)))
      (write-packet-identifier in/out pi)
      (write-utf8 in/out u8-topic)
      (put-u8 in/out qos)
      ;; receive suback
      (let-values (((type flag len) (read-fixed-header in/out)))
	(unless (= type +suback+)
	  (error 'mqtt-subscribe "Server respond non SUBACK packet" type))
	(let-values (((vh payload) (read-variable-header&payload in/out len
								 :pi)))
	  (unless (= (car vh) pi)
	    (error 'mqtt-subscribe "Server respond invalid packet identifier"
		   (car vh)))
	  (let ((rc (get-u8 payload)))
	    (when (= rc +suback-failure+)
	      (error 'mqtt-subscribe "Failed to subscribe" topic))
	    (set! (~ conn 'callbacks) 
		  (list-sort (lambda (a b)
			       (mqtt-topic>? (car a) (car b)))
			     (acons topic callback (~ conn 'callbacks))))
	    rc)))))
  
  (define (mqtt-puback conn pi)
    (let ((in/out (~ conn 'port)))
      (write-fixed-header in/out +puback+ 0 2)
      (write-packet-identifier in/out pi)))

  (define (mqtt-pubrec conn pi)
    (define in/out (~ conn 'port))
    ;; send pubrec
    (write-fixed-header in/out +pubrec+ 0 2)
    (write-packet-identifier in/out pi)
    ;; receive pubrel
    (let-values (((type flag len) (read-fixed-header in/out)))
      (unless (= type +pubrel+)
	(error 'mqtt-pubrec "Sender sent invalid packet" type))
      (let-values (((vh payload) (read-variable-header&payload in/out len :pi)))
	(unless (= (car vh) pi)
	  (error 'mqtt-pubrec "Sender sent invalid packet identifier" 
		 (car vh)))))
    ;; send pubcomp
    (write-fixed-header in/out +pubcomp+ 0 2)
    (write-packet-identifier in/out pi))

  (define (mqtt-receive-message conn)
    (define (parse-flags flag)
      (values (bitwise-arithmetic-shift-right flag 3)
	      (bitwise-and (bitwise-arithmetic-shift-right flag 1) #x03)
	      (bitwise-and flag #x01)))
    (when (null? (~ conn 'callbacks))
      (error 'mqtt-receive-message "No subscription"))
    (let*-values (((type flag len) (read-fixed-header (~ conn 'port)))
		  ((dup qos retain) (parse-flags flag))
		  ((vh payload) (apply read-variable-header&payload 
				       (~ conn 'port) len
				       :utf8 
				       (if (= qos +qos-at-most-once+)
					   '()
					   '(:pi)))))
      ;; handle response
      (case qos
	((0))
	((1) (mqtt-puback conn (cadr vh)))
	((2) (mqtt-pubrec conn (cadr vh))))
      ;; find callback
      (let ((callback (assoc (car vh) (~ conn 'callbacks)
			     (lambda (topic filter)
			       (mqtt-topic-match? filter topic)))))
	(unless callback
	  (error 'mqtt-receive-message
		 "No subscription but got message from server"
		 (car vh)))
	((cdr callback) (car vh) payload))))

  ;;; publish
  ;; message must be a bytevector
  ;; TODO DUP
  (define (mqtt-publish conn topic message
			:key (qos +qos-at-most-once+) (retain? #f))
    (define (construct-flag)
      (bitwise-ior (bitwise-arithmetic-shift qos 1)
		   (if retain? #x01 #x00)))
    (define in/out (~ conn 'port))
    (define (handle-puback pi len)
      (let-values (((vh payload) (read-variable-header&payload in/out len :pi)))
	(unless (= pi (car vh))
	  (error 'mqtt-publish "Server respond invalid packet identifier" pi))
	pi))
    (define (handle-pubrec pi len)
      ;; TODO do more properly
      (let-values (((vh payload) (read-variable-header&payload in/out len :pi)))
	(unless (= pi (car vh))
	  (error 'mqtt-publish "Server respond invalid packet identifier" pi))
	(write-fixed-header in/out +pubrel+ 2 2)
	(write-packet-identifier in/out (car vh))
	(let-values (((type flag len) (read-fixed-header in/out)))
	  (unless (= type +pubcomp+)
	    (error 'mqtt-publish "Server respond invalid packet" type))
	  (let-values (((vh payload) 
			(read-variable-header&payload in/out len :pi)))
	    (unless (= pi (car vh))
	      (error 'mqtt-publish 
		     "Server respond invalid packet identifier" pi))
	    pi))))
    (define packet-prefix
      (if (= qos +qos-at-most-once+) 2 4))
    (unless (mqtt-valid-topic? topic)
      (error 'mqtt-publish "not a valid topic" topic))
    (let* ((u8-topic (string->utf8 topic))
	   (len (+ packet-prefix (bytevector-length u8-topic)))
	   (pi (allocate-packet-identifier conn)))
      (write-fixed-header in/out +publish+ (construct-flag)
			  (+ len (bytevector-length message)))
      (write-utf8 in/out u8-topic)
      (unless (= qos +qos-at-most-once+)
	(write-packet-identifier in/out pi))
      (put-bytevector in/out message)
      ;; now handle response
      (if (= qos +qos-at-most-once+)
	  pi
	  (let-values (((type flag len) (read-fixed-header in/out)))
	    (unless (or (and (= qos +qos-at-least-once+) 
			     (= type +puback+))
			(and (= qos +qos-exactly-once+) 
			     (= type +pubrec+)))
	      (error 'mqtt-publish "Server respond invalid packet" type))
	    (case qos
	      ((1) (handle-puback pi len))
	      ((2) (handle-pubrec pi len)))))))

  ;; unsubscribe
  ;; TODO wild card
  (define (mqtt-unsubscribe conn topic)
    (define in/out (~ conn 'port))
    (unless (mqtt-valid-topic? topic)
      (error 'mqtt-publish "not a valid topic" topic))
    (let ((u8-topic (string->utf8 topic))
	  (pi (allocate-packet-identifier conn)))
      (write-fixed-header in/out +unsubscribe+ 2 
			  (+ 4 (bytevector-length u8-topic)))
      (write-packet-identifier in/out pi)
      (write-utf8 in/out u8-topic)
      ;; unsuback
      (let-values (((type flag len) (read-fixed-header in/out)))
	(unless (= type +unsuback+)
	  (error 'mqtt-unsubscribe "Server respond invalid packet" type))
	(let-values (((vh pay) (read-variable-header&payload in/out len :pi)))
	  (unless (= pi (car vh))
	    (error 'mqtt-unsubscribe 
		   "Server respond invalid packet identifier" (car vh)))))
      ;; now remove callback
      (set! (~ conn 'callbacks) (alist-delete! topic (~ conn 'callbacks)))
      #t))
    
  ;;; pingreq
  (define (mqtt-ping conn)
    (define in/out (~ conn 'port))
    (write-fixed-header in/out +pingreq+ 0 0)
    (let-values (((type flag len) (read-fixed-header in/out)))
      (unless (= type +pingresp+)
	(error 'mqtt-ping
	       "Server respond back invalid packet" type))
      #t))
)
