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
	    mqtt-broker-connect!
	    mqtt-broker-disconnect!
	    mqtt-broker-publish
	    mqtt-broker-puback
	    mqtt-broker-pubrel
	    mqtt-broker-pubrec
	    mqtt-broker-pubcomp
	    mqtt-broker-subscribe
	    mqtt-broker-unsubscribe
	    mqtt-broker-pingreq

	    mqtt-broker-will-message

	    mqtt-session-alive?

	    <mqtt-broker-context>
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (net mq mqtt packet)
	    (net mq mqtt topic)
	    (binary pack)
	    (util hashtables)
	    (srfi :1)
	    (srfi :18)
	    (srfi :19)
	    (srfi :26))

  ;; TODO
  ;;  - handling authentication
  (define-class <mqtt-broker-context> ()
    ;; holds client ID and session object
    ((sessions    :init-form (make-string-hashtable))
     (topics      :init-form (make-string-hashtable))
     (cleaner-interval :init-keyword :cleaner-interval 
		       :init-value 10)
     cleaner-thread
     (authentication-handler :init-keyword :authentication-handler
			     :init-value #f)))

  (define-method initialize ((r <mqtt-broker-context>) initargs)
    (define (thunk)
      (let loop ()
	(thread-sleep! (~ r 'cleaner-interval))
	(dolist (session (hashtable-values-list (~ r 'sessions)))
	  (mutex-lock! (~ session 'lock))
	  (and-let* ((until (~ session 'alive-until))
		     ( (time? until) ))
	    (when (time<? until (current-time))
	      (set! (~ session 'alive-until) #f)
	      ;; let client know it's closed
	      (close-port (~ session 'port))))
	  (mutex-unlock! (~ session 'lock)))
	;; TODO maybe we want to check context state
	;; so that we can terminate this thread
	(loop)))
    ;; in case of GCed
    (set! (~ r 'cleaner-thread) (thread-start! (make-thread thunk)))
    (call-next-method))

  (define (make-mqtt-broker-context . opt)
    (apply make <mqtt-broker-context> opt))

  (define-class <mqtt-session> ()
    ((context :init-keyword :context)
     (client-id :init-keyword :client-id)
     (version :init-keyword :version)
     (port    :init-keyword :port)
     (packets :init-form (make-eqv-hashtable))
     (server-packets :init-form (make-eqv-hashtable))
     ;; set of subscriptions
     (subscriptions :init-value '())
     (keep-alive :init-keyword :keep-alive)
     (alive-until :init-value #t) ;; time object
     ;; these are will topic thing
     (topic   :init-keyword :topic)
     (message :init-keyword :message)
     (retain? :init-keyword :retain?)
     (qos     :init-keyword :qos)
     ;; lock
     (lock    :init-form (make-mutex))))

  (define-condition-type &session-expired &error 
    make-session-expired session-expired?)
  (define (session-expired session)
    ;; TODO handling will topic
    ;; when session is expired then 
    (session-invalidate! session)
    (raise (condition (make-session-expired)
		      (make-who-condition 'broker-session)
		      (make-message-condition "session expired"))))

  (define (update-alive-until! session)
    (define (compute-period)
      (let ((keep-alive (~ session 'keep-alive)))
	(if (zero? keep-alive)
	    #t ;; infinite
	    (add-duration! (current-time)
			   (make-time time-duration 0 keep-alive)))))
    (mutex-lock! (~ session 'lock))
    (unwind-protect
	(if (~ session 'alive-until)
	    (set! (~ session 'alive-until) (compute-period))
	    (session-expired session))
      (mutex-unlock! (~ session 'lock))))

  ;; we will delete them when QoS protocol is finished
  (define (allocate-packet-identifier session message)
    (define packets (~ session 'server-packets))
    (define pis (hashtable-keys-list packets))
    (let ((next (if (null? pis) 1 (+ (apply max pis) 1))))
      (hashtable-set! packets next message)
      next))
  
  (define (mqtt-session-alive? session) (~ session 'context))

  ;; This is the entry when server got a connection.
  (define-constant +name-level+
    '(("MQTT" . 4)
      ("MQIsdp" . 3)))

  ;; reading variable header and payload
  (define (read-utf8-string in)
    (if (eof-object? (lookahead-u8 in))
	(eof-object)
	(let ((len (get-unpack in "!S")))
	  (utf8->string (get-bytevector-n in len)))))
  ;; (define (read-packet-identifier in) (get-unpack in "!S"))
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
	(define (make/retrieve-session client-id flag vh)
	  (cond ((and (not (bitwise-bit-set? flag clean-session-bit))
		      (hashtable-ref (~ context 'sessions) client-id #f))
		 => (lambda (s) 
		      ;; restore session with given port
		      (set! (~ session 'port) in/out)
		      (values #f s)))
		(else
		 (values #t 
			 (make <mqtt-session>
			   :context context :client-id client-id
			   :port in/out
			   :keep-alive (cadddr vh)
			   :topic topic :message message
			   :retain? (bitwise-bit-set? flag will-retain-bit)
			   :qos (bitwise-arithmetic-shift-right
				 (bitwise-and flag will-qos-mask) 2))))))
	;; not check for client-id should we?
	(and-let* ((flag (caddr vh))
		   ( (check-verion vh) )
		   ( (authenticate user password) ))
	  (let-values (((created? session) 
			(make/retrieve-session client-id flag vh)))
	    (update-alive-until! session)
	    (hashtable-set! (~ context 'sessions) client-id session)
	    (send-conack 0 (not created?) session)
	    session)))))

  ;; disconnect
  (define (session-invalidate! session)
    (close-port (~ session 'port))
    (hashtable-clear! (~ session 'packets))
    (hashtable-clear! (~ session 'server-packets))
    (hashtable-delete! (~ session 'context 'sessions)
		       (~ session 'client-id))
    (set! (~ session 'context) #f))
  (define (mqtt-broker-disconnect! session type flags len in/out)
    ;; no response back
    ;; invalidate session
    (session-invalidate! session)
    )

  ;;; publish
  (define (create/retrieve-topic context topic)
    (cond ((hashtable-ref (~ context 'topics) topic #f))
	  (else (let ((t (make-mqtt-topic topic)))
		  (hashtable-set! (~ context 'topics) topic t)
		  t))))
  (define (mqtt-broker-will-message session)
    (and-let* ((topic (~ session 'topic))
	       (message (~ session 'message))
	       (retain? (~ session 'retain?))
	       (qos   (~ session 'qos))
	       (context (~ session 'context)))
      (let ((t (create/retrieve-topic context topic)))
	(when retain?
	  (mqtt-topic-retain-message-set! t message)
	  (mqtt-topic-retain-qos-set! t qos))
	(store/send-payload context topic qos retain?
			    (open-bytevector-input-port message)))))

  (define (mqtt-broker-publish session type flags len in/out)
    (define (parse-flags flag)
      (values (bitwise-arithmetic-shift-right flag 3)
	      (bitwise-and (bitwise-arithmetic-shift-right flag 1) #x03)
	      (bitwise-and flag #x01)))
    (define (send-resp type pi)
      (write-fixed-header in/out type 0 2)
      (write-packet-identifier in/out pi))
    (define (send-puback pi) (send-resp +puback+ pi))
    (define (send-pubrec pi) (send-resp +pubrec+ pi))
    (update-alive-until! session)
    (let*-values (((dup qos retain) (parse-flags flags))
		  ((vh payload) (apply read-variable-header&payload 
				       in/out len :utf8
				       (if (= qos +qos-at-most-once+)
					   '()
					   '(:pi)))))
      (unless (mqtt-valid-topic? (car vh))
	(error 'mqtt-broker-publish "Invalid topic" (car vh)))
      ;; store packat identifier (only one)
      (cond ((= qos +qos-at-most-once+)
	     (store/send-payload (~ session 'context)
				 (car vh) qos retain payload))
	    ((= qos +qos-at-least-once+)
	     (send-puback (cadr vh))
	     (store/send-payload (~ session 'context)
				 (car vh) qos retain payload))
	    (else
	     (let ((pi (cadr vh)))
	       (send-pubrec pi)
	       (hashtable-set! (~ session 'packets) pi 
			       (vector (car vh) qos retain payload)))))))
  
  (define (mqtt-broker-puback session type flags len in/out)
    (define packets (~ session 'server-packets))
    (update-alive-until! session)
    (let-values (((vh payload) (read-variable-header&payload in/out len :pi)))
      ;; delete message accociated with given packet identifier
      ;; if not, we don't do anything
      (cond ((hashtable-ref packets (car vh))
	     (hashtable-delete! packets (car vh))))))

  (define (mqtt-broker-pubrel session type flags len in/out)
    (define packets (~ session 'packets))
    (define (send-pubcomp pi)
      (write-fixed-header in/out +pubcomp+ 0 2)
      (write-packet-identifier in/out pi))
    (update-alive-until! session)
    (let-values (((vh payload) (read-variable-header&payload in/out len :pi)))
      ;; delete message accociated with given packet identifier
      ;; if not, we don't do anything
      (cond ((hashtable-ref packets (car vh)) =>
	     (lambda (appdata)
	       (hashtable-delete! packets (car vh))
	       (send-pubcomp (car vh))
	       (store/send-payload (~ session 'context)
				   (vector-ref appdata 0)
				   (vector-ref appdata 1)
				   (vector-ref appdata 2)
				   (vector-ref appdata 3)))))))

  (define (mqtt-broker-pubrec session type flags len in/out)
    (define packets (~ session 'server-packets))
    (define (send-pubrel pi)
      (write-fixed-header in/out +pubrel+ 0 2)
      (write-packet-identifier in/out pi))
    (update-alive-until! session)
    (let-values (((vh payload) (read-variable-header&payload in/out len :pi)))
      ;; delete message accociated with given packet identifier
      ;; if not, we don't do anything
      (cond ((hashtable-ref packets (car vh))
	     (send-pubrel (car vh))
	     (hashtable-set! packets (car vh) #t)))))

  (define (mqtt-broker-pubcomp session type flags len in/out)
    (define packets (~ session 'server-packets))
    (update-alive-until! session)
    (let-values (((vh payload) (read-variable-header&payload in/out len :pi)))
      ;; delete message accociated with given packet identifier
      ;; if not, we don't do anything
      (cond ((hashtable-ref packets (car vh))
	     (hashtable-delete! packets (car vh))))))

  ;; this is not an API but helper (will be used puback and pubrel)
  (define (store/send-payload context topic qos retain payload)
    (define (get-subscribers topic sessions)
      (filter-map (lambda (session)
		    (and (member topic (~ session 'subscriptions)
				 (lambda (topic subscription)
				   (mqtt-topic-match? subscription topic)))
			 session))
		  (hashtable-values-list sessions)))
    (let ((t (create/retrieve-topic context topic))
	  (m (get-bytevector-all payload)))
      (let ((subscribers (get-subscribers topic (~ context 'sessions))))
	(if (null? subscribers)
	    (mqtt-topic-enqueue! t qos m) ;; for what?
	    ;; send it without storing
	    (for-each (cut send-publish <> topic qos m) subscribers)))
      (unless (zero? retain) (mqtt-topic-retain-message-set! t m))))

  ;; subscribe
  ;; TODO should we create topic if doesn't exist?
  (define (mqtt-broker-subscribe session type flags len in/out)
    (define context (~ session 'context))
    (define topics (~ context 'topics))
    (define (send-retain qos&filter)
      (define (check-type type expected)
	(or (= type expected)
	    (error 'mqtt-broker-subscribe
		   "Client respond invalid packet type for retain message"
		   type)))
      (hashtable-for-each 
       (lambda (name topic)
	 (and-let* (( (not (= (car qos&filter) #x80)) )
		    (retain (mqtt-topic-retain-message topic))
		    (qos    (min (car qos&filter)
				 (mqtt-topic-retain-qos topic)))
		    ( (mqtt-topic-match? (cdr qos&filter) name) ))
	   (send-publish session (mqtt-topic-name topic) qos retain)
	   (cond ((= qos +qos-at-least-once+)
		  (let-values (((type flags len)
				(read-fixed-header in/out)))
		    (check-type type +puback+)
		    (mqtt-broker-puback session type flags len in/out)))
		 ((= qos +qos-exactly-once+)
		  (let-values (((type flags len)
				(read-fixed-header in/out)))
		    (check-type type +pubrec+)
		    (mqtt-broker-pubrec session type flags len in/out))
		  (let-values (((type flags len)
				(read-fixed-header in/out)))
		    (check-type type +pubcomp+)
		    (mqtt-broker-pubcomp session type flags len in/out))))))
       topics))
    (define (send-suback pi codes)
      (define (update-subscription filters)
	(let ((subscriptions (~ session 'subscriptions)))
	  ;; TODO merge topic?
	  ;; I'm not sure if we actually need to merge topics
	  ;; say client subscribes '#' first then 'topics/#'.
	  ;; it may have different handler for message. but
	  ;; how about vice versa? should we send to more specific
	  ;; topic or last in? something like this?
	  ;; assume mqtt-topic< compares topics which is more
	  ;; specific.
	  ;; (list-sort mqtt-topic< subscriptions)
	  (set! (~ session 'subscriptions) `(,@filters ,@subscriptions))
	  ))
      (update-subscription (map cdr codes))
      (let ((len (length codes)))
	(write-fixed-header in/out +suback+ 0 (+ 2 len))
	(write-packet-identifier in/out pi)
	(for-each (cut put-u8 in/out <>) (map car codes)))
      ;; now we need to send publish if subscribed topics contains
      ;; retain messages
      (for-each send-retain codes))
    (define (read-topic&qos in)
      (if (eof-object? (lookahead-u8 in))
	  (values #f #f)
	  (let ((name (read-utf8-string in)))
	    (values name (get-u8 in)))))
    (update-alive-until! session)
    (let-values (((vh payload) (read-variable-header&payload in/out len :pi)))
      (let loop ((res '()))
	(let-values (((topic qos) (read-topic&qos payload)))
	  (if topic
	      (cond ((mqtt-valid-topic? topic)
		     (loop (acons qos topic res)))
		    (else
		     (loop (acons #x80 topic res))))
	      (send-suback (car vh) (reverse! res)))))))
  
  ;; FIXME almost the same as client so refactor it
  (define (send-publish session topic qos message)
    (define in/out (~ session 'port))
    (define packet-prefix (if (= qos +qos-at-most-once+) 2 4))
    (define pi (allocate-packet-identifier session message))
    (guard (e ((i/o-error? e) #f) ;; must likely socket erorr so ignore.
	      (else (raise e)))
      (let* ((u8-topic (string->utf8 topic))
	     (len (+ packet-prefix (bytevector-length u8-topic))))
	(write-fixed-header in/out +publish+ (bitwise-arithmetic-shift qos 1)
			    (+ len (bytevector-length message)))
	(write-utf8 in/out u8-topic)
	(unless (= qos +qos-at-most-once+)
	  (write-packet-identifier in/out pi))
	(put-bytevector in/out message))))

  ;; unsubscribe
  (define (mqtt-broker-unsubscribe session type flag len in/out)
    ;; MQTT-3.10.4-1, comparing character-by-character
    ;; easy huh?
    (define (unsubscribe topics)
      (let ((subscriptions (~ session 'subscriptions)))
	(set! (~ session 'subscriptions)
	      (lset-difference string=? subscriptions topics))))
    (update-alive-until! session)
    (let-values (((vh payload) (read-variable-header&payload in/out len :pi)))
      (let loop ((r '()))
	(let ((topic (read-utf8-string payload)))
	  (if (eof-object? topic)
	      (unsubscribe r)
	      (loop (cons topic r)))))))

  (define (mqtt-broker-pingreq session type flag len in/out)
    (update-alive-until! session)
    (write-fixed-header in/out +pingresp+ 0 0))

)
		
