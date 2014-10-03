;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/mqtt/packet.scm - MQTT v3.1.1 packet
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

;; This library only provides reading and parsing packets.
;; MQTT is one simple protocol so we don't make much complexity
;; on this layer. (no need)
(library (net mq mqtt packet)
    (export read-fixed-header
	    read-variable-header&payload
	    ;; write
	    write-fixed-header
	    write-utf8
	    write-packet-identifier

	    ;; packet types
	    +connect+
	    +connack+
	    +publish+
	    +puback+
	    +pubrec+
	    +pubrel+
	    +pubcomp+
	    +subscribe+
	    +suback+
	    +unsubscribe+
	    +unsuback+
	    +pingreq+
	    +pingresp+
	    +disconnect+
	    ;; CONNACK code
	    +connack-accepted+
	    +connack-unacceptable-protocol+
	    +connack-identifier-rejected+
	    +connack-server-unavailable+
	    +connack-bad-username/password+
	    +connack-authentication-error+
	    ;; QoS
	    +qos-at-most-once+
	    +qos-at-least-once+
	    +qos-exactly-once+
	    ;; suback
	    +suback-failure+
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (binary pack)
	    (binary io))

  (define-constant +connect+   	 1)
  (define-constant +connack+   	 2)
  (define-constant +publish+   	 3)
  (define-constant +puback+    	 4)
  (define-constant +pubrec+    	 5)
  (define-constant +pubrel+    	 6)
  (define-constant +pubcomp+   	 7)
  (define-constant +subscribe+ 	 8)
  (define-constant +suback+    	 9)
  (define-constant +unsubscribe+ 10)
  (define-constant +unsuback+    11)
  (define-constant +pingreq+     12)
  (define-constant +pingresp+    13)
  (define-constant +disconnect+  14)

  ;; connack code
  (define-constant +connack-accepted+              0)
  (define-constant +connack-unacceptable-protocol+ 1)
  (define-constant +connack-identifier-rejected+   2)
  (define-constant +connack-server-unavailable+    3)
  (define-constant +connack-bad-username/password+ 4)
  (define-constant +connack-authentication-error+  5)

  ;; QoS
  (define-constant +qos-at-most-once+  0)
  (define-constant +qos-at-least-once+ 1)
  (define-constant +qos-exactly-once+  2)

  ;; subscription failure
  (define-constant +suback-failure+    #x80)

  (define (read-fixed-header in)
    (define (read-length in)
      (define (check-error multiplier)
	(when (> multiplier (expt #x80 3))
	  (error 'read-fixed-header "Malformed remaining length")))
      (do ((encoded-byte (get-u8 in) (get-u8 in))
	   (v 0 (+ v (* (bitwise-and encoded-byte #x7F) multiplier)))
	   (multiplier 1 (* multiplier #x80)))
	  ((zero? (bitwise-and encoded-byte #x80))
	   (check-error multiplier)
	   (+ v (* encoded-byte multiplier)))
	(check-error multiplier)))
    (let ((b1 (get-u8 in)))
      (if (eof-object? b1)
	  (values #f #f #f)
	  (let ((len (read-length in)))
	    (values (bitwise-arithmetic-shift-right b1 4)
		    (bitwise-and b1 #x0F)
		    len)))))

  (define (write-fixed-header out type flag remaining-length)
    (define (encode-length out len)
      (let loop ((encoded-byte (mod len #x80)) (X (div len #x80)))
	(if (positive? X)
	    (put-u8 out (bitwise-ior encoded-byte #x80))
	    (put-u8 out encoded-byte))
	(unless (zero? X) (loop (mod X #x80) (div X #x80)))))
    (put-u8 out (bitwise-ior (bitwise-arithmetic-shift type 4) flag))
    (encode-length out remaining-length))
  (define (write-utf8 out bv)
    (put-u16 out (bytevector-length bv) (endianness big))
    (put-bytevector out bv))
  (define (write-packet-identifier out pi) (put-u16 out pi (endianness big)))

  ;; reading variable header and payload
  (define (read-utf8-string in)
    (let ((len (get-unpack in "!S")))
      (values (+ len 2) (utf8->string (get-bytevector-n in len)))))
  (define (read-application-data in)
    (let ((len (get-unpack in "!S")))
      (values (+ len 2) (get-bytevector-n in len))))
  ;; hmmmm, binary?
  (define (read-packet-identifier in) (values 2 (get-bytevector-n in 2)))

  ;; rules are how to read variable header
  ;; followings are the rules
  ;; :u8   - byte
  ;; :u16  - 16 bit unsigned integer
  ;; :utf8 - utf8 string
  ;; :pi   - packet identifier
  ;; As far as I know, there is no other types on MQTT spec
  ;; remaining length is required in case of more than 1 packets
  ;; are sent in one go.
  (define (read-variable-header&payload in len
					:key (chunk-size 4096)
					:allow-other-keys rules)

    (define (read-variable-header in rules)
      (let loop ((rules rules) (size 0) (r '()))
	(if (null? rules)
	    (values size (reverse! r))
	    (case (car rules)
	      ((:u8) (loop (cdr rules) (+ size 1) (cons (get-u8 in) r)))
	      ((:u16 :pi)
	       (loop (cdr rules) (+ size 2) (cons (get-unpack in "!S") r)))
	      ((:utf8)
	       (let-values (((len str) (read-utf8-string in)))
		 (loop (cdr rules) (+ size len) (cons str r))))
	      (else =>
	       (lambda (t) (error 'read-variable-header "unknown rule" t)))))))
    ;; first read variables
    (let-values (((vlen vals) (read-variable-header in rules)))
      (let ((len (- len vlen)))
	(when (negative? len)
	  (error 'read-variable-header&payload "corrupted data stream"))
	(values vals
		(if (> len chunk-size)
		    (input-port->chunked-binary-input-port 
		     in :chunk-size chunk-size :threshold len)
		  (open-bytevector-input-port (get-bytevector-n in len)))))))

)
