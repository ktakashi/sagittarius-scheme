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
	    ;; for testing
	    ;; we might want to move this to (binary io)
	    ;; if it's useful enough
	    make-chunked-binary-input-port

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
	    ;; QoS
	    +qos-at-most-once+
	    +qos-at-least-once+
	    +qos-exactly-once+
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

  ;; QoS
  (define-constant +qos-at-most-once+  0)
  (define-constant +qos-at-least-once+ 1)
  (define-constant +qos-exactly-once+  2)

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
		    (make-chunked-binary-input-port in :chunk-size chunk-size
						    :threshold len)
		  (open-bytevector-input-port (get-bytevector-n in len)))))))
  
  ;; payload will be returned as a binary input port. the builtin one
  ;; may consume maximum number of memory allocation and it wouldn't be
  ;; handle (well techinically it's only 255MB so possible but not
  ;; efficient, i guess). the idea for this port is that without allocating
  ;; max number byte but doing the same as binary input port.
  (define-class <chunked-input-port> ()
    ((position :init-value 0) ;; index of vector
     (offset   :init-value 0) ;; offset of bytevector
     (chunks   :init-keyword :chunks)))

  ;; buffer size default 4096
  ;; threshold is how much bytes this needs to read from source port
  ;; default #f (infinite).
  (define (make-chunked-binary-input-port in :key (chunk-size 4096)
					   (threshold #f))
    (define (read-as-chunk in)
      (define (next-chunk-size read-size)
	(if threshold (min (- threshold read-size) chunk-size) chunk-size))

      (let loop ((chunk-size (next-chunk-size 0))
		 (read-size   0)
		 (r '()))
	(let ((buf (get-bytevector-n in chunk-size)))
	  (if (eof-object? buf) (list->vector (reverse! r))
	      (let ((len (bytevector-length buf))) 
		(if (or (< len chunk-size)
			(and threshold (>= (+ read-size len) threshold)))
		    (list->vector (reverse! (cons buf r)))
		    (let ((read-size (+ read-size len)))
		      (loop (next-chunk-size read-size)
			    read-size
			    (cons buf r)))))))))
    (let ((chunked-port (make <chunked-input-port>
			   :chunks (read-as-chunk in))))
      (define (read! bv start count)
	(define (last-chunk? chunks position)
	  (= (vector-length chunks) (+ position 1)))
	(let loop ((start start) (copied 0) (count count))
	  (let ((offset   (~ chunked-port 'offset))
		(position (~ chunked-port 'position))
		(chunks  (~ chunked-port 'chunks)))
	    (if (= (vector-length chunks) position)
		0
		(let* ((current-chunk (vector-ref chunks position))
		       (chunk-size (bytevector-length current-chunk))
		       (diff (- chunk-size offset)))
		  (cond ((>= diff count)
			 (bytevector-copy! current-chunk offset bv start count)
			 (cond ((and (not (last-chunk? chunks position))
				     (= (+ offset count) chunk-size))
				(set! (~ chunked-port 'offset) 0)
				(set! (~ chunked-port 'position) 
				      (+ position 1)))
			       (else
				(set! (~ chunked-port 'offset) 
				      (+ offset count))))
			 (+ count copied))
			((not (last-chunk? chunks position))
			 (bytevector-copy! current-chunk offset bv start diff)
			 (set! (~ chunked-port 'offset) 0)
			 (set! (~ chunked-port 'position) (+ position 1))
			 (loop (+ start diff) (+ diff copied) (- count diff)))
			(else
			 ;; last chunk and not enough
			 (bytevector-copy! current-chunk offset bv start diff)
			 (set! (~ chunked-port 'offset) (+ offset count))
			 (set! (~ chunked-port 'position) (+ position 1))
			 (+ diff copied))))))))
      (define (get-position)
	(let ((offset (~ chunked-port 'offset))
	      (position (~ chunked-port 'position)))
	  (+ offset (* position chunk-size))))
      (define (set-position! pos)
	(let ((index (div pos chunk-size))
	      (offset (mod pos chunk-size))
	      (chunks (~ chunked-port 'chunks)))
	  (cond ((>= index (vector-length chunks))
		 (let ((last (- (vector-length chunks) 1)))
		   (set! (~ chunked-port 'position) last)
		   (set! (~ chunked-port 'offset) 
			 (bytevector-length (vector-ref chunks last)))))
		(else
		 (set! (~ chunked-port 'position) index)
		 (set! (~ chunked-port 'offset) offset)))))
      (define (close) (set! (~ chunked-port 'chunks) #f))
      (make-custom-binary-input-port "chunked-binary-input-port" 
				     read! get-position set-position! close)))

)
