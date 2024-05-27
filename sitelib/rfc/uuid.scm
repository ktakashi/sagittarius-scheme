;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; uuid.scm - UUID generator library.
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

;; ref:
;; - https://www.rfc-editor.org/rfc/rfc4122.html (Obsoleted)
;; - https://www.rfc-editor.org/rfc/rfc9562.html
#!nounbound
(library (rfc uuid)
    (export <uuid>
	    make-null-uuid
	    make-v1-uuid
	    make-v3-uuid
	    make-v4-uuid
	    make-v5-uuid
	    uuid?
	    null-uuid?
	    v1-uuid?
	    v3-uuid?
	    v4-uuid?
	    v5-uuid?
	    uuid=?
	    uuid-time-low
	    uuid-time-mid
	    uuid-time-high
	    uuid-clock-seq-var
	    uuid-clock-seq-low
	    uuid-node
	    ;; namespace
	    +namespace-dns+
	    +namespace-url+
	    +namespace-oid+
	    +namespace-x500+
	    ;; converter
	    uuid->bytevector
	    bytevector->uuid
	    uuid->string
	    string->uuid
	    uuid->urn-format
	    ;; parameters
	    *uuid-random-state*
	    *uuids-per-tick*
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius crypto digests)
	    (sagittarius crypto random)
	    (clos user)
	    (srfi :39 parameters))

;; Holds the amount of ticks per count. The ticks per count determine
;; the number of possible version 1 uuids created for one time interval.
(define *uuids-per-tick* (make-parameter 1024))
(define *uuid-random-state* 
  (make-parameter (secure-random-generator *prng:chacha20*)))

;; Represents an uuid
#| The field names are kept the same as before for backward compatibility
   So, using v1 names
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                           time_low                            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|           time_mid            |  ver  |       time_high       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|var|         clock_seq         |             node              |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                              node                             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|#
(define-class <uuid> ()
  (;; unsigned 32 bit integer
   ;; The low field of the timestamp
   (time-low :init-keyword :time-low :init-value 0 :reader uuid-time-low)
   ;; unsigned 16 bit integer
   ;; The middle field of the timestamp
   (time-mid :init-keyword :time-mid :init-value 0 :reader uuid-time-mid)
   ;; unsigned 4 bit integer
   (ver      :init-keyword :ver      :init-value 0 :reader uuid-ver)
   ;; unsigned 12 bit integer
   ;; The high field of the timestamp multiplexed with the version number
   (time-high :init-keyword :time-high :init-value 0 :reader uuid-time-high)
   ;; unsigned 2 bit integer
   (var       :init-keyword :var :init-value 0 :reader uuid-var)
   ;; unsigned 14 bit integer
   ;; The high field of the clock sequence multiplexed with the variant
   (clock-seq :init-keyword :clock-seq :init-value 0 :reader uuid-clock-seq)
   ;; unsigned 48 bit integer
   ;; The spatially unique node identifier
   (node :init-keyword :node :init-value 0 :reader uuid-node)))

(define (uuid? obj) (is-a? obj <uuid>))
;; for backward compatibilities
(define (uuid-clock-seq-var (uuid uuid?))
  (bitwise-ior (bitwise-arithmetic-shift-left (uuid-var uuid) 6)
   (bitwise-and (bitwise-arithmetic-shift-right (uuid-clock-seq uuid) 8)
		#x3F)))
(define (uuid-clock-seq-low (uuid uuid?))
  (bitwise-and (uuid-clock-seq uuid) #x00FF))

(define (null-uuid? obj)
  (and (uuid? obj)
       (zero? (uuid-time-low obj))
       (zero? (uuid-time-mid obj))
       (zero? (uuid-ver obj))
       (zero? (uuid-time-high obj))
       (zero? (uuid-var obj))
       (zero? (uuid-clock-seq obj))
       (zero? (uuid-node obj))))
(define (check-version obj ver)
  (and (uuid? obj)
       (= ver (uuid-ver obj))))
(define (v1-uuid? obj) (check-version obj 1))
(define (v3-uuid? obj) (check-version obj 3))
(define (v4-uuid? obj) (check-version obj 4))
(define (v5-uuid? obj) (check-version obj 5))

(define-method write-object ((uuid <uuid>) out)
  (format out "#<uuid ~a>" (uuid->string uuid)))
(define-method object-equal? ((uuid1 <uuid>) (uuid2 <uuid>))
  (uuid=? uuid1 uuid2))

(define-method ->string ((uuid <uuid>)) (uuid->string uuid))

(define (uuid->string uuid)
  (format "~8,'0X-~4,'0X-~1,'0X~3,'0X-~4,'0X-~12,'0X"
	  (uuid-time-low uuid)
	  (uuid-time-mid uuid)
	  (uuid-ver uuid)
	  (uuid-time-high uuid)
	  (bitwise-and 
	   (bitwise-ior (bitwise-arithmetic-shift-left (uuid-var uuid) 14)
			(uuid-clock-seq uuid))
	   #xFFFF)
	  (uuid-node uuid)))
;; creates an uuid from the string representation of an uuid.
;; (example input string 6ba7b810-9dad-11d1-80b4-00c04fd430c8)
(define-constant +uuid-string-length+ 36)
(define (string->uuid s)
  (define (parse-block b start end) 
    (cond ((string->number (substring b start end) 16))
	  (else (assertion-violation 'string->uuid
				     "Invalid UUID block" b s))))
  (unless (= (string-length s) +uuid-string-length+)
    (assertion-violation 
     'string->uuid (format "Could not parse ~s as UUID: invalid length" s) s))
  (unless (and (char=? (ref s  8) #\-)
	       (char=? (ref s 13) #\-)
	       (char=? (ref s 18) #\-)
	       (char=? (ref s 23) #\-))
    (assertion-violation
     'string->uuid (format "Could not parse ~s as UUID: invalid format" s) s))
  (make <uuid>
    :time-low      (parse-block s  0  8)
    :time-mid      (parse-block s  9 13)
    :ver           (parse-block s 14 15)
    :time-high     (parse-block s 15 18)
    :var           (bitwise-arithmetic-shift-right (parse-block s 19 20) 2)
    :clock-seq     (bitwise-and (parse-block s 19 23) #x3FFF)
    :node          (parse-block s 24 36)))
  
;; Following namespaces can be used for generation of uuids version 3 and 5
;; The DNS namespace.
(define +namespace-dns+ (string->uuid "6ba7b810-9dad-11d1-80b4-00c04fd430c8"))
;; The URL namespace
(define +namespace-url+ (string->uuid "6ba7b811-9dad-11d1-80b4-00c04fd430c8"))
;; The OID namespace
(define +namespace-oid+ (string->uuid "6ba7b812-9dad-11d1-80b4-00c04fd430c8"))
;; The x500 namespace
(define +namespace-x500+ (string->uuid "6ba7b814-9dad-11d1-80b4-00c04fd430c8"))

;; helper
(define get-current-time
  (let ((time-last -1)
	(uuids-this-tick (*uuids-per-tick*)))
    (lambda ()
      (let loop ((time-now (+ (* (microsecond) 10) #x01B21DD213814000)))
	(cond ((not (= time-last time-now))
	       (set! uuids-this-tick 0)
	       (set! time-last time-now)
	       time-now)
	      (else
	       (cond ((< uuids-this-tick (*uuids-per-tick*))
		      (set! uuids-this-tick (+ uuids-this-tick 1))
		      (+ time-now uuids-this-tick))
		     (else
		      ;; TODO sleep but we don't do it
		      (loop (+ (* (microsecond) 10) #x01B21DD213814000))))))))))
(define (random seed bound) (random-generator-random-integer seed bound))
;; generators
(define (make-null-uuid) (make <uuid>))
;; From RFC4122 reference implementation
;; Timestamp will be retrived inside of the procedure
(define (mask n shift mask)
  (let ((sn (if (zero? shift)
		n
		(bitwise-arithmetic-shift-right n shift))))
    (bitwise-and sn mask)))
(define make-v1-uuid
  (let ((node (bytevector->integer (get-mac-address)))
	(clock-seq (random (*uuid-random-state*) 10000)))
    (lambda ()
      (let ((timestamp (get-current-time)))
	(make <uuid>
	  :time-low (mask timestamp 0 #xFFFFFFFF)
	  :time-mid (mask timestamp 32 #xFFFF)
	  :ver #x0001
	  :time-high (mask timestamp 48 #x0FFF)
	  :var #b10
	  :clock-seq (bitwise-and clock-seq #x3FFF)
	  :node node)))))

;; Generates a version3 (name based MD5) uuid.
(define (make-v3-uuid namespace name)
  (format-v3or5-uuid
   (digest-uuid *digest:md5* (uuid->bytevector namespace) name)
   3))
;; Generates a version4 (random) uuid.
;; make this default random...
(define (make-v4-uuid :optional (seed (*uuid-random-state*)))
  (make <uuid>
    :time-low (random seed #xffffffff)
    :time-mid (random seed #xffff)
    :ver #x04
    :time-high (bitwise-and #x0FFF (random seed #xffff))
    :var #b10
    :clock-seq (bitwise-and #x3fff (random seed #xffff))
    :node (random seed #xffffffffffff)))

;; Generates a version5 (name based SHA1) uuid.
(define (make-v5-uuid namespace name)
  (format-v3or5-uuid
   (digest-uuid *digest:sha-1* (uuid->bytevector namespace) name)
   5))

;; compare
(define (uuid=? uuid1 uuid2)
  (or (eq? uuid1 uuid2)		; short cut
      (and (= (uuid-time-low uuid1) (uuid-time-low uuid2))
	   (= (uuid-time-mid uuid1) (uuid-time-mid uuid2))
	   (= (uuid-ver uuid1) (uuid-ver uuid2))
	   (= (uuid-time-high uuid1) (uuid-time-high uuid2))
	   (= (uuid-var uuid1) (uuid-var uuid2))
	   (= (uuid-clock-seq uuid1) (uuid-clock-seq uuid2))
	   (= (uuid-node uuid1) (uuid-node uuid2)))))

;; Converts an uuid to bytevector
(define (uuid->bytevector id)
  (bytevector-append 
   (integer->bytevector (uuid-time-low id) 4)
   (integer->bytevector (uuid-time-mid id) 2)
   (integer->bytevector (bitwise-ior
			 (bitwise-arithmetic-shift-left (uuid-ver id) 12)
			 (uuid-time-high id))
			2)
   (integer->bytevector (bitwise-ior
			 (bitwise-arithmetic-shift-left (uuid-var id) 14)
			 (uuid-clock-seq id))
			2)
   (integer->bytevector (uuid-node id) 6)))

(define (bytevector->uuid bv)
  (make <uuid>
    :time-low (bytevector->integer bv 0 4)
    :time-mid (bytevector->integer bv 4 6)
    :ver (bitwise-arithmetic-shift-right (bytevector-u8-ref bv 6) 4)
    :time-high (bitwise-and (bytevector->integer bv 6 8) #x0fff)
    :var (bitwise-arithmetic-shift-right (bytevector-u8-ref bv 8) 6)
    :clock-seq (bitwise-and (bytevector->integer bv 8 10) #x3fff)
    :node (bytevector->integer bv 10 16)))

(define (uuid->urn-format uuid)
  (string-append "urn:uuid:" (uuid->string uuid)))

(define (format-v3or5-uuid hash ver)
  (let ((result (bytevector->uuid (bytevector-copy hash 0 16))))
    (set! (ref result 'ver) ver)
    (set! (ref result 'var) #b10)
    result))
  
(define (digest-uuid digest uuid name)
  (let ((md (make-message-digest digest))
	(out (make-bytevector (digest-descriptor-digest-size digest) 0)))
    (message-digest-init! md)
    (message-digest-process! md uuid)
    (message-digest-process! md (string->utf8 name))
    (message-digest-done! md out)
    out))
)
