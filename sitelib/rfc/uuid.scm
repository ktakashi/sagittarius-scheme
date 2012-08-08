;;; -*- Scheme -*-
;;;
;;; uuid.scm - UUID generator library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; RFC 4122 UUID
(library (rfc uuid)
    (export <uuid>
	    make-null-uuid
	    make-v1-uuid
	    make-v3-uuid
	    make-v4-uuid
	    make-v5-uuid
	    uuid?
	    uuid=?
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
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius object)
	    (clos user)
	    (math)
	    (srfi :39 parameters))

  ;; Holds the amount of ticks per count. The ticks per count determine
  ;; the number of possible version 1 uuids created for one time interval.
  (define *uuids-per-tick* (make-parameter 1024))

  ;; Represents an uuid
  (define-class <uuid> ()
    (;; unsigned 32 bit integer
     ;; The low field of the timestamp
     (time-low :init-keyword :time-low
	       :init-value 0
	       :reader uuid-time-low)
     ;; unsigned 16 bit integer
     ;; The middle field of the timestamp
     (time-mid :init-keyword :time-mid
	       :init-value 0
	       :reader uuid-time-mid)
     ;; unsigned 16 bit integer
     ;; The high field of the timestamp multiplexed with the version number
     (time-hi-and-version :init-keyword :time-high
			  :init-value 0
			  :reader uuid-time-high)
     ;; unsigned 8 bit integer
     ;; The high field of the clock sequence multiplexed with the variant
     (clock-seq-and-reserved :init-keyword :clock-seq-var
			     :init-value 0
			     :reader uuid-clock-seq-var)
     ;; unsigned 8 bit integer
     ;; The low field of the clock sequence
     (clock-seq-low :init-keyword :clock-seq-low
		    :init-value 0
		    :reader uuid-clock-seq-low)
     ;; unsigned 48 bit integer
     ;; The spatially unique node identifier
     (node :init-keyword :node
	   :init-value 0
	   :reader uuid-node)))

  (define (uuid? obj) (is-a? obj <uuid>))
  (define-method write-object ((uuid <uuid>) out)
    (format out "#<uuid ~a>" (uuid->string uuid)))
  (define-method object-equal? ((uuid1 <uuid>) (uuid2 <uuid>))
    (uuid=? uuid1 uuid2))

  (define-method ->string ((uuid <uuid>)) (uuid->string uuid))

  (define (uuid->string uuid)
    (format "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~12,'0X"
	    (uuid-time-low uuid)
	    (uuid-time-mid uuid)
	    (uuid-time-high uuid)
	    (uuid-clock-seq-var uuid)
	    (uuid-clock-seq-low uuid)
	    (uuid-node uuid)))
  ;; creates an uuid from the string representation of an uuid.
  ;; (example input string 6ba7b810-9dad-11d1-80b4-00c04fd430c8)
  (define-constant +uuid-string-length+ 36)
  (define (string->uuid s)
    (define (parse-block s start end) (->number (substring s start end) 16))
    (unless (= (string-length s) +uuid-string-length+)
      (assertion-violation 
       'string->uuid (format "Could not parse ~s as UUID: invalid length" s)
       s))
    (unless (and (char=? (ref s  8) #\-)
		 (char=? (ref s 13) #\-)
		 (char=? (ref s 18) #\-)
		 (char=? (ref s 23) #\-))
      (assertion-violation
       'string->uuid (format "Could not parse ~s as UUID: invalid format" s)
       s))
    (make <uuid>
      :time-low      (parse-block s  0  8)
      :time-mid      (parse-block s  9 13)
      :time-high     (parse-block s 14 18)
      :clock-seq-var (parse-block s 19 21)
      :clock-seq-low (parse-block s 21 23)
      :node          (parse-block s 24 36)))
  
  ;; Following namespaces can be used for generation of uuids version 3 and 5
  ;; The DNS namespace.
  (define +namespace-dns+ (string->uuid "6ba7b810-9dad-11d1-80b4-00c04fd430c8"))
  ;; The URL namespace
  (define +namespace-url+ (string->uuid "6ba7b811-9dad-11d1-80b4-00c04fd430c8"))
  ;; The OID namespace
  (define +namespace-oid+ (string->uuid "6ba7b812-9dad-11d1-80b4-00c04fd430c8"))
  ;; The x500 namespace
  (define +namespace-x500+ 
    (string->uuid "6ba7b814-9dad-11d1-80b4-00c04fd430c8"))

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
			(loop (+ (* (microsecond) 10) #x01B21DD213814000))))))
	  ))))
  ;; generators
  (define (make-null-uuid) (make <uuid>))
  ;; From RFC4122 reference implementation
  ;; Timestamp will be retrived inside of the procedure
  (define (mask n shift mask)
    (let ((sn (if (zero? shift)
		  n
		  (bitwise-arithmetic-shift-right n shift))))
      (bitwise-and sn mask)))
  (define (make-v1-uuid clock-seq node)
    (let ((timestamp (get-current-time)))
      (make <uuid>
	:time-low (mask timestamp 0 #xFFFFFFFF)
	:time-mid (mask timestamp 32 #xFFFF)
	:time-high (bitwise-ior (mask timestamp 48 #x0FFF) #x1000)
	:clock-seq-var (bitwise-ior (bitwise-arithmetic-shift-right 
				     (bitwise-and clock-seq #x3F00)
				     8)
				    #x80)
	:clock-seq-low (bitwise-and clock-seq #xFF)
	:node node)))

  ;; Generates a version3 (name based MD5) uuid.
  (define (make-v3-uuid namespace name)
    (format-v3or5-uuid
     (digest-uuid MD5 (uuid->bytevector namespace) name)
     3))
  ;; Generates a version4 (random) uuid.
  ;; make this default random...
  (define (make-v4-uuid :optional (seed (secure-random RC4)))
    (make <uuid>
      :time-low (random seed #xffffffff)
      :time-mid (random seed #xffff)
      :time-high (bitwise-ior #x4000 (bitwise-and #x0FFF (random seed #xffff)))
      :clock-seq-var (bitwise-ior #x80 (bitwise-and #x3F (random seed #xff)))
      :clock-seq-low (random seed #xff)
      :node (random seed #xffffffffffff))
    )

  ;; Generates a version5 (name based SHA1) uuid.
  (define (make-v5-uuid namespace name)
    (format-v3or5-uuid
     (digest-uuid SHA-1 (uuid->bytevector namespace) name)
     5))

  ;; compare
  (define (uuid=? uuid1 uuid2)
    (or (eq? uuid1 uuid2)		; short cut
	(and (= (uuid-time-low uuid1) (uuid-time-low uuid2))
	     (= (uuid-time-mid uuid1) (uuid-time-mid uuid2))
	     (= (uuid-time-high uuid1) (uuid-time-high uuid2))
	     (= (uuid-clock-seq-var uuid1) (uuid-clock-seq-var uuid2))
	     (= (uuid-clock-seq-low uuid1) (uuid-clock-seq-low uuid2))
	     (= (uuid-node uuid1) (uuid-node uuid2)))))

  ;; Converts an uuid to bytevector
  (define (uuid->bytevector id)
    (bytevector-append 
     (integer->bytevector (uuid-time-low id) 4)
     (integer->bytevector (uuid-time-mid id) 2)
     (integer->bytevector (uuid-time-high id) 2)
     (integer->bytevector (uuid-clock-seq-var id) 1)
     (integer->bytevector (uuid-clock-seq-low id) 1)
     (integer->bytevector (uuid-node id) 6)))

  (define (bytevector->uuid bv)
    (make <uuid>
      :time-low (bytevector->integer bv 0 4)
      :time-mid (bytevector->integer bv 4 6)
      :time-high (bytevector->integer bv 6 8)
      :clock-seq-var (bytevector-u8-ref bv 8)
      :clock-seq-low (bytevector-u8-ref bv 9)
      :node (bytevector->integer bv 10 16)))

  (define (format-v3or5-uuid hash ver)
    (define logior bitwise-ior)
    (define logand bitwise-and)
    (define u8-ref bytevector-u8-ref)
    (define ash bitwise-arithmetic-shift)
    (let ((result (bytevector->uuid (bytevector-copy hash 0 16))))
      (set! (ref result 'time-hi-and-version)
	    (logior (ash ver 12)
		    (logand #x0FFF (uuid-time-high result))))
      (set! (ref result 'clock-seq-and-reserved)
	    (logior #x80 (logand #x3F (u8-ref hash 8))))
      result))
  
  (define (digest-uuid digest uuid name)
    (let* ((hash (hash-algorithm digest))
	   (out (make-bytevector (hash-size hash) 0)))
      (hash-init! hash)
      (hash-process! hash uuid)
      (hash-process! hash (string->utf8 name))
      (hash-done! hash out)
      out))
)
