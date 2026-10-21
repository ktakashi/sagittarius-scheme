;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/http2/stream-manager.scm - HTTP/2 stream capacity manager
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (rfc http2 stream-manager)
    (export http2-stream-manager?
	    make-http2-stream-manager
	    http2-stream-manager-streams
	    http2-stream-manager-max-concurrent-streams
	    http2-stream-manager-max-concurrent-streams-set!
	    http2-stream-manager-active-stream-count
	    http2-stream-manager-next-client-stream-id!
	    http2-stream-manager-can-accept-new-stream?
	    http2-stream-manager-draining?
	    http2-stream-manager-goaway-last-stream-id
	    http2-stream-manager-mark-goaway!
	    http2-stream-manager-find-stream
	    http2-stream-manager-register-stream!
	    http2-stream-manager-unregister-stream!
	    http2-stream-manager-for-each-stream)
    (import (rnrs)
	    (srfi :18 multithreading))

(define-record-type http2-stream-manager
  (fields streams
	  lock
	  (mutable max-concurrent-streams)
	  (mutable active-stream-count)
	  (mutable draining?)
	  (mutable goaway-last-stream-id)
	  (mutable next-client-stream-id))
  (protocol
   (lambda (n)
     (case-lambda
      (() (n (make-eqv-hashtable)
	     (make-mutex "http2-stream-manager")
	     100 0 #f #f 1))
      ((max-concurrent-streams)
       (n (make-eqv-hashtable)
	  (make-mutex "http2-stream-manager")
	  max-concurrent-streams 0 #f #f 1))
      ((max-concurrent-streams next-client-stream-id)
       (n (make-eqv-hashtable)
	  (make-mutex "http2-stream-manager")
	  max-concurrent-streams 0 #f #f next-client-stream-id))))))

(define-syntax with-manager-lock
  (syntax-rules ()
    ((_ manager body ...)
     (dynamic-wind
	 (lambda () (mutex-lock! (http2-stream-manager-lock manager)))
	 (lambda () body ...)
	 (lambda () (mutex-unlock! (http2-stream-manager-lock manager)))))))

(define (http2-stream-manager-next-client-stream-id! manager)
  (with-manager-lock manager
    (let ((sid (http2-stream-manager-next-client-stream-id manager)))
      (http2-stream-manager-next-client-stream-id-set! manager (+ sid 2))
      sid)))

(define (http2-stream-manager-can-accept-new-stream? manager)
  (with-manager-lock manager
    (and (not (http2-stream-manager-draining? manager))
	 (< (http2-stream-manager-active-stream-count manager)
	    (http2-stream-manager-max-concurrent-streams manager)))))

(define (http2-stream-manager-mark-goaway! manager last-stream-id)
  (with-manager-lock manager
    (http2-stream-manager-draining?-set! manager #t)
    (http2-stream-manager-goaway-last-stream-id-set! manager last-stream-id)
    #t))

(define (http2-stream-manager-find-stream manager sid)
  (hashtable-ref (http2-stream-manager-streams manager) sid #f))

(define (http2-stream-manager-register-stream! manager sid stream)
  (with-manager-lock manager
    (if (and (not (http2-stream-manager-draining? manager))
	     (< (http2-stream-manager-active-stream-count manager)
		(http2-stream-manager-max-concurrent-streams manager)))
	(begin
	  (hashtable-set! (http2-stream-manager-streams manager) sid stream)
	  (http2-stream-manager-active-stream-count-set!
	   manager
	   (+ (http2-stream-manager-active-stream-count manager) 1))
	  #t)
	#f)))

(define (http2-stream-manager-unregister-stream! manager sid)
  (with-manager-lock manager
    (when (hashtable-ref (http2-stream-manager-streams manager) sid #f)
      (hashtable-delete! (http2-stream-manager-streams manager) sid)
      (http2-stream-manager-active-stream-count-set!
       manager
       (max 0 (- (http2-stream-manager-active-stream-count manager) 1))))
    #t))

(define (http2-stream-manager-for-each-stream manager proc)
  (let-values (((keys values)
		(hashtable-entries (http2-stream-manager-streams manager))))
    (let loop ((i 0) (n (vector-length keys)))
      (unless (= i n)
	(let ((v (vector-ref values i)))
	  (when v (proc v)))
	(loop (+ i 1) n)))))

)
