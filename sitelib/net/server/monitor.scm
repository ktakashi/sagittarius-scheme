;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/server/monitor.scm - Simple server monitor
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (net server monitor)
    (export make-non-blocking-server-monitor
	    server-status?
	    report-server-status

	    server-status-target-server
	    server-status-num-threads
	    server-status-num-idling-threads
	    server-status-thread-statuses
	    thread-status-num-pending-task)
    (import (rnrs)
	    (sagittarius)
	    (clos user)
	    (util concurrent thread-pool)
	    (util concurrent shared-queue))

(define-record-type server-status
  (fields target-server
	  num-threads
	  num-idling-threads
	  thread-statuses))
(define-record-type thread-status
  (fields num-pending-task))

(define (make-non-blocking-server-monitor server thread-pool)
  (lambda ()
    (define (->thread-status queue)
      (make-thread-status (shared-queue-size queue)))
    (make-server-status server
			(thread-pool-size thread-pool)
			(thread-pool-idling-count thread-pool)
			(vector-map ->thread-status
				    (slot-ref thread-pool 'queues)))))

(define (report-server-status status :optional (to-port (current-error-port)))
  (let-values (((out extract) (open-string-output-port)))
    (format out "Total thread count: ~a~%" (server-status-num-threads status))
    (format out "Idling thread count: ~a~%"
	    (server-status-num-idling-threads status))
    (let ((statuses (server-status-thread-statuses status)))
      (do ((i 0 (+ i 1))
	   (len (vector-length statuses)))
	  ((= i len))
	(format out "  Thread #~a --- pending task ~a~%" (+ i 1)
		(thread-status-num-pending-task (vector-ref statuses i)))))
    (newline out)
    (display (extract) to-port)))

)
