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
	    server-status-thread-count
	    server-status-thread-statuses
	    thread-status-thread-info
	    thread-status-thread-id
	    thread-status-active-socket-count
	    )
    (import (rnrs)
	    (sagittarius)
	    (util concurrent thread-pool)
	    (util concurrent shared-queue))

(define-record-type server-status
  (fields target-server
	  thread-count
	  thread-statuses))
(define-record-type thread-status
  (fields thread-id thread-info active-socket-count))

(define (make-non-blocking-server-monitor server thread-pool socket-manager)
  (lambda ()
    (define (->thread-status e)
      (let ((tid (car e)))
	(make-thread-status tid
			    ;; we don't want to expose thread itself
			    ;; so write it :)
			    (format "~a" (thread-pool-thread thread-pool tid))
			    (cdr e))))
    (define (socket-manager->list)
      (list-sort (lambda (a b) (< (car a) (car b)))
		 (shared-priority-queue->list socket-manager)))
    (make-server-status server
			(thread-pool-size thread-pool)
			(map ->thread-status (socket-manager->list)))))

(define (report-server-status status :optional (to-port (current-error-port)))
  (let-values (((out extract) (open-string-output-port)))
    (format out "Total thread count: ~a~%" (server-status-thread-count status))
    (let ((statuses (server-status-thread-statuses status)))
      ;; this must be the same as total thread count
      ;; if this is not the same then it's a bug on (net server)
      (format out "Active thread count: ~a~%" (length statuses))
      (for-each (lambda (status)
		  (format out "  Thread #~a ~a~%"
			  (thread-status-thread-id status)
			  (thread-status-thread-info status))
		  (format out "    - active sockets ~a~%"
			  (thread-status-active-socket-count status)))
		statuses))
    (newline out)
    (display (extract) to-port)))

)
