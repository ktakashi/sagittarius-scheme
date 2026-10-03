;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/operation.scm - HTTP operation lifecycle core
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
(library (net http-client operation)
    (export http:operation?
            http:operation-state
            http:operation-cancel!

            ;; internal APIs
            make-http:operation
            http:operation-transition!
            http:operation-cancelled?
            http:operation-terminal?
            http:operation-set-cancel-handler!
            http:operation-clear-cancel-handler!
	    http:operation-on-init! http:operation-on-finalize!
            http:operation-notify-headers!
            http:operation-notify-data!
            http:operation-notify-complete!
            http:operation-notify-error!)
    (import (rnrs)
            (srfi :18 multithreading))

(define-record-type http:operation
  (fields request
          (mutable %state)
          on-init
          on-headers
          on-data
          on-complete
	  on-finalize
          on-error
          lock
          (mutable cancel-handler))
  (protocol
   (lambda (p)
     (lambda (request on-init on-headers on-data on-complete on-finalize on-error)
       (p request
          'created
          on-init
          on-headers
          on-data
          on-complete
	  on-finalize
          on-error
          (make-mutex "http-operation-lock")
          #f)))))

(define-syntax with-operation-lock
  (syntax-rules ()
    ((_ operation exp ...)
     (let ((lock (http:operation-lock operation)))
       (dynamic-wind
           (lambda () (mutex-lock! lock))
           (lambda () exp ...)
           (lambda () (mutex-unlock! lock)))))))

(define (terminal-state? state)
  (memq state '(completed failed cancelled)))

(define (http:operation-state operation)
  (with-operation-lock operation
    (http:operation-%state operation)))

(define (http:operation-terminal? operation)
  (with-operation-lock operation
    (terminal-state? (http:operation-%state operation))))

(define (http:operation-cancelled? operation)
  (with-operation-lock operation
    (eq? (http:operation-%state operation) 'cancelled)))

(define (http:operation-transition! operation next-state)
  (with-operation-lock operation
    (let ((current (http:operation-%state operation)))
      (cond ((eq? current next-state) #t)
            ((terminal-state? current) #f)
            (else
             (http:operation-%state-set! operation next-state)
             #t)))))

(define (http:operation-set-cancel-handler! operation handler)
  (with-operation-lock operation
    (let ((current (http:operation-%state operation)))
      (cond ((terminal-state? current) #f)
            (else
             (http:operation-cancel-handler-set! operation handler)
             #t)))))

(define (http:operation-clear-cancel-handler! operation)
  (with-operation-lock operation
    (http:operation-cancel-handler-set! operation #f)
    #t))

(define (http:operation-cancel! operation)
  (define (check-state! operation)
    (with-operation-lock operation
      (let ((current (http:operation-%state operation)))
        (cond ((terminal-state? current) (values #f #f))
              (else
	       (let ((handler (http:operation-cancel-handler operation)))
		 (http:operation-cancel-handler-set! operation #f)
		 (http:operation-%state-set! operation 'cancelled)
		 (values #t handler)))))))
  (let-values (((cancelled? handler) (check-state! operation)))
    (when (and cancelled? handler) (handler))
    cancelled?))

(define (http:operation-on-init! operation . args)
  (apply (http:operation-on-init operation) args))

(define (http:operation-on-finalize! operation . args)
  (apply (http:operation-on-finalize operation) args))

(define (http:operation-notify-headers! operation context
             status headers has-data?)
  (and (http:operation-transition! operation 'receiving-headers)
       ((http:operation-on-headers operation)
   operation context status headers has-data?)
       #t))

(define (http:operation-notify-data! operation context data end?)
  (and (http:operation-transition! operation 'receiving-body)
  ((http:operation-on-data operation) operation context data end?)
       #t))

(define (http:operation-notify-complete! operation response)
  (let ((callback #f))
    (let ((deliver?
           (with-operation-lock operation
             (let ((current (http:operation-%state operation)))
               (cond ((terminal-state? current) #f)
                     (else
                      (http:operation-%state-set! operation 'completed)
                      (set! callback (http:operation-on-complete operation))
                      (http:operation-cancel-handler-set! operation #f)
                      #t))))))
      (when deliver? (callback operation response))
      deliver?)))

(define (http:operation-notify-error! operation err)
  (let ((callback #f))
    (let ((deliver?
           (with-operation-lock operation
             (let ((current (http:operation-%state operation)))
               (cond ((terminal-state? current) #f)
                     (else
                      (http:operation-%state-set! operation 'failed)
                      (set! callback (http:operation-on-error operation))
                      (http:operation-cancel-handler-set! operation #f)
                      #t))))))
      (when deliver? (callback operation err))
      deliver?)))

)
