;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/events.scm - DOM events
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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
;; https://dom.spec.whatwg.org/

(library (text xml dom events)
    (export (rename event <event>) make-event event?
	    event-type event-target event-current-target
	    event-composed-path
	    event-bubbles? event-cancelable? event-default-prevented?
	    event-composed?
	    event-time-stamp
	    ;; interface method
	    event:stop-propagation event:stop-immediate-propagation
	    event:prevent-default
	    ;; interface constant
	    +event:none+ +event:capturing-phase+ +event:at-target+
	    +event:bubbling-phase+
	    
	    make-event-init event-init?

	    (rename custom-event <custom-event>)
	    make-custom-event custom-event?
	    custom-event-detail

	    make-custom-event-init custom-event-init?
	    )
    (import (rnrs)
	    (sagittarius)
	    (clos)
	    (srfi :19 time))

(define-record-type event-init
  (fields bubbles?
	  cancelable?
	  composed?)
  (protocol (lambda (p)
	      (lambda (:key (bubbles? #f) (cancelable? #f) (composed? #f))
		(p bubbles? cancelable? composed?)))))

(define-constant +event:none+ 0)
(define-constant +event:capturing-phase+ 1)
(define-constant +event:at-target+ 2)
(define-constant +event:bubbling-phase+ 3)

;;; Event interface
(define-record-type event
  (fields type		 ;; DOMString
	  target	 ;; EventTarget?
	  current-target ;; EventTarget?
	  composed-path	 ;; sequence<EventTarget>
	  event-phase	 ;; unsigned short
	  bubbles?	 ;; boolean
	  cancelable?	 ;; boolean
	  default-prevented? ;; boolean
	  composed?	     ;; boolean
	  time-stamp	     ;; DOMHighResTimeStamp (using SRF-19 time)
	  )
  (protocol (lambda (p)
	      (lambda (type :optional (eid #f))
		;; To be properly done
		(p type #f #f '() +event:none+
		   (and eid (event-init-bubbles? eid))
		   (and eid (event-init-cancelable? eid))
		   #f
		   (and eid (event-init-composed? eid))
		   (current-time))))))

;; event methods
(define-generic event:stop-propagation)
(define-generic event:stop-immediate-propagation)
(define-generic event:prevent-default)

;;; CustomEvent
(define-record-type custom-event-init
  (parent event-init)
  (fields detail)
  (protocol (lambda (n)
	      (lambda (:key (detail #f) :allow-other-keys opt)
		(let ((p (apply n opt)))
		  (p detail))))))
(define-record-type custom-event
  (parent event)
  (fields detail) ;; any
  (protocol (lambda (n)
	      (lambda (type :optional (eid #f))
		(let ((p (n type eid)))
		  (p (and (custom-event-init? eid)
			  (custom-event-init-detail eid))))))))
)

