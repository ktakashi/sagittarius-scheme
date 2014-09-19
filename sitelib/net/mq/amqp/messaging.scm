;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/amqp/messagint/types - AMQP v1.0 messaging 
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

;; reference:
;; http://docs.oasis-open.org/amqp/core/v1.0/os/amqp-core-messaging-v1.0-os.html

(library (net mq amqp messaging)
    (export make-amqp-source amqp-source?
	    make-amqp-target amqp-target?
	    address-string
	    ;; handling disposition message
	    disposition-handler

	    +message-format+
	    ;; utility
	    annotation-set!
	    annotation-ref
	    annotation-delete!

	    <amqp-header> make-amqp-header
	    <amqp-delivery-annotations> make-amqp-delivery-annotation
	    <amqp-message-annotations> make-amqp-message-annotation
	    <amqp-properties> make-amqp-properties
	    <amqp-application-properties> make-amqp-application-properties
	    <amqp-data> make-amqp-data
	    <amqp-amqp-sequence>
	    <amqp-amqp-value> make-amqp-amqp-value
	    <amqp-footer> make-amqp-footer
	    )
    (import (except (rnrs) fields)
	    (sagittarius)
	    (sagittarius object)
	    (clos user)
	    (clos core)
	    (net mq amqp types)
	    (net mq amqp transport))

  (define-restricted-type terminus-durability    :uint)
  (define-restricted-type terminus-expiry-policy :symbol)
  (define-restricted-type node-properties        fields)
  (define-restricted-type filter-set             :map)
  (define-restricted-type address-string  :string :provides (address))
  (define-restricted-type annotations            :map)

  (define-constant +message-format+ 0)

  ;; terminus-durability
  (define-constant +amqp-none+            0)
  (define-constant +amqp-configuration+   1)
  (define-constant +amqp-unsettled-state+ 2)
  ;; terminus-expiry-policy
  (define-constant +amqp-link-detach+      'link-detach)
  (define-constant +amqp-session-end+      'session-end)
  (define-constant +amqp-connection-close+ 'connection-close)
  (define-constant +amqp-never+            'never)

  (define-composite-type source amqp:source:list #x00000000 #x00000028
    ((address         :type :* :requires 'address)
     (durable         :type terminus-durability :default +amqp-none+)
     (expiry-policy   :type terminus-expiry-policy :default +amqp-session-end+)
     (timeout         :type seconds :default 0)
     (dynamic         :type :boolean :default #f)
     (dynamic-node-properties :type node-properties)
     (distribution-mode :type :symbol)
     (filter          :type filter-set)
     (default-outcome :type :* :requires 'outcome)
     (outcomes        :type :symbol :multiple #t)
     (capabilities    :type :symbol :multiple #t))
    :provides (source))
  
  (define-composite-type target amqp:target:list #x00000000 #x00000029
    ((address         :type :* :requires 'address)
     (durable         :type terminus-durability :default +amqp-none+)
     (expiry-policy   :type terminus-expiry-policy :default +amqp-session-end+)
     (timeout         :type seconds :default 0)
     (dynamic         :type :boolean :default #f)
     (dynamic-node-properties :type node-properties)
     (capabilities    :type :symbol :multiple #t))
    :provides (target))

  ;; delivery-state
  (define-composite-type received amqp:received:list #x00000000 #x00000023
    ((section-number :type :uint  :mandatory #t)
     (section-offset :type :ulong :mandatory #t))
    :provides (delivery-state))

  ;; outcome and delivery-state
  (define-composite-type accepted amqp:accepted:list #x00000000 #x00000024 ()
    :provides (delivery-state outcome))
  (define-composite-type rejected amqp:rejected:list #x00000000 #x00000025 
    ((error :type error))
    :provides (delivery-state outcome))
  (define-composite-type released amqp:released:list #x00000000 #x00000026 ()
    :provides (delivery-state outcome))
  (define-composite-type modified amqp:modified:list #x00000000 #x00000027
    ((delivery-failed :type :boolean)
     (undeliverable-here :type :boolean)
     (message-annotations :type fields))
    :provides (delivery-state outcome))
  
  ;; 3.2.1 Header
  (define-composite-type header amqp:header:list #x00000000 #x00000070
    ((durable  :type :boolean :default #f)
     (priority :type :ubyte   :default 4)
     (ttl      :type milliseconds)
     (first-aquirer :type :boolean :default #f)
     (delivery-count :type :uint :default 0))
    :provides (section))

  ;; 3.2.2 Delivery Annotations
  (define-restricted-type delivery-annotations annotations
    :provides (section)
    :descriptor (amqp:delivery-annotations:map #x00000000 #x00000071))
  (define (make-amqp-delivery-annotation)
    (make <amqp-delivery-annotations> 
      :value (->amqp-value :map (make-equal-hashtable))))

  ;; 3.2.3 Message Annotations
  (define-restricted-type message-annotations annotations
    :provides (section)
    :descriptor (amqp:message-annotations:map #x00000000 #x00000072))
  (define (make-amqp-message-annotation)
    (make <amqp-message-annotations> 
      :value (->amqp-value :map (make-equal-hashtable))))

  ;; 3.2.4 Properties
  (define-composite-type properties amqp:properties:list #x00000000 #x00000073
    ((message-id :type :* :requires 'message-id)
     (user-id :type :binary)
     (to :type :* :requires 'address)
     (subject :type :string)
     (reply-to :type :* :requires 'address)
     (correlation-id :type :* :requires 'address)
     (content-type :type :symbol)
     (content-encoding :type :symbol)
     (absolute-expiry-time :type :timestamp)
     (creation-time :type :timestamp)
     (group-id :type :string)
     (group-sequence :type sequence-no)
     (reply-to-group-id :type :string)))

  ;; 3.2.5 Application Properties
  (define-restricted-type application-properties :map
    :provides (section)
    :descriptor (amqp:application-properties:map #x00000000 #x00000074))

  (define (make-amqp-application-properties)
    (make <amqp-application-properties> 
      :value (->amqp-value :map (make-equal-hashtable))))

  ;; 3.2.6 Data
  (define-restricted-type data :binary
    :provides (section)
    :descriptor (amqp:data:binary #x00000000 #x00000075))
  (define (make-amqp-data bv) (make <amqp-data> :value bv))

  ;; 3.2.7 Amqp Sequence
  (define-restricted-type amqp-sequence :list
    :provides (section)
    :descriptor (amqp:amqp-sequence:list #x00000000 #x00000076))

  ;; 3.2.8 Amqp Value
  (define-restricted-type amqp-value :*
    :provides (section)
    :descriptor (amqp:amqp-value:* #x00000000 #x00000077))
  (define (make-amqp-amqp-value value)
    (make <amqp-amqp-value> :value value))

  ;; 3.2.9 Footer
  (define-restricted-type footer annotations
    :provides (section)
    :descriptor (amqp:footer:map #x00000000 #x00000078))
  (define (make-amqp-footer)
    (make <amqp-footer> 
      :value (->amqp-value :map (make-equal-hashtable))))

  ;; utilities
  (define (annotation-set! annot key value)
    (let* ((class (class-of annot))
	   (type (~ class 'restricted)))
      (unless (eq? type :map)
	(error 'annotation-set! "amqp-annotation required" annot))
      (hashtable-set! (scheme-value (scheme-value annot)) key value)))

  (define (annotation-ref annot key)
    (let* ((class (class-of annot))
	   (type (~ class 'restricted)))
      (unless (eq? type :map)
	(error 'annotation-ref "amqp-annotation required" annot))
      (hashtable-ref (scheme-value (scheme-value annot)) key)))

  (define (annotation-delete! annot key)
    (let* ((class (class-of annot))
	   (type (~ class 'restricted)))
      (unless (eq? type :map)
	(error 'annotation-delete! "amqp-annotation required" annot))
      (hashtable-delete! (scheme-value (scheme-value annot)) key)))

  ;; FIXME
  (define (disposition-handler disposition make-disposition)
    (if (symbol? disposition)
	(case disposition
	  ((accepted) (make-disposition #t (make-amqp-accepted)))
	  (else       (make-disposition #f (make-amqp-released))))
	(let ((state (~ disposition 'state)))
	  (if (amqp-accepted? state)
	      (make-disposition #t (make-amqp-accepted))
	      (make-disposition #f (make-amqp-received))))))
)
