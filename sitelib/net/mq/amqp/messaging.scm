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
    (export source make-amqp-source amqp-source?
	    target make-amqp-target amqp-target?)
    (import (except (rnrs) fields)
	    (sagittarius)
	    (net mq amqp types)
	    (net mq amqp transport))

  (define-restricted-type terminus-durability    :uint)
  (define-restricted-type terminus-expiry-policy :symbol)
  (define-restricted-type node-properties        fields)
  (define-restricted-type filter-set             :map)
  (define-restricted-type address-string  :string :provides (address))

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
  
  
)