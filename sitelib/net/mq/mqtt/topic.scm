;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/mqtt/topic.scm - MQTT v3.1.1 utilities for topics
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

(library (net mq mqtt topic)
    (export mqtt-valid-topic?
	    mqtt-topic-match?
	    mqtt-topic-compare
	    mqtt-topic<?
	    mqtt-topic>?
	    mqtt-topic=?
	    mqtt-topic<=?
	    mqtt-topic>=?

	    make-mqtt-topic
	    mqtt-topic-enqueue!
	    mqtt-topic-dequeue!
	    mqtt-topic-name
	    mqtt-topic-retain-message
	    mqtt-topic-retain-message-set!
	    mqtt-topic-retain-qos
	    mqtt-topic-retain-qos-set!
	    <mqtt-topic>
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius object)
	    (net mq mqtt packet)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (util queue))
  (define-class <mqtt-topic> ()
    ((name  :init-keyword :name :reader mqtt-topic-name)
     (queue :init-form (make-mtqueue))
     (retain-message :init-value #f :reader mqtt-topic-retain-message
		     :writer mqtt-topic-retain-message-set!)
     (retain-qos     :init-value +qos-at-most-once+ 
		     :reader mqtt-topic-retain-qos
		     :writer mqtt-topic-retain-qos-set!)))
  (define-class <mqtt-topic-entry> ()
    ((qos :init-keyword :qos)
     (payload :init-keyword :payload)))
  (define (make-mqtt-topic name) (make <mqtt-topic> :name name))
  (define (mqtt-topic-enqueue! topic qos payload)
    (enqueue! (~ topic 'queue) (make <mqtt-topic-entry> 
				 :qos qos :payload payload)))
  (define (mqtt-topic-dequeue! topic)
    (let ((e (dequeue! (~ topic 'queue))))
      (values (~ e 'qos) (~ e 'payload))))

  ;; checks given topic/topic filter is valid
  (define (mqtt-valid-topic? topic)
    (define (check-valid-hash topic)
      (cond ((string-contains topic "#") =>
	     (lambda (pos)
	       ;; "#" must be last
	       (let ((len (string-length topic)))
		 (cond ((= len 1))		    ;; topic is "#"
		       ((not (= pos (- len 1))) #f) ;; topic is /foo/#/bar
		       ;; topic is /foo/bar#
		       ((not (char=? #\/ (string-ref topic (- pos 1)))) #f)
		       (else #t)))))
	    ;; no hash
	    (else #t)))
    ;; + can be multiple...
    (define (check-valid+ topic)
      (let ((len (string-length topic)))
	(let loop ((i 0) (prev #f))
	  (if (= i len)
	      #t
	      (let ((c (string-ref topic i)))
		(if (char=? #\+ (string-ref topic i))
		    ;; it doesn't specify ++ case, we treat it as a topic
		    ;; so it's invalid (like soprt+ case)
		    (and (or (zero? i) (char=? #\/ prev))
			 (loop (+ i 1) c))
		    (loop (+ i 1) c)))))))
    (and (not (string-null? topic))
	 (check-valid+ topic)
	 (check-valid-hash topic)))

  ;; `filter` is a topic filter e.g.) "topic/#"
  ;; `topic` is actual topic name. e.g.) "topic/foo"
  ;; now we need to make them match
  ;; NOTE: Assumes `filter` is a valid topic
  (define topic-name-set (char-set-complement (string->char-set "/")))
  ;; add empty entry for convenience
  (define (topic-tokenize topic)
    (define (string-last s)
      (let ((len (string-length s)))
	(string-ref s (- len 1))))
    (define (%fixup topic topics)
      (let ((l (if (char=? (string-last topic) #\/) '("") '()))
	    (f (if (char=? (string-ref topic 0) #\/) '("") '())))
	(append f topics l)))
    (%fixup topic (string-tokenize topic topic-name-set)))

  (define (mqtt-topic-match? filter topic)    
    (let ((filters (topic-tokenize filter))
	  (topics  (topic-tokenize topic)))
      (let loop ((first? #t) (filters filters) (topics topics))
	(cond ((and (null? filters) (null? topics)))   ;; matched
	      ((or (null? filters) (null? topics))
	       (and (not (null? filters))
		    ;; foo/# foo case
		    (or (string=? (car filters) "#"))))
	      ((string=? (car filters) "#")
	       (or (not first?)
		   ;; starting "$" doesn't match if with "#"
		   (not (char=? #\$ (string-ref topic 0)))))
	      ((string=? (car filters) "+")
	       (and (or (not first?)
			;; starting "$" doesn't match if with "+"
			(and (not (char=? #\$ (string-ref topic 0)))))
		    (loop #f (cdr filters) (cdr topics))))
	      ((string=? (car filters) (car topics))
	       (loop #f (cdr filters) (cdr topics)))
	      (else #f)))))

  ;; compares given topics. 
  ;; the returning values are followings
  ;;   -1 a < b - b is more specific
  ;;   0  a = b - equally specified
  ;;   1  a > b - a is more specific
  ;; Definition
  ;;   - longer is more specific
  ;;   - '#' makes less specific
  ;;   - '+' makes less specific ('#' < '+')
  ;;   - the same hierarcy is equally specified
  ;; NOTE: a = b doesn't mean topics are the same but equally specified
  (define (mqtt-topic-compare a b)
    (define (plus/hash s)
      (or (string=? s "#") (string=? s "+")))
    (unless (and (mqtt-valid-topic? a)
		 (mqtt-valid-topic? b))
      (error 'mqtt-topic-compare "not a valid topic" a b))
    (let loop ((a* (topic-tokenize a)) (b* (topic-tokenize b)))
      (cond ((and (null? a*) (null? b*)) 0)
	    ((null? a*) -1)
	    ((null? b*) 1)
	    ;; avoid +/foo +/bar thing
	    ((string=? (car a*) (car b*)) (loop (cdr a*) (cdr b*)))
	    ((plus/hash (car a*)) -1)
	    ((plus/hash (car b*)) 1)
	    (else (loop (cdr a*) (cdr b*))))))

  (define (mqtt-topic<? a b)  (< (mqtt-topic-compare a b) 0))
  (define (mqtt-topic>? a b)  (> (mqtt-topic-compare a b) 0))
  (define (mqtt-topic=? a b)  (= (mqtt-topic-compare a b) 0))
  (define (mqtt-topic<=? a b) (<= (mqtt-topic-compare a b) 0))
  (define (mqtt-topic>=? a b) (>= (mqtt-topic-compare a b) 0))
)

;; 
;; reference
;;   http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/cos01/mqtt-v3.1.1-cos01.html
;; 
;; From 4.7.1.2 Multi-level wildcard
;; 
;; The number sign ('#' U+0023) is a wildcard character that matches
;; any number of levels within a topic. The multi-level wildcard 
;; represents the parent and any number of child levels. The multi-level
;; wildcard character MUST be specified either on its own or following a
;; topic level separator. In either case it MUST be the last character
;; specified in the Topic Filter [MQTT-4.7.1-2].
;; 
;; Non normative comment
;; For example, if a Client subscribes to "sport/tennis/player1/#",
;; it would receive messages published using these topic names:
;; - "sport/tennis/player1"
;; - "sport/tennis/player1/ranking"
;; - "sport/tennis/player1/score/wimbledon"
;; 
;; Non normative comment
;; - "sport/#" also matches the singular "sport", since # includes
;;   the parent level.
;; - "#" is valid and will receive every Application Message
;; - "sport/tennis/#" is valid
;; - "sport/tennis#" is not valid
;; - "sport/tennis/#/ranking" is not valid

;; From 4.7.1.3 Single level wildcard
;; 
;; The plus sign ('+' U+002B) is a wildcard character that matches only
;; one topic level.
;; The single-level wildcard can be used at any level in the Topic Filter,
;; including first and last levels. Where it is used it MUST occupy an
;; entire level of the filter [MQTT-4.7.1-3]. It can be used at more than
;; one level in the Topic Filter and can be used in conjunction with the
;; multilevel wildcard.
;;
;; Non normative comment
;; For example, "sport/tennis/+" matches "sport/tennis/player1" and
;; "sport/tennis/player2", but not "sport/tennis/player1/ranking". Also,
;; because the single-level wildcard matches only a single level, "sport/+"
;; does not match "sport" but it does match "sport/".
;; 
;; Non normative comment
;; - "+" is valid
;; - "+/tennis/#" is valid
;; - "sport+" is not valid
;; - "sport/+/player1" is valid
;; - "/finance" matches "+/+" and "/+", but not "+"

;; From 4.7.2  Topics beginning with $
;;
;; The Server MUST NOT match Topic Filters starting with a wildcard
;; character (# or +) with Topic Names beginning with a $ character
;; [MQTT-4.7.2-1]. The Server SHOULD prevent Clients from using such
;; Topic Names to exchange messages with other Clients. Server
;; implementations MAY use Topic Names that start with a leading $
;; character for other purposes. 
;;
;; Non normative comment
;; - $SYS/ has been widely adopted as a prefix to topics that contain
;;   Server-specific information or control APIs
;; - Applications cannot use a topic with a leading $ character for
;;   their own purposes
;; 
;; Non normative comment
;; - A subscription to "#" will not receive any messages published to a
;;   topic beginning with a $
;; - A subscription to "+/monitor/Clients" will not receive any messages
;;   published to "$SYS/monitor/Clients"
;; - A subscription to "$SYS/#" will receive messages published to topics
;;   beginning with "$SYS/"
;; - A subscription to "$SYS/monitor/+" will receive messages published
;;   to "$SYS/monitor/Clients"
;; - For a Client to receive messages from topics that begin with $SYS/
;;   and from topics that don't begin with a $, it has to subscribe to both
;;   "#" and "$SYS/#"

