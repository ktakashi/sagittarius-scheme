;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/xpath/tools.scm - XPath tools
;;;
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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


;; This library is bascally DOM object version of
;; sxpath tools (e.g. sxml:descendant)
#!nounbound
(library (text xml xpath tools)
    (export xml:descendant
	    xml:descendant-or-self

	    ;; utilities
	    xml:child
	    xml:filter
	    )
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :117 list-queues)
	    (text xml dom nodes))

(define (ncar nl) (node-list:item nl 0))
(define (ncdr nl) (node-list-sub-list nl 1))
(define (nappend nl nl2) (node-list-append nl nl2))

;;; descendant axis
(define (xml:descendant test-pred?)
  (lambda (node)
    (if (node-list? node)
	(node-list:map-union (xml:descendant test-pred?) node)
	(do ((res '() (if (test-pred? (ncar more)) (cons (ncar more) res) res))
	     (more ((xml:child node?) node)
		   (nappend ((xml:child node?) (ncar more)) (ncdr more))))
	    ((zero? (node-list-length more))
	     (list->node-list (reverse! res)))))))

;; descendant-or-self
(define (xml:descendant-or-self test-pred?)
  (lambda (node)
    (if (node-list? node)
	(node-list:map-union (xml:descendant-or-self test-pred?) node)
	(do ((res '() (if (test-pred? (ncar more)) (cons (ncar more) res) res))
	     (more (list->node-list (list node))
		   (nappend ((xml:child node?) (ncar more)) (ncdr more))))
	    ((zero? (node-list-length more))
	     (list->node-list (reverse! res)))))))

(define (node-list:map-union proc node-list)
  (define len (node-list-length node-list))
  (define (push-all proc-res queue)
    (do ((len (node-list-length proc-res)) (i 0 (+ i 1)))
	((= i len))
      (list-queue-add-back! queue (node-list:item proc-res i))))
  (do ((i 0 (+ i 1)) (queue (list-queue)))
      ((= i len) (make-node-list queue))
    (let ((proc-res (proc (node-list:item node-list i))))
      (if (node-list? proc-res)
	  (push-all proc-res queue)
	  (list-queue-add-back! queue proc-res)))))

(define (xml:child test-pred?)
  (lambda (node)
    (cond ((node? node) ((xml:filter test-pred?) (node-child-nodes node)))
	  ((node-list? node)
	   (node-list:map-union (xml:child test-pred?) node))
	  (else #f))))

(define (xml:filter pred?)
  (lambda (nl)
    (define node-list (if (node-list? nl) nl (list->node-list (list nl))))
    (define len (node-list-length node-list))
    (let loop ((i 0) (res '()))
      (if (= i len)
	  (list->node-list (reverse! res))
	  (let* ((item (node-list:item node-list i))
		 (pred-result (pred? item)))
	    (loop (+ i 1) (if pred-result (cons item res) res)))))))

)
