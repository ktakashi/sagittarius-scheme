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
	    xml:ancestor
	    xml:ancestor-or-self

	    xml:attribute

	    xml:boolean
	    
	    ;; utilities
	    xml:child
	    xml:filter
	    xml:ntype??
	    xml:map-union
	    )
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :117 list-queues)
	    (text xml errors)
	    (text xml dom nodes))

(define (ncar nl) (node-list:item nl 0))
(define (ncdr nl) (node-list-sub-list nl 1))
(define (nappend nl nl2) (node-list-append nl nl2))

;;; descendant axis
(define (xml:descendant test-pred?)
  (lambda (node)
    (if (node-list? node)
	(xml:map-union (xml:descendant test-pred?) node)
	(do ((res '() (if (test-pred? (ncar more)) (cons (ncar more) res) res))
	     (more ((xml:child node?) node)
		   (nappend ((xml:child node?) (ncar more)) (ncdr more))))
	    ((zero? (node-list-length more))
	     (list->node-list (reverse! res)))))))

;; descendant-or-self
(define (xml:descendant-or-self test-pred?)
  (lambda (node)
    (if (node-list? node)
	(xml:map-union (xml:descendant-or-self test-pred?) node)
	(do ((res '() (if (test-pred? (ncar more)) (cons (ncar more) res) res))
	     (more (list->node-list (list node))
		   (nappend ((xml:child node?) (ncar more)) (ncdr more))))
	    ((zero? (node-list-length more))
	     (list->node-list (reverse! res)))))))
 
;; ancestor
(define (xml:ancestor pred?)
  (lambda (node)
    (if (node-list? node)
	(xml:map-union (xml:ancestor pred?) node)
	(do ((res '() (if (pred? parent) (cons parent res) res))
	     (parent (node-parent-node node) (node-parent-node parent)))
	    ((not parent) (list->node-list res))))))

;; ancestor-or-self
(define (xml:ancestor-or-self pred?)
  (lambda (node)
    (if (node-list? node)
	(xml:map-union (xml:ancestor-or-self pred?) node)
	(do ((res '() (if (pred? parent) (cons parent res) res))
	     (parent node (node-parent-node parent)))
	    ((not parent) (list->node-list res))))))

(define (xml:map-union proc node-list)
  (define len (node-list-length node-list))
  (define (push-all proc-res queue)
    (do ((len (node-list-length proc-res)) (i 0 (+ i 1)))
	((= i len))
      (list-queue-add-back! queue (node-list:item proc-res i))))
  (do ((i 0 (+ i 1)) (queue (list-queue)))
      ((= i len) (make-node-list queue))
    (let ((proc-res (proc (node-list:item node-list i))))
      (cond ((node-list? proc-res) (push-all proc-res queue))
	    ((node? proc-res) (list-queue-add-back! queue proc-res))))))

(define (xml:child test-pred?)
  (lambda (node)
    (cond ((node? node) ((xml:filter test-pred?) (node-child-nodes node)))
	  ((node-list? node) (xml:map-union (xml:child test-pred?) node))
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


(define (xml:attribute pred)
  (define filter (xml:filter pred))
  (lambda (node)
    (xml:map-union
     (lambda (node)
       (if (element? node)
	   (filter (named-node-map->node-list (element-attributes node)))
	   (list->node-list '())))
     (if (node-list? node)
	 node
	 (list->node-list (list node))))))

;;; conversion
;; https://www.w3.org/TR/xpath-functions-31/
;; 7.3.1
;; fn:boolean($arg) equivalent
(define (xml:boolean node)
  (cond ((node-list? node)
	 (and (not (zero? (node-list-length node)))
	      (node? (node-list:item node 0))))
	((string? node) (> (string-length node) 0))
	((number? node) (not (= node 0)))
	((boolean? node) node)
	(else (xqt-error 'FORG0006 'xml:boolean "Invalid argument" node))))



(define (xml:ntype?? crit . maybe-ns-bindings)
  (define ns-bindings (if (null? maybe-ns-bindings) '() (car maybe-ns-bindings)))
  (define (ns-element=? namespace local-name)
    (lambda (node)
      (and (element? node)
	   (equal? namespace (element-namespace-uri node))
	   (equal? local-name (element-local-name node)))))
  (cond ((eq? crit '*) element?)
	;; local name 
	((string? crit)
	 (lambda (node)
	   (and (element? node)
		(not (element-namespace-uri node)) ;; no namespace
		(equal? crit (element-local-name node)))))
	((and (pair? crit) (eq? 'qname (car crit)))
	 ;; namespace with
	 (let ((ns (caddr crit))
	       (local-name (cadddr crit)))
	   (cond ((assoc ns ns-bindings) =>
		  (lambda (namespace) (ns-element=? (cdr namespace) local-name)))
		 (else
		  ;; unknown namespace
		  ;; TODO how to handle this?
		  (let ((node-name (string-append ns ":" local-name)))
		    (lambda (node)
		      (and (element? node)
			   (equal? (node-node-name node) node-name))))))))
	((and (pair? crit) (eq? 'eqname (car crit)))
	 (ns-element=? (cadr crit) (caddr crit)))
	(else
	 (assertion-violation 'xml:ntype?? "not supported yet" crit))))

)
