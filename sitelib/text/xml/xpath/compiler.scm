;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/xpath/compiler.scm - XPath compiler
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

#!nounbound
(library  (text xml xpath compiler)
    (export xpath:compile
	    xpath:compile-string)
    (import (rnrs)
	    (text xml errors)
	    (text xml dom nodes)
	    (text xml xpath parser)
	    (text xml xpath tools)
	    (srfi :1 lists)
	    (srfi :127 lseqs)
	    (sagittarius generators))

(define (xpath:compile-string xpath-string . options)
  (apply xpath:compile (open-string-input-port xpath-string) options))

(define (xpath:compile in . maybe-ns-bindings)
  (define ns-binding (if (null? maybe-ns-bindings) '() (car maybe-ns-bindings)))
  (define (wrap evaluator)
    (lambda (dom)
      (let* ((context (evaluator
		       (make-xpath-context (list->node-list (list dom)) '())))
	     (result (xpath-context-targets context)))
	(cond ((node-list? result) result)
	      ((node? result) (list->node-list result))
	      (else #f)))))
  (let ((expr* (xpath:parse (generator->lseq (port->char-generator in)))))
    ;; TODO how should we handle multiple expression?
    ;;      for now, pipe
    (wrap 
     (fold-left (lambda (acc expr)
		   (let ((evaluator (xpath:compile1 expr ns-binding)))
		     (lambda (context)
		       (acc (evaluator context)))))
		 (lambda (context) context)
		 expr*))))

;; internal
(define-record-type xpath-context
  (fields targets ;; also the results
	  ;; for now alist
	  variables))

(define (xpath:compile1 expr ns-binding)
  (cond ((and (pair? expr) (pair? (car expr)))
	 (xpath:compile-path expr ns-binding))
	((and (pair? expr) (assq (car expr) axis-list)) =>
	 (lambda (slot) ((cadr slot) expr ns-binding)))
	((or (string? expr) (number? expr) (boolean? expr))
	 (xpath:compile-literal (list 'dummy expr) ns-binding))
	(else
	 (xqt-error 'unknown 'xpath:compile "not yet" expr))))

(define (xpath:compile-path expr ns-binding)
  (fold-left (lambda (acc segment)
	       (let ((evaluator (xpath:compile-path-segment segment ns-binding)))
		 (lambda (context)
		   (evaluator (acc context)))))
	     (lambda (context) context) expr))

(define descendant-nodes (xml:descendant-or-self node?))
(define (xpath:compile-path-segment segment ns-binding)
  (define (make-filter type pred)
    (define selector
      (cond ((or (string? pred)
		 ;; qname and eqname are handled by xml:ntype??
		 (and (pair? pred) (memq (car pred) '(qname eqname))))
	     (xml:child (xml:ntype?? pred ns-binding)))
	    ((and (pair? pred) (string? (car pred)) (pair? (cadr pred)))
	     (let ((predicate (xpath:compile-predicate (cadr pred) ns-binding))
		   (select (xml:child (xml:ntype?? (car pred) ns-binding))))
	       (lambda (node)
		 (let ((target (select node)))
		   (and target (predicate target) target)))))
	    ((and (pair? pred) (symbol? (car pred)))
	     (xpath:compile1 pred ns-binding))
	    (else (assertion-violation 'xpath:compile-path-segment
				       "Unknown path segment"  segment))))
    (case type
      ((//) (lambda (node) (selector (descendant-nodes node))))
      ((/) (lambda (node) (selector node)))
      (else (assertion-violation 'xpath:compile-path-segment "Invalid type"
				 segment))))
  (let ((filter (make-filter (car segment) (cadr segment))))
    (lambda (context)
      (define doms (xpath-context-targets context))
      (let ((r (xml:map-union filter doms)))
	(make-xpath-context r (xpath-context-variables context))))))

(define (xpath:compile-predicate pred ns-binding)
  ;; ugly...
  ;; we need to make compare procedure like value vs node
  (define (get-value obj)
    (cond ((not (node? obj)) obj)
	  ((attr? obj) (attr-value obj))
	  (else obj)))
  (case (car pred)
    ((??)
     (unless (pair? (cadr pred))
       (assertion-violation 'xpath:compile-predicate "Unknown predicate" pred))
     (let* ((condition (cadr pred))
	    (lhs (xpath:compile1 (cadr condition) ns-binding))
	    (rhs (xpath:compile1 (caddr condition) ns-binding)))
       (case (car condition)
	 ((=)
	  (lambda (node)
	    (let ((lvar (get-value (lhs node)))
		  (rvar (get-value (rhs node))))
	      (and (equal? lvar rvar) node))))
	 (else (assertion-violation 'xpath:compile-predicate "not yet" pred)))))
    (else (assertion-violation 'xpath:compile-predicate "not yet" pred))))

(define (xpath:compile-attribute expr ns-binding)
  (let ((name (cadr expr)))
    ;; TODO handle attribute namespace
    (lambda (node)
      (define (pred attr)
	(equal? (attr-name attr) name))
      (let ((attr ((xml:attribute pred) node)))
	(unless (<= (node-list-length attr) 1)
	  (assertion-violation '@ "More than one attribute have the same name"
				  name))
	(and (= (node-list-length attr) 1)
	     (node-list:item attr 0))))))

(define (xpath:compile-literal expr ns-binding)
  (lambda (node) (cadr expr)))


(define axis-list
  `((@    ,xpath:compile-attribute)
    (str  ,xpath:compile-literal)))

;;; helpers
(define (xml:equality-cmp bool-op number-op string-op)
  (lambda (obj1 obj2)
    (eq? obj1 obj2)))
(define xml:equal? (xml:equality-cmp eq? = string=?))
(define xml:not-equal?
  (xml:equality-cmp
   (lambda (b1 b2) (not (eq? b1 b2)))
   (lambda (n1 n2) (not (= n1 n2)))
   (lambda (s1 s2) (not (string=? s1 s2)))))

)
