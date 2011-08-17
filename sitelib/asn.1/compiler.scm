;;; -*- Scheme -*-
;;;
;;; compiler.scm - ASN.1 tree compiler
;;;
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; For now we just implemented only the part which RKCS#1 v1.5 encode requires.
(library (asn.1 compiler)
    (export compile verify)
    (import (asn.1 types)
	    (rnrs)
	    (rnrs mutable-pairs)
	    (sagittarius)
	    (sagittarius control))

  ;; Tree must be verified.
  ;; This function processes given tree destructively.
  (define (compile tree)
    (define (compile-one tree ops name)
      (for-each (lambda (op)
		  (let1 type (asn.1-type-type op)
		    (if (symbol? type)
			#t ;; next
			(begin
			  (cond ((assoc type *base-types*)
				 => (lambda (btype)
				      (asn.1-type-type-set! op (cddr btype))
				      (unless (asn.1-type-tag op)
					(asn.1-type-tag-set! op (cadr btype)))))
				(else
				 (unless (assoc type tree)
				   (raise-asn.1-error 'compile
						      "Unknown type" type))
				 (let1 ref (compile-one tree (cdr (assoc type tree))
							(if (asn.1-type-name op)
							    (format "~a.~a" name (asn.1-type-name op))
							    name))
				   (unless (asn.1-type-tag op)
				     (asn.1-type-tag-set! op (asn.1-type-tag (car ref))))
				   (asn.1-type-type-set! op (asn.1-type-type (car ref)))
				   (asn.1-type-child-set! op (asn.1-type-child (car ref)))
				   (asn.1-type-loop-set! op (asn.1-type-loop (car ref))))))
			  (when (or (eq? (asn.1-type-type op) 'SET)
				    (eq? (asn.1-type-type op) 'SEQUENCE))
			    (tag-constructive! op))
			  (when (asn.1-type-child op)
			    ;; If we have children we are one of SET SEQUENCE CHOICE
			    (compile-one tree (asn.1-type-child op) (if (asn.1-type-name op)
									(format "~a.~a" name (asn.1-type-name op))
									name))
			    ;; If a CHOICE is given tag, then it must be EXPLICIT
			    (when (and (eq? (asn.1-type-type op) 'CHOICE)
				       (asn.1-type-tag op))
			      (tag-explicit! op)
			      (tag-constructive! op)
			      (asn.1-type-type-set! op 'SEQUENCE))
			    (cond ((positive? (length (asn.1-type-child op)))
				   ;; Here we need to flatten CHOICEs and check that SET and CHOICE
				   ;; do not contain duplicate tags
				   (when (eq? (asn.1-type-type op) 'SET)
				     ;; In case we do CER encoding we order the SET elements by thier tags
				     (let1 tags (map (lambda (c)
						       (cond ((positive? (bitwise-length (asn.1-type-tag c)))
							      (asn.1-type-tag c))
							     ((eq? (asn.1-type-type c) 'CHOICE)
							      (list-sort < (map (lambda (cc)
										  (asn.1-type-tag cc))
										(asn.1-type-child c))))
							     (else 0)))
						     (asn.1-type-child op))
				       ;; TODO
				       )))
				  (else
				   ;; A SET of one element can be treated the same as SEQUENCE
				   (when (asn.1-type-type op 'SET)
				     (asn.1-type-type-set! op 'SEQUENCE)))))))))
		ops)
      ops)
    ;; The tree should be valid enought to be able to
    ;; - resolve reference
    ;; - encode tags
    ;; - verify CHOICEs do not contain duplicate tags
    (for-each (lambda (leaf)
		(let ((name (car leaf))
		      (ops (cdr leaf)))
		  (compile-one tree ops name)))
	      tree)
    tree)

  ;; tree is alist.
  ;; This function processes tree destructively.
  (define (verify tree)
    ;; Well it parsed correctly, now we
    ;; - check reference exist
    ;; - flatten COMPONENTS OF (checking for loops)
    ;; - check for duplicate var names
    (for-each (lambda (leaf)
		(let ((name (car leaf))
		      (ops  (cdr leaf))
		      (path "")
		      (stash '())
		      (scope '()))
		  (let loop ((ops ops))
		    (cond ((null? ops) 
			   (unless (null? scope)
			     (receive (s p ops) (apply values scope)
			       (set! stash s)
			       (set! path p)
			       (loop ops))))
			  (else
			   (let1 op (car ops)
			     (cond ((asn.1-type-name op)
				    => (lambda (name)
					 (when (member name stash)
					   (raise-asn.1-error 'vefiry
							      (format "~a: ~a.~a used multiple times." name path (asn.1-type-name op))))
					 (set! stash (cons name stash)))))
			     (cond ((asn.1-type-child op)
				    => (lambda (child)
					 (cond ((pair? child)
						(append! scope (list stash path (cdr ops)))
						(when (asn.1-type-name op)
						  (set! stash '())
						  (set! path (string-append path (format ".~a" (asn.1-type-name op)))))
						(loop child))
					       ((string=? (asn.1-type-type op) "COMPONENTS")
						(set-car! ops (expand-operation tree child '())))
					       (else
						(raise-asn.1-error 'verify
								   "invalid child type" 
								   name child))))))
			     (loop (cdr ops))))))))
	      tree)
    tree)

  (define (expand-operation tree want seen)
    (when (member want seen)
      (raise-asn.1-error 'expand-operation
			 "COMPONENTS OF loop" want))
    (unless (assoc want tree)
      (raise-asn.1-error 'expand-operation "Undefined macro" want))
    (set! seen (cons want seen))
    (let1 ops (cdr (assoc want tree))
      (if (and (= (length ops) 1)
	       (or (string=? (asn.1-type-type (car ops)) "SEQUENCE")
		   (string=? (asn.1-type-type (car ops)) "SET"))
	       (pair? (asn.1-type-child (car ops))))
	  (let loop ((ops (asn.1-type-child (car ops))))
	    (unless (null? ops)
	      (cond ((string=? (asn.1-type-type (car ops)) "COMPONENTS")
		     (set-car! ops (expand-operation tree (asn.1-type-child (car ops)) seen))
		     (loop ops))
		    (else
		     (loop (cdr ops))))))
	  (raise-asn.1-error 'expand-operation
			     (format "Bad macro COMPONENTS OF ~a" want)))
      ops))
)

;; Local Variables:
;; coding: utf-8
;; End: