;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/extensions/constraints.scm - Constraints extension
;;;
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius crypto pkix extensions constraints)
    (export x509-basic-constraints? <x509-basic-constraints>
	    make-x509-basic-constraints
	    x509-basic-constraints-ca?
	    x509-basic-constraints-path-length-constraint

	    basic-constraints->x509-basic-constraints
	    x509-basic-constraints->basic-constraints 
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius mop immutable))

(define-class <x509-basic-constraints> (<immutable>)
  ((ca? :init-keyword :ca :init-value #f
	:reader x509-basic-constraints-ca?)
   (path-length-constraint :init-keyword :path-length-constraint :init-value #f
    :reader x509-basic-constraints-path-length-constraint)))
(define-method write-object ((o <x509-basic-constraints>) p)
  (cond ((x509-basic-constraints-path-length-constraint o) =>
	 (lambda (v)
	   (format p "#<x509-basic-constraints ca=~a path-length-constraint=~a>"
		   (x509-basic-constraints-ca? o) v)))
	(else
	 (format p "#<x509-basic-constraints ca=~a>"
		 (x509-basic-constraints-ca? o)))))
(define (x509-basic-constraints? o) (is-a? o <x509-basic-constraints>))
(define (make-x509-basic-constraints . keys)
  (apply make <x509-basic-constraints> keys))

(define (basic-constraints->x509-basic-constraints
	 (basic-constraints basic-constraints?))
  (define (maybe o conv) (and o (conv o)))
  (make <x509-basic-constraints>
    :ca (maybe (basic-constraints-ca basic-constraints) der-boolean->boolean)
    :path-length-constraint (maybe (basic-constraints-path-length-constraint basic-constraints) der-integer->integer)))

(define (x509-basic-constraints->basic-constraints
	 (basic-constraints x509-basic-constraints?))
  (make <basic-constraints>
    :ca (boolean->der-boolean (x509-basic-constraints-ca? basic-constraints))
    :path-length-constraint (cond ((x509-basic-constraints-path-length-constraint basic-constraints) => integer->der-integer)
				  (else #f))))

;; TODO add name constrains and policy constraints
)
