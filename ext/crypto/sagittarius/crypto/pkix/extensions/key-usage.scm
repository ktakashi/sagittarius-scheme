;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/extensions/key-usage.scm - Key Usage extension
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
(library (sagittarius crypto pkix extensions key-usage)
    (export x509-key-usages x509-key-usages?
	    x509-key-usages->bits
	    bits->x509-key-usages

	    x509-private-key-usage-period? <x509-private-key-usage-period>
	    make-x509-private-key-usage-period
	    x509-private-key-usage-period-not-before
	    x509-private-key-usage-period-not-after

	    ;; For testing
	    x509-private-key-usage-period->private-key-usage-period
	    )
    (import (rnrs)
	    (core enums) ;; for enum-set?...
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius mop immutable)
	    (srfi :19 time))

;; KeyUsage ::= BIT STRING {
;;      digitalSignature        (0),
;;      nonRepudiation          (1), --  recent editions of X.509 have
;;                                   --  renamed this bit to
;;                                   --  contentCommitment
;;      keyEncipherment         (2),
;;      dataEncipherment        (3),
;;      keyAgreement            (4),
;;      keyCertSign             (5),
;;      cRLSign                 (6),
;;      encipherOnly            (7),
;;      decipherOnly            (8)
;;  }
(define *x509-key-usage-list*
  '(digital-signature
    non-repudiation    ;;
    content-commitment ;; = non-repudiation
    key-encipherment
    data-encipherment
    key-agreement
    crl-sign
    key-cert-sign
    encipher-only
    decipher-only
    ))
(define x509-key-usage-enumeration (make-enumeration *x509-key-usage-list*))
(define make-x509-key-usages (enum-set-constructor x509-key-usage-enumeration))

(define-syntax x509-key-usages
  (syntax-rules ()
    ((_ symbol ...)
     (make-x509-key-usages '(symbol ...)))))

(define *x509-key-usages* (enum-set-universe (x509-key-usages)))
(define (x509-key-usages? o)
  (and (enum-set? o)
       (enum-set-subset? o *x509-key-usages*)))

(define (key-usage->bit-position key-usage)
  (case key-usage
    ((digital-signature)                  7)
    ((non-repudiation content-commitment) 6)
    ((key-encipherment)                   5)
    ((data-encipherment)                  4)
    ((key-agreement)                      3)
    ((crl-sign)                           2)
    ((key-cert-sign)                      1)
    ((encipher-only)                      0)
    ;; Where does this 15 come from?
    ((decipher-only)                      15)))

(define (key-usage->bit key-usage)
  (bitwise-arithmetic-shift 1 (key-usage->bit-position key-usage)))

(define (x509-key-usages->bits (key-usages x509-key-usages?))
  (define usages (enum-set->list key-usages))
  (when (null? usages)
    (assertion-violation 'x509-key-usages->bits
			 "At least one usage must be set" key-usages))
  (let loop ((usages usages) (r 0))
    (if (null? usages)
	r
	(loop (cdr usages)
	      (bitwise-ior r (key-usage->bit (car usages)))))))

(define (bits->x509-key-usages bits)
  (let loop ((usages (enum-set->list *x509-key-usages*)) (r '()))
    (cond ((null? usages) (make-x509-key-usages r))
	  ((bitwise-bit-set? bits (key-usage->bit-position (car usages)))
	   (loop (cdr usages) (cons (car usages) r)))
	  (else (loop (cdr usages) r)))))

;; Private key usage period
(define-class <x509-private-key-usage-period> (<immutable>)
  ((not-before :init-keyword :not-before
	       :reader x509-private-key-usage-period-not-before)
   (not-after :init-keyword :not-after
	      :reader x509-private-key-usage-period-not-after)))
(define (x509-private-key-usage-period? o)
  (is-a? o <x509-private-key-usage-period>))
(define (make-x509-private-key-usage-period
	 :key ((not-before (or #f date?)) #f) ((not-after (or #f date?)) #f))
  (unless (or not-before not-after)
    (assertion-violation 'make-x509-private-key-usage-period
			 "Either not-before or not-after must be present"))
  (make <x509-private-key-usage-period>
    :not-before not-before :not-after not-after))
(define (x509-private-key-usage-period->private-key-usage-period
	 (private-key-usage-period x509-private-key-usage-period?))
  (let ((not-before
	 (x509-private-key-usage-period-not-before private-key-usage-period))
	(not-after
	 (x509-private-key-usage-period-not-after private-key-usage-period)))
    (make <private-key-usage-period>
      :not-before (and not-before (date->der-generalized-time not-before))
      :not-after (and not-after (date->der-generalized-time not-after)))))
)
    
