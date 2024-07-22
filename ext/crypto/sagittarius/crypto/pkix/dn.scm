;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/dn.scm - X.509 DN
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
(library (sagittarius crypto pkix dn)
    (export x509-name? <x509-name> x509-name
	    x509-name->string
	    string->x509-name
	    list->x509-name

	    name->x509-name
	    x509-name->name)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius mop immutable)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (util bytevector))

;; subjectDN and issuerDN
(define-class <x509-name> (<immutable>) 
  ((name :init-keyword :name :reader x509-name->name)))
(define (x509-name? o) (is-a? o <x509-name>))
(define (x509-name->string (name x509-name?) :optional (style #f))
  (let ((style (or style default-style))
	(rdn* (name-rdn* (x509-name->name name))))
    (string-join (map style rdn*) ",")))

(define (string->x509-name (dn string?))
  (define (string-dn->list-components s)
    (define in (open-string-input-port s))
    (define (read-element in mark)
      (let loop ((r '()))
	(let ((c (get-char in)))
	  (cond ((eof-object? c)
		 (if (null? r)
		     c
		     (list->string (reverse! r))))
		((eqv? c #\\)
		 (let ((c1 (get-char in)))
		   (cond ((eof-object? c1) (list->string (reverse! r)))
			 (else (loop (cons c1 r))))))
		((eqv? c mark) (list->string (reverse! r)))
		(else (loop (cons c r)))))))
    (define (read-rdn in)
      (let* ((k (read-element in #\=))
	     (v (read-element in #\,)))	
	(cond ((and (eof-object? k) (eof-object? v)) k)
	      ((or (eof-object? k) (eof-object? v)) #f)
	      (else
	       (let* ((k1 (string->symbol k))
		      (n (cond ((assoc k1 *reverse-oid-map*) k1)
			       (else k))))
		 (list n v))))))

    (let loop ((r '()))
      (let ((kv (read-rdn in)))
	(cond ((eof-object? kv) (reverse! r))
	      ((not kv) #f) ;; invalid
	      (else (loop (cons kv r)))))))
  (cond ((string-dn->list-components dn) => list->x509-name)
	(else (assertion-violation 'string->x509-name "Invalid DN" dn))))

(define-method write-object ((o <x509-name>) p)
  (format p "#<509-name ~a>" (x509-name->string o)))
(define-method object-equal? ((a <x509-name>) (b <x509-name>))
  ;; FIXME this is very inefficient
  (equal? (asn1-encodable->asn1-object (x509-name->name a))
	  (asn1-encodable->asn1-object (x509-name->name b))))

(define *rfc1779-map*
  `((,*attribute:common-name*            . "CN")
    (,*attribute:locality-name*          . "L")
    (,*attribute:state-or-province-name* . "ST")
    (,*attribute:organization-name*      . "O")
    (,*attribute:organization-unit-name* . "OU")
    (,*attribute:country-name*           . "C")
    (,*attribute:street-address*         . "STREET")))
(define *rfc2253-map*
  `(,@*rfc1779-map*
    (,*attribute:domain-component*       . "DC")
    (,*attribute:userid*                 . "UID")))

(define *default-map*
  `(,@*rfc2253-map*
    (,*attribute:name*                   . "Name")
    (,*attribute:surname*                . "SURNAME")
    (,*attribute:given-name*             . "GIVENNAME")
    (,*attribute:initials*               . "INITIALS")
    (,*attribute:generation-qualifier*   . "GENERATION")
    (,*attribute:title*                  . "T")
    (,*attribute:dn-qualifier*           . "DN")
    (,*attribute:serial-number*          . "SERIALNUMBER")
    (,*attribute:pseudonym*              . "Pseudonym")
    (,*attribute:email-address*          . "E")))

(define *escape-chars* (string->char-set ",=+<>#;\"\\"))
;; TODO handle canonical
(define ((make-style style oid-map canonical?)
	 (rdn relative-distinguished-name?))
  (define (->keyword oid oid-map)
    (cond ((assoc oid oid-map) => (lambda (slot) (values #f (cdr slot))))
	  ((eq? style 'rfc1779)
	   (values #t (string-append "OID."
				     (der-object-identifier->oid-string oid))))
	  (else (values #t (der-object-identifier->oid-string oid)))))
  (define (escape s)
    (let-values (((out e) (open-string-output-port)))
      (string-for-each (lambda (c)
			 (when (char-set-contains? *escape-chars* c)
			   (put-char out #\\))
			 (put-char out c)) s)
      (e)))
  (define (ava->string ava)
    (let-values (((raw? key) (->keyword (single-attribute-type ava) oid-map)))
      (let ((value (single-attribute-value ava)))
	(if (or raw? (not (asn1-string? value)))
	    (string-append key "=#"
	     (bytevector->hex-string (asn1-encodable->bytevector value)))
	    (let ((s (escape (asn1-string->string value))))
	      (string-append key "=" s))))))
  (define (sort names) names) ;; TODO
  (let ((names (relative-distinguished-name-components rdn)))
    (cond ((null? names) "") ;; should never happen...
	  ((null? (cdr names)) (ava->string (car names)))
	  (else
	   (string-join (map ava->string (if canonical? (sort names) names))
			"+")))))
;; RFC1779 style not really compliant
(define rfc1779-style (make-style 'rfc1779 *rfc1779-map* #f))
(define rfc2253-style (make-style 'rfc2553 *rfc2253-map* #f))
(define rfc2253-canonical-style (make-style 'rfc2553 *rfc2253-map* #t))
(define default-style (make-style 'rfc2553 *default-map* #f))

(define (name->x509-name (name name?))
  (make <x509-name> :name name))

(define *reverse-oid-map*
  (map (lambda (s) (cons (string->symbol (cdr s)) (car s)))
       *default-map*))
;; We use UTF8String for DirectoryString, unless it's specified
;; NOTE X520Name = DirectoryString as well
(define *oid-ctr-map*
  `((,*attribute:dn-qualifier*         . ,string->der-printable-string)
    (,*attribute:country-name*         . ,string->der-printable-string)
    (,*attribute:serial-number*        . ,string->der-printable-string)
    (,*attribute:domain-component*     . ,string->der-ia5-string)
    (,*attribute:email-address*        . ,string->der-ia5-string)))
(define (list->x509-name l)
  (define (oid->der-ctr oid)
    (cond ((assoc oid *oid-ctr-map*) => cdr)
	  (else string->der-utf8-string)))
  (define (->rdn oid&value)
    (make <relative-distinguished-name>
      :elements (list (make <single-attribute>
		  :type (car oid&value) :value (cdr oid&value)))))
  (define (->oid&value s)
    (let ((oid (cond ((assq (car s) *reverse-oid-map*) => cdr)
		     (else (oid-string->der-object-identifier (car s))))))
    (cons oid ((oid->der-ctr oid) (cadr s)))))
  (make <x509-name>
    :name (make <rdn-sequence> :elements (map ->rdn (map ->oid&value l)))))
(define (x509-name . c) (list->x509-name c))

)
