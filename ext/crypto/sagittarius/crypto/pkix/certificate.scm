;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/certificate.scm - X.509 certificate
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
(library (sagittarius crypto pkix certificate)
    (export subject-public-key-info->public-key

	    x509-name? <x509-name>
	    x509-name->string
	    
	    x509-validity? <x509-validity>
	    x509-validity-not-before
	    x509-validity-not-after

	    x509-extension? <x509-extension>
	    x509-extension-id
	    x509-extension-critical?
	    x509-extension-value

	    x509-extensions? <x509-extensions>
	    x509-extensions->list
	    
	    x509-certificate? <x509-certificate>
	    bytevector->x509-certificate read-x509-certificate
	    x509-certificate->bytevector write-x509-certificate
	    x509-certificate-issuer-dn
	    x509-certificate-subject-dn
	    x509-certificate-public-key
	    x509-certificate-validity
	    x509-certificate-serial-number
	    x509-certificate-version
	    x509-certificate-signature
	    x509-certificate-signature-algorithm
	    x509-certificate-extensions)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto keys)
	    (sagittarius combinators)
	    (sagittarius mop allocation)
	    (sagittarius mop immutable)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (util bytevector))
;; useful utility :)
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))

;; subjectDN and issuerDN
(define-class <x509-name> (<immutable>) 
  ((name :init-keyword :name :reader x509-name-name)))
(define (x509-name? o) (is-a? o <x509-name>))
(define (x509-name->string (name x509-name?) :optional (style #f))
  (let ((style (or style default-style))
	(rdn* (name-rdn* (x509-name-name name))))
    (string-join (map style rdn*) ",")))
(define-method write-object ((o <x509-name>) p)
  (format p "#<509-name ~a>" (x509-name->string o)))

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

(define-class <x509-validity> (<immutable> <cached-allocation>)
  ((validity :init-keyword :validity)
   (not-before :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ validity-not-before (lambda (o) (slot-ref o 'validity)))
	       asn1-time->date)
    :reader x509-validity-not-before)
   (not-after :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ validity-not-after (lambda (o) (slot-ref o 'validity)))
	       asn1-time->date)
    :reader x509-validity-not-after)))
(define (x509-validity? o) (is-a? o <x509-validity>))
(define (validity->x509-validity (validity validity?))
  (make <x509-validity> :validity validity))

(define (x509-extension-source o) (slot-ref o 'extension))
(define-class <x509-extension> (<immutable> <cached-allocation>)
  ((extension :init-keyword :extension)
   (id :allocation :virtual :slot-ref (make-slot-ref
				       (.$ extension-id x509-extension-source)
				       der-object-identifier->oid-string)
       :reader x509-extension-id)
   (critical? :allocation :virtual :cached #t
	      :slot-ref (make-slot-ref
			 (.$ extension-critical x509-extension-source)
			 (lambda (o) (and o (der-boolean->boolean o))))
	      :reader x509-extension-critical?)
   (value :allocation :virtual :cached #t
	  :slot-ref (make-slot-ref
		     (.$ extension-value x509-extension-source)
		     der-octet-string->bytevector)
	  :reader x509-extension-value)))
(define (x509-extension? o) (is-a? o <x509-extension>))
(define (extension->x509-extension (e extension?))
  (make <x509-extension> :extension e))
(define (list->x509-extension-list l)
  (map extension->x509-extension l))
(define-method write-object ((o <x509-extension>) p)
  (format p "#<x509-extension id=~a critical=~a value=~a>"
	  (x509-extension-id o)
	  (x509-extension-critical? o)
	  (x509-extension-value o)))

(define (x509-extensions-extensions o) (slot-ref o 'extensions))
(define-class <x509-extensions> (<immutable> <cached-allocation>)
  ((extensions :init-keyword :extensions)
   (elements :allocation :virtual :cached #t
	     :slot-ref (make-slot-ref
			(.$ extensions->list x509-extensions-extensions)
			list->x509-extension-list)
	     :reader x509-extensions->list)))
(define (x509-extensions? o) (is-a? o <x509-extensions>))
(define (extensions->x509-extensions (extensions extensions?))
  (make <x509-extensions> :extensions extensions))

(define-method import-public-key ((key <subject-public-key-info>))
  (import-public-key (asn1-encodable->asn1-object key)
		     (public-key-format subject-public-key-info)))

(define (subject-public-key-info->public-key (spki subject-public-key-info?))
  (import-public-key spki))

(define (x509-certificate? o) (is-a? o <x509-certificate>))
(define (x509-certificate-c (o x509-certificate?)) (slot-ref o 'c))
(define tbs-cert (.$ certificate-c x509-certificate-c))
(define-class <x509-certificate> (<cached-allocation> <immutable>)
  ((c :init-keyword :c)
   (encoded :init-keyword :encoded :init-value #f :mutable #t
	    :reader x509-certificate-encoded
	    :writer x509-certificate-encoded-set!)
   (issuer-dn :allocation :virtual :cached #t
    :slot-ref (make-slot-ref 
	       (.$ tbs-certificate-issuer tbs-cert)
	       name->x509-name)
    :reader x509-certificate-issuer-dn)
   (subject-dn :allocation :virtual :cached #t
    :slot-ref (make-slot-ref 
	       (.$ tbs-certificate-subject tbs-cert)
	       name->x509-name)
    :reader x509-certificate-subject-dn)
   (public-key :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-subject-public-key-info tbs-cert)
	       subject-public-key-info->public-key)
    :reader x509-certificate-public-key)
   (validity :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-validity tbs-cert)
	       validity->x509-validity)
    :reader x509-certificate-validity)
   (serial-number :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-serial-number tbs-cert)
	       der-integer->integer)
    :reader x509-certificate-serial-number)
   (version :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-version tbs-cert)
	       (lambda (v) (+ (or (and v (der-integer->integer v)) 0) 1)))
    :reader x509-certificate-version)
   (signature :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ certificate-signature x509-certificate-c)
	       der-bit-string->bytevector)
    :reader x509-certificate-signature)
   (signature-algorithm :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ algorithm-identifier-algorithm certificate-algorithm x509-certificate-c)
	       der-object-identifier->oid-string)
    :reader x509-certificate-signature-algorithm)
   (extensions :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-extensions tbs-cert)
	       (lambda (e) (and e (extensions->x509-extensions e))))
    :reader x509-certificate-extensions)))

(define-method write-object ((o <x509-certificate>) p)
  (define signature-hex
    (bytevector->hex-string (x509-certificate-signature o)))
  (define extensions (x509-certificate-extensions o))
  (let-values (((out e) (open-string-output-port)))
    (format out "#<x509-certificate~%")
    (format out "  [0]          Version: ~a~%" (x509-certificate-version o))
    (format out "          SerialNumber: ~a~%" (x509-certificate-serial-number o))
    (format out "              IssuerDN: ~a~%" (x509-name->string (x509-certificate-issuer-dn o)))
    (format out "            Start Date: ~a~%" (x509-validity-not-before (x509-certificate-validity o)))
    (format out "            Final Date: ~a~%" (x509-validity-not-after (x509-certificate-validity o)))
    (format out "             SubjectDN: ~a~%" (x509-name->string (x509-certificate-subject-dn o)))
    (format out "            Public Key: ~a~%" (x509-certificate-public-key o))
    (format out "   Signature Algorithm: ~a~%" (x509-certificate-signature-algorithm o))
    (cond ((< (string-length signature-hex) 40)
	   (format out "             Signature: ~a~%" signature-hex))
	  (else
	   (format out "             Signature: ~a~%" (substring signature-hex 0 40))
	   (do ((i 40 (+ i 40)) (len (string-length signature-hex))) ((>= i len))
	     (if (< i (- len 40))
		 (format out "                        ~a~%" (substring signature-hex i (+ i 40)))
		 (format out "                        ~a~%" (substring signature-hex i len))))))
    (when extensions
      (format out "            Extensions: ~%")
      (for-each (lambda (e) (format out "              ~a~%" e))
		(x509-extensions->list extensions)))
    (display ">" out)
    (display (e) p)))

(define (bytevector->x509-certificate (bv bytevector?))
  (make <x509-certificate>
    :c (asn1-object->asn1-encodable <certificate>
				    (bytevector->asn1-object bv))
    :encoded bv))
(define (read-x509-certificate (port (and input-port? binary-port?)))
  (let ((obj (read-asn1-object port)))
    (make <x509-certificate>
      :c (asn1-object->asn1-encodable <certificate> obj))))

(define (x509-certificate->bytevector (x509-certificate x509-certificate?))
  (cond ((x509-certificate-encoded x509-certificate))
	(else
	 (let ((r (asn1-encodable->bytevector
		   (x509-certificate-c x509-certificate))))
	   (x509-certificate-encoded-set! x509-certificate r)
	   r))))

(define (write-x509-certificate (x509-certificate x509-certificate?)
				:optional (out (current-output-port)))
  (put-bytevector out (x509-certificate->bytevector x509-certificate)))
)
