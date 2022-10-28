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

	    x509-name? <x509-name> x509-name
	    x509-name->string
	    list->x509-name

	    x509-validity? <x509-validity>
	    x509-validity-not-before
	    x509-validity-not-after
	    
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
	    x509-certificate-extensions

	    validate-x509-certificate
	    x509-certificate-signature-validator
	    x509-certificate-validity-validator
	    
	    x509-certificate-template?
	    x509-certificate-template-builder
	    sign-x509-certificate-template
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix extensions)
	    (sagittarius crypto keys)
	    (sagittarius crypto signatures)
	    (sagittarius combinators)
	    (sagittarius mop allocation)
	    (sagittarius mop immutable)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :19 time)
	    (util bytevector)
	    (record builder))
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

;; Maybe we should move these to somewhere else...
(define-method import-public-key ((key <subject-public-key-info>))
  (import-public-key (asn1-encodable->asn1-object key)
		     (public-key-format subject-public-key-info)))

(define (subject-public-key-info->public-key (spki subject-public-key-info?))
  (import-public-key spki))
(define (public-key->subject-public-key-info (pk public-key?))
  (let ((bv (export-public-key pk (public-key-format subject-public-key-info))))
    (bytevector->asn1-encodable <subject-public-key-info> bv)))

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

(define (validate-x509-certificate (x509-certificate x509-certificate?)
				   . validators)
  (for-all (lambda (v) (v x509-certificate)) validators))
(define ((x509-certificate-signature-validator (public-key public-key?))
	 (x509-certificate x509-certificate?))
  (let ((verifier ((oid->verifier-maker
		    (x509-certificate-signature-algorithm x509-certificate))
		   public-key
		   :der-encode #f)))
    (unless (verifier-verify-signature verifier
	      (asn1-encodable->bytevector (tbs-cert x509-certificate))
	      (x509-certificate-signature x509-certificate))
      (assertion-violation 'x509-certificate-signature
			   "Invalid signature" x509-certificate))))
(define ((x509-certificate-validity-validator :optional (when (current-date)))
	 (x509-certificate x509-certificate?))
  (let* ((validity (x509-certificate-validity x509-certificate))
	 (utc (date->time-utc when))
	 (not-before (date->time-utc (x509-validity-not-before validity)))
	 (not-after (date->time-utc (x509-validity-not-after validity))))
    (unless (and (time<=? not-before utc) (time<=? utc not-after))
      (assertion-violation 'x509-certificate-validity
			   "Certificate is either expired or not in effect"
			   x509-certificate))))

(define-record-type x509-certificate-template
  (fields issuer-dn
	  subject-dn
	  serial-number
	  not-before
	  not-after
	  issuer-unique-id
	  subject-unique-id
	  extensions))
(define ((required who pred conv) o)
  (unless (pred o)
    (assertion-violation (list 'x509-certificate-template who)
			 "Invalid value"
			 o `(must satisfy ,pred)))
  (conv o))
(define ((optional who pred conv) o)
  (unless (or (not o) (pred o))
    (assertion-violation (list 'x509-certificate-template who)
			 "Invalid value"
			 o `(must satisfy ,pred)))
  (and o (conv o)))

(define check-issuer (required 'issuer-dn x509-name? x509-name-name))
(define check-subject (required 'subject-dn x509-name? x509-name-name))
(define check-serial-number
  (required 'serial-number integer? integer->der-integer))
(define check-not-before
  (required 'not-before date? date->der-generalized-time))
(define check-not-after
  (required 'not-after date? date->der-generalized-time))
(define check-issuer-unique-id
  (optional 'issuer-unique-id bytevector? bytevector->der-bit-string))
(define check-subject-unique-id
  (optional 'subject-unique-id bytevector? bytevector->der-bit-string))
(define check-extensions
  (optional 'extensions x509-extensions? x509-extensions->extensions))

(define-syntax x509-certificate-template-builder
  (make-record-builder x509-certificate-template
    ((issuer-dn #f check-issuer)
     (subject-dn #f check-subject)
     (serial-number #f check-serial-number)
     (not-before #f check-not-before)
     (not-after #f check-not-after)
     (issuer-unique-id #f check-issuer-unique-id)
     (subject-unique-id #f check-subject-unique-id)
     (extensions #f check-extensions))))

(define (x509-certificate-template->tbs-certificate template
						    signature-algorithm
						    public-key)
  (define (version template)
    (cond ((x509-certificate-template-extensions template)
	   (integer->der-integer 2)) ;; v3
	  ((or (x509-certificate-template-issuer-unique-id template)
	       (x509-certificate-template-subject-unique-id template))
	   (integer->der-integer 1)) ;; v2
	  (else #f)))		     ;; v1 (we don't set)
  (make <tbs-certificate>
    :version (version template)
    :serial-number (x509-certificate-template-serial-number template)
    :signature signature-algorithm
    :issuer (x509-certificate-template-issuer-dn template)
    :validity (make <validity>
		:not-before (x509-certificate-template-not-before template)
		:not-after (x509-certificate-template-not-after template))
    :subject-public-key-info (public-key->subject-public-key-info public-key)
    :subject (x509-certificate-template-subject-dn template)
    :issuer-unique-id (x509-certificate-template-issuer-unique-id template)
    :subject-unique-id (x509-certificate-template-subject-unique-id template)
    :extensions (x509-certificate-template-extensions template)))

(define (sign-x509-certificate-template (template x509-certificate-template?)
					oid
					(key-pair key-pair?))
  (define signer ((oid->signer-maker oid) (key-pair-private key-pair)
		  :der-encode #f))
  (let* ((algorithm (make <algorithm-identifier>
		      :algorithm (oid-string->der-object-identifier oid)))
	 (tbs-certificate (x509-certificate-template->tbs-certificate
			   template algorithm (key-pair-public key-pair)))
	 (signature (signer-sign-message signer
		     (asn1-encodable->bytevector tbs-certificate)))
	 (certificate (make <certificate>
			:c tbs-certificate
			:algorithm algorithm
			:signature (bytevector->der-bit-string signature))))
    (make <x509-certificate> :c certificate)))
)
