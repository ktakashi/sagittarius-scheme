;;; -*- mode:scheme; coding:utf-8; -*-
;;; Shared internal PQC key handling.
;;;
;;; Copyright (c) 2026 Sagittarius contributors.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 1. Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;;    this list of conditions and the following disclaimer in the documentation
;;;    and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.
#!nounbound
(library (sagittarius crypto keys operations asymmetric pqc)
    (export define-pqc-parameter
            <pqc-key> <pqc-public-key> <pqc-private-key>
            pqc-key-parameter pqc-private-key-public-key
            pqc-private-key-bytes pqc-public-key-bytes
            pqc-parameter-identifier pqc-parameter-family pqc-parameter-number
            pqc-parameter-oid pqc-parameter-randomness-size
            pqc-check-parameter pqc-call-with-randomness)
    (import (rnrs) (clos user) (sagittarius)
            (sagittarius crypto asn1)
            (sagittarius crypto keys types)
            (sagittarius crypto keys operations asymmetric apis)
            (sagittarius crypto logic pqc)
            (sagittarius crypto random)
            (sagittarius crypto secure))

(define-record-type pqc-parameter
  (fields identifier family number oid public-class private-class
          public-size private-size output-size seed-size randomness-size))

(define (parameter identifier family number oid public-class private-class)
  (let-values (((pk sk output seed randomness) (pqc-sizes family number)))
    (make-pqc-parameter identifier family number oid public-class private-class
                        pk sk output seed randomness)))

(define-class <pqc-key> ()
  ((parameter :init-keyword :parameter :reader pqc-key-parameter)))
(define-class <pqc-public-key> (<public-key> <pqc-key>)
  ((bytes :init-keyword :bytes)))
(define-class <pqc-private-key> (<private-key> <pqc-key> <destroyable>)
  ((bytes :init-keyword :bytes)
   (seed :init-keyword :seed :init-value #f)
   (public-key :init-keyword :public-key :reader pqc-private-key-public-key)))

(define-method write-object ((key <pqc-key>) port)
  (format port "#<~a ~a>"
          (if (private-key? key) 'pqc-private-key 'pqc-public-key)
          (pqc-parameter-identifier (pqc-key-parameter key))))

(define (pqc-public-key-bytes key) (slot-ref key 'bytes))
(define (pqc-private-key-bytes key)
  (when (slot-ref key 'destroyed?)
    (assertion-violation 'pqc-private-key "Key has been destroyed"))
  (slot-ref key 'bytes))

(define-method destroy-destroyable! ((key <pqc-private-key>))
  (unless (slot-ref key 'destroyed?)
    (pqc-clear! (slot-ref key 'bytes))
    (when (slot-ref key 'seed) (pqc-clear! (slot-ref key 'seed)))
    (slot-set! key 'bytes #f)
    (slot-set! key 'seed #f)
    (slot-set! key 'destroyed? #t)))

(define (pqc-check-parameter p key)
  (unless (eq? p (pqc-key-parameter key))
    (assertion-violation 'pqc-key "Key parameter set does not match algorithm")))

(define (pqc-call-with-randomness prng size proc)
  (let ((bytes (pqc-protect! (make-bytevector size 0))))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (random-generator-read-random-bytes! prng bytes)
        (proc bytes))
      (lambda () (pqc-clear! bytes)))))

(define (make-public p bytes)
  (make (pqc-parameter-public-class p) :parameter p :bytes bytes))
(define (make-private p bytes seed public)
  (make (pqc-parameter-private-class p) :parameter p
        :bytes bytes :seed seed :public-key public))

(define (public-from-bytes p bytes)
  (make-public p (pqc-import (pqc-parameter-family p)
                             (pqc-parameter-number p) bytes #f)))

(define (private-from-seed p seed)
  (unless (= (bytevector-length seed) (pqc-parameter-seed-size p))
    (assertion-violation 'generate-private-key "Invalid PQC seed length"))
  (let ((copy (pqc-protect! (bytevector-copy seed))))
    (guard (e (else (pqc-clear! copy) (raise e)))
      (let-values (((pk sk) (pqc-expand (pqc-parameter-family p)
                                       (pqc-parameter-number p) copy)))
        (when (= (pqc-parameter-family p) 2) (pqc-clear! copy))
        (make-private p sk
                      (and (not (= (pqc-parameter-family p) 2)) copy)
                      (make-public p pk))))))

(define (private-from-expanded p bytes)
  (let ((copy (pqc-protect! (bytevector-copy bytes))))
    (guard (e (else (pqc-clear! copy) (raise e)))
      (let ((pk (pqc-import (pqc-parameter-family p)
                            (pqc-parameter-number p) copy #t)))
        (make-private p copy #f (make-public p pk))))))

(define (private-from-bytes p bytes)
  (if (= (bytevector-length bytes) (pqc-parameter-seed-size p))
      (private-from-seed p bytes)
      (private-from-expanded p bytes)))

(define-method key->oid ((key <pqc-key>))
  (pqc-parameter-oid (pqc-key-parameter key)))

(define-method export-public-key ((key <pqc-public-key>)
                                  :optional (format 'raw))
  (case format
    ((raw) (bytevector-copy (pqc-public-key-bytes key)))
    ((subject-public-key-info)
     (asn1-encodable->bytevector
       (der-sequence
         (der-sequence (oid-string->der-object-identifier (key->oid key)))
         (bytevector->der-bit-string (pqc-public-key-bytes key)))))
    (else (assertion-violation 'export-public-key
                               "Unknown public key format" format))))

(define-method export-private-key ((key <pqc-private-key>)
                                   :optional (format 'raw))
  (pqc-private-key-bytes key)
  (case format
    ((raw expanded) (bytevector-copy (pqc-private-key-bytes key)))
    ((seed)
     (let ((seed (slot-ref key 'seed)))
       (unless seed
         (assertion-violation 'export-private-key "Key has no retained seed"))
       (bytevector-copy seed)))
    ((private-key-info) (export-pkix-private-key key))
    (else (assertion-violation 'export-private-key
                               "Unknown private key format" format))))

(define (encode-private-value key representation)
  (define p (pqc-key-parameter key))
  (define bytes (pqc-private-key-bytes key))
  (define seed (slot-ref key 'seed))
  (if (= (pqc-parameter-family p) 2)
      (begin
        (unless (memq representation '(auto expanded))
          (assertion-violation 'export-private-key
                               "SLH-DSA uses expanded private keys"))
        bytes)
      (asn1-encodable->bytevector
        (case representation
          ((auto)
           (if seed
               (make <der-tagged-object> :explicit? #f :tag-no 0
                     :obj (bytevector->der-octet-string seed))
               (bytevector->der-octet-string bytes)))
          ((seed)
           (unless seed
             (assertion-violation 'export-private-key "Key has no retained seed"))
           (make <der-tagged-object> :explicit? #f :tag-no 0
                 :obj (bytevector->der-octet-string seed)))
          ((expanded) (bytevector->der-octet-string bytes))
          ((both)
           (unless seed
             (assertion-violation 'export-private-key "Key has no retained seed"))
           (der-sequence (bytevector->der-octet-string seed)
                         (bytevector->der-octet-string bytes)))
          (else (assertion-violation 'export-private-key
                                     "Unknown private key representation"
                                     representation))))))

(define-method export-pkix-private-key ((key <pqc-private-key>)
                                       :key (representation 'auto)
                                            (public-key? #f))
  (export-pqc-private-key-info key representation public-key?))

(define (export-pqc-private-key-info key representation public?)
  (let ((encoded (encode-private-value key representation)))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (let ((parts
               (list (integer->der-integer (if public? 1 0))
                     (der-sequence
                       (oid-string->der-object-identifier (key->oid key)))
                     (bytevector->der-octet-string encoded))))
          (asn1-encodable->bytevector
            (apply der-sequence
                   (if public?
                       (append parts
                         (list (make <der-tagged-object> :explicit? #f :tag-no 1
                               :obj (bytevector->der-bit-string
                                      (pqc-public-key-bytes
                                        (pqc-private-key-public-key key))))))
                       parts)))))
      (lambda ()
        (unless (eq? encoded (pqc-private-key-bytes key))
          (pqc-clear! encoded))))))

(define (read-key-object input)
  (cond ((bytevector? input)
         (let* ((port (open-bytevector-input-port input))
                (object (read-asn1-object port)))
           (unless (eof-object? (get-u8 port))
             (assertion-violation 'import-key "Trailing data in PQC key"))
           (unless (equal? input (asn1-encodable->bytevector object))
             (assertion-violation 'import-key "PQC keys require DER encoding"))
           object))
        ((port? input) (read-asn1-object input))
        (else input)))

(define (check-algorithm p aid)
  (unless (and (der-sequence? aid) (= (asn1-collection-length aid) 1)
               (der-object-identifier? (asn1-collection-ref aid 0))
               (string=? (pqc-parameter-oid p)
                 (der-object-identifier->oid-string
                   (asn1-collection-ref aid 0))))
    (assertion-violation 'import-key
                         "PQC algorithm mismatch or forbidden parameters")))

(define (import-pqc-public-key-info p input)
  (let ((seq (read-key-object input)))
    (unless (and (der-sequence? seq) (= (asn1-collection-length seq) 2))
      (assertion-violation 'import-public-key "Invalid SubjectPublicKeyInfo"))
    (check-algorithm p (asn1-collection-ref seq 0))
    (let ((bits (asn1-collection-ref seq 1)))
      (unless (and (der-bit-string? bits)
                   (zero? (der-bit-string-padding-bits bits)))
        (assertion-violation 'import-public-key "Invalid public key BIT STRING"))
      (public-from-bytes p (der-bit-string->bytevector bits)))))

(define (decode-private-value p encoded)
  (if (= (pqc-parameter-family p) 2)
      (private-from-expanded p encoded)
      (let ((value (read-key-object encoded)))
        (cond
          ((and (der-tagged-object? value)
                (= (der-tagged-object-tag-no value) 0)
                (not (der-tagged-object-explicit? value))
                (der-octet-string? (der-tagged-object-obj value)))
           (private-from-seed p
             (der-octet-string->bytevector (der-tagged-object-obj value))))
          ((der-octet-string? value)
           (private-from-expanded p (der-octet-string->bytevector value)))
          ((and (der-sequence? value) (= (asn1-collection-length value) 2)
                (der-octet-string? (asn1-collection-ref value 0))
                (der-octet-string? (asn1-collection-ref value 1)))
           (let ((key (private-from-seed p
                        (der-octet-string->bytevector
                          (asn1-collection-ref value 0)))))
             (unless (safe-bytevector=? (pqc-private-key-bytes key)
                        (der-octet-string->bytevector
                          (asn1-collection-ref value 1)))
               (destroy-key! key)
               (assertion-violation 'import-private-key
                                    "Inconsistent PQC seed and expanded key"))
             key))
          (else (assertion-violation 'import-private-key
                                     "Invalid PQC private key CHOICE"))))))

(define (valid-attributes? tagged)
  (define (attribute? value)
    (and (der-sequence? value) (= (asn1-collection-length value) 2)
         (der-object-identifier? (asn1-collection-ref value 0))
         (der-set? (asn1-collection-ref value 1))))
  (let ((value (der-tagged-object-obj tagged)))
    (if (der-tagged-object-explicit? tagged)
        (or (not value) (attribute? value))
        (and (der-sequence? value)
             (for-all attribute? (asn1-collection->list value))))))

(define (import-pqc-private-key-info p input)
  (let ((seq (read-key-object input)))
    (unless (and (der-sequence? seq) (>= (asn1-collection-length seq) 3)
                 (der-integer? (asn1-collection-ref seq 0))
                 (der-octet-string? (asn1-collection-ref seq 2)))
      (assertion-violation 'import-private-key "Invalid PrivateKeyInfo"))
    (check-algorithm p (asn1-collection-ref seq 1))
    (let ((version (der-integer->integer (asn1-collection-ref seq 0)))
          (key (decode-private-value p
                 (der-octet-string->bytevector (asn1-collection-ref seq 2)))))
      (guard (e (else (destroy-key! key) (raise e)))
        (let loop ((extras (cdddr (asn1-collection->list seq)))
                   (attributes? #f) (public? #f))
          (if (null? extras)
              (begin
                (unless (= version (if public? 1 0))
                  (assertion-violation 'import-private-key
                                       "Invalid OneAsymmetricKey version"))
                key)
              (let ((extra (car extras)))
                (unless (der-tagged-object? extra)
                  (assertion-violation 'import-private-key
                                       "Invalid OneAsymmetricKey field"))
                (case (der-tagged-object-tag-no extra)
                  ((0)
                   (when (or attributes? public? (not (valid-attributes? extra)))
                     (assertion-violation 'import-private-key
                                          "Invalid attributes field order"))
                   (loop (cdr extras) #t public?))
                  ((1)
                   (when (or public? (der-tagged-object-explicit? extra))
                     (assertion-violation 'import-private-key
                                          "Invalid public key field"))
                   ;; Implicit primitive tags are read as OCTET STRING values.
                   (let ((bits (der-tagged-object-obj extra)))
                     (unless (and (der-octet-string? bits)
                                  (let ((bv (der-octet-string->bytevector bits)))
                                    (and (> (bytevector-length bv) 0)
                                         (zero? (bytevector-u8-ref bv 0))
                                         (equal? (bytevector-copy bv 1)
                                           (pqc-public-key-bytes
                                             (pqc-private-key-public-key key))))))
                       (assertion-violation 'import-private-key
                                            "Inconsistent public key")))
                   (loop (cdr extras) attributes? #t))
                  (else (assertion-violation 'import-private-key
                                             "Unknown OneAsymmetricKey field"))))))))))

(define-syntax define-pqc-parameter
  (syntax-rules ()
    ((_ name keyword descriptor family number oid public-class private-class)
     (begin
       (define name keyword)
       (define descriptor
         (parameter name family number oid public-class private-class))
       (define-method oid->key-operation ((o (equal oid))) name)
       (define-method generate-key-pair ((m (eql name))
                                         :key
                                         (prng (secure-random-generator
                                                 *prng:chacha20* 32)))
         (pqc-call-with-randomness prng (pqc-parameter-seed-size descriptor)
           (lambda (seed)
             (let ((key (private-from-seed descriptor seed)))
               (make-key-pair key (pqc-private-key-public-key key))))))
       (define-method generate-public-key ((m (eql name)) (bytes <bytevector>))
         (public-from-bytes descriptor bytes))
       (define-method generate-private-key ((m (eql name)) (bytes <bytevector>))
         (private-from-bytes descriptor bytes))
       (define-method import-public-key ((m (eql name)) input
                                         :optional (format 'raw))
         (case format
           ((raw) (public-from-bytes descriptor
                    (if (port? input) (get-bytevector-all input) input)))
           ((subject-public-key-info)
            (import-pqc-public-key-info descriptor input))
           (else (assertion-violation 'import-public-key
                                      "Unknown public key format" format))))
       (define-method import-private-key ((m (eql name)) input
                                          :optional (format 'raw))
         (case format
           ((raw seed expanded)
            ((case format
               ((seed) private-from-seed)
               ((expanded) private-from-expanded)
               (else private-from-bytes))
             descriptor (if (port? input) (get-bytevector-all input) input)))
           ((private-key-info) (import-pqc-private-key-info descriptor input))
           (else (assertion-violation 'import-private-key
                                      "Unknown private key format" format))))
       (define-method export-public-key ((m (eql name))
                                         (key <pqc-public-key>) . opts)
         (pqc-check-parameter descriptor key)
         (apply export-public-key key opts))
       (define-method export-private-key ((m (eql name))
                                          (key <pqc-private-key>) . opts)
         (pqc-check-parameter descriptor key)
         (apply export-private-key key opts))
       (define-method export-private-key :around
           ((m (eql name)) (key <pqc-private-key>)
            (format (eq 'private-key-info)) . opts)
         (pqc-check-parameter descriptor key)
         (apply export-private-key key format opts))
       (define-method import-pkix-public-key ((m (eql name)) input
                                             :optional (original #f))
         (import-pqc-public-key-info descriptor (or original input)))
       (define-method import-pkix-private-key ((m (eql name)) input
                                              :optional (original #f))
         (import-pqc-private-key-info descriptor (or original input)))))))
)
