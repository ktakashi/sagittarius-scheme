(include "./pqc-vectors.scm")
(import (sagittarius crypto asn1)
        (sagittarius crypto pkix signatures)
        (srfi :18))

(define (private-public key)
  (cond ((ml-kem-private-key? key) (ml-kem-private-key-public-key key))
        ((ml-dsa-private-key? key) (ml-dsa-private-key-public-key key))
        (else (slh-dsa-private-key-public-key key))))

(define (changed bytes)
  (let* ((copy (bytevector-copy bytes)) (last (- (bytevector-length copy) 1)))
    (bytevector-u8-set! copy last (bitwise-xor (bytevector-u8-ref copy last) 1))
    copy))

(define (test-serialization scheme seed)
  (let* ((key (generate-private-key scheme seed))
         (raw (export-private-key key))
         (public (private-public key))
         (public-raw (export-public-key public))
         (spki (export-public-key public 'subject-public-key-info))
         (oid (oid-string->der-object-identifier (key->oid key)))
         (aid (der-sequence oid))
         (pki (export-private-key key 'private-key-info))
         (value (asn1-collection-ref (bytevector->asn1-object pki) 2)))
    (define (encode-private algorithm payload . extras)
      (asn1-encodable->bytevector
        (apply der-sequence (integer->der-integer 0) algorithm payload extras)))
    (define (check-private bytes)
      (let ((imported (import-private-key bytes 'private-key-info)))
        (test-equal "PrivateKeyInfo raw value" raw (export-private-key imported))
        (test-equal "PrivateKeyInfo public value" public-raw
          (export-public-key (private-public imported)))
        (destroy-key! imported)))
    (test-group (keyword->string scheme)
      (test-eq scheme (oid->key-operation (key->oid key)))
      (test-equal "SPKI round trip" public-raw
        (export-public-key (import-public-key spki 'subject-public-key-info)))
      (test-equal "SPKI ASN.1 round trip" public-raw
        (export-public-key
          (import-public-key (bytevector->asn1-object spki)
                             'subject-public-key-info)))
      (let ((imported (import-private-key (bytevector->asn1-object pki)
                                         'private-key-info)))
        (test-equal "PrivateKeyInfo ASN.1 round trip" raw
          (export-private-key imported))
        (destroy-key! imported))
      (for-each
        (lambda (representation)
          (for-each
            (lambda (public?)
              (check-private
                (export-private-key key 'private-key-info
                  :representation representation :public-key? public?)))
            '(#f #t)))
        (if (slh-dsa-private-key? key) '(auto expanded) '(auto seed expanded both)))
      (test-error assertion-violation?
        (import-public-key scheme (bytevector-copy public-raw 1)))
      (test-error assertion-violation?
        (import-private-key scheme (bytevector-copy raw 1) 'expanded))
      (test-error "Trailing SPKI bytes" assertion-violation?
        (import-public-key (bytevector-append spki #vu8(0)) 'subject-public-key-info))
      (test-error "Trailing private bytes" assertion-violation?
        (import-private-key (bytevector-append pki #vu8(0)) 'private-key-info))
      (test-error "Forbidden SPKI NULL" assertion-violation?
        (import-public-key
          (asn1-encodable->bytevector
            (der-sequence (der-sequence oid (make-der-null))
                          (bytevector->der-bit-string public-raw)))
          'subject-public-key-info))
      (test-error "Forbidden private NULL" assertion-violation?
        (import-private-key
          (encode-private (der-sequence oid (make-der-null)) value)
          'private-key-info))
      (test-error "Unexpected private field" assertion-violation?
        (import-private-key (encode-private aid value (make-der-null))
                           'private-key-info))
      (test-error "Primitive attributes" assertion-violation?
        (import-private-key
          (encode-private aid value
            (make <der-tagged-object> :explicit? #f :tag-no 0
                  :obj (bytevector->der-octet-string #vu8())))
          'private-key-info))
      (unless (slh-dsa-private-key? key)
        (test-error "Seed/expanded consistency" assertion-violation?
          (import-private-key
            (encode-private aid
              (bytevector->der-octet-string
                (asn1-encodable->bytevector
                  (der-sequence (bytevector->der-octet-string seed)
                                (bytevector->der-octet-string (changed raw))))))
            'private-key-info)))
      (test-error "OneAsymmetricKey public consistency" assertion-violation?
        (import-private-key
          (asn1-encodable->bytevector
            (der-sequence (integer->der-integer 1) aid value
              (make <der-tagged-object> :explicit? #f :tag-no 1
                    :obj (bytevector->der-bit-string (changed public-raw)))))
          'private-key-info))
      (test-error "OneAsymmetricKey version" assertion-violation?
        (import-private-key
          (asn1-encodable->bytevector
            (der-sequence (integer->der-integer 1) aid value))
          'private-key-info))
      (let* ((input (bytevector-copy raw))
             (copy (import-private-key scheme input 'expanded))
             (output (export-private-key copy)))
        (bytevector-fill! input 0)
        (bytevector-fill! output 0)
        (test-equal "Secret input/output isolation" raw (export-private-key copy))
        (test-assert "Private printing has no secret bytevector"
          (< (string-length (format "~s" copy)) 128))
        (destroy-key! copy)
        (destroy-key! copy)
        (test-error "Destroyed key export" assertion-violation?
          (export-private-key copy)))
      (let ((copy (import-public-key scheme public-raw)))
        (bytevector-fill! public-raw 0)
        (test-equal "Public input isolation" spki
          (export-public-key copy 'subject-public-key-info))))
    (destroy-key! key)))

(define (test-signing-options scheme seed)
  (let* ((key (generate-private-key scheme seed))
         (other (generate-private-key scheme (changed seed)))
         (public (private-public key))
         (context (make-bytevector 255 17))
         (original-context (bytevector-copy context))
         (signer (make-signer scheme key :deterministic? #t :context context))
         (message (string->utf8 "PQC incremental signing"))
         (signature (signer-sign-message signer message))
         (verifier (make-verifier scheme public :context original-context)))
    (test-group (keyword->string scheme)
      (bytevector-fill! context 0)
      (test-equal "Context is copied" signature
        (signer-sign-message signer message))
      (signer-init! signer)
      (signer-process! signer message 0 3)
      (signer-process! signer message 3)
      (test-equal "Buffered signature" signature (signer-sign! signer))
      (test-assert (verifier-verify-signature verifier message signature))
      (test-assert "Wrong message"
        (not (verifier-verify-signature verifier (changed message) signature)))
      (test-assert "Wrong signature"
        (not (verifier-verify-signature verifier message (changed signature))))
      (test-assert "Wrong key"
        (not (verifier-verify-signature
               (make-verifier scheme (private-public other)
                 :context original-context) message signature)))
      (test-assert "Wrong context"
        (not (verifier-verify-signature
               (make-verifier scheme public) message signature)))
      (for-each
        (lambda (invalid)
          (test-assert "Malformed signature"
            (not (verifier-verify-signature verifier message invalid))))
        (list #vu8() (bytevector-copy signature 1)
              (bytevector-append signature #vu8(0))))
      (test-error assertion-violation?
        (make-signer scheme key :context (make-bytevector 256 0)))
      (test-error assertion-violation?
        (make-signer scheme key :digest *digest:sha-1*))
      (let* ((randomized (make-signer scheme key))
             (first (signer-sign-message randomized #vu8()))
             (second (signer-sign-message randomized #vu8())))
        (test-assert "Randomized signing is default" (not (equal? first second)))
        (test-assert "Empty message"
          (verifier-verify-signature (make-verifier scheme public) #vu8() first)))
      (let* ((oid (key->oid key))
             (oid-signer ((oid->signer-maker oid) key :deterministic? #t))
             (oid-verifier ((oid->verifier-maker oid) public)))
        (test-assert "Signature OID dispatch"
          (verifier-verify-signature oid-verifier message
            (signer-sign-message oid-signer message)))
        (test-error assertion-violation?
          (x509-algorithm-identifier->verifier
            (make-x509-algorithm-identifier oid (make-der-null)) public)))
      (destroy-key! key)
      (test-error "Destroyed signer key" assertion-violation?
        (signer-sign-message signer message)))
    (destroy-key! other)))

(test-begin "PQC public API and PKIX interoperability")
(test-assert (private-key-format? (private-key-format seed)))
(test-assert (private-key-format? (private-key-format expanded)))
(test-assert (private-key-format? (private-key-format private-key-info)))
(test-assert (not (private-key-format? 'subject-public-key-info)))

(test-openssl-pkix)

(for-each
  (lambda (family)
    (vector-for-each
      (lambda (group)
        (let* ((test (vector-ref (field group "tests") 0))
               (seed
                 (cond
                   ((string=? family "ML-KEM")
                    (bytevector-append (hex-field test "d") (hex-field test "z")))
                   ((string=? family "ML-DSA") (hex-field test "seed"))
                   (else
                    (bytevector-append (hex-field test "skSeed")
                                       (hex-field test "skPrf")
                                       (hex-field test "pkSeed"))))))
          (test-serialization (group-scheme group) seed)
          (when (string=? family "SLH-DSA")
            (test-serialization
              (group-scheme (cons '("preHash" . "preHash") group)) seed))))
      (field (read-vectors (string-append family "-keyGen.json")) "testGroups")))
  '("ML-KEM" "ML-DSA" "SLH-DSA"))

(test-signing-options *signature:ml-dsa-44* (make-bytevector 32 1))
(test-signing-options *signature:slh-dsa-sha2-128f* (make-bytevector 48 2))
(test-signing-options *signature:slh-dsa-shake-128f* (make-bytevector 48 3))

(let* ((key (generate-private-key *key:ml-dsa-44* (make-bytevector 32 4)))
       (public (private-public key))
       (signer (make-signer *signature:hash-ml-dsa-44-with-sha512* key
                 :deterministic? #t))
       (signature (signer-sign-message signer #vu8())))
  (test-assert "HashML-DSA OID"
    (verifier-verify-signature
      ((oid->verifier-maker *signature-algorithm:hash-ml-dsa-44-with-sha512*) public)
      #vu8() signature))
  (test-assert "Pure and prehash separation"
    (not (verifier-verify-signature
           (make-verifier *signature:ml-dsa-44* public) #vu8() signature)))
  (test-error assertion-violation?
    (make-verifier *signature:ml-dsa-65* public))
  (test-error assertion-violation?
    (make-signer *signature:hash-ml-dsa-44-with-sha512* key :digest *digest:sha-256*))
  (destroy-key! key))

(let* ((scheme *key:ml-kem-512*)
       (key (generate-private-key scheme (make-bytevector 64 1)))
       (other (generate-private-key scheme (make-bytevector 64 2))))
  (let-values (((ciphertext secret) (encapsulate scheme (private-public key))))
    (test-equal "ML-KEM shared secret length" 32 (bytevector-length secret))
    (test-equal secret (decapsulate scheme key ciphertext))
    (test-assert "Different key implicit rejection"
      (not (equal? secret (decapsulate scheme other ciphertext))))
    (test-equal "Implicit rejection returns one value" 1
      (length (call-with-values
                (lambda () (decapsulate scheme key (changed ciphertext))) list)))
    (test-error assertion-violation?
      (decapsulate scheme key (bytevector-copy ciphertext 1)))
    (test-error assertion-violation?
      (decapsulate *key:ml-kem-768* key ciphertext)))
  (test-error assertion-violation?
    (export-private-key *key:ml-kem-768* key 'private-key-info))
  (destroy-key! key)
  (destroy-key! other))

(let* ((key (generate-private-key *key:ml-dsa-44* (make-bytevector 32 5)))
       (der (export-private-key key 'private-key-info)))
  (do ((end 0 (+ end 1))) ((= end (bytevector-length der)))
    (test-error "Truncated PrivateKeyInfo"
      (import-private-key (bytevector-copy der 0 end) 'private-key-info)))
  (test-error "Indefinite length encoding"
    (import-private-key
      (bytevector-append #vu8(48 128) (bytevector-copy der 2) #vu8(0 0))
      'private-key-info))
  (test-error "Nonminimal DER length"
    (import-private-key
      (bytevector-append
        (u8-list->bytevector (list 48 129 (bytevector-u8-ref der 1)))
        (bytevector-copy der 2))
      'private-key-info))
  (destroy-key! key))

(let ((threads
        (map (lambda (n)
               (make-thread
                 (lambda ()
                   (let* ((key (generate-private-key *key:ml-dsa-44*
                                 (make-bytevector 32 n)))
                          (signature (signer-sign-message
                                       (make-signer *signature:ml-dsa-44* key
                                         :deterministic? #t) #vu8()))
                          (result (verifier-verify-signature
                                    (make-verifier *signature:ml-dsa-44*
                                      (private-public key)) #vu8() signature)))
                     (destroy-key! key)
                     result))))
             '(1 2 3 4))))
  (for-each thread-start! threads)
  (for-each (lambda (thread) (test-assert "Concurrent keys" (thread-join! thread)))
            threads))

(test-end)
