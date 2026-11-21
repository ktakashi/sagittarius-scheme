(import (rnrs)
        (clos user)
        (sagittarius)
        (sagittarius crypto keys)
        (sagittarius crypto signatures)
        (sagittarius crypto digests)
        (sagittarius crypto random)
        (sagittarius crypto logic pqc)
        (sagittarius crypto x509)
        (text json)
        (util bytevector)
        (util file)
        (srfi :13)
        (srfi :39)
        (srfi :64))

(define pqc-vector-directory
  (let-values (((dir base ext) (decompose-path (current-load-path))))
    (build-path* dir "testvectors" "pqc")))

(define (field object name . default)
  (cond ((assoc name object) => cdr)
        ((pair? default) (car default))
        (else (assertion-violation 'pqc-vectors "Missing vector field" name))))

(define (hex-field object name)
  (hex-string->bytevector (field object name)))

(define (read-vectors name)
  (parameterize ((*json-map-type* 'alist))
    (call-with-input-file (build-path pqc-vector-directory name) json-read)))

(define (fixed-prng bytes)
  (let ((position 0))
    (make <custom-random-generator>
      :set-seed! (lambda args
                   (assertion-violation 'fixed-prng "Unexpected reseeding"))
      :read-random!
      (lambda (prng output start length)
        (unless (<= (+ position length) (bytevector-length bytes))
          (assertion-violation 'fixed-prng "Unexpected entropy consumption"))
        (bytevector-copy! bytes position output start length)
        (set! position (+ position length))
        length))))

(define (parameter-name name)
  (string->keyword (string-downcase name)))

(define (slh-hash-name name)
  (let ((sha2? (string-contains name "SHA2"))
        (small? (string-contains name "128")))
    (if sha2?
        (if small? "sha256" "sha512")
        (if small? "shake128" "shake256"))))

(define (group-scheme group)
  (let ((name (field group "parameterSet")))
    (if (and (string-contains name "SLH-DSA")
             (equal? (field group "preHash" #f) "preHash"))
        (parameter-name
          (string-append "hash-" name "-with-" (slh-hash-name name)))
        (parameter-name name))))

(define (vector-digest name)
  (cond ((assoc name
           `(("SHA2-224" . ,*digest:sha-224*)
             ("SHA2-256" . ,*digest:sha-256*)
             ("SHA2-384" . ,*digest:sha-384*)
             ("SHA2-512" . ,*digest:sha-512*)
             ("SHA2-512/224" . ,*digest:sha-512/224*)
             ("SHA2-512/256" . ,*digest:sha-512/256*)
             ("SHA3-224" . ,*digest:sha3-224*)
             ("SHA3-256" . ,*digest:sha3-256*)
             ("SHA3-384" . ,*digest:sha3-384*)
             ("SHA3-512" . ,*digest:sha3-512*)
             ("SHAKE-128" . ,*digest:shake-128*)
             ("SHAKE-256" . ,*digest:shake-256*))) => cdr)
        (else (assertion-violation 'vector-digest "Unknown hash" name))))

(define (signature-options family group test)
  (append (list :context (hex-field test "context"))
    (if (and (string=? family "ML-DSA")
             (equal? (field group "preHash") "preHash"))
        (list :digest (vector-digest (field test "hashAlg")))
        '())))

(define (test-key-generation family scheme test)
  (let* ((seed (cond
                ((string=? family "ML-KEM")
                 (bytevector-append (hex-field test "d") (hex-field test "z")))
                ((string=? family "ML-DSA") (hex-field test "seed"))
                (else (bytevector-append (hex-field test "skSeed")
                                        (hex-field test "skPrf")
                                        (hex-field test "pkSeed")))))
         (pair (generate-key-pair scheme :prng (fixed-prng seed)))
         (private (key-pair-private pair)))
    (test-equal "NIST public key"
      (hex-field test (if (string=? family "ML-KEM") "ek" "pk"))
      (export-public-key (key-pair-public pair)))
    (test-equal "NIST expanded private key"
      (hex-field test (if (string=? family "ML-KEM") "dk" "sk"))
      (export-private-key private))
    (destroy-key! private)))

(define (test-kem-operation group scheme test)
  (case (string->symbol (field group "function"))
    ((encapsulation)
     (let ((public (import-public-key scheme (hex-field test "ek"))))
       (let-values (((ciphertext secret)
                     (encapsulate scheme public
                       :prng (fixed-prng (hex-field test "m")))))
         (test-equal "NIST ciphertext" (hex-field test "c") ciphertext)
         (test-equal "NIST encapsulated secret" (hex-field test "k") secret))))
    ((decapsulation)
     (let ((private (import-private-key scheme (hex-field test "dk"))))
       (test-equal (field test "reason") (hex-field test "k")
         (decapsulate scheme private (hex-field test "c")))
       (destroy-key! private)))
    ((encapsulationKeyCheck decapsulationKeyCheck)
     (let ((private? (string=? (field group "function") "decapsulationKeyCheck")))
       (test-equal (field test "reason") (field test "testPassed")
         (guard (e ((assertion-violation? e) #f))
           (let ((key ((if private? import-private-key import-public-key)
                       scheme (hex-field test (if private? "dk" "ek")))))
             (when private? (destroy-key! key))
             #t)))))))

(define (test-signature-generation family group scheme test)
  (let* ((private (import-private-key scheme (hex-field test "sk")))
         (public (import-public-key scheme (hex-field test "pk")))
         (options (signature-options family group test))
         (signer (apply make-signer scheme private
                   :deterministic? (field group "deterministic")
                   :prng (fixed-prng
                           (hex-string->bytevector
                             (field test "rnd"
                               (field test "additionalRandomness" ""))))
                   options))
         (signature (signer-sign-message signer (hex-field test "message"))))
    (test-equal "NIST signature length" (field test "signatureBytes")
      (bytevector-length signature))
    (test-equal "NIST signature SHA-256" (hex-field test "signatureSha256")
      (digest-message (make-message-digest *digest:sha-256*) signature))
    (test-assert "NIST signature verifies"
      (verifier-verify-signature (apply make-verifier scheme public options)
        (hex-field test "message") signature))
    (destroy-key! private)))

(define (test-signature-verification family group scheme test)
  (let ((public (import-public-key scheme (hex-field test "pk"))))
    (test-equal (field test "reason" "NIST verification")
      (field test "testPassed")
      (verifier-verify-signature
        (apply make-verifier scheme public (signature-options family group test))
        (hex-field test "message") (hex-field test "signature")))))

(define (test-openssl-vectors family)
  (vector-for-each
    (lambda (test)
      (let* ((name (field test "algorithm")) (scheme (parameter-name name)))
        (when (string-contains name family)
          (test-group name
            (if (string=? family "ML-DSA")
                (test-assert "OpenSSL signature"
                  (verifier-verify-signature
                    (make-verifier scheme
                      (import-public-key scheme (hex-field test "pk")))
                    (hex-field test "message") (hex-field test "signature")))
                (let* ((private (generate-private-key scheme
                                  (hex-field test "seed")))
                       (pair (generate-key-pair scheme
                               :prng (fixed-prng (hex-field test "seed")))))
                  (test-equal "OpenSSL public key"
                    (hex-field test "pk")
                    (export-public-key (key-pair-public pair)))
                  (when (string=? family "SLH-DSA")
                    (test-equal "OpenSSL private key" (hex-field test "sk")
                      (export-private-key private)))
                  (destroy-key! private)
                  (destroy-key! (key-pair-private pair))))))))
    (field (read-vectors "openssl.json") "tests")))

(define (run-pqc-vectors family)
  (test-begin (string-append family " conformance"))
  (for-each
    (lambda (mode)
      (let ((data (read-vectors (string-append family "-" mode ".json"))))
        (vector-for-each
          (lambda (group)
            (let ((scheme (group-scheme group)))
              (vector-for-each
                (lambda (test)
                  (test-group
                    (string-append mode "/" (number->string (field group "tgId"))
                                   "/" (number->string (field test "tcId")))
                    (cond
                      ((string=? mode "keyGen")
                       (test-key-generation family scheme test))
                      ((string=? mode "encapDecap")
                       (test-kem-operation group scheme test))
                      ((string=? mode "sigGen")
                       (test-signature-generation family group scheme test))
                      (else
                       (test-signature-verification family group scheme test)))))
                (field group "tests"))))
          (field data "testGroups"))))
    (if (string=? family "ML-KEM")
        '("keyGen" "encapDecap")
        '("keyGen" "sigGen" "sigVer")))
  (test-openssl-vectors family)
  (test-end))

(define (test-openssl-pkix)
  (vector-for-each
    (lambda (test)
      (let ((scheme (parameter-name (field test "algorithm")))
            (der (hex-field test "der")))
        (test-group (string-append (field test "algorithm") "/" (field test "kind"))
          (case (string->symbol (field test "kind"))
            ((pk)
             (let ((key (import-public-key der 'subject-public-key-info)))
               (test-equal "OpenSSL SubjectPublicKeyInfo" der
                 (export-public-key key 'subject-public-key-info))
               (test-eq scheme (oid->key-operation (key->oid key)))))
            ((sk)
             (let ((key (import-private-key der 'private-key-info)))
               (test-equal "OpenSSL PrivateKeyInfo" der
                 (export-private-key key 'private-key-info
                   :representation
                   (if (slh-dsa-private-key? key) 'expanded 'both)))
               (test-eq scheme (oid->key-operation (key->oid key)))
               (destroy-key! key)))
            ((x509)
             (let ((cert (bytevector->x509-certificate der)))
               (test-assert "OpenSSL certificate self-signature"
                 (x509-certificate-signed-by? cert cert))))))))
    (field (read-vectors "openssl-pkix.json") "tests")))
