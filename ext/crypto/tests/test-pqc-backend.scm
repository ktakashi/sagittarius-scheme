(include "./pqc-vectors.scm")
(import (sagittarius crypto keys operations asymmetric pqc))

(define (test-backend family parameter expected)
  (let-values (((pklen sklen outlen seedlen rndlen) (pqc-sizes family parameter)))
    (test-equal "Normalized sizes" expected
      (list pklen sklen outlen seedlen rndlen))
    (let ((seed (make-bytevector seedlen parameter)))
      (let-values (((pk sk) (pqc-expand family parameter seed)))
        (test-equal pklen (bytevector-length pk))
        (test-equal sklen (bytevector-length sk))
        (test-equal "Public import" pk (pqc-import family parameter pk #f))
        (test-equal "Private import" pk (pqc-import family parameter sk #t))
        (test-error assertion-violation?
          (pqc-expand family parameter (bytevector-copy seed 1)))
        (test-error assertion-violation?
          (pqc-import family parameter (bytevector-copy pk 1) #f))
        (if (zero? family)
            (let-values (((ciphertext secret)
                          (pqc-encapsulate parameter pk (make-bytevector 32 7))))
              (test-equal outlen (bytevector-length ciphertext))
              (test-equal "Decapsulation" secret
                (pqc-decapsulate parameter sk ciphertext))
              (test-error assertion-violation?
                (pqc-encapsulate parameter pk (make-bytevector 31 0)))
              (pqc-clear! secret))
            (let* ((randomness (if (= family 1) (make-bytevector rndlen 0)
                                  (bytevector-copy pk 0 rndlen)))
                   (signature (pqc-sign family parameter sk #vu8() #vu8()
                                        randomness #f)))
              (test-equal outlen (bytevector-length signature))
              (test-assert "Native signature"
                (pqc-verify family parameter pk #vu8() #vu8() signature #f))
              (test-assert "Native rejection"
                (not (pqc-verify family parameter pk #vu8(1) #vu8() signature #f)))
              (test-error assertion-violation?
                (pqc-sign family parameter sk #vu8() #vu8() #vu8() #f))
              (pqc-clear! randomness)))
        (pqc-clear! sk)
        (test-equal "Secret clearing" (make-bytevector sklen 0) sk)))
    (guard (e ((assertion-violation? e)
               (test-equal "No secret error irritants" '() (condition-irritants e))))
      (pqc-import family parameter #vu8(1 2 3) #t)
      (test-assert "Invalid key raises" #f))))

(test-begin "PQC backend contract")
(for-each
  (lambda (entry) (apply test-backend entry))
  '((0 0 (800 1632 768 64 32))
    (0 1 (1184 2400 1088 64 32))
    (0 2 (1568 3168 1568 64 32))
    (1 0 (1312 2560 2420 32 32))
    (1 1 (1952 4032 3309 32 32))
    (1 2 (2592 4896 4627 32 32))))

(do ((parameter 0 (+ parameter 1))) ((= parameter 24))
  (let* ((base (mod parameter 12))
         (level (quotient (mod base 6) 2))
         (n (vector-ref '#(16 24 32) level))
         (out (vector-ref '#(7856 17088 16224 35664 29792 49856) (mod base 6))))
    (test-backend 2 parameter (list (* 2 n) (* 4 n) out (* 3 n) n))))

(for-each
  (lambda (parameter)
    (test-error assertion-violation? (pqc-sizes 0 parameter)))
  (list -1 3 (least-fixnum) (greatest-fixnum)))
(test-error assertion-violation? (pqc-sizes (greatest-fixnum) 0))
(test-error assertion-violation? (pqc-sizes -1 0))
(test-error assertion-violation? (pqc-sizes 0 (expt 2 32)))
(test-error assertion-violation? (pqc-sizes (expt 2 32) 0))
(test-error assertion-violation?
  (pqc-sign 0 0 #vu8() #vu8() #vu8() #vu8() #f))

(let* ((key (generate-private-key *key:ml-dsa-44* (make-bytevector 32 1)))
       (secret (pqc-private-key-bytes key))
       (seed (slot-ref key 'seed)))
  (destroy-key! key)
  (test-equal "Long-lived key clearing" (make-bytevector 2560 0) secret)
  (test-equal "Retained seed clearing" (make-bytevector 32 0) seed))

(test-end)
