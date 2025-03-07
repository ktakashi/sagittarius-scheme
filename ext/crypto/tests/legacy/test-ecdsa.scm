(define-syntax test-ecdsa
  (syntax-rules ()
    ((_ param hash msg d Qx Qy k R S)
     (%test-ecdsa 'param param hash msg d Qx Qy k R S))))

;;; Self tests
(define (test-ecdsa-sign/verify name ec-param hash)
  (define kp (generate-key-pair ECDSA :ec-parameter ec-param))
  (define (sign msg der?)
    (let ((cipher (make-cipher ECDSA (keypair-private kp))))
      (cipher-signature cipher msg :hash hash :der-encode der?)))
  (define (verify msg S der?)
    (let ((cipher (make-cipher ECDSA (keypair-public kp))))
      (cipher-verify cipher msg S :hash hash :der-encode der?)))
  (define (check msg)
    (test-assert (format "~s self check DER" name)
		 (verify msg (sign msg #t) #t))
    (test-assert (format "~s self check (~a)" name msg)
		 (verify msg (sign msg #f) #f)))
  (check #vu8())
  (check #vu8(1 2 3 4 5))
  (check (string->utf8 "Sagittarius ECDSA")))

(define-syntax ecdsa-self-test
  (syntax-rules ()
    ((_ param ...)
     (begin
       (test-ecdsa-sign/verify 'param param SHA-224) ...))))
(ecdsa-self-test NIST-P-192
		 NIST-P-224
		 NIST-P-256
		 NIST-P-384
		 NIST-P-521
		 NIST-K-163
		 NIST-K-233
		 NIST-K-283
		 NIST-K-409
		 NIST-K-571
		 NIST-B-163
		 NIST-B-233
		 NIST-B-283
		 NIST-B-409
		 NIST-B-571
		 secp192k1
		 secp224k1
		 secp256k1
		 sect163r1
		 sect239k1
		 sect113r1)

(include "test-b-163.scm")
(include "test-b-233.scm")
(include "test-b-283.scm")
(include "test-b-409.scm")
(include "test-b-571.scm")
(include "test-k-163.scm")
(include "test-k-233.scm")
(include "test-k-283.scm")
(include "test-k-409.scm")
(include "test-k-571.scm")
(include "test-p-192.scm")
(include "test-p-224.scm")
(include "test-p-256.scm")
(include "test-p-384.scm")
(include "test-p-521.scm")
