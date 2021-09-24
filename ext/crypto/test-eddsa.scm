(import (crypto eddsa))

(define (%test-eddsa type priv pub msg sig signer verifier :key (context #f))
  (define priv-key (generate-private-key type (integer->bytevector priv)))
  (define raw-pub (integer->bytevector pub))
  (define signature (integer->bytevector sig))
  (test-assert (eddsa-private-key? priv-key))
  
  (test-equal "EdDSA public key" raw-pub
	      (eddsa-public-key-data (eddsa-private-key-public-key priv-key)))
  (test-equal "EdDSA signature" signature (signer msg priv-key :context context))
  (let ((pub-key (generate-public-key type raw-pub)))
    (test-assert "EdDSA verify" (verifier msg signature pub-key :context context))))

(define (make-test-eddsa type signer verifier)
  (lambda (priv pub msg sig . args)
    (apply %test-eddsa type priv pub msg sig signer verifier args)))
(include "test-ed25519.scm")
(include "test-ed448.scm")
