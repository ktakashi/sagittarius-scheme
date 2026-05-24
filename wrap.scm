(import (rnrs)
	(sagittarius crypto keys)
	(sagittarius crypto pem)
	(sagittarius crypto x509)
	(sagittarius crypto ciphers)
	(util bytevector))

(define pubkey
  (subject-public-key-info->public-key
   (pem-object->object
    (call-with-input-file "/Users/yo32es/Downloads/RSA-Key-Wrapper.pem"
      read-pem-object))))

(define rsa-cipher
  (asymmetric-cipher-init! (make-asymmetric-cipher *scheme:rsa*) pubkey))

(asymmetric-cipher-encrypt-bytevector rsa-cipher
				      (hex-string->bytevector 
(print pubkey)
