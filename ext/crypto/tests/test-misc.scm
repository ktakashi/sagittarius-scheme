(import (rnrs)
	(sagittarius crypto keys)
	(sagittarius crypto x509)
	(sagittarius crypto keystores)
	(sagittarius crypto signatures)
	(util duration)
	(srfi :19 time)
	(srfi :64))

(test-begin "Other tests")

(define subject-dn (x509-name '(C "NL")
			      '(ST "Zuid-Holland")
			      '(OU "Sagittarius Scheme")
			      '(CN "Takashi Kato")
			      '(E "ktakashi@ymail.com")))

;; Empty PKCS12 keystore load and write, easy to do it here :(
(let ((ks (make-pkcs12-keystore))
      (password "password"))
  (define (->template dn sn period public-key)
    (define now (current-time))
    (define p (duration:of-days period))
    (x509-certificate-template-builder
     (issuer-dn dn)
     (subject-dn dn)
     (serial-number sn)
     (not-before (time-utc->date now))
     (not-after (time-utc->date (add-duration now p)))
     (public-key public-key)
     (extensions
      (list
       (make-x509-key-usage-extension
	(x509-key-usages digital-signature
			 key-encipherment
			 key-agreement
			 decipher-only)
	#t)
       (make-x509-private-key-usage-period-extension
	(make-x509-private-key-usage-period
	 :not-before (time-utc->date now))
	#t)
       
       (make-x509-basic-constraints-extension
	(make-x509-basic-constraints :ca #f))))))
  (define kp (generate-key-pair *key:ecdsa*))
  (define cert
    (sign-x509-certificate-template
     (->template subject-dn 1000 1 (key-pair-public kp))
     *signature-algorithm:ecdsa-sha256* (key-pair-private kp)))
  
  (define (write-it ks)
    (let-values (((out e) (open-bytevector-output-port)))
      (write-pkcs12-keystore ks password out)
      (e)))
  (define (read-it in) (read-pkcs12-keystore password in))
  (let ((ks (read-it (open-bytevector-input-port (write-it ks)))))
    (pkcs12-keystore-private-key-set! ks "key" (key-pair-private kp)
				      password (list cert))
    (test-assert (write-it ks))))

(test-end)
