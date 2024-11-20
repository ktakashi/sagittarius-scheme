(import (rnrs)
	(sagittarius)
	(sagittarius crypto asn1)
	(sagittarius crypto pkcs keys)
	(sagittarius crypto pkcs pbes)
	(sagittarius crypto pkcs scrypt)
	(sagittarius crypto pkix algorithms)
	(sagittarius crypto keys)
	(rfc base64)
	(srfi :64))

;; PBES1 data
(define epki1
  (base64-decode-string
   (string-append
    "MIIBWTAbBgkqhkiG9w0BBQMwDgQINNizKJdRv+YCAggABIIBOAHoonfM3/K257To"
    "fv9ogJ3djFErkWdX8ZgWIausgfBe/ewtz7L9rVmF5IOwAmiaN51hdD9h5lVqhNQU"
    "mmpjN9h8xyKTd8CGRr8hNZ1XF+KIkRh8ZPoUwHKuOzncWhC9zIFvlkj/JAe/x+bf"
    "mA+KVtTV3jgeSxObtAUb4AWvJkc8IrjSoKbbbFveZaTn5X7TcIx0Hk/bHt/EPrVn"
    "JhjO0VCcTixYQtHvNtGBMwnVUpenFSFAEFVGlrPVEg1DDlmG4uEV6lVpFOFABMB8"
    "Y3HBEu7rPnVn76a291lEbMuGRIqbixTxjHezKLFl2vCJNqkcmbmH3Pg9qLHOZGsW"
    "KoOtnuCpQYjncimXtyNW/+3dKDa34MIC2ChFai5Rt8nN3G7jwNuY/c9nBsf5chAm"
    "Wqkv6GMymGxIc6ONRg==")
   :transcoder #f))

;; PBES2 data
(define epki2
  (base64-decode-string
   (string-append
    "MIIBnTBXBgkqhkiG9w0BBQ0wSjApBgkqhkiG9w0BBQwwHAQIEo24KPXTLowCAggA"
    "MAwGCCqGSIb3DQIJBQAwHQYJYIZIAWUDBAEqBBDR22zSFOd8AloY/qU4Q80qBIIB"
    "QFEErJSDuAtDUoRJfWoQaWrVz0VN+tTL5nQ4Bh/LIcChpW8Qh3d1nqTYbEDHYN6p"
    "BkqjKuEBGrcNs268ibLHJKRMbf/FyEz5a1yIq7A/kqz89JRwPiLGOaVoEsFRvwkC"
    "jO4FRooSSDuQEjfYbU4Hs2+D1L7Q43kCsE2i68RUY//gjS22qACwd9TC3HEBl9sE"
    "Ooushv/VwVEa4caZk9Z9aJD8TfYNDdUwsYOc1Ca3r/5dZZ2eShfNgMI1jqVfLR6B"
    "sWvLaXanYYEwcSNvlJpH5ttN6acsgQjIcKeriaJjFqz/mdsazEl3GXI4Mbz7b8UB"
    "deSVpGUjZOD2zHGAVWrndnehKg1jVEbpYkBctrDOkL2c24DUAEPh2fMY/AUQ7UKw"
    "Mcno8wTx0LpW5jRs1RbMWdT3YkL1booqA7Rc9C+EpdSP")
   :transcoder #f))

;; PBES2 + DES3
(define epki3
  (base64-decode-string
   (string-append
    "MIIBjDBOBgkqhkiG9w0BBQ0wQTApBgkqhkiG9w0BBQwwHAQI3iJZLsWvg4MCAggA"
    "MAwGCCqGSIb3DQIJBQAwFAYIKoZIhvcNAwcECIzn6eTqAPxiBIIBODisebV+aYlJ"
    "D72BMqhE7GEtJOXPyhah1Mxci6dMQDD2yg5R6GSTN0FRpICCyIfTcgla4pgTGTQi"
    "mMzY4ayx4GONvzcQRzABMiUH3PfD11XOvUflO2EMZkkvWIlUfjgBf2SC4fsHP4Qm"
    "2dxORnvvCd8BxxpKLwWXhmCDcpLActb5ug2TTCaLxxiSqZrs8NxQM3KfLBQGFARK"
    "fhtLUZ6WB6uUi9BvO4JUl66pgeWHe841ctczM8G3lxGzXDsSMs6Rd1tsUuIXt49Q"
    "6/Vf6DgrOKRwW0fdxpJySuSj+0x5efWkeq8Bn3nAgQOvsajKFXOgrwLdD6oB2A79"
    "h/OQfohSk9PPS3zX9iQ3zswlySoppVMqXcCFKngSwbg/t0ygvWtPW1SwUXTB0cyD"
    "UprPtDydck9skek3R246Xw==")
   :transcoder #f))

;; scrypt + AES256 CBC
(define epki4
  (base64-decode-string
   (string-append
    "MIHiME0GCSqGSIb3DQEFDTBAMB8GCSsGAQQB2kcECzASBAVNb3VzZQIDEAAAAgEI"
    "AgEBMB0GCWCGSAFlAwQBKgQQyYmguHMsOwzGMPoyObk/JgSBkJb47EWd5iAqJlyy"
    "+ni5ftd6gZgOPaLQClL7mEZc2KQay0VhjZm/7MbBUNbqOAXNM6OGebXxVp6sHUAL"
    "iBGY/Dls7B1TsWeGObE0sS1MXEpuREuloZjcsNVcNXWPlLdZtkSH6uwWzR0PyG/Z"
    "+ZXfNodZtd/voKlvLOw5B3opGIFaLkbtLZQwMiGtl42AS89lZg==")
   :transcoder #f))

(test-begin "PKCS#8")

(define password "test1234")
(define (test-pbe bv password)
  (test-assert (pkcs-encrypted-private-key-info?
		(bytevector->pkcs-encrypted-private-key-info bv)))
  (let ((epki (bytevector->pkcs-encrypted-private-key-info bv)))
    (test-assert (x509-algorithm-identifier?
		  (pkcs-encrypted-private-key-info-encryption-algorithm epki)))
    (test-assert (bytevector?
		  (pkcs-encrypted-private-key-info-encrypted-data epki)))
    (test-error (pkcs-encrypted-private-key-info->pkcs-one-asymmetric-key
		epki "this password is not correct"))
    (let ((pki (pkcs-encrypted-private-key-info->pkcs-one-asymmetric-key
		epki password)))
      (test-assert (pkcs-one-asymmetric-key? pki))
      (test-assert (private-key? (pkcs-one-asymmetric-key-private-key pki)))
      (test-assert (pkcs-encrypted-private-key-info?
		    (pkcs-one-asymmetric-key->pkcs-encrypted-private-key-info
		     pki
		     (pkcs-encrypted-private-key-info-encryption-algorithm epki)
		     password)))
      (let ((epki2 (pkcs-one-asymmetric-key->pkcs-encrypted-private-key-info
		    pki
		    (pkcs-encrypted-private-key-info-encryption-algorithm epki)
		    password)))
	(test-assert (pkcs-one-asymmetric-key?
		      (pkcs-encrypted-private-key-info->pkcs-one-asymmetric-key
		       epki2 password)))))))

(test-pbe epki1 password)
(test-pbe epki2 password)
(test-pbe epki3 password)
;; It's too heavy to do it on CI...
(unless (getenv "CI") (test-pbe epki4 "Rabbit"))

(define key-pair (generate-key-pair *key:ed25519*))
(define pk (key-pair-private key-pair))

(define (test-pkcs8 pk enc-alg)
  (define pki (private-key->pkcs-one-asymmetric-key pk))
  (test-assert (pkcs-one-asymmetric-key? pki))
  (let ((epki (pkcs-one-asymmetric-key->pkcs-encrypted-private-key-info
	       pki enc-alg password)))
    (test-assert pkcs-encrypted-private-key-info? epki)
    (let ((pki2 (pkcs-encrypted-private-key-info->pkcs-one-asymmetric-key
		 epki password)))
      (test-assert (pkcs-one-asymmetric-key? pki2))
      (test-assert (private-key? (pkcs-one-asymmetric-key-private-key pki2))))))

(define salt #vu8(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10))
(define count 1000)
(test-pkcs8 pk (make-pbe-md2-des-cbc-x509-algorithm-identifier  salt count))
(test-pkcs8 pk (make-pbe-md2-rc2-cbc-x509-algorithm-identifier  salt count))
(test-pkcs8 pk (make-pbe-md5-des-cbc-x509-algorithm-identifier  salt count))
(test-pkcs8 pk (make-pbe-md5-rc2-cbc-x509-algorithm-identifier  salt count))
(test-pkcs8 pk (make-pbe-sha1-des-cbc-x509-algorithm-identifier salt count))
(test-pkcs8 pk (make-pbe-sha1-rc2-cbc-x509-algorithm-identifier salt count)) 

(define iv #vu8(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6))
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-aes128-encryption-x509-algorithm-identifier iv)))
(define prfs (list *pbes:hmac/sha1*
		   *pbes:hmac/sha224*
		   *pbes:hmac/sha256*
		   *pbes:hmac/sha384*
		   *pbes:hmac/sha512*
		   *pbes:hmac/sha512/224*
		   *pbes:hmac/sha512/256*
		   *pbes:hmac/sha3-224*
		   *pbes:hmac/sha3-256*
		   *pbes:hmac/sha3-384*
		   *pbes:hmac/sha3-512*))
(for-each (lambda (prf)
	    (test-pkcs8 pk
	     (make-pbes2-x509-algorithm-identifier
	      (make-pbkdf2-x509-algorithm-identifier salt count :prf prf)
	      (make-aes128-encryption-x509-algorithm-identifier iv))))
	  prfs)
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-aes192-encryption-x509-algorithm-identifier iv)))
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-aes256-encryption-x509-algorithm-identifier iv)))
;; We don't IV length check so we can just pass longer block...
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-des3-encryption-x509-algorithm-identifier iv)))
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-des-encryption-x509-algorithm-identifier iv)))

(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-rc2-encryption-x509-algorithm-identifier 160 iv)))
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-rc2-encryption-x509-algorithm-identifier 120 iv)))
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-rc2-encryption-x509-algorithm-identifier 58 iv)))
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-rc2-encryption-x509-algorithm-identifier 256 iv)))
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count)
   (make-rc2-encryption-x509-algorithm-identifier #f iv)))
(test-error (make-rc2-encryption-x509-algorithm-identifier 64 iv))

;; Limitation due to libtomcrypto
;; rounds can only be between 12 and 24, 0 can also be but that'd be
;; rejected by the API call :D
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count :key-length 32)
   (make-rc5-encryption-x509-algorithm-identifier 12 64)))
(test-pkcs8 pk
  (make-pbes2-x509-algorithm-identifier
   (make-pbkdf2-x509-algorithm-identifier salt count :key-length 32)
   (make-rc5-encryption-x509-algorithm-identifier 12 128)))

(test-end)
