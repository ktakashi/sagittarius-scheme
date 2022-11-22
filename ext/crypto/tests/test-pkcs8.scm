(import (rnrs)
	(sagittarius crypto asn1)
	(sagittarius crypto pkcs keys)
	(sagittarius crypto pkcs pbes)
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

(test-begin "PKCS#8")
(define (test-pbe bv)
  (define password (string->utf8 "test1234"))
  (test-assert (pkcs-encrypted-private-key-info?
		(bytevector->pkcs-encrypted-private-key-info bv)))
  (let ((epki (bytevector->pkcs-encrypted-private-key-info bv)))
    (test-assert (x509-algorithm-identifier?
		  (pkcs-encrypted-private-key-info-encryption-algorithm epki)))
    (test-assert (bytevector?
		  (pkcs-encrypted-private-key-info-encrypted-data epki)))
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

(test-pbe epki1)
(test-pbe epki2)

(test-end)
