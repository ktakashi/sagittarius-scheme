(import (rnrs)
	(sagittarius crypto pem)
	(sagittarius crypto x509)
	(sagittarius crypto pkcs cms)
	(sagittarius crypto pkcs keys)
	(sagittarius crypto keys)
	(srfi :13)
	(srfi :64))

(test-begin "PEM reader/writer")

(define all-styles (list *standard-style*
		     *lax-style*
		     *standard-style*
		     *rfc1421-style*))
(define styles-w/o-legacy (list *standard-style*
				*lax-style*
				*standard-style*))
(define (test-pem pem-string label label2 pred
		  :optional (styles all-styles)
			    (pred2 pred))
  (define (run style)
    (let ((pm (string->pem-object pem-string style)))
      (test-assert (pem-object? pm))
      (test-equal label (pem-object-label pm))
      (test-assert (pred (pem-object->object pm)))
      (let* ((s (pem-object->string (->pem-object (pem-object->object pm))))
	     (pm2 (string->pem-object s)))
	(test-equal label2 (pem-object-label pm2))
	(test-assert (pred2 (pem-object->object pm2))))))
  (for-each run styles))
	
(define cert-pem
  (string-join
   '("-----BEGIN CERTIFICATE-----"
     "MIICLDCCAdKgAwIBAgIBADAKBggqhkjOPQQDAjB9MQswCQYDVQQGEwJCRTEPMA0G"
     "A1UEChMGR251VExTMSUwIwYDVQQLExxHbnVUTFMgY2VydGlmaWNhdGUgYXV0aG9y"
     "aXR5MQ8wDQYDVQQIEwZMZXV2ZW4xJTAjBgNVBAMTHEdudVRMUyBjZXJ0aWZpY2F0"
     "ZSBhdXRob3JpdHkwHhcNMTEwNTIzMjAzODIxWhcNMTIxMjIyMDc0MTUxWjB9MQsw"
     "CQYDVQQGEwJCRTEPMA0GA1UEChMGR251VExTMSUwIwYDVQQLExxHbnVUTFMgY2Vy"
     "dGlmaWNhdGUgYXV0aG9yaXR5MQ8wDQYDVQQIEwZMZXV2ZW4xJTAjBgNVBAMTHEdu"
     "dVRMUyBjZXJ0aWZpY2F0ZSBhdXRob3JpdHkwWTATBgcqhkjOPQIBBggqhkjOPQMB"
     "BwNCAARS2I0jiuNn14Y2sSALCX3IybqiIJUvxUpj+oNfzngvj/Niyv2394BWnW4X"
     "uQ4RTEiywK87WRcWMGgJB5kX/t2no0MwQTAPBgNVHRMBAf8EBTADAQH/MA8GA1Ud"
     "DwEB/wQFAwMHBgAwHQYDVR0OBBYEFPC0gf6YEr+1KLlkQAPLzB9mTigDMAoGCCqG"
     "SM49BAMCA0gAMEUCIDGuwD1KPyG+hRf88MeyMQcqOFZD0TbVleF+UsAGQ4enAiEA"
     "l4wOuDwKQa+upc8GftXE2C//4mKANBC6It01gUaTIpo="
     "-----END CERTIFICATE-----")
   "\n"))

(test-pem cert-pem "CERTIFICATE" "CERTIFICATE" x509-certificate?)

(define cert-pem2
  (string-join
   '("Subject: CN=Atlantis"
     "Issuer: CN=Atlantis"
     "Validity: from 7/9/2012 3:10:38 AM UTC to 7/9/2013 3:10:37 AM UTC"
     "-----BEGIN CERTIFICATE-----"
     "MIIBmTCCAUegAwIBAgIBKjAJBgUrDgMCHQUAMBMxETAPBgNVBAMTCEF0bGFudGlz"
     "MB4XDTEyMDcwOTAzMTAzOFoXDTEzMDcwOTAzMTAzN1owEzERMA8GA1UEAxMIQXRs"
     "YW50aXMwXDANBgkqhkiG9w0BAQEFAANLADBIAkEAu+BXo+miabDIHHx+yquqzqNh"
     "Ryn/XtkJIIHVcYtHvIX+S1x5ErgMoHehycpoxbErZmVR4GCq1S2diNmRFZCRtQID"
     "AQABo4GJMIGGMAwGA1UdEwEB/wQCMAAwIAYDVR0EAQH/BBYwFDAOMAwGCisGAQQB"
     "gjcCARUDAgeAMB0GA1UdJQQWMBQGCCsGAQUFBwMCBggrBgEFBQcDAzA1BgNVHQEE"
     "LjAsgBA0jOnSSuIHYmnVryHAdywMoRUwEzERMA8GA1UEAxMIQXRsYW50aXOCASow"
     "CQYFKw4DAh0FAANBAKi6HRBaNEL5R0n56nvfclQNaXiDT174uf+lojzA4lhVInc0"
     "ILwpnZ1izL4MlI9eCSHhVQBHEp2uQdXJB+d5Byg="
     "-----END CERTIFICATE-----")
   "\n"))
(test-pem cert-pem2 "CERTIFICATE" "CERTIFICATE" x509-certificate?
	  styles-w/o-legacy)

(define crl-pem
  (string-join
   '("-----BEGIN X509 CRL-----"
     "MIIB9DCCAV8CAQEwCwYJKoZIhvcNAQEFMIIBCDEXMBUGA1UEChMOVmVyaVNpZ24s"
     "IEluYy4xHzAdBgNVBAsTFlZlcmlTaWduIFRydXN0IE5ldHdvcmsxRjBEBgNVBAsT"
     "PXd3dy52ZXJpc2lnbi5jb20vcmVwb3NpdG9yeS9SUEEgSW5jb3JwLiBieSBSZWYu"
     "LExJQUIuTFREKGMpOTgxHjAcBgNVBAsTFVBlcnNvbmEgTm90IFZhbGlkYXRlZDEm"
     "MCQGA1UECxMdRGlnaXRhbCBJRCBDbGFzcyAxIC0gTmV0c2NhcGUxGDAWBgNVBAMU"
     "D1NpbW9uIEpvc2Vmc3NvbjEiMCAGCSqGSIb3DQEJARYTc2ltb25Aam9zZWZzc29u"
     "Lm9yZxcNMDYxMjI3MDgwMjM0WhcNMDcwMjA3MDgwMjM1WjAjMCECEC4QNwPfRoWd"
     "elUNpllhhTgXDTA2MTIyNzA4MDIzNFowCwYJKoZIhvcNAQEFA4GBAD0zX+J2hkcc"
     "Nbrq1Dn5IKL8nXLgPGcHv1I/le1MNo9t1ohGQxB5HnFUkRPAY82fR6Epor4aHgVy"
     "b+5y+neKN9Kn2mPF4iiun+a4o26CjJ0pArojCL1p8T0yyi9Xxvyc/ezaZ98HiIyP"
     "c3DGMNR+oUmSjKZ0jIhAYmeLxaPHfQwR"
     "-----END X509 CRL-----")
   "\n"))

(test-pem crl-pem "X509 CRL" "X509 CRL" x509-certificate-revocation-list?)

(define csr-pem
  (string-join
   '("-----BEGIN CERTIFICATE REQUEST-----"
     "MIIBWDCCAQcCAQAwTjELMAkGA1UEBhMCU0UxJzAlBgNVBAoTHlNpbW9uIEpvc2Vm"
     "c3NvbiBEYXRha29uc3VsdCBBQjEWMBQGA1UEAxMNam9zZWZzc29uLm9yZzBOMBAG"
     "ByqGSM49AgEGBSuBBAAhAzoABLLPSkuXY0l66MbxVJ3Mot5FCFuqQfn6dTs+9/CM"
     "EOlSwVej77tj56kj9R/j9Q+LfysX8FO9I5p3oGIwYAYJKoZIhvcNAQkOMVMwUTAY"
     "BgNVHREEETAPgg1qb3NlZnNzb24ub3JnMAwGA1UdEwEB/wQCMAAwDwYDVR0PAQH/"
     "BAUDAwegADAWBgNVHSUBAf8EDDAKBggrBgEFBQcDATAKBggqhkjOPQQDAgM/ADA8"
     "AhxBvfhxPFfbBbsE1NoFmCUczOFApEuQVUw3ZP69AhwWXk3dgSUsKnuwL5g/ftAY"
     "dEQc8B8jAcnuOrfU"
     "-----END CERTIFICATE REQUEST-----")
   "\n"))
(test-pem csr-pem "CERTIFICATE REQUEST" "CERTIFICATE REQUEST"
	  x509-certification-request?)

(define csr-pem2
  (string-join
   '("-----BEGIN NEW CERTIFICATE REQUEST-----"
     "MIIBWDCCAQcCAQAwTjELMAkGA1UEBhMCU0UxJzAlBgNVBAoTHlNpbW9uIEpvc2Vm"
     "c3NvbiBEYXRha29uc3VsdCBBQjEWMBQGA1UEAxMNam9zZWZzc29uLm9yZzBOMBAG"
     "ByqGSM49AgEGBSuBBAAhAzoABLLPSkuXY0l66MbxVJ3Mot5FCFuqQfn6dTs+9/CM"
     "EOlSwVej77tj56kj9R/j9Q+LfysX8FO9I5p3oGIwYAYJKoZIhvcNAQkOMVMwUTAY"
     "BgNVHREEETAPgg1qb3NlZnNzb24ub3JnMAwGA1UdEwEB/wQCMAAwDwYDVR0PAQH/"
     "BAUDAwegADAWBgNVHSUBAf8EDDAKBggrBgEFBQcDATAKBggqhkjOPQQDAgM/ADA8"
     "AhxBvfhxPFfbBbsE1NoFmCUczOFApEuQVUw3ZP69AhwWXk3dgSUsKnuwL5g/ftAY"
     "dEQc8B8jAcnuOrfU"
     "-----END NEW CERTIFICATE REQUEST-----")
   "\n"))
(test-pem csr-pem2 "NEW CERTIFICATE REQUEST" "CERTIFICATE REQUEST"
	  x509-certification-request?)

(define pkcs7-pem
  (string-join
   '("-----BEGIN PKCS7-----"
     "MIHjBgsqhkiG9w0BCRABF6CB0zCB0AIBADFho18CAQCgGwYJKoZIhvcNAQUMMA4E"
     "CLfrI6dr0gUWAgITiDAjBgsqhkiG9w0BCRADCTAUBggqhkiG9w0DBwQIZpECRWtz"
     "u5kEGDCjerXY8odQ7EEEromZJvAurk/j81IrozBSBgkqhkiG9w0BBwEwMwYLKoZI"
     "hvcNAQkQAw8wJDAUBggqhkiG9w0DBwQI0tCBcU09nxEwDAYIKwYBBQUIAQIFAIAQ"
     "OsYGYUFdAH0RNc1p4VbKEAQUM2Xo8PMHBoYdqEcsbTodlCFAZH4="
     "-----END PKCS7-----")
   "\n"))
(test-pem pkcs7-pem "PKCS7" "CMS" cms-content-info?)

(define cms-pem
  (string-join
   '("-----BEGIN CMS-----"
     "MIGDBgsqhkiG9w0BCRABCaB0MHICAQAwDQYLKoZIhvcNAQkQAwgwXgYJKoZIhvcN"
     "AQcBoFEET3icc87PK0nNK9ENqSxItVIoSa0o0S/ISczMs1ZIzkgsKk4tsQ0N1nUM"
     "dvb05OXi5XLPLEtViMwvLVLwSE0sKlFIVHAqSk3MBkkBAJv0Fx0="
     "-----END CMS-----")
   "\n"))
(test-pem cms-pem "CMS" "CMS" cms-content-info?)

(define private-key-pem
  (string-join
   '("-----BEGIN PRIVATE KEY-----"
     "MIGEAgEAMBAGByqGSM49AgEGBSuBBAAKBG0wawIBAQQgVcB/UNPxalR9zDYAjQIf"
     "jojUDiQuGnSJrFEEzZPT/92hRANCAASc7UJtgnF/abqWM60T3XNJEzBv5ez9TdwK"
     "H0M6xpM2q+53wmsN/eYLdgtjgBd3DBmHtPilCkiFICXyaA8z9LkJ"
     "-----END PRIVATE KEY-----")
   "\n"))
(test-pem private-key-pem "PRIVATE KEY" "PRIVATE KEY" pkcs-one-asymmetric-key?)

(define encrypted-private-key-pem
  (string-join
   '("-----BEGIN ENCRYPTED PRIVATE KEY-----"
     "MIHNMEAGCSqGSIb3DQEFDTAzMBsGCSqGSIb3DQEFDDAOBAghhICA6T/51QICCAAw"
     "FAYIKoZIhvcNAwcECBCxDgvI59i9BIGIY3CAqlMNBgaSI5QiiWVNJ3IpfLnEiEsW"
     "Z0JIoHyRmKK/+cr9QPLnzxImm0TR9s4JrG3CilzTWvb0jIvbG3hu0zyFPraoMkap"
     "8eRzWsIvC5SVel+CSjoS2mVS87cyjlD+txrmrXOVYDE+eTgMLbrLmsWh3QkCTRtF"
     "QC7k0NNzUHTV9yGDwfqMbw=="
     "-----END ENCRYPTED PRIVATE KEY-----")
   "\n"))
(test-pem encrypted-private-key-pem
	  "ENCRYPTED PRIVATE KEY" "ENCRYPTED PRIVATE KEY"
	  pkcs-encrypted-private-key-info?)

(define public-key-pem
  (string-join
   '("-----BEGIN PUBLIC KEY-----"
     "MHYwEAYHKoZIzj0CAQYFK4EEACIDYgAEn1LlwLN/KBYQRVH6HfIMTzfEqJOVztLe"
     "kLchp2hi78cCaMY81FBlYs8J9l7krc+M4aBeCGYFjba+hiXttJWPL7ydlE+5UG4U"
     "Nkn3Eos8EiZByi9DVsyfy9eejh+8AXgp"
     "-----END PUBLIC KEY-----")
   "\n"))
(test-pem public-key-pem "PUBLIC KEY" "PUBLIC KEY" subject-public-key-info?)

(define ec-private-key-pem
  (string-join
   '("-----BEGIN EC PRIVATE KEY-----"
     "MHcCAQEEICajCcEdM+yv3idVWhOMcLS5tz0E/d6Ac0Shbt5gHKyPoAoGCCqGSM49"
     "AwEHoUQDQgAEMWwGFfCppX3QTAAOCEvtIft3ptTVeZWiWuuLYPjhhOF7qpEA69xT"
     "6fuVXBb8SMrVkxjfVoqWqung69IdZ6mzuw=="
     "-----END EC PRIVATE KEY-----")
   "\n"))
(test-pem ec-private-key-pem "EC PRIVATE KEY" "PRIVATE KEY"
	  ecdsa-private-key? all-styles pkcs-one-asymmetric-key?)
(let* ((pem (string->pem-object ec-private-key-pem))
       (pem2 (ecdsa-private-key->pem-object (pem-object->object pem))))
  (test-equal "EC PRIVATE KEY" (pem-object-label pem2)))

(let ((kp (generate-key-pair *key:ed25519*)))
  (test-equal "PRIVATE KEY"
	      (pem-object-label (->pem-object (key-pair-private kp))))
  (test-equal "PUBLIC KEY"
	      (pem-object-label (->pem-object (key-pair-public kp)))))

(test-end)
