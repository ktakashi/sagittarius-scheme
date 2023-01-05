(import (rnrs)
	(sagittarius crypto pem)
	(sagittarius crypto x509)
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
		  :optional (styles all-styles))
  (define (run style)
    (let ((pm (string->pem-object pem-string style)))
      (test-assert (pem-object? pm))
      (test-equal label (pem-object-label pm))
      (test-assert (pred (pem-object->object pm)))
      (let* ((s (pem-object->string (->pem-object (pem-object->object pm))))
	     (pm2 (string->pem-object s)))
	(test-equal label2 (pem-object-label pm2))
	(test-assert (pred (pem-object->object pm2))))))
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


(test-end)
