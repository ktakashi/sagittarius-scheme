(import (rnrs)
	(sagittarius crypto pkix certificate)
	(sagittarius crypto keys)
	(rfc base64)
	(srfi :19)
	(srfi :64))

(test-begin "X.509 certificate")

(define cert
  (base64-decode-string
   (string-append
    "MIICcDCCAhWgAwIBAgIUOJ0BDDovxF2W3+0Kndg1cl9q5NQwCgYIKoZIzj0EAwIw"
    "gYwxCzAJBgNVBAYTAk5MMRUwEwYDVQQIDAxadWlkLUhvbGxhbmQxDzANBgNVBAcM"
    "BkxlaWRlbjEbMBkGA1UECgwSU2FnaXR0YXJpdXMgU2NoZW1lMRUwEwYDVQQDDAxU"
    "YWthc2hpIEthdG8xITAfBgkqhkiG9w0BCQEWEmt0YWthc2hpQHltYWlsLmNvbTAe"
    "Fw0yMjEwMjQxODM2MjhaFw0zMjEwMjExODM2MjhaMIGMMQswCQYDVQQGEwJOTDEV"
    "MBMGA1UECAwMWnVpZC1Ib2xsYW5kMQ8wDQYDVQQHDAZMZWlkZW4xGzAZBgNVBAoM"
    "ElNhZ2l0dGFyaXVzIFNjaGVtZTEVMBMGA1UEAwwMVGFrYXNoaSBLYXRvMSEwHwYJ"
    "KoZIhvcNAQkBFhJrdGFrYXNoaUB5bWFpbC5jb20wWTATBgcqhkjOPQIBBggqhkjO"
    "PQMBBwNCAAQdBG9xFor8DDnduyf9S1oJvX7Z7fZnZlbyen0/vXD/CghqKJRrgins"
    "yUb4D2cp0N/gvKX5b1Lkmw5dLCD02bJXo1MwUTAdBgNVHQ4EFgQUgm9i/kQ+u/KT"
    "zBcddqH9Gy8tT48wHwYDVR0jBBgwFoAUgm9i/kQ+u/KTzBcddqH9Gy8tT48wDwYD"
    "VR0TAQH/BAUwAwEB/zAKBggqhkjOPQQDAgNJADBGAiEAr3vM7SgzcSdsJ25chY5G"
    "DdYbGazEK4PU6oJRGj2RXCACIQDbm8/zohffUKa2zYJ77NUVEN4+oZquRW/ZMg1i"
    "UeWU6g==")
   :transcoder #f))

(let ((x509-cert
       (read-x509-certificate (open-bytevector-input-port cert))))
  (test-assert (x509-certificate? x509-cert))
  (test-equal 3 (x509-certificate-version x509-cert))
  (test-assert (x509-name? (x509-certificate-issuer-dn x509-cert)))
  (test-assert (x509-name? (x509-certificate-subject-dn x509-cert)))
  (let ((validity (x509-certificate-validity x509-cert)))
    (test-assert (x509-validity? validity))
    (test-assert (date? (x509-validity-not-before validity)))
    (test-assert (date? (x509-validity-not-after validity))))
  (test-assert (public-key? (x509-certificate-public-key x509-cert)))
  (let-values (((out e) (open-bytevector-output-port)))
    (let ((bport (open-base64-encode-output-port out)))
      (write-x509-certificate x509-cert bport)
      (close-output-port bport)
      (test-equal (base64-encode cert :line-width #f) (e)))))

(test-end)
