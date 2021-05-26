(import (rnrs)
	(rfc x.509)
	(except (crypto) verify) ;; avoid name confiliction
	(math)
	(rfc base64)
	(rfc pem)
	(srfi :64 testing)
	(srfi :19 time))

(define private-key
  "MIICXgIBAAKBgQDM4W4nwaRKowjjjV4JeVlCRMbZSx2y+lEmKAwWRGTRsWI7UpRN\
   1f4L7TLWzHOi84yk2f7YQxuelTfXauhox4Wmp83kaEPULNcYKNgryfO9BH52xgII\
   M5Vw0dAfHUzwtasJO3LYMGr6keMSJGn11HwK6gJGrS/mv7f2AOmlbMil8QIDAQAB\
   AoGBALslUFHQeBoVQpFZGITe68G0wU7LZp2OOYH5ItFUOCOzPF+j1wQibKTITqAW\
   saovflE3L6qr4hbBhvinFxRe7Jzi1quf67bkT6oCF9xmkosaHqyM2aHhsBv2dN1o\
   0F/YB7CO5Vk06jciyyS+0SSIHjxLq4879lXmG3zNozn1IqaBAkEA+Tt+0KUWxXIE\
   Sw1j+RrjvnjokNmCchbA1sMkev306/DMUdFi2HbjngyLZqQ5bsz4MH9dR7iOX5uQ\
   o+cMu11S9QJBANJxoSAOTn367a31ElVz9CSxVYFRyUYfNwRjALvFWbHz8v0sjzKD\
   G6b01tIRiGzsvIoPvtcOg99nVuvoqpVzAY0CQQC0xamKzmOA3N0uTs0XjqBntC0D\
   ezB5bfV7EFmyd+chByw7oALw/PLF3VVzImSh8DuIeaj/72YdBKnZqCl88QjNAkB+\
   VUakBtKqIGx1BO13rT+Ak5DALIgG95BAqupyztu0z70srYZdjYWZKdyYiXv73/aj\
   byZi1kzmXZQ51Rgq1bq1AkEAvnAgJa2bb+wEYBc0YK9pJi8rx2SNHVfANP03eZiN\
   HT01fVXB+UaKQYPdPNaJvZ3XbOKMaCghPUAcmgrHlEapXg==")

;; the certificate is from
;;  http://www9.atwiki.jp/kurushima/pub/jsrsa/sample-rsasign.html
(define certificate
  "MIICkzCCAfwCCQC915I99J3TtjANBgkqhkiG9w0BAQsFADCBjDELMAkGA1UEBhMC\
   TkwxFTATBgNVBAgMDFp1aWQtSG9sbGFuZDEPMA0GA1UEBwwGTGVpZGVuMRswGQYD\
   VQQKDBJTYWdpdHRhcml1cyBTY2hlbWUxFTATBgNVBAMMDFRha2FzaGkgS2F0bzEh\
   MB8GCSqGSIb3DQEJARYSa3Rha2FzaGlAeW1haWwuY29tMCAXDTIwMDUyODIwMjIz\
   MFoYDzIxMjAwNTA0MjAyMjMwWjCBjDELMAkGA1UEBhMCTkwxFTATBgNVBAgMDFp1\
   aWQtSG9sbGFuZDEPMA0GA1UEBwwGTGVpZGVuMRswGQYDVQQKDBJTYWdpdHRhcml1\
   cyBTY2hlbWUxFTATBgNVBAMMDFRha2FzaGkgS2F0bzEhMB8GCSqGSIb3DQEJARYS\
   a3Rha2FzaGlAeW1haWwuY29tMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDM\
   4W4nwaRKowjjjV4JeVlCRMbZSx2y+lEmKAwWRGTRsWI7UpRN1f4L7TLWzHOi84yk\
   2f7YQxuelTfXauhox4Wmp83kaEPULNcYKNgryfO9BH52xgIIM5Vw0dAfHUzwtasJ\
   O3LYMGr6keMSJGn11HwK6gJGrS/mv7f2AOmlbMil8QIDAQABMA0GCSqGSIb3DQEB\
   CwUAA4GBAHJ6WE5m1TeUusirAQ0HEcTv0eH+1b1e7IhtHnW5pwJYGp9G4vDrYaDQ\
   c7/OIrAJeHCQQVCJyr5M6sCoM43q9NtIfWm/43X8krBV1xKwkEgX8ll7IizjIETa\
   wvGt0SDMednxL/WDOuYSDQ9/3t+ywKQsnWBWqwKIRcHz89rAAEs+")

;; for sign
(define rsa-private-key (import-private-key RSA
			 (open-bytevector-input-port
			  (base64-decode (string->utf8 private-key)))))
(define rsa-private-cipher (cipher RSA rsa-private-key))
(define message (string->utf8 "test message for verify"))

(define external-signature 
  (integer->bytevector #x5eb02b782b3003334d1fdf5133f19ccd5fa6fb3706f54531cf01dc5b4e20fdf1349ab09595b918b068c2a075ca314ff275ecf83b4df8798a050e7bfb3d87817f597a7fbad649b8148fddbf56d657aa536282a494c300ee7cb7ca56dea5a6fb7b2c2e906647eb227a3707c5e0c2a7c276bb7d116220510d5d4c4f4b4ade6deacc))

(test-begin "x.509 certificate test")

(let ((x509 (make-x509-certificate
	     (open-bytevector-input-port
	      (base64-decode (string->utf8 certificate))))))
  (test-assert "x509-certificate?" (x509-certificate? x509))
  ;; basic information check
  (test-equal "version" 1 (x509-certificate-get-version x509))
  (test-equal "serial number"
	      #xbdd7923df49dd3b6
	      (x509-certificate-get-serial-number x509))
  (test-equal "serial number algorithm"
	      "1.2.840.113549.1.1.11"
	      (x509-certificate-get-signature-algorithm x509))

  (test-assert "default verify" (verify x509 message external-signature))
  ;; invalid hash type
  (test-error "default verify (failed)" 
	      (verify x509 message external-signature :hash SHA-256))
  (test-assert "check validity (default)" (check-validity x509))
  (test-error "check validity (before)" (check-validity 
					 x509
					 (string->date "2009/01/01" "Y/m/d")))
  (test-error "check validity (after)" (check-validity 
					 x509
					 (string->date "2021/01/01" "Y/m/d")))

  ;; try other signature and hash algorithm
  (let ((signature-sha1-emsa-pss (sign rsa-private-cipher message))
	(signature-sha256-emsa-pss (sign rsa-private-cipher message
					 :hash SHA-256))
	(signature-sha256-pkcs1.5 (sign rsa-private-cipher message
					:encode pkcs1-emsa-v1.5-encode
					:hash SHA-256)))
    
    (test-assert "SHA-1 EMSA-PSS" (verify x509 message signature-sha1-emsa-pss
					  :verify pkcs1-emsa-pss-verify))
    (test-assert "SHA-256 EMSA-PSS" (verify x509 message
					    signature-sha256-emsa-pss
					    :verify pkcs1-emsa-pss-verify
					    :hash SHA-256))
    (test-assert "SHA-256 PKCS-1.5" (verify x509 message
					    signature-sha256-pkcs1.5
					    :hash SHA-256))

    (test-error "SHA-1 EMSA-PSS (error)"
		(verify x509 message signature-sha1-emsa-pss))
    (test-error "SHA-256 EMSA-PSS (error)"
		(verify x509 message signature-sha1-emsa-pss))
    (test-error "SHA-256 PKCS-1.5 (error)"
		(verify x509 message signature-sha1-emsa-pss))

    )

  )

(let ()
  (define cert "-----BEGIN CERTIFICATE-----
MIIDFjCCAc0CFB4LLlNT9Ju1ZHqjOGIltmQrLX34MD4GCSqGSIb3DQEBCjAxoA0w
CwYJYIZIAWUDBAIBoRowGAYJKoZIhvcNAQEIMAsGCWCGSAFlAwQCAaIEAgIA3jAN
MQswCQYDVQQDDAJDQTAeFw0yMTA1MjUxODI2MTlaFw0yMTA2MjQxODI2MTlaMCIx
IDAeBgNVBAMMF3NhZ2l0dGFyaXVzLXNjaGVtZS10ZXN0MIIBIDALBgkqhkiG9w0B
AQoDggEPADCCAQoCggEBAK+KEHYsl65M6HMYL9pmuqqAgtJ0fJ3tSnNFK1gR6a7u
21zDGLpSdO4XA/Shrqf0svzC4QrOVh7XZ9uIFOfp2WwR3Q3JUrPDK6x8MsDLV/Vl
S2CQAptBLqZnPGEu4gFkz4GGBR6QwCyjnx9yNXpkaRHlIV72j0kt+zdgjykX4cPP
HCZwvHne9oshOodFwjwc385pl7rRnFR7T+eGBKIwmzU/GuWvn1XBkHpj7eDeOtx4
H/5eAdoeA1loNKnLu4xW62ZGaTmhiDlaAvF1ENLCJJM8ZVSut2fk+221arL70dvd
GEumHhHqr5kN5tzRUBPkrwdnEIIPzX4B1dXx8XjFWIUCAwEAATA+BgkqhkiG9w0B
AQowMaANMAsGCWCGSAFlAwQCAaEaMBgGCSqGSIb3DQEBCDALBglghkgBZQMEAgGi
BAICAN4DggEBAGoeDbXSs0PKYSNbUXSjwXySKN7reFD+wIrHI9UbS1fbusEOUmY7
5fmb6BGkFPN8wMvI33/qkzwrVKlx2SKRIq2wPSRVq/Ir702Kds9rog4lexIMIKzs
bkUi5hY7bddRZeDNJIPNZ/uxuywkY2d5U4wvmv1cOjhaVLkbUt2BLVZAKsHtXW3f
5XM5RwxAejuB+9sN5AkjBTTxx5byslAJPgVT8bC9siARiLNSgKMHDEo7okLmW1k5
exxnVrQCVb2ZV5Dw2DXLgp/y5ZJwWN7hLs+STXofwxosOyQ8uQ/l2cYk7QiesiOb
02B5oKmq6fqr3jWXNzsMhE7xg8AcEpm65Xc=
-----END CERTIFICATE-----")
  (define ca-cert "-----BEGIN CERTIFICATE-----
MIIDWzCCAhKgAwIBAgIUZJMFS/qyis7N/WB900YOeZzzJlowPgYJKoZIhvcNAQEK
MDGgDTALBglghkgBZQMEAgGhGjAYBgkqhkiG9w0BAQgwCwYJYIZIAWUDBAIBogQC
AgDeMA0xCzAJBgNVBAMMAkNBMB4XDTIxMDUyNTE4MjMzOVoXDTIxMDYyNDE4MjMz
OVowDTELMAkGA1UEAwwCQ0EwggEgMAsGCSqGSIb3DQEBCgOCAQ8AMIIBCgKCAQEA
vlli8oJPmnIEzPAzoiHM9OhkDwdtdCEaDoLcHOvFbhP9NZ/5T/QT+pxOCpAxZ6op
+Mb0hy2eB8eycmDJnx8AKoO6pWWrBh2TRLkZVfpLiuyfm+NRBxkAFa8GZ7bvYbN7
/IhTN5kU/qEOVbVwC3U5huqrWf+2CF0RP+JuvG6ORq8WqhRaJJC/0hMIGSZUW14h
kMEOR1F3IyTlkFty68wUQQWkVdOd40WDMGe0CXOBQ2rbGlYC3dSRlepvQi/P8ssA
V76ir6rMNGabLBfh3owu7BAL8kSLjqDMVhfuQEMurzx/XYogoSmxzBar0NSncDBD
V2FLv2jh/wxCRu/PEILIjQIDAQABo1MwUTAdBgNVHQ4EFgQUBhazEC0vLruOuXLx
WGFKGAo1BBowHwYDVR0jBBgwFoAUBhazEC0vLruOuXLxWGFKGAo1BBowDwYDVR0T
AQH/BAUwAwEB/zA+BgkqhkiG9w0BAQowMaANMAsGCWCGSAFlAwQCAaEaMBgGCSqG
SIb3DQEBCDALBglghkgBZQMEAgGiBAICAN4DggEBALM/Luineq3/CwpItNT63tdN
COMJJvDqtCTVInizmWpwJOxL780Lpm2j0mX9O5qNnlMuAlhYK4HxHjDjv12X2CFg
wbA8Ny7CU/vxCmPzwci7/MkNFVTuaD6fWS0WpB3k7T6qSW+M2ShogM7Q4Zw1pJhy
owWjWOqxibtsCpmeTrcCuA3lvrJpPUdpTpbg7Oh7yVzS2quP/LapottfYjBzCFza
LDCa44pYOpeQpMG5eOaTw2VXzGlPvwdusmMTqxp1aXLsk+tiS3jEAuKS0+q3qdtN
RibZ62DoxRmJC/mOLiFVwPFLZqJrtzO7/aJ2QBGBiKLnW6KnAKyrRsilNpQAR6c=
-----END CERTIFICATE-----")
  (define (->certificate file)
    (let-values (((param content) (parse-pem-string file)))
      (make-x509-certificate content)))
  (test-assert "RSASSA-PSS signature certificate verify"
   (x509:verify-certificate (->certificate cert)
    (x509-certificate-get-public-key (->certificate ca-cert)))))

(test-end)
