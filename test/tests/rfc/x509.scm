(import (rnrs)
	(rfc x509)
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

  (test-assert "check validity (default)" (check-validity x509))
  (test-error "check validity (before)" (check-validity 
					 x509
					 (string->date "2009/01/01" "Y/m/d")))
  (test-error "check validity (after)" (check-validity 
					 x509
					 (string->date "2021/01/01" "Y/m/d")))
  )

(let ()
  (define cert (string-append (current-directory)
			      "/test/data/certs/EE.cer"))
  (define ca-cert (string-append (current-directory)
			      "/test/data/certs/CA.cer"))
  (define (->certificate file)
    (let-values (((param content) (parse-pem-file file)))
      (make-x509-certificate content)))
  (test-assert "RSASSA-PSS signature certificate verify"
   (x509:verify-certificate (->certificate cert)
    (x509-certificate-get-public-key (->certificate ca-cert)))))

(test-end)
