(import (rnrs)
	(rfc x.509)
	(except (crypto) verify) ;; avoid name confiliction
	(math)
	(rfc base64)
	(srfi :64 testing)
	(srfi :19 time))

(define private-key
  "MIICWwIBAAKBgQDRhGF7X4A0ZVlEg594WmODVVUIiiPQs04aLmvfg8SborHss5gQ\r\n\
   Xu0aIdUT6nb5rTh5hD2yfpF2WIW6M8z0WxRhwicgXwi80H1aLPf6lEPPLvN29EhQ\r\n\
   NjBpkFkAJUbS8uuhJEeKw0cE49g80eBBF4BCqSL6PFQbP9/rByxdxEoAIQIDAQAB\r\n\
   AoGAA9/q3Zk6ib2GFRpKDLO/O2KMnAfR+b4XJ6zMGeoZ7Lbpi3MW0Nawk9ckVaX0\r\n\
   ZVGqxbSIX5Cvp/yjHHpww+QbUFrw/gCjLiiYjM9E8C3uAF5AKJ0r4GBPl4u8K4bp\r\n\
   bXeSxSB60/wPQFiQAJVcA5xhZVzqNuF3EjuKdHsw+dk+dPECQQDubX/lVGFgD/xY\r\n\
   uchz56Yc7VHX+58BUkNSewSzwJRbcueqknXRWwj97SXqpnYfKqZq78dnEF10SWsr\r\n\
   /NMKi+7XAkEA4PVqDv/OZAbWr4syXZNv/Mpl4r5suzYMMUD9U8B2JIRnrhmGZPzL\r\n\
   x23N9J4hEJ+Xh8tSKVc80jOkrvGlSv+BxwJAaTOtjA3YTV+gU7Hdza53sCnSw/8F\r\n\
   YLrgc6NOJtYhX9xqdevbyn1lkU0zPr8mPYg/F84m6MXixm2iuSz8HZoyzwJARi2p\r\n\
   aYZ5/5B2lwroqnKdZBJMGKFpUDn7Mb5hiSgocxnvMkv6NjT66Xsi3iYakJII9q8C\r\n\
   Ma1qZvT/cigmdbAh7wJAQNXyoizuGEltiSaBXx4H29EdXNYWDJ9SS5f070BRbAIl\r\n\
   dqRh3rcNvpY6BKJqFapda1DjdcncZECMizT/GMrc1w==")

;; the certificate is from
;;  http://www9.atwiki.jp/kurushima/pub/jsrsa/sample-rsasign.html
(define certificate
  "MIIBvTCCASYCCQD55fNzc0WF7TANBgkqhkiG9w0BAQUFADAjMQswCQYDVQQGEwJK\r\n\
   UDEUMBIGA1UEChMLMDAtVEVTVC1SU0EwHhcNMTAwNTI4MDIwODUxWhcNMjAwNTI1\r\n\
   MDIwODUxWjAjMQswCQYDVQQGEwJKUDEUMBIGA1UEChMLMDAtVEVTVC1SU0EwgZ8w\r\n\
   DQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBANGEYXtfgDRlWUSDn3haY4NVVQiKI9Cz\r\n\
   Thoua9+DxJuiseyzmBBe7Roh1RPqdvmtOHmEPbJ+kXZYhbozzPRbFGHCJyBfCLzQ\r\n\
   fVos9/qUQ88u83b0SFA2MGmQWQAlRtLy66EkR4rDRwTj2DzR4EEXgEKpIvo8VBs/\r\n\
   3+sHLF3ESgAhAgMBAAEwDQYJKoZIhvcNAQEFBQADgYEAEZ6mXFFq3AzfaqWHmCy1\r\n\
   ARjlauYAa8ZmUFnLm0emg9dkVBJ63aEqARhtok6bDQDzSJxiLpCEF6G4b/Nv/M/M\r\n\
   LyhP+OoOTmETMegAVQMq71choVJyOFE5BtQa6M/lCHEOya5QUfoRF2HF9EjRF44K\r\n\
   3OK+u3ivTSj3zwjtpudY5Xo=")

;; for sign
(define rsa-private-key (import-private-key RSA
			 (open-bytevector-input-port
			  (base64-decode (string->utf8 private-key)))))
(define rsa-private-cipher (cipher RSA rsa-private-key))
(define message (string->utf8 "test message for verify"))

(define external-signature 
  (integer->bytevector #x5576ce2c7cb15d70a793da8defb991317bc8c7264f72e79b5e048938e7a5d621db5fc1d68ab37a1ae1142c0a9c45257a9fe3f668906ba2dffaa221e125f54d45f1c0c36fbee1e38c7d19a7e192096afa9e45870c70707c39f73111e5a3d58c724d3c9a24930769309c7286bfe7c7e78d15feebca72c33b57330f2a79d171a443))

(test-begin "x.509 certificate test")

(let ((x509 (make-x509-certificate
	     (open-bytevector-input-port
	      (base64-decode (string->utf8 certificate))))))
  (test-assert "x509-certificate?" (x509-certificate? x509))
  ;; basic information check
  (test-equal "version" 1 (x509-certificate-get-version x509))
  (test-equal "serial number"
	      #x00f9e5f373734585ed
	      (x509-certificate-get-serial-number x509))
  (test-equal "serial number algorithm"
	      "1.2.840.113549.1.1.5"
	      (x509-certificate-get-signature-algorithm x509))

  (test-assert "default verify" (verify x509 message external-signature))
  ;; invalid hash type
  (test-error "default verify (failed)" 
	      (verify x509 message external-signature :hash SHA-256))
  ;; TODO the certificate valid from 2010 until 2020
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
					  :verify pkcs1-emsa-pss-encode))
    (test-assert "SHA-256 EMSA-PSS" (verify x509 message signature-sha1-emsa-pss
					  :verify pkcs1-emsa-pss-encode
					  :hash SHA-256))
    (test-assert "SHA-256 PKCS-1.5" (verify x509 message signature-sha1-emsa-pss
					    :hash SHA-256))

    (test-error "SHA-1 EMSA-PSS (error)"
		(verify x509 message signature-sha1-emsa-pss))
    (test-error "SHA-256 EMSA-PSS (error)"
		(verify x509 message signature-sha1-emsa-pss))
    (test-error "SHA-256 PKCS-1.5 (error)"
		(verify x509 message signature-sha1-emsa-pss))

    )

  )

(test-end)