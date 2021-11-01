(import (rnrs)
	(rfc jwe)
	(rfc jwk)
	(rfc base64)
	(srfi :64))

(define jwe-string
  "eyJhbGciOiJSU0EtT0FFUCIsImVuYyI6IkEyNTZHQ00ifQ.OKOawDo13gRp2ojaHV7LFpZcgV7T6DVZKTyKOMTYUmKoTCVJRgckCL9kiMT03JGeipsEdY3mx_etLbbWSrFr05kLzcSr4qKAq7YN7e9jwQRb23nfa6c9d-StnImGyFDbSv04uVuxIp5Zms1gNxKKK2Da14B8S4rzVRltdYwam_lDp5XnZAYpQdb76FdIKLaVmqgfwX7XWRxv2322i-vDxRfqNzo_tETKzpVLzfiwQyeyPGLBIO56YJ7eObdv0je81860ppamavo35UgoRdbYaBcoh9QcfylQr66oc6vFWXRcZ_ZT2LawVCWTIy3brGPi6UklfCpIMfIjf7iGdXKHzg.48V1_ALb6US04U3b.5eym8TW_c8SuK0ltJ3rpYIzOeDQz7TALvtu6UG9oMo4vpzs9tX_EFShS8iB7j6jiSdiwkIr3ajwQzaBtQD_A.XFBoMYUZodetZdvTiFvSkQ")

(test-begin "JWE")

(let ((jwe (jwe:parse jwe-string)))
  (test-assert (jwe-object? jwe)))

(import (crypto))
(define-syntax test-content-encrypt/decrypt
  (syntax-rules ()
    ((_ header iv raw-key plain-text cipher-text tag)
     (let ((jwe-header header))
       (define (iv-generator size) iv)
       (define key (json->jwk
		    `#(("kty" . "oct")
		       ("k" . ,(utf8->string (base64url-encode raw-key))))))
       (define dir-encryptor
	 (make-direct-encryptor key :iv-generator iv-generator))
       (define dir-decryptor
	 (make-direct-decryptor key :strict? #f))
       (define bv-plain-text (string->utf8 plain-text))
       (let ((jwe-object (jwe:encrypt dir-encryptor jwe-header bv-plain-text)))
	 (test-equal cipher-text
		     (utf8->string
		      (base64url-encode (jwe-object-cipher-text jwe-object))))
	 (test-equal tag
		     (utf8->string
		      (base64url-encode
		       (jwe-object-authentication-tag jwe-object))))
	 (test-equal plain-text
		     (utf8->string (jwe:decrypt dir-decryptor jwe-object))))))))

;; These tests don't respect 'alg' field, so don't do it in a real application
;; it's just for test purpose.
(test-content-encrypt/decrypt
 (jwe-header-builder
  (alg 'RSA1_5)
  (enc 'A128CBC-HS256))
 #vu8(3 22 60 12 43 67 104 105 108 108 105 99 111 116 104 101)
 #vu8(4 211 31 197 84 157 252 254 11 100 157 250 63 170 106 206
      107 124 212 45 111 107 9 219 200 177 0 240 143 156 44 207)
 "Live long and prosper."
 "KDlTtXchhZTGufMYmOYGS4HffxPSUrfmqCHXaI9wOGY"
 "9hH0vgRfYgPnAHOd8stkvw")

(test-content-encrypt/decrypt
 (jwe-header-builder
  (alg 'RSA-OAEP)
  (enc 'A256GCM))
 #vu8(227 197 117 252 2 219 233 68 180 225 77 219)
 #vu8(177 161 244 128 84 143 225 115 63 180 3 255 107 154
      212 246 138 7 110 91 112 46 34 105 47 130 203 46 122
      234 64 252)
 "The true sign of intelligence is not knowledge but imagination."
 "5eym8TW_c8SuK0ltJ3rpYIzOeDQz7TALvtu6UG9oMo4vpzs9tX_EFShS8iB7j6jiSdiwkIr3ajwQzaBtQD_A"
 "XFBoMYUZodetZdvTiFvSkQ")
(test-end)


  
