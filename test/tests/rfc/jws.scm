(import (rnrs)
	(rfc jws)
	(rfc jwk)
	(text json compare)
	(sagittarius crypto keys)
	(rfc base64)
	(rfc pem)
	(rsa pkcs :8)
	(rsa pkcs :10)
	(srfi :64))

(test-begin "JWS")

(test-assert (jws-header? (jws-header-builder (typ 'JWT) (alg 'RS256))))

(test-error "Invalid JWS (0)" (jws:parse "abc.def"))
(test-error "Invalid JWS (1)" (jws:parse "abc...def"))
(test-error "Invalid JWS (2)" (jws:parse "ab\\+c..def"))

(test-assert "No body JWS"
	     (jws-object?
	      (jws:parse "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..d5JVICI5HVHxe9Qn7Q9rEyZzWhQriuSJBXzyh7_gXEM")))
(test-error "Body with maybe-payload"
	    (jws:parse "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.abc.d5JVICI5HVHxe9Qn7Q9rEyZzWhQriuSJBXzyh7_gXEM" #vu8(1 2 3)))

(let ((jws (jws:parse "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..d5JVICI5HVHxe9Qn7Q9rEyZzWhQriuSJBXzyh7_gXEM")))
  (test-equal #vu8() (jws-object-payload jws)))

(let ((jws (jws:parse "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..d5JVICI5HVHxe9Qn7Q9rEyZzWhQriuSJBXzyh7_gXEM" #vu8(1 2 3))))
  (test-equal #vu8(1 2 3) (jws-object-payload jws)))

(define jws-string "eyJhbGciOiJQUzI1NiIsInR5cCI6IkpXVCIsImNyaXQiOlsidXJpOmF1ZCJdLCJ1cmk6YXVkIjoibWUifQ.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.ISbe0I5T6w7mkxxdovu9ozCqg0YyWlNhhI99Z9UaBnB5elYq0df_FJNHMAIKBLRzSW9QdPW6AnFedGD4vWxuGHi_1pN3pl87G2dAVSpT6UQUs1vmodVy60cRGVVA2g4TN9V61bqDHHw74b4Aie1eOR4CkPx7tr4CrAFhRPrJWjnVgksSTE1pvjmxG7q2AJr3xF4gr2MgGKQSJquO5nhtoiSL_AgM0p8nV-Yl0QYKwRXxMPxicKD_zjSwT2aMDdIoDYm5Uze0Fb__4W-ZOkU5dHf_lQA-PSkPOfc-HXyDvOpUmWhb14hVwPZUYivMZM3yxHHiVQ5QAWNLNku_VYZC_Q")

(let ((jws (jws:parse jws-string)))
  (test-assert (jws-header? (jws-object-header jws)))
  (test-assert (json=?
		'#(("typ" . "JWT")
		   ("alg" . "PS256")
		   ("crit" "uri:aud")
		   ("uri:aud" . "me"))
		(jws-header->json (jws-object-header jws))))
  ;; for debug purpose
  (test-assert (string? (jws-object->string jws)))
  (test-equal "jws:serialize" jws-string (jws:serialize jws))
  (test-equal "jws:serialize without payload"
	      "eyJhbGciOiJQUzI1NiIsInR5cCI6IkpXVCIsImNyaXQiOlsidXJpOmF1ZCJdLCJ1cmk6YXVkIjoibWUifQ..ISbe0I5T6w7mkxxdovu9ozCqg0YyWlNhhI99Z9UaBnB5elYq0df_FJNHMAIKBLRzSW9QdPW6AnFedGD4vWxuGHi_1pN3pl87G2dAVSpT6UQUs1vmodVy60cRGVVA2g4TN9V61bqDHHw74b4Aie1eOR4CkPx7tr4CrAFhRPrJWjnVgksSTE1pvjmxG7q2AJr3xF4gr2MgGKQSJquO5nhtoiSL_AgM0p8nV-Yl0QYKwRXxMPxicKD_zjSwT2aMDdIoDYm5Uze0Fb__4W-ZOkU5dHf_lQA-PSkPOfc-HXyDvOpUmWhb14hVwPZUYivMZM3yxHHiVQ5QAWNLNku_VYZC_Q"
	      (jws:serialize jws #t)))

(define (pem->public-key pem-string)
  (let-values (((param content) (parse-pem-string pem-string)))
    (let* ((spki (import-public-key PKCS10 content)))
      (subject-public-key-info->public-key spki))))
(define (pem->rsa-private-key pem-string)
  (let-values (((param content) (parse-pem-string pem-string)))
    (import-private-key *key:rsa* content)))

(define (test-jws jws-string public-key private-key)
  (define get-verifier public-key->jws-verifier)
  (define get-signer private-key->jws-signer)
  (let ((jws (jws:parse jws-string))
	(verifier (get-verifier public-key)))
    (test-assert (list 'jws:verify (jws-header-alg (jws-object-header jws)))
		 (jws:verify jws verifier))
    (let* ((signer (get-signer private-key))
	   (re-signed-jws (jws:sign jws signer)))
      (test-assert (list 'jws:sign (jws-header-alg (jws-object-header jws)))
		   (not (eq? jws re-signed-jws))) ;; different object
      (test-assert (list 're: 'jws:verify
			 (jws-header-alg (jws-object-header jws)))
		   (jws:verify re-signed-jws verifier)))))

(define rsa-public-key
  (pem->public-key
   "-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnzyis1ZjfNB0bBgKFMSv
vkTtwlvBsaJq7S5wA+kzeVOVpVWwkWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHc
aT92whREFpLv9cj5lTeJSibyr/Mrm/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIy
tvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0
e+lf4s4OxQawWD79J9/5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWb
V6L11BWkpzGXSW4Hv43qa+GSYOD2QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9
MwIDAQAB
-----END PUBLIC KEY-----"))
(define rsa-private-key
  (pem->rsa-private-key
   "-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEAnzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA+kzeVOVpVWw
kWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr/Mr
m/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEi
NQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e+lf4s4OxQawWD79J9/5d3Ry0vbV
3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa+GSYOD2
QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9MwIDAQABAoIBACiARq2wkltjtcjs
kFvZ7w1JAORHbEufEO1Eu27zOIlqbgyAcAl7q+/1bip4Z/x1IVES84/yTaM8p0go
amMhvgry/mS8vNi1BN2SAZEnb/7xSxbflb70bX9RHLJqKnp5GZe2jexw+wyXlwaM
+bclUCrh9e1ltH7IvUrRrQnFJfh+is1fRon9Co9Li0GwoN0x0byrrngU8Ak3Y6D9
D8GjQA4Elm94ST3izJv8iCOLSDBmzsPsXfcCUZfmTfZ5DbUDMbMxRnSo3nQeoKGC
0Lj9FkWcfmLcpGlSXTO+Ww1L7EGq+PT3NtRae1FZPwjddQ1/4V905kyQFLamAA5Y
lSpE2wkCgYEAy1OPLQcZt4NQnQzPz2SBJqQN2P5u3vXl+zNVKP8w4eBv0vWuJJF+
hkGNnSxXQrTkvDOIUddSKOzHHgSg4nY6K02ecyT0PPm/UZvtRpWrnBjcEVtHEJNp
bU9pLD5iZ0J9sbzPU/LxPmuAP2Bs8JmTn6aFRspFrP7W0s1Nmk2jsm0CgYEAyH0X
+jpoqxj4efZfkUrg5GbSEhf+dZglf0tTOA5bVg8IYwtmNk/pniLG/zI7c+GlTc9B
BwfMr59EzBq/eFMI7+LgXaVUsM/sS4Ry+yeK6SJx/otIMWtDfqxsLD8CPMCRvecC
2Pip4uSgrl0MOebl9XKp57GoaUWRWRHqwV4Y6h8CgYAZhI4mh4qZtnhKjY4TKDjx
QYufXSdLAi9v3FxmvchDwOgn4L+PRVdMwDNms2bsL0m5uPn104EzM6w1vzz1zwKz
5pTpPI0OjgWN13Tq8+PKvm/4Ga2MjgOgPWQkslulO/oMcXbPwWC3hcRdr9tcQtn9
Imf9n2spL/6EDFId+Hp/7QKBgAqlWdiXsWckdE1Fn91/NGHsc8syKvjjk1onDcw0
NvVi5vcba9oGdElJX3e9mxqUKMrw7msJJv1MX8LWyMQC5L6YNYHDfbPF1q5L4i8j
8mRex97UVokJQRRA452V2vCO6S5ETgpnad36de3MUxHgCOX3qL382Qx9/THVmbma
3YfRAoGAUxL/Eu5yvMK8SAt/dJK6FedngcM3JEFNplmtLYVLWhkIlNRGDwkg3I5K
y18Ae9n7dHVueyslrb6weq7dTkYDi3iOYRW8HRkIQh06wEdbxt0shTzAJvvCQfrB
jg/3747WSsf/zBTcHihTRBdAv6OmdhV4/dD5YBfLAkLrd+mX7iE=
-----END RSA PRIVATE KEY-----"))

(let ()
  (define jws-header (jws-header-builder (typ 'JWT) (alg 'RS256)))
  (define payload (string->utf8 "aardvark"))
  (define jws-object (make-jws-object jws-header payload))
  (define rsa-signer (make-rsa-jws-signer rsa-private-key))
  (let ((jws (jws:sign jws-object rsa-signer)))
    (define rsa-verifier (make-rsa-jws-verifier rsa-public-key))
    (test-assert (jws:verify jws rsa-verifier))))

(test-jws "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.POstGetfAytaZS82wHcjoTyoqhMyxXiWdR7Nn7A29DNSl0EiXLdwJ6xC6AfgZWF1bOsS_TuYI3OG85AmiExREkrS6tDfTQ2B3WXlrr-wp5AokiRbz3_oB4OxG-W9KcEEbDRcZc0nH3L7LzYptiy1PtAylQGxHTWZXtGz4ht0bAecBgmpdgXMguEIcoqPJ1n3pIWk_dUZegpqx0Lka21H6XxUTxiy8OcaarA8zdnPUnV6AmNP3ecFawIFYdvJB_cm-GvpCSbr8G8y_Mllj8f4x9nBH8pQux89_6gUY618iYv7tuPWBFfEbLxtF2pZS6YC1aSfLQxeNe8djT9YjpvRZA"
	  rsa-public-key
	  rsa-private-key)
(test-jws "eyJhbGciOiJSUzM4NCIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.D4kXa3UspFjRA9ys5tsD4YDyxxam3l_XnOb3hMEdPDTfSLRHPv4HPwxvin-pIkEmfJshXPSK7O4zqSXWAXFO52X-upJjFc_gpGDswctNWpOJeXe1xBgJ--VuGDzUQCqkr9UBpN-Q7TE5u9cgIVisekSFSH5Ax6aXQC9vCO5LooNFx_WnbTLNZz7FUia9vyJ544kLB7UcacL-_idgRNIWPdd_d1vvnNGkknIMarRjCsjAEf6p5JGhYZ8_C18g-9DsfokfUfSpKgBR23R8v8ZAAmPPPiJ6MZXkefqE7p3jRbA--58z5TlHmH9nTB1DYE2872RYvyzG3LoQ-2s93VaVuw"
	  rsa-public-key
	  rsa-private-key)
(test-jws "eyJhbGciOiJSUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.JlX3gXGyClTBFciHhknWrjo7SKqyJ5iBO0n-3S2_I7cIgfaZAeRDJ3SQEbaPxVC7X8aqGCOM-pQOjZPKUJN8DMFrlHTOdqMs0TwQ2PRBmVAxXTSOZOoEhD4ZNCHohYoyfoDhJDP4Qye_FCqu6POJzg0Jcun4d3KW04QTiGxv2PkYqmB7nHxYuJdnqE3704hIS56pc_8q6AW0WIT0W-nIvwzaSbtBU9RgaC7ZpBD2LiNE265UBIFraMDF8IAFw9itZSUCTKg1Q-q27NwwBZNGYStMdIBDor2Bsq5ge51EkWajzZ7ALisVp-bskzUsqUf77ejqX_CBAqkNdH1Zebn93A"
	  rsa-public-key
	  rsa-private-key)
(test-jws "eyJhbGciOiJQUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.V5y-D0tNXTauU-1zkxyfteAWnGxCLcjNG1koLyVYhdheQYOx1rrGjoskkPKk-reTd3wJIon22gqFyTQyro1jyfhiAsnrqwDy3SigxniIsR3nIcdyaoj5iez_w0xQJuTxWXCKpv7IYpd-SRD6P5wCcGgnCK0hCe0MazP-c2WyflElY8rHzfsVS6jyvyEI8wIeHati-Ab93zgXp2rXxGE2mRWnAZL6-UFgULJtuirbheVkZA7qTHKdBUhU5ilY93SiJ4cP3hBN9mluomXqPFfxRTzvT8WbOfbLuq4-8u4SveB1lMxSMm2J4Bth6zWGKDDVxfVMCvUov0jk4_Zq6QmLng"
	  rsa-public-key
	  rsa-private-key)
(test-jws "eyJhbGciOiJQUzM4NCIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.MqF1AKsJkijKnfqEI3VA1OnzAL2S4eIpAuievMgD3tEFyFMU67gCbg-fxsc5dLrxNwdZEXs9h0kkicJZ70mp6p5vdv-j2ycDKBWg05Un4OhEl7lYcdIsCsB8QUPmstF-lQWnNqnq3wra1GynJrOXDL27qIaJnnQKlXuayFntBF0j-82jpuVdMaSXvk3OGaOM-7rCRsBcSPmocaAO-uWJEGPw_OWVaC5RRdWDroPi4YL4lTkDEC-KEvVkqCnFm_40C-T_siXquh5FVbpJjb3W2_YvcqfDRj44TsRrpVhk6ohsHMNeUad_cxnFnpolIKnaXq_COv35e9EgeQIPAbgIeg"
	  rsa-public-key
	  rsa-private-key)

#!read-macro=sagittarius/bv-string
(test-jws "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
	  #*"your-256-bit-secret"
	  #*"your-256-bit-secret")
(test-jws "eyJhbGciOiJIUzM4NCIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.bQTnz6AuMJvmXXQsVPrxeQNvzDkimo7VNXxHeSBfClLufmCVZRUuyTwJF311JHuh"
	  #*"your-384-bit-secret"
	  #*"your-384-bit-secret")
(test-jws "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.VFb0qJ1LRg_4ujbZoRMXnVkUgiuKq5KxWqNdbKq_G9Vvz-S1zZa9LPxtHWKa64zDl2ofkT8F6jBt_K4riU-fPg"
	  #*"your-512-bit-secret"
	  #*"your-512-bit-secret")

;; apparently it's just PKCS8 format
(define (pem->pkcs8-private-key pem-string)
  (let-values (((param content) (parse-pem-string pem-string)))
    (let* ((spki (import-private-key PKCS8 content)))
      (private-key-info->private-key spki))))

(define ec-public-key-p256
  (pem->public-key
   "-----BEGIN PUBLIC KEY-----
MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEEVs/o5+uQbTjL3chynL4wXgUg2R9
q9UU8I5mEovUf86QZ7kOBIjJwqnzD1omageEHWwHdBO6B+dFabmdT9POxg==
-----END PUBLIC KEY-----"))
(define ec-private-key-p256
  (pem->pkcs8-private-key
  "-----BEGIN PRIVATE KEY-----
MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgevZzL1gdAFr88hb2
OF/2NxApJCzGCEDdfSp6VQO30hyhRANCAAQRWz+jn65BtOMvdyHKcvjBeBSDZH2r
1RTwjmYSi9R/zpBnuQ4EiMnCqfMPWiZqB4QdbAd0E7oH50VpuZ1P087G
-----END PRIVATE KEY-----"))

(test-jws "eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.tyh-VfuzIxCyGYDlkBA7DfyjrqmSHu6pQ2hoZuFqUSLPNY2N0mpHb3nk5K17HWP_3cYHBw7AhHale5wky6-sVA"
	  ec-public-key-p256
	  ec-private-key-p256)

(define (pem->ecdsa-private-key pem-string)
  (let-values (((param content) (parse-pem-string pem-string)))
    (import-private-key *key:ecdsa* content)))

(define ec-public-key-p384
  (pem->public-key
   "-----BEGIN PUBLIC KEY-----
MHYwEAYHKoZIzj0CAQYFK4EEACIDYgAEC1uWSXj2czCDwMTLWV5BFmwxdM6PX9p+
Pk9Yf9rIf374m5XP1U8q79dBhLSIuaojsvOT39UUcPJROSD1FqYLued0rXiooIii
1D3jaW6pmGVJFhodzC31cy5sfOYotrzF
-----END PUBLIC KEY-----"))
(define ec-private-key-p384
  (pem->ecdsa-private-key
  "-----BEGIN EC PRIVATE KEY-----
MIGkAgEBBDCAHpFQ62QnGCEvYh/pE9QmR1C9aLcDItRbslbmhen/h1tt8AyMhske
enT+rAyyPhGgBwYFK4EEACKhZANiAAQLW5ZJePZzMIPAxMtZXkEWbDF0zo9f2n4+
T1h/2sh/fviblc/VTyrv10GEtIi5qiOy85Pf1RRw8lE5IPUWpgu553SteKigiKLU
PeNpbqmYZUkWGh3MLfVzLmx85ii2vMU=
-----END EC PRIVATE KEY-----"))

(test-jws "eyJhbGciOiJFUzM4NCIsInR5cCI6IkpXVCIsImtpZCI6ImlUcVhYSTB6YkFuSkNLRGFvYmZoa00xZi02ck1TcFRmeVpNUnBfMnRLSTgifQ.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.cJOP_w-hBqnyTsBm3T6lOE5WpcHaAkLuQGAs1QO-lg2eWs8yyGW8p9WagGjxgvx7h9X72H7pXmXqej3GdlVbFmhuzj45A9SXDOAHZ7bJXwM1VidcPi7ZcrsMSCtP1hiN"
	  ec-public-key-p384
	  ec-private-key-p384)

(define ec-public-key-p512
  (pem->public-key
   "-----BEGIN PUBLIC KEY-----
MIGbMBAGByqGSM49AgEGBSuBBAAjA4GGAAQBgc4HZz+/fBbC7lmEww0AO3NK9wVZ
PDZ0VEnsaUFLEYpTzb90nITtJUcPUbvOsdZIZ1Q8fnbquAYgxXL5UgHMoywAib47
6MkyyYgPk0BXZq3mq4zImTRNuaU9slj9TVJ3ScT3L1bXwVuPJDzpr5GOFpaj+WwM
Al8G7CqwoJOsW7Kddns=
-----END PUBLIC KEY-----"))
(define ec-private-key-p512
  (pem->ecdsa-private-key
  "-----BEGIN EC PRIVATE KEY-----
MIHcAgEBBEIBiyAa7aRHFDCh2qga9sTUGINE5jHAFnmM8xWeT/uni5I4tNqhV5Xx
0pDrmCV9mbroFtfEa0XVfKuMAxxfZ6LM/yKgBwYFK4EEACOhgYkDgYYABAGBzgdn
P798FsLuWYTDDQA7c0r3BVk8NnRUSexpQUsRilPNv3SchO0lRw9Ru86x1khnVDx+
duq4BiDFcvlSAcyjLACJvjvoyTLJiA+TQFdmrearjMiZNE25pT2yWP1NUndJxPcv
VtfBW48kPOmvkY4WlqP5bAwCXwbsKrCgk6xbsp12ew==
-----END EC PRIVATE KEY-----"))

(test-jws "eyJhbGciOiJFUzUxMiIsInR5cCI6IkpXVCIsImtpZCI6InhaRGZacHJ5NFA5dlpQWnlHMmZOQlJqLTdMejVvbVZkbTd0SG9DZ1NOZlkifQ.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.AP_CIMClixc5-BFflmjyh_bRrkloEvwzn8IaWJFfMz13X76PGWF0XFuhjJUjp7EYnSAgtjJ-7iJG4IP7w3zGTBk_AUdmvRCiWp5YAe8S_Hcs8e3gkeYoOxiXFZlSSAx0GfwW1cZ0r67mwGtso1I3VXGkSjH5J0Rk6809bn25GoGRjOPu"
	  ec-public-key-p512
	  ec-private-key-p512)

(define ed25519-okp
  (json->jwk #(("kty" . "OKP")
	       ("crv" . "Ed25519")
	       ("d" . "nWGxne_9WmC6hEr0kuwsxERJxWl7MmkZcDusAxyuf2A")
	       ("x" . "11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo"))))
(define ed25519-private-key (jwk->private-key ed25519-okp))
(define ed25519-public-key (jwk->public-key ed25519-okp))

(test-jws "eyJhbGciOiJFZERTQSJ9.RXhhbXBsZSBvZiBFZDI1NTE5IHNpZ25pbmc.hgyY0il_MGCjP0JzlnLWG1PPOt7-09PGcvMg3AIbQR6dWbhijcNR4ki4iylGjg5BhVsPt9g7sVvpAr_MuM0KAg"
	  ed25519-public-key
	  ed25519-private-key)

;; RFC 7797
(let* ((jwk (json->jwk #(("kty" . "oct")
			 ("k" . "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow"))))
       (jws (make-jws-object (jws-header-builder (alg 'HS256))
			     (base64url-decode-string "JC4wMg" :transcoder #f)))
       (jws-b64 (jws:parse
		 "eyJhbGciOiJIUzI1NiIsImI2NCI6ZmFsc2UsImNyaXQiOlsiYjY0Il19..A5dxf2s96_n5FLueVuW1Z_vh161FwXZC4YLPff6dmDY"
		 (string->utf8 "$.02")))
       (signer (private-key->jws-signer jwk)))
  (test-equal "eyJhbGciOiJIUzI1NiJ9.JC4wMg.5mvfOroL-g7HyqJoozehmsaqmvTYGEq5jTI1gVvoEoQ"
	      (jws:serialize (jws:sign jws signer)))
  (test-equal "eyJhbGciOiJIUzI1NiIsImI2NCI6ZmFsc2UsImNyaXQiOlsiYjY0Il19..A5dxf2s96_n5FLueVuW1Z_vh161FwXZC4YLPff6dmDY"
	      (jws:serialize jws-b64)))
      

(test-end)
