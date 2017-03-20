(import (rnrs)
	(rfc jwk)
	(srfi :64)
	(rfc x.509)
	(crypto))

(test-begin "RFC - JWK")

;; jwk:ec
(let ((json #(("keys"
	       #(("kty" . "EC")
		 ("crv" . "P-256")
		 ("x"   . "MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4")
		 ("y"   . "4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM")
		 ("use" . "enc")
		 ("kid" . "1"))))))
  (test-assert (jwk-set? (json->jwk-set json)))
  (let ((jwk-set (json->jwk-set json)))
    (test-assert (list? (jwk-set-keys jwk-set)))
    (test-equal 1 (length (jwk-set-keys jwk-set)))
    (let ((jwk (car (jwk-set-keys jwk-set))))
      (test-assert (jwk? jwk))
      (test-equal "enc" (jwk-use jwk))
      (test-equal "1" (jwk-kid jwk))
      (test-equal 'EC (jwk-kty jwk))
      (test-assert (jwk:ec? jwk))
      (test-equal 'P-256 (jwk:ec-crv jwk))
      (test-equal #x30a0424cd21c2944838a2d75c92b37e76ea20d9f00893a3b4eee8a3c0aafec3e
		  (jwk:ec-x jwk))
      (test-equal #xe04b65e92456d9888b52b379bdfbd51ee869ef1f0fc65b6659695b6cce081723
		  (jwk:ec-y jwk))
      (test-assert (public-key? (jwk->public-key jwk)))
      (test-error  (jwk->private-key jwk))
      (test-equal '() (jwk->certificate-chain jwk)))
    (test-equal "jwk-set->json jwk:ec" json (jwk-set->json jwk-set))))

;; jwk:ec-private
(let ((json #(("keys"
	       #(("kty" . "EC")
		 ("crv" . "P-256")
		 ("x"   . "MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4")
		 ("y"   . "4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM")
		 ("d"   . "870MB6gfuTJ4HtUnUvYMyJpr5eUZNP4Bk43bVdj3eAE")
		 ("use" . "enc")
		 ("kid" . "1"))))))
  (test-assert (jwk-set? (json->jwk-set json)))
  (let ((jwk-set (json->jwk-set json)))
    (test-assert (list? (jwk-set-keys jwk-set)))
    (test-equal 1 (length (jwk-set-keys jwk-set)))
    (let ((jwk (car (jwk-set-keys jwk-set))))
      (test-assert (jwk? jwk))
      (test-equal "enc" (jwk-use jwk))
      (test-equal "1" (jwk-kid jwk))
      (test-equal 'EC (jwk-kty jwk))
      (test-assert (jwk:ec? jwk))
      (test-assert (jwk:ec-private? jwk))
      (test-equal 'P-256 (jwk:ec-crv jwk))
      (test-equal #x30a0424cd21c2944838a2d75c92b37e76ea20d9f00893a3b4eee8a3c0aafec3e
		  (jwk:ec-x jwk))
      (test-equal #xe04b65e92456d9888b52b379bdfbd51ee869ef1f0fc65b6659695b6cce081723
		  (jwk:ec-y jwk))
      (test-equal #xf3bd0c07a81fb932781ed52752f60cc89a6be5e51934fe01938ddb55d8f77801
		  (jwk:ec-private-d jwk))
      (test-assert (public-key? (jwk->public-key jwk)))
      (test-assert (private-key?  (jwk->private-key jwk)))
      (test-equal '() (jwk->certificate-chain jwk)))
    (test-equal "jwk-set->json jwk:ec-private" json (jwk-set->json jwk-set))))

;; jwk:rsa
(let ((json #(("keys"
	       #(("kty" . "RSA")
		 ("n"   . "0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw")
		 ("e"   . "AQAB")
		 ("alg" . "RS256")
		 ("kid" . "2011-04-29"))))))
  (test-assert (jwk-set? (json->jwk-set json)))
  (let ((jwk-set (json->jwk-set json)))
    (test-assert (list? (jwk-set-keys jwk-set)))
    (test-equal 1 (length (jwk-set-keys jwk-set)))
    (let ((jwk (car (jwk-set-keys jwk-set))))
      (test-assert (jwk? jwk))
      (test-assert (not (jwk-use jwk)))
      (test-equal "2011-04-29" (jwk-kid jwk))
      (test-equal 'RSA (jwk-kty jwk))
      (test-assert (jwk:rsa? jwk))
      (test-equal #xd2fc7b6a0a1e6c67104aeb8f88b257669b4df679ddad099b5c4a6cd9a88015b5a133bf0b856c7871b6df000b554fceb3c2ed512bb68f145c6e8434752fab52a1cfc124408f79b58a4578c16428855789f7a249e384cb2d9fae2d67fd96fb926c198e077399fdc815c0af097dde5aadeff44de70e827f4878432439bfeeb96068d0474fc50d6d90bf3a98dfaf1040c89c02d692ab3b3c2896609d86fd73b774ce0740647ceeeaa310bd12f985a8eb9f59fdd426cea5b2120f4f2a34bcab764b7e6c54d6840238bcc40587a59e66ed1f33894577635c470af75cf92c20d1da43e1bfc419e222a6f0d0bb358c5e38f9cb050aeafe904814f1ac1aa49cca9ea0ca83
		  (jwk:rsa-n jwk))
      (test-equal #x10001 (jwk:rsa-e jwk))
      (test-assert (public-key? (jwk->public-key jwk)))
      (test-error (jwk->private-key jwk))
      (test-equal '() (jwk->certificate-chain jwk)))
    (test-equal json (jwk-set->json jwk-set))))

;; jwk:rsa-private
(let ((json #(("keys"
	       #(("kty" . "RSA")
		 ("n"   . "0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw")
		 ("e"   . "AQAB")
		 ("d"   . "X4cTteJY_gn4FYPsXB8rdXix5vwsg1FLN5E3EaG6RJoVH-HLLKD9M7dx5oo7GURknchnrRweUkC7hT5fJLM0WbFAKNLWY2vv7B6NqXSzUvxT0_YSfqijwp3RTzlBaCxWp4doFk5N2o8Gy_nHNKroADIkJ46pRUohsXywbReAdYaMwFs9tv8d_cPVY3i07a3t8MN6TNwm0dSawm9v47UiCl3Sk5ZiG7xojPLu4sbg1U2jx4IBTNBznbJSzFHK66jT8bgkuqsk0GjskDJk19Z4qwjwbsnn4j2WBii3RL-Us2lGVkY8fkFzme1z0HbIkfz0Y6mqnOYtqc0X4jfcKoAC8Q")
		 ("alg" . "RS256")
		 ("kid" . "2011-04-29"))))))
  (test-assert (jwk-set? (json->jwk-set json)))
  (let ((jwk-set (json->jwk-set json)))
    (test-assert (list? (jwk-set-keys jwk-set)))
    (test-equal 1 (length (jwk-set-keys jwk-set)))
    (let ((jwk (car (jwk-set-keys jwk-set))))
      (test-assert (jwk? jwk))
      (test-assert (not (jwk-use jwk)))
      (test-equal "2011-04-29" (jwk-kid jwk))
      (test-equal 'RSA (jwk-kty jwk))
      (test-assert (jwk:rsa? jwk))
      (test-assert (jwk:rsa-private? jwk))
      (test-equal #xd2fc7b6a0a1e6c67104aeb8f88b257669b4df679ddad099b5c4a6cd9a88015b5a133bf0b856c7871b6df000b554fceb3c2ed512bb68f145c6e8434752fab52a1cfc124408f79b58a4578c16428855789f7a249e384cb2d9fae2d67fd96fb926c198e077399fdc815c0af097dde5aadeff44de70e827f4878432439bfeeb96068d0474fc50d6d90bf3a98dfaf1040c89c02d692ab3b3c2896609d86fd73b774ce0740647ceeeaa310bd12f985a8eb9f59fdd426cea5b2120f4f2a34bcab764b7e6c54d6840238bcc40587a59e66ed1f33894577635c470af75cf92c20d1da43e1bfc419e222a6f0d0bb358c5e38f9cb050aeafe904814f1ac1aa49cca9ea0ca83
		  (jwk:rsa-n jwk))
      (test-equal #x10001 (jwk:rsa-e jwk))
      (test-equal #x5f8713b5e258fe09f81583ec5c1f2b7578b1e6fc2c83514b37913711a1ba449a151fe1cb2ca0fd33b771e68a3b1944649dc867ad1c1e5240bb853e5f24b33459b14028d2d6636befec1e8da974b352fc53d3f6127ea8a3c29dd14f3941682c56a78768164e4dda8f06cbf9c734aae8003224278ea9454a21b17cb06d178075868cc05b3db6ff1dfdc3d56378b4edadedf0c37a4cdc26d1d49ac26f6fe3b5220a5dd29396621bbc688cf2eee2c6e0d54da3c782014cd0739db252cc51caeba8d3f1b824baab24d068ec903264d7d678ab08f06ec9e7e23d960628b744bf94b3694656463c7e417399ed73d076c891fcf463a9aa9ce62da9cd17e237dc2a8002f1
		  (jwk:rsa-private-d jwk))
      (test-assert (public-key? (jwk->public-key jwk)))
      (test-assert (private-key? (jwk->private-key jwk)))
      (test-equal '() (jwk->certificate-chain jwk)))
    (test-equal json (jwk-set->json jwk-set))))

;; jwk:rsa-crt-private
(let ((json #(("keys"
	       #(("kty" . "RSA")
		 ("n"   . "0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw")
		 ("e"   . "AQAB")
		 ("d"   . "X4cTteJY_gn4FYPsXB8rdXix5vwsg1FLN5E3EaG6RJoVH-HLLKD9M7dx5oo7GURknchnrRweUkC7hT5fJLM0WbFAKNLWY2vv7B6NqXSzUvxT0_YSfqijwp3RTzlBaCxWp4doFk5N2o8Gy_nHNKroADIkJ46pRUohsXywbReAdYaMwFs9tv8d_cPVY3i07a3t8MN6TNwm0dSawm9v47UiCl3Sk5ZiG7xojPLu4sbg1U2jx4IBTNBznbJSzFHK66jT8bgkuqsk0GjskDJk19Z4qwjwbsnn4j2WBii3RL-Us2lGVkY8fkFzme1z0HbIkfz0Y6mqnOYtqc0X4jfcKoAC8Q")
		 ("p"   . "83i-7IvMGXoMXCskv73TKr8637FiO7Z27zv8oj6pbWUQyLPQBQxtPVnwD20R-60eTDmD2ujnMt5PoqMrm8RfmNhVWDtjjMmCMjOpSXicFHj7XOuVIYQyqVWlWEh6dN36GVZYk93N8Bc9vY41xy8B9RzzOGVQzXvNEvn7O0nVbfs")
		 ("q"   . "3dfOR9cuYq-0S-mkFLzgItgMEfFzB2q3hWehMuG0oCuqnb3vobLyumqjVZQO1dIrdwgTnCdpYzBcOfW5r370AFXjiWft_NGEiovonizhKpo9VVS78TzFgxkIdrecRezsZ-1kYd_s1qDbxtkDEgfAITAG9LUnADun4vIcb6yelxk")
		 ("dp"  . "G4sPXkc6Ya9y8oJW9_ILj4xuppu0lzi_H7VTkS8xj5SdX3coE0oimYwxIi2emTAue0UOa5dpgFGyBJ4c8tQ2VF402XRugKDTP8akYhFo5tAA77Qe_NmtuYZc3C3m3I24G2GvR5sSDxUyAN2zq8Lfn9EUms6rY3Ob8YeiKkTiBj0")
		 ("dq"  . "s9lAH9fggBsoFR8Oac2R_E2gw282rT2kGOAhvIllETE1efrA6huUUvMfBcMpn8lqeW6vzznYY5SSQF7pMdC_agI3nG8Ibp1BUb0JUiraRNqUfLhcQb_d9GF4Dh7e74WbRsobRonujTYN1xCaP6TO61jvWrX-L18txXw494Q_cgk")
		 ("qi"  . "GyM_p6JrXySiz1toFgKbWV-JdI3jQ4ypu9rbMWx3rQJBfmt0FoYzgUIZEVFEcOqwemRN81zoDAaa-Bk0KWNGDjJHZDdDmFhW3AN7lI-puxk_mHZGJ11rxyR8O55XLSe3SPmRfKwZI6yU24ZxvQKFYItdldUKGzO6Ia6zTKhAVRU")
		 ("alg" . "RS256")
		 ("kid" . "2011-04-29"))))))
  (test-assert (jwk-set? (json->jwk-set json)))
  (let ((jwk-set (json->jwk-set json)))
    (test-assert (list? (jwk-set-keys jwk-set)))
    (test-equal 1 (length (jwk-set-keys jwk-set)))
    (let ((jwk (car (jwk-set-keys jwk-set))))
      (test-assert (jwk? jwk))
      (test-assert (not (jwk-use jwk)))
      (test-equal "2011-04-29" (jwk-kid jwk))
      (test-equal 'RSA (jwk-kty jwk))
      (test-assert (jwk:rsa? jwk))
      (test-assert (jwk:rsa-private? jwk))
      (test-assert (jwk:rsa-crt-private? jwk))
      (test-equal #xd2fc7b6a0a1e6c67104aeb8f88b257669b4df679ddad099b5c4a6cd9a88015b5a133bf0b856c7871b6df000b554fceb3c2ed512bb68f145c6e8434752fab52a1cfc124408f79b58a4578c16428855789f7a249e384cb2d9fae2d67fd96fb926c198e077399fdc815c0af097dde5aadeff44de70e827f4878432439bfeeb96068d0474fc50d6d90bf3a98dfaf1040c89c02d692ab3b3c2896609d86fd73b774ce0740647ceeeaa310bd12f985a8eb9f59fdd426cea5b2120f4f2a34bcab764b7e6c54d6840238bcc40587a59e66ed1f33894577635c470af75cf92c20d1da43e1bfc419e222a6f0d0bb358c5e38f9cb050aeafe904814f1ac1aa49cca9ea0ca83
		  (jwk:rsa-n jwk))
      (test-equal #x10001 (jwk:rsa-e jwk))
      (test-equal #x5f8713b5e258fe09f81583ec5c1f2b7578b1e6fc2c83514b37913711a1ba449a151fe1cb2ca0fd33b771e68a3b1944649dc867ad1c1e5240bb853e5f24b33459b14028d2d6636befec1e8da974b352fc53d3f6127ea8a3c29dd14f3941682c56a78768164e4dda8f06cbf9c734aae8003224278ea9454a21b17cb06d178075868cc05b3db6ff1dfdc3d56378b4edadedf0c37a4cdc26d1d49ac26f6fe3b5220a5dd29396621bbc688cf2eee2c6e0d54da3c782014cd0739db252cc51caeba8d3f1b824baab24d068ec903264d7d678ab08f06ec9e7e23d960628b744bf94b3694656463c7e417399ed73d076c891fcf463a9aa9ce62da9cd17e237dc2a8002f1
		  (jwk:rsa-private-d jwk))
      (test-equal #xf378beec8bcc197a0c5c2b24bfbdd32abf3adfb1623bb676ef3bfca23ea96d6510c8b3d0050c6d3d59f00f6d11fbad1e4c3983dae8e732de4fa2a32b9bc45f98d855583b638cc9823233a949789c1478fb5ceb95218432a955a558487a74ddfa19565893ddcdf0173dbd8e35c72f01f51cf3386550cd7bcd12f9fb3b49d56dfb
		  (jwk:rsa-crt-private-p jwk))
      (test-equal #xddd7ce47d72e62afb44be9a414bce022d80c11f173076ab78567a132e1b4a02baa9dbdefa1b2f2ba6aa355940ed5d22b7708139c276963305c39f5b9af7ef40055e38967edfcd1848a8be89e2ce12a9a3d5554bbf13cc583190876b79c45ecec67ed6461dfecd6a0dbc6d9031207c0213006f4b527003ba7e2f21c6fac9e9719
		  (jwk:rsa-crt-private-q jwk))
      (test-equal #x1b8b0f5e473a61af72f28256f7f20b8f8c6ea69bb49738bf1fb553912f318f949d5f7728134a22998c31222d9e99302e7b450e6b97698051b2049e1cf2d436545e34d9746e80a0d33fc6a4621168e6d000efb41efcd9adb9865cdc2de6dc8db81b61af479b120f153200ddb3abc2df9fd1149aceab63739bf187a22a44e2063d
		  (jwk:rsa-crt-private-dp jwk))
      (test-equal #xb3d9401fd7e0801b28151f0e69cd91fc4da0c36f36ad3da418e021bc896511313579fac0ea1b9452f31f05c3299fc96a796eafcf39d8639492405ee931d0bf6a02379c6f086e9d4151bd09522ada44da947cb85c41bfddf461780e1edeef859b46ca1b4689ee8d360dd7109a3fa4ceeb58ef5ab5fe2f5f2dc57c38f7843f7209
		  (jwk:rsa-crt-private-dq jwk))
      (test-equal #x1b233fa7a26b5f24a2cf5b6816029b595f89748de3438ca9bbdadb316c77ad02417e6b7416863381421911514470eab07a644df35ce80c069af819342963460e3247643743985856dc037b948fa9bb193f987646275d6bc7247c3b9e572d27b748f9917cac1923ac94db8671bd0285608b5d95d50a1b33ba21aeb34ca8405515
		  (jwk:rsa-crt-private-qi jwk))
      (test-assert (public-key? (jwk->public-key jwk)))
      (test-assert (private-key? (jwk->private-key jwk)))
      (test-equal '() (jwk->certificate-chain jwk)))
    (test-equal "jwk-set->json jwk:rsa-crt-private "json
		(jwk-set->json jwk-set))))

;; jwk:rsa with x5c
(let ((json #(("keys"
	       #(("kty" . "RSA")
		 ("n"   . "vrjOfz9Ccdgx5nQudyhdoR17V-IubWMeOZCwX_jj0hgAsz2J_pqYW08PLbK_PdiVGKPrqzmDIsLI7sA25VEnHU1uCLNwBuUiCO11_-7dYbsr4iJmG0Qu2j8DsVyT1azpJC_NG84Ty5KKthuCaPod7iI7w0LK9orSMhBEwwZDCxTWq4aYWAchc8t-emd9qOvWtVMDC2BXksRngh6X5bUYLy6AyHKvj-nUy1wgzjYQDwHMTplCoLtU-o-8SNnZ1tmRoGE9uJkBLdh5gFENabWnU5m1ZqZPdwS-qo-meMvVfJb6jJVWRpl2SUtCnYG2C32qvbWbjZ_jBPD5eunqsIo1vQ")
		 ("e"   . "AQAB")
		 ("use" . "sig")
		 ("kid" . "2011-04-29")
		 ("x5c"
		  "MIIDQjCCAiqgAwIBAgIGATz/FuLiMA0GCSqGSIb3DQEBBQUAMGIxCzAJBgNVBAYTAlVTMQswCQYDVQQIEwJDTzEPMA0GA1UEBxMGRGVudmVyMRwwGgYDVQQKExNQaW5nIElkZW50aXR5IENvcnAuMRcwFQYDVQQDEw5CcmlhbiBDYW1wYmVsbDAeFw0xMzAyMjEyMzI5MTVaFw0xODA4MTQyMjI5MTVaMGIxCzAJBgNVBAYTAlVTMQswCQYDVQQIEwJDTzEPMA0GA1UEBxMGRGVudmVyMRwwGgYDVQQKExNQaW5nIElkZW50aXR5IENvcnAuMRcwFQYDVQQDEw5CcmlhbiBDYW1wYmVsbDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAL64zn8/QnHYMeZ0LncoXaEde1fiLm1jHjmQsF/449IYALM9if6amFtPDy2yvz3YlRij66s5gyLCyO7ANuVRJx1NbgizcAblIgjtdf/u3WG7K+IiZhtELto/A7Fck9Ws6SQvzRvOE8uSirYbgmj6He4iO8NCyvaK0jIQRMMGQwsU1quGmFgHIXPLfnpnfajr1rVTAwtgV5LEZ4Iel+W1GC8ugMhyr4/p1MtcIM42EA8BzE6ZQqC7VPqPvEjZ2dbZkaBhPbiZAS3YeYBRDWm1p1OZtWamT3cEvqqPpnjL1XyW+oyVVkaZdklLQp2Btgt9qr21m42f4wTw+Xrp6rCKNb0CAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAh8zGlfSlcI0o3rYDPBB07aXNswb4ECNIKG0CETTUxmXl9KUL+9gGlqCz5iWLOgWsnrcKcY0vXPG9J1r9AqBNTqNgHq2G03X09266X5CpOe1zFo+Owb1zxtp3PehFdfQJ610CDLEaS9V9Rqp17hCyybEpOGVwe8fnk+fbEL2Bo3UPGrpsHzUoaGpDftmWssZkhpBJKVMJyf/RuP2SmmaIzmnw9JiSlYhzo4tpzd5rFXhjRbg4zW9C+2qok+2+qDM1iJ684gPHMIY8aLWrdgQTxkumGmTqgawR+N5MDtdPTEQ0XfIBc2cJEUyMTY5MPvACWpkA6SdS4xSvdXK3IVfOWA=="))))))
  (test-assert (jwk-set? (json->jwk-set json)))
  (let ((jwk-set (json->jwk-set json)))
    (test-assert (list? (jwk-set-keys jwk-set)))
    (test-equal 1 (length (jwk-set-keys jwk-set)))
    (let ((jwk (car (jwk-set-keys jwk-set))))
      (test-assert (jwk? jwk))
      (test-equal "sig" (jwk-use jwk))
      (test-equal "2011-04-29" (jwk-kid jwk))
      (test-equal 'RSA (jwk-kty jwk))
      (test-assert (jwk:rsa? jwk))
      (test-equal #xbeb8ce7f3f4271d831e6742e77285da11d7b57e22e6d631e3990b05ff8e3d21800b33d89fe9a985b4f0f2db2bf3dd89518a3ebab398322c2c8eec036e551271d4d6e08b37006e52208ed75ffeedd61bb2be222661b442eda3f03b15c93d5ace9242fcd1bce13cb928ab61b8268fa1dee223bc342caf68ad2321044c306430b14d6ab869858072173cb7e7a677da8ebd6b553030b605792c467821e97e5b5182f2e80c872af8fe9d4cb5c20ce36100f01cc4e9942a0bb54fa8fbc48d9d9d6d991a0613db899012dd87980510d69b5a75399b566a64f7704beaa8fa678cbd57c96fa8c9556469976494b429d81b60b7daabdb59b8d9fe304f0f97ae9eab08a35bd
		  (jwk:rsa-n jwk))
      (test-equal #x10001 (jwk:rsa-e jwk))

      (test-assert (public-key? (jwk->public-key jwk)))
      (test-error (jwk->private-key jwk))
      (test-equal '(#t) (map x509-certificate? (jwk->certificate-chain jwk))))
    (test-equal "jwk-set->json jwk:rsa x5c "json
		(jwk-set->json jwk-set))))


(test-end)
