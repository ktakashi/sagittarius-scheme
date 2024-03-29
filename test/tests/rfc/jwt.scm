(import (rnrs)
	(rfc jwt)
	(rfc jwk)
	(rfc jwe)
	(rfc jws)
	(rfc uuid)
	(text json)
	(record accessor)
	(except (crypto) make-eddsa-signer make-eddsa-verifier)
	(srfi :19)
	(sagittarius combinators)
	(srfi :64))

(test-begin "JWT")

(define (json-string->json s) (json-read (open-string-input-port s)))
(define jwk
  (json-string->jwk
   "{\"kty\":\"RSA\",
     \"n\":\"sXchDaQebHnPiGvyDOAT4saGEUetSyo9MKLOoWFsueri23bOdgWp4Dy1Wl
           UzewbgBHod5pcM9H95GQRV3JDXboIRROSBigeC5yjU1hGzHHyXss8UDpre
           cbAYxknTcQkhslANGRUZmdTOQ5qTRsLAt6BTYuyvVRdhS8exSZEy_c4gs_
           7svlJJQ4H9_NxsiIoLwAEk7-Q3UXERGYw_75IDrGA84-lA_-Ct4eTlXHBI
           Y2EaV7t7LjJaynVJCpkv4LKjTTAumiGUIuQhrNhZLuF_RJLqHpM2kgWFLU
           7-VTdL1VbC2tejvcI2BlMkEpk1BzBZI0KQB0GaDWFLN-aEAw3vRw\",
     \"e\":\"AQAB\",
     \"d\":\"VFCWOqXr8nvZNyaaJLXdnNPXZKRaWCjkU5Q2egQQpTBMwhprMzWzpR8Sxq
           1OPThh_J6MUD8Z35wky9b8eEO0pwNS8xlh1lOFRRBoNqDIKVOku0aZb-ry
           nq8cxjDTLZQ6Fz7jSjR1Klop-YKaUHc9GsEofQqYruPhzSA-QgajZGPbE_
           0ZaVDJHfyd7UUBUKunFMScbflYAAOYJqVIVwaYR5zWEEceUjNnTNo_CVSj
           -VvXLO5VZfCUAVLgW4dpf1SrtZjSt34YLsRarSb127reG_DUwg9Ch-Kyvj
           T1SkHgUWRVGcyly7uvVGRSDwsXypdrNinPA4jlhoNdizK2zF2CWQ\",
     \"p\":\"9gY2w6I6S6L0juEKsbeDAwpd9WMfgqFoeA9vEyEUuk4kLwBKcoe1x4HG68
           ik918hdDSE9vDQSccA3xXHOAFOPJ8R9EeIAbTi1VwBYnbTp87X-xcPWlEP
           krdoUKW60tgs1aNd_Nnc9LEVVPMS390zbFxt8TN_biaBgelNgbC95sM\",
     \"q\":\"uKlCKvKv_ZJMVcdIs5vVSU_6cPtYI1ljWytExV_skstvRSNi9r66jdd9-y
           BhVfuG4shsp2j7rGnIio901RBeHo6TPKWVVykPu1iYhQXw1jIABfw-MVsN
           -3bQ76WLdt2SDxsHs7q7zPyUyHXmps7ycZ5c72wGkUwNOjYelmkiNS0\",
     \"dp\":\"w0kZbV63cVRvVX6yk3C8cMxo2qCM4Y8nsq1lmMSYhG4EcL6FWbX5h9yuv
           ngs4iLEFk6eALoUS4vIWEwcL4txw9LsWH_zKI-hwoReoP77cOdSL4AVcra
           Hawlkpyd2TWjE5evgbhWtOxnZee3cXJBkAi64Ik6jZxbvk-RR3pEhnCs\",
     \"dq\":\"o_8V14SezckO6CNLKs_btPdFiO9_kC1DsuUTd2LAfIIVeMZ7jn1Gus_Ff
           7B7IVx3p5KuBGOVF8L-qifLb6nQnLysgHDh132NDioZkhH7mI7hPG-PYE_
           odApKdnqECHWw0J-F0JWnUd6D2B_1TvF9mXA2Qx-iGYn8OVV1Bsmp6qU\",
     \"qi\":\"eNho5yRBEBxhGBtQRww9QirZsB66TrfFReG_CcteI1aCneT0ELGhYlRlC
           tUkTRclIfuEPmNsNDPbLoLqqCVznFbvdB7x-Tl-m0l_eFTj2KiqwGqE9PZ
           B9nNTwMVvH3VRRSLWACvPnSiwP8N5Usy-WRXS-V7TbpxIhvepTfE0NNo\"
     }"))

(define verify-jwk
  (json-string->jwk
   "{\"kty\":\"RSA\",
     \"n\":\"ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddx
          HmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMs
          D1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSH
          SXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdV
          MTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8
          NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ\",
     \"e\":\"AQAB\",
     \"d\":\"Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88vLNO97I
          jlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0
          BkTGoVEn2bKVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn
          439X0M_V51gfpRLI9JYanrC4D4qAdGcopV_0ZHHzQlBjudU2QvXt4ehNYT
          CBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLh
          BOneufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ\",
     \"p\":\"4BzEEOtIpmVdVEZNCqS7baC4crd0pqnRH_5IB3jw3bcxGn6QLvnEtfdUdi
          YrqBdss1l58BQ3KhooKeQTa9AB0Hw_Py5PJdTJNPY8cQn7ouZ2KKDcmnPG
          BY5t7yLc1QlQ5xHdwW1VhvKn-nXqhJTBgIPgtldC-KDV5z-y2XDwGUc\",
     \"q\":\"uQPEfgmVtjL0Uyyx88GZFF1fOunH3-7cepKmtH4pxhtCoHqpWmT8YAmZxa
          ewHgHAjLYsp1ZSe7zFYHj7C6ul7TjeLQeZD_YwD66t62wDmpe_HlB-TnBA
          -njbglfIsRLtXlnDzQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdc\",
     \"dp\":\"BwKfV3Akq5_MFZDFZCnW-wzl-CCo83WoZvnLQwCTeDv8uzluRSnm71I3Q
          CLdhrqE2e9YkxvuxdBfpT_PI7Yz-FOKnu1R6HsJeDCjn12Sk3vmAktV2zb
          34MCdy7cpdTh_YVr7tss2u6vneTwrA86rZtu5Mbr1C1XsmvkxHQAdYo0\",
     \"dq\":\"h_96-mK1R_7glhsum81dZxjTnYynPbZpHziZjeeHcXYsXaaMwkOlODsWa
          7I9xXDoRwbKgB719rrmI2oKr6N3Do9U0ajaHF-NKJnwgjMd2w9cjz3_-ky
          NlxAr2v4IKhGNpmM5iIgOS1VZnOZ68m6_pbLBSp3nssTdlqvd0tIiTHU\",
     \"qi\":\"IYd7DHOhrWvxkwPQsRM2tOgrjbcrfvtQJipd-DlcxyVuuM9sQLdgjVk2o
          y26F0EmpScGLq2MowX7fhd_QJQ3ydy5cY7YIBi87w93IKLEdfnbJtoOPLU
          W0ITrJReOgo1cq9SbsxYawBgfp_gh6A5603k2-ZQwVK0JKSHuLFkuQ3U\"
     }"))

(define expected-claims
  (json-string->json
   "{\"iss\":\"joe\",\"exp\":1300819380,\"http://example.com/is_root\":true}"))
(let ()
  (define decryptor (make-rsa-jwe-decryptor jwk))
  (define consumer (jwt-consumer-builder (decryptor decryptor)
					 (claims-validator
					  (compose
					   jwt:exp-required-validator
					   jwt:iss-required-validator
					   (jwt:iss-value-validator "foo" "joe")
					   #;(jwt:exp-validator)))))
  (let ((claims (jwt:consume consumer "eyJhbGciOiJSU0ExXzUiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0.QR1Owv2ug2WyPBnbQrRARTeEk9kDO2w8qDcjiHnSJflSdv1iNqhWXaKH4MqAkQtMoNfABIPJaZm0HaA415sv3aeuBWnD8J-Ui7Ah6cWafs3ZwwFKDFUUsWHSK-IPKxLGTkND09XyjORj_CHAgOPJ-Sd8ONQRnJvWn_hXV1BNMHzUjPyYwEsRhDhzjAD26imasOTsgruobpYGoQcXUwFDn7moXPRfDE8-NoQX7N7ZYMmpUDkR-Cx9obNGwJQ3nM52YCitxoQVPzjbl7WBuB7AohdBoZOdZ24WlN1lVIeh8v1K4krB8xgKvRU8kgFrEn_a1rZgN5TiysnmzTROF869lQ.AxY8DCtDaGlsbGljb3RoZQ.MKOle7UQrG6nSxTLX6Mqwt0orbHvAKeWnDYvpIAeZ72deHxz3roJDXQyhxx0wKaMHDjUEOKIwrtkHthpqEanSBNYHZgmNOV7sln1Eu9g3J8.fiK51VwhsxJ-siBMR-YFiA")))
    (test-assert "JWE as JWT"
		 (json=? expected-claims (jwt-claims->json claims)))))

(let ()  
  (define none-verifier (lambda args #t))
  (define jws-consumer (jwt-consumer-builder (verifier none-verifier)))
  (let ((claims (jwt:consume jws-consumer "eyJhbGciOiJub25lIn0.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.")))
    (test-assert "JWS (insecure) as JWT"
		 (json=? expected-claims (jwt-claims->json claims)))))

(let ()
  (define decryptor (make-rsa-jwe-decryptor jwk))
  (define verifier (make-rsa-jws-verifier verify-jwk))
  (define nested-jwt
    "eyJhbGciOiJSU0ExXzUiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2IiwiY3R5IjoiSldUIn0.g_hEwksO1Ax8Qn7HoN-BVeBoa8FXe0kpyk_XdcSmxvcM5_P296JXXtoHISr_DD_MqewaQSH4dZOQHoUgKLeFly-9RI11TG-_Ge1bZFazBPwKC5lJ6OLANLMd0QSL4fYEb9ERe-epKYE3xb2jfY1AltHqBO-PM6j23Guj2yDKnFv6WO72tteVzm_2n17SBFvhDuR9a2nHTE67pe0XGBUS_TK7ecA-iVq5COeVdJR4U4VZGGlxRGPLRHvolVLEHx6DYyLpw30Ay9R6d68YCLi9FYTq3hIXPK_-dmPlOUlKvPr1GgJzRoeC9G5qCvdcHWsqJGTO_z3Wfo5zsqwkxruxwA.UmVkbW9uZCBXQSA5ODA1Mg.VwHERHPvCNcHHpTjkoigx3_ExK0Qc71RMEParpatm0X_qpg-w8kozSjfNIPPXiTBBLXR65CIPkFqz4l1Ae9w_uowKiwyi9acgVztAi-pSL8GQSXnaamh9kX1mdh3M_TT-FZGQFQsFhu0Z72gJKGdfGE-OE7hS1zuBD5oEUfk0Dmb0VzWEzpxxiSSBbBAzP10l56pPfAtrjEYw-7ygeMkwBl6Z_mLS6w6xUgKlvW6ULmkV-uLC4FUiyKECK4e3WZYKw1bpgIqGYsw2v_grHjszJZ-_I5uM-9RA8ycX9KqPRp9gc6pXmoU_-27ATs9XCvrZXUtK2902AUzqpeEUJYjWWxSNsS-r1TJ1I-FMJ4XyAiGrfmo9hQPcNBYxPz3GQb28Y5CLSQfNgKSGt0A4isp1hBUXBHAndgtcslt7ZoQJaKe_nNJgNliWtWpJ_ebuOpEl8jdhehdccnRMIwAmU1n7SPkmhIl1HlSOpvcvDfhUN5wuqU955vOBvfkBOh5A11UzBuo2WlgZ6hYi9-e3w29bR0C2-pp3jbqxEDw3iWaf2dc5b-LnR0FEYXvI_tYk5rd_J9N0mg0tQ6RbpxNEMNoA9QWk5lgdPvbh9BaO195abQ.AVO9iT5AV4CzvDJCdhSFlQ")

  (define consumer (jwt-consumer-builder (verifier verifier)))
  (define jwe-object (jwe:parse nested-jwt))

  (let ((claims (jwt:consume consumer
		 (utf8->string (jwe:decrypt decryptor jwe-object)))))
    (test-assert "Nested JWT"
		 (json=? expected-claims (jwt-claims->json claims)))))

(let ((claims (json->jwt-claims expected-claims)))
  (test-assert "Claims serialization"
	       (json=? expected-claims
		       (json-string->json
			(jwt-claims->json-string claims))))
  (test-assert "Claims deserialization"
	       (jwt-claims? (json-string->jwt-claims
			     (jwt-claims->json-string claims)))))

(define (jwt-round-trip-test keypair alg claims)
  (define jws-header
    (jws-header-builder
     (alg alg)))
  (define payload (string->utf8 (jwt-claims->json-string claims)))
  (define jws-object (make-jws-object jws-header payload))
  (define signer (private-key->jws-signer (keypair-private keypair)))
  (define jwks
    (make-jwk-set (list (public-key->jwk (keypair-public keypair)))))

  (let* ((jwt-object (jws:sign jws-object signer))
	 (verifier (public-key->jws-verifier (car (jwk-set-keys jwks))))
	 (jwt-consumer (jwt-consumer-builder
			(verifier verifier)
			(claims-validator
			 (compose jwt:iss-required-validator
				  jwt:sub-required-validator
				  jwt:aud-required-validator
				  jwt:exp-required-validator
				  jwt:nbf-required-validator
				  jwt:iat-required-validator
				  jwt:jti-required-validator
				  (jwt:iss-value-validator "Sagittarius Scheme"
							   "Sagittarius")
				  (jwt:sub-value-validator "Use Sagittarius")
				  (jwt:aud-value-validator "Taurus"
							   "All saints")
				  (jwt:nbf-validator)
				  (jwt:exp-validator)
				  (jwt:iat-validator
				   (add-duration (current-time)
						 (make-time time-duration 0 -10))
				   (add-duration (current-time)
						 (make-time time-duration 0 10)))
				  )))))
    (jwt-claims->json (jwt:consume jwt-consumer jwt-object))))
(define (build-claims :key
		      (iss "Sagittarius Scheme")
		      (aud "All saints")
		      (sub "Use Sagittarius")
		      (iat (current-time))
		      (nbf -10)
		      (exp 600)
		      (jti (uuid->string (make-v4-uuid))))
  (define cur (current-time))
  (jwt-claims-builder
   (iss iss)
   (aud aud)
   (sub sub)
   (iat iat)
   (nbf (and nbf (add-duration cur (make-time time-duration 0 nbf))))
   (exp (and exp (add-duration cur (make-time time-duration 0 exp))))
   (jti jti)))
;; RSA
(let ((keypair (generate-key-pair RSA :size 2048)))
  (let ((claims (build-claims)))
    (test-assert "RS256" (json=? (jwt-claims->json claims)
				 (jwt-round-trip-test keypair 'RS256 claims)))
    (test-assert "RS384" (json=? (jwt-claims->json claims)
				 (jwt-round-trip-test keypair 'RS384 claims)))
    (test-assert "RS512" (json=? (jwt-claims->json claims)
				 (jwt-round-trip-test keypair 'RS512 claims)))
    (test-assert "PS256" (json=? (jwt-claims->json claims)
				 (jwt-round-trip-test keypair 'PS256 claims)))
    (test-assert "PS384" (json=? (jwt-claims->json claims)
				 (jwt-round-trip-test keypair 'PS384 claims)))
    (test-assert "PS512" (json=? (jwt-claims->json claims)
				 (jwt-round-trip-test keypair 'PS512 claims))))
  ;; required claims validator test
  (test-error "iss" (jwt-round-trip-test keypair 'RS256 (build-claims :iss #f)))
  (test-error "aud" (jwt-round-trip-test keypair 'RS256 (build-claims :aud #f)))
  (test-error "sub" (jwt-round-trip-test keypair 'RS256 (build-claims :sub #f)))
  (test-error "iat" (jwt-round-trip-test keypair 'RS256 (build-claims :iat #f)))
  (test-error "nbf" (jwt-round-trip-test keypair 'RS256 (build-claims :nbf #f)))
  (test-error "exp" (jwt-round-trip-test keypair 'RS256 (build-claims :exp #f)))
  (test-error "jti" (jwt-round-trip-test keypair 'RS256 (build-claims :jti #f)))
  ;; value check failure
  (test-assert "iss (ok)" (jwt-round-trip-test keypair 'RS256
			   (build-claims :iss "Sagittarius")))
  (test-assert "aud (ok)" (jwt-round-trip-test keypair 'RS256
			   (build-claims :aud "Taurus")))
  (test-error "iss" (jwt-round-trip-test keypair 'RS256
					 (build-claims :iss "Gauche")))
  (test-error "aud" (jwt-round-trip-test keypair 'RS256
					 (build-claims :aud "Left")))
  (test-error "sub" (jwt-round-trip-test keypair 'RS256
		     (build-claims :sub "Don't use Sagittarius")))
  (test-error "iat" (jwt-round-trip-test keypair 'RS256
		     (build-claims :iat (add-duration (current-time) -10))))
  (test-error "nbf" (jwt-round-trip-test keypair 'RS256 (build-claims :nbf 10)))
  (test-assert "nbf (ok)"
	       (jwt-round-trip-test keypair 'RS256 (build-claims :nbf 0)))
  (test-error "exp" (jwt-round-trip-test keypair 'RS256 (build-claims :exp -1)))
  )
;; ES256
(let ((keypair (generate-key-pair ECDSA :ec-parameter NIST-P-256))
      (claims (build-claims)))
  (test-assert "ES256" (json=? (jwt-claims->json claims)
			       (jwt-round-trip-test keypair 'ES256 claims))))
;; ES384
(let ((keypair (generate-key-pair ECDSA :ec-parameter NIST-P-384))
      (claims (build-claims)))
  (test-assert "ES384" (json=? (jwt-claims->json claims)
			       (jwt-round-trip-test keypair 'ES384 claims))))
;; ES512
(let ((keypair (generate-key-pair ECDSA :ec-parameter NIST-P-521))
      (claims (build-claims)))
  (test-assert "ES512" (json=? (jwt-claims->json claims)
			       (jwt-round-trip-test keypair 'ES512 claims))))
;; EdDSA
(let ((keypair (generate-key-pair Ed25519))
      (claims (build-claims)))
  (test-assert "EdDSA" (json=? (jwt-claims->json claims)
			       (jwt-round-trip-test keypair 'EdDSA claims))))
(let ((keypair (generate-key-pair Ed448))
      (claims (build-claims)))
  (test-assert "EdDSA" (json=? (jwt-claims->json claims)
			       (jwt-round-trip-test keypair 'EdDSA claims))))


(define (invalid-signature-test alg keytype . rest)
  (define kp1 (apply generate-key-pair keytype rest))
  (define kp2 (apply generate-key-pair keytype rest))
  (define jws-header
    (jws-header-builder
     (alg alg)))
  (define payload (string->utf8 (jwt-claims->json-string (build-claims))))
  (define signer (private-key->jws-signer (keypair-private kp1)))
  (define verifier (public-key->jws-verifier (keypair-public kp2)))
  (let ((jws-object (jws:sign (make-jws-object jws-header payload) signer))
	(jwt-consumer (jwt-consumer-builder
		       (verifier verifier))))
    (test-error keytype (jwt:consume jwt-consumer jws-object))))

(invalid-signature-test 'RS256 RSA)
(invalid-signature-test 'RS384 RSA)
(invalid-signature-test 'RS512 RSA)
(invalid-signature-test 'PS256 RSA)
(invalid-signature-test 'PS384 RSA)
(invalid-signature-test 'PS512 RSA)
(invalid-signature-test 'ES256 ECDSA :ec-parameter NIST-P-256)
(invalid-signature-test 'ES384 ECDSA :ec-parameter NIST-P-384)
(invalid-signature-test 'ES512 ECDSA :ec-parameter NIST-P-521)
(invalid-signature-test 'EdDSA Ed25519)
(invalid-signature-test 'EdDSA Ed448)

(test-end)
