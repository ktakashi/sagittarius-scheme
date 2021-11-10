(import (rnrs)
	(rfc jwt)
	(rfc jwk)
	(rfc jwe)
	(text json)
	(record accessor)
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

(define expected-claims
  (json-string->json
   "{\"iss\":\"joe\",\"exp\":1300819380,\"http://example.com/is_root\":true}"))

(define decryptor (make-rsa-decryptor jwk))
(define consumer (jwt-consumer-builder (decryptor decryptor)
				       (claims-validator
					(compose
					 jwt:exp-required-validator
					 jwt:iss-required-validator
					 (jwt:iss-value-validator "foo" "joe")
					 #;(jwt:exp-validator)))))
(let ((claims (jwt:consume consumer "eyJhbGciOiJSU0ExXzUiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0.QR1Owv2ug2WyPBnbQrRARTeEk9kDO2w8qDcjiHnSJflSdv1iNqhWXaKH4MqAkQtMoNfABIPJaZm0HaA415sv3aeuBWnD8J-Ui7Ah6cWafs3ZwwFKDFUUsWHSK-IPKxLGTkND09XyjORj_CHAgOPJ-Sd8ONQRnJvWn_hXV1BNMHzUjPyYwEsRhDhzjAD26imasOTsgruobpYGoQcXUwFDn7moXPRfDE8-NoQX7N7ZYMmpUDkR-Cx9obNGwJQ3nM52YCitxoQVPzjbl7WBuB7AohdBoZOdZ24WlN1lVIeh8v1K4krB8xgKvRU8kgFrEn_a1rZgN5TiysnmzTROF869lQ.AxY8DCtDaGlsbGljb3RoZQ.MKOle7UQrG6nSxTLX6Mqwt0orbHvAKeWnDYvpIAeZ72deHxz3roJDXQyhxx0wKaMHDjUEOKIwrtkHthpqEanSBNYHZgmNOV7sln1Eu9g3J8.fiK51VwhsxJ-siBMR-YFiA")))
  (test-assert "JWE as JWT"
	       (json=? expected-claims (jwt-claims->json claims))))

(define none-verifier (lambda args #t))
(define jws-consumer (jwt-consumer-builder (verifier none-verifier)))
(let ((claims (jwt:consume jws-consumer "eyJhbGciOiJub25lIn0.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.")))
  (test-assert "JWS (insecure) as JWT"
	       (json=? expected-claims (jwt-claims->json claims))))

(let ((claims (json->jwt-claims expected-claims)))
  (test-assert "Claims serialization"
	       (json=? expected-claims
		       (json-string->json
			(jwt-claims->json-string claims))))
  (test-assert "Claims deserialization"
	       (jwt-claims? (json-string->jwt-claims
			     (jwt-claims->json-string claims)))))

(test-end)
