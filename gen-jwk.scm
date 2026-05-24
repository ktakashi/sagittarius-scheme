(import (rnrs)
	(rfc jwk)
	(sagittarius crypto keys))

(define kp (generate-key-pair *key:ecdsa*))

(define jwk-config (jwk-config-builder (kid "e2e-test-key-A")))

(define jwk-priv (private-key->jwk (key-pair-private kp) jwk-config))
(define jwk-pub (public-key->jwk (key-pair-public kp) jwk-config))

(print (jwk->json-string jwk-priv))
(print (jwk->json-string jwk-pub))
