(define ed25519ph-signer (make-eddsa-signer ed25519ph-scheme))
(define ed25519ph-verifier (make-eddsa-verifier ed25519ph-scheme))

(define test-ed25519ph
  (make-test-eddsa Ed25519ph ed25519ph-signer ed25519ph-verifier))

(test-ed25519ph
 #x833fe62409237b9d62ec77587520911e9a759cec1d19755b7da901b96dca3d42
 #xec172b93ad5e563bf4932c70e1245034c35467ef2efd4d64ebf819683467e2bf
 (integer->bytevector #x616263)
 #x98a70222f0b8121aa9d30f813d683f809e462b469c7ff87639499bb94e6dae4131f85042463c2a355a2003d062adf5aaa10b8c61e636062aaad11c2a26083406)
