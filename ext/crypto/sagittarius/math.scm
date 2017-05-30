;;; -*- mode: scheme; coding: utf-8 -*-
(library (sagittarius math)
    (export prng?
	    pseudo-random?
	    secure-random?
	    read-random-bytes
	    read-random-bytes!
	    make-pseudo-random
	    make-secure-random
	    %random-seed-set!
	    read-sys-random
	    Yarrow Fortuna RC4 SOBER-128 System ChaCha20
	    ;; hash
	    hash-algorithm?
	    make-hash-algorithm
	    hash-init!
	    hash-process!
	    hash-done!
	    hash-size
	    hash-block-size
	    hash-oid
	    WHIRLPOOL 
	    SHA-224 SHA-256 SHA-384 SHA-512 SHA-512/224 SHA-512/256
	    SHA-3-224 SHA-3-256 SHA-3-384 SHA-3-512
	    RIPEMD-320 RIPEMD-256   
	    Tiger-192 SHA-1 RIPEMD-160 RIPEMD-128
	    MD5 MD4 MD2
	    BLAKE2s-128 BLAKE2s-160 BLAKE2s-224 BLAKE2s-256
	    BLAKE2b-160 BLAKE2b-256 BLAKE2b-384 BLAKE2b-512
	    ;; clos
	    <prng> <user-prng> <builtin-prng> <secure-random>
	    <hash-algorithm> <user-hash-algorithm>
	    <builtin-hash-algorithm>
	    prng-state
	    ;; register
	    register-hash
	    lookup-hash
	    register-prng
	    lookup-prng
	    )
    (import (sagittarius) (sagittarius dynamic-module))
  (load-dynamic-module "sagittarius--math")
  ;; pseudo-random type
  (define-constant Yarrow "yarrow")
  (define-constant Fortuna "fortuna")
  (define-constant RC4 "rc4")
  (define-constant SOBER-128 "sober128")
  (define-constant System "sprng")
  (define-constant ChaCha20 "chacha20")

  ;; hash-algorithm type
  (define-constant WHIRLPOOL  "whirlpool")
  (define-constant RIPEMD-320 "rmd320")
  (define-constant RIPEMD-256 "rmd256")
  ;; SHA1
  (define-constant SHA-1      "sha1")
  ;; SHA2
  (define-constant SHA-224    "sha224")
  (define-constant SHA-256    "sha256")
  (define-constant SHA-384    "sha384")
  (define-constant SHA-512    "sha512")
  (define-constant SHA-512/224 "sha512-224")
  (define-constant SHA-512/256 "sha512-256")
  ;; SHA3
  (define-constant SHA-3-224   "sha3-224")
  (define-constant SHA-3-256   "sha3-256")
  (define-constant SHA-3-384   "sha3-384")
  (define-constant SHA-3-512   "sha3-512")
  
  (define-constant Tiger-192  "tiger")

  (define-constant RIPEMD-160 "rmd160")
  (define-constant RIPEMD-128 "rmd128")
  (define-constant MD5        "md5")
  (define-constant MD4        "md4")
  (define-constant MD2        "md2")

  (define-constant BLAKE2s-128 "blake2s-128")
  (define-constant BLAKE2s-160 "blake2s-160")
  (define-constant BLAKE2s-224 "blake2s-224")
  (define-constant BLAKE2s-256 "blake2s-256")
  (define-constant BLAKE2b-160 "blake2b-160")
  (define-constant BLAKE2b-256 "blake2b-256")
  (define-constant BLAKE2b-384 "blake2b-384")
  (define-constant BLAKE2b-512 "blake2b-512")
)
