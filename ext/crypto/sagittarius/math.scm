;;; -*- mode: scheme; coding: utf-8 -*-
(load-dynamic-library "sagittarius--math")
(library (sagittarius math)
    (export prng?
	    read-random-bytes
	    make-pseudo-random
	    make-secure-random
	    %random-seed-set!
	    make-custom-prng
	    Yarrow Fortuna RC4 SOBER-128
	    ;; hash
	    hash-algorithm?
	    make-hash-algorithm
	    hash-init!
	    hash-process!
	    hash-done!
	    hash-size
	    hash-oid
	    WHIRLPOOL SHA-512 SHA-384 RIPEMD-320
	    SHA-256 RIPEMD-256 SHA-224 SHA-224   
	    Tiger-192 SHA-1 RIPEMD-160 RIPEMD-128
	    MD5 MD4 MD2
	    )
    (import (core) (sagittarius math impl))
  ;; pseudo-random type
  (define Yarrow "yarrow")
  (define Fortuna "fortuna")
  (define RC4 "rc4")
  (define SOBER-128 "sober128")

  ;; hash-algorithm type
  (define WHIRLPOOL  "whirlpool")
  (define SHA-512    "sha512")
  (define SHA-384    "sha384")
  (define RIPEMD-320 "rmd320")
  (define SHA-256    "sha256")
  (define RIPEMD-256 "rmd256")
  (define SHA-224    "sha224")
  (define SHA-224    "sha224")
  (define Tiger-192  "tiger")
  (define SHA-1      "sha1")
  (define RIPEMD-160 "rmd160")
  (define RIPEMD-128 "rmd128")
  (define MD5        "md5")
  (define MD4        "md4")
  (define MD2        "md2")
)
