;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; tomcrypt.scm -
;;;
;;; This library is a wrapper for tomcrypt.

;; Keep this library just as minimum as possible, in case I want to
;; build a new style APIs, again.
;; Current, or old, implementation of (crypto) and (math) is very
;; inconvenient to do change the style, but if it's just a simple
;; wrapper, it's very easy to do it in the Scheme world.
#!nounbound
(library (sagittarius crypto tomcrypt)
    (export mode-key?
	    find-cipher

	    cipher-descriptor?
	    cipher-descriptor-name
	    cipher-descriptor-block-length
	    cipher-descriptor-min-key-length cipher-descriptor-max-key-length
	    cipher-descriptor-default-rounds
	    cipher-descriptor-suggested-keysize

	    *scheme:blowfish*
	    *scheme:x-tea*
	    *scheme:rc2* *scheme:rc5* *scheme:rc6*
	    *scheme:safer+* *scheme:safer-k64* *scheme:safer-sk64*
	    *scheme:safer-k128* *scheme:safer-sk128*
	    *scheme:aes* *scheme:aes-128* *scheme:aes-192* *scheme:aes-256*
	    *scheme:twofish*
	    *scheme:des* *scheme:des3* *scheme:desede*
	    *scheme:cast5* *scheme:cast-128*
	    *scheme:noekeon*
	    *scheme:skipjack*
	    *scheme:khazad*
	    *scheme:seed*
	    *scheme:kasumi*
	    *scheme:camellia*

	    *mode:ecb* *mode:cbc* *mode:cfb* *mode:ofb*
	    *mode:ctr* *mode:lrw* *mode:f8*

	    ecb-start ecb-encrypt! ecb-decrypt! ecb-done!

	    cbc-start cbc-encrypt! cbc-decrypt! cbc-done!
	    cbc-get-iv! cbc-set-iv!

	    cfb-start cfb-encrypt! cfb-decrypt! cfb-done!
	    cfb-get-iv! cfb-set-iv!

	    ofb-start ofb-encrypt! ofb-decrypt! ofb-done!
	    ofb-get-iv! ofb-set-iv!

	    *ctr-mode:little-endian*
	    *ctr-mode:big-endian*
	    *ctr-mode:rfc3686*
	    ctr-start ctr-encrypt! ctr-decrypt! ctr-done!
	    ctr-get-iv! ctr-set-iv!

	    lrw-start lrw-encrypt! lrw-decrypt! lrw-done!
	    lrw-get-iv! lrw-set-iv!

	    f8-start f8-encrypt! f8-decrypt! f8-done!
	    f8-get-iv! f8-set-iv!

	    *encauth:eax* *encauth:ocb* *encauth:ocb3*
	    *encauth:ccm* *encauth:gcm*
	    eax-init eax-encrypt! eax-decrypt! eax-add-header! eax-done!

	    ocb-init ocb-encrypt! ocb-decrypt!
	    ocb-done-encrypt! ocb-done-decrypt!

	    ocb3-init ocb3-encrypt! ocb3-decrypt!
	    ocb3-encrypt-last! ocb3-decrypt-last!
	    ocb3-add-aad! ocb3-done!

	    ccm-init ccm-reset! ccm-add-nonce! ccm-add-aad!
	    ccm-encrypt! ccm-decrypt! ccm-done!

	    gcm-init gcm-reset! gcm-add-iv! gcm-add-aad!
	    gcm-encrypt! gcm-decrypt! gcm-done!

	    ;; stream cipher
	    chacha-setup chacha-ivctr! chacha-crypt! chacha-done!

	    xchacha-ivctr!

	    chacha20-poly1305-setup chacha20-poly1305-setiv!
	    chacha20-poly1305-add-aad! chacha20-poly1305-encrypt!
	    chacha20-poly1305-decrypt! chacha20-poly1305-done!

	    xchacha20-poly1305-setiv!

	    ;; digest
	    *digest:whirlpool*

	    *digest:ripemd-128* *digest:ripemd-160* 
	    *digest:ripemd-256* *digest:ripemd-320*

	    *digest:sha-1*

	    *digest:sha-224* *digest:sha-256*
	    *digest:sha-384*
	    *digest:sha-512* *digest:sha-512/224* *digest:sha-512/256*

	    *digest:sha3-224* *digest:sha3-256* *digest:sha3-384*
	    *digest:sha3-512*

	    *digest:keccak-224* *digest:keccak-256* *digest:keccak-384*
	    *digest:keccak-512*

	    *digest:tiger-192*
	    
	    *digest:md5* *digest:md4* *digest:md2*

	    *digest:blake2s-128* *digest:blake2s-160* *digest:blake2s-224*
	    *digest:blake2s-256* *digest:blake2b-160* *digest:blake2b-256*
	    *digest:blake2b-384* *digest:blake2b-512*

	    find-digest digest-descriptor?
	    digest-descriptor-name digest-descriptor-block-size
	    digest-descriptor-digest-size digest-descriptor-oid

	    digest-state?
	    digest-init digest-process! digest-done!
	    sha3-shake-init sha3-shake-process! sha3-shake-done!
	    keccak-init (rename (sha3-shake-process! keccak-process!))
	    keccak-done!

	    ;; prng
	    *prng:yarrow* *prng:fortuna* *prng:rc4* *prng:sober-128*
	    *prng:system* *prng:chacha20*

	    find-prng prng-descriptor? prng-descriptor-name

	    prng-state?
	    prng-start prng-add-entropy! prng-read! prng-ready!
	    prng-done!

	    ;; mac
	    hmac-state? hmac-init hmac-process! hmac-done!
	    cmac-state? cmac-init cmac-process! cmac-done!

	    ;; KDF
	    hkdf pkcs12-kdf
	    *pkcs12:key-material*
	    *pkcs12:iv-material*
	    *pkcs12:mac-material*

	    ;; GCM
	    gcm-multiply!
	    )
    (import (only (sagittarius) define-constant)
	    (sagittarius dynamic-module))

(load-dynamic-module "sagittarius--tomcrypt")

(define-constant *scheme:blowfish*    "blowfish")
(define-constant *scheme:x-tea*       "xtea")
(define-constant *scheme:rc2*         "rc2")
(define-constant *scheme:rc5*         "rc5")
(define-constant *scheme:rc6*         "rc6")
(define-constant *scheme:safer+*      "safer+")
(define-constant *scheme:safer-k64*   "safer-k64")
(define-constant *scheme:safer-sk64*  "safer-sk64")
(define-constant *scheme:safer-k128*  "safer-k128")
(define-constant *scheme:safer-sk128* "safer-sk128")
(define-constant *scheme:aes*         "aes")
(define-constant *scheme:aes-128*     "aes128")
(define-constant *scheme:aes-192*     "aes192")
(define-constant *scheme:aes-256*     "aes256")
(define-constant *scheme:twofish*     "twofish")
(define-constant *scheme:des*         "des")
(define-constant *scheme:des3*        "3des")
(define-constant *scheme:desede*      *scheme:des3*)
(define-constant *scheme:cast5*       "cast5")
(define-constant *scheme:cast-128*    *scheme:cast5*)
(define-constant *scheme:noekeon*     "noekeon")
(define-constant *scheme:skipjack*    "skipjack")
(define-constant *scheme:khazad*      "khazad")
(define-constant *scheme:seed*        "seed")
(define-constant *scheme:kasumi*      "kasumi")
(define-constant *scheme:camellia*    "camellia")

;; Prng
(define-constant *prng:yarrow*        "yarrow")
(define-constant *prng:fortuna*       "fortuna")
(define-constant *prng:rc4*           "rc4")
(define-constant *prng:sober-128*     "sober128")
(define-constant *prng:system*        "sprng")
(define-constant *prng:chacha20*      "chacha20")

;; Digest
(define-constant *digest:whirlpool*   "whirlpool")
(define-constant *digest:ripemd-320*  "rmd320")
(define-constant *digest:ripemd-256*  "rmd256")
;; SHA1
(define-constant *digest:sha-1*       "sha1")
;; SHA2
(define-constant *digest:sha-224*     "sha224")
(define-constant *digest:sha-256*     "sha256")
(define-constant *digest:sha-384*     "sha384")
(define-constant *digest:sha-512*     "sha512")
(define-constant *digest:sha-512/224* "sha512-224")
(define-constant *digest:sha-512/256* "sha512-256")
;; SHA3
(define-constant *digest:sha3-224*    "sha3-224")
(define-constant *digest:sha3-256*    "sha3-256")
(define-constant *digest:sha3-384*    "sha3-384")
(define-constant *digest:sha3-512*    "sha3-512")

(define-constant *digest:keccak-224*  "keccak224")
(define-constant *digest:keccak-256*  "keccak256")
(define-constant *digest:keccak-384*  "keccak384")
(define-constant *digest:keccak-512*  "keccak512")

(define-constant *digest:tiger-192*   "tiger")

(define-constant *digest:ripemd-160*  "rmd160")
(define-constant *digest:ripemd-128*  "rmd128")
(define-constant *digest:md5*         "md5")
(define-constant *digest:md4*         "md4")
(define-constant *digest:md2*         "md2")

(define-constant *digest:blake2s-128* "blake2s-128")
(define-constant *digest:blake2s-160* "blake2s-160")
(define-constant *digest:blake2s-224* "blake2s-224")
(define-constant *digest:blake2s-256* "blake2s-256")
(define-constant *digest:blake2b-160* "blake2b-160")
(define-constant *digest:blake2b-256* "blake2b-256")
(define-constant *digest:blake2b-384* "blake2b-384")
(define-constant *digest:blake2b-512* "blake2b-512")

;; PKCS#12 purpose
(define-constant *pkcs12:key-material* 1)
(define-constant *pkcs12:iv-material*  2)
(define-constant *pkcs12:mac-material* 3)
)
