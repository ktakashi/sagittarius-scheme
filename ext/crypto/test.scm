(add-load-path "./crypto")
(library (crypto test)
    (export run-crypto-test)
    (import (srfi :64 testing)
	    (rnrs)
	    (crypto))

  ;; plain value for all test cases
  (define *plain-value* (string->utf8 "I want to encrypt this text with the key!"))

  ;; DES test variables
  ;; DES-key must be 8 byte.
  (define *des-key* (generate-secret-key DES (string->utf8 "8bytekey")))
  ;; mode ECB
  (define *des/ecb-encrypted-value* #vu8(84 85 42 221 141 106 0 232 14 121 91
					 247 72 234 116 240 172 64 21 37 132 95
					 218 33 115 60 198 159 177 7 190 132 192
					 41 72 56 147 35 16 19 57 251 193 32 172
					 60 149 88 ))
  ;; we only support NoPadding for now.
  ;; DES/ECB/PKCS5Padding
  (define des/ecb-cipher (cipher DES *des-key*))
  ;; mode CBC
  (define *des/cbc-encrypted-value* #vu8(84 85 42 221 141 106 0 232 27 30 182
					 118 4 145 76 6 188 159 250 21 101 77
					 164 108 21 17 88 238 91 182 222 224 167
					 190 146 224 245 191 197 96 180 131 240
					 188 245 131 173 214 ))
  ;; DES/CBC/PKCS5Padding
  (define des/cbc-cipher (cipher DES *des-key*
				 :mode MODE_CBC
				 :iv (make-bytevector 8 0)))
  ;; Triple DES test variables
  ;; we need 24 bytes for key
  (define *des3-key* (generate-secret-key DES3 (string->utf8 "24bytekey123456789012345")))
  (define des3/ecb-cipher (cipher DES3 *des3-key*))
  (define *des3/ecb-encrypted-value* #vu8(206 108 187 150 233 197 246 31 249 102
					  106 26 0 109 134 172 189 147 206 65
					  130 22 96 65 19 127 169 159 56 4 89
					  237 101 140 28 239 201 52 238 48 225
					  116 191 153 151 81 167 192 ))
  ;; mode CBC
  (define des3/cbc-cipher (cipher DES3 *des3-key*
				  :mode MODE_CBC
				  :iv (make-bytevector 8 0)))
  (define *des3/cbc-encrypted-value* #vu8(206 108 187 150 233 197 246 31 137 218
					  183 8 206 147 188 108 248 23 19 187
					  239 219 114 237 233 214 34 79 76 209
					  191 75 26 223 140 38 166 255 236 146
					  140 167 157 42 240 38 203 30 ))

  (define *pseudo-primes*
    '(341 561 645 1105 1387 1729 1905 2047 2465 2701 
      2821 3277 4033 4369 4371 4681 5461 6601 7957 8321 
      8481 8911))

  (define (run-crypto-test)
    ;; basic test
    (test-assert "crypto-object?" (crypto-object? des/cbc-cipher))
    (test-assert "cipher?" (cipher? des/cbc-cipher))
    (test-assert "key?" (key? *des-key*))
    ;; key suggest
    (test-equal "DES key size" 8 (cipher-keysize DES 8))
    (test-equal "DES3 key size" 24 (cipher-keysize DES3 24))
    ;; encryption
    (test-equal "DES/ECB encrypt"
		*des/ecb-encrypted-value*
		(encrypt des/ecb-cipher *plain-value*))
    (test-equal "DES/CBC encrypt"
		*des/cbc-encrypted-value*
		(encrypt des/cbc-cipher *plain-value*))
    (test-equal "DES3/ECB encrypt"
		*des3/ecb-encrypted-value*
		(encrypt des3/ecb-cipher *plain-value*))
    (test-equal "DES3/CBC encrypt"
		*des3/cbc-encrypted-value*
		(encrypt des3/cbc-cipher *plain-value*))
    ;; decryption
    (test-equal "DES/ECB decrypt"
		*plain-value*
		(decrypt des/ecb-cipher *des/ecb-encrypted-value*))
    (test-equal "DES/CBC decrypt"
		*plain-value*
		(decrypt des/cbc-cipher *des/cbc-encrypted-value*))
    (test-equal "DES3/ECB decrypt"
		*plain-value*
		(decrypt des3/ecb-cipher *des3/ecb-encrypted-value*))
    (test-equal "DES3/CBC decrypt"
		*plain-value*
		(decrypt des3/cbc-cipher *des3/cbc-encrypted-value*))

    ;; prime?
    (test-assert "is-prime?"
		 (not (exists is-prime? *pseudo-primes*)))
    )
)
