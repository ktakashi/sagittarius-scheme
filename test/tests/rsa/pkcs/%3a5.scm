(import (rnrs)
	(sagittarius)
	(rsa pkcs :5)
	(crypto)
	(srfi :64 testing))

(test-begin "PKCS#5 tests")

;; key derivation tests
(define (test-derive-key expected password salt count key-length)
  (let ((d-key (derive-key (string->utf8 password)
			   (string->utf8 salt)
			   count
			   key-length)))
    (test-equal (format "derive key(password ~a salt ~a count ~a length ~a)" 
			password salt count key-length)
		expected d-key)))

;; The expected data is from RFC 6070
(test-derive-key #vu8(#x0c #x60 #xc8 #x0f #x96 #x1f #x0e #x71
		      #xf3 #xa9 #xb5 #x24 #xaf #x60 #x12 #x06
		      #x2f #xe0 #x37 #xa6)
		 "password" "salt" 1 20)

(test-derive-key #vu8(#xea #x6c #x01 #x4d #xc7 #x2d #x6f #x8c
		      #xcd #x1e #xd9 #x2a #xce #x1d #x41 #xf0
		      #xd8 #xde #x89 #x57)
		 "password" "salt" 2 20)

(test-derive-key #vu8(#x4b #x00 #x79 #x01 #xb7 #x65 #x48 #x9a
		      #xbe #xad #x49 #xd9 #x26 #xf7 #x21 #xd0
		      #x65 #xa4 #x29 #xc1)
		 "password" "salt" 4096 20)
#|
;; this is too much to round for Sagittarius...
(test-derive-key #vu8(#xee #xfe #x3d #x61 #xcd #x4d #xa4 #xe4
		      #xe9 #x94 #x5b #x3d #x6b #xa2 #x15 #x8c
		      #x26 #x34 #xe9 #x84)
		 "password" "salt" 16777216 20)
|#

(test-derive-key #vu8(#x3d #x2e #xec #x4f #xe4 #x1c #x84 #x9b
		      #x80 #xc8 #xd8 #x36 #x62 #xc0 #xe4 #x4a
		      #x8b #x29 #x1a #x96 #x4c #xf2 #xf0 #x70
		      #x38)
		 "passwordPASSWORDpassword"
		 "saltSALTsaltSALTsaltSALTsaltSALTsalt" 4096 25)

(test-derive-key #vu8(#x56 #xfa #x6a #xa7 #x55 #x48 #x09 #x9d
		      #xcc #x37 #xd7 #xf0 #x34 #x25 #xe0 #xc3)
		 "pass\x0;word" "sa\x0;lt"
		 4096 16)

;; encryption tests
(define (test expected algorithm password salt count)
  (let* ((param (make-pbe-parameter (string->utf8 salt) count))
	 (key (generate-secret-key algorithm password))
	 (pbe-cipher (cipher algorithm key :parameter param))
	 (ciphertext (encrypt pbe-cipher 
			      (string->utf8 "This is an example."))))
    (test-equal algorithm expected ciphertext)))


;; algorithm PBEWithMD5AndDES, password "password", salt "", count 1024
(test #vu8(#xDD #xFA #x2D #x7E #x00 #x1E #xEE #x47 
	   #x8E #x48 #x96 #x8A #xF6 #x3E #x12 #x67 
	   #x77 #x25 #x32 #x5B #xD8 #x18 #xDA #x31)
      pbe-with-md5-and-des "password" "" 1024)
;; algorithm PBEWithMD5AndDES, password "password", salt "salt", count 1024
(test #vu8(#x99 #x0C #x80 #x0D #x0A #xFB #xB8 #xEE 
	   #x97 #x4A #x5B #x71 #x17 #xAA #x98 #x24 
	   #x24 #x86 #x4B #x40 #x5A #x8B #xB4 #xBE)
      pbe-with-md5-and-des "password" "salt" 1024)
;; algorithm PBEwithSHA1AndDES, password "password", salt "", count 1024
(test #vu8(#x15 #xAF #x01 #x45 #xF0 #xE0 #xCC #xFB 
	   #xFB #x6E #xF3 #xE6 #x57 #x8D #xF2 #x64 
	   #x59 #x92 #x78 #x72 #xCE #x63 #x08 #x5E)
      pbe-with-sha1-and-des "password" "" 1024)
;; algorithm PBEWithSha1AndDES, password "password", salt "salt", count 1024
(test #vu8(#xFE #xE5 #x9B #xA8 #xA7 #xC0 #xF9 #x2B 
	   #x21 #xC0 #xA1 #xD7 #x1C #x75 #xA9 #x81 
	   #x93 #x3C #x10 #x34 #xEB #x4F #x5A #x17)
      pbe-with-sha1-and-des "password" "salt" 1024)

;; PBES2 test
;; I couldn't find any example of PBES2. This should be OK.

(import (clos user) (rfc hmac) (math))
(define-class <pbkef2-with-hmac-sha1-des3> () ())
(define pbkdf2-with-hmac-sha1-des3 (make <pbkef2-with-hmac-sha1-des3>))
(define-method generate-secret-key ((mark <pbkef2-with-hmac-sha1-des3>)
				    (password <string>))
  (make <pbe-secret-key> :password  password :hash (hash-algorithm HMAC)
	:scheme DES3 :iv-size 8 :length 24
	:type PKCS5-S2))
(register-spi pbkdf2-with-hmac-sha1-des3 <pbe-cipher-spi>)

(let* ((c 1024)
       (param (make-pbe-parameter (string->utf8 "saltsalt") c))
       (key (generate-secret-key pbkdf2-with-hmac-sha1-des3 "password"))
       (pbe-cipher (cipher pbkdf2-with-hmac-sha1-des3
			   key :parameter param))
       (ciphertext (encrypt pbe-cipher 
			    (string->utf8 "This is an example."))))
  (test-equal "PBES2 test" 
	      "This is an example."
	      (utf8->string (decrypt pbe-cipher ciphertext))))

(test-end)