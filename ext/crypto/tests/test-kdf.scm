(import (rnrs)
	(sagittarius crypto kdfs)
	(sagittarius crypto digests)
	(sagittarius crypto mac)
	(util bytevector)
	(srfi :64))
(test-begin "KDFs")

(test-equal "PBKDF-1 with SHA-256"
	    (hex-string->bytevector
	     "E2AF0AABF7C9B53D815876AEEE578F56F43DB6EAB44DFD207E83566C99F58BDD")
	    (pbkdf-1 (string->utf8 "password") (string->utf8 "salt")
		     1024
		     (digest-descriptor-digest-size *digest:sha-256*)
		     :digest *digest:sha-256*))
(test-equal "PBKDF-1 with SHA-256"
	    (hex-string->bytevector
	     "E2AF0AABF7C9B53D815876AEEE578F56")
	    (pbkdf-1 (string->utf8 "password") (string->utf8 "salt")
		     1024
		     16
		     :digest *digest:sha-256*))

(define (test-pbkdf-2 md password salt c expected)
  (let ((dk (hex-string->bytevector expected)))
    (test-equal md
		dk
		(pbkdf-2 (string->utf8 password)
			 (string->utf8 salt)
			 c
			 (bytevector-length dk)
			 :prf (mac->prf-provider *mac:hmac* :digest md)))))

;; Test vector from https://www.ietf.org/rfc/rfc6070.txt
(test-pbkdf-2 *digest:sha-1* "password" "salt" 1
	      "0c60c80f961f0e71f3a9b524af6012062fe037a6")
(test-pbkdf-2 *digest:sha-1* "password" "salt" 2
	      "ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957")
(test-pbkdf-2 *digest:sha-1* "password" "salt" 4096
	      "4b007901b765489abead49d926f721d065a429c1")
;; Ignore this, takes too much time
#;(test-pbkdf-2 *digest:sha-1* "password" "salt" 16777216
	      "eefe3d61cd4da4e4e9945b3d6ba2158c2634e984")

(test-pbkdf-2 *digest:sha-1*
	      "passwordPASSWORDpassword"
	      "saltSALTsaltSALTsaltSALTsaltSALTsalt"
	      4096
	      "3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038")

(test-pbkdf-2 *digest:sha-1* "pass\x0;word" "sa\x0;lt" 4096
	      "56fa6aa75548099dcc37d7f03425e0c3")

;; From https://github.com/brycx/Test-Vector-Generation/blob/master/PBKDF2/pbkdf2-hmac-sha2-test-vectors.md
#|
Input:
  P = "password" (8 octets)
  S = "salt" (4 octets)
  c = 1
  dkLen = 20

Output:
  PBKDF2-HMAC-SHA224 = 3c198cbdb9464b7857966bd05b7bc92bc1cc4e6e (20 octets)

  PBKDF2-HMAC-SHA256 = 120fb6cffcf8b32c43e7225256c4f837a86548c9 (20 octets)

  PBKDF2-HMAC-SHA384 = c0e14f06e49e32d73f9f52ddf1d0c5c719160923 (20 octets)

  PBKDF2-HMAC-SHA512 = 867f70cf1ade02cff3752599a3a53dc4af34c7a6 (20 octets)
|#
(test-pbkdf-2 *digest:sha-224*
	      "password"
	      "salt"
	      1
	      "3c198cbdb9464b7857966bd05b7bc92bc1cc4e6e")
(test-pbkdf-2 *digest:sha-256*
	      "password"
	      "salt"
	      1
	      "120fb6cffcf8b32c43e7225256c4f837a86548c9")
(test-pbkdf-2 *digest:sha-384*
	      "password"
	      "salt"
	      1
	      "c0e14f06e49e32d73f9f52ddf1d0c5c719160923")
(test-pbkdf-2 *digest:sha-512*
	      "password"
	      "salt"
	      1
	      "867f70cf1ade02cff3752599a3a53dc4af34c7a6")


(test-end)
