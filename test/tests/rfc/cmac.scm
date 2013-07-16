(import (rnrs) (rfc cmac) (crypto) (math)
	(sagittarius object)
	(srfi :64))

;; test vector from RFC 4493
(define K (generate-secret-key AES (integer->bytevector 
				    #x2b7e151628aed2a6abf7158809cf4f3c)))
(define aes-cipher (cipher AES K))

(define aes-mac (hash-algorithm CMAC :cipher aes-cipher))

(test-begin "CMAC")

(test-assert "CMAS?" (hash-algorithm? aes-mac))
;; we can't test K1 and K2 
(test-equal "AES-128(key, 0)"
	    #x7df76b0c1ab899b33e42f047b91b546f
	    (bytevector->integer (encrypt aes-cipher (make-bytevector 16 0))
				 0 16))

(test-equal "Example 1 len = 0"
	    (integer->bytevector #xbb1d6929e95937287fa37d129b756746)
	    (hash aes-mac #vu8()))

(test-equal "Example 2 len = 16"
	    (integer->bytevector #x070a16b46b4d4144f79bdd9dd04a287c)
	    (hash aes-mac (uint-list->bytevector 
			   '(#x6bc1bee2 #x2e409f96 #xe93d7e11 #x7393172a)
			   (endianness big) 4)))

(test-equal "Example 3 len = 40"
	    (integer->bytevector #xdfa66747de9ae63030ca32611497c827)
	    (hash aes-mac (uint-list->bytevector 
			   '(#x6bc1bee2 #x2e409f96 #xe93d7e11 #x7393172a
			     #xae2d8a57 #x1e03ac9c #x9eb76fac #x45af8e51
			     #x30c81c46 #xa35ce411)
			   (endianness big) 4)))

(test-equal "Example 4 len = 64"
	    (integer->bytevector #x51f0bebf7e3b9d92fc49741779363cfe)
	    (hash aes-mac (uint-list->bytevector 
			   '(#x6bc1bee2 #x2e409f96 #xe93d7e11 #x7393172a
			     #xae2d8a57 #x1e03ac9c #x9eb76fac #x45af8e51
			     #x30c81c46 #xa35ce411 #xe5fbc119 #x1a0a52ef
			     #xf69f2445 #xdf4f9b17 #xad2b417b #xe66c3710)
			   (endianness big) 4)))

(test-equal "splited MAC"
	    (integer->bytevector #x51f0bebf7e3b9d92fc49741779363cfe)
	    (begin
	      (hash-init! aes-mac)
	      (hash-process! aes-mac
			     (uint-list->bytevector 
			      '(#x6bc1bee2 #x2e409f96 #xe93d7e11 #x7393172a
				#xae2d8a57 #x1e03ac9c #x9eb76fac #x45af8e51)
			      (endianness big) 4))
	      (hash-process! aes-mac
			     (uint-list->bytevector
			      '(#x30c81c46 #xa35ce411 #xe5fbc119 #x1a0a52ef
				#xf69f2445 #xdf4f9b17 #xad2b417b #xe66c3710)
			      (endianness big) 4))
	      (let ((out (make-bytevector 16 0)))
		(hash-done! aes-mac out)
		out)))

;; AES_CMAC_96
(define aes-mac-96 (hash-algorithm CMAC :cipher aes-cipher :size (/ 96 8)))
;; from RFC 4494
(test-equal "Test Case 1 len = 0"
	    (integer->bytevector #xbb1d6929e95937287fa37d12)
	    (hash aes-mac-96 #vu8()))

(test-equal "Test Case 2 len = 16"
	    (integer->bytevector #x070a16b46b4d4144f79bdd9d)
	    (hash aes-mac-96 (uint-list->bytevector 
			   '(#x6bc1bee2 #x2e409f96 #xe93d7e11 #x7393172a)
			   (endianness big) 4)))

(test-equal "Test Case 3 len = 40"
	    (integer->bytevector #xdfa66747de9ae63030ca3261)
	    (hash aes-mac-96 (uint-list->bytevector 
			   '(#x6bc1bee2 #x2e409f96 #xe93d7e11 #x7393172a
			     #xae2d8a57 #x1e03ac9c #x9eb76fac #x45af8e51
			     #x30c81c46 #xa35ce411)
			   (endianness big) 4)))

(test-equal "Test Case 4 len = 64"
	    (integer->bytevector #x51f0bebf7e3b9d92fc497417)
	    (hash aes-mac-96 (uint-list->bytevector 
			   '(#x6bc1bee2 #x2e409f96 #xe93d7e11 #x7393172a
			     #xae2d8a57 #x1e03ac9c #x9eb76fac #x45af8e51
			     #x30c81c46 #xa35ce411 #xe5fbc119 #x1a0a52ef
			     #xf69f2445 #xdf4f9b17 #xad2b417b #xe66c3710)
			   (endianness big) 4)))


(test-end)