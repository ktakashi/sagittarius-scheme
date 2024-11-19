(import (rnrs)
	(sagittarius)
	(sagittarius crypto kdfs scrypt)
	(util bytevector)
	(srfi :64))

(test-begin "Scrypt")

(define-syntax test-components
  (syntax-rules ()
    ((_ proc expect input args ...)
     (test-equal 'proc
		 (hex-string->bytevector expect)
		 (proc (hex-string->bytevector input) args ...)))))

(test-components
 salsa20/8
 "a41f859c6608cc993b81cacb020cef05044b2181a2fd337dfd7b1c6396682f29b4393168e3c9e6bcfe6bc5b7a06d96bae424cc102c91745c24ad673dc7618f81"
 "7e879a214f3ec9867ca940e641718f26baee555b8c61c1b50df846116dcd3b1dee24f319df9b3d8514121e4b5ac5aa3276021d2909c74829edebc68db8b8c25e")

(test-components
 scrypt-block-mix
 "a41f859c6608cc993b81cacb020cef05044b2181a2fd337dfd7b1c6396682f29b4393168e3c9e6bcfe6bc5b7a06d96bae424cc102c91745c24ad673dc7618f8120edc975323881a80540f64c162dcd3c21077cfe5f8d5fe2b1a4168f953678b77d3b3d803b60e4ab920996e59b4d53b65d2a225877d5edf5842cb9f14eefe425"
 "f7ce0b653d2d72a4108cf5abe912ffdd777616dbbb27a70e8204f3ae2d0f6fad89f68f4811d1e87bcc3bd7400a9ffd29094f0184639574f39ae5a1315217bcd7894991447213bb226c25b54da86370fbcd984380374666bb8ffcb5bf40c254b067d27c51ce4ad5fed829c90b505a571b7f4d1cad6a523cda770e67bceaaf7e89"
 1)

(test-components
 scrypt-ro-mix
 "79ccc193629debca047f0b70604bf6b62ce3dd4a9626e355fafc6198e6ea2b46d58413673b99b029d665c357601fb426a0b2f4bba200ee9f0a43d19b571a9c71ef1142e65d5a266fddca832ce59faa7cac0b9cf1be2bffca300d01ee387619c4ae12fd4438f203a0e4e1c47ec314861f4e9087cb33396a6873e8f9d2539a4b8e"
 "f7ce0b653d2d72a4108cf5abe912ffdd777616dbbb27a70e8204f3ae2d0f6fad89f68f4811d1e87bcc3bd7400a9ffd29094f0184639574f39ae5a1315217bcd7894991447213bb226c25b54da86370fbcd984380374666bb8ffcb5bf40c254b067d27c51ce4ad5fed829c90b505a571b7f4d1cad6a523cda770e67bceaaf7e89"
 16 1)

(define (test-scrypt P S N r p dk-len expected)
  (if (string=? "" P)
      (test-error (list "Empty password not allowed" N r p)
		  (hex-string->bytevector expected)
		  (scrypt (string->utf8 P)
			  (string->utf8 S)
			  N r p dk-len))
      (test-equal (list P S N r p)
		  (hex-string->bytevector expected)
		  (scrypt (string->utf8 P)
			  (string->utf8 S)
			  N r p dk-len))))
  
(test-scrypt "" "" 16 1 1 64
	     "77d6576238657b203b19ca42c18a0497f16b4844e3074ae8dfdffa3fede21442fcd0069ded0948f8326a753a0fc81f17e8d3e0fb2e0d3628cf35e20c38d18906")

(test-scrypt "password" "NaCl" 1024 8 16 64
	     "fdbabe1c9d3472007856e7190d01e9fe7c6ad7cbc8237830e77376634b3731622eaf30d92e22a3886ff109279d9830dac727afb94a83ee6d8360cbdfa2cc0640")

(test-scrypt "pleaseletmein" "SodiumChloride" 16384 8 1 64
	     "7023bdcb3afd7348461c06cd81fd38ebfda8fbba904f8e3ea9b543f6545da1f2d5432955613f0fcf62d49705242a9af9e61e85dc0d651e40dfcf017b45575887")

(unless (getenv "CI")
  (test-scrypt "pleaseletmein" "SodiumChloride" 1048576 8 1 64
	       "2101cb9b6a511aaeaddbbe09cf70f881ec568d574a2ffd4dabe5ee9820adaa478e56fd8f4ba5d09ffa1c6d927c40f4c337304049e8a952fbcbf45c6fa77a41a4"))
  
(test-end)
