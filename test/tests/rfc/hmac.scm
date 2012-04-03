(import (rnrs)
	(sagittarius)
	(rfc hmac)
	(math) ;; for non default hasher
	(srfi :64 testing))

(test-begin "HMAC tests")
;; the test data is created by http://buchananweb.co.uk/security01.aspx
(define message (string->utf8 "password"))
(define key (string->utf8 "salt"))

(test-equal "HMAC-MD5"
	    (integer->bytevector #x961FF2373921D4EADFE97E4CCC56D3E2)
	    (hash HMAC message :key key :hash (hash-algorithm MD5)))
(test-equal "HMAC-SHA1"
	    (integer->bytevector #xC1D0E06998305903AC76F589BBD6D4B61A670BA6)
	    ;; SHA-1 is default
	    (hash HMAC message :key key))
(test-equal "HMAC-SHA256"
	    (integer->bytevector #x84EC44C7D6FC41917953A1DAFCA3C7D7856F7A9D0328B991B76F0D36BE1224B9)
	    ;; SHA-1 is default
	    (hash HMAC message :key key :hash SHA-256))
(test-equal "HMAC-SHA384"
	    (integer->bytevector #x10CA3A035F41DFC26877E594C0776E567C94266D8422C8571FC8B0E22D5FBC1F4D32C880929E97E3D35341F047CF10DA)
	    ;; SHA-1 is default
	    (hash HMAC message :key key :hash SHA-384))
(test-equal "HMAC-SHA512"
	    (integer->bytevector #x1C8E432462648D825ADE4983DA4B1C9CC231180D3DD0E77B0CFE0B28C5E2F2B39AA3ADABFCD5E1FE968B9E815005CF67499C30177F4C0199E39064CEAA5ADEFA)
	    ;; SHA-1 is default
	    (hash HMAC message :key key :hash SHA-512))

(test-end)