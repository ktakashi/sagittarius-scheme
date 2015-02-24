(import (rnrs)
	(rfc http2 hpack)
	(sagittarius control) ;; for with-library
	(srfi :64))

(test-begin "HPACK")

(test-equal "HPACK huffman decode"
	    (string->utf8 "Mon, 21 Oct 2013 20:13:21 GMT")
	    (hpack-huffman->bytevector 
	     #vu8(#xd0 #x7a #xbe #x94 #x10 #x54 #xd4 #x44 #xa8 #x20 #x05
		  #x95 #x04 #x0b #x81 #x66 #xe0 #x82 #xa6 #x2d #x1b #xff)))

(test-equal "HPACK huffman encode"
	    #vu8(#xd0 #x7a #xbe #x94 #x10 #x54 #xd4 #x44 #xa8 #x20 #x05
		 #x95 #x04 #x0b #x81 #x66 #xe0 #x82 #xa6 #x2d #x1b #xff)
	    (bytevector->hpack-huffman 
	     (string->utf8 "Mon, 21 Oct 2013 20:13:21 GMT")))

;; read-hpack
(let ()
  (define ctx (make-hpack-context 4096))
  (define reader (make-hpack-reader ctx))
  (define req1
    (uint-list->bytevector
     '(#x8286 #x8441 #x0f77 #x7777 #x2e65 #x7861 #x6d70 #x6c65
	      #x2e63 #x6f6d)
     'big 2))

  (test-equal "read request1 (without huffman)"
	      '((":method" . "GET")
		(":scheme" . "http")
		(":path" . "/")
		(":authority" . "www.example.com"))
	      (map (lambda (k&v)
		     (cons (utf8->string (car k&v))
			   (utf8->string (cdr k&v))))
		   (reader (open-bytevector-input-port req1))))
  )

(let ()
  (define ctx (make-hpack-context 4096))
  (define reader (make-hpack-reader ctx))
  (define req1 (integer->bytevector #x828684418cf1e3c2e5f23a6ba0ab90f4ff))

  (test-equal "read request1 (with huffman)"
	      '((":method" . "GET")
		(":scheme" . "http")
		(":path" . "/")
		(":authority" . "www.example.com"))
	      (map (lambda (k&v)
		     (cons (utf8->string (car k&v))
			   (utf8->string (cdr k&v))))
		   (reader (open-bytevector-input-port req1))))
  )

;; read-variable integer
(let ()
  ;; it's not exposed so use with-library to test.
  (define read-hpack-integer 
    (with-library (rfc http2 hpack) read-hpack-integer))
  (test-equal "read variable length integer"
	      1306
	      (read-hpack-integer (open-bytevector-input-port
				   (integer->bytevector #b1001101000001010))
				  #x1F #x1F)))

(test-end)

;;(test-begin "HTTP2")
;;(test-end)
