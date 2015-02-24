(import (rnrs)
	(rfc http2 hpack)
	(sagittarius control) ;; for with-library
	(util bytevector)
	(srfi :64)
	(clos user))

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


;; depending on the slot definition
;; do not use other than tests (may change)
(define (table-size context)
  (slot-ref (slot-ref context 'dynamic-table) 'current-size))
(define (table-max-size context)
  (slot-ref (slot-ref context 'dynamic-table) 'max-size))

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
  (define req2 (integer->bytevector #x828684be5886a8eb10649cbf))
  (define req3
    (uint-list->bytevector
     '(#x8287 #x85bf #x4088 #x25a8 #x49e9 #x5ba9 #x7d7f #x8925
       #xa849 #xe95b #xb8e8 #xb4bf)
     'big 2))
  (test-equal "read request1 (with huffman)"
	      '((":method" . "GET")
		(":scheme" . "http")
		(":path" . "/")
		(":authority" . "www.example.com"))
	      (map (lambda (k&v)
		     (cons (utf8->string (car k&v))
			   (utf8->string (cdr k&v))))
		   (reader (open-bytevector-input-port req1))))
  (test-equal "table size (req1)" 57 (table-size ctx))

  (test-equal "read request2 (with huffman)"
	      '((":method" . "GET")
		(":scheme" . "http")
		(":path" . "/")
		(":authority" . "www.example.com")
		("cache-control" . "no-cache"))
	      (map (lambda (k&v)
		     (cons (utf8->string (car k&v))
			   (utf8->string (cdr k&v))))
		   (reader (open-bytevector-input-port req2))))
  (test-equal "table size (req2)" 110 (table-size ctx))

  (test-equal "read request3 (with huffman)"
	      '((":method" . "GET")
		(":scheme" . "https")
		(":path" . "/index.html")
		(":authority" . "www.example.com")
		("custom-key" . "custom-value"))
	      (map (lambda (k&v)
		     (cons (utf8->string (car k&v))
			   (utf8->string (cdr k&v))))
		   (reader (open-bytevector-input-port req3))))
  (test-equal "table size (req3)" 164 (table-size ctx))
  )

(let ()
  (define ctx (make-hpack-context 256))
  (define reader (make-hpack-reader ctx))
  (define res1
    (uint-list->bytevector
     '(#x4882 #x6402 #x5885 #xaec3 #x771a #x4b61 #x96d0 #x7abe
       #x9410 #x54d4 #x44a8 #x2005 #x9504 #x0b81 #x66e0 #x82a6
       #x2d1b #xff6e #x919d #x29ad #x1718 #x63c7 #x8f0b #x97c8
       #xe9ae #x82ae #x43d3)
     'big 2))
  (define res2 (integer->bytevector #x4883640effc1c0bf))
  (define res3 (hex-string->bytevector
		"88c16196d07abe941054d444a8200595\
                 040b8166e084a62d1bffc05a839bd9ab\
                 77ad94e7821dd7f2e6c7b335dfdfcd5b\
                 3960d5af27087f3672c1ab270fb5291f\
                 9587316065c003ed4ee5b1063d5007"))

  (test-equal "read response1 (with huffman)"
	      '((":status" . "302")
		("cache-control" . "private")
		("date" . "Mon, 21 Oct 2013 20:13:21 GMT")
		("location" . "https://www.example.com"))
	      (map (lambda (k&v)
		     (cons (utf8->string (car k&v))
			   (utf8->string (cdr k&v))))
		   (reader (open-bytevector-input-port res1))))
  (test-equal "table size (res1)" 222 (table-size ctx))

  (test-equal "read response2 (with huffman)"
	      '((":status" . "307")
		("cache-control" . "private")
		("date" . "Mon, 21 Oct 2013 20:13:21 GMT")
		("location" . "https://www.example.com"))
	      (map (lambda (k&v)
		     (cons (utf8->string (car k&v))
			   (utf8->string (cdr k&v))))
		   (reader (open-bytevector-input-port res2))))
  (test-equal "table size (res2)" 222 (table-size ctx))

  (test-equal "read response3 (with huffman)"
	      '((":status" . "200")
		("cache-control" . "private")
		("date" . "Mon, 21 Oct 2013 20:13:22 GMT")
		("location" . "https://www.example.com")
		("content-encoding" . "gzip")
		("set-cookie" . "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))
	      (map (lambda (k&v)
		     (cons (utf8->string (car k&v))
			   (utf8->string (cdr k&v))))
		   (reader (open-bytevector-input-port res3))))
  (test-equal "table size (res3)" 215 (table-size ctx))
  )

(let ()
  (define ctx (make-hpack-context 4096))
  (define reader (make-hpack-reader ctx))
  (define max-size-16 #vu8(#b00110000))
  (define max-size-1306 #vu8(#b00111111 #b10011010 #b00001010))

  (test-equal "max size" '() (reader (open-bytevector-input-port max-size-16)))
  (test-equal "max size 16" 16 (table-max-size ctx))

  (test-equal "max size" '() 
	      (reader (open-bytevector-input-port max-size-1306)))
  (test-equal "max size 1306" 1306 (table-max-size ctx))
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
