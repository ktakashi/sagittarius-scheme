(import (rnrs)
	(rfc http2 hpack)
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

(test-end)

;;(test-begin "HTTP2")
;;(test-end)
