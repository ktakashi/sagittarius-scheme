#!read-macro=sagittarius/bv-string
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
	      '((#*":method"     #*"GET")
		(#*":scheme"     #*"http")
		(#*":path"       #*"/")
		(#*":authority"  #*"www.example.com"))
	      (reader (open-bytevector-input-port req1)))
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
	      '((#*":method"     #*"GET")
		(#*":scheme"     #*"http")
		(#*":path"       #*"/")
		(#*":authority"  #*"www.example.com"))
	      (reader (open-bytevector-input-port req1)))
  (test-equal "table size (req1)" 57 (table-size ctx))

  (test-equal "read request2 (with huffman)"
	      '((#*":method"       #*"GET")
		(#*":scheme"       #*"http")
		(#*":path"         #*"/")
		(#*":authority"    #*"www.example.com")
		(#*"cache-control" #*"no-cache"))
	      (reader (open-bytevector-input-port req2)))
  (test-equal "table size (req2)" 110 (table-size ctx))

  (test-equal "read request3 (with huffman)"
	      '((#*":method"    #*"GET")
		(#*":scheme"    #*"https")
		(#*":path"      #*"/index.html")
		(#*":authority" #*"www.example.com")
		(#*"custom-key" #*"custom-value"))
	      (reader (open-bytevector-input-port req3)))
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
	      '((#*":status"       #*"302")
		(#*"cache-control" #*"private")
		(#*"date"          #*"Mon, 21 Oct 2013 20:13:21 GMT")
		(#*"location"      #*"https://www.example.com"))
	      (reader (open-bytevector-input-port res1)))
  (test-equal "table size (res1)" 222 (table-size ctx))

  (test-equal "read response2 (with huffman)"
	      '((#*":status"       #*"307")
		(#*"cache-control" #*"private")
		(#*"date"          #*"Mon, 21 Oct 2013 20:13:21 GMT")
		(#*"location"      #*"https://www.example.com"))
	      (reader (open-bytevector-input-port res2)))
  (test-equal "table size (res2)" 222 (table-size ctx))

  (test-equal "read response3 (with huffman)"
	      '((#*":status"          #*"200")
		(#*"cache-control"    #*"private")
		(#*"date"             #*"Mon, 21 Oct 2013 20:13:22 GMT")
		(#*"location"         #*"https://www.example.com")
		(#*"content-encoding" #*"gzip")
		(#*"set-cookie"       #*"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))
	      (reader (open-bytevector-input-port res3)))
  (test-equal "table size (res3)" 215 (table-size ctx))
  )

(let ()
  (define ctx (make-hpack-context 4096))
  (define reader (make-hpack-reader ctx))
  (define max-size-16 #vu8(#b00110000))
  (define max-size-1337 #vu8(#b00111111 #b10011010 #b00001010))

  (test-equal "max size" '() 
	      (reader (open-bytevector-input-port max-size-1337)))
  (test-equal "max size 1337" 1337 (table-max-size ctx))

  (test-equal "max size" '() (reader (open-bytevector-input-port max-size-16)))
  (test-equal "max size 16" 16 (table-max-size ctx))

  (test-error "setting bigger max size" condition?
	      (reader (open-bytevector-input-port max-size-1337)))
  (test-equal "max size 16 (2)" 16 (table-max-size ctx))
  )

;; encode test
(let ()
  (define ctx (make-hpack-context 256))
  (define writer (make-hpack-writer ctx))
  (define (hpack->bytevector hpack)
    (call-with-bytevector-output-port
     (lambda (out)
       (writer out hpack))))
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

  (test-equal "decode response1"
	      res1
	      (hpack->bytevector 
	       '((#*":status"       #*"302")
		 (#*"cache-control" #*"private")
		 (#*"date"          #*"Mon, 21 Oct 2013 20:13:21 GMT")
		 (#*"location"      #*"https://www.example.com"))))

  (test-equal "read response2 (with huffman)"
	      res2
	      (hpack->bytevector 
	       '((#*":status"       #*"307")
		 (#*"cache-control" #*"private")
		 (#*"date"          #*"Mon, 21 Oct 2013 20:13:21 GMT")
		 (#*"location"      #*"https://www.example.com"))))

  (test-equal "read response3 (with huffman)"
	      res3
	      (hpack->bytevector 
	       '((#*":status"          #*"200")
		(#*"cache-control"    #*"private")
		(#*"date"             #*"Mon, 21 Oct 2013 20:13:22 GMT")
		(#*"location"         #*"https://www.example.com")
		(#*"content-encoding" #*"gzip")
		(#*"set-cookie"       #*"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))))
  )

(test-end)

;; RFC HTTP2 
(import (rfc http2 conditions)
	(rfc http2 frame))

(define-syntax test-http2-condition
  (lambda (x)
    (define (->const name)
      (let ((str (symbol->string (syntax->datum name))))
	(string->symbol (string-append "+http2-error-code-" str "+"))))
    (define (->names name)
      (let ((str (symbol->string (syntax->datum name))))
	(list (string->symbol (string-append "&http2-" str))
	      (string->symbol (string-append "make-http2-" str))
	      (string->symbol (string-append "http2-" str "?")))))
    (define (->raise name)
      (let ((str (symbol->string (syntax->datum name))))
	(string->symbol (string-append "http2-" str))))
    (syntax-case x ()
      ((_ name code) 
       (with-syntax ((const (datum->syntax #'k (->const #'name)))
		     ((type ctr pred) (datum->syntax #'k (->names #'name)))
		     (raise (datum->syntax #'k (->raise #'name))))
	 #'(begin
	     (test-equal "error code" code const)
	     (guard (e (else 
			(test-assert "condition predicate" (pred e))
			(test-equal "http2-error-code" code 
				    (http2-error-code e))))
	       (raise 'name "dummy"))))))))

(test-begin "HTTP2")

(test-http2-condition no-error            #x0)
(test-http2-condition protocol-error      #x1)
(test-http2-condition internal-error      #x2)
(test-http2-condition flow-control-error  #x3)
(test-http2-condition settings-timeout    #x4)
(test-http2-condition stream-closed       #x5)
(test-http2-condition frame-size-error    #x6)
(test-http2-condition refused-stream      #x7)
(test-http2-condition cancel              #x8)
(test-http2-condition compression-error   #x9)
(test-http2-condition connect-error       #xa)
(test-http2-condition enhance-your-calm   #xb)
(test-http2-condition inadequate-security #xc)
(test-http2-condition http/1.1-required   #xd)

(test-assert "frame-buffer?" (frame-buffer? (make-frame-buffer)))
(test-equal "initial buffer size" #x4000 +http2-initial-frame-buffer-size+)
(test-equal "initial buffer-size" +http2-initial-frame-buffer-size+
	    (bytevector-length (frame-buffer-buffer (make-frame-buffer))))
(test-equal "current size" 0 (frame-buffer-size (make-frame-buffer)))

(test-error "invalid size range(1)" http2-protocol-error?
	    (make-frame-buffer
	     (- +http2-initial-frame-buffer-size+ 1)))
(test-error "invalid size range(2)" http2-protocol-error?
	    (make-frame-buffer
	     (+ +http2-max-frame-buffer-size+ 1)))
(test-error "invalid size range(3)" http2-protocol-error?
	    (update-frame-buffer! (make-frame-buffer)
				  (- +http2-initial-frame-buffer-size+ 1)))
(test-error "invalid size range(4)" http2-protocol-error?
	    (update-frame-buffer! (make-frame-buffer)
				  (+ +http2-max-frame-buffer-size+ 1)))

;; frame types
(test-equal "frame type" 0  +http2-frame-type-data+)
(test-equal "frame type" 1  +http2-frame-type-headers+)
(test-equal "frame type" 2  +http2-frame-type-priority+)
(test-equal "frame type" 3  +http2-frame-type-rst-stream+)
(test-equal "frame type" 4  +http2-frame-type-settings+)
(test-equal "frame type" 5  +http2-frame-type-push-promise+)
(test-equal "frame type" 6  +http2-frame-type-ping+)
(test-equal "frame type" 7  +http2-frame-type-goaway+)
(test-equal "frame type" 8  +http2-frame-type-window-update+)
(test-equal "frame type" 9  +http2-frame-type-continuation+)

(let ()
  (define frame #vu8(0 0 1 0 0 0 0 0 1 1))
  (test-equal "fill-http2-frame-buffer!" '(0 0 1 #vu8(1))
	      (let ((buffer (make-frame-buffer)))
		(let-values (((type flags si)
			      (fill-http2-frame-buffer! 
			       (open-bytevector-input-port frame) buffer)))
		  (list type flags si (bytevector-copy
				       (frame-buffer-buffer buffer)
				       0
				       (frame-buffer-size buffer))))))

  (test-equal "write-http2-frame!" frame
	      (call-with-bytevector-output-port
	       (lambda (out)
		 (let* ((buf (make-frame-buffer))
			(fport (->frame-buffer-output-port buf)))
		   (put-u8 fport 1)
		   (write-http2-frame! out 0 0 1 buf)))))
)

(define-syntax test-http2-frame
  (lambda (x)
    (define (->const name)
      (let ((str (symbol->string (syntax->datum name))))
	(string->symbol (string-append "+http2-frame-type-" str "+"))))
    (define (->names name)
      (let ((str (symbol->string (syntax->datum name))))
	(list (string->symbol (string-append "make-http2-frame-" str))
	      (string->symbol (string-append "http2-frame-" str "?")))))
    ;; R6RS record naming convension
    (define (->accessors name fields)
      (map (lambda (field)
	     (let ((str (symbol->string (syntax->datum name)))
		   (f   (symbol->string (syntax->datum field))))
	       (string->symbol (string-append "http2-frame-" str "-" f))))
	   fields))
    (define (->data fields)
      (fold (lambda (f acc) (cons (length acc) acc)) '() fields))
    (syntax-case x ()
      ((_ name code fields ...)
       (with-syntax ((const (datum->syntax #'k (->const #'name)))
		     ((ctr pred) (datum->syntax #'k (->names #'name)))
		     ((datum ...) (datum->syntax #'k (->data #'(fields ...))))
		     ((accessor ...) 
		      (datum->syntax #'k (->accessors #'name #'(fields ...)))))
	 #'(begin
	     (test-assert 'ctr (pred (ctr 0 1 datum ...)))
	     (let ((frame (ctr 0 1 datum ...)))
	       (test-equal "frame type" code (http2-frame-type frame))
	       (test-equal 'accessor datum (accessor frame))
	       ...)))))))

(test-http2-frame data          #x0 data)
(test-http2-frame headers       #x1 stream-dependency weight headers)
(test-http2-frame priority      #x2 stream-dependency weight)
(test-http2-frame rst-stream    #x3 error-code)
(test-http2-frame settings      #x4 settings)
(test-http2-frame push-promise  #x5 pushed-promise-id headers)
(test-http2-frame ping          #x6 opaque-data)
(test-http2-frame goaway        #x7 last-stream-id error-code data)
(test-http2-frame window-update #x8 window-size-increment)
(test-http2-frame continuation  #x9 headers)

(define (test-http2-frame-data frame)
  (let ((buffer (make-frame-buffer)))
    (let ((frame (read-http2-frame (open-bytevector-input-port frame)
				   buffer
				   #f)))
      (test-assert "data?" (http2-frame-data? frame))
      (test-equal "data" #vu8(1) (http2-frame-data-data frame)))))
(test-http2-frame-data #vu8(0 0 1 0 0 0 0 0 1 1))
;; with padding
(test-http2-frame-data #vu8(0 0 4 0 8 0 0 0 1 2 1 0 0))

(let ((buffer (make-frame-buffer)))
  (define ctx (make-hpack-context 4096))
  (define reader (make-hpack-reader ctx))
  (define headers-frame
    (bytevector-append #vu8(0 0 17 1 0 0 0 0 1)
       (integer->bytevector #x828684418cf1e3c2e5f23a6ba0ab90f4ff)))
  (let ((frame (read-http2-frame (open-bytevector-input-port headers-frame)
				 buffer
				 reader)))
    (test-assert "headers?" (http2-frame-headers? frame))
    (test-assert "dependency"
		 (not (http2-frame-headers-stream-dependency frame)))
    (test-assert "weight" (not (http2-frame-headers-weight frame)))
    (test-equal "headers" 
		'((#*":method"     #*"GET")
		  (#*":scheme"     #*"http")
		  (#*":path"       #*"/")
		  (#*":authority"  #*"www.example.com"))
		(http2-frame-headers-headers frame))))

(test-end)
