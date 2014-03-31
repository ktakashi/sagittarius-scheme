(import (rnrs)
	(net mq amqp type)
	(srfi :19)
	(sagittarius time)
	(rfc uuid)
	(clos user)
	(clos core)
	(srfi :64))

(test-begin "AMQP tests")

(define-syntax test-primitive
  (syntax-rules ()
    ((_ type bv v)
     (test-primitive type bv v bv))
    ((_ type bv v expect)
     (let ()
       (test-equal (format "read(~a:~a:~a)" type bv v) v
		   (read-amqp-data (open-bytevector-input-port bv)))
       (test-equal (format "write(~a:~a:~a)" type bv v) expect
		   (call-with-bytevector-output-port
		    (lambda (out)
		      (write-primitive-amqp-data out type v))))))))

(test-primitive :null    #vu8(#x40) '())
(test-primitive :boolean #vu8(#x41) #t)
(test-primitive :boolean #vu8(#x42) #f)
(test-primitive :boolean #vu8(#x56 0) #f #vu8(#x42))
(test-primitive :boolean #vu8(#x56 1) #t #vu8(#x41))
;; signed
(test-primitive :ubyte   #vu8(#x50 255) 255)
(test-primitive :ubyte   #vu8(#x50 0) 0)
(test-primitive :ushort  #vu8(#x60 255 255) #xFFFF)
(test-primitive :ushort  #vu8(#x60 0 0) 0)
(test-primitive :uint    #vu8(#x70 #xFF #xFF #xFF #xFF) #xFFFFFFFF)
(test-primitive :uint    #vu8(#x52 255) 255)
(test-primitive :uint    #vu8(#x43) 0)
(test-primitive :ulong   #vu8(#x80 #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF) #xFFFFFFFFFFFFFFFF)
(test-primitive :ulong   #vu8(#x53 255) 255)
(test-primitive :ulong   #vu8(#x44) 0)
;; unsinged
(test-primitive :byte    #vu8(#x51 255) -1)
(test-primitive :byte    #vu8(#x51 127) 127)
(test-primitive :byte    #vu8(#x51 128) -128)
(test-primitive :short   #vu8(#x61 #xFF #xFF) -1)
(test-primitive :short   #vu8(#x61 #x80 #x00) #x-8000)
(test-primitive :short   #vu8(#x61 #x7F #xFF) #x7FFF)
(test-primitive :int     #vu8(#x71 #xFF #xFF #xFF #xFF) -1 #vu8(#x54 #xFF))
(test-primitive :int     #vu8(#x71 #x80 #x00 #x00 #x00) #x-80000000)
(test-primitive :int     #vu8(#x71 #x7F #xFF #xFF #xFF) #x7FFFFFFF)
(test-primitive :long    #vu8(#x81 #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF) -1 #vu8(#x55 #xFF))
(test-primitive :long    #vu8(#x81 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00) #x-8000000000000000)
(test-primitive :long    #vu8(#x81 #x7F #xFF #xFF #xFF #xFF #xFF #xFF #xFF) #x7FFFFFFFFFFFFFFF)
;; floating number
(test-primitive :float   #vu8(#x72 63 128 0 0) 1.0)
(test-primitive :double  #vu8(#x82 63 240 0 0 0 0 0 0) 1.0)
;; char
(test-primitive :char    #vu8(#x73 0 0 0 #x41) #\A)
(test-primitive :char    #vu8(#x73 0 0 #x03 #xbb) #\λ)

;; timestamp
(define-method object-equal? ((date1 <date>) (date2 <date>))
  (time=? (date->time-utc date1) (date->time-utc date2)))

(let ((date (time-utc->date (make-time time-utc 521000000 1311704463))))
  (test-primitive :timestamp 
		  (bytevector-append #vu8(#x83) 
				     (integer->bytevector 1311704463521 8))
		  date))

(let ((uuid (bytevector->uuid #vu8(200 58 39 22 185 8 17 227 162 243 0 255 48 18 112 11))))
  (test-primitive :uuid #vu8(#x93 200 58 39 22 185 8 17 227 162 243 0 255 48 18 112 11) uuid))

(test-end)