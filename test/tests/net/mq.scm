(import (rnrs)
	(net mq amqp types)
	(srfi :19)
	(sagittarius time)
	(rfc uuid)
	(clos user)
	(clos core)
	(srfi :1)
	(srfi :64))

(test-begin "AMQP tests")

(define-syntax test-primitive
  (syntax-rules ()
    ((_ type bv v)
     (test-primitive type bv v bv))
    ((_ type bv v expect)
     (let ((bvv bv) (vv v) (e expect))
       (test-equal (format "read(~a:~a:~a)" type bvv vv) vv
		   (scheme-value 
		    (read-amqp-data (open-bytevector-input-port bvv))))
       (test-equal (format "write(~a:~a:~a)" type bvv vv) e
		   (call-with-bytevector-output-port
		    (lambda (out)
		      (write-primitive-amqp-data out type vv))))))))

(test-primitive :null    #vu8(#x40) +amqp-null+)
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

(let ((date (time-utc->date (make-time time-utc 521000000 1311704463) 0)))
  (test-primitive :timestamp 
		  (bytevector-append #vu8(#x83) 
				     (integer->bytevector 1311704463521 8))
		  date))

(let ((uuid (bytevector->uuid #vu8(200 58 39 22 185 8 17 227 162 243 0 255 48 18 112 11))))
  (test-primitive :uuid #vu8(#x93 200 58 39 22 185 8 17 227 162 243 0 255 48 18 112 11) uuid))

(test-primitive :binary #vu8(#xA0 #x05 1 2 3 4 5) #vu8(1 2 3 4 5))

(let ((bin256 (make-bytevector 256 10)))
  (test-primitive :binary (bytevector-append #vu8(#xB0 0 0 1 0) bin256)
		  bin256))

(test-primitive :string (bytevector-append #vu8(#xA1 5) (string->utf8 "hello"))
		"hello")
(let ((str256 (apply string (make-list 256 #\a))))
  (test-primitive :string (bytevector-append #vu8(#xB1 0 0 1 0)
					     (string->utf8 str256))
		  str256))

(test-primitive :symbol (bytevector-append #vu8(#xA3 5) (string->utf8 "hello"))
		'hello)

(let ((str256 (apply string (make-list 256 #\a))))
  (test-primitive :symbol (bytevector-append #vu8(#xB3 0 0 1 0)
					     (string->utf8 str256))
		  (string->symbol str256)))

;; well it's primitive but composite
(define-syntax test-list
  (syntax-rules ()
    ((_ bv v)
     (let ((bvv bv) (vv v))
       (test-equal (format "read(~a)" :list) (map scheme-value vv)
		   ;; unwrap it
		   (map scheme-value
			(scheme-value 
			 (read-amqp-data (open-bytevector-input-port bvv)))))
       (test-equal (format "write(~a)" :list) bvv
		   (call-with-bytevector-output-port
		    (lambda (out)
		      (write-primitive-amqp-data out :list vv))))))))
(test-list #vu8(#xC0 9 3 #x50 1 #xA1 1 #x61 #xA3 1 #x61) 
	   (list (->amqp-value :ubyte 1)
		 (->amqp-value :string "a")
		 (->amqp-value :symbol 'a)))

(test-equal ":array (read)"
	    '#(aaa bbb ccc)
	    (vector-map 
	     scheme-value
	     (scheme-value
	      (read-amqp-data
	       (open-bytevector-input-port
		#vu8(224 14 3 163 3 97 97 97 3 98 98 98 3 99 99 99))))))
(test-equal ":array (write)"
	    #vu8(224 14 3 163 3 97 97 97 3 98 98 98 3 99 99 99)
	    (call-with-bytevector-output-port
	     (lambda (out)
	       (write-amqp-data out
				(->amqp-value :array
				   (vector (->amqp-value :symbol 'aaa)
					   (->amqp-value :symbol 'bbb)
					   (->amqp-value :symbol 'ccc)))))))


(test-end)