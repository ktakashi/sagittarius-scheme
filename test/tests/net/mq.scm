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
		      (write-primitive-amqp-data out type vv))))
       (test-assert (format "compare(~a)" type)
		    (equal? (->amqp-value type vv)
			    (->amqp-value type vv)))))))

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

(let ((ht (make-eq-hashtable))
      (bv #vu8(#xC1 37 6 
		    #xA3 4 107 101 121 49
		    #xA1 4 118 97 108 49
		    #xA3 4 107 101 121 50
		    #xA1 4 118 97 108 50
		    #xA3 4 107 101 121 51
		    #xA1 4 118 97 108 51)))
  (define (read-map bv)
    (list-sort
     (lambda (al bl) (string<? (cdr al) (cdr bl)))
     (map (lambda (p)
	    (cons (scheme-value (car p))
		  (scheme-value (cdr p))))
	  (hashtable->alist 
	   (scheme-value
	    (read-amqp-data (open-bytevector-input-port bv)))))))
  (hashtable-set! ht (->amqp-value :symbol 'key1)
		  (->amqp-value :string "val1"))
  (hashtable-set! ht (->amqp-value :symbol 'key2)
		  (->amqp-value :string "val2"))
  (hashtable-set! ht (->amqp-value :symbol 'key3)
		  (->amqp-value :string "val3"))
  
  ;; map
  (test-equal ":map (read)"
	      '((key1 . "val1") (key2 . "val2") (key3 . "val3"))
	      (read-map bv))
  ;; comparing by bytevector won't be equal for current implementation
  (test-equal ":map (write)"
	      '((key1 . "val1") (key2 . "val2") (key3 . "val3"))
	      (read-map 
	       (call-with-bytevector-output-port
		(lambda (out)
		  (write-primitive-amqp-data out :map ht))))))

;; composite
(define-composite-type book example:book:list #x00000003 #x00000002
  ((title   :type :string :mandatory #t)
   (authors :type :string :multiple #t)
   (isbn    :type :string)))

(test-equal "descriptor (name)" 'example:book:list 
	    (slot-ref <amqp-book> 'descriptor-name))
(test-equal "descriptor (code)" #x0000000300000002 
	    (slot-ref <amqp-book> 'descriptor-code))

(test-assert "make-amqp-book" make-amqp-book)
(define book-bv 
  #vu8(#x00 ;; composite 
       ;; the writer writes ulong
       ;; #xA3 #x11 ;; sym8 & size
       ;;  101 120 97 109 112 108 101 58 98 111 111 107 58 108 105 115 116
       #x80 0 0 0 3 0 0 0 2
       #xC0 ;; list
         #x40 ;; size
	 #x03 ;; element
       #xA1 #x15 ;; sym256 & size
         65 77 81 80 32 102 111 114 32 38 32 98 121 32 68 117 109 109 105 101 115
       #xE0 ;; array
         #x25 ;; size
	 #x02 ;; count
	 #xA1 ;; constructor
	 #x0E 82 111 98 32 74 46 32 71 111 100 102 114 101 121
	 #x13 82 97 102 97 101 108 32 72 46 32 83 99 104 108 111 109 105 110 103
       #x40))

;; TODO more tests
(let ((book (make-amqp-book 
	     :title "AMQP for & by Dummies"
	     :authors '("Rob J. Godfrey" "Rafael H. Schloming"))))

  (test-equal "title" "AMQP for & by Dummies" (slot-ref book 'title))
  (test-equal "authors" '("Rob J. Godfrey" "Rafael H. Schloming")
	      (slot-ref book 'authors))
  (test-assert "isbn" (not (slot-bound? book 'isbn)))
  (test-equal "write book" book-bv
	      (call-with-bytevector-output-port 
	       (lambda (out) (write-amqp-data out book)))))

(let ((book (read-amqp-data (open-bytevector-input-port book-bv))))
  (test-assert "amqp-book?" (amqp-book? book))
  (test-equal "title" "AMQP for & by Dummies" (slot-ref book 'title))
  (test-equal "authors" '("Rob J. Godfrey" "Rafael H. Schloming")
	      (slot-ref book 'authors))
  (test-assert "isbn" (not (slot-bound? book 'isbn))))

;; restricted with descriptor
(define-restricted-type ext-list :list
  :descriptor (ext:list #x00 #xF1))

(let ((bv (bytevector-append #vu8(#x00 #x53 #xF1)
			     (amqp-primitive-value->bytevector
			      :list
			      (list (->amqp-value :ubyte 1)
				    (->amqp-value :string "a")
				    (->amqp-value :symbol 'a))))))
  (test-assert "read (ext-list)" (bytevector->amqp-value bv))
  (let ((el (bytevector->amqp-value bv)))
    (test-assert "amqp-list" (amqp-list? (scheme-value el)))
    (test-equal "write (ext-list)" bv (amqp-value->bytevector el)))
  (let ((bv2 (bytevector-append #vu8(#x00 #x53 #xF1)
				(amqp-primitive-value->bytevector :symbol 'a))))
    (test-error "read error (ext-list)" condition? 
		(bytevector->amqp-value bv2))))

(test-end)