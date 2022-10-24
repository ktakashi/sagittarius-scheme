(import (rnrs)
	(clos core)
	(clos user)
	(srfi :64)
	(srfi :117)
	(util bytevector)
	(sagittarius crypto asn1)
	(sagittarius crypto asn1 modules))

(test-begin "ASN1 read/write")

(define (test-asn1-read/write bv obj)
  (let ((written (asn1-encodable->bytevector obj)))
    (test-equal bv written)
    (let ((read (bytevector->asn1-object bv)))
      (test-assert (asn1-object? read))
      (test-equal (format "bytevector->asn1-object: ~a ~a"
			  (bytevector->hex-string bv)
			  (class-of obj))
		  obj read))))

(test-asn1-read/write #vu8(1 1 #xFF) (boolean->der-boolean #t))
(test-asn1-read/write #vu8(1 1 #x0) (boolean->der-boolean #f))

;; Integer
(test-asn1-read/write #vu8(2 2 0 #xFF) (integer->der-integer #xFF))
(test-asn1-read/write #vu8(2 1 #x0) (integer->der-integer 0))
(test-asn1-read/write #vu8(2 2 #xFF 1) (integer->der-integer #x-FF))

;; Bit string
(test-asn1-read/write #vu8(3 2 0 #xFF) (bytevector->der-bit-string #vu8(#xFF)))
(test-asn1-read/write #vu8(3 2 5 #xFF) 
 (bytevector->der-bit-string #vu8(#xFF) 5))

;; Octet string
(test-asn1-read/write #vu8(4 0) (bytevector->der-octet-string #vu8()))
(test-asn1-read/write #vu8(4 3 1 2 3) 
 (bytevector->der-octet-string #vu8(1 2 3)))

;; null
(test-asn1-read/write #vu8(5 0) (make-der-null))

;; oid
(test-asn1-read/write #vu8(6 3 42 3 4) 
 (oid-string->der-object-identifier "1.2.3.4"))
(test-asn1-read/write #vu8(6 9 96 134 72 1 101 3 4 2 1)
 (oid-string->der-object-identifier "2.16.840.1.101.3.4.2.1"))

;; external
(test-asn1-read/write #vu8(40 22 6 3 42 3 4 2 1 1 4 4 1 2 3 4 162 6 4 4 5 6 7 8)
 (make <der-external>
   :direct-reference (oid-string->der-object-identifier "1.2.3.4")
   :indirect-reference 1
   :data-value-descriptor (bytevector->der-octet-string #vu8(1 2 3 4))
   :encoding (make <der-tagged-object>
	       :tag-no 2 :explicit? #t
	       :obj (bytevector->der-octet-string #vu8(5 6 7 8)))))

;; enumerated
(test-asn1-read/write #vu8(10 1 1) (integer->der-enumerated 1))
(test-asn1-read/write #vu8(10 1 #xFF) (integer->der-enumerated -1))

;; sequence
(test-asn1-read/write #vu8(48 0) (make-der-sequence '()))
(test-asn1-read/write #vu8(48 3 1 1 #xFF)
  (make-der-sequence (list (boolean->der-boolean #t))))

;; set
(test-asn1-read/write #vu8(49 0) (make-der-set '()))
(test-asn1-read/write #vu8(49 3 1 1 #xFF) 
 (make-der-set (list (boolean->der-boolean #t))))

;; numeric string
(test-asn1-read/write #vu8(18 5 49 50 51 52 53) 
 (string->der-numeric-string "12345"))
(test-asn1-read/write #vu8(18 6 49 50 51 32 52 53)
		(string->der-numeric-string "123 45"))
;; This is rather weird but we don't check at this moment
(test-asn1-read/write #vu8(18 7 49 50 51 32 52 53 97)
		(string->der-numeric-string "123 45a"))

;; printable string
(test-asn1-read/write #vu8(19 8 65 66 67 68 32 69 70 36)
		(string->der-printable-string "ABCD EF$"))

;; T61 string
(test-asn1-read/write #vu8(20 8 65 66 67 68 32 69 70 36)
		(string->der-t61-string "ABCD EF$"))

;; Videotex string
(test-asn1-read/write #vu8(21 8 65 66 67 68 32 69 70 36)
		(string->der-videotex-string "ABCD EF$"))

;; IA5 string
(test-asn1-read/write #vu8(22 8 65 66 67 68 32 69 70 36)
		(string->der-ia5-string "ABCD EF$"))

;; UTC time
(test-asn1-read/write #vu8(23 13 50 50 48 55 49 50 48 57 49 57 53 49 90)
		      (string->der-utc-time "220712091951Z"))

;; Generalized time
(test-asn1-read/write #vu8(24 15 50 48 50 50 48 55 49 50 48 57 49 57 53 49 90)
		      (string->der-generalized-time "20220712091951Z"))

;; Graphic string
(test-asn1-read/write #vu8(25 8 65 66 67 68 32 69 70 36)
		(string->der-graphic-string "ABCD EF$"))

;; Visible string
(test-asn1-read/write #vu8(26 8 65 66 67 68 32 69 70 36)
		(string->der-visible-string "ABCD EF$"))

;; General string
(test-asn1-read/write #vu8(27 8 65 66 67 68 32 69 70 36)
		(string->der-general-string "ABCD EF$"))

;; Universal string
(test-asn1-read/write #vu8(28 8 65 66 67 68 32 69 70 36)
		(string->der-universal-string "ABCD EF$"))

;; BMP string
(test-asn1-read/write #vu8(30 16 0 65 0 66 0 67 0 68 0 32 0 69 0 70 0 36)
		(string->der-bmp-string "ABCD EF$"))

;; UTF8 string
(test-asn1-read/write #vu8(12 8 65 66 67 68 32 69 70 36)
		(string->der-utf8-string "ABCD EF$"))

;; application specific
(test-asn1-read/write
 #vu8(#x44 #x5 #x68 #x65 #x6c #x6c #x6f)
 (make <der-application-specific>
   :constructed? #f
   :tag *asn1:octet-string*
   :octets (string->utf8 "hello")))

(test-asn1-read/write
 #vu8(112 9 48 7 4 5 104 101 108 108 111)
 (make <der-application-specific>
   :constructed? #t
   :tag *asn1:sequence*
   :octets (asn1-encodable->bytevector
	    (make-der-sequence
	     (list (bytevector->der-octet-string (string->utf8 "hello")))))))

;; tagged object
;; empty
(test-asn1-read/write #vu8(161 0)
 (make <der-tagged-object> :tag-no 1 :explicit? #t :obj #f))
(test-asn1-read/write #vu8(129 0)
 (make <der-tagged-object> :tag-no 1 :explicit? #f :obj #f))
(test-asn1-read/write #vu8(161 2 5 0)
 (make <der-tagged-object> :tag-no 1 :explicit? #t :obj (make-der-null)))

;; (make-der-tagged-object 1 #f (make-der-integer 1))
;; Above will lose the type info, we reader would only recognise it
;; as a octet string
(test-asn1-read/write #vu8(129 1 1)
 (make <der-tagged-object> :tag-no 1 :explicit? #f
       :obj (bytevector->der-octet-string #vu8(1))))

(test-asn1-read/write #vu8(161 3 2 1 1)
 (make <der-tagged-object> :tag-no 1 :explicit? #t
       :obj (integer->der-integer 1)))


(define-asn1-encodable <algorithm-identifier>
  (asn1-sequence
   ((algorithm :type <der-object-identifier>)
    (parameter :type <asn1-encodable> :optional #t))))
(let* ((aid-der (der-sequence
		 (oid-string->der-object-identifier "1.1.1.1")
		 (make-der-null)))
       (aid (asn1-object->asn1-encodable <algorithm-identifier> aid-der)))
  (test-equal aid-der (asn1-encodable->asn1-object aid 'der)))

(test-end)
