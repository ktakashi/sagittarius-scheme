#!read-macro=sagittarius/regex
(import (rnrs)
	(tlv)
	(util bytevector)
	(sagittarius regex)
	(srfi :64))

(test-begin "TLV library tests")

;; the data is from EMV lab
;; TODO add more tests
(let ()
  (define emv-parser (make-tlv-parser EMV))
  (define tlv-value 
    (integer->bytevector
     #x6F1A840E315041592E5359532E4444463031A5088801025F2D02656E))
  (define tlv-structure '((#xEF (#xA0 (#x80 . #vu8(1 2 3 4 5)))
				(#xA1 (#x80 1 2 3 4 5))
				(#xA1 (#x80 . #x123456)))
			  (#xC9)))
  (test-assert "tlv-object?"
	       (tlv-object? (call-with-port
				(open-bytevector-input-port tlv-value)
			      emv-parser)))
  (let ((tlv (call-with-port (open-bytevector-input-port tlv-value)
	       emv-parser)))
    (test-equal "tlv-tag" #x6F (tlv-tag tlv))
    (test-assert "tlv-data" (not (tlv-data tlv)))
    (test-assert "tlv-components" (not (null? (tlv-components tlv))))

    (test-equal "tlv->bytevector" tlv-value (tlv->bytevector tlv))

    (test-equal "write-tlv" tlv-value
		(call-with-bytevector-output-port
		 (lambda (out) (write-tlv tlv out))))
    )

  (test-equal "read-tlv" 2 (length (read-tlv (open-bytevector-input-port
					      (bytevector-concatenate
					       (map tlv->bytevector 
						    (->tlv tlv-structure))))
					     emv-parser)))

)

(let ()
  (define dgi-parser (make-tlv-parser DGI))
  (define tlv-value #vu8(00 01 05 01 02 03 04 05))
  
  (test-assert "tlv-object?"
	       (tlv-object? (call-with-port
				(open-bytevector-input-port tlv-value)
			      dgi-parser)))
  (let ((tlv (call-with-port (open-bytevector-input-port tlv-value)
	       dgi-parser)))
    (test-equal "tlv-tag" #x0001 (tlv-tag tlv))
    (test-equal "tlv-data (dgi)" #vu8(1 2 3 4 5) (tlv-data tlv))
    ;; so far we don't have DGI style writer...
    (test-equal "tlv->bytevector" tlv-value
		(tlv->bytevector tlv :writer write-dgi-tlv))

    (test-equal "write-tlv" tlv-value
		(call-with-bytevector-output-port
		 (lambda (out) (write-dgi-tlv tlv out)))))
)

;; some of the length related issue tests
(let ()
  (define (make-bogus-tlv-bytevector len)
    (let ((tlv (make-tlv-unit #x80 (make-bytevector len #xFF))))
      (open-bytevector-input-port (tlv->bytevector tlv))))

  (test-assert "length #x7F" (read-tlv (make-bogus-tlv-bytevector #x7F)))
  (test-assert "length #xFF" (read-tlv (make-bogus-tlv-bytevector #xFF)))
  (test-assert "length #xFFFF" (read-tlv (make-bogus-tlv-bytevector #xFFFF)))
  (test-assert "length #x1FFFF" (read-tlv (make-bogus-tlv-bytevector #x1FFFF))))

;; multiple length tags
(let ()
  (define (bytevector->tlv bv) 
    (car (read-tlv (open-bytevector-input-port bv))))
  (test-equal "3 length tag" #x1FFF7F
	      (tlv-tag (bytevector->tlv  #vu8(#x1F #xFF #x7F #x00))))
  (test-equal "4 length tag" #x1FFFFF7F
	      (tlv-tag (bytevector->tlv  #vu8(#x1F #xFF #xFF #x7F #x00)))))

;; LV
(let ()
  (define LV "41 042cb5deaf002307ea866c32af628a59\
                 a5b24d3cefc615ba924660819ee96461\
                 9770f3175e53e2bfea58bad28465ed1e\
                 2dc463c8ae33a950ae54136c2cd6f8c8\
                 08\
              1d a61b9002aa0295011080018881011083\
                 01319103000000450411223344\
              10 601c32691f0c339b1136770a2ab060cd\
              03 f2367b\
              40 afcb1b01887d58d0e628e2bc2986f41d\
                 82de36e35e1cd35413032bc7fa8c3b2f\
                 d88492edff456712c9e5f805b3e0cae2\
                 142e12a3b37099a10763986a39da4690")
  (define Vs '("042cb5deaf002307ea866c32af628a59\
                a5b24d3cefc615ba924660819ee96461\
                9770f3175e53e2bfea58bad28465ed1e\
                2dc463c8ae33a950ae54136c2cd6f8c8\
                08"
	       "a61b9002aa0295011080018881011083\
                01319103000000450411223344"
	       "601c32691f0c339b1136770a2ab060cd"
	       "f2367b"
	       "afcb1b01887d58d0e628e2bc2986f41d\
                82de36e35e1cd35413032bc7fa8c3b2f\
                d88492edff456712c9e5f805b3e0cae2\
                142e12a3b37099a10763986a39da4690"))
  (test-equal "read-lv"
	      (map hex-string->bytevector Vs)
	      (read-lv (open-bytevector-input-port
			(hex-string->bytevector
			 (regex-replace-all #/\s+/ LV "")))))
  )

(test-end)
