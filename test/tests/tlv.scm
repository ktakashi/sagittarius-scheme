(import (rnrs) (tlv) (srfi :64))

(test-begin "TLV library tests")

;; the data is from EMV lab
;; TODO add more tests
(let ()
  (define emv-parser (make-tlv-parser EMV))
  (define tlv-value 
    (integer->bytevector
     #x6F1A840E315041592E5359532E4444463031A5088801025F2D02656E))
  
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
    (test-equal "tlv-data" #vu8(1 2 3 4 5) (tlv-data tlv))
    ;; so far we don't have DGI style writer...
    (test-equal "tlv->bytevector" tlv-value
		(tlv->bytevector tlv :writer write-dgi-tlv))

    (test-equal "write-tlv" tlv-value
		(call-with-bytevector-output-port
		 (lambda (out) (write-dgi-tlv tlv out)))))
)


(test-end)