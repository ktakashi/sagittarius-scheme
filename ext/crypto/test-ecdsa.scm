;; this file is included from test.scm

;; no hash
(define-syntax define-no-hash
  (syntax-rules ()
    ((_ name marker size)
     (begin
       (define-class <no-hash> (<user-hash-algorithm>)
	 ((buffer :init-form (make-bytevector size))))
       (define-method initialize ((o <no-hash>) initargs)
	 (call-next-method)
	   (slot-set! o 'init values)
	   (slot-set! o 'process
		      (lambda (h bv)
			(bytevector-copy! bv 0 (slot-ref h 'buffer) 0 size)))
	   (slot-set! o 'done
		      (lambda (h out)
			(bytevector-copy! (slot-ref h 'buffer) 0 out 0 size)))
	   (slot-set! o 'block-size 16)
	   (slot-set! o 'hash-size size)
	   (slot-set! o 'oid #f)
	   (slot-set! o 'state #f))
       (define name marker)
       (define dummy (register-hash name <no-hash>))))))

(define-no-hash no-20 :no-20 20) ;; sha1
(define-no-hash no-28 :no-38 28) ;; sha224
(define-no-hash no-32 :no-32 32) ;; sha256

;; test vector from NIST
#|
Msg = #x608079423f12421de616b7493ebe551cf4d65b92
d =   #xe14f37b3d1374ff8b03f41b9b3fdd2f0ebccf275d660d7f3
Qx =  #x07008ea40b08dbe76432096e80a2494c94982d2d5bcf98e6
Qy =  #x76fab681d00b414ea636ba215de26d98c41bd7f2e4d65477
k =   #xcb0abc7043a10783684556fb12c4154d57bc31a289685f25
R =   #x6994d962bdd0d793ffddf855ec5bf2f91a9698b46258a63e
S =   #x02ba6465a234903744ab02bc8521405b73cf5fc00e1a9f41
|#
(define (test-ecdsa param hash msg d Qx Qy k R S)
  (define priv (generate-private-key ECDSA d param))
  (define pub (generate-public-key ECDSA Qx Qy))
  (define cipher (make-cipher ECDSA priv))

  (let ((sig (cipher-signature cipher (uinteger->bytevector msg) :hash hash
			       :k-generator (lambda (n d) k)
			       :der-encode #f)))
    (test-equal (bytevector-append (uinteger->bytevector R)
				   (uinteger->bytevector S))
		sig)))

(test-ecdsa NIST-P-192 no-20
	    #x608079423f12421de616b7493ebe551cf4d65b92
	    #xe14f37b3d1374ff8b03f41b9b3fdd2f0ebccf275d660d7f3
	    #x07008ea40b08dbe76432096e80a2494c94982d2d5bcf98e6
	    #x76fab681d00b414ea636ba215de26d98c41bd7f2e4d65477
	    #xcb0abc7043a10783684556fb12c4154d57bc31a289685f25
	    #x6994d962bdd0d793ffddf855ec5bf2f91a9698b46258a63e
	    #x02ba6465a234903744ab02bc8521405b73cf5fc00e1a9f41)

;; TODO add more


