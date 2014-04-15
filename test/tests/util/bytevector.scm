(import (rnrs)
	(util bytevector)
	(srfi :1)
	(srfi :64 testing))

(test-begin "Extra bytevector utilities tests")

;; TODO more tests
(test-equal "bytevector-xor" #vu8(#x66 #x66)
	    (bytevector-xor #vu8(#x35 #x35) #vu8(#x53 #x53)))

(test-equal "bytevector-ior" #vu8(#x53 #x53)
	    (bytevector-ior #vu8(#x00 #x00) #vu8(#x53 #x53)))

(test-equal "bytevector-and" #vu8(#xF0 #x0F)
	    (bytevector-and #vu8(#xFa #x6F) #vu8(#xF0 #x0F)))

(test-equal "bytevector-slices (normal)"
	    '(#vu8(0 1 2 3) #vu8(4 5 6 7) #vu8(8 9 10 11) #vu8(12 13 14 15))
	    (bytevector-slices (u8-list->bytevector (iota 16)) 4))
(test-equal "bytevector-slices (boundary)" '() (bytevector-slices #vu8() 4))
(test-equal "bytevector-slices (short)"
	    '(#vu8(0 1 2 3) #vu8(4 5 6 7) #vu8(8 9 10 11) #vu8(12))
	    (bytevector-slices (u8-list->bytevector (iota 13)) 4))
(test-equal "bytevector-slices (short)" '(#vu8(0 1))
	    (bytevector-slices (u8-list->bytevector (iota 2)) 4))
(test-equal "bytevector-slices (padding)" 
	    '(#vu8(0 1 2 3) #vu8(4 5 6 7) #vu8(8 9 10 11) #vu8(12 0 0 0))
	    (bytevector-slices (u8-list->bytevector (iota 13)) 4 
			       :padding
			       (lambda (bv) 
				 ;; lazy
				 (bytevector-append bv #vu8(0 0 0)))))
(test-error "bytevector-slices (error)" assertion-violation? 
	    (bytevector-slices (u8-list->bytevector (iota 13)) 0))
(test-error "bytevector-slices (error)" assertion-violation? 
	    (bytevector-slices (u8-list->bytevector (iota 13)) -1))

(test-equal "bytevector-split-at* (normal)" '(#vu8(1 2 3) #vu8(4)) 
	    (receive r (bytevector-split-at* #vu8(1 2 3 4) 3) r))
(test-equal "bytevector-split-at* (boundary)" '(#vu8() #vu8(1 2 3 4)) 
	    (receive r (bytevector-split-at* #vu8(1 2 3 4) 0) r))
(test-equal "bytevector-split-at* (boundary)" '(#vu8(1 2 3 4) #vu8())
	    (receive r (bytevector-split-at* '#vu8(1 2 3 4) 4) r))
(test-error "bytevector-split-at* (error)" assertion-violation?
	    (bytevector-split-at* #vu8(1 2 3 4) -1))
(test-equal "bytevector-split-at* (shorten)" '(#vu8(1 2 3 4) #vu8())
	    (receive r (bytevector-split-at* #vu8(1 2 3 4) 5) r))
(test-equal "bytevector-split-at* (padding)" '(#vu8(1 2 3 4 0 0) #vu8())
	    (receive r (bytevector-split-at* #vu8(1 2 3 4) 6 
			:padding (lambda (bv) 
				   (bytevector-append bv #vu8(0 0)))) r))


(test-end)