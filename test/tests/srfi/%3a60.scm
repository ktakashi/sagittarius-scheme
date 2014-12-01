(import (rnrs)
	(srfi :60)
	(srfi :64))

(test-begin "SRFI-60 Integers as Bits")

;; most of the defined procedures are re-exporting of RnRS ones
;; we only tests something different here.

(test-equal "logand" #b1000 (logand #b1100 #b1010))
(test-equal "logior" #b1110 (logior #b1100 #b1010))
(test-equal "logxor" #b0110 (logxor #b1100 #b1010))

(test-equal "lognot" #b-10000001 (lognot #b10000000))
(test-equal "lognot" #b-1        (lognot #b0))

(test-equal "bitwise-if"
	     #b01100110
	    (bitwise-if #b10101100 #b00110101 #b11001010))

(test-assert "logtest (1)" (not (logtest #b0100 #b1011)))
(test-assert "logtest (2)" (logtest #b0100 #b0111))
(test-assert "logtest (3)" (logtest #xfeedbabe #x10000000))
(test-assert "logtest (4)" (not (logtest #xfeedbabe #x01100101)))

(test-equal "logcount (1)" 4 (logcount #b10101010))
(test-equal "logcount (2)" 0 (logcount 0))
(test-equal "logcount (3)" 1 (logcount -2))

;; from Gauche
(let loop ((a 1)   ; 1, 10, 100, ...
           (b 1)   ; 1, 11, 111, ...
           (c 2)   ; 10, 101, 1001, ...
           (n 1))  ; counter
  (when (< n 69)
    (test-equal (format "logcount (positive, 100...) ~a" n) 1 (logcount a))
    (test-equal (format "logcount (positive, 111...) ~a" n) n (logcount b))
    (test-equal (format "logcount (negative, 100...) ~a" n) 
		(- n 1) (logcount (- a)))
    (test-equal (format "logcount (negative, 100..1) ~a" n) 1 (logcount (- c)))
    (loop (+ b 1) (+ b b 1) (+ b b 3) (+ n 1))))

(test-equal "logbit?" '(#f #t #t #f #t #f #f)
	    (map (lambda (i) (logbit? i #b10110)) '(0 1 2 3 4 5 6)))
(test-equal "logbit?" '(#f #t #f #t #f #t #t)
	    (map (lambda (i) (logbit? i #b-10110)) '(0 1 2 3 4 5 6)))


(test-equal "integer-length" 8 (integer-length #b10101010))
(test-equal "integer-length" 0 (integer-length 0))
(test-equal "integer-length" 4 (integer-length #b1111))

(for-each (lambda (n r) (test-equal (format "log2-binary-factors(~a)" n) r
				    (log2-binary-factors n)))
          '(0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16
              -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16)
          '(-1 0  1  0  2  0  1  0  3  0   1   0   2   0   1   0   4
               0  1  0  2  0  1  0  3  0   1   0   2   0   1   0   4))


(test-assert "logbit?" (logbit? 0 #b1101))
(test-assert "logbit?" (not (logbit? 1 #b1101)))
(test-assert "logbit?" (logbit? 2 #b1101))
(test-assert "logbit?" (logbit? 3 #b1101))
(test-assert "logbit?" (not (logbit? 4 #b1101))) 

(test-equal "copy-bit" 1      (copy-bit 0 0 #t))
(test-equal "copy-bit" #b100  (copy-bit 2 0 #t))
(test-equal "copy-bit" #b1011 (copy-bit 2 #b1111 #f))
(test-equal "copy-bit" #b11010110 (copy-bit 4 #b11000110 #t))
(test-equal "copy-bit" #b11000110 (copy-bit 4 #b11000110 #f))
(test-equal "copy-bit" #b10000110 (copy-bit 6 #b11000110 #f))


(test-equal "copy-bit-field" #b1101100000 (copy-bit-field #b1101101010 0 0 4))
(test-equal "copy-bit-field" #b1101101111 (copy-bit-field #b1101101010 -1 0 4))
(test-equal "copy-bit-field" #b1111111111101010 
	    (copy-bit-field #b1101101010 -1 5 16))

(define (test-rotate-bit-field n c s e r)
  (test-equal
   (format "rotate-bit-field(~s,~a,~a,~a)" (number->string n 2) c s e) r
   (number->string (rotate-bit-field n c s e) 2)))

(test-rotate-bit-field #b0100 3 4 0 "100") ; trivial path
(test-rotate-bit-field #b0100 3 0 4 "10")
(test-rotate-bit-field #b0100 -1 0 4 "10")
(test-rotate-bit-field #b0100 10 0 4 "1")
(test-rotate-bit-field #b110100100010000 -1 5 9 "110100010010000")
(test-rotate-bit-field #b110100100010000 1 5 9 "110100000110000")

(define (test-reverse-bit-field n s e r)
  (test-equal (format "reverse-bit-field(~s,~a,~a)" (number->string n 2) s e) r
	      (number->string (reverse-bit-field n s e) 2)))
(test-reverse-bit-field #xa7 8 0 "10100111")
(test-reverse-bit-field #xa7 0 8 "11100101")
(test-reverse-bit-field #xa7 1 5 "10111001")

(test-equal "integer->list" '(#t #f #f #t) (integer->list 9))
(test-equal "integer->list" '(#f #f #t #f #f #t) (integer->list 9 6))
(test-equal "list->integer" 9 (list->integer '(#t #f #f #t)))
(test-equal "list->integer" 9 (list->integer '(#f #f #t #f #f #t)))
;; tests bignum path
(test-equal "list->integer" (+ (expt 2 63) (expt 2 62) (expt 2 31) (expt 2 30) 1)
	    (list->integer '(#t #t #f #f #f #f #f #f
			     #f #f #f #f #f #f #f #f
			     #f #f #f #f #f #f #f #f
			     #f #f #f #f #f #f #f #f
			     #t #t #f #f #f #f #f #f
			     #f #f #f #f #f #f #f #f
			     #f #f #f #f #f #f #f #f
			     #f #f #f #f #f #f #f #t)))

(test-equal "booleans->integer" 9 (booleans->integer #f #f #t #f #f #t))


(test-end)
