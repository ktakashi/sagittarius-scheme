(import (rnrs) (rnrs eval)
	(srfi :64)
	(binary pack))

(test-begin "Binary pack tests")

(define-syntax check
  (syntax-rules (=>)
    ((_ expr => expect)
     (test-equal (format "~s" 'expr) expect expr))))

(define (check-pack expect fmt . values)
  (let ()
    (check (apply pack fmt values) => expect)
    (check (eval `(pack ,fmt ,@values)
                 (environment '(rnrs) '(binary pack)))
           => expect)
    (check (call-with-values (lambda () (unpack fmt expect)) list)
           => values)
    (check (eval `(call-with-values (lambda () (unpack ,fmt ',expect)) list)
                 (environment '(rnrs) '(binary pack)))
           => values)
    #f))


;; unpack tests from industria
(check (unpack "!C" '#vu8(1 2 3 4 6 0) 5) => 0)
(check (unpack "!S" '#vu8(1 2 3 4 0 0) 4) => 0)

(check (let ((bv (make-bytevector 4 0))) (pack! "!S" bv 2 #xffee) bv)
       => '#vu8(0 0 #xff #xee))

(check (let-values ((x (get-unpack (open-bytevector-input-port #vu8(4 3 2 1 2 1 1 #xff #xff))
                                "<LSC")))
         x)
       => '(#x01020304 #x0102 #x01))
(check (apply get-unpack (open-bytevector-input-port #vu8(#xff))
              "c" '())
       => -1)

(check (unpack "C" #vu8(0 1) 1) => 1)

(check (let ((offset 1)) (unpack "C" #vu8(0 1) offset)) => 1)

(check (let ((offset 1))
         (unpack "!uxxS" #vu8(5 5 5 0 1) offset))
       => 1)

(check (let-values ((x (unpack "4C" #vu8(1 2 3 4)))) x)
       => '(1 2 3 4))

(check (let-values ((x (unpack (car '("4C")) #vu8(1 2 3 4)))) x)
       => '(1 2 3 4))


;; pack tests from industria
(check (pack "!SS" 1 2) => #vu8(0 1 0 2))

(check (let ((bv (make-bytevector 6 #xff))
             (offset 1))
         (pack! "!SS" bv offset 1 2))
       => #vu8(#xff 0 0 1 0 2))

(check (let ((bv (make-bytevector 9 #xff)))
         (pack! "<ucQ" bv (+ 0) 1 2))
       => #vu8(1 2 0 0 0 0 0 0 0))
(check (map pack
            '("C" "!S" "!xxC")
            '(1 1 1))
       => '(#vu8(1) #vu8(0 1) #vu8(0 0 1)))

(check (pack "4C" 1 2 3 4)
       =>
       (pack "CCCC" 1 2 3 4))

(check (pack (car '("4C")) 1 2 3 4)
       =>
       #vu8(1 2 3 4))
;; explicitly native endianness
(check (pack (car '("=l")) 1)
       =>
       (if (eq? (endianness little) (native-endianness))
	   #vu8(1 0 0 0)
	   #vu8(0 0 0 1)))

(check (let ((zero 0)
             (bv (make-bytevector 8 1)))
         (pack! "!L" bv (+ zero 2) #xFFFFFFFF))
       => #vu8(1 1 0 0 255 255 255 255))

;; some indefinite length test
(check (let-values ((r (unpack "*C" #vu8(1 2 3 4)))) r)
       =>
       '(1 2 3 4))

(check (pack "*C" 1 2 3 4)
       =>
       (pack "CCCC" 1 2 3 4))

(check (pack (car '("*C")) 1 2 3 4)
       =>
       #vu8(1 2 3 4))

(check (pack "!2s!*l" 1 2 3 4)
       => #vu8(0 1 0 2 0 0 0 3 0 0 0 4))

(check (pack "2s!*l" 1 2 3 4)
       =>
       (cond-expand
	(little-endian
	 #vu8(1 0 2 0 0 0 0 3 0 0 0 4))
	(big-endian
	 #vu8(0 1 0 2 0 0 0 3 0 0 0 4))))

(check-pack '#vu8() "")
(check-pack '#vu8(0) "x")
(check-pack '#vu8(0 0 0) "3x")
(check-pack '#vu8() "0x")
(check-pack '#vu8() "!0x")
(check-pack '#vu8(#xff) "c" -1)
(check-pack '#vu8(0 #xff) "xC" 255)

(check-pack '#vu8(0 1 0 0 0 0 0 2 0 0 0 0 0 0 0 3) "!SLQ" 1 2 3)
(check-pack '#vu8(0 0 0 0 0 0 0 1 0 0 0 2 0 3) "!QLS" 1 2 3)
(check-pack '#vu8(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 3) "!SQL" 1 2 3)
(check-pack '#vu8(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 3) ">SQL" 1 2 3)
(check-pack '#vu8(1 0 0 0 0 0 0 0 2 0 0 0 3 0) "<QLS" 1 2 3)

(check-pack '#vu8(4 1 0) "u!C S" 4 #x100)
(check-pack '#vu8(4 0 1 0) "u!CaS" 4 #x100)

(check-pack '#vu8(4 0 0 1 0) "u!C L" 4 #x100)

(check-pack '#vu8(4 0 0 0 0 0 0 1 0) "u!C Q" 4 #x100)


(test-end)