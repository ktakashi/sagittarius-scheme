(import (rnrs)
	(srfi :64)
	(binary pack))

(test-begin "Binary pack tests")

(define-syntax check
  (syntax-rules (=>)
    ((_ expr => expect)
     (test-equal (format "~s" 'expr) expect expr))))

;; pack tests from industria
(check (pack "!aSS" 1 2) => #vu8(0 1 0 2))

(check (let ((bv (make-bytevector 6 #xff))
             (offset 1))
         (pack! "!aSS" bv offset 1 2))
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

(check (let ((zero 0)
             (bv (make-bytevector 8 1)))
         (pack! "!L" bv (+ zero 2) #xFFFFFFFF))
       => #vu8(1 1 0 0 255 255 255 255))

;; some indefinite length test
(check (pack "*C" 1 2 3 4)
       =>
       (pack "CCCC" 1 2 3 4))

(check (pack (car '("*C")) 1 2 3 4)
       =>
       #vu8(1 2 3 4))

(check (pack "2s!*l" 1 2 3 4)
       =>
       #vu8(1 0 2 0 0 0 0 3 0 0 0 4))

(test-end)