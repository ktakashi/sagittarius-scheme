(import (rnrs)
	(rfc totp)
	(math)
	(srfi :18)
	(srfi :19)
	(srfi :64))

(test-begin "RFC TOTP")

(test-assert totp)
(test-assert generate-time-based-one-time-password)

(let ((K (string->utf8 "12345678901234567890")))
  (define (stretch-seed K mode)
    (define size (hash-size mode))
    (define len (bytevector-length K))
    (let ((diff (- size len)))
      (if (zero? diff)
	  K
	  (let loop ((n size) (r '()))
	    (if (< n len)
		(bytevector-concatenate
		 (reverse! (cons (bytevector-copy K 0 n) r)))
		(loop (- n len) (cons K r)))))))
  (define test-vectors
    `(
      ;; Time(sec)   TOTP     Mode
      (     59     94287082 , SHA-1 )
      (     59     46119246 ,SHA-256)
      (     59     90693936 ,SHA-512)
      ( 1111111109 07081804 , SHA-1 )
      ( 1111111109 68084774 ,SHA-256)
      ( 1111111109 25091201 ,SHA-512)
      ( 1111111111 14050471 , SHA-1 )
      ( 1111111111 67062674 ,SHA-256)
      ( 1111111111 99943326 ,SHA-512)
      ( 1234567890 89005924 , SHA-1 )
      ( 1234567890 91819424 ,SHA-256)
      ( 1234567890 93441116 ,SHA-512)
      ( 2000000000 69279037 , SHA-1 )
      ( 2000000000 90698825 ,SHA-256)
      ( 2000000000 38618901 ,SHA-512)
      (20000000000 65353130 , SHA-1 )
      (20000000000 77737706 ,SHA-256)
      (20000000000 47863826 ,SHA-512)
      ))
  
  (for-each (lambda (v)
	      (test-equal v (cadr v) (totp (stretch-seed K (caddr v)) 8
					   :time (make-time time-utc 0 (car v))
					   :mode (caddr v))))
	    test-vectors))

(let ((K (string->utf8 "12345678901234567890")))
  (test-equal (totp K 6) (totp K 6))
  (let ((p1 (totp K 6)))
    (thread-sleep! 1)
    (test-assert (not (= p1 (totp K 6 :step 1))))))

(test-end)
