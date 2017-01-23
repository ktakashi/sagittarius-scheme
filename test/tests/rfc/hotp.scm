(import (rnrs)
	(rfc hotp)
	(srfi :64))

(test-begin "RFC HOTP")

(test-assert hotp)
(test-assert generate-hmac-one-time-password)

;; From Appendix D
(let ((secret (string->utf8 "12345678901234567890")))
  (define test-vectors
    ;; Truncated
    ;; Count    Hexadecimal      Decimal        HOTP
    '((0        #x4c93cf18       1284755224     755224)
      (1        #x41397eea       1094287082     287082)
      (2         #x82fef30        137359152     359152)
      (3        #x66ef7655       1726969429     969429)
      (4        #x61c5938a       1640338314     338314)
      (5        #x33c083d4        868254676     254676)
      (6        #x7256c032       1918287922     287922)
      (7         #x4e5b397         82162583     162583)
      (8        #x2823443f        673399871     399871)
      (9        #x2679dc69        645520489     520489)))

  (for-each (lambda (v)
	      (test-equal v (cadddr v) (hotp secret (car v) 6)))
	    test-vectors))
  

(test-end)
