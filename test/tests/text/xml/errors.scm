(import (rnrs)
	(text xml errors)
	(srfi :64))


(test-begin "XQuery, XSLT, and XPath Error Codes")

(define (test-xqt-error code msg)
  (define (check e)
    (test-assert code (xqt-error? e))
    (test-equal code (xqt-error-code e))
    (test-equal msg (xqt-error-description e)))
  (guard (e (else (check e)))
    (xqt-error code 'who "message")))

(for-each (lambda (c&m)
	    (apply test-xqt-error c&m)) +xqt-errors+)

(test-end)
