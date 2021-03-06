(import (rnrs)
	(sagittarius)
	(sagittarius control)
	(clos user)
	(rfc pem)
	(asn.1)
	(srfi :64 testing))

(test-begin "RFC PEM (RFC 1421) tests")

(define test-file-1 (string-append (current-directory) "/test/data/pem-1.pem"))
(define test-file-2 (string-append (current-directory) "/test/data/pem-2.pem"))

(receive (header content) (parse-pem-file test-file-1)
  (test-assert "PEM-1 header" (not (null? header)))
  (test-equal "PEM-1 header count" 8 (length header))
  (test-assert "PEM-1 content" (bytevector? content)))

(receive (header content) (parse-pem-file test-file-2 :asn1 #t)
  (test-assert "PEM-2 header" (null? header))
  (test-assert "PEM-2 content" (is-a? content <asn.1-encodable>)))

(let ((r (parse-pem-file test-file-2 :multiple #t :asn1 #t)))
  (test-equal "PEM-2 (multiple)" 2 (length r))
  (dolist (l r)
    (test-assert "PEM-2 (multiple) header" (null? (car l)))
    (test-assert "PEM-2 (multiple) content" (is-a? (cdr l) <asn.1-encodable>))))

;; NB: 'foo' = 'Zm9v'
;;     'bar' = 'YmFy'
(define content
  "-----BEGIN FOO-----\r\n\
   Zm9v\r\n\
   -----BEGIN BAR-----\r\n\
   YmFy\r\n\
   -----END BAR-----\r\n")
(let ((s content))
  (test-equal "multiple values without END"
	      '((() . "foo") (() . "bar"))
	      (parse-pem-string s :multiple #t :builder utf8->string)))

(let ((s content))
  (test-equal "PEM :decoder #f"
	      ;; decoder #f have crlf even on the last line
	      '((() . "Zm9v\r\n") (() . "YmFy\r\n"))
	      (parse-pem-string s :multiple #t :builder utf8->string
				:decoder #f)))

;; condition test
(let ((s content))
  (test-error "PEM :multiple #f" pre-eb-as-boundary? (parse-pem-string s)))
(let ((s content))
  (test-error "PEM invalid format" invalid-pem-format? (parse-pem-string "")))

(test-end)
