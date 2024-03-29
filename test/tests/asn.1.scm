(import (rnrs)
	(sagittarius)
	(asn.1)
	(crypto)
	(clos user)
	(srfi :19)
	(srfi :64 testing))

(test-begin "ASN.1 test")

(define *private-key* (string-append (current-directory)
				     "/test/data/private-key.der"))
(define *public-key*  (string-append (current-directory)
				     "/test/data/public-key.der"))

(define *private-key-data* (call-with-input-file *private-key*
			     (lambda (p) (get-bytevector-all p))
			     :transcoder #f))
(define *public-key-data* (call-with-input-file *public-key*
			    (lambda (p) (get-bytevector-all p))
			    :transcoder #f))

(define *encoded* #vu8(48 22 12 11 84 101 115 116 32 115 116 114
			  105 110 103 2 2 48 57 5 0 1 1 255))

(test-assert "asn.1-object?" (make-der-integer 1))

(let ((obj (make-der-sequence (make-der-utf8-string "Test string")
			      (make-der-integer 12345)
			      (make-der-null)
			      (make-der-boolean #t))))
  ;; check hierarchy
  (test-assert "der-sequence?" (is-a? obj <der-sequence>))
  (test-assert "asn.1-sequence?" (is-a? obj <asn.1-sequence>))
  (test-assert "asn.1-object?" (asn.1-object? obj))
  (test-assert "der-object?" (is-a? obj <der-object>))
  (test-assert "asn.1-encodable?" (is-a? obj <asn.1-encodable>))
  (test-equal "encoded" *encoded* (encode obj)))

(call-with-input-file *private-key*
  (lambda (p)
    (let ((o (read-asn.1-object p)))
      ;; just check
      (test-assert "sequence?(private key)" (is-a? o <der-sequence>))
      (test-equal "data check(private key)" *private-key-data*
		  (encode o))))
  :transcoder #f)
(call-with-input-file *private-key*
  (lambda (p)
    (let ((key (import-private-key RSA p)))
      (test-assert "private-key?" (private-key? key))))
  :transcoder #f)

(call-with-input-file *public-key*
  (lambda (p)
    (let ((o (read-asn.1-object p)))
      ;; just check
      (test-assert "sequence?(public key)" (is-a? o <der-sequence>))
      (test-equal "data check(public key)" *public-key-data*
		  (encode o))))
  :transcoder #f)
(call-with-input-file *public-key*
  (lambda (p)
    (let ((key (import-public-key RSA p)))
      (test-assert "public-key?" (public-key? key))))
  :transcoder #f)

(test-equal "der integer (unsigned)" #vu8(2 2 0 255)
	    (asn.1-encode (make-der-integer #xFF)))
(test-equal "der integer (signed)" #vu8(2 1 255)
	    (asn.1-encode (make-der-integer -1)))

(let ((d (make-date 0 0 0 0 31 1 2018 0)))
  (test-assert (time=? (date->time-utc d)
		       (date->time-utc (der-time->date (make-der-utc-time d)))))
  (test-assert (time=? (date->time-utc d)
		       (date->time-utc (der-time->date (make-der-utc-time "180131000000Z"))))))

(test-assert (is-a? (read-asn.1-object (open-bytevector-input-port #vu8(112 9 48 7 4 5 104 101 108 108 111))) <der-application-specific>))

(test-end)
