;; -*- scheme -*-
#!read-macro=sagittarius/regex
(import (rnrs)
	(rfc base64)
	(sagittarius regex)
	(srfi :0 cond-expand)
	(srfi :64 testing))

(define *long-string* 
  ;; from Wikipedia Base64
  "Base64 is a group of similar encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation. The Base64 term originates from a specific MIME content transfer encoding.\n\nBase64 encoding schemes are commonly used when there is a need to encode binary data that needs be stored and transferred over media that are designed to deal with textual data. This is to ensure that the data remains intact without modification during transport. Base64 is used commonly in a number of applications including email via MIME, and storing complex data in XML.")

(define *encoded-long-string*
  "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVz\nZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcg\naXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9uLiBUaGUgQmFzZTY0IHRlcm0gb3JpZ2lu\nYXRlcyBmcm9tIGEgc3BlY2lmaWMgTUlNRSBjb250ZW50IHRyYW5zZmVyIGVuY29kaW5nLgoKQmFz\nZTY0IGVuY29kaW5nIHNjaGVtZXMgYXJlIGNvbW1vbmx5IHVzZWQgd2hlbiB0aGVyZSBpcyBhIG5l\nZWQgdG8gZW5jb2RlIGJpbmFyeSBkYXRhIHRoYXQgbmVlZHMgYmUgc3RvcmVkIGFuZCB0cmFuc2Zl\ncnJlZCBvdmVyIG1lZGlhIHRoYXQgYXJlIGRlc2lnbmVkIHRvIGRlYWwgd2l0aCB0ZXh0dWFsIGRh\ndGEuIFRoaXMgaXMgdG8gZW5zdXJlIHRoYXQgdGhlIGRhdGEgcmVtYWlucyBpbnRhY3Qgd2l0aG91\ndCBtb2RpZmljYXRpb24gZHVyaW5nIHRyYW5zcG9ydC4gQmFzZTY0IGlzIHVzZWQgY29tbW9ubHkg\naW4gYSBudW1iZXIgb2YgYXBwbGljYXRpb25zIGluY2x1ZGluZyBlbWFpbCB2aWEgTUlNRSwgYW5k\nIHN0b3JpbmcgY29tcGxleCBkYXRhIGluIFhNTC4=")

(test-begin "RFC Base64 tests")
;; I should add the cases which specifies transcoder.
(test-equal "base64 encode" "YWJjZGVmZw==" (base64-encode-string "abcdefg"))
(test-equal "base64 decode" "abcdefg" (base64-decode-string "YWJjZGVmZw=="))

(test-equal "base64 encode long"
	    *encoded-long-string* 
	    (base64-encode-string *long-string*))
(test-equal "base64 decode long"
	    *long-string* (base64-decode-string *encoded-long-string*))

(define (test-encode-port name expect v . opt)
  (define (test-out)
    (define (all)
      (define out (open-output-bytevector))
      (define bout (apply open-base64-encode-output-port out opt))
      (put-bytevector bout (string->utf8 v))
      (close-port bout)
      (test-equal (format "~a (output port all)" name) expect 
		  (utf8->string (get-output-bytevector out))))
    (define (one)
      (define out (open-output-bytevector))
      (define bout (apply open-base64-encode-output-port out opt))
      (let ((bv (string->utf8 v)))
	(do ((len (bytevector-length bv)) (i 0 (+ i 1)))
	    ((= i len) (close-port bout))
	  (put-u8 bout (bytevector-u8-ref bv i))))
      (test-equal (format "~a (output port one)" name) expect 
		  (utf8->string (get-output-bytevector out))))
    (all)
    (one))
  (define (test-in)
    (define (all)
      (define in (open-bytevector-input-port (string->utf8 v)))
      (define bin (apply open-base64-encode-input-port in opt))
      (let ((bv (get-bytevector-all bin)))
	(close-port bin)
	(unless (eof-object? bv)
	  (test-equal (format "~a (input port all)" name) expect 
		      (utf8->string bv)))))
    (define (one)
      (define in (open-bytevector-input-port (string->utf8 v)))
      (define bin (apply open-base64-encode-input-port in opt))
      (let-values (((out extract) (open-bytevector-output-port)))
	(do ((b (get-u8 bin) (get-u8 bin)))
	    ((eof-object? b) (close-port bin))
	  (put-u8 out b))
	(test-equal (format "~a (input port one)" name) expect 
		    (utf8->string (extract)))))
    (all)
    (one))

  (test-out)
  (test-in))

(define-syntax test-encode
  (syntax-rules (base64-encode-string)
    ((_ name expect (base64-encode-string v opt ...))
     (begin
       (test-equal name expect (base64-encode-string v opt ...))
       (test-encode-port name expect v opt ...)))))

(test-encode "encode" "" (base64-encode-string ""))
(test-encode "encode" "YQ==" (base64-encode-string "a"))
(test-encode "encode" "MA==" (base64-encode-string "0"))
(test-encode "encode" "Cg==" (base64-encode-string "\n"))
(test-encode "encode" "YTA=" (base64-encode-string "a0"))
(test-encode "encode" "YTAK" (base64-encode-string "a0\n"))
(test-encode "encode" "PQk0" (base64-encode-string "=\t4"))
(test-encode "encode" "eTQ5YQ==" (base64-encode-string "y49a"))
(test-encode "encode" "RWdqYWk=" (base64-encode-string "Egjai"))
(test-encode "encode" "OTNiamFl" (base64-encode-string "93bjae"))
(test-encode "encode" "QkFSMGVyOQ==" (base64-encode-string "BAR0er9"))

;; there is no default line-width on encode port, so this
;; test must be done with test-equal
(test-equal "encode w/ line width (default)"
       "MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2\n"
       (base64-encode-string "012345678901234567890123456789012345678901234567890123456"))
(test-encode "encode w/ line width 10, e1"
       "MDEyMzQ1Ng\n=="
       (base64-encode-string "0123456" :line-width 10))
(test-encode "encode w/ line width 11, e1"
       "MDEyMzQ1Ng=\n="
       (base64-encode-string "0123456" :line-width 11))
(test-encode "encode w/ line width 12, e1"
       "MDEyMzQ1Ng==\n"
       (base64-encode-string "0123456" :line-width 12))
(test-encode "encode w/ line width 11, e2"
       "MDEyMzQ1Njc\n="
       (base64-encode-string "01234567" :line-width 11))
(test-encode "encode w/ line width 12, e2"
       "MDEyMzQ1Njc=\n"
       (base64-encode-string "01234567" :line-width 12))
(test-encode "encode w/ line width 4"
       "MDEy\nMzQ=\n"
       (base64-encode-string "01234" :line-width 4))
(test-encode "encode w/ line width 3"
       "MDE\nyMz\nQ="
       (base64-encode-string "01234" :line-width 3))
(test-encode "encode w/ line width 2"
       "MD\nEy\nMz\nQ=\n"
       (base64-encode-string "01234" :line-width 2))
(test-encode "encode w/ line width 1"
       "M\nD\nE\ny\nM\nz\nQ\n=\n"
       (base64-encode-string "01234" :line-width 1))
(test-encode "encode w/ line width 0"
       "MDEyMzQ="
       (base64-encode-string "01234" :line-width 0))

(define (test-decode-port name expect v)
  (define (test-input)
    (define (test-all)
      (define in (open-bytevector-input-port (string->utf8 v)))
      (define bin (open-base64-decode-input-port in))
      (let ((e (get-bytevector-all bin)))
	(unless (eof-object? e)
	  (test-equal (format "~a (input port all)" name) expect 
		      (utf8->string e))))
      (close-port bin)) ;; just for GC friendliness...
    (define (test1)
      (define in (open-bytevector-input-port (string->utf8 v)))
      (define bin (open-base64-decode-input-port in))
      (let-values (((out extract) (open-string-output-port)))
	(let loop ()
	  (let ((b (get-u8 bin)))
	    (if (eof-object? b)
		(test-equal (format "~a (input port one)" name) expect 
			    (extract))
		(let ((c (integer->char b)))
		  (put-char out c)
		  (loop))))))
      (close-port bin)) ;; just for GC friendliness...
    (test-all)
    (test1))
  (define (test-output)
    (define (test-all)
      (define out (open-output-bytevector))
      (define bout (open-base64-decode-output-port out))
      (put-bytevector bout (string->utf8 v))
      (close-port bout)
      (test-equal (format "~a (output port all)" name) expect 
		  (utf8->string (get-output-bytevector out))))
    (define (test1)
      (define out (open-output-bytevector))
      (define bout (open-base64-decode-output-port out))
      (let ((bv (string->utf8 v)))
	(do ((len (bytevector-length bv))
	     (i 0 (+ i 1)))
	    ((= i len) (close-port bout))
	  (put-u8 bout (bytevector-u8-ref bv i)))) 
      (test-equal (format "~a (output port one)" name) expect 
		  (utf8->string (get-output-bytevector out))))

    (test-all)
    (test1))
  (test-input)
  (test-output))

(define-syntax test-decode
  (syntax-rules (base64-decode-string)
    ((_ name expect (base64-decode-string v))
     (begin
       (test-equal name expect (base64-decode-string v))
       (test-decode-port name expect v)))))

(test-decode "decode" "" (base64-decode-string ""))
(test-decode "decode" "a" (base64-decode-string "YQ=="))
(test-decode "decode" "a" (base64-decode-string "YQ="))
(test-decode "decode" "a" (base64-decode-string "YQ"))
(test-decode "decode" "a0" (base64-decode-string "YTA="))
(test-decode "decode" "a0" (base64-decode-string "YTA"))
(test-decode "decode" "a0\n" (base64-decode-string "YTAK"))
(test-decode "decode" "y49a" (base64-decode-string "eTQ5YQ=="))
(test-decode "decode" "Egjai" (base64-decode-string "RWdqYWk="))
(test-decode "decode" "93bjae" (base64-decode-string "OTNiamFl"))
(test-decode "decode" "BAR0er9" (base64-decode-string "QkFSMGVyOQ=="))
(test-decode "decode" "BAR0er9" (base64-decode-string "QkFS\r\nMGVyOQ\r\n=="))

(test-assert
 (let ()
   (define b64 "MIIDQjCCAiqgAwIBAgIGATz/FuLiMA0GCSqGSIb3DQEBBQUAMGIxCzAJB
       gNVBAYTAlVTMQswCQYDVQQIEwJDTzEPMA0GA1UEBxMGRGVudmVyMRwwGgYD
       VQQKExNQaW5nIElkZW50aXR5IENvcnAuMRcwFQYDVQQDEw5CcmlhbiBDYW1
       wYmVsbDAeFw0xMzAyMjEyMzI5MTVaFw0xODA4MTQyMjI5MTVaMGIxCzAJBg
       NVBAYTAlVTMQswCQYDVQQIEwJDTzEPMA0GA1UEBxMGRGVudmVyMRwwGgYDV
       QQKExNQaW5nIElkZW50aXR5IENvcnAuMRcwFQYDVQQDEw5CcmlhbiBDYW1w
       YmVsbDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAL64zn8/QnH
       YMeZ0LncoXaEde1fiLm1jHjmQsF/449IYALM9if6amFtPDy2yvz3YlRij66
       s5gyLCyO7ANuVRJx1NbgizcAblIgjtdf/u3WG7K+IiZhtELto/A7Fck9Ws6
       SQvzRvOE8uSirYbgmj6He4iO8NCyvaK0jIQRMMGQwsU1quGmFgHIXPLfnpn
       fajr1rVTAwtgV5LEZ4Iel+W1GC8ugMhyr4/p1MtcIM42EA8BzE6ZQqC7VPq
       PvEjZ2dbZkaBhPbiZAS3YeYBRDWm1p1OZtWamT3cEvqqPpnjL1XyW+oyVVk
       aZdklLQp2Btgt9qr21m42f4wTw+Xrp6rCKNb0CAwEAATANBgkqhkiG9w0BA
       QUFAAOCAQEAh8zGlfSlcI0o3rYDPBB07aXNswb4ECNIKG0CETTUxmXl9KUL
       +9gGlqCz5iWLOgWsnrcKcY0vXPG9J1r9AqBNTqNgHq2G03X09266X5CpOe1
       zFo+Owb1zxtp3PehFdfQJ610CDLEaS9V9Rqp17hCyybEpOGVwe8fnk+fbEL
       2Bo3UPGrpsHzUoaGpDftmWssZkhpBJKVMJyf/RuP2SmmaIzmnw9JiSlYhzo
       4tpzd5rFXhjRbg4zW9C+2qok+2+qDM1iJ684gPHMIY8aLWrdgQTxkumGmTq
       gawR+N5MDtdPTEQ0XfIBc2cJEUyMTY5MPvACWpkA6SdS4xSvdXK3IVfOWA==")

   (bytevector=? (base64-decode-string b64 :transcoder #f)
		 (base64-decode-string (regex-replace-all #/\s/ b64 "")
				       :transcoder #f))))


(test-end)
