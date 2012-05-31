;; -*- scheme -*-
(import (rnrs)
	(rfc base64)
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
	    (base64-encode-string *long-string*
				  :transcoder
				  (cond-expand
				   (sagittarius.os.windows
				    (make-transcoder (utf-8-codec) 'lf))
				   (else
				    (native-transcoder)))))
(test-equal "base64 decode long"
	    *long-string* (base64-decode-string *encoded-long-string*))

;; from Gauche
(test-equal "encode" "" (base64-encode-string ""))
(test-equal "encode" "YQ==" (base64-encode-string "a"))
(test-equal "encode" "MA==" (base64-encode-string "0"))
(test-equal "encode" "Cg==" (base64-encode-string "\n"))
(test-equal "encode" "YTA=" (base64-encode-string "a0"))
(test-equal "encode" "YTAK" (base64-encode-string "a0\n"))
(test-equal "encode" "PQk0" (base64-encode-string "=\t4"))
(test-equal "encode" "eTQ5YQ==" (base64-encode-string "y49a"))
(test-equal "encode" "RWdqYWk=" (base64-encode-string "Egjai"))
(test-equal "encode" "OTNiamFl" (base64-encode-string "93bjae"))
(test-equal "encode" "QkFSMGVyOQ==" (base64-encode-string "BAR0er9"))

(test-equal "encode w/ line width (default)"
       "MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2\n"
       (base64-encode-string "012345678901234567890123456789012345678901234567890123456"))
(test-equal "encode w/ line width 10, e1"
       "MDEyMzQ1Ng\n=="
       (base64-encode-string "0123456" :line-width 10))
(test-equal "encode w/ line width 11, e1"
       "MDEyMzQ1Ng=\n="
       (base64-encode-string "0123456" :line-width 11))
(test-equal "encode w/ line width 12, e1"
       "MDEyMzQ1Ng==\n"
       (base64-encode-string "0123456" :line-width 12))
(test-equal "encode w/ line width 11, e2"
       "MDEyMzQ1Njc\n="
       (base64-encode-string "01234567" :line-width 11))
(test-equal "encode w/ line width 12, e2"
       "MDEyMzQ1Njc=\n"
       (base64-encode-string "01234567" :line-width 12))
(test-equal "encode w/ line width 4"
       "MDEy\nMzQ=\n"
       (base64-encode-string "01234" :line-width 4))
(test-equal "encode w/ line width 3"
       "MDE\nyMz\nQ="
       (base64-encode-string "01234" :line-width 3))
(test-equal "encode w/ line width 2"
       "MD\nEy\nMz\nQ=\n"
       (base64-encode-string "01234" :line-width 2))
(test-equal "encode w/ line width 1"
       "M\nD\nE\ny\nM\nz\nQ\n=\n"
       (base64-encode-string "01234" :line-width 1))
(test-equal "encode w/ line width 0"
       "MDEyMzQ="
       (base64-encode-string "01234" :line-width 0))

(test-equal "decode" "" (base64-decode-string ""))
(test-equal "decode" "a" (base64-decode-string "YQ=="))
(test-equal "decode" "a" (base64-decode-string "YQ="))
(test-equal "decode" "a" (base64-decode-string "YQ"))
(test-equal "decode" "a0" (base64-decode-string "YTA="))
(test-equal "decode" "a0" (base64-decode-string "YTA"))
(test-equal "decode" "a0\n" (base64-decode-string "YTAK"))
(test-equal "decode" "y49a" (base64-decode-string "eTQ5YQ=="))
(test-equal "decode" "Egjai" (base64-decode-string "RWdqYWk="))
(test-equal "decode" "93bjae" (base64-decode-string "OTNiamFl"))
(test-equal "decode" "BAR0er9" (base64-decode-string "QkFSMGVyOQ=="))
(test-equal "decode" "BAR0er9" (base64-decode-string "QkFS\r\nMGVyOQ\r\n=="))

(test-end)