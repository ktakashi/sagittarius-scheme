;; -*- scheme -*-
(library (tests rfc base64)
    (export run-rfc-base64-tests)
    (import (rnrs)
	    (rfc base64)
	    (srfi :64 testing))

  (define *long-string* 
    ;; from Wikipedia Base64
    "Base64 is a group of similar encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation. The Base64 term originates from a specific MIME content transfer encoding.\n\nBase64 encoding schemes are commonly used when there is a need to encode binary data that needs be stored and transferred over media that are designed to deal with textual data. This is to ensure that the data remains intact without modification during transport. Base64 is used commonly in a number of applications including email via MIME, and storing complex data in XML.")

  (define *encoded-long-string*
    "QmFzZTY0IGlzIGEgZ3JvdXAgb2Ygc2ltaWxhciBlbmNvZGluZyBzY2hlbWVzIHRoYXQgcmVwcmVz\nZW50IGJpbmFyeSBkYXRhIGluIGFuIEFTQ0lJIHN0cmluZyBmb3JtYXQgYnkgdHJhbnNsYXRpbmcg\naXQgaW50byBhIHJhZGl4LTY0IHJlcHJlc2VudGF0aW9uLiBUaGUgQmFzZTY0IHRlcm0gb3JpZ2lu\nYXRlcyBmcm9tIGEgc3BlY2lmaWMgTUlNRSBjb250ZW50IHRyYW5zZmVyIGVuY29kaW5nLgoKQmFz\nZTY0IGVuY29kaW5nIHNjaGVtZXMgYXJlIGNvbW1vbmx5IHVzZWQgd2hlbiB0aGVyZSBpcyBhIG5l\nZWQgdG8gZW5jb2RlIGJpbmFyeSBkYXRhIHRoYXQgbmVlZHMgYmUgc3RvcmVkIGFuZCB0cmFuc2Zl\ncnJlZCBvdmVyIG1lZGlhIHRoYXQgYXJlIGRlc2lnbmVkIHRvIGRlYWwgd2l0aCB0ZXh0dWFsIGRh\ndGEuIFRoaXMgaXMgdG8gZW5zdXJlIHRoYXQgdGhlIGRhdGEgcmVtYWlucyBpbnRhY3Qgd2l0aG91\ndCBtb2RpZmljYXRpb24gZHVyaW5nIHRyYW5zcG9ydC4gQmFzZTY0IGlzIHVzZWQgY29tbW9ubHkg\naW4gYSBudW1iZXIgb2YgYXBwbGljYXRpb25zIGluY2x1ZGluZyBlbWFpbCB2aWEgTUlNRSwgYW5k\nIHN0b3JpbmcgY29tcGxleCBkYXRhIGluIFhNTC4=")

  (define (run-rfc-base64-tests)
    ;; I should add the cases which specifies transcoder.
    (test-equal "base64 encode" "YWJjZGVmZw==" (base64-encode-string "abcdefg"))
    (test-equal "base64 decode" "abcdefg" (base64-decode-string "YWJjZGVmZw=="))

    (test-equal "base64 encode long" *encoded-long-string* (base64-encode-string *long-string*))
    (test-equal "base64 decode long" *long-string* (base64-decode-string *encoded-long-string*))
    )
)