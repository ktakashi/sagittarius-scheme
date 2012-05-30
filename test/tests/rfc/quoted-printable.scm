;; -*- scheme -*-
;; Local Variables:
;; coding: utf-8-unix
;; End:

(import (rnrs)
	(rfc quoted-printable)
	(srfi :64 testing))

(define *utf-8-string* "日本語のquoted-printableテスト用文字列")
(define *encoded-string* "=E6=97=A5=E6=9C=AC=E8=AA=9E=E3=81=AEquoted-printable=E3=83=86=E3=82=B9=E3=\r\n=83=88=E7=94=A8=E6=96=87=E5=AD=97=E5=88=97")


(test-begin "(run-rfc-quoted-printable-tests)")
;; I should add the cases which specifies transcoder.
(test-equal "quoted-printable encode" *encoded-string*
	    (quoted-printable-encode-string *utf-8-string*))

(test-equal "quoted-printable decode" *utf-8-string*
	    (quoted-printable-decode-string *encoded-string*))
(test-end)