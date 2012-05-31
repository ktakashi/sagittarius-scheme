;; -*- scheme -*-

(import (rnrs)
	(rfc quoted-printable)
	(srfi :64 testing))

(define *utf-8-string* "日本語のquoted-printableテスト用文字列")
(define *encoded-string* "=E6=97=A5=E6=9C=AC=E8=AA=9E=E3=81=AEquoted-printable=E3=83=86=E3=82=B9=E3=\r\n=83=88=E7=94=A8=E6=96=87=E5=AD=97=E5=88=97")


(test-begin "RFC quoted printable tests")
;; I should add the cases which specifies transcoder.
(test-equal "quoted-printable encode" *encoded-string*
	    (quoted-printable-encode-string *utf-8-string*))

(test-equal "quoted-printable decode" *utf-8-string*
	    (quoted-printable-decode-string *encoded-string*))

;; from Gauche
(test-equal "encode" "abcd=0Cefg"
	    (quoted-printable-encode-string "abcd\x0c;efg"))
(test-equal "encode"
	    "abcd\r\nefg"
	    (quoted-printable-encode-string "abcd\r\nefg"))
(test-equal "encode (tab/space at eol)"
	    "abcd=09\r\nefg=20\r\n"
	    (quoted-printable-encode-string "abcd\t\r\nefg \r\n"))
(test-equal "encode (soft line break)"
	    "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abc=\r\ndefghij0123456789abcdefghij"
	    (quoted-printable-encode-string "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"))
(test-equal "encode (soft line break w/line-width)"
	    "0123456789abcdefg=\r\nhij0123456789abcd=\r\nefghij"
	    (quoted-printable-encode-string
	     "0123456789abcdefghij0123456789abcdefghij"
	     :line-width 20))
(test-equal "encode (soft line break w/line-width)"
	    "0123456789abcdef=3D=\r\nghij0123456789a=3D=\r\n=3Dbcdefghij"
	    (quoted-printable-encode-string
	     "0123456789abcdef=ghij0123456789a==bcdefghij"
	     :line-width 20))
(test-equal "encode (soft line break w/line-width lower bound)"
	    "a=\r\n=3F=\r\nb"
	    (quoted-printable-encode-string "a?b" :line-width 4))
(test-equal "encode (no line break)"
	    "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"
	    (quoted-printable-encode-string "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"
					    :line-width #f))
(test-equal "encode (hard line break)"
	    "a\r\nb\r\nc\r\n"
	    (quoted-printable-encode-string "a\rb\nc\r\n"))
(test-equal "encode (binary)"
	    "a=0Db=0Ac=0D=0A"
	    (quoted-printable-encode-string "a\rb\nc\r\n" :binary? #t))

(test-equal "decode" "\x01;\x08;abcde=\r\n"
	    (quoted-printable-decode-string "=01=08abc=64=65=3D\r\n"))
(test-equal "decode (soft line break)"
	    "Now's the time for all folk to come to the aid of their country."
	    (quoted-printable-decode-string "Now's the time =\r\nfor all folk to come=   \r\n to the aid of their country."))
(test-equal "decode (robustness)"
	    "foo=1qr =  j\r\n"
	    (quoted-printable-decode-string "foo=1qr =  j\r\n="))

(test-end)