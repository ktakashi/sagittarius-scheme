(import (except (rnrs) string-titlecase)
	(srfi :129)
	(srfi :64))

(test-begin "SRFI 129 titlecase")

;; values are from SRFI-129 sample implementation tests
(test-assert (char-title-case? #\x01C5))
(test-assert (char-title-case? #\x1FFC))
(test-assert (not (char-title-case? #\Z)))
(test-assert (not (char-title-case? #\z)))

(test-equal #\x01C5 (char-titlecase #\x01C4))
(test-equal #\x01C5 (char-titlecase #\x01C6))
(test-equal #\Z (char-titlecase #\Z))
(test-equal #\Z (char-titlecase #\z))

(let ()
  (define Floo "\xFB02;oo")
  (define Floo-bar "\xFB02;oo bar")
  (define Baffle "Ba\xFB04;e")
  (define LJUBLJANA "\x01C7;ub\x01C7;ana")
  (define Ljubljana "\x01C8;ub\x01C9;ana")
  (define ljubljana "\x01C9;ub\x01C9;ana")

  (define-syntax test
    (syntax-rules ()
      ((_ expect expr)
       (test-equal expect expect expr))))

  (test "\x01C5;" (string-titlecase "\x01C5;"))
  (test "\x01C5;" (string-titlecase "\x01C4;"))
  (test "Ss" (string-titlecase "\x00DF;"))
  (test "Xi\x0307;" (string-titlecase "x\x0130;"))
  (test "\x1F88;" (string-titlecase "\x1F80;"))
  (test "Bar Baz" (string-titlecase "bAr baZ"))
  (test "Floo" (string-titlecase "floo"))
  (test "Floo" (string-titlecase "FLOO"))
  (test "Floo" (string-titlecase Floo))
  (test "Floo Bar" (string-titlecase"floo bar"))
  (test "Floo Bar" (string-titlecase "FLOO BAR"))
  (test "Floo Bar" (string-titlecase Floo-bar))
  (test Baffle (string-titlecase Baffle))
  (test Ljubljana (string-titlecase LJUBLJANA))
  (test Ljubljana (string-titlecase Ljubljana))
  (test Ljubljana (string-titlecase ljubljana)))

(test-end)
