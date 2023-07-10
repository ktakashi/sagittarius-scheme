(import (rnrs)
	(text unicode)
	(sagittarius generators) ;; for generator->list
	(srfi :64))

(test-begin "Unicode")

(let ()
  (define zwj-emoji "abc👨‍👨‍👧‍👧d") ;; -> "a" "b" "c" "👨‍👨‍👧‍👧" "d"
  (define gb3 "a\r\nb")		    ;; -> "a" "\r\n" "b"
  (define gb4&gb5 "a\rb")	    ;; -> "a" "\r" "b"

  (define (parse-it s)
    (generator->list (string->unicode-break-generator s grapheme-strategy)))

  (test-equal '("a" "b" "c" "👨‍👨‍👧‍👧" "d") (parse-it zwj-emoji))
  (test-equal '("a" "\r\n" "b") (parse-it gb3))
  (test-equal '("a" "\r" "b") (parse-it gb4&gb5)))

(test-end)
