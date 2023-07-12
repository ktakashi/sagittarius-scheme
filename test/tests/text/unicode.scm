(import (rnrs)
	(text unicode)
	(sagittarius generators) ;; for generator->list
	(srfi :64))

(test-begin "Unicode")

(define (test-break i strategy expected in)
  (define (parse-it s)
    (generator->list (string->unicode-break-generator s strategy)))
  (define (->text out expected)
    (define (->hex s)
      (map (lambda (c) (format "~4,'0X" (char->integer c))) (string->list s)))
    (put-string out "÷ ")
    (put-string out (string-join
		     (map (lambda (s) (string-join (->hex s) " × ")) expected)
		     " ÷ "))
    (put-string out " ÷"))
  (let-values (((out e) (open-string-output-port)))
    (put-datum out in)
    (put-string out " -> ")
    (->text out expected)
    (test-equal (e) expected (parse-it in))))

(let ()
  (define zwj-emoji "abc👨‍👨‍👧‍👧d") ;; -> "a" "b" "c" "👨‍👨‍👧‍👧" "d"
  (define gb3 "a\r\nb")		    ;; -> "a" "\r\n" "b"
  (define gb4&gb5 "a\rb")	    ;; -> "a" "\r" "b"

  (test-break 0 grapheme-strategy '("a" "b" "c" "👨‍👨‍👧‍👧" "d") zwj-emoji)
  (test-break 1 grapheme-strategy '("a" "\r\n" "b") gb3)
  (test-break 2 grapheme-strategy '("a" "\r" "b") gb4&gb5))

(define grapheme-data (include "./unicode/grapheme-data.scm"))
(do ((i 0 (+ i 1)) (len (vector-length grapheme-data)))
    ((= i len))
  (let ((v (vector-ref grapheme-data i)))
    (test-break i grapheme-strategy (cadr v) (car v))))

(test-end)
