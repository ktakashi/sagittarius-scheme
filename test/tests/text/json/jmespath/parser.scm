(import (rnrs)
	(text json jmespath parser)
	(sagittarius generators)
	(peg)
	(srfi :127)
	(srfi :64))

(test-begin "JMESPath parser")

(define (test-parser parser expected string)
  (let ((lseq (generator->lseq (string->generator string))))
    (let-values (((s v nl) (parser lseq)))
      (test-assert (parse-success? s))
      (test-equal expected v))))

(test-parser jmespath:identifier "foo" "foo")
(test-parser jmespath:identifier "fo\"o" "\"fo\\\"o\"")

(test-end)
