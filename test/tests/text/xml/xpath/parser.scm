(import (rnrs)
	(text xml xpath parser)
	(sagittarius generators)
	(peg)
	(srfi :127)
	(srfi :64))

(test-begin "XPath 3.1 parser")

(define (parse-it parser text)
  (define lseq (generator->lseq (string->generator text)))
  (parser lseq))

(define (success-test parser text expected)
  (let-values (((s v nl) (parse-it parser text)))
    ;; (write v) (newline)
    (test-assert (parse-success? s))
    (test-equal text expected v)))

(success-test $xpath:for-expr "for $x in X return $x"
	      '(for (x ("X")) ((ref x))))
(success-test $xpath:for-expr "for $x in \"X\", $y in 'Y' return $x + $y"
	      '(for (x ((str "X"))) (for (y ((str "Y")))
		 (+ ((ref x)) ((ref y))))))

(success-test $xpath:expr-single "\"\"\"\"" '((str "\"")))
(success-test $xpath:expr-single "''''" '((str "'")))

(success-test $xpath:expr-single "/foo/bar" '((/ "foo") (/ "bar")))
(success-test $xpath:expr-single "/foo/bar/baz"
	      '((/ "foo") (/ "bar") (/ "baz")))

(success-test $xpath:expr-single "/foo//baz" '((/ "foo") (// "baz")))
(success-test $xpath:expr-single "/" '((/)))
(success-test $xpath:expr-single "/foo" '((/ "foo")))
(success-test $xpath:expr-single "//bar" '((// "bar")))
(success-test $xpath:expr-single "//*" '((// *)))
(success-test $xpath:expr-single "//*:foo" '((// (* "foo"))))
(success-test $xpath:expr-single "//foo:*" '((// ("foo" *))))

(success-test $xpath:expr-single "/parent::foo" '((/ (parent:: "foo"))))
(success-test $xpath:expr-single "/ancestor::foo" '((/ (ancestor:: "foo"))))
(success-test $xpath:expr-single "/preceding-sibling::foo"
	      '((/ (preceding-sibling:: "foo"))))
(success-test $xpath:expr-single "/preceding::foo"
	      '((/ (preceding:: "foo"))))
(success-test $xpath:expr-single "/ancestor-or-self::foo"
	      '((/ (ancestor-or-self:: "foo"))))

(success-test $xpath:expr-single "/node()" '((/ (node))))
(success-test $xpath:expr-single "/text()" '((/ (text))))
(success-test $xpath:expr-single "/comment()" '((/ (comment))))
(success-test $xpath:expr-single "/namespace-node()" '((/ (namespace-node))))
(success-test $xpath:expr-single "/processing-instruction()"
	      '((/ (processing-instruction #f))))
(success-test $xpath:expr-single "/processing-instruction('foo')"
	      '((/ (processing-instruction (str "foo")))))
(success-test $xpath:expr-single "/processing-instruction(name)"
	      '((/ (processing-instruction "name"))))

(success-test $xpath:expr-single "a or b" '(or ("a") ("b")))
(success-test $xpath:expr-single "a or b or c" '(or ("a") ("b") ("c")))
(success-test $xpath:expr-single "a or b and c" '(or ("a") (and ("b") ("c"))))
(success-test $xpath:expr-single "a or b and c or d"
	      '(or ("a") (and ("b") ("c")) ("d")))

(test-end)
