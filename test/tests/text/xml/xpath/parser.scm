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
    (test-assert text (parse-success? s))
    (test-assert text (null? nl))
    (unless (equal? expected v) (write v) (newline))
    (test-equal text expected v)))

(success-test $xpath:expr-single "for $x in X return $x"
	      '(for (x "X") (ref x)))
(success-test $xpath:expr-single "for $x in \"X\", $y in 'Y' return $x + $y"
	      '(for (x (str "X")) (for (y (str "Y"))
		 (+ (ref x) (ref y)))))
(success-test $xpath:expr-single "let $x := X return $x"
	      '(let (x "X") (ref x)))
(success-test $xpath:expr-single "let $x := \"X\", $y:='Y' return $x + $y"
	      '(let (x (str "X")) (let (y (str "Y"))
		 (+ (ref x) (ref y)))))
(success-test $xpath:expr-single
	      "some $x in X, $y in Y satisfies $x + $y = 4"
	      '(some ((x "X") (y "Y")) (= (+ (ref x) (ref y)) 4)))
(success-test $xpath:expr-single
	      "every $x in X, $y in Y satisfies $x + $y = 4"
	      '(every ((x "X") (y "Y")) (= (+ (ref x) (ref y)) 4)))

(success-test $xpath:expr-single
	      "if (Y) then 1 + 2 else 3 + 4"
	      '(if ("Y") (+ 1 2) (+ 3 4)))

(success-test $xpath:item-type "item()" '(item))
(success-test $xpath:item-type "element(*)" '(element *))
(success-test $xpath:item-type "function(*)" '(function * (*)))
(success-test $xpath:item-type "function(item()) as item()"
	      '(function (item) ((item))))
(success-test $xpath:item-type "function((item())) as item()"
	      '(function (item) ((item))))
(success-test $xpath:item-type "map(*)" '(map? *))
(success-test $xpath:item-type "map(bla, empty-sequence())"
	      '(map? bla (sequence)))
(success-test $xpath:item-type "array(*)" '(array? *))
(success-test $xpath:item-type "array(empty-sequence())"
	      '(array? (sequence)))
(success-test $xpath:item-type "bla" 'bla)

(success-test $xpath:expr-single "\"\"\"\"" '(str "\""))
(success-test $xpath:expr-single "''''" '(str "'"))
(success-test $xpath:expr-single "1" '1)
(success-test $xpath:expr-single "1234" '1234)
(success-test $xpath:expr-single ".0" '0.0)
(success-test $xpath:expr-single "1.12" '1.12)
(success-test $xpath:expr-single "1.0e1" '10.0)
(success-test $xpath:expr-single "1.0e+1" '10.0)
(success-test $xpath:expr-single "1.0e-1" '0.1)

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

(success-test $xpath:expr-single "/child::foo" '((/ (child:: "foo"))))
(success-test $xpath:expr-single "/descendant::foo" '((/ (descendant:: "foo"))))
(success-test $xpath:expr-single "/attribute::foo" '((/ (attribute:: "foo"))))
(success-test $xpath:expr-single "/self::foo" '((/ (self:: "foo"))))
(success-test $xpath:expr-single "/descendant-or-self::foo"
	      '((/ (descendant-or-self:: "foo"))))
(success-test $xpath:expr-single "/following-sibling::foo"
	      '((/ (following-sibling:: "foo"))))
(success-test $xpath:expr-single "/following::foo" '((/ (following:: "foo"))))
(success-test $xpath:expr-single "/namespace::foo" '((/ (namespace:: "foo"))))

(success-test $xpath:expr-single "/foo[text()]" '((/ ("foo" (?? (text))))))
(success-test $xpath:expr-single "/foo[text()][b]"
	      '((/ ("foo" (?? (text)) (?? "b")))))


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

(success-test $xpath:expr-single "/element(*)" '((/ (element *))))
(success-test $xpath:expr-single "/element(*, type)"
	      '((/ (element (* of "type")))))
(success-test $xpath:expr-single "/element(*, type?)"
	      '((/ (element (* of (? "type"))))))
(success-test $xpath:expr-single "/element(e)" '((/ (element "e"))))
(success-test $xpath:expr-single "/element(e, type)"
	      '((/ (element ("e" of "type")))))
(success-test $xpath:expr-single "/element(e, type?)"
	      '((/ (element ("e" of (? "type"))))))

(success-test $xpath:expr-single "/attribute(*)" '((/ (attribute *))))
(success-test $xpath:expr-single "/attribute(*, type)"
	      '((/ (attribute (* of "type")))))
(success-test $xpath:expr-single "/attribute(*, type?)"
	      '((/ (attribute (* of (? "type"))))))
(success-test $xpath:expr-single "/attribute(e)" '((/ (attribute "e"))))
(success-test $xpath:expr-single "/attribute(e, type)"
	      '((/ (attribute ("e" of "type")))))
(success-test $xpath:expr-single "/attribute(e, type?)"
	      '((/ (attribute ("e" of (? "type"))))))

(success-test $xpath:expr-single "/schema-element(e)"
	      '((/ (schema-element "e"))))

(success-test $xpath:expr-single "/document-node()" '((/ (document-node))))
(success-test $xpath:expr-single "/document-node(element(*))"
	      '((/ (document-node (element *)))))
(success-test $xpath:expr-single "/document-node(schema-element(e))"
	      '((/ (document-node (schema-element "e")))))

(success-test $xpath:expr-single "/schema-attribute(a)"
	      '((/ (schema-attribute "a"))))

(success-test $xpath:expr-single "a or b" '(or "a" "b"))
(success-test $xpath:expr-single "a or b or c" '(or "a" "b" "c"))
(success-test $xpath:expr-single "a or b and c" '(or "a" (and "b" "c")))
(success-test $xpath:expr-single "a or b and c or d"
	      '(or "a" (and "b" "c") "d"))
;; "postfix-expr with predicate"
(success-test $xpath:expr-single "/$a[b]" '((/ ((ref a) (?? "b")))))
;; "postfix-expr with argument list"
(success-test $xpath:expr-single "/$a()" '((/ ((ref a) ()))))
(success-test $xpath:expr-single "/$a?a" '((/ ((ref a) (lookup "a")))))
(success-test $xpath:expr-single "/$a?1" '((/ ((ref a) (lookup 1)))))
(success-test $xpath:expr-single "/$a?(a)"
	      '((/ ((ref a) (lookup (group "a"))))))
(success-test $xpath:expr-single "/$a?*" '((/ ((ref a) (lookup *)))))

(success-test $xpath:expr-single "a eq b" '(eq "a" "b"))
(success-test $xpath:expr-single "a ne b" '(ne "a" "b"))
(success-test $xpath:expr-single "a lt b" '(lt "a" "b"))
(success-test $xpath:expr-single "a le b" '(le "a" "b"))
(success-test $xpath:expr-single "a gt b" '(gt "a" "b"))
(success-test $xpath:expr-single "a ge b" '(ge "a" "b"))

(success-test $xpath:expr-single "a = b"  '(= "a" "b"))
(success-test $xpath:expr-single "a != b" '(!= "a" "b"))
(success-test $xpath:expr-single "a < b"  '(< "a" "b"))
(success-test $xpath:expr-single "a <= b" '(<= "a" "b"))
(success-test $xpath:expr-single "a > b"  '(> "a" "b"))
(success-test $xpath:expr-single "a >= b" '(>= "a" "b"))

(success-test $xpath:expr-single "a is b" '(is "a" "b"))
(success-test $xpath:expr-single "a << b" '(<< "a" "b"))
(success-test $xpath:expr-single "a >> b" '(>> "a" "b"))

(success-test $xpath:expr-single "." '~)
(success-test $xpath:expr-single "foo('a', 'b')"
	      '(apply "foo" (str "a") (str "b")))
(success-test $xpath:expr-single "foo#1" '(fref "foo" 1))
(success-test $xpath:expr-single "function ($x, $y) as int { $x + $y }"
	      '(function (x y) int ((+ (ref x) (ref y)))))

(success-test $xpath:expr-single "map {}" '(map))
(success-test $xpath:expr-single "map {\"key\": 'value'}"
	      '(map ((str "key") (str "value"))))
(success-test $xpath:expr-single "map {\"key\": 'value', key2: 2}"
	      '(map ((str "key") (str "value")) ("key2" 2)))

(success-test $xpath:expr-single "[]" '(array))
(success-test $xpath:expr-single "[1, 2, 3]" '(array 1 2 3))
(success-test $xpath:expr-single "array {}" '(array))
(success-test $xpath:expr-single "array {/a, /b}" '(array ((/ "a")) ((/ "b"))))
(success-test $xpath:expr-single "? key" '(lookup "key"))
(success-test $xpath:expr-single "? 1" '(lookup 1))
(success-test $xpath:expr-single "? *" '(lookup *))
(success-test $xpath:expr-single "? (1)" '(lookup (group 1)))

;; namespace
(success-test $xpath:expr-single "/ns:foo" '((/ (qname #f "ns" "foo"))))
(success-test $xpath:expr-single "/Q{fqn}foo" '((/ (eqname "fqn" "foo"))))


(test-equal '(((/ "foo") (// "baz")))
	    (xpath:parse (generator->lseq (string->generator "/foo//baz"))))
(test-error "parse error test" xpath-parse-error?
	    (xpath:parse (generator->lseq (string->generator "???"))))

(test-end)
