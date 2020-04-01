(import (rnrs)
	(text xml dom)
	(text xml xpath compiler)
	(srfi :127)
	(sagittarius generators)
	(srfi :64))

(test-begin "XPath - compiler")

(define (execute-xpath xpath xml . options)
  (define dom (input-port->dom-tree (open-string-input-port xml)))
  ((apply xpath:compile-string xpath options) dom))

(define (node-list->node-name nl) (map node-node-name (node-list->list nl)))

(let ((xml "<ns:foo xmlns:ns=\"ns-foo\"><foo a='12'><foo></foo></foo></ns:foo>"))
  (define (test-xpath xpath . options)
    (node-list->node-name (apply execute-xpath xpath xml options)))
  (test-equal '("foo" "foo") (test-xpath "//foo"))
  (test-equal '("foo") (test-xpath "//foo/foo"))
  (test-equal '("ns:foo") (test-xpath "//ns:foo" '(("ns" . "ns-foo"))))

  (test-equal '("foo") (test-xpath "//ns:foo/foo" '(("ns" . "ns-foo"))))
  )

(test-end)
