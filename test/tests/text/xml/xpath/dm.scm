(import (rnrs)
	(text xml xpath dm)
	(text xml dom)
	(srfi :64))

(define (string->dom xml)
  (input-port->dom-tree (open-string-input-port xml)))

(test-begin "XPath Data Model")

(let* ((xml "<foo>foo<bar><baz id='child'>baz</baz>abc</bar></foo>")
       (dom (string->dom xml))
       (e (document:get-element-by-id dom "child")))
  (test-equal "foobazabc" (xpath-dm:string-value dom))
  (test-equal "baz" (xpath-dm:string-value e)))

(test-end)
