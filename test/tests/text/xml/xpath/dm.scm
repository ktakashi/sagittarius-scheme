(import (rnrs)
	(text xml xpath dm)
	(text xml dom)
	(srfi :64))

(define (string->dom xml)
  (input-port->dom-tree (open-string-input-port xml)))

(test-begin "XPath Data Model")

(let* ((xml "<foo xmlns:ns=\"ns-foo\">foo<bar><baz id='child'>baz</baz>abc</bar></foo>")
       (dom (string->dom xml))
       (e (document:get-element-by-id dom "child")))
  (test-equal "xpath-dm:string-value" "foobazabc" (xpath-dm:string-value dom))
  (test-equal "xpath-dm:string-value" "baz" (xpath-dm:string-value e))
  (test-equal "xpath-dm:string-value" "child"
	      (xpath-dm:string-value (element:get-attribute-node e "id")))
  (test-equal "xpath-dm:string-value" "ns-foo"
	      (xpath-dm:string-value
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))
  (test-error "xpath-dm:string-value"
	      (xpath-dm:string-value
	       (document:create-document-type dom "public-id" "system-id"))))

(let* ((xml "<foo><!-- comment --></foo>")
       (dom (string->dom xml))
       (c (node-first-child (document-document-element dom))))
  (test-equal "xpath-dm:string-value" " comment " (xpath-dm:string-value c)))

(let* ((xml "<foo>text</foo>")
       (dom (string->dom xml))
       (c (node-first-child (document-document-element dom))))
  (test-equal "xpath-dm:string-value" "text" (xpath-dm:string-value c)))

(let* ((xml "<foo><?sample-pi content?></foo>")
       (dom (string->dom xml))
       (c (node-first-child (document-document-element dom))))
  (test-equal "xpath-dm:string-value" "content" (xpath-dm:string-value c)))

(test-end)
