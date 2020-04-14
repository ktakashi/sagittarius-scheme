(import (rnrs)
	(text xml xpath dm)
	(text xml dom)
	(srfi :64))

(define (string->dom xml)
  (input-port->dom-tree (open-string-input-port xml)))

(test-begin "XPath Data Model")

(let* ((xml "<ns:foo xmlns:ns=\"ns-foo\">foo<bar><baz id='child'>baz</baz>abc</bar></ns:foo>")
       (dom (string->dom xml))
       (e (document:get-element-by-id dom "child"))
       (attr (element:get-attribute-node e "id")))
  (test-equal "xpath-dm:attributes (1)" '() (xpath-dm:attributes dom))
  (test-equal "xpath-dm:attributes (2)" '("id")
	      (map attr-name (xpath-dm:attributes e)))
  (test-equal "xpath-dm:attributes (3)" '()
	      (xpath-dm:attributes (document-document-element dom)))
  (test-equal "xpath-dm:attributes (4)" '() (xpath-dm:attributes attr))
  (test-equal "xpath-dm:attributes (5)" '()
	      (xpath-dm:attributes
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))
  
  (test-equal "xpath-dm:node-name" () (xpath-dm:node-name dom))
  (test-equal "xpath-dm:node-name" "baz" (xpath-dm:node-name e))
  (test-equal "xpath-dm:node-name" "ns:foo"
	      (xpath-dm:node-name (document-document-element dom)))
  (test-equal "xpath-dm:node-name" "id" (xpath-dm:node-name attr))
  (test-equal "xpath-dm:node-name" "ns"
	      (xpath-dm:node-name
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))
  
  (test-equal "xpath-dm:string-value" "foobazabc" (xpath-dm:string-value dom))
  (test-equal "xpath-dm:string-value" "baz" (xpath-dm:string-value e))
  (test-equal "xpath-dm:string-value" "child" (xpath-dm:string-value attr))
  (test-equal "xpath-dm:string-value" "ns-foo"
	      (xpath-dm:string-value
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))
  (test-error "xpath-dm:string-value"
	      (xpath-dm:string-value
	       (document:create-document-type dom "public-id" "system-id")))


  (test-equal "xpath-dm:typed-value" "foobazabc" (xpath-dm:typed-value dom))
  (test-equal "xpath-dm:typed-value" "baz" (xpath-dm:typed-value e))
  (test-equal "xpath-dm:typed-value" "child" (xpath-dm:typed-value attr))
  (test-equal "xpath-dm:typed-value" "ns-foo"
	      (xpath-dm:typed-value
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))
  (test-error "xpath-dm:typed-value"
	      (xpath-dm:typed-value
	       (document:create-document-type dom "public-id" "system-id")))
  )

(let* ((xml "<foo><!-- comment --></foo>")
       (dom (string->dom xml))
       (c (node-first-child (document-document-element dom))))
  (test-equal "xpath-dm:attributes" '() (xpath-dm:attributes c))
  (test-equal "xpath-dm:node-name" '() (xpath-dm:node-name c))
  (test-equal "xpath-dm:string-value" " comment " (xpath-dm:string-value c))
  (test-equal "xpath-dm:typed-value" " comment " (xpath-dm:typed-value c)))

(let* ((xml "<foo>text</foo>")
       (dom (string->dom xml))
       (c (node-first-child (document-document-element dom))))
  (test-equal "xpath-dm:attributes" '() (xpath-dm:attributes c))
  (test-equal "xpath-dm:node-name" '() (xpath-dm:node-name c))
  (test-equal "xpath-dm:string-value" "text" (xpath-dm:string-value c))
  (test-equal "xpath-dm:typed-value" "text" (xpath-dm:typed-value c)))

(let* ((xml "<foo><?sample-pi content?></foo>")
       (dom (string->dom xml))
       (c (node-first-child (document-document-element dom))))
  (test-equal "xpath-dm:attributes" '() (xpath-dm:attributes c))
  (test-equal "xpath-dm:node-name" "sample-pi" (xpath-dm:node-name c))
  (test-equal "xpath-dm:string-value" "content" (xpath-dm:string-value c))
  (test-equal "xpath-dm:typed-value" "content" (xpath-dm:typed-value c)))

(test-end)
