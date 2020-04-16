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

  (test-equal "xpath-dm:base-uri (1)" '() (xpath-dm:base-uri dom))
  (test-equal "xpath-dm:base-uri (2)" '() (xpath-dm:base-uri e))
  (test-equal "xpath-dm:base-uri (3)" '()
	      (xpath-dm:base-uri (document-document-element dom)))
  (test-equal "xpath-dm:base-uri (4)" '() (xpath-dm:base-uri attr))
  (test-equal "xpath-dm:base-uri (5)" '()
	      (xpath-dm:base-uri
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))

  (test-equal "xpath-dm:children (1)" '("ns:foo")
	      (map node-node-name (xpath-dm:children dom)))
  (test-equal "xpath-dm:children (2)" '("#text")
	      (map node-node-name (xpath-dm:children e)))
  (test-equal "xpath-dm:children (3)" '("#text" "bar")
	      (map node-node-name (xpath-dm:children (document-document-element dom))))
  (test-equal "xpath-dm:children (4)" '() (xpath-dm:children attr))
  (test-equal "xpath-dm:children (5)" '()
	      (xpath-dm:children
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))

  (test-equal "xpath-dm:document-uri (1)" '() (xpath-dm:document-uri dom))
  (test-equal "xpath-dm:document-uri (2)" '() (xpath-dm:document-uri e))
  (test-equal "xpath-dm:document-uri (3)" '()
	      (xpath-dm:document-uri (document-document-element dom)))
  (test-equal "xpath-dm:document-uri (4)" '() (xpath-dm:document-uri attr))
  (test-equal "xpath-dm:document-uri (5)" '()
	      (xpath-dm:document-uri
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))

  (test-equal "xpath-dm:is-id (1)" '() (xpath-dm:is-id dom))
  (test-equal "xpath-dm:is-id (2)" #f (xpath-dm:is-id e))
  (test-equal "xpath-dm:is-id (3)" #f (xpath-dm:is-id attr))
  (test-equal "xpath-dm:is-id (4)" '()
	      (xpath-dm:is-id
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))

  (test-equal "xpath-dm:is-idrefs (1)" '() (xpath-dm:is-idrefs dom))
  (test-equal "xpath-dm:is-idrefs (2)" #f (xpath-dm:is-idrefs e))
  (test-equal "xpath-dm:is-idrefs (3)" #f (xpath-dm:is-idrefs attr))
  (test-equal "xpath-dm:is-idrefs (4)" '()
	      (xpath-dm:is-idrefs
	       (node-list:item
		(element:namespace-nodes (document-document-element dom)) 0)))

  (test-equal "xpath-dm:namespace-nodes (1)" '() (xpath-dm:namespace-nodes dom))
  (test-equal "xpath-dm:namespace-nodes (2)" '() (xpath-dm:namespace-nodes e))
  (test-equal "xpath-dm:namespace-nodes (3)" '("ns")
	      (map xpath-dm:node-name (xpath-dm:namespace-nodes (document-document-element dom))))
  (test-equal "xpath-dm:namespace-nodes (4)" '() (xpath-dm:namespace-nodes attr))
  (test-equal "xpath-dm:namespace-nodes (5)" '()
	      (xpath-dm:namespace-nodes
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
  (test-equal "xpath-dm:base-uri" '() (xpath-dm:base-uri c))
  (test-equal "xpath-dm:children" '() (xpath-dm:children c))
  (test-equal "xpath-dm:document-uri" '() (xpath-dm:document-uri c))
  (test-equal "xpath-dm:is-id" '() (xpath-dm:is-id c))
  (test-equal "xpath-dm:is-idrefs" '() (xpath-dm:is-idrefs c))
  (test-equal "xpath-dm:string-value" " comment " (xpath-dm:string-value c))
  (test-equal "xpath-dm:typed-value" " comment " (xpath-dm:typed-value c)))

(let* ((xml "<foo>text</foo>")
       (dom (string->dom xml))
       (c (node-first-child (document-document-element dom))))
  (test-equal "xpath-dm:attributes" '() (xpath-dm:attributes c))
  (test-equal "xpath-dm:node-name" '() (xpath-dm:node-name c))
  (test-equal "xpath-dm:base-uri" '() (xpath-dm:base-uri c))
  (test-equal "xpath-dm:children" '() (xpath-dm:children c))
  (test-equal "xpath-dm:document-uri" '() (xpath-dm:document-uri c))
  (test-equal "xpath-dm:is-id" '() (xpath-dm:is-id c))
  (test-equal "xpath-dm:is-idrefs" '() (xpath-dm:is-idrefs c))
  (test-equal "xpath-dm:string-value" "text" (xpath-dm:string-value c))
  (test-equal "xpath-dm:typed-value" "text" (xpath-dm:typed-value c)))

(let* ((xml "<foo><?sample-pi content?></foo>")
       (dom (string->dom xml))
       (c (node-first-child (document-document-element dom))))
  (test-equal "xpath-dm:attributes" '() (xpath-dm:attributes c))
  (test-equal "xpath-dm:node-name" "sample-pi" (xpath-dm:node-name c))
  (test-equal "xpath-dm:base-uri" '() (xpath-dm:base-uri c))
  (test-equal "xpath-dm:children" '() (xpath-dm:children c))
  (test-equal "xpath-dm:document-uri" '() (xpath-dm:document-uri c))
  (test-equal "xpath-dm:is-id" '() (xpath-dm:is-id c))
  (test-equal "xpath-dm:is-idrefs" '() (xpath-dm:is-idrefs c))
  (test-equal "xpath-dm:string-value" "content" (xpath-dm:string-value c))
  (test-equal "xpath-dm:typed-value" "content" (xpath-dm:typed-value c)))

(let* ((xml-file (string-append (current-directory) "/test/data/test-xml.xml"))
       (doc (xml-file->dom-tree xml-file)))
  (test-equal (absolute-path xml-file) (xpath-dm:base-uri doc))
  (test-equal (absolute-path xml-file) (xpath-dm:document-uri doc)))

(test-end)
