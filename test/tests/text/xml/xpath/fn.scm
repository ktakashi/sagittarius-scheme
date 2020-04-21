(import (rnrs)
	(text xml dom)
	(text xml errors)
	(text xml xpath fn)
	(srfi :64))

(test-begin "XPath functions and operators")

(define xml-file (string-append (current-directory) "/test/data/test-xml.xml"))
(define dom (xml-file->dom-tree xml-file))
(define root-element (document-document-element dom))

(test-group "Accessors"
  (test-group "xpath-fn:node-name"
    (test-equal '() (xpath-fn:node-name '()))
    (test-error xqt-error? (xpath-fn:node-name "not a node"))
    (test-equal "root" (xpath-fn:node-name root-element)))

  (test-group "xpath-fn:nilled"
    (test-equal '() (xpath-fn:nilled '()))
    (test-error xqt-error? (xpath-fn:nilled "not a node"))
    (test-equal #f (xpath-fn:nilled root-element))
    (test-equal '() (xpath-fn:nilled dom)))

  (test-group "xpath-fn:string"
    (test-equal "" (xpath-fn:string '()))
    (test-equal "foo" (xpath-fn:string "foo"))
    (test-equal "1" (xpath-fn:string 1))
    (test-equal "true" (xpath-fn:string #t))
    (test-equal "false" (xpath-fn:string #f))
    (test-equal " cdata section "
		(xpath-fn:string
		 ;; get cdata section
		 (node-previous-sibling (node-last-child root-element))))
    (test-error xqt-error? (xpath-fn:string 'symbol))
    ;; TODO add error cases, array and map
    )

  (test-group "xpath-fn:data"
    (test-equal 123 (xpath-fn:data 123))
    (test-equal '(123 456) (xpath-fn:data '(123 456)))
    (test-equal " cdata section "
		(xpath-fn:data
		 ;; get cdata section (laziness)
		 (node-previous-sibling (node-last-child root-element))))
    (test-error xqt-error? (xpath-fn:data 'symbol))
    ;; TODO error cases
    )

  (test-group "xpath-fn:base-uri"
    (test-equal '() (xpath-fn:base-uri '()))
    (test-equal (absolute-path xml-file) (xpath-fn:base-uri dom))
    (test-equal '() (xpath-fn:base-uri root-element))
    (test-error xqt-error? (xpath-fn:base-uri 'symbol))
    )

  (test-group "xpath-fn:document-uri"
    (test-equal '() (xpath-fn:document-uri '()))
    (test-equal (absolute-path xml-file) (xpath-fn:document-uri dom))
    (test-equal '() (xpath-fn:document-uri root-element))
    (test-error xqt-error? (xpath-fn:document-uri 'symbol))
    )
  )

(test-end)
