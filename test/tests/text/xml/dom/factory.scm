(import (rnrs)
	(util file)
	(text xml dom factory)
	(text xml dom nodes)
	(srfi :64))

(define xml-file (string-append (current-directory) "/test/data/test-xml.xml"))

(test-begin "DOM factory")

(let ((doc (xml-file->dom-tree xml-file)))
  (test-assert (document? doc))
  (test-equal (absolute-path xml-file) (document-document-uri doc)))
  
(test-assert (document? (call-with-input-file xml-file input-port->dom-tree)))

(test-end)
