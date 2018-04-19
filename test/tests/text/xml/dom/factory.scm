(import (rnrs)
	(text xml dom factory)
	(text xml dom nodes)
	(srfi :64))

(define xml-file (string-append (current-directory) "/test/data/test-xml.xml"))

(test-begin "DOM factory")

(test-assert (document? (xml-file->dom-tree xml-file)))
(test-assert (document? (call-with-input-file xml-file input-port->dom-tree)))

(test-end)
