(import (rnrs)
	(text xml dom writer)
	(text xml dom nodes)
	(srfi :1)
	(srfi :64))

(test-begin "DOM writer")

(define (writer-test expected proc)
  (let-values (((out extract) (open-string-output-port)))
    (test-equal expected (begin (proc out) (extract)))))

(writer-test
 "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><foo:foo xmlns:foo=\"urn:foo\" foo:bla=\"blabla\" foo:buz=\"buzzz\"><foo:bar/></foo:foo>"
 (lambda (out)
   (let* ((document (make-document))
	  (e (document:create-element-ns document "urn:foo" "foo:foo")))
     (node:append-child! document e)
     (element:set-attribute-ns! e "urn:foo" "foo:bla" "blabla")
     (element:set-attribute! e "foo:buz" "buzzz")
     (node:append-child! e (document:create-element-ns document
						       "urn:foo" "foo:bar"))
     ((make-dom-writer) document out))))

(test-end)

