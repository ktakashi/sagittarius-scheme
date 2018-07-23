(import (rnrs)
	(text xml dom writer)
	(text xml dom nodes)
	(srfi :1)
	(srfi :64))

(test-begin "DOM writer")

(define (writer-test expected proc)
  (let-values (((out extract) (open-string-output-port)))
    ;;(write (begin (proc out) (extract))) (newline)
    (test-equal expected (begin (proc out) (extract)))))

(writer-test
 "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\
  <foo:foo xmlns:foo=\"urn:foo\" foo:bla=\"blabla\" foo:buz=\"buzzz\">\
    <foo:bar/>\
  </foo:foo>"
 (lambda (out)
   (let* ((document (make-xml-document))
	  (e (document:create-element-ns document "urn:foo" "foo:foo")))
     (node:append-child! document e)
     (element:set-attribute-ns! e "urn:foo" "foo:bla" "blabla")
     (element:set-attribute! e "foo:buz" "buzzz")
     (node:append-child! e (document:create-element-ns document
						       "urn:foo" "foo:bar"))
     ((make-dom-writer) document out))))

;; it's rather weird to put here to test insert/replace node but
;; it's easier to do it here.
(let* ((document (make-xml-document))
       (e (document:create-element document "foo")))
  (node:append-child! document e)
  (node:insert-before! document e (document:create-comment document "boo"))
  (let ((e2 (document:create-element document "foo2"))
	(e3 (document:create-element document "foo3"))
	(e4 (document:create-element document "foo4"))
	(e5 (document:create-element document "foo5")))
    (node:append-child! e e2)
    (node:insert-before! e e2 e3)
    (node:insert-before! e e2 e4)
    (node:insert-before! e #f e5)
    (writer-test
     "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\
      <!--boo-->\
      <foo><foo3/><foo4/><foo2/><foo5/></foo>"
     (lambda (out)((make-dom-writer) document out)))
    (node:replace-child! e e5 (document:create-element document "foo6"))
    (writer-test
     "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\
      <!--boo-->\
      <foo><foo3/><foo4/><foo2/><foo6/></foo>"
     (lambda (out)((make-dom-writer) document out)))
    (node:remove-child! e e2)
    (writer-test
     "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\
      <!--boo-->\
      <foo><foo3/><foo4/><foo6/></foo>"
     (lambda (out)((make-dom-writer) document out)))
    ))

(test-end)

