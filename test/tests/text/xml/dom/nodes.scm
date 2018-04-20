(import (rnrs)
	(text xml dom factory)
	(text xml dom nodes)
	(srfi :1) ;; for unfold
	(srfi :64))

(define xml-file (string-append (current-directory) "/test/data/test-xml.xml"))

(define (tree-walker-unfold tw gen)
  (unfold (lambda (node) (not node))
	  node-node-name
	  (lambda (seed) (gen tw)) (gen tw)))

(define (node-list->list nl)
  (do ((len (node-list-length nl))
       (i 0 (+ i 1))
       (r '() (cons (node-list:item nl i) r)))
      ((= i len) (reverse r))))

(test-begin "DOM nodes")

(let ((document (xml-file->dom-tree xml-file)))
  (test-assert (document? document))
  (test-assert (comment? (node-first-child document)))
  (test-assert (processing-instruction?
		(node-next-sibling (node-first-child document))))
  (test-equal "xml-stylesheet"
	      (processing-instruction-target
	       (node-next-sibling (node-first-child document))))
  (test-assert (comment? (node-previous-sibling
			  (node-next-sibling (node-first-child document)))))
  (test-assert (document-type? (document-doctype document)))
  (test-equal "root" (document-type-name (document-doctype document)))
  ;; empty document type
  (test-equal "" (document-type-public-id (document-doctype document)))
  (test-equal "" (document-type-system-id (document-doctype document)))

  (test-assert (element? (document-document-element document)))
  (let ((e (document-document-element document)))
    (test-equal "root" (node-node-name e))
    (test-equal "urn:boo" (element-namespace-uri e))

    (test-equal '("foo:bar")
		(map node-node-name
		     (node-list->list
		      (element:get-elements-by-tag-name e "foo:bar"))))
    (test-equal '("foo:foo")
		(map node-node-name
		     (node-list->list
		      (element:get-elements-by-tag-name-ns e "urn:foo" "foo")))))

  (test-equal '("foo:bar")
		(map node-node-name
		     (node-list->list
		      (document:get-elements-by-tag-name document "foo:bar"))))
  (test-equal '("foo:foo")
		(map node-node-name
		     (node-list->list
		      (document:get-elements-by-tag-name-ns document
							    "urn:foo" "foo"))))
  (test-assert (element? (document:get-element-by-id document "id-of-foo")))
  (test-assert (not (document:get-element-by-id document "no-such-id")))
  
  (let ((tw (document:create-tree-walker document document
					 +node-filter-show-element+)))
    (test-equal '("root" "foo:foo" "foo:bar" "boo")
		(tree-walker-unfold tw tree-walker:next-node))
    ;; boo is the current node
    (test-equal "boo" (node-node-name (tree-walker-current-node tw)))
    (test-equal '("foo:bar" "foo:foo" "root")
		(tree-walker-unfold tw tree-walker:previous-node)))
)

(test-end)
