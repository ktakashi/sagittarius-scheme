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
  (test-equal #f (document-type-public-id (document-doctype document)))
  (test-equal #f (document-type-system-id (document-doctype document)))

  (test-assert (element? (document-document-element document)))
  (let ((e (document-document-element document)))
    (test-equal "root" (node-node-name e))
    (test-equal "urn:boo" (element-namespace-uri e))

    (test-equal '("root")
		(map node-node-name
		     (node-list->list
		      (element:get-elements-by-tag-name e "root"))))
    (test-equal '("foo:bar")
		(map node-node-name
		     (node-list->list
		      (element:get-elements-by-tag-name e "foo:bar"))))
    (test-equal '("foo:foo")
		(map node-node-name
		     (node-list->list
		      (element:get-elements-by-tag-name-ns e "urn:foo" "foo"))))
    (test-equal '("root")
		(map node-node-name
		     (node-list->list
		      (element:get-elements-by-class-name e "foo")))))

  (test-equal '("root")
	      (map node-node-name
		   (node-list->list
		    (document:get-elements-by-tag-name document "root"))))
  (test-equal '("foo:bar")
		(map node-node-name
		     (node-list->list
		      (document:get-elements-by-tag-name document "foo:bar"))))
  (test-equal '("foo:foo")
		(map node-node-name
		     (node-list->list
		      (document:get-elements-by-tag-name-ns document
							    "urn:foo" "foo"))))
  (test-equal '("root")
		(map node-node-name
		     (node-list->list
		      (document:get-elements-by-class-name document "foo"))))
  (test-assert (element? (document:get-element-by-id document "id-of-foo")))
  (test-assert (not (document:get-element-by-id document "no-such-id")))

  (let ()
    (define (test-walker tw expected-next expected-previous
			 :optional (current "boo"))
      (test-equal expected-next
		  (tree-walker-unfold tw tree-walker:next-node))
      ;; boo is the current node
      (test-equal current (node-node-name (tree-walker-current-node tw)))
      (test-equal expected-previous
		  (tree-walker-unfold tw tree-walker:previous-node)))
    (define (test-iterator tw expected-next expected-previous
			   :optional (current "boo"))
      (test-equal expected-next
		  (tree-walker-unfold tw node-iterator:next-node))
      ;; boo is the current node
      (test-equal current (node-node-name (node-iterator-reference-node tw)))
      (test-equal expected-previous
		  (tree-walker-unfold tw node-iterator:previous-node)))
    (test-walker (document:create-tree-walker document document
					      +node-filter-show-element+)
		 '("root" "foo:foo" "foo:bar" "boo")
		 '("foo:bar" "foo:foo" "root"))
    (test-walker (document:create-tree-walker document
		   (document-document-element document)
		   +node-filter-show-element+)
		 '("foo:foo" "foo:bar" "boo")
		 '("foo:bar" "foo:foo" "root"))
    (test-walker (document:create-tree-walker document
		   (document:get-element-by-id document "id-of-foo")
		   +node-filter-show-element+)
		   '("foo:bar")
		   '("foo:foo")
		   "foo:bar")
    (test-iterator (document:create-node-iterator document document
						  +node-filter-show-element+)
		   '("root" "foo:foo" "foo:bar" "boo")
		   '("boo" "foo:bar" "foo:foo" "root"))
    (test-iterator (document:create-node-iterator document
		     (document-document-element document)
		     +node-filter-show-element+)
		   '("root" "foo:foo" "foo:bar" "boo")
		   '("boo" "foo:bar" "foo:foo" "root"))
    (test-iterator (document:create-node-iterator document
		     (document:get-element-by-id document "id-of-foo")
		     +node-filter-show-element+)
		   '("foo:foo" "foo:bar")
		   '("foo:bar" "foo:foo")
		   "foo:bar")))

(let* ((document (make-xml-document))
       (e (document:create-element-ns document "urn:foo" "foo:foo")))
  (node:append-child! document e)
  (element:set-attribute-ns! e "urn:foo" "foo:bla" "blabla")
  (element:set-attribute! e "foo:buz" "buzzz")
  (node:append-child! e (document:create-element-ns document
						    "urn:foo" "foo:bar"))
  (test-assert (element:has-attributes? e))
  (test-assert (element:has-attribute? e "foo:bla"))
  (test-assert (element:has-attribute-ns? e "urn:foo" "bla"))
  (test-assert (lset= string=? '("xmlns:foo" "foo:bla" "foo:buz")
		      (element:get-attribute-names e))))

(let* ((document (make-xml-document))
       (data "abcdefg")
       (text (document:create-text-node document data)))
  (test-error assertion-violation? (character-data:substring-data text 100 3))
  (test-equal "bcd" (character-data:substring-data text 1 3))
  (test-equal data (character-data:substring-data text 0 -1))
  (test-equal data (character-data:substring-data text 0 100))

  (test-assert (character-data:append-data! text "hijk"))
  (test-equal "abcdefghijk" (character-data-data text))
  (test-assert (character-data:insert-data! text 0 "z"))
  (test-equal "zabcdefghijk" (character-data-data text))
  (test-error (character-data:insert-data! text 100 "a"))
  (test-equal "zabcdefghijk" (character-data-data text))
  (test-assert (character-data:insert-data! text 5 "zz"))
  (test-equal "zabcdzzefghijk" (character-data-data text))
  (test-assert (character-data:insert-data! text 1 "w"))
  (test-equal "zwabcdzzefghijk" (character-data-data text))

  (test-error assertion-violation? (character-data:delete-data! text -1 0))
  (test-error assertion-violation? (character-data:delete-data! text 20 0))
  (test-error assertion-violation? (character-data:delete-data! text 0 20))
  (test-assert (character-data:delete-data! text 5 3))
  (test-equal "zwabcefghijk" (character-data-data text))

  (test-error assertion-violation? (character-data:replace-data! text -1 0 ""))
  (test-error assertion-violation? (character-data:replace-data! text 20 0 ""))
  (test-error assertion-violation? (character-data:replace-data! text 0 20 ""))
  (test-assert (character-data:replace-data! text 5 3 "**data**"))
  (test-equal "zwabc**data**hijk" (character-data-data text))
  )

(test-end)
