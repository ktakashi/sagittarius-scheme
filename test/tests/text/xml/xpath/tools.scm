(import (rnrs)
	(text xml dom)
	(text xml xpath tools)
	(srfi :127)
	(sagittarius generators)
	(srfi :64))

(test-begin "XPath - tools")

(define (select-xml-node-names selector xml)
  (define dom (input-port->dom-tree (open-string-input-port xml)))
  (define node-list (selector (document-document-element dom)))
  (do ((len (node-list-length node-list)) (i 0 (+ i 1))
       (res '() (cons (node-node-name (node-list:item node-list i)) res)))
      ((= len i) (reverse res))))

(define-syntax test-selector
  (syntax-rules ()
    ((_ selector expected xml)
     (test-equal xml expected (select-xml-node-names selector xml)))))

(test-selector (xml:descendant element?) '("bar" "baz")
	       "<foo><bar><baz></baz></bar></foo>")
(test-selector (xml:descendant-or-self element?) '("foo" "bar" "baz")
	       "<foo><bar><baz></baz></bar></foo>")

;; comment has a special node name
(test-selector (xml:child comment?) '("#comment") "<foo><!-- comment --></foo>")

;; ancestor needs to be tested differently
#;(let* ((xml "<foo><bar> <baz id='child'></baz> </bar></foo>")
       (dom (input-port->dom-tree (open-string-input-port xml)))
       (elm (document:get-element-by-id dom "child")))
  (define (ansestor-selector selector element)
    (define node-list (selector element))
    (do ((len (node-list-length node-list)) (i 0 (+ i 1))
	 (res '() (cons (node-node-name (node-list:item node-list i)) res)))
	((= len i) (reverse res))))
  (print (ansestor-selector (xml:ancestor element?) elm))
  (test-equal '("bar" "foo") (ansestor-selector (xml:ancestor element?) elm))
  (test-equal '("baz" "bar" "foo")
	      (ansestor-selector (xml:ancestor-or-self element?) elm)))


(test-end)
