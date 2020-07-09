(import (rnrs)
	(text xml dom)
	(text xml errors)
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

(define (string->dom xml)
  (input-port->dom-tree (open-string-input-port xml)))

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
(let* ((dom (string->dom "<foo><bar> <baz id='child'></baz> </bar></foo>"))
       (elm (document:get-element-by-id dom "child")))
  (define (ansestor-selector selector element)
    (define node-list (selector element))
    (do ((len (node-list-length node-list)) (i 0 (+ i 1))
	 (res '() (cons (node-node-name (node-list:item node-list i)) res)))
	((= len i) (reverse res))))
  (test-equal '("foo" "bar") (ansestor-selector (xml:ancestor element?) elm))
  (test-equal '("foo" "bar" "baz")
	      (ansestor-selector (xml:ancestor-or-self element?) elm)))


(let* ((dom (string->dom "<foo a='1'><bar b='2'><baz id='child'></baz></bar></foo>"))
       (elm (document-document-element dom)))
  (define a=1
    (lambda (attr)
      (and (equal? (attr-name attr) "a")
	   (equal? (attr-value attr) "1"))))
  (define b=2
    (lambda (attr)
      (and (equal? (attr-name attr) "b")
	   (equal? (attr-value attr) "2"))))
  (define (attribute-selector selector element)
    (define node-list (selector element))
    (do ((len (node-list-length node-list)) (i 0 (+ i 1))
	 (res '() (cons (node-node-name (node-list:item node-list i)) res)))
	((= len i) (reverse res))))
  (test-equal '("a") (attribute-selector (xml:attribute a=1) elm))
  ;; works only the current node
  (test-equal '() (attribute-selector (xml:attribute b=2) elm)))

(let* ((dom (string->dom "<foo><bar><baz id='child'/></bar></foo>"))
       (nl1 (node-child-nodes (document-document-element dom)))
       (nl2 (node-child-nodes (document:get-element-by-id dom "child"))))
  (test-assert "xml:boolean (true) s" (xml:boolean "1"))
  (test-assert "xml:boolean (true) n" (xml:boolean 1))
  (test-assert "xml:boolean (true) b" (xml:boolean #t))
  (test-assert "xml:boolean (true) l" (xml:boolean nl1))
  (test-assert "xml:boolean (false) s" (not (xml:boolean "")))
  (test-assert "xml:boolean (false) n" (not (xml:boolean 0)))
  (test-assert "xml:boolean (false) b" (not (xml:boolean #f)))
  (test-assert "xml:boolean (false) l" (not (xml:boolean nl2)))

  (test-error xqt-error? (xml:boolean 'a)))

(test-end)
