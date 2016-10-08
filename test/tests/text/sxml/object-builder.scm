(import (rnrs)
	(text sxml object-builder)
	(srfi :64))

(test-begin "SXML to Scheme object")

(define (make-protocol tag)
  (lambda (n)
    (lambda (name attr content)
      ((n tag attr content)))))
(define-record-type root
  (parent xml-object)
  (protocol (make-protocol 'root)))
(define-record-type child
  (parent xml-object)
  (protocol (make-protocol 'child)))
(define-record-type grand-child
  (parent xml-object)
  (protocol (make-protocol 'grand-child)))

(let ()
  (define grand-child-builder
    (make-simple-object-builder
     (lambda (tag) (eq? tag 'grand-child))
     make-grand-child
     #f))
  (define child-builder
    (make-simple-object-builder
     (lambda (tag) (eq? tag 'child))
     make-child
     grand-child-builder))
  (define root-builder
    (make-simple-object-builder
     (lambda (tag) (eq? tag 'root))
     make-root
     child-builder))
  
  (test-assert (root? (sxml->object '(root) root-builder)))
  (test-assert (root? (sxml->object '(root (child)) root-builder)))
  (test-assert (root? (sxml->object '(root (child (grand-child)))
				    root-builder)))

  (test-assert (child? (sxml->object '(child) child-builder)))
  (test-assert (child? (sxml->object '(child (grand-child)) child-builder)))

  (test-assert (grand-child? (sxml->object '(grand-child) grand-child-builder)))


  (let ((root (sxml->object '(root (child)) root-builder)))
    (test-equal 1 (length (xml-object-contents root)))
    (test-equal '(#t) (map child? (xml-object-contents root))))
  )

(let ()
  (define root-builder
    (sxml-object-builder
     (root make-root
       (child make-child
	 (grand-child make-grand-child)))))

  (test-assert (root? (sxml->object '(root (child (grand-child)))
				    root-builder)))
  (let ((root (sxml->object '(root (child)) root-builder)))
    (test-equal 1 (length (xml-object-contents root)))
    (test-equal '(#t) (map child? (xml-object-contents root))))
  )

(let ()
  (define root-builder
    (sxml-object-builder
     (root make-root
       (* child make-child (grand-child make-grand-child)))))

  (test-assert (root? (sxml->object '(root (child (grand-child))
				       (child (grand-child)))
				    root-builder)))
  (let ((root (sxml->object '(root (child) (child)) root-builder)))
    (test-equal 2 (length (car (xml-object-contents root))))
    (test-equal '(#t #t) (map child? (car (xml-object-contents root)))))
  )

(let ()
  (define sxml
    '(*TOP* (*PI* xml "version=\"1.0\"")
	    (tag (@ (attr "val"))
		 "foo"
		 (tag (@) "bar"))))
  (test-assert (xml-object? (sxml->xml-object sxml)))
  (let ((o (sxml->xml-object sxml)))
    (test-equal 'tag (xml-object-name o))
    (test-equal 2 (length (xml-object-contents o)))
    (test-equal '((attr "val")) (xml-object-attributes o))
    (let ((c (cadr (xml-object-contents o))))
      (test-equal 'tag (xml-object-name c))
      (test-equal 1 (length (xml-object-contents c)))
      (test-equal '("bar") (xml-object-contents c)))))

(test-end)
