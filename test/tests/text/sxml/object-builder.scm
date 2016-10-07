(import (rnrs)
	(text sxml object-builder)
	(srfi :64))

(test-begin "SXML to Scheme object")

(define-record-type xml-object
  (fields tag attr contents))
(define (make-protocol tag)
  (lambda (n)
    (lambda (attr content)
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

(test-end)
