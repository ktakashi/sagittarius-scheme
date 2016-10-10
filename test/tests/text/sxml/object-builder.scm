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
  (define-record-type choice1
    (parent xml-object)
    (protocol (make-protocol 'choice1)))
  (define-record-type choice2
    (parent xml-object)
    (protocol (make-protocol 'choice2)))
  (define-record-type choice3
    (parent xml-object)
    (protocol (make-protocol 'choice3)))
  ;; simple case
  (let ()
    (define choice-builder
      (sxml-object-builder
       (/ (choice1 make-choice1)
	  (choice2 make-choice2)
	  (choice3 make-choice3))))

    (test-assert (choice1? (sxml->object '(choice1) choice-builder)))
    (test-assert (choice2? (sxml->object '(choice2) choice-builder)))
    (test-assert (choice3? (sxml->object '(choice3) choice-builder)))
    (test-error assertion-violation? (sxml->object '(choice4) choice-builder)))
  ;; combination
  (let ()
    (define choice-builder
      (sxml-object-builder
       (root make-root
	 (child make-child)
	 (/ (choice1 make-choice1)
	    (choice2 make-choice2)
	    (choice3 make-choice3)))))
    (define (test-choice sxml pred?)
      (let ((obj (sxml->object sxml choice-builder)))
	(test-assert (pred? (cadr (xml-object-contents obj))))))
    (test-choice '(root (child) (choice1)) choice1?)
    (test-choice '(root (child) (choice2)) choice2?)
    (test-choice '(root (child) (choice3)) choice3?)))

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
