;; -*- mode:scheme; coding: utf-8 -*-
;; not so beautiful
(library (text sxml sxpath private)
    (export sxml:attr-list nodeset? ntype-names?? ntype??
	    sxml:filter select-kids sxml:element? map-union as-nodeset)
    (import (rnrs))
; Apply proc to each element of lst and return the list of results.
; if proc returns a nodelist, splice it into the result
;
; From another point of view, map-union is a function Converter->Converter,
; which places an argument-converter in a joining context.
(define (map-union proc lst)
  (if (null? lst) lst
      (let ((proc-res (proc (car lst))))
	((if (nodeset? proc-res) append cons)
	 proc-res (map-union proc (cdr lst))))))

; Predicate which returns #t if <obj> is SXML element, otherwise returns #f. 
(define (sxml:element? obj)	
   (and (pair? obj)
	(symbol? (car obj))
	(not (memq (car obj) 
			   ; '(@ @@ *PI* *COMMENT* *ENTITY* *NAMESPACES*)
			   ; the line above is a workaround for old SXML
	'(@ @@ *PI* *COMMENT* *ENTITY*)))))

; select-kids:: Pred -> Node -> Nodelist
; Given a Node, return an (ordered) subset its children that satisfy
; the Pred (a converter, actually)
; select-kids:: Pred -> Nodelist -> Nodelist
; The same as above, but select among children of all the nodes in
; the Nodelist
;
; More succinctly, the signature of this function is
; select-kids:: Converter -> Converter
(define (select-kids test-pred?)
  (lambda (node)		; node or node-set
    (cond 
     ((null? node) node)
     ((not (pair? node)) '())   ; No children
     ((symbol? (car node))
      ((sxml:filter test-pred?) (cdr node)))	; it's a single node
     (else (map-union (select-kids test-pred?) node)))))

; filter:: Converter -> Converter
; A filter applicator, which introduces a filtering context. The argument
; converter is considered a predicate, with either #f or nil result meaning
; failure.
(define (sxml:filter pred?)
  (lambda (lst)	; a nodelist or a node (will be converted to a singleton nset)
    (let loop ((lst (as-nodeset lst)) 
	       (res '()))
      (if (null? lst)
	  (reverse res)
	  (let ((pred-result (pred? (car lst))))
	    (loop (cdr lst)
		  (if (and pred-result (not (null? pred-result)))
		      (cons (car lst) res)
		      res)))))))


; The function ntype?? takes a type criterion and returns
; a function, which, when applied to a node, will tell if the node satisfies
; the test.
;	ntype?? :: Crit -> Node -> Boolean
;
; The criterion 'crit' is 
;  one of the following symbols:
;	id		- tests if the Node has the right name (id)
;	@		- tests if the Node is an <attributes-list>
;	*		- tests if the Node is an <Element>
;	*text*		- tests if the Node is a text node
;	*data*		- tests if the Node is a data node 
;                         (text, number, boolean, etc., but not pair)
;	*PI*		- tests if the Node is a PI node
;	*COMMENT*	- tests if the Node is a COMMENT node
;	*ENTITY*        - tests if the Node is a ENTITY node
;	*any*		- #t for any type of Node
(define (ntype?? crit)
  (case crit
    ((*) sxml:element?)
    ((*any*) (lambda (node) #t))
    ((*text*) (lambda (node) (string? node)))
    ((*data*) (lambda (node) (not (pair? node))))
    (else (lambda (node) (and (pair? node) (eq? crit (car node)))))
    ))

; Returns the list of attributes for a given SXML node
; Empty list is returned if the given node os not an element,
; or if it has no list of attributes
(define (sxml:attr-list obj)
  (if (and  (sxml:element? obj) 
	    (not (null? (cdr obj)))
	    (pair? (cadr obj)) 
	    (eq? '@ (caadr obj)))
	 (cdadr obj)
	 '()))

; Returns #t if given object is a nodelist
(define (nodeset? x)
  (or (and (pair? x) (not (symbol? (car x)))) (null? x)))

; If x is a nodelist - returns it as is, otherwise wrap it in a list.
(define (as-nodeset x)
  (if (nodeset? x) x (list x)))

; The function ntype-names?? takes a list of acceptable node names as a
; criterion and returns a function, which, when applied to a node, 
; will return #t if the node name is present in criterion list and #f
; othervise.
;	ntype-names?? :: ListOfNames -> Node -> Boolean
(define (ntype-names?? crit)
  (lambda(node)
    (and (pair? node)
	 (memq (car node) crit))))

)
