;; -*- mode:scheme; coding: utf-8 -*-
;; sxpath.scm - SXPath: SXML Query Language
;; This file is ported from following files.
;;   - sxpathlib.scm
;;   - sxpath.scm
;;   - sxpath-ext.scm
;;   - txpath.scm
#!nobacktrace
(library (text sxml sxpath)
    (export sxpath nodeset? as-nodeset sxml:element? ntype-names??
	    ntype?? ntype-namespace-id?? sxml:complement node-eq? node-equal?
	    node-pos sxml:filter take-until take-after map-union
	    node-parent
	    node-reverse node-trace select-kids node-self node-join
	    node-reduce node-or node-closure
	    if-sxpath if-car-sxpath car-sxpath
	    sxml:id-alist
	    sxml:string sxml:boolean sxml:number sxml:string-value
	    sxml:node? sxml:attr-list sxml:id sxml:equality-cmp
	    sxml:equal? sxml:not-equal? sxml:relational-cmp
	    sxml:attribute sxml:child sxml:parent sxml:ancestor
	    sxml:ancestor-or-self sxml:descendant sxml:descendant-or-self
	    sxml:following sxml:following-sibling sxml:namespace
	    sxml:preceding sxml:preceding-sibling
	    sxml:child-nodes sxml:child-elements
	    sxml:xpath
	    sxml:xpath+index
	    sxml:xpath+root+vars
	    sxml:xpointer+index
	    sxml:xpointer+root+vars)
    (import (rnrs)
	    (rnrs r5rs)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (pp)
	    (sagittarius io)
	    (text parse)
	    (text sxml tools)
	    (text sxml sxpath private)
	    (text sxml txpath-parser)
	    (text sxml helper))

;;			XML processing in Scheme
;		     SXPath -- SXML Query Language
;
; $Id: sxpathlib.scm,v 3.918 2004/02/05 22:52:33 kl Exp kl $
;
; This code is in Public Domain
; It's based on SXPath by Oleg Kiselyov, and multiple improvements 
; implemented by Dmitry Lizorkin.
;
; The list of differences from original SXPath.scm my be found in changelog.txt
; 
;  Kirill Lisovsky    lisovsky@acm.org
;
;                                 *  *  *
;
; SXPath is a query language for SXML, an instance of XML Information
; set (Infoset) in the form of s-expressions. See SSAX.scm for the
; definition of SXML and more details. SXPath is also a translation into
; Scheme of an XML Path Language, XPath:
;	http://www.w3.org/TR/xpath
; XPath and SXPath describe means of selecting a set of Infoset's items
; or their properties.
;
; To facilitate queries, XPath maps the XML Infoset into an explicit
; tree, and introduces important notions of a location path and a
; current, context node. A location path denotes a selection of a set of
; nodes relative to a context node. Any XPath tree has a distinguished,
; root node -- which serves as the context node for absolute location
; paths. Location path is recursively defined as a location step joined
; with a location path. A location step is a simple query of the
; database relative to a context node. A step may include expressions
; that further filter the selected set. Each node in the resulting set
; is used as a context node for the adjoining location path. The result
; of the step is a union of the sets returned by the latter location
; paths.
;
; The SXML representation of the XML Infoset (see SSAX.scm) is rather
; suitable for querying as it is. Bowing to the XPath specification,
; we will refer to SXML information items as 'Nodes':
; 	<Node> ::= <Element> | <attributes-coll> | <attrib>
; 		   | "text string" | <PI>
; This production can also be described as
;	<Node> ::= (name . <Nodelist>) | "text string"
; An (ordered) set of nodes is just a list of the constituent nodes:
; 	<Nodelist> ::= (<Node> ...)
; Nodelists, and Nodes other than text strings are both lists. A
; <Nodelist> however is either an empty list, or a list whose head is not
; a symbol.  A symbol at the head of a node is either an XML name (in
; which case it's a tag of an XML element), or an administrative name
; such as '@'.  This uniform list representation makes processing rather
; simple and elegant, while avoiding confusion. The multi-branch tree
; structure formed by the mutually-recursive datatypes <Node> and
; <Nodelist> lends itself well to processing by functional languages.
;
; A location path is in fact a composite query over an XPath tree or
; its branch. A singe step is a combination of a projection, selection
; or a transitive closure. Multiple steps are combined via join and
; union operations. This insight allows us to _elegantly_ implement
; XPath as a sequence of projection and filtering primitives --
; converters -- joined by _combinators_. Each converter takes a node
; and returns a nodelist which is the result of the corresponding query
; relative to that node. A converter can also be called on a set of
; nodes. In that case it returns a union of the corresponding queries over
; each node in the set. The union is easily implemented as a list
; append operation as all nodes in a SXML tree are considered
; distinct, by XPath conventions. We also preserve the order of the
; members in the union. Query combinators are high-order functions:
; they take converter(s) (which is a Node|Nodelist -> Nodelist function)
; and compose or otherwise combine them. We will be concerned with
; only relative location paths [XPath]: an absolute location path is a
; relative path applied to the root node.
;
; Similarly to XPath, SXPath defines full and abbreviated notations
; for location paths. In both cases, the abbreviated notation can be
; mechanically expanded into the full form by simple rewriting
; rules. In case of SXPath the corresponding rules are given as
; comments to a sxpath function, below. The regression test suite at
; the end of this file shows a representative sample of SXPaths in
; both notations, juxtaposed with the corresponding XPath
; expressions. Most of the samples are borrowed literally from the
; XPath specification, while the others are adjusted for our running
; example, tree1.
;


;=============================================================================
; Basic converters and applicators
; A converter is a function
;	type Converter = Node|Nodelist -> Nodelist
; A converter can also play a role of a predicate: in that case, if a
; converter, applied to a node or a nodelist, yields a non-empty
; nodelist, the converter-predicate is deemed satisfied. Throughout
; this file a nil nodelist is equivalent to #f in denoting a failure.

;; nodeset? has been moved to tools.scm
;; as-nodeset has been moved to tools.scm

;-----------------------------------------------------------------------------
; Node test
; The following functions implement 'Node test's as defined in
; Sec. 2.3 of XPath document. A node test is one of the components of a
; location step. It is also a converter-predicate in SXPath.

;; sxml:element? has been moved to tools.scm
;; ntype-names?? has been moved to tools.scm

;; ntype??  has been moved to tools.scm
; This function takes a namespace-id, and returns a predicate
; Node -> Boolean, which is #t for nodes with this very namespace-id.
; ns-id is a string
; (ntype-namespace-id?? #f) will be #t for nodes with non-qualified names.
(define (ntype-namespace-id?? ns-id)
  (lambda (node)
    (and (pair? node)
	 (not (memq (car node) 
			 '(@ @@ *PI* *COMMENT* *ENTITY*)))
	 (let ((nm (symbol->string (car node))))
	   (cond 
	     ((string-rindex nm #\:)	   
	      => (lambda (pos) 
	      (and 
		(= pos (string-length ns-id))
		(string-prefix? ns-id nm))))
	     (else (not ns-id)))))))
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

; This function takes a predicate and returns it complemented 
; That is if the given predicate yelds #f or '() the complemented one  
; yields the given node (#t) and vice versa.
(define (sxml:complement pred)
  (lambda(node)
    (case (pred node)
      ((#f '()) node)
      (else #f))))

; Curried equivalence converter-predicates
(define (node-eq? other)
  (lambda (node)
    (eq? other node)))

(define (node-equal? other)
  (lambda (node)
    (equal? other node)))

; node-pos:: N -> Nodelist -> Nodelist, or
; node-pos:: N -> Converter
; Select the N'th element of a Nodelist and return as a singular Nodelist;
; Return an empty nodelist if the Nth element does not exist.
; ((node-pos 1) Nodelist) selects the node at the head of the Nodelist,
; if exists; ((node-pos 2) Nodelist) selects the Node after that, if
; exists.
; N can also be a negative number: in that case the node is picked from
; the tail of the list.
; ((node-pos -1) Nodelist) selects the last node of a non-empty nodelist;
; ((node-pos -2) Nodelist) selects the last but one node, if exists.
(define (node-pos n)
  (lambda (nodelist)
    (cond
     ((not (nodeset? nodelist)) '())
     ((null? nodelist) nodelist)
     ((eqv? n 1) (list (car nodelist)))
     ((negative? n) ((node-pos (- n)) (reverse nodelist)))
     (else
      (assert (positive? n))
      ((node-pos (-- n)) (cdr nodelist))))))

;; sxml:filter has been moved to tools.scm


; take-until:: Converter -> Converter, or
; take-until:: Pred -> Node|Nodelist -> Nodelist
; Given a converter-predicate and a nodelist, apply the predicate to
; each element of the nodelist, until the predicate yields anything but #f or
; nil. Return the elements of the input nodelist that have been processed
; till that moment (that is, which fail the predicate).
; take-until is a variation of the filter above: take-until passes
; elements of an ordered input set till (but not including) the first
; element that satisfies the predicate.
; The nodelist returned by ((take-until (not pred)) nset) is a subset -- 
; to be more precise, a prefix -- of the nodelist returned by
; ((filter pred) nset)
(define (take-until pred?)
  (lambda (lst)	; a nodelist or a node (will be converted to a singleton nset)
    (let loop ((lst (as-nodeset lst)))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		'()
		(cons (car lst) (loop (cdr lst)))))
	  ))))

; take-after:: Converter -> Converter, or
; take-after:: Pred -> Node|Nodelist -> Nodelist
; Given a converter-predicate and a nodelist, apply the predicate to
; each element of the nodelist, until the predicate yields anything but #f or
; nil. Return the elements of the input nodelist that have not been processed:
; that is, return the elements of the input nodelist that follow the first
; element that satisfied the predicate.
; take-after along with take-until partition an input nodelist into three
; parts: the first element that satisfies a predicate, all preceding
; elements and all following elements.
(define (take-after pred?)
  (lambda (lst)	; a nodelist or a node (will be converted to a singleton nset)
    (let loop ((lst (as-nodeset lst)))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		(cdr lst)
		(loop (cdr lst))))
	  ))))

;; map-union has been moved to tools.scm

; node-reverse :: Converter, or
; node-reverse:: Node|Nodelist -> Nodelist
; Reverses the order of nodes in the nodelist
; This basic converter is needed to implement a reverse document order
; (see the XPath Recommendation).
(define node-reverse 
  (lambda (node-or-nodelist)
    (if (not (nodeset? node-or-nodelist)) (list node-or-nodelist)
	(reverse node-or-nodelist))))

; node-trace:: String -> Converter
; (node-trace title) is an identity converter. In addition it prints out
; a node or nodelist it is applied to, prefixed with the 'title'.
; This converter is very useful for debugging.
(define (node-trace title)
  (lambda (node-or-nodelist)
    (cout nl "-->" title " :")
    (pp node-or-nodelist)
    node-or-nodelist))


;------------------------------------------------------------------------------
; Converter combinators
;
; Combinators are higher-order functions that transmogrify a converter
; or glue a sequence of converters into a single, non-trivial
; converter. The goal is to arrive at converters that correspond to
; XPath location paths.
;
; From a different point of view, a combinator is a fixed, named
; _pattern_ of applying converters. Given below is a complete set of
; such patterns that together implement XPath location path
; specification. As it turns out, all these combinators can be built
; from a small number of basic blocks: regular functional composition,
; map-union and filter applicators, and the nodelist union.


;; select-kids has been moved to tools.scm

; node-self:: Pred -> Node -> Nodelist, or
; node-self:: Converter -> Converter
; Similar to select-kids but apply to the Node itself rather
; than to its children. The resulting Nodelist will contain either one
; component, or will be empty (if the Node failed the Pred).
(define node-self sxml:filter)


; node-join:: [LocPath] -> Node|Nodelist -> Nodelist, or
; node-join:: [Converter] -> Converter
; join the sequence of location steps or paths as described
; in the title comments above.
(define (node-join . selectors)
  (lambda (nodelist)		; Nodelist or node
    (let loop ((nodelist nodelist) (selectors selectors))
      (if (null? selectors) nodelist
	  (loop 
	   (if (nodeset? nodelist)
	       (map-union (car selectors) nodelist)
	       ((car selectors) nodelist))
	   (cdr selectors))))))


; node-reduce:: [LocPath] -> Node|Nodelist -> Nodelist, or
; node-reduce:: [Converter] -> Converter
; A regular functional composition of converters.
; From a different point of view,
;    ((apply node-reduce converters) nodelist)
; is equivalent to
;    (foldl apply nodelist converters)
; i.e., folding, or reducing, a list of converters with the nodelist
; as a seed.
(define (node-reduce . converters)
  (lambda (nodelist)		; Nodelist or node
    (let loop ((nodelist nodelist) (converters converters))
      (if (null? converters) nodelist
	  (loop ((car converters) nodelist) (cdr converters))))))


; node-or:: [Converter] -> Converter
; This combinator applies all converters to a given node and
; produces the union of their results.
; This combinator corresponds to a union, '|' operation for XPath
; location paths.
(define (node-or . converters)
  (lambda (node-or-nodelist)
    (let loop ((result '()) (converters converters))
      (if (null? converters) result
	  (loop (append result (or ((car converters) node-or-nodelist) '()))
		(cdr converters))))))


; node-closure:: Converter -> Converter
; Select all _descendants_ of a node that satisfy a converter-predicate.
; This combinator is similar to select-kids but applies to
; grand... children as well.
; This combinator implements the "descendant::" XPath axis
; Conceptually, this combinator can be expressed as
; (define (node-closure f)
;      (node-or
;        (select-kids f)
;	 (node-reduce (select-kids (ntype?? '*)) (node-closure f))))
; This definition, as written, looks somewhat like a fixpoint, and it
; will run forever. It is obvious however that sooner or later
; (select-kids (ntype?? '*)) will return an empty nodelist. At
; this point further iterations will no longer affect the result and
; can be stopped.
(define (node-closure test-pred?)	    
(let ((kid-selector (select-kids test-pred?)))
  (lambda (node) ; Nodelist or node
    (let loop ((parent node) (result '()))
      (if (null? parent) result
	(loop (sxml:child-elements parent)
	  (append result
	    (kid-selector parent)))
	)))))

;=============================================================================
; Unified with sxpath-ext and sxml-tools

; According to XPath specification 2.3, this test is true for any
; XPath node.
; For SXML auxiliary lists and lists of attributes has to be excluded.
(define (sxml:node? node)
  (not (and 
	 (pair? node)
	 (memq (car node) '(@ @@)))))

;; sxml:attr-list has been moved to tools.scm


; Attribute axis
(define (sxml:attribute test-pred?)
  (let ((fltr (sxml:filter test-pred?)))
    (lambda (node)
      (map-union
       (lambda (node) (fltr (sxml:attr-list node)))
       (as-nodeset node)))))

; Child axis
;  This function is similar to 'select-kids', but it returns an empty
;  child-list for PI, Comment and Entity nodes
(define (sxml:child test-pred?)
  (lambda (node)		; node or node-set
    (cond 
      ((null? node) node)
      ((not (pair? node)) '())   ; No children
      ((memq (car node) '(*PI* *COMMENT* *ENTITY*))   ; PI, Comment or Entity
       '())   ; No children
      ((symbol? (car node))    ; it's a single node       
       ((sxml:filter test-pred?) (cdr node)))
      (else (map-union (sxml:child test-pred?) node)))))

; Parent axis
; Given a predicate, it returns a function 
;  RootNode -> Converter
; which which yields a 
;  node -> parent 
; converter then applied to a rootnode.
; Thus, such a converter may be constructed using
;  ((sxml:parent test-pred) rootnode)
; and returns a parent of a node it is applied to.
; If applied to a nodelist, it returns the 
; list of parents of nodes in the nodelist. The rootnode does not have
; to be the root node of the whole SXML tree -- it may be a root node
; of a branch of interest.
; The parent:: axis can be used with any SXML node.
(define (sxml:parent test-pred?)
  (lambda (root-node)   ; node or nodelist
    (lambda (node)   ; node or nodelist
      (if (nodeset? node)
	(map-union ((sxml:parent test-pred?) root-node) node)
	(let rpt ((pairs
		    (apply append
		     (map 
			      (lambda (root-n)
				(map
				  (lambda (arg) (cons arg root-n))
				  (append 
				    (sxml:attr-list root-n)
				    (sxml:child-nodes root-n))))
                              (as-nodeset root-node)))
		     ))
	  (if (null? pairs)
	    '()
	    (let ((pair (car pairs)))
	      (if (eq? (car pair) node)
		((sxml:filter test-pred?) (list (cdr pair)))
		(rpt (append
			(map
			  (lambda (arg) (cons arg (car pair)))
			  (append 
			    (sxml:attr-list (car pair))
			    (sxml:child-nodes (car pair))))
			(cdr pairs)
			))))))))))


;=============================================================================
; Popular short cuts 

; node-parent:: RootNode -> Converter
; (node-parent rootnode) yields a converter that returns a parent of a
; node it is applied to. If applied to a nodelist, it returns the list
; of parents of nodes in the nodelist.
; Given the notation of Philip Wadler's paper on semantics of XSLT,
;  parent(x) = { y | y=subnode*(root), x=subnode(y) }
; Therefore, node-parent is not the fundamental converter: it can be
; expressed through the existing ones.  Yet node-parent is a rather
; convenient converter. It corresponds to a parent:: axis of SXPath.
;
; Please note: this function is provided for backward compatibility 
; with SXPath/SXPathlib ver. 3.5.x.x and earlier.
; Now it's a particular case of 'sxml:parent' application: 
(define node-parent (sxml:parent (ntype?? '*any*)))

(define sxml:child-nodes (sxml:child sxml:node?))

(define sxml:child-elements (select-kids sxml:element?))

;; $Id: sxpath.scm,v 1.5 2005/09/07 09:27:34 lizorkin Exp $
;; Highghest level SXPath 
;; Refactored from sxml-tools.scm and sxpathlib.scm

;==============================================================================
; Abbreviated SXPath

; Evaluate an abbreviated SXPath
;	sxpath:: AbbrPath -> Converter, or
;	sxpath:: AbbrPath -> Node|Nodeset -> Nodeset
; AbbrPath is a list. It is translated to the full SXPath according
; to the following rewriting rules
; (sxpath '()) -> (node-join)
; (sxpath '(path-component ...)) ->
;		(node-join (sxpath1 path-component) (sxpath '(...)))
; (sxpath1 '//) -> (sxml:descendant-or-self sxml:node?)
; (sxpath1 '(equal? x)) -> (select-kids (node-equal? x))
; (sxpath1 '(eq? x))    -> (select-kids (node-eq? x))
; (sxpath1 '(*or* ...))  -> (select-kids (ntype-names??
;                                          (cdr '(*or* ...))))
; (sxpath1 '(*not* ...)) -> (select-kids (sxml:complement 
;                                         (ntype-names??
;                                          (cdr '(*not* ...)))))
; (sxpath1 '(ns-id:* x)) -> (select-kids 
;                                      (ntype-namespace-id?? x))
; (sxpath1 ?symbol)     -> (select-kids (ntype?? ?symbol))
; (sxpath1 ?string)     -> (txpath ?string)
; (sxpath1 procedure)   -> procedure
; (sxpath1 '(?symbol ...)) -> (sxpath1 '((?symbol) ...))
; (sxpath1 '(path reducer ...)) ->
;		(node-reduce (sxpath path) (sxpathr reducer) ...)
; (sxpathr number)      -> (node-pos number)
; (sxpathr path-filter) -> (filter (sxpath path-filter))
(define (sxpath path . ns-binding)
  (let ((ns-binding (if (null? ns-binding) ns-binding (car ns-binding))))
  (let loop ((converters '())
             (root-vars '())  ; a list of booleans, one per location step:
	                      ;  #t - location step function is binary
                              ;  #f - location step function is unary
             (path (if (string? path) (list path) path)))
    (cond
      ((null? path)  ; parsing is finished
       (lambda (node . var-binding)
         (let ((var-binding
                (if (null? var-binding) var-binding (car var-binding))))
           (let rpt ((nodeset (as-nodeset node))
                     (conv (reverse converters))
                     (r-v (reverse root-vars)))
             (if
              (null? conv)  ; the path is over
              nodeset
              (rpt
               (if (car r-v)  ; the current converter consumes 2 arguments
                   ((car conv) nodeset var-binding)
                   ((car conv) nodeset))
               (cdr conv)
               (cdr r-v)))))))
      ; *or* handler
      ((and (pair? (car path)) 
            (not (null? (car path)))
            (eq? '*or* (caar path)))
       (loop (cons (select-kids (ntype-names?? (cdar path))) converters)
             (cons #f root-vars)
             (cdr path)))
      ; *not* handler 
      ((and (pair? (car path)) 
            (not (null? (car path)))
            (eq? '*not* (caar path)))
       (loop (cons
              (select-kids (sxml:complement (ntype-names?? (cdar path))))
              converters)
             (cons #f root-vars)
             (cdr path)))
      ((procedure? (car path))
       (loop (cons (car path) converters)
             (cons #t root-vars)
             (cdr path)))
      ((eq? '// (car path))
       (if (or (null? (cdr path))
               (not (symbol? (cadr path)))
               (eq? (cadr path) '@))
           (loop (cons (sxml:descendant-or-self sxml:node?)
                       converters)
                 (cons #f root-vars)
                 (cdr path))
           (loop (cons (sxml:descendant (ntype?? (cadr path)))
                       converters)
                 (cons #f root-vars)
                 (cddr path))))
      ((symbol? (car path))
       (loop (cons (select-kids (ntype?? (car path))) converters)
             (cons #f root-vars)
             (cdr path)))
      ((string? (car path))
       (and-let*
        ((f (sxml:xpath-expr (car path) ns-binding)))  ; DL: was: txpath
        (loop (cons f converters)
              (cons #t root-vars)
              (cdr path))))
      ((and (pair? (car path)) (eq? 'equal? (caar path)))
       (loop (cons (select-kids (apply node-equal? (cdar path))) converters)
             (cons #f root-vars)
             (cdr path)))
      ; ns-id:* handler 
      ((and (pair? (car path)) (eq? 'ns-id:* (caar path)))
       (loop
        (cons (select-kids (ntype-namespace-id?? (cadar path))) converters)
        (cons #f root-vars)
        (cdr path)))
      ((and (pair? (car path)) (eq? 'eq? (caar path)))
       (loop (cons (select-kids (apply node-eq? (cdar path))) converters)
             (cons #f root-vars)
             (cdr path)))      
      ((pair? (car path))
       (and-let*
        ((select
          (if
           (symbol? (caar path))
           (lambda (node . var-binding)
             ((select-kids (ntype?? (caar path))) node))
           (sxpath (caar path) ns-binding))))
        (let reducer ((reducing-path (cdar path))
                      (filters '()))
          (cond
            ((null? reducing-path)
             (loop
              (cons
               (lambda (node var-binding)
                 (map-union
                  (lambda (node)
                    (let label ((nodeset (select node var-binding))
                                (fs (reverse filters)))
                      (if
                       (null? fs)
                       nodeset
                       (label
                        ((car fs) nodeset var-binding)
                        (cdr fs)))))
                  (if (nodeset? node) node (list node))))
               converters)
              (cons #t root-vars)
              (cdr path)))
            ((number? (car reducing-path))
             (reducer
              (cdr reducing-path)
              (cons
               (lambda (node var-binding)
                 ((node-pos (car reducing-path)) node))
               filters)))
            (else
             (and-let*
              ((func (sxpath (car reducing-path) ns-binding)))
              (reducer
               (cdr reducing-path)
               (cons
                (lambda (node var-binding)
                  ((sxml:filter
                    (lambda (n) (func n var-binding)))
                    node))
                filters))))))))
      (else
       (cerr "Invalid path step: " (car path))
       #f)))))


;==============================================================================
; Wrappers 

; sxpath always returns a list, which is #t in Scheme 
; if-sxpath returns #f instead of empty list
(define (if-sxpath path)
  (lambda (obj)
   (let ((x ((sxpath path) obj)))
     (if (null? x) #f x))))

; Returns first node found, if any.
; Otherwise returns #f.
(define (if-car-sxpath path)
  (lambda (obj)
   (let ((x ((sxpath path) obj)))
     (if (null? x) #f (car x)))))

; Returns first node found, if any.
; Otherwise returns empty list.
(define (car-sxpath path)
  (lambda (obj)
   (let ((x ((sxpath path) obj)))
     (if (null? x) '() (car x)))))

;==============================================================================
; lookup by a value of ID type attribute
; See also sxml:lookup in sxml-tools

; Built an index as a list of (ID_value . element) pairs for given
; node. lpaths are location paths for attributes of type ID.
(define (sxml:id-alist node . lpaths)
  (apply
    append
    (map 
      (lambda(lp)
	(let ((lpr (reverse lp)))
	  (map 
	    (lambda (nd)
	      (cons (sxml:attr nd (car lpr))
		    nd))
	    ; Selects elements with ID attributes
	    ;  using (lpath ,(node-self (sxpath '(@ attrname))))
	    ((sxpath (reverse (cons 
			  (lambda(n r+v)
			   ((node-self (sxpath `(@ ,(car lpr)))) n))
				(cddr lpr)))) node))   
	  ))
      lpaths)))

;; W3C compliant extensions to SXPathlib
; $Id: sxpath-ext.scm,v 1.911 2002/12/06 22:10:53 kl Exp kl $:
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;=========================================================================
; SXML counterparts to W3C XPath Core Functions Library

; The counterpart to XPath 'string' function (section 4.2 XPath Rec.)
; Converts a given object to a string
; NOTE:
;  1. When converting a nodeset - a document order is not preserved
;  2. number->string function returns the result in a form which is slightly
; different from XPath Rec. specification
(define (sxml:string object)
  (cond
    ((string? object) object)
    ((nodeset? object) (if (null? object)
			 ""
			 (sxml:string-value (car object))))
    ((number? object)
     (if (and (rational? object) (not (integer? object)))  ; like 1/2
         (number->string (exact->inexact object))
         (number->string object)))
    ((boolean? object) (if object "true" "false"))
    (else "")))  ; Unknown type -> empty string. 
                 ; Option: write its value to string port?

; The counterpart to XPath 'boolean' function (section 4.3 XPath Rec.)
; Converts its argument to a boolean
(define (sxml:boolean object)
  (cond
    ((boolean? object) object)
    ((number? object) (not (= object 0)))
    ((string? object) (> (string-length object) 0))
    ((nodeset? object) (not (null? object)))
    (else #f)))  ; Not specified in XPath Rec.

; The counterpart to XPath 'number' function (section 4.4 XPath Rec.)
; Converts its argument to a number
; NOTE: 
;  1. The argument is not optional (yet?)
;  2. string->number conversion is not IEEE 754 round-to-nearest
;  3. NaN is represented as 0
(define (sxml:number obj)
  (cond
    ((number? obj) obj)
    ((string? obj)
     (let ((nmb (call-with-input-string obj read)))
       (if (number? nmb)
	 nmb
	 0))) ; NaN
    ((boolean? obj) (if obj 1 0))
    ((nodeset? obj) (sxml:number (sxml:string obj)))
    (else 0))) ; unknown datatype

; Returns a string value for a given node in accordance to
; XPath Rec. 5.1 - 5.7 
(define (sxml:string-value node)
  (cond
    ((not (pair? node))  ; a text node or data node
     (sxml:string node))
    ((null? (cdr node))
     "")
    (else
     (apply string-append  ; a list of arguments is always non-null
            (map
             (lambda (node)
               (if (sxml:node? node)  ; not annot-attr node or aux list node
                   (sxml:string-value node) ""))
             (cdr node))))))

; Select SXML element by its unique IDs
; XPath Rec. 4.1
;  object - a nodeset or a datatype which can be converted to a string by means
; of a 'string' function
;  id-index = ( (id-value . element) (id-value . element) ... ) 
; This index is used for selection of an element by its unique ID. 
; The result is a nodeset
(define (sxml:id id-index)
  (lambda(object)
    (if (nodeset? object)
      (let loop ((str-lst (map sxml:string-value object))
		 (res '()))
	(if (null? str-lst)
	  (reverse res)
	  (let ((node (sxml:lookup (car str-lst) id-index)))
	    (if (not node)  ; no such element
	      (loop (cdr str-lst) res)
	      (loop (cdr str-lst) (cons node res))))))
      (let rpt ((lst (string->list (sxml:string object)))
		(tmp '())
		(res '()))
	(cond
	  ((null? lst)
	   (if (null? tmp) 
	     (reverse res)
	     (let ((node (sxml:lookup (list->string (reverse tmp)) id-index)))
	       (if (not node)
		 (reverse res)
		 (reverse (cons node res))))))
	  ((member (car lst) '(#\space #\return #\newline #\tab))
	   (if (null? tmp)
	     (rpt (cdr lst) tmp res)
	     (let ((node (sxml:lookup (list->string (reverse tmp)) id-index)))
	       (if (not node)
		 (rpt (cdr lst) '() res)
		 (rpt (cdr lst) '() (cons node res))))))
	  (else (rpt (cdr lst) (cons (car lst) tmp) res)))))))
                       
             
;=========================================================================
; Comparators for XPath objects 

; Implements XPath equality comparison in a straightforward nested loop manner
(define (sxml:nested-loop-join string-set1 string-set2 string-op)
  (let first ((str-set1 string-set1)
              (str-set2 string-set2))
    (cond
      ((null? str-set1) #f)
      ((let second ((elem (car str-set1))
                    (set2 str-set2))
         (cond
           ((null? set2) #f)
           ((string-op elem (car set2)) #t)
           (else (second elem (cdr set2))))) #t)
      (else
       (first (cdr str-set1) str-set2)))))

;-------------------------------------------------
; Merge-sort for speeding up equality comparison of two nodesets

; Similar to R5RS 'list-tail' but returns the new list consisting of the first
; 'k' members of 'lst'
(define (sxml:list-head lst k)
  (if (or (null? lst) (zero? k))
      '()
      (cons (car lst) (sxml:list-head (cdr lst) (- k 1)))))

; Implements merge-sort of the given lst
; Returns the sorted list, the smallest member first
; less-than?-pred ::= (lambda (obj1 obj2) ...)
; less-than?-pred returns #t if obj1<obj2 with respect to the given ordering
(define (sxml:merge-sort less-than?-pred lst)
  (letrec
      ((merge-sorted-lists
        ; Merges 2 sorted lists into one sorted list
        (lambda (lst1 lst2)
          (cond
            ((null? lst1) lst2)
            ((null? lst2) lst1)
            ; both lists are non-null here
            ((less-than?-pred (car lst1) (car lst2))
             (cons (car lst1)
                   (merge-sorted-lists (cdr lst1) lst2)))
            (else
             (cons (car lst2)
                   (merge-sorted-lists lst1 (cdr lst2))))))))
    (if
     (or (null? lst) (null? (cdr lst)))  ; already sorted
     lst
     (let ((middle (inexact->exact (round (/ (length lst) 2)))))
       (merge-sorted-lists
        (sxml:merge-sort less-than?-pred (sxml:list-head lst middle))
        (sxml:merge-sort less-than?-pred (list-tail lst middle)))))))

; Implementation of XPath equality comparison for 2 string-sets with
; merge-sort join algorithm
(define (sxml:merge-sort-join string-set1 string-set2 string-op)
  (let loop ((str-set1 (sxml:merge-sort string<? string-set1))
             (str-set2 (sxml:merge-sort string<? string-set2)))
    (cond
      ((or (null? str-set1) (null? str-set2))
       #f)
      ((string-op (car str-set1) (car str-set2))
       ; comparison condition fulfilled for a pair of nodes
       #t)
      ((string<? (car str-set1) (car str-set2))
       ; we can remove (car str-set1) from our further consideration
       (loop (cdr str-set1) str-set2))
      (else  ; vice versa
       (loop str-set1 (cdr str-set2))))))

;-------------------------------------------------
; Radix-sort join for equality comparison of 2 nodesets
; The running time of the algorithm is proportional to the nodeset size and
; to node string-value length
; 
; Since each nodeset contains O(n) nodes and string-value for each node is
; O(n) in length, radix-sort join algorithm evaluates in O(n^2) time. By
; comparison, nested loop join requires O(n^3) time, merge-sort join
; implemented above requires O(n^2*log(n)).
;
; On the other hand, radix-sort join is time-ineffective for relatively small
; nodesets being joined. For small nodesets, the above implemented sort-merge
; join runs more effectively. Radix-sort join is promising for large nodesets.

; Represents a list of chars as a branch in the string-tree
; The list of chars must be non-empty
(define (sxml:charlst->branch lst)
  (if (null? (cdr lst))  ; this is the last character in the lst
      `(,(car lst) #t)
      `(,(car lst) #f ,(sxml:charlst->branch (cdr lst)))))

; Converts a string to a string-tree
(define (sxml:string->tree str)
  (let ((lst (string->list str)))
    (if (null? lst)   ; an empty string is given
        '(*top* #t)
        `(*top* #f ,(sxml:charlst->branch lst)))))

; Adds a new string to string-tree
; In a special case, tree257 may be #f. The function than creates a new tree,
; which contains just the representation for str
(define (sxml:add-string-to-tree str tree)
  (letrec
      ((add-lst-to-tree   ; adds the list of chars to tree
        (lambda (lst tree)
          (if
           (null? lst)  ; the lst is over
           (if
            (cadr tree)  ; whether it is already in the tree
            tree
            (cons (car tree)
                  (cons #t (cddr tree))))
           (let ((curr-char (car lst)))
             (let iter-alist ((alist (cddr tree))
                              (res (list (cadr tree) (car tree))))
               (cond
                 ((null? alist)  ; branch not in a tree
                  (reverse
                   (cons
                    (sxml:charlst->branch lst)
                    res)))
                 ((char=? (caar alist) curr-char)  ; entry found
                  (if
                   (null? (cdr alist))  ; nothing more in the alist
                   (reverse
                    (cons
                     (add-lst-to-tree (cdr lst) (car alist))
                     res))
                   (append
                    (reverse
                     (cons
                      (add-lst-to-tree (cdr lst) (car alist))
                      res))
                    (cdr alist))))
                 ((char>? (caar alist) curr-char)
                  (if
                   (null? (cdr alist))  ; nothing more in the alist
                   (reverse
                    (cons (car alist)
                          (cons (sxml:charlst->branch lst) res)))
                   (append
                    (reverse
                     (cons
                      (sxml:charlst->branch lst)
                      res))
                    alist)))
                 (else
                  (iter-alist (cdr alist)
                              (cons (car alist) res))))))))))
    (add-lst-to-tree (string->list str) tree)))

; Whether a given string is presented in the string-tree
(define (sxml:string-in-tree? str tree)  
  (let loop ((lst (string->list str))
             (tree tree))
    (cond
      ((null? lst)  ; the string is over
       (cadr tree))
      ((assv (car lst) (cddr tree))             
       => (lambda (new-tree)
            (loop (cdr lst) new-tree)))
      (else #f))))
        
; XPath equality comparison for 2 string-sets
;  bool-op - comparison function for 2 boolean values
(define (sxml:radix-sort-join string-set1 string-set2 bool-op)
  (if
   (null? string-set1)  ; always #f
   #f
   (let ((tree
          (let iter-1 ((set1 (cdr string-set1))
                       (tree (sxml:string->tree (car string-set1))))
            (if (null? set1)
                tree
                (iter-1 (cdr set1)
                        (sxml:add-string-to-tree (car set1) tree))))))
     (let iter-2 ((set2 string-set2))
       (cond
         ((null? set2)  ; equality not found
          #f)
         ((bool-op (sxml:string-in-tree? (car set2) tree) #t)
          #t)
         (else
          (iter-2 (cdr set2))))))))

;-------------------------------------------------
; Equality comparison

; A helper for XPath equality operations: = , !=
;  'bool-op', 'number-op' and 'string-op' are comparison operations for 
; a pair of booleans,  numbers and strings respectively
(define (sxml:equality-cmp bool-op number-op string-op)
  (lambda (obj1 obj2)
    (cond
      ((and (not (nodeset? obj1)) (not (nodeset? obj2)))  
       ; neither object is a nodeset
       (cond
         ((boolean? obj1) (bool-op obj1 (sxml:boolean obj2)))
         ((boolean? obj2) (bool-op (sxml:boolean obj1) obj2))
         ((number? obj1) (number-op obj1 (sxml:number obj2)))
         ((number? obj2) (number-op (sxml:number obj1) obj2))
         (else  ; both objects are strings
          (string-op obj1 obj2))))
      ((and (nodeset? obj1) (nodeset? obj2))  ; both objects are nodesets
       (let ((lng1 (length obj1))
             (lng2 (length obj2)))
         (cond
           ((and (< lng1 100000) (< lng2 100000))
            ((if  ; either nodeset is a short one              
              (or (<= lng1 2) (<= lng2 2))
              sxml:nested-loop-join
              sxml:merge-sort-join)
             (map sxml:string-value obj1)
             (map sxml:string-value obj2)
             string-op))
           ((< lng1 lng2)            
            (sxml:radix-sort-join (map sxml:string-value obj1)
                                  (map sxml:string-value obj2)
                                  bool-op))
           (else  ; lng2 < lng1
            (sxml:radix-sort-join (map sxml:string-value obj2)
                                  (map sxml:string-value obj1)
                                  bool-op)))))
      (else  ; one of the objects is a nodeset, another is not
       (call-with-values
        (lambda ()  ; Equality operations are commutative
          (if (nodeset? obj1) (values obj1 obj2) (values obj2 obj1)))
        (lambda (nset elem)
          (cond
            ((boolean? elem) (bool-op elem (sxml:boolean nset)))
            ((number? elem)
             (let loop ((nset 
                         (map
                          (lambda (node) (sxml:number (sxml:string-value node)))
                          nset)))
               (cond
                 ((null? nset) #f)
                 ((number-op elem (car nset)) #t)
                 (else (loop (cdr nset))))))
            ((string? elem)
             (let loop ((nset (map sxml:string-value nset)))
               (cond
                 ((null? nset) #f)
                 ((string-op elem (car nset)) #t)
                 (else (loop (cdr nset))))))
            (else  ; unknown datatype
             (cerr "Unknown datatype: " elem nl)
             #f))))))))

(define sxml:equal? (sxml:equality-cmp eq? = string=?))

(define sxml:not-equal?
  (sxml:equality-cmp
   (lambda (bool1 bool2) (not (eq? bool1 bool2)))
   (lambda (num1 num2) (not (= num1 num2)))
   (lambda (str1 str2) (not (string=? str1 str2)))))

;-------------------------------------------------
; Relational comparison

; Relational operation ( < , > , <= , >= ) for two XPath objects
;  op is comparison procedure: < , > , <= or >=
(define (sxml:relational-cmp op)
  (lambda (obj1 obj2)
    (cond
      ((not (or (nodeset? obj1) (nodeset? obj2)))  ; neither obj is a nodeset
       (op (sxml:number obj1) (sxml:number obj2)))
      ((boolean? obj1)  ; 'obj1' is a boolean, 'obj2' is a nodeset
       (op (sxml:number obj1) (sxml:number (sxml:boolean obj2))))
      ((boolean? obj2)  ; 'obj1' is a nodeset, 'obj2' is a boolean
       (op (sxml:number (sxml:boolean obj1)) (sxml:number obj2)))
      ((or (null? obj1) (null? obj2)) ; one of the objects is an empty nodeset
       #f)
      (else  ; at least one object is a nodeset
       (op
        (cond
          ((nodeset? obj1)  ; 'obj1' is a (non-empty) nodeset
           (let ((nset1 (map
                         (lambda (node) (sxml:number (sxml:string-value node)))
                         obj1)))
             (let first ((num1 (car nset1))
                         (nset1 (cdr nset1)))
               (cond
                 ((null? nset1) num1)
                 ((op num1 (car nset1)) (first num1 (cdr nset1)))
                 (else (first (car nset1) (cdr nset1)))))))
          ((string? obj1) (sxml:number obj1))
          (else  ; 'obj1' is a number
           obj1))
        (cond
          ((nodeset? obj2)  ; 'obj2' is a (non-empty) nodeset
           (let ((nset2 (map
                         (lambda (node) (sxml:number (sxml:string-value node)))
                         obj2)))
             (let second ((num2 (car nset2))
                          (nset2 (cdr nset2)))
               (cond
                 ((null? nset2) num2)
                 ((op num2 (car nset2)) (second (car nset2) (cdr nset2)))
                 (else (second num2 (cdr nset2)))))))
          ((string? obj2) (sxml:number obj2))
          (else  ; 'obj2' is a number
           obj2)))))))
           

;=========================================================================
; XPath axes
; An order in resulting nodeset is preserved

; Ancestor axis
(define (sxml:ancestor test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)      ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:ancestor test-pred?) root-node) node)
	(let rpt ((paths (if (nodeset? root-node)
			   (map list root-node)
			   (list (list root-node)))))
	  (if (null? paths)
	    '()
	    (let ((path (car paths)))
	      (if (eq? (car path) node)
		((sxml:filter test-pred?) (cdr path))
		(rpt (append
		       (map
			 (lambda (arg) (cons arg path))
			 (append 
			   ((sxml:attribute (ntype?? '*)) (car path))
			   ((sxml:child sxml:node?) (car path))))
		       (cdr paths)))))))))))

; Ancestor-or-self axis
(define (sxml:ancestor-or-self test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:ancestor-or-self test-pred?) root-node) node)
	(let rpt ((paths (if (nodeset? root-node)
			   (map list root-node)
			   (list (list root-node)))))
	  (if (null? paths)
	    ((sxml:filter test-pred?) (list node))
	    (let ((path (car paths)))
	      (if (eq? (car path) node)
		((sxml:filter test-pred?) path)
		(rpt (append
		       (map
			 (lambda (arg) (cons arg path))
			 (append 
			   ((sxml:attribute (ntype?? '*)) (car path))
			   ((sxml:child sxml:node?) (car path))))
		       (cdr paths)))))))))))
                                                                      
; Descendant axis
; It's similar to original 'node-closure' a resulting nodeset is 
; in depth-first order rather than breadth-first
; Fix: din't descend in non-element nodes!
(define (sxml:descendant test-pred?)
  (lambda (node)   ; node or nodeset
    (if (nodeset? node)
      (map-union (sxml:descendant test-pred?) node)
      (let rpt ((res '())
		(more ((sxml:child sxml:node?) node)))
	(if (null? more)
	  (reverse res)
	  (rpt (if (test-pred? (car more))
		 (cons (car more) res)
		 res)
	       (append ((sxml:child sxml:node?) (car more))
		       (cdr more))))))))

; Descendant-or-self axis
(define (sxml:descendant-or-self test-pred?)
  (lambda (node)   ; node or nodeset
    (if (nodeset? node)
      (map-union (sxml:descendant-or-self test-pred?) node)
      (let rpt ((res '())
		(more (list node)))
	(if (null? more)
	  (reverse res)
	  (rpt (if (test-pred? (car more))
		 (cons (car more) res)
		 res)
	       (append ((sxml:child sxml:node?) (car more))
		       ; sxml:node?
		       (cdr more))))))))

; Following axis
(define (sxml:following test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)      ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:following test-pred?) root-node) node)
	(let loop ((seq (if (nodeset? root-node)
			  (list root-node)
			  (list (list root-node)))))
	  (cond
	    ((null? seq) '())
	    ((null? (car seq)) (loop (cdr seq)))
	    ((eq? (caar seq) node)
	     (let rpt ((seq (cdr (apply append seq)))
		       (res '()))
	       (if (null? seq)
		 res
		 (rpt (cdr seq)
		      (append 
			res
			((sxml:descendant-or-self test-pred?) (car seq)))))))
	    ((and (sxml:element? (caar seq))
		  (memq node (sxml:attr-list (caar seq))))
	     (let rpt ((sq (cdr (apply append seq)))
		       (res ((sxml:descendant test-pred?) (caar seq))))
	       (if (null? sq)
		 res
		 (rpt (cdr sq)
		      (append res
			      ((sxml:descendant-or-self test-pred?) (car sq)))))))
	    (else
	      (loop (cons 
		      ((sxml:child sxml:node?) (caar seq))
		      (cons (cdar seq) (cdr seq)))))))))))

; Following-sibling axis
(define (sxml:following-sibling test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:following-sibling test-pred?) root-node) node)
	(let loop ((seqs (if (nodeset? root-node)
			   (list root-node)
			   (list (list root-node)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map (sxml:child sxml:node?)
			      (car seqs))
			 (cdr seqs))))
		((eq? (car seq) node) ((sxml:filter test-pred?) (cdr seq)))
		(else (rpt (cdr seq)))))))))))

; Namespace axis
(define (sxml:namespace test-pred?)
  (lambda (node)   ; node or nodeset
    ((sxml:filter test-pred?) 
     (sxml:ns-list node))))

; Preceding axis
(define (sxml:preceding test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:preceding test-pred?) root-node) node)
	(let loop ((seq (if (nodeset? root-node)
			  (list (reverse root-node))
			  (list (list root-node)))))
	  (cond
	    ((null? seq) '())
	    ((null? (car seq)) (loop (cdr seq)))
	    ((or (eq? (caar seq) node)
		 (not (null? ((sxml:attribute 
				(lambda (n)
				  (eq? n node))) 
			      (caar seq)))))
	     (let rpt ((seq (cdr (apply append seq)))
		       (res '()))
	       (if (null? seq)
		 res
		 (rpt (cdr seq)
		      (append res
			      (reverse ((sxml:descendant-or-self test-pred?) 
					(car seq))))))))
	    (else (loop (cons (reverse ((sxml:child sxml:node?) (caar seq)))
			      (cons (cdar seq) (cdr seq)))))))))))

; Preceding-sibling axis
(define (sxml:preceding-sibling test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if(nodeset? node)
	(map-union ((sxml:preceding-sibling test-pred?) root-node) node)
	(let loop ((seqs (if (nodeset? root-node)
			   (list root-node)
			   (list (list root-node)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map
			   (lambda (n)
			     (reverse ((sxml:child sxml:node?) n)))
			   (car seqs))
			 (cdr seqs))))
		((eq? (car seq) node) ((sxml:filter test-pred?) (cdr seq)))
		(else (rpt (cdr seq)))))))))))

;; Classic TXPath implementation based on sxpathlib, sxpath-ext and txp-parser
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; XPointer's points and ranges are NOT implemented
;
; Full XPath Core Function Library is supported. That is:
; 4.1 Node Set Functions
;    number last()
;    number position()
;    number count(node-set)
;    node-set id(object)
;    string local-name(node-set?)
;    string namespace-uri(node-set?)
;    string name(node-set?)
; 4.2 String Functions
;    string string(object?)
;    string concat(string, string, string*)
;    boolean starts-with(string, string)
;    boolean contains(string, string)
;    string substring-before(string, string)
;    string substring-after(string, string)
;    string substring(string, number, number?)
;    number string-length(string?)
;    string normalize-space(string?)
;    string translate(string, string, string)
; 4.3 Boolean Functions
;    boolean boolean(object)
;    boolean not(boolean)
;    boolean true()
;    boolean false()
;    boolean lang(string)
; 4.4 Number Functions
;    number number(object?)
;    number sum(node-set)
;    number floor(number)
;    number ceiling(number)
;    number round(number)


;==========================================================================
; Auxilliary

; Runtime errors handler (unbound variable, bad argument, etc).
; It may be re-defined (say, like a warning) without 'exit',  and evaluation will 
; be continued.
; In this case, a default value (usually empty nodeset or 0) is returned by 
; a sub-expression which caused an XPath/XPointer runtime error.
(define (sxml:xpointer-runtime-error . text)
  (apply cerr (append (list "XPath/XPointer runtime error: ") text (list nl)))
  (exit -1))


;--------------------------------------------------------------------------
; Helper functions

; Filter nodeset using preds-list as described in XPath rec. 2.4
; A helper for sxml:parse-step and sxml:parse-filter-expr
(define (sxml:xpath-nodeset-filter preds-list nodeset root-node var-binding)
  (let rpt ((nodeset nodeset)
	    (ps preds-list))
    (if (null? ps) 
      nodeset
      (let lab ((nset nodeset)
		(res '())
		(pos 1)) 
	(if (null? nset)
	  (rpt (reverse res) (cdr ps))
	  (let* ((size (length nodeset))
		 (val ((car ps) 
		       (list (car nset)) 
		       root-node 
		       (cons pos size) 
		       var-binding)))
	    (lab (cdr nset)
		 (if (if (number? val)
		       (= val pos)
		       (sxml:boolean val))
		   (cons (car nset) res)
		   res)
		 (+ pos 1))))))))


; A helper for arithmetic expressions
;   sxml:parse-additive-expr and sxml:parse-multiplicative-expr 
(define (sxml:arithmetic-eval unary-expr-res-lst op-lst add-on)
  (lambda (nodeset root-node context var-binding)
    (let rpt
      ((res (sxml:number
             ((car unary-expr-res-lst) nodeset root-node context var-binding)))
       (fs (cdr unary-expr-res-lst))
       (ops op-lst))
    (if (null? fs)
        res
        (rpt ((car ops)
              res
              (sxml:number ((car fs) nodeset root-node context var-binding)))
             (cdr fs)
             (cdr ops))))))


;==========================================================================
; XPath Core Function Library

;-------------------------------------------------
; 4.1 Node Set Functions

; last()
(define (sxml:core-last)
  (lambda (nodeset root-node context var-binding)
    (cdr context)))

; position()
(define (sxml:core-position)
  (lambda (nodeset root-node context var-binding)
    (car context)))

; count(node-set)
(define (sxml:core-count arg-func)
  (lambda (nodeset root-node context var-binding)
    (let ((res (arg-func nodeset root-node context var-binding)))
      (cond
        ((nodeset? res) (length res))
        (else
         (sxml:xpointer-runtime-error
          "count() function - an argument is not a nodeset")
         0)))))

; id(object)
(define (sxml:core-id arg-func)
  (lambda (nodeset root-node context var-binding)
    (let* ((id-nset ((sxml:child (ntype?? 'id-index))
                     ((sxml:child (ntype?? '@@)) root-node))))
      (if
       (null? id-nset)  ; no id-index
       '()  ; ID function returns an empty nodeset
       ((sxml:id (cdar id-nset))  ; implemented in "sxpath-ext.scm"
        (arg-func nodeset root-node context var-binding))))))

; local-name(node-set?)
(define (sxml:core-local-name . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (cond
          ((null? nodeset) "")
          ((not (pair? (car nodeset))) "")  ; no name
          (else
           (let ((name (symbol->string (caar nodeset))))
             (cond
               ((string-rindex name #\:)
                => (lambda (pos)
                     (substring name (+ pos 1) (string-length name))))
               (else  ; a NCName
                name))))))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let ((obj (func nodeset root-node context var-binding)))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")              
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring
                          name (+ pos 1) (string-length name))))
                   (else  ; a NCName
                    name))))))))))

; namespace-uri(node-set?)
(define (sxml:core-namespace-uri . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (cond
          ((null? nodeset) "")
          ((not (pair? (car nodeset))) "")  ; no name
          (else
           (let ((name (symbol->string (caar nodeset))))
             (cond
               ((string-rindex name #\:)
                => (lambda (pos)
                     (substring name 0 pos)))
               (else ""))))))  ; a NCName
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let ((obj (func nodeset root-node context var-binding)))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring name 0 pos)))
                   (else ""))))))))))

; name(node-set?)
(define (sxml:core-name . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (cond
          ((null? nodeset) "")
          ((not (pair? (car nodeset))) "")  ; no name
          (else
           (symbol->string (caar nodeset)))))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let ((obj (func nodeset root-node context var-binding)))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (symbol->string (caar obj)))))))))


;-------------------------------------------------
; 4.2 String Functions

; string(object?)
(define (sxml:core-string . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (sxml:string nodeset))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (sxml:string 
           (func nodeset root-node context var-binding))))))

; concat(string, string, string*)
(define (sxml:core-concat . arg-func-lst)
  (lambda (nodeset root-node context var-binding)
    (apply
     string-append
     (map
      (lambda (f)
        (sxml:string (f nodeset root-node context var-binding)))
      arg-func-lst))))

; starts-with(string, string)
(define (sxml:core-starts-with arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let ((str1 (sxml:string
                 (arg-func1 nodeset root-node context var-binding)))
          (str2 (sxml:string
                 (arg-func2 nodeset root-node context var-binding))))
      (string-prefix? str2 str1))))

; contains(string, string)
(define (sxml:core-contains arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let ((str1 (sxml:string
                 (arg-func1 nodeset root-node context var-binding)))
          (str2 (sxml:string
                 (arg-func2 nodeset root-node context var-binding))))
      (if (substring? str2 str1) #t #f)  ; must return a boolean
      )))
  
; substring-before(string, string)
(define (sxml:core-substring-before arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let* ((str1 (sxml:string
                  (arg-func1 nodeset root-node context var-binding)))
           (str2 (sxml:string
                  (arg-func2 nodeset root-node context var-binding)))
           (pos (substring? str2 str1)))
      (if (not pos)  ; STR1 doesn't contain STR2
          ""
          (substring str1 0 pos)))))

; substring-after(string, string)
(define (sxml:core-substring-after arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let* ((str1 (sxml:string
                  (arg-func1 nodeset root-node context var-binding)))
           (str2 (sxml:string
                  (arg-func2 nodeset root-node context var-binding)))
           (pos (substring? str2 str1)))
      (if
       (not pos)  ; STR1 doesn't contain STR2
       ""
       (substring
        str1 (+ pos (string-length str2)) (string-length str1))))))

; substring(string, number, number?)
(define (sxml:core-substring arg-func1 arg-func2 . arg-func3)
  (if (null? arg-func3)  ; no third argument supplied
      (lambda (nodeset root-node context var-binding)
        (let ((str (sxml:string
                    (arg-func1 nodeset root-node context var-binding)))
              (num1 (sxml:number
                     (arg-func2 nodeset root-node context var-binding))))
          (let ((len (string-length str))
                (start (- (inexact->exact (round num1)) 1)))
            (if (> start len)
                ""
                (substring str (if (< start 0) 0 start) len)))))
      (let ((arg-func3 (car arg-func3)))
        (lambda (nodeset root-node context var-binding)
          (let ((str (sxml:string
                      (arg-func1 nodeset root-node context var-binding)))
                (num1 (sxml:number
                       (arg-func2 nodeset root-node context var-binding)))
                (num2 (sxml:number
                       (arg-func3 nodeset root-node context var-binding))))
            (let* ((len (string-length str))
                   (start (- (inexact->exact (round num1)) 1))
                   (fin (+ start (inexact->exact (round num2)))))
              (if (or (> start len) (< fin 0) (< fin start))
                  ""
                  (substring str
                             (if (< start 0) 0 start)
                             (if (> fin len) len fin)))))))))

; string-length(string?)
(define (sxml:core-string-length . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (string-length (sxml:string nodeset)))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (string-length
           (sxml:string
            (func nodeset root-node context var-binding)))))))

; normalize-space(string?)
(define (sxml:core-normalize-space . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (let rpt ((src (string-split (sxml:string nodeset) sxml:whitespace))
                  (res '()))
          (cond
            ((null? src)
             (apply string-append (reverse res)))
            ((= (string-length (car src)) 0)  ; empty string
             (rpt (cdr src) res))
            ((null? res)
             (rpt (cdr src) (cons (car src) res)))
            (else
             (rpt (cdr src) (cons (car src) (cons " " res)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let rpt ((src (string-split
                          (sxml:string
                           (func nodeset root-node context var-binding))
                          sxml:whitespace))
                    (res '()))
            (cond
              ((null? src)
               (apply string-append (reverse res)))
              ((= (string-length (car src)) 0)  ; empty string
               (rpt (cdr src) res))
              ((null? res)
               (rpt (cdr src) (cons (car src) res)))
              (else
               (rpt (cdr src) (cons (car src) (cons " " res))))))))))

; translate(string, string, string)
(define (sxml:core-translate arg-func1 arg-func2 arg-func3)
  (lambda (nodeset root-node context var-binding)
    (let ((str1 (sxml:string
                 (arg-func1 nodeset root-node context var-binding)))
          (str2 (sxml:string
                 (arg-func2 nodeset root-node context var-binding)))
          (str3 (sxml:string
                 (arg-func3 nodeset root-node context var-binding))))
      (let ((alist
             (let while ((lst2 (string->list str2))
                         (lst3 (string->list str3))
                         (alist '()))
               (cond
                 ((null? lst2) (reverse alist))
                 ((null? lst3)
                  (append
                   (reverse alist)
                   (map
                    (lambda (ch) (cons ch #f))
                    lst2)))
                 (else
                  (while
                   (cdr lst2)
                   (cdr lst3)
                   (cons (cons (car lst2) (car lst3)) alist)))))))
        (let rpt ((lst1 (string->list str1))
                  (res '()))
          (cond
            ((null? lst1) (list->string (reverse res)))
            ((assoc (car lst1) alist)
             => (lambda (pair)
                  (if (cdr pair)
                      (rpt (cdr lst1) (cons (cdr pair) res))
                      (rpt (cdr lst1) res))))
            (else
             (rpt (cdr lst1) (cons (car lst1) res)))))))))
  

;-------------------------------------------------
; 4.3 Boolean Functions

; boolean(object)
(define (sxml:core-boolean arg-func)
  (lambda (nodeset root-node context var-binding)
    (sxml:boolean 
     (arg-func nodeset root-node context var-binding))))

; not(boolean)
(define (sxml:core-not arg-func)
  (lambda (nodeset root-node context var-binding)
    (not (sxml:boolean 
          (arg-func nodeset root-node context var-binding)))))

; true()
(define (sxml:core-true)
  (lambda (nodeset root-node context var-binding) #t))

; false()
(define (sxml:core-false)
  (lambda (nodeset root-node context var-binding) #f))

; lang(string)
(define (sxml:core-lang arg-func)
  (lambda (nodeset root-node context var-binding)
    (if
     (null? nodeset)
     #f
     (let ((arg (sxml:string
                 (arg-func nodeset root-node context var-binding)))
           (context-node (car nodeset)))
       (let rpt ((pairs
                  (map
                   (lambda (node) (cons node #f))
                   root-node)))
         (if
          (null? pairs)  ; context node not found
          #f
          (let* ((lng
                  ((sxml:child (ntype?? '*text*))
                   ((sxml:attribute (ntype?? 'xml:lang))
                    (caar pairs))))
                 (lng (if (null? lng) (cdar pairs) (car lng))))
            (if
             (eq? context-node (caar pairs)) ; context node found
             (and
              lng
              (or (string-ci=? arg lng)
                  (string-prefix-ci? (string-append arg "-") lng)))
             (rpt
              (append
               (map
                (lambda (node) (cons node lng))
                ((sxml:attribute (ntype?? '*)) (caar pairs)))
               (map
                (lambda (node) (cons node lng))
                ((sxml:child sxml:node?) (caar pairs)))
               (cdr pairs)))))))))))
  

;-------------------------------------------------
; 4.4 Number Functions

; number(object?)
(define (sxml:core-number . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (sxml:number nodeset))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (sxml:number 
           (func nodeset root-node context var-binding))))))

; sum(node-set)
(define (sxml:core-sum arg-func)
  (lambda (nodeset root-node context var-binding)
    (let ((res (arg-func nodeset root-node context var-binding)))
      (cond
        ((nodeset? res)
         (apply +
                (map
                 (lambda (node)
                   (sxml:number (sxml:string-value node)))
                 res)))
        (else
         (sxml:xpointer-runtime-error
          "SUM function - an argument is not a nodeset")
         0)))))

; floor(number)
(define (sxml:core-floor arg-func)
  (lambda (nodeset root-node context var-binding)
    (inexact->exact
     (floor (sxml:number 
             (arg-func nodeset root-node context var-binding))))))

; ceiling(number)
(define (sxml:core-ceiling arg-func)
  (lambda (nodeset root-node context var-binding)
    (inexact->exact
     (ceiling (sxml:number
               (arg-func nodeset root-node context var-binding))))))

; round(number)
(define (sxml:core-round arg-func)
  (lambda (nodeset root-node context var-binding)
    (inexact->exact
     (round (sxml:number
             (arg-func nodeset root-node context var-binding))))))



;==========================================================================
; Parameters for classic TXPath implementation

(define sxml:classic-params
  `(
    ; For XPath axes, the result is returned in the form of the pair
    ; (cons  (lambda ...)  root-node-required)  
    ;  (lambda ...) - one of the axis functions
    ;  root-node-required - a boolean value
    ; If root-node-required = #t, lambda's signature is
    ;  (lambda (test-pred?)
    ;   (lambda (root-node)
    ;    (lambda (nodeset) ... )))
    ; otherwise
    ;  (lambda (test-pred?)
    ;   (lambda (nodeset) ... ))
    (axis
     ((ancestor
       ,(lambda (add-on) (cons sxml:ancestor #t)))
      (ancestor-or-self
       ,(lambda (add-on) (cons sxml:ancestor-or-self #t)))
      (attribute
       ,(lambda (add-on) (cons sxml:attribute #f)))
      (child
       ,(lambda (add-on) (cons sxml:child #f)))
      (descendant
       ,(lambda (add-on) (cons sxml:descendant #f)))
      (descendant-or-self
       ,(lambda (add-on) (cons sxml:descendant-or-self #f)))
      (following
       ,(lambda (add-on) (cons sxml:following #t)))
      (following-sibling
       ,(lambda (add-on) (cons sxml:following-sibling #t)))
      (namespace
       ,(lambda (add-on) (cons sxml:namespace #f)))
      (parent
       ,(lambda (add-on) (cons sxml:parent #t)))
      (preceding
       ,(lambda (add-on) (cons sxml:preceding #t)))
      (preceding-sibling
       ,(lambda (add-on) (cons sxml:preceding-sibling #t)))
      (self
       ,(lambda (add-on) (cons sxml:filter #f)))))
    
    ; For NodeTests, the result is
    ;  (lambda (node) ...)  - a node test function
    ; or 'txp:semantic-error (namely, for point and range)
    (node-test
     ((star
       ,(lambda (add-on) (ntype?? '*)))
      (uri+star
       ,(lambda (uri add-on) (ntype-namespace-id?? uri)))
      (qname
       ,(lambda (uri local-name add-on)
          (if (not uri)
              (ntype?? (string->symbol local-name))
              (ntype?? (string->symbol (string-append uri ":" local-name))))))
      (comment
       ,(lambda (add-on) (ntype?? '*COMMENT*)))
      (text
       ,(lambda (add-on) (ntype?? '*text*)))
      (processing-instruction
       ,(lambda (literal-string add-on)
          (if (not literal-string)  ; no literal provided
              (lambda (node)
                (and (pair? node) (eq? (car node) '*PI*)))
              (let ((literal (string->symbol literal-string)))
                (lambda (node)
                  (and (pair? node)
                       (eq? (car node) '*PI*)
                       (equal? (cadr node) literal)))))))
      (node
       ,(lambda (add-on) sxml:node?))
      (point
       ,(lambda (add-on)
          (txp:signal-semantic-error
           "point() NodeTest is not supported by this implementation")))
      (range
       ,(lambda (add-on)
          (txp:signal-semantic-error
           "range() NodeTest is not supported by this implementation")))))
    
    ;-------------
    ; The remaining parameter values return the following
    ; (lambda (nodeset root-node context var-binding) - an SXPath-like
    ; function (it transforms a nodeset into a new nodeset)
    ;  nodeset - a current set of nodes
    ;  root-node - the root of a document (a singleton nodeset)
    ;  context - the context of the node; list of two elements - (position size)
    ;  position - context position (a number)
    ;  size - context size (a number)
    
    ; Parse step implementation
    (step
     ((common
       ,(lambda (axis-res node-test-res predicate-res-lst add-on)
          (let ((axis (car axis-res))
                (root-node-required (cdr axis-res)))
            (if
             (null? predicate-res-lst)
             (lambda (nodeset root-node context var-binding)
               (if root-node-required
                   (((axis node-test-res) root-node) nodeset)
                   ((axis node-test-res) nodeset)))
             (lambda (nodeset root-node context var-binding)
               (map-union
                (lambda (node)
                  (sxml:xpath-nodeset-filter 
                   predicate-res-lst
                   ((if root-node-required
                        ((axis node-test-res) root-node)
                        (axis node-test-res))
                    node)
                   root-node var-binding))
                nodeset))))))
      (range-to
       ,(lambda (expr-res predicate-res-lst add-on)
          (txp:signal-semantic-error "range-to function not implemented")))))
    
    ; Relative location path implementation
    (relative-lpath
     ,(lambda (step-res-lst add-on)
        (if
         (null? (cdr step-res-lst))  ; the only step
         (car step-res-lst)
         (lambda (nodeset root-node context var-binding)
           (let rpt ((nset nodeset)
                     (fs step-res-lst))
             (if (null? fs)
                 nset
                 (rpt ((car fs) nset root-node context var-binding)
                      (cdr fs))))))))
    
    ; Location path implementation
    (location-path
     ((bare-slash
       ,(lambda (add-on)
          (lambda (nodeset root-node context var-binding) root-node)))       
      (slash
       ,(lambda (relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (relative-lpath-res root-node root-node context var-binding))))
      (double-slash
       ,(lambda (relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (relative-lpath-res
             ((sxml:descendant-or-self sxml:node?) root-node)
             root-node context var-binding))))))
    
    ; Predicate implementation
    ; Note that (according to specification) a Predicate must return a number
    ; or a boolean value. However, the return value type is not checked in this
    ; function. This is performed in functions that use 'parse-predicate'
    (predicate
     ,(lambda (expr-res add-on) expr-res))  ; similar to identity function
    
    ; Variable reference implementation
    (variable-ref
     ,(lambda (var-name-string add-on)
        (let ((name (string->symbol var-name-string)))
          (lambda (nodeset root-node context var-binding)
            (cond
              ((assoc name var-binding)
               => cdr)
              (else
               (sxml:xpointer-runtime-error "unbound variable - " name)
               '()))))))
    
    ; Function call implementation
    (function-call
     ,(lambda (fun-name-string arg-res-lst add-on)
        (let ((core-alist
               ; (list fun-name min-num-args max-num-args impl)
               `((last 0 0 ,sxml:core-last)
                 (position 0 0 ,sxml:core-position)
                 (count 1 1 ,sxml:core-count)
                 (id 1 1 ,sxml:core-id)
                 (local-name 0 1 ,sxml:core-local-name)
                 (namespace-uri 0 1 ,sxml:core-namespace-uri)
                 (name 0 1 ,sxml:core-name)
                 (string 0 1 ,sxml:core-string)
                 (concat 2 -1 ,sxml:core-concat)
                 (starts-with 2 2 ,sxml:core-starts-with)
                 (contains 2 2 ,sxml:core-contains)
                 (substring-before 2 2 ,sxml:core-substring-before)
                 (substring-after 2 2 ,sxml:core-substring-after)
                 (substring 2 3 ,sxml:core-substring)
                 (string-length 0 1 ,sxml:core-string-length)
                 (normalize-space 0 1 ,sxml:core-normalize-space)
                 (translate 3 3 ,sxml:core-translate)
                 (boolean 1 1 ,sxml:core-boolean)
                 (not 1 1 ,sxml:core-not)
                 (true 0 0 ,sxml:core-true)
                 (false 0 0 ,sxml:core-false)
                 (lang 1 1 ,sxml:core-lang)
                 (number 0 1 ,sxml:core-number)
                 (sum 1 1 ,sxml:core-sum)
                 (floor 1 1 ,sxml:core-floor)
                 (ceiling 1 1 ,sxml:core-ceiling)
                 (round 1 1 ,sxml:core-round))))
          (cond
           ((assq (string->symbol fun-name-string) core-alist)
            => (lambda (quad)  ; Core function found
                 (cond
                   ((< (length arg-res-lst) (cadr quad))
                    (txp:signal-semantic-error
                     "too few arguments for the Core Function call - "
                     fun-name-string))
                   ((and (> (caddr quad) 0)
                         (> (length arg-res-lst) (caddr quad)))
                    (txp:signal-semantic-error
                     "too many arguments for the Core Function call - "
                     fun-name-string))
                   (else  ; correct number of arguments
                    ; Producing a function implementation
                    (apply (cadddr quad) arg-res-lst)))))
           (else  ; function definition not found
            (txp:signal-semantic-error
             "function call to an unknown function - " fun-name-string))))))
    
    ; Primary expression
    (primary-expr
     ((literal
       ,(lambda (literal add-on)
          (lambda (nodeset root-node context var-binding) literal)))
      (number
       ,(lambda (number add-on)
          (lambda (nodeset root-node context var-binding) number)))))

    ; Filter expression
    (filter-expr
     ,(lambda (primary-expr-res predicate-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let ((nodeset
                 (primary-expr-res nodeset root-node context var-binding)))
            (sxml:xpath-nodeset-filter
             predicate-res-lst
             (cond
               ((nodeset? nodeset) nodeset)
               (else 
                (sxml:xpointer-runtime-error 
                 "expected - nodeset instead of " nodeset)
                '()))
             root-node var-binding)))))
    
    ; Path expression
    (path-expr
     ((slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (let ((nset
                   (filter-expr-res nodeset root-node context var-binding)))
              (let ((nset 
                     (cond
                       ((nodeset? nset) nset)
                       (else 
                        (sxml:xpointer-runtime-error 
                         "expected - nodeset instead of " nset)
                        '()))))
                (relative-lpath-res nset root-node context var-binding))))))
      (double-slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (let ((nset
                   (filter-expr-res nodeset root-node context var-binding)))
              (let ((nset 
                     (cond
                       ((nodeset? nset) nset)
                       (else 
                        (sxml:xpointer-runtime-error 
                         "expected - nodeset instead of " nset)
                        '()))))
                (let ((nset ((sxml:descendant-or-self sxml:node?) nset)))
                  (relative-lpath-res
                   nset root-node context var-binding)))))))))
    
    ; Union expression
    (union-expr
     ,(lambda (path-expr-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((res '())
                    (fs path-expr-res-lst))
            (if
             (null? fs)
             res
             (let ((nset ((car fs) nodeset root-node context var-binding)))
               (rpt
                (append 
                 res
                 (cond
                   ((not (nodeset? nset))
                    (sxml:xpointer-runtime-error 
                     "expected - nodeset instead of " nset)
                    '())
                   (else nset)))
                (cdr fs))))))))
    
    ; Unary expression
    (unary-expr
     ,(lambda (union-expr-res num-minuses add-on)
        (if (even? num-minuses)
            (lambda (nodeset root-node context var-binding)
              (sxml:number
               (union-expr-res nodeset root-node context var-binding)))
            (lambda (nodeset root-node context var-binding)
              (- (sxml:number
                  (union-expr-res nodeset root-node context var-binding)))))))
    
    ; Different operations
    (operations
     ((* ,(lambda (add-on) *))
      (div ,(lambda (add-on) /))
      (mod ,(lambda (add-on) remainder))
      (+ ,(lambda (add-on) +))
      (- ,(lambda (add-on) -))
      (< ,(lambda (add-on) (sxml:relational-cmp <)))
      (> ,(lambda (add-on) (sxml:relational-cmp >)))
      (<= ,(lambda (add-on) (sxml:relational-cmp <=)))
      (>= ,(lambda (add-on) (sxml:relational-cmp >=)))
      (= ,(lambda (add-on) sxml:equal?))
      (!= ,(lambda (add-on) sxml:not-equal?))))
    
    ; Additive and multiplicative expressions
    (mul-expr ,sxml:arithmetic-eval)
    (add-expr ,sxml:arithmetic-eval)
    
    ; Relational expression
    (relational-expr
     ,(lambda (additive-expr-res-lst cmp-op-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((res ((car additive-expr-res-lst)
                          nodeset root-node context var-binding))
                    (fs (cdr additive-expr-res-lst))
                    (ops cmp-op-lst))
            (if (null? fs)
                res
                (rpt ((car ops)
                      res
                      ((car fs) nodeset root-node context var-binding))
                     (cdr fs)
                     (cdr ops)))))))        
    
    ; Equality expression
    (equality-expr
     ,(lambda (relational-expr-res-lst cmp-op-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((res ((car relational-expr-res-lst)
                          nodeset root-node context var-binding))
                    (fs (cdr relational-expr-res-lst))
                    (ops cmp-op-lst))
            (if (null? fs)
                res
                (rpt ((car ops) 
                      res 
                      ((car fs) nodeset root-node context var-binding))
                     (cdr fs)
                     (cdr ops)))))))
    
    ; And-expression
    ; Note that according to 3.4 in XPath specification, the right operand
    ; is not evaluated if the left operand evaluates to false
    (and-expr
     ,(lambda (equality-expr-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((fs equality-expr-res-lst))
            (cond
              ((null? fs) #t)
              ((not (sxml:boolean
                     ((car fs) nodeset root-node context var-binding))) #f)
              (else (rpt (cdr fs))))))))
    
    ; Or-expression
    (or-expr
     ,(lambda (and-expr-res-lst add-on)    
        (lambda (nodeset root-node context var-binding)
          (let rpt ((fs and-expr-res-lst))
            (cond
              ((null? fs) #f)
              ((sxml:boolean
                ((car fs) nodeset root-node context var-binding)) #t)
              (else (rpt (cdr fs))))))))
    
    ; Full XPointer
    (full-xptr
     ,(lambda (expr-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((fs expr-res-lst))
            (if (null? fs)
                '()
                (let ((nset ((car fs) nodeset root-node context var-binding)))
                  (if (null? nset)
                      (rpt (cdr fs))
                      nset)))))))
    
    ; XPointer child sequence
    (child-seq
     ((with-name
      ,(lambda (name-string number-lst add-on)
         (let ((funcs
                 (apply append
                        (map
                         (lambda (num)
                           (list (sxml:child (ntype?? '*)) (node-pos num)))
                         number-lst))))
           (lambda (nodeset root-node context var-binding)
             (let ((id-nset ((sxml:child (ntype?? 'id-index))
                             ((sxml:child (ntype?? '@@)) root-node))))
               (if
                (null? id-nset)  ; no id-index
                '()
                (let ((nd (sxml:lookup name-string (cdar id-nset))))
                  (if (not nd)
                      '()
                      (let rpt ((nset (list nd))
                                (fs funcs))
                        (if (null? fs)
                            nset
                            (rpt ((car fs) nset) (cdr fs))))))))))))
      (without-name
       ,(lambda (number-lst add-on)
          (let ((funcs
                 (apply append
                        (map
                         (lambda (num)
                           (list (sxml:child (ntype?? '*)) (node-pos num)))
                         number-lst))))
            (lambda (nodeset root-node context var-binding)
              (if (nodeset? nodeset)
                  (let rpt ((nodeset nodeset) (res '()))
                    (if (null? nodeset)
                        res
                        (let rpt2 ((nset (list (car nodeset))) 
                                   (fs funcs))
                          (if (null? fs)
                              (rpt (cdr nodeset) (append res nset))
                              (rpt2 ((car fs) nset) (cdr fs))))))
                  (let rpt ((nodeset nodeset) (fs funcs))
                    (if (null? fs)
                        nodeset
                        (rpt ((car fs) nodeset) (cdr fs)))))))))))                
    ))
     
;=========================================================================
; Highest level API functions

;------------------------------------------------
; 'sxml:xpath' and 'sxml:xpointer' functions
;
;  xpath-string - an XPath location path (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   (lambda (node . var-binding) ...)  
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)
;  (lambda (node . var-binding) ...)  - an SXPath function
;  node - a node (or a node-set) of the SXML document
;  var-binding - XPath variable bindings (an optional argument)
;  var-binding = (list  (var-name . value)
;                       (var-name . value)
;                       ...)
;  var-name - (a symbol) a name of a variable
;  value - its value. The value can have the following type: boolean, number,
; string, nodeset. NOTE: a node must be represented as a singleton nodeset
; 
; Administrative SXPath variables:
;  *root* - if presented in the 'var-binding', its value (a node or a nodeset)
; specifies the root of the SXML document

(define (sxml:api-helper0 parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f
          (lambda (node . var-binding)
            (let ((node (as-nodeset node)))
              (if
               (null? var-binding)  ; no variables supplied
               (res node node (cons 1 1) '())
               (let ((var-binding (car var-binding)))
                 (res
                  node
                  (cond ((assq '*root* var-binding)
                         => (lambda (pair) (as-nodeset (cdr pair))))
                        (else node))
                  (cons 1 1)
                  var-binding)))))))))

(define sxml:classic-res (txp:parameterize-parser sxml:classic-params))

(define (sxml:api-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f
          (lambda (node . var-binding)
            (let ((node (as-nodeset node)))
              (if
               (null? var-binding)  ; no variables supplied
               (res node node (cons 1 1) '())
               (let ((var-binding (car var-binding)))
                 (res
                  node
                  (cond ((assq '*root* var-binding)
                         => (lambda (pair) (as-nodeset (cdr pair))))
                        (else node))
                  (cons 1 1)
                  var-binding)))))))))
              
(define sxml:xpath
  (sxml:api-helper (cadr (assq 'xpath sxml:classic-res))))
(define sxml:xpointer
  (sxml:api-helper (cadr (assq 'xpointer sxml:classic-res))))
(define sxml:xpath-expr
  (sxml:api-helper (cadr (assq 'expr sxml:classic-res))))

; Some (deprecated!) aliases for backward compatibility
; which will be eventually removed
(define sxml:xpath+root+vars sxml:xpath)
(define sxml:xpointer+root+vars sxml:xpointer)
(define sxml:xpath+root sxml:xpath)
(define txpath sxml:xpath)


;------------------------------------------------
; 'sxml:xpath+index' and 'sxml:xpointer+index' functions
;
; NOTE: THESE FUNCTIONS ARE JUST STUBS NOW, BECAUSE THEY ALWAYS RETURN #t
; FOR 'index-required'. THESE FUNCTIONS ARE INCLUDED HERE FOR THE SAKE OF
; BACKWARD COMPATIBILITY ONLY.
;
;  xpath-string - an XPath location path (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   (cons (lambda (node . id-index) ...)  
;                              index-required )
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)
;  (lambda (node) ...)  - an SXPath function
;  node - a root node of the SXML document
;  index-required - a boolean value: whether an id-index is required

(define (sxml:api-index-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f
          (cons
           (lambda (node)
             (let ((node (as-nodeset node)))
               (res node node (cons 1 1) '())))
           #t)))))
     
(define sxml:xpath+index
  (sxml:api-index-helper (cadr (assq 'xpath sxml:classic-res))))
(define sxml:xpointer+index
  (sxml:api-index-helper (cadr (assq 'xpointer sxml:classic-res))))


)