;; -*- mode: scheme; coding: utf-8 -*-
#!nobacktrace
(library (tests text sxml sxpath)
    (export run-sxpath-test)
    (import (except (rnrs) display newline write)
	    (rename (only (rnrs) display newline write)
		    (display r6rs:display)
		    (newline r6rs:newline)
		    (write r6rs:write))
	    (text sxml sxpath)
	    (except (text sxml helper) cout cerr)
	    (text parse)
	    (core misc)
	    (sagittarius)
	    (sagittarius io)
	    (util list)
	    (prefix (pp) pp:)
	    (srfi :6)
	    (srfi :13)
	    (srfi :64))

(define (list-intersperse lst item)
  (intersperse item lst))

;; it'll be closed when testing finished. bit awkward.
(define *log-port*
  (open-file-output-port "sxpath-test-result.log" (file-options no-fail) 'block (native-transcoder)))

(define (display o) (r6rs:display o *log-port*))
(define (newline)   (r6rs:newline *log-port*))
(define (write o)   (r6rs:write o *log-port*))

(define (cout . args)
  (for-each (lambda (x)
	      (if (procedure? x) (x) (display x)))
	    args)
  (newline))
(define cerr cout)

(define (pp o) (pp:pp o *log-port*))

(define-syntax xtest-assert
  (syntax-rules ()
    ((_ expected selector params ...)
     (test-equal (list params ...) expected
		 (selector params ...)))))

(define-macro (sxp:run-test selector node expected-result)
  (let ((res (gensym)))
    `(let ((,res (,selector ,node)))
       (test-equal ',selector ,expected-result ,res))))

#;(define-syntax sxp:run-test
  (syntax-rules ()
    ((_ selector node expected)
     (test-equal 'selector
		 expected
		 (selector node)))))

;; Validation tests for "sxpathlib.scm"
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

(define vsxplb:tree1 
  '(html
    (head (title "Slides"))
    (body
     (p (@ (align "center"))
	(table (@ (style "font-size: x-large"))
	       (tr
		(td (@ (align "right")) "Talks ")
		(td (@ (align "center")) " = ")
		(td " slides + transition"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " data + control"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " programs"))))
     (ul
      (li (a (@ (href "slides/slide0001.gif")) "Introduction"))
      (li (a (@ (href "slides/slide0010.gif")) "Summary")))
     )))


; Example from a posting "Re: DrScheme and XML", 
; Shriram Krishnamurthi, comp.lang.scheme, Nov. 26. 1999.
; http://www.deja.com/getdoc.xp?AN=553507805
(define vsxplb:tree3
  '(poem (@ (title "The Lovesong of J. Alfred Prufrock")
	    (poet "T. S. Eliot"))
	 (stanza
	  (line "Let us go then, you and I,")
	  (line "When the evening is spread out against the sky")
	  (line "Like a patient etherized upon a table:"))
	 (stanza
	  (line "In the room the women come and go")
	  (line "Talking of Michaelangelo."))))

(define (test-sxpath path)
  (let ((func (sxpath path)))
    (if func
     (lambda (node) (func node))
     (lambda (node) '@error@))))  ; this can never be an expected result

;; Validation tests for "sxpath-ext.scm"
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;@ test-document
(define xt-doc
'(*TOP*
 (*PI* xml "version='1.0'")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com")))))))
)

 ; Some particular nodes from a document above

;@node '(chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
(define node1
  (car ((sxpath '(doc body (chapter 3))) xt-doc xt-doc)))

;@ node: '(title "XML Pointing Language (XPointer)")
(define node2
  (car ((sxpath '(doc body appendix bibliographic (item 2) title)) xt-doc xt-doc)))

;@ node: '(id "toc3")
(define node3
  (car ((sxpath '(doc (item 3) @ id)) xt-doc xt-doc)))

;; Validation tests for SXPath textual syntax: "txpath.scm" 
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;@ Document
(define vtxp:old-doc
'(*TOP*
 (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com")))))))
)


;------------------------------------------------
; The second step - creating an 'id-index' using 'id-attrs'

; This function forms an 'id-index' and inserts it in the document
;  document - a root node of the document (SXML presentation)
;  id-attrs - the result of the previous step
; A new SXML document is returned. It contains an auxiliary list with an
; 'id-index subtree. If the source document already contains such a
; subtree, it will be replaced. Other subtrees in an auxiliary list will
; remain unchanged.
; DL: local version of the function - prefixed with `vtxp:'
(define (vtxp:SXML->SXML+id document id-attrs)
  (let((aux-subtrees
        (let((aux ((select-kids (ntype?? '@@)) document)))
          (if(null? aux)
             '()
             (let rpt ((res '())
                       (to-see (cdar aux)))
               (cond
                 ((null? to-see) (reverse res))
                 ((equal? (caar to-see) 'id-index) (rpt res (cdr to-see)))
                 (else (rpt (cons (car to-see) res)
                            (cdr to-see)))))))))
    (let loop ((nodeset (list document))
               (id-index '()))
      (if(null? nodeset)
         (let((kids ((select-kids
                      (lambda (node)
                        (not (and (pair? node) (equal? (car node) '@@)))))
                     document)))
           (cons '*TOP*
                 (cons (cons '@@
                             (cons (cons 'id-index id-index)
                                   aux-subtrees))
                  kids)))
         (let((cur-node (car nodeset)))
           (cond
             ((not (pair? cur-node))  ; a text node
              (loop (cdr nodeset) id-index))
             ((assoc (car cur-node) id-attrs)
              =>
              (lambda (lst)
                (let((id-values
                      ((select-kids (lambda (x) #t))
                       ((sxml:filter (lambda (x) (member (car x) (cdr lst))))
                        ((select-kids (lambda (x) #t))
                         ((select-kids (ntype?? '@)) cur-node))))))
                  (loop
                   (append 
                    ((select-kids (ntype?? '*)) (car nodeset))
                    (cdr nodeset))
                   (append
                    id-index
                    (map
                     (lambda (x) (cons x cur-node))
                     id-values))))))
             (else
              (loop
               (append ((select-kids (ntype?? '*)) (car nodeset)) (cdr nodeset))
               id-index))))))))

(define vtxp:doc
  (vtxp:SXML->SXML+id
   vtxp:old-doc
   '((item id) (chapter id) (section ID) (appendix id))))

;@ Namespace binding
(define vtxp:ns-binding (list (cons 'xlink "http://www.w3.org/1999/xlink")))


;=========================================================================
; Test functions

(define (sxml:test-xpath+index xpath-string . ns-binding)
  (let ((lst (if (null? ns-binding)
                 (sxml:xpath+index xpath-string)
                 (sxml:xpath+index xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        ((car lst) vtxp:doc))))

(define (sxml:test-xpointer+index xpath-string . ns-binding)
  (let ((lst (if (null? ns-binding)
	       (sxml:xpointer+index xpath-string)
	       (sxml:xpointer+index xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        ((car lst) vtxp:doc))))
       

(define (sxml:test-xpath+root+vars xpath-string var-binding . ns-binding)
  (let ((lst (if (null? ns-binding)
                 (sxml:xpath+root+vars xpath-string)
                 (sxml:xpath+root+vars xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        (lst vtxp:doc (cons `(*root* . ,vtxp:doc) var-binding)))))

(define (sxml:test-xpath+root xpath-string . ns-binding)
  (let ((lst (if (null? ns-binding)
                 (sxml:xpath+root xpath-string)
                 (sxml:xpath+root xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        (lst vtxp:doc `((*root* . ,vtxp:doc))))))

(define (sxml:test-xpointer+root+vars xpath-string var-binding . ns-binding)
  (let ((lst (if (null? ns-binding)
                 (sxml:xpointer+root+vars xpath-string)
                 (sxml:xpointer+root+vars xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        (lst vtxp:doc (cons `(*root* . ,vtxp:doc) var-binding)))))


(define (run-sxpathlib-test)
; Location path, full form: child::para 
; Location path, abbreviated form: para
; selects the para element children of the context node
(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para (@) "second par"))
       )
      (expected '((para (@) "para") (para (@) "second par")))
      )
  (sxp:run-test (select-kids (ntype?? 'para)) tree expected)
  (sxp:run-test (test-sxpath '(para)) tree expected)
)

; Location path, full form: child::* 
; Location path, abbreviated form: *
; selects all element children of the context node

(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par"))
       )
      (expected
       '((para (@) "para") (br (@)) (para "second par")))
      )
  (sxp:run-test (select-kids (ntype?? '*)) tree expected)
  (sxp:run-test (test-sxpath '(*)) tree expected)
)



; Location path, full form: child::text() 
; Location path, abbreviated form: text()
; selects all text node children of the context node
(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par"))
       )
      (expected
       '("cdata"))
      )
  (sxp:run-test (select-kids (ntype?? '*text*)) tree expected)
  (sxp:run-test (test-sxpath '(*text*)) tree expected)
)


; Location path, full form: child::node() 
; Location path, abbreviated form: node()
; selects all the children of the context node, whatever their node type
(let* ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par"))
       )
      (expected (cdr tree))
      )
  (sxp:run-test (select-kids (ntype?? '*any*)) tree expected)
  (sxp:run-test (test-sxpath '(*any*)) tree expected)
)

; Location path, full form: child::*/child::para 
; Location path, abbreviated form: */para
; selects all para grandchildren of the context node

(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para "third para")))
       )
      (expected
       '((para "third para")))
      )
  (sxp:run-test
   (node-join (select-kids (ntype?? '*))
	      (select-kids (ntype?? 'para)))
   tree expected)
  (sxp:run-test (test-sxpath '(* para)) tree expected)
)


; Location path, full form: attribute::name 
; Location path, abbreviated form: @name
; selects the 'name' attribute of the context node

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para (@) "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((name "elem")))
      )
  (sxp:run-test
   (node-join (select-kids (ntype?? '@))
	      (select-kids (ntype?? 'name)))
   tree expected)
  (sxp:run-test (test-sxpath '(@ name)) tree expected)
)

; Location path, full form:  attribute::* 
; Location path, abbreviated form: @*
; selects all the attributes of the context node
(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((name "elem") (id "idz")))
      )
  (sxp:run-test
   (node-join (select-kids (ntype?? '@))
	      (select-kids (ntype?? '*)))
   tree expected)
  (sxp:run-test (test-sxpath '(@ *)) tree expected)
)


; Location path, full form: descendant::para 
; Location path, abbreviated form: .//para
; selects the para element descendants of the context node

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((para (@) "para") (para "second par") (para (@) "third para")))
      )
  (sxp:run-test
   (node-closure (ntype?? 'para))
   tree expected)
  (sxp:run-test (test-sxpath '(// para)) tree expected)
)

; Location path, full form: self::para 
; Location path, abbreviated form: _none_
; selects the context node if it is a para element; otherwise selects nothing

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      )
  (sxp:run-test (node-self (ntype?? 'para)) tree '())
  (sxp:run-test (node-self (ntype?? 'elem)) tree (list tree))
)

; Location path, full form: descendant-or-self::node()
; Location path, abbreviated form: //
; selects the context node, all the children (including attribute nodes)
; of the context node, and all the children of all the (element)
; descendants of the context node.
; This is _almost_ a powerset of the context node.
(let* ((tree
       '(para (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       (cons tree
             '((para (@) "para")
               "para"
               (br (@))
               "cdata"
               (para "second par")
               "second par"
               (div (@ (name "aa")) (para (@) "third para"))
               (para (@) "third para")
               "third para")))
      )
  (sxp:run-test
   (sxml:descendant-or-self sxml:node?)
   tree expected)
  (sxp:run-test (test-sxpath '(//)) tree expected)
)

; Location path, full form: ancestor::div 
; Location path, abbreviated form: _none_
; selects all div ancestors of the context node
; This Location expression is equivalent to the following:
;	/descendant-or-self::div[descendant::node() = curr_node]
; This shows that the ancestor:: axis is actually redundant. Still,
; it can be emulated as the following test-sxpath expression demonstrates.

; The insight behind "ancestor::div" -- selecting all "div" ancestors
; of the current node -- is
;  S[ancestor::div] context_node =
;    { y | y=subnode*(root), context_node=subnode(subnode*(y)),
;          isElement(y), name(y) = "div" }
; We observe that
;    { y | y=subnode*(root), pred(y) }
; can be expressed in test-sxpath as 
;    ((node-or (node-self pred) (node-closure pred)) root-node)
; The composite predicate 'isElement(y) & name(y) = "div"' corresponds to 
; (node-self (ntype?? 'div)) in test-sxpath. Finally, filter
; context_node=subnode(subnode*(y)) is tantamount to
; (node-closure (node-eq? context-node)), whereas node-reduce denotes the
; the composition of converters-predicates in the filtering context.

(let*
    ((root
	 '(div (@ (name "elem") (id "idz")) 
		(para (@) "para") (br (@)) "cdata" (para (@) "second par")
		(div (@ (name "aa")) (para (@) "third para"))))
     (context-node	; /descendant::any()[child::text() == "third para"]
      (car
       ((node-closure 
	 (select-kids
	  (node-equal? "third para")))
       root)))
    (pred
     (node-reduce (node-self (ntype?? 'div))
		  (node-closure (node-eq? context-node))
		  ))
     )
  (sxp:run-test
   (node-or
     (node-self pred)
     (node-closure pred))
   root 
   (cons root
	 '((div (@ (name "aa")) (para (@) "third para")))))
)



; Location path, full form: child::div/descendant::para 
; Location path, abbreviated form: div//para
; selects the para element descendants of the div element
; children of the context node

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")
	     (div (para "fourth para"))))
       )
      (expected
       '((para (@) "third para") (para "fourth para")))
      )
  (sxp:run-test
   (node-join 
    (select-kids (ntype?? 'div))
    (node-closure (ntype?? 'para)))
   tree expected)
  (sxp:run-test (test-sxpath '(div // para)) tree expected)
)


; Location path, full form: /descendant::olist/child::item 
; Location path, abbreviated form: //olist/item
; selects all the item elements that have an olist parent (which is not root)
; and that are in the same document as the context node
; See the following test.

; Location path, full form: /descendant::td/attribute::align 
; Location path, abbreviated form: //td/@align
; Selects 'align' attributes of all 'td' elements in tree1
(let ((tree vsxplb:tree1)
      (expected
       '((align "right") (align "center") (align "center") (align "center"))
      ))
  (sxp:run-test
   (node-join 
    (node-closure (ntype?? 'td))
    (select-kids (ntype?? '@))
    (select-kids (ntype?? 'align)))
   tree expected)
  (sxp:run-test (test-sxpath '(// td @ align)) tree expected)
)


; Location path, full form: /descendant::td[attribute::align] 
; Location path, abbreviated form: //td[@align]
; Selects all td elements that have an attribute 'align' in tree1
(let ((tree vsxplb:tree1)
      (expected
       '((td (@ (align "right")) "Talks ") (td (@ (align "center")) " = ")
	 (td (@ (align "center")) " = ") (td (@ (align "center")) " = "))
       ))
  (sxp:run-test
   (node-reduce 
    (node-closure (ntype?? 'td))
    (sxml:filter
     (node-join
      (select-kids (ntype?? '@))
      (select-kids (ntype?? 'align)))))
   tree expected)
  (sxp:run-test (test-sxpath 
             `(// td
                  ,(lambda (node . var-binding)
                     ((node-self (test-sxpath '(@ align))) node))))
             tree expected)
  (sxp:run-test (test-sxpath '(// (td (@ align)))) tree expected)
  (sxp:run-test (test-sxpath '(// ((td) (@ align)))) tree expected)
  ; note! (test-sxpath ...) is a converter. Therefore, it can be used
  ; as any other converter, for example, in the full-form test-sxpath.
  ; Thus we can mix the full and abbreviated form test-sxpath's freely.
  (sxp:run-test
   (node-reduce 
    (node-closure (ntype?? 'td))
    (sxml:filter
     (test-sxpath '(@ align))))
   tree expected)
)


; Location path, full form: /descendant::td[attribute::align = "right"] 
; Location path, abbreviated form: //td[@align = "right"]
; Selects all td elements that have an attribute align = "right" in tree1
(let ((tree vsxplb:tree1)
      (expected
       '((td (@ (align "right")) "Talks "))
       ))
  (sxp:run-test
   (node-reduce 
    (node-closure (ntype?? 'td))
    (sxml:filter
     (node-join
      (select-kids (ntype?? '@))
      (select-kids (node-equal? '(align "right"))))))
   tree expected)
  (sxp:run-test (test-sxpath '(// (td (@ (equal? (align "right")))))) tree expected)
)

; Location path, full form: child::para[position()=1] 
; Location path, abbreviated form: para[1]
; selects the first para child of the context node
(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((para (@) "para"))
      ))
  (sxp:run-test
   (node-reduce
    (select-kids (ntype?? 'para))
    (node-pos 1))
   tree expected)
  (sxp:run-test (test-sxpath '((para 1))) tree expected)
)

; Node-pos for a negative index that is not present in a list
; Test suggested by Peter Bex
(sxp:run-test
 (node-pos -6)
 '((div "hi") (span "hello") (em "is this thing on?"))
 '())

; Location path, full form: child::para[position()=last()] 
; Location path, abbreviated form: para[last()]
; selects the last para child of the context node
(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((para "second par"))
      ))
  (sxp:run-test
   (node-reduce
    (select-kids (ntype?? 'para))
    (node-pos -1))
   tree expected)
  (sxp:run-test (test-sxpath '((para -1))) tree expected)
)

; Illustrating the following Note of Sec 2.5 of XPath:
; "NOTE: The location path //para[1] does not mean the same as the
; location path /descendant::para[1]. The latter selects the first
; descendant para element; the former selects all descendant para
; elements that are the first para children of their parents."

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      )
  (sxp:run-test
   (node-reduce	; /descendant::para[1] in test-sxpath
    (node-closure (ntype?? 'para))
    (node-pos 1))
   tree '((para (@) "para")))
  (sxp:run-test (test-sxpath '(// (para 1))) tree
	    '((para (@) "para") (para (@) "third para")))
)

; Location path, full form: parent::node()
; Location path, abbreviated form: ..
; selects the parent of the context node. The context node may be
; an attribute node!
; For the last test:
; Location path, full form: parent::*/attribute::name
; Location path, abbreviated form: ../@name
; Selects the name attribute of the parent of the context node

(let* ((tree
	'(elem (@ (name "elem") (id "idz")) 
	       (para (@) "para") (br (@)) "cdata" (para "second par")
	       (div (@ (name "aa")) (para (@) "third para")))
	)
       (para1		; the first para node
	(car ((test-sxpath '(para)) tree)))
       (para3		; the third para node
	(car ((test-sxpath '(div para)) tree)))
       (div		; div node
	(car ((test-sxpath '(// div)) tree)))
       )
  (sxp:run-test
   (node-parent tree)
   para1 (list tree))
  (sxp:run-test
   (node-parent tree)
   para3 (list div))
  (sxp:run-test		; checking the parent of an attribute node
   (node-parent tree)
   ((test-sxpath '(@ name)) div) (list div))
  (sxp:run-test
   (node-join
    (node-parent tree)
    (select-kids (ntype?? '@))
    (select-kids (ntype?? 'name)))
   para3 '((name "aa")))
  (sxp:run-test
   (test-sxpath `(,(lambda (node . var-binding)
                     ((node-parent tree) node))
                  @ name))
   para3 '((name "aa")))
)

; Location path, full form: following-sibling::chapter[position()=1]
; Location path, abbreviated form: none
; selects the next chapter sibling of the context node
; The path is equivalent to
;  let cnode = context-node
;    in
;	parent::* / child::chapter [take-after node_eq(self::*,cnode)] 
;		[position()=1]
(let* ((tree
       '(document
	 (preface "preface")
	 (chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (chapter (@ (id "four")) "Chap 4 text")
	 (epilogue "Epilogue text")
	 (appendix (@ (id "A")) "App A text")
	 (References "References"))
       )
       (a-node	; to be used as a context node
	(car ((test-sxpath '(// (chapter (@ (equal? (id "two")))))) tree)))
       (expected
       '((chapter (@ (id "three")) "Chap 3 text")))
      )
  (sxp:run-test
   (node-reduce
    (node-join
     (node-parent tree)
     (select-kids (ntype?? 'chapter)))
    (take-after (node-eq? a-node))
    (node-pos 1)
    )
   a-node expected)
)

; preceding-sibling::chapter[position()=1]
; selects the previous chapter sibling of the context node
; The path is equivalent to
;  let cnode = context-node
;    in
;	parent::* / child::chapter [take-until node_eq(self::*,cnode)] 
;		[position()=-1]
(let* ((tree
       '(document
	 (preface "preface")
	 (chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (chapter (@ (id "four")) "Chap 4 text")
	 (epilogue "Epilogue text")
	 (appendix (@ (id "A")) "App A text")
	 (References "References"))
       )
       (a-node	; to be used as a context node
	(car ((test-sxpath '(// (chapter (@ (equal? (id "three")))))) tree)))
       (expected
       '((chapter (@ (id "two")) "Chap 2 text")))
      )
  (sxp:run-test
   (node-reduce
    (node-join
     (node-parent tree)
     (select-kids (ntype?? 'chapter)))
    (take-until (node-eq? a-node))
    (node-pos -1)
    )
   a-node expected)
)


; /descendant::figure[position()=42]
; selects the forty-second figure element in the document
; See the next example, which is more general.

; Location path, full form:
;    child::table/child::tr[position()=2]/child::td[position()=3] 
; Location path, abbreviated form: table/tr[2]/td[3]
; selects the third td of the second tr of the table
(let ((tree ((node-closure (ntype?? 'p)) vsxplb:tree1))
      (expected
       '((td " data + control"))
       ))
  (sxp:run-test
   (node-join
    (select-kids (ntype?? 'table))
    (node-reduce (select-kids (ntype?? 'tr))
		 (node-pos 2))
    (node-reduce (select-kids (ntype?? 'td))
		 (node-pos 3)))
   tree expected)
  ((test-sxpath '(table (tr 2) (td 3))) tree)
)


; Location path, full form:
;		child::para[attribute::type='warning'][position()=5] 
; Location path, abbreviated form: para[@type='warning'][5]
; selects the fifth para child of the context node that has a type
; attribute with value warning
(let ((tree
       '(chapter
	 (para "para1")
	 (para (@ (type "warning")) "para 2")
	 (para (@ (type "warning")) "para 3")
	 (para (@ (type "warning")) "para 4")
	 (para (@ (type "warning")) "para 5")
	 (para (@ (type "warning")) "para 6"))
       )
      (expected
       '((para (@ (type "warning")) "para 6"))
      ))
  (sxp:run-test
   (node-reduce
    (select-kids (ntype?? 'para))
    (sxml:filter
     (node-join
      (select-kids (ntype?? '@))
      (select-kids (node-equal? '(type "warning")))))
    (node-pos 5))
   tree expected)
  (sxp:run-test (test-sxpath '( (((para (@ (equal? (type "warning"))))) 5 )  ))
	    tree expected)
  ((test-sxpath '( (para (@ (equal? (type "warning"))) 5 )  ))
	    tree)
)


; Location path, full form:
;		child::para[position()=5][attribute::type='warning'] 
; Location path, abbreviated form: para[5][@type='warning']
; selects the fifth para child of the context node if that child has a 'type'
; attribute with value warning
(let ((tree
       '(chapter
	 (para "para1")
	 (para (@ (type "warning")) "para 2")
	 (para (@ (type "warning")) "para 3")
	 (para (@ (type "warning")) "para 4")
	 (para (@ (type "warning")) "para 5")
	 (para (@ (type "warning")) "para 6"))
       )
      (expected
       '((para (@ (type "warning")) "para 5"))
      ))
  (sxp:run-test
   (node-reduce
    (select-kids (ntype?? 'para))
    (node-pos 5)
    (sxml:filter
     (node-join
      (select-kids (ntype?? '@))
      (select-kids (node-equal? '(type "warning"))))))
   tree expected)
  (sxp:run-test (test-sxpath '( (( (para 5))  (@ (equal? (type "warning"))))))
	    tree expected)
  (sxp:run-test (test-sxpath '( (para 5 (@ (equal? (type "warning")))) ))
	    tree expected)
)

; Location path, full form:
;		child::*[self::chapter or self::appendix]
; Location path, semi-abbreviated form: *[self::chapter or self::appendix]
; selects the chapter and appendix children of the context node
(let ((tree
       '(document
	 (preface "preface")
	 (chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (epilogue "Epilogue text")
	 (appendix (@ (id "A")) "App A text")
	 (References "References"))
       )
      (expected
       '((chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (appendix (@ (id "A")) "App A text"))
      ))
  (sxp:run-test
   (node-join
    (select-kids (ntype?? '*))
    (sxml:filter
     (node-or
      (node-self (ntype?? 'chapter))
      (node-self (ntype?? 'appendix)))))
   tree expected)
  (sxp:run-test (test-sxpath `(* ,(lambda (node . var-binding)
                                ((node-or
                                  (node-self (ntype?? 'chapter))
				  (node-self (ntype?? 'appendix)))
                                 node))))
	    tree expected)
)


; Location path, full form: child::chapter[child::title='Introduction'] 
; Location path, abbreviated form: chapter[title = 'Introduction']
; selects the chapter children of the context node that have one or more
; title children with string-value equal to Introduction
; See a similar example: //td[@align = "right"] above.

; Location path, full form: child::chapter[child::title] 
; Location path, abbreviated form: chapter[title]
; selects the chapter children of the context node that have one or
; more title children
; See a similar example //td[@align] above.

(cerr "\nExample with tree3: extracting the first lines of every stanza\n")
(let ((tree vsxplb:tree3)
      (expected
       '("Let us go then, you and I," "In the room the women come and go")
      ))
  (sxp:run-test
   (node-join
    (node-closure (ntype?? 'stanza))
    (node-reduce 
     (select-kids (ntype?? 'line)) (node-pos 1))
    (select-kids (ntype?? '*text*)))
   tree expected)
  (sxp:run-test (test-sxpath '(// stanza (line 1) *text*)) tree expected)
)

(cout nl "Sxpathlib tests passed successfully!" nl)
)

(define (run-sxpathext-test)
; sxml:string
(xtest-assert ; Expected result:
"some string"
; <--- of:
sxml:string
"some string"
)

; sxml:string
(xtest-assert ; Expected result:
"0"
; <--- of:
sxml:string
0
)

; sxml:string
(xtest-assert ; Expected result:
"-76"
; <--- of:
sxml:string
-76
)

; sxml:string
(xtest-assert ; Expected result:
"true"
; <--- of:
sxml:string
#t
)

; sxml:string
(xtest-assert ; Expected result:
"false"
; <--- of:
sxml:string
#f
)

; sxml:string
(xtest-assert ; Expected result:
"What is XLink?hyperlink"
; <--- of:
sxml:string
(list node1 node2)
)

; sxml:string
(xtest-assert ; Expected result:
""
; <--- of:
sxml:string
'()
)

; sxml:boolean
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:boolean
#t
)

; sxml:boolean
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:boolean
-56
)

; sxml:boolean
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:boolean
0
)

; sxml:boolean
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:boolean
"word"
)

; sxml:boolean
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:boolean
""
)

; sxml:boolean
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:boolean
(list node1 node2)
)

; sxml:boolean
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:boolean
'()
)

; sxml:number
(xtest-assert ; Expected result:
45
; <--- of:
sxml:number
45
)

; sxml:number
(xtest-assert ; Expected result:
-34
; <--- of:
sxml:number
"  -34  "
)

; sxml:number
(xtest-assert ; Expected result:
-34.654
; <--- of:
sxml:number
"  -34.654  "
)

; sxml:number
(xtest-assert ; Expected result:
1
; <--- of:
sxml:number
#t
)

; sxml:number
(xtest-assert ; Expected result:
0
; <--- of:
sxml:number
#f
)

;------------------------------------------------
; sxml:string-value

; sxml:string-value
(xtest-assert ; Expected result:
"erer"
; <--- of:
sxml:string-value
"erer"
)

; sxml:string-value
(xtest-assert ; Expected result:
'"What is XLink?hyperlink"
; <--- of:
sxml:string-value
node1
)

; sxml:string-value
(xtest-assert ; Expected result:
'"XML Pointing Language (XPointer)"
; <--- of:
sxml:string-value
node2
)

; sxml:string-value
(xtest-assert ; Expected result:
'"version='1.0'chapter1chapter2chapter3chapter4chapter5chapter6chapter7AbstractThis document describes about XLink Engine...IntroductionThis document is written in XML (eXtensible Markup Language) ver.1.0.What is XLink?hyperlinkWhat is XPointer?XPointer is the fragment identifier of documents having the mime-type hogehoge.Models for using XLink/XPointer There are important keywords.samplesConclusionThanks a lot.Who1XML Linking Language (XLink)foo.comWho2XML Pointing Language (XPointer)boo.com"
; <--- of:
sxml:string-value
xt-doc
)


;=========================================================================
; XPath object comparison

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
#f
12
)

; sxml:equal?
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:equal?
#t
12
)

; sxml:not-equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:not-equal?
#t
0
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
#f
""
)

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
#f
"something"
)

; sxml:not-equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:not-equal?
#t
"something"
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
"  12 "
12
)

; sxml:not-equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:not-equal?
"  123 "
12
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp >)
#t
0
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp <)
#f
1
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#f
; <--- of:
(sxml:relational-cmp <=)
" 12   "
1
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp >=)
" 12   "
" -2 "
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp <=)
"foo"
1
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#f
; <--- of:
(sxml:relational-cmp <)
"5 "
"  -53"
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#f
; <--- of:
(sxml:relational-cmp <)
"5 "
"eee"
)

;------------------------------------------------
 ;; Nodeset comparison:
 
;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp >)
"  100"
(list node1 node2)
)

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
(list node1 node2)
7
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#f
; <--- of:
(sxml:relational-cmp >=)
(list node1 node2)
2
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp <=)
(list node1 node2)
3
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
(list node1 node2)
#t
)

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
'()
#t
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
'()
#f
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
(list node1 node2)
(list node2 node3)
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp >=)
(list node1 node2)
(list node2 node3)
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp <=)
(list node1 node2)
(list node2 node3)
)

; sxml:not-equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:not-equal?
(list node1 node2)
(list node2 node3)
)

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
(list node1 node2 )
(list node3)
)

; sxml:equal?
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:equal?
(list xt-doc)
(list node3)
)

;=========================================================================
; Node tests

; sxml:node?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:node?
node1
)

; sxml:node?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:node?
'(@ (id "chap3"))
)

; sxml:node?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:node?
node2
)

; sxml:node?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:node?
"XML Pointing Language (XPointer)"
)

 ; XPath axises

;@ sxml:ancestor 
(xtest-assert ; Expected result:
(list
 '(body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 '(doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 xt-doc)
; <--- of:
((sxml:ancestor (ntype?? '*)) xt-doc)
node1
)

;@ sxml:ancestor
(xtest-assert ; Expected result:
`((item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc)
; <--- of:
((sxml:ancestor (ntype?? '*)) xt-doc)
node2
)

;@ sxml:ancestor
(xtest-assert ; Expected result:
`((item (@ (id "toc3")) "chapter3")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc)
; <--- of:
((sxml:ancestor (ntype?? '*)) xt-doc)
node3
)

;@ sxml:ancestor
(xtest-assert ; Expected result:
`((item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc
 (item (@ (id "toc3")) "chapter3")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc)
; <--- of:
((sxml:ancestor (ntype?? '*)) xt-doc)
(list node2 node2 node3)
)

;@ sxml:ancestor-or-self 
(xtest-assert ; Expected result:
`((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc)
; <--- of:
((sxml:ancestor-or-self (ntype?? '*)) xt-doc)
node1
)

;@ sxml:ancestor-or-self
(xtest-assert ; Expected result:
`((title "XML Pointing Language (XPointer)")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc)
; <--- of:
((sxml:ancestor-or-self (ntype?? '*)) xt-doc)
node2
)

;@ sxml:ancestor-or-self
(xtest-assert ; Expected result:
`((id "toc3")
 (item (@ (id "toc3")) "chapter3")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
  ,xt-doc)
; <--- of:
((sxml:ancestor-or-self (ntype?? '*)) xt-doc)
node3
)

;@ sxml:ancestor-or-self 
(xtest-assert ; Expected result:
`((title "XML Pointing Language (XPointer)")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc
 (title "XML Pointing Language (XPointer)")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc
 (id "toc3")
 (item (@ (id "toc3")) "chapter3")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 ,xt-doc)
; <--- of:
((sxml:ancestor-or-self (ntype?? '*)) xt-doc)
(list node2 node2 node3)
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'((id "chap3"))
; <--- of:
(sxml:attribute (ntype?? '*))
node1
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'((id "chap3"))
; <--- of:
(sxml:attribute (ntype?? 'id))
node1
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'()
; <--- of:
(sxml:attribute (ntype?? '*))
node2
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'()
; <--- of:
(sxml:attribute (ntype?? 'absent))
'(empty-node)
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:title "Chap.1 toc")
 (http://www.w3.org/1999/xlink:role "booboo")
 (http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:href "#toc1"))
; <--- of:
(sxml:attribute (ntype?? '*))
'(loc
 (@
  (http://www.w3.org/1999/xlink:type "locator")
  (http://www.w3.org/1999/xlink:title "Chap.1 toc")
  (http://www.w3.org/1999/xlink:role "booboo")
  (http://www.w3.org/1999/xlink:label "boo")
  (http://www.w3.org/1999/xlink:href "#toc1")))
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:title "Chap.1 toc")
 (http://www.w3.org/1999/xlink:role "booboo")
 (http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:href "#toc1")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:title "Chap.1 cont")
 (http://www.w3.org/1999/xlink:role "text/xml")
 (http://www.w3.org/1999/xlink:label "hoge")
 (http://www.w3.org/1999/xlink:href "#chap1"))
; <--- of:
(sxml:attribute (ntype?? '*))
'((loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1"))))
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((title "What is XLink?") (p "hyperlink"))
; <--- of:
(sxml:descendant (ntype?? '*))
node1
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((title "What is XLink?") "What is XLink?" (p "hyperlink") "hyperlink")
; <--- of:
(sxml:descendant sxml:node?)
node1
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (item (@ (id "toc1")) "chapter1")
 (item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc6")) "chapter6")
 (item (@ (id "toc7")) "chapter7")
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 (p "This document describes about XLink Engine...")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 (p "Thanks a lot.")
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 (title "XML Linking Language (XLink)")
 (ref "foo.com")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 (title "XML Pointing Language (XPointer)")
 (ref "boo.com"))
; <--- of:
(sxml:descendant (ntype?? '*))
xt-doc
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((title "What is XLink?")
 "What is XLink?"
 (p "hyperlink")
 "hyperlink"
 "XML Pointing Language (XPointer)")
; <--- of:
(sxml:descendant sxml:node?)
(list node1 node2)
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((title "Abstract")
 (p "This document describes about XLink Engine...")
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (title "What is XLink?")
 (p "hyperlink")
 (title "What is XPointer?")
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (title "samples")
 (title "Conclusion")
 (p "Thanks a lot."))
; <--- of:
(sxml:descendant (ntype?? '*))
'((chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (chapter (@ (id "chap6")) (title "samples"))
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot.")))
)


;@ sxml:descendant-or-self
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink"))
; <--- of:
(sxml:descendant-or-self (ntype?? '*))
node1
)

;@ sxml:descendant-or-self
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 "What is XLink?"
 (p "hyperlink")
 "hyperlink")
; <--- of:
(sxml:descendant-or-self sxml:node?)
node1
)

;@ sxml:descendant-or-self 
(xtest-assert ; Expected result:
`(,xt-doc
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (item (@ (id "toc1")) "chapter1")
 (item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc6")) "chapter6")
 (item (@ (id "toc7")) "chapter7")
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 (p "This document describes about XLink Engine...")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 (p "Thanks a lot.")
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 (title "XML Linking Language (XLink)")
 (ref "foo.com")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 (title "XML Pointing Language (XPointer)")
 (ref "boo.com"))
; <--- of:
(sxml:descendant-or-self (ntype?? '*))
xt-doc
)


;@ sxml:descendant-or-self
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 "What is XLink?"
 (p "hyperlink")
 "hyperlink"
 (title "XML Pointing Language (XPointer)")
 "XML Pointing Language (XPointer)")
; <--- of:
(sxml:descendant-or-self sxml:node?)
(list node1 node2)
)

;@ sxml:descendant-or-self
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 (p "This document describes about XLink Engine...")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 (p "Thanks a lot."))
; <--- of:
(sxml:descendant-or-self (ntype?? '*))
'((chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (chapter (@ (id "chap6")) (title "samples"))
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot.")))
)
 
;@ sxml:following 
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 (p "Thanks a lot.")
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 (title "XML Linking Language (XLink)")
 (ref "foo.com")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 (title "XML Pointing Language (XPointer)")
 (ref "boo.com"))
; <--- of:
((sxml:following (ntype?? '*)) xt-doc)
node1
)

;@ sxml:following sxml:node?
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 "What is XPointer?"
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 "Models for using XLink/XPointer "
 (p "There are important keywords.")
 "There are important keywords."
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 "samples"
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 "Conclusion"
 (p "Thanks a lot.")
 "Thanks a lot."
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 "Who1"
 (title "XML Linking Language (XLink)")
 "XML Linking Language (XLink)"
 (ref "foo.com")
 "foo.com"
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 "Who2"
 (title "XML Pointing Language (XPointer)")
 "XML Pointing Language (XPointer)"
 (ref "boo.com")
 "boo.com")
; <--- of:
((sxml:following sxml:node?) xt-doc)
node1
)

;@ sxml:following
(xtest-assert ; Expected result:
'((ref "boo.com"))
; <--- of:
((sxml:following (ntype?? '*)) xt-doc)
node2
)

;@ sxml:following
(xtest-assert ; Expected result:
'((ref "boo.com") "boo.com")
; <--- of:
((sxml:following sxml:node?) xt-doc)
node2
)

;@ sxml:following 
(xtest-assert ; Expected result:
'((item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc6")) "chapter6")
 (item (@ (id "toc7")) "chapter7")
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 (p "This document describes about XLink Engine...")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 (p "Thanks a lot.")
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 (title "XML Linking Language (XLink)")
 (ref "foo.com")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 (title "XML Pointing Language (XPointer)")
 (ref "boo.com"))
; <--- of:
((sxml:following (ntype?? '*)) xt-doc)
node3
)

;@ sxml:following
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 "What is XPointer?"
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 "Models for using XLink/XPointer "
 (p "There are important keywords.")
 "There are important keywords."
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 "samples"
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 "Conclusion"
 (p "Thanks a lot.")
 "Thanks a lot."
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 "Who1"
 (title "XML Linking Language (XLink)")
 "XML Linking Language (XLink)"
 (ref "foo.com")
 "foo.com"
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 "Who2"
 (title "XML Pointing Language (XPointer)")
 "XML Pointing Language (XPointer)"
 (ref "boo.com")
 "boo.com"
 (ref "boo.com")
 "boo.com")
; <--- of:
((sxml:following sxml:node?) xt-doc)
(list node1 node2)
)

;@ sxml:following-sibling
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (chapter (@ (id "chap6")) (title "samples"))
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com")))))
; <--- of:
((sxml:following-sibling (ntype?? '*)) xt-doc)
node1
)

;@ sxml:following-sibling
(xtest-assert ; Expected result:
'((ref "boo.com"))
; <--- of:
((sxml:following-sibling (ntype?? '*)) xt-doc)
node2
)

;@ sxml:following-sibling
(xtest-assert ; Expected result:
'()
; <--- of:
((sxml:following-sibling (ntype?? '*)) xt-doc)
node3
)

;@ sxml:following-sibling
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (chapter (@ (id "chap6")) (title "samples"))
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (ref "boo.com"))
; <--- of:
((sxml:following-sibling (ntype?? '*)) xt-doc)
(list node1 node2)
)

;@ sxml:namespace
(xtest-assert ; Expected result:
'()
; <--- of:
(sxml:namespace (ntype?? '*))
node1
)

;@ sxml:namespace
(xtest-assert ; Expected result:
'((pref "http://www.pref.org") (npref "http://www.npref.org"))
; <--- of:
(sxml:namespace (ntype?? '*))
'(pref:tag (@) (@@ (*NAMESPACES* (pref "http://www.pref.org")
				 (npref "http://www.npref.org"))))
)

;@ sxml:namespace
(xtest-assert ; Expected result:
'((pref "http://www.pref.org"))
; <--- of:
(sxml:namespace (ntype?? 'pref))
'(pref:tag (@) (@@ (*NAMESPACES* (pref "http://www.pref.org")
				 (npref "http://www.npref.org"))))
)

;@ sxml:parent
(xtest-assert ; Expected result:
'((body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com"))))))
; <--- of:
((sxml:parent (ntype?? '*)) xt-doc)
node1
)

;@ sxml:parent
(xtest-assert ; Expected result:
'((item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com")))
; <--- of:
((sxml:parent (ntype?? '*)) xt-doc)
node2
)


;@ sxml:parent 
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
((sxml:parent (ntype?? '*)) xt-doc)
node3
)

;@ sxml:parent 
(xtest-assert ; Expected result:
'((body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (item (@ (id "toc3")) "chapter3"))
; <--- of:
((sxml:parent (ntype?? '*)) xt-doc)
(list node1 node2 node3)
)

;@ sxml:preceding
(xtest-assert ; Expected result:
'((p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (p "This document describes about XLink Engine...")
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (item (@ (id "toc7")) "chapter7")
 (item (@ (id "toc6")) "chapter6")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   )
; <--- of:
((sxml:preceding (ntype?? '*)) xt-doc)
node1
)


;@ sxml:preceding
(xtest-assert ; Expected result:
'("This document is written in XML (eXtensible Markup Language) ver.1.0."
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 "Introduction"
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 "This document describes about XLink Engine..."
 (p "This document describes about XLink Engine...")
 "Abstract"
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 "chapter7"
 (item (@ (id "toc7")) "chapter7")
 "chapter6"
 (item (@ (id "toc6")) "chapter6")
 "chapter5"
 (item (@ (id "toc5")) "chapter5")
 "chapter4"
 (item (@ (id "toc4")) "chapter4")
 "chapter3"
 (item (@ (id "toc3")) "chapter3")
 "chapter2"
 (item (@ (id "toc2")) "chapter2")
 "chapter1"
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (*PI* xml "version='1.0'"))   
; <--- of:
((sxml:preceding sxml:node?) xt-doc)
node1
)

;@ sxml:preceding
(xtest-assert ; Expected result:
'((author "Who2")
 (ref "foo.com")
 (title "XML Linking Language (XLink)")
 (author "Who1")
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (p "Thanks a lot.")
 (title "Conclusion")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "samples")
 (chapter (@ (id "chap6")) (title "samples"))
 (p "There are important keywords.")
 (title "Models for using XLink/XPointer ")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (title "What is XPointer?")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (p "hyperlink")
 (title "What is XLink?")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (p "This document describes about XLink Engine...")
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (item (@ (id "toc7")) "chapter7")
 (item (@ (id "toc6")) "chapter6")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))))
; <--- of:
((sxml:preceding (ntype?? '*)) xt-doc)
node2
)

;@ sxml:preceding
(xtest-assert ; Expected result:
'("Who2"
 (author "Who2")
 "foo.com"
 (ref "foo.com")
 "XML Linking Language (XLink)"
 (title "XML Linking Language (XLink)")
 "Who1"
 (author "Who1")
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 "Thanks a lot."
 (p "Thanks a lot.")
 "Conclusion"
 (title "Conclusion")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 "samples"
 (title "samples")
 (chapter (@ (id "chap6")) (title "samples"))
 "There are important keywords."
 (p "There are important keywords.")
 "Models for using XLink/XPointer "
 (title "Models for using XLink/XPointer ")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 "What is XPointer?"
 (title "What is XPointer?")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 "hyperlink"
 (p "hyperlink")
 "What is XLink?"
 (title "What is XLink?")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 "This document is written in XML (eXtensible Markup Language) ver.1.0."
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 "Introduction"
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 "This document describes about XLink Engine..."
 (p "This document describes about XLink Engine...")
 "Abstract"
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 "chapter7"
 (item (@ (id "toc7")) "chapter7")
 "chapter6"
 (item (@ (id "toc6")) "chapter6")
 "chapter5"
 (item (@ (id "toc5")) "chapter5")
 "chapter4"
 (item (@ (id "toc4")) "chapter4")
 "chapter3"
 (item (@ (id "toc3")) "chapter3")
 "chapter2"
 (item (@ (id "toc2")) "chapter2")
 "chapter1"
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (*PI* xml "version='1.0'"))
; <--- of:
((sxml:preceding sxml:node?) xt-doc)
node2
)

;@ sxml:preceding
(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))))
; <--- of:
((sxml:preceding (ntype?? '*)) xt-doc)
node3
)

;@ sxml:preceding
(xtest-assert ; Expected result:
'("This document is written in XML (eXtensible Markup Language) ver.1.0."
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 "Introduction"
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 "This document describes about XLink Engine..."
 (p "This document describes about XLink Engine...")
 "Abstract"
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 "chapter7"
 (item (@ (id "toc7")) "chapter7")
 "chapter6"
 (item (@ (id "toc6")) "chapter6")
 "chapter5"
 (item (@ (id "toc5")) "chapter5")
 "chapter4"
 (item (@ (id "toc4")) "chapter4")
 "chapter3"
 (item (@ (id "toc3")) "chapter3")
 "chapter2"
 (item (@ (id "toc2")) "chapter2")
 "chapter1"
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (*PI* xml "version='1.0'")
 "Who2"
 (author "Who2")
 "foo.com"
 (ref "foo.com")
 "XML Linking Language (XLink)"
 (title "XML Linking Language (XLink)")
 "Who1"
 (author "Who1")
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 "Thanks a lot."
 (p "Thanks a lot.")
 "Conclusion"
 (title "Conclusion")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 "samples"
 (title "samples")
 (chapter (@ (id "chap6")) (title "samples"))
 "There are important keywords."
 (p "There are important keywords.")
 "Models for using XLink/XPointer "
 (title "Models for using XLink/XPointer ")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 "What is XPointer?"
 (title "What is XPointer?")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 "hyperlink"
 (p "hyperlink")
 "What is XLink?"
 (title "What is XLink?")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 "This document is written in XML (eXtensible Markup Language) ver.1.0."
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 "Introduction"
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 "This document describes about XLink Engine..."
 (p "This document describes about XLink Engine...")
 "Abstract"
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 "chapter7"
 (item (@ (id "toc7")) "chapter7")
 "chapter6"
 (item (@ (id "toc6")) "chapter6")
 "chapter5"
 (item (@ (id "toc5")) "chapter5")
 "chapter4"
 (item (@ (id "toc4")) "chapter4")
 "chapter3"
 (item (@ (id "toc3")) "chapter3")
 "chapter2"
 (item (@ (id "toc2")) "chapter2")
 "chapter1"
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (*PI* xml "version='1.0'"))
; <--- of:
((sxml:preceding sxml:node?) xt-doc)
(list node1 node2)
)

;@ sxml:preceding-sibling
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine...")))
; <--- of:
((sxml:preceding-sibling (ntype?? '*)) xt-doc)
node1
)

;@ sxml:preceding-sibling
(xtest-assert ; Expected result:
'((author "Who2"))
; <--- of:
((sxml:preceding-sibling (ntype?? '*)) xt-doc)
node2
)

;@ sxml:preceding-sibling
(xtest-assert ; Expected result:
'()
; <--- of:
((sxml:preceding-sibling (ntype?? '*)) xt-doc)
node3
)

;@ sxml:preceding-sibling
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (author "Who2"))
; <--- of:
((sxml:preceding-sibling (ntype?? '*)) xt-doc)
(list node1 node2)
)

(cout nl "SXPath-ext tests passed successfully!" nl)
)

(define (run-txpath-test)

;=========================================================================
; Test assertions are performed here

 ; SXPath testing

; sxml:xpath
(xtest-assert ; Expected result:
'("Text node")
; <--- of:
(sxml:xpath ".")
'("Text node")
)

 ; These XPath expressions are equal:

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"child::*/child::*[2]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"*/*[2]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"/*/*[2]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"descendant-or-self::node()[attribute::id ='toc1']"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"//*[attribute::* ='toc1']"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"/descendant::node()[attribute::id][1]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"//*[ self::node() = id('toc1') ]"
)

 ; Node tests

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0' encoding=\"Shift_JIS\""))
; <--- of:
sxml:test-xpath+index
"descendant::processing-instruction()"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0' encoding=\"Shift_JIS\""))
; <--- of:
sxml:test-xpath+index
"descendant::processing-instruction( 'xml' )"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"descendant::processing-instruction( 'smth else' )"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"//*[ self::processing-instruction('smth else') ]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'("chapter1"
 "chapter2"
 "chapter3"
 "chapter4"
 "chapter5"
 "chapter6"
 "chapter7"
 "Abstract"
 "This document describes about XLink Engine..."
 "Introduction"
 "This document is written in XML (eXtensible Markup Language) ver.1.0."
 "What is XLink?"
 "hyperlink"
 "What is XPointer?"
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 "Models for using XLink/XPointer "
 "There are important keywords."
 "samples"
 "Conclusion"
 "Thanks a lot."
 "Who1"
 "XML Linking Language (XLink)"
 "foo.com"
 "Who2"
 "XML Pointing Language (XPointer)"
 "boo.com")
; <--- of:
sxml:test-xpath+index
"descendant-or-self::text()"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'("boo.com")
; <--- of:
sxml:test-xpath+index
"descendant-or-self::text()[ self::node() = 'boo.com' ]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'("chapter1" "chapter2" "chapter3" "chapter4" "chapter5" "chapter6" "chapter7")
; <--- of:
sxml:test-xpath+index
"*/*/text()"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "extended")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "arc")
 (http://www.w3.org/1999/xlink:type "arc"))
; <--- of:
sxml:test-xpath+index
"//attribute::xlink:type"
vtxp:ns-binding
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "hoge")
 (http://www.w3.org/1999/xlink:to "hoge")
 (http://www.w3.org/1999/xlink:from "hoge"))
; <--- of:
sxml:test-xpath+index
"//attribute::xlink:*[ self::* = 'hoge' ]"
vtxp:ns-binding
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"//attribute::xlink:*"
(list (cons 'xlink "http://www.else.com"))
)


;------------------------------------------------
; Axises

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((id "chap3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/@*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((id "toc3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/."
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((id "toc3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/."
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com"))))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/.."
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/.."
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (chapter (@ (id "chap6")) (title "samples"))
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com")))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/following-sibling::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/following-sibling::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine...")))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/preceding-sibling::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/preceding-sibling::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 (p "Thanks a lot.")
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 (title "XML Linking Language (XLink)")
 (ref "foo.com")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 (title "XML Pointing Language (XPointer)")
 (ref "boo.com"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/following::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'("chapter3"
 (item (@ (id "toc4")) "chapter4")
 "chapter4"
 (item (@ (id "toc5")) "chapter5")
 "chapter5"
 (item (@ (id "toc6")) "chapter6")
 "chapter6"
 (item (@ (id "toc7")) "chapter7")
 "chapter7"
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 "Abstract"
 (p "This document describes about XLink Engine...")
 "This document describes about XLink Engine..."
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 "Introduction"
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 "This document is written in XML (eXtensible Markup Language) ver.1.0."
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 "What is XLink?"
 (p "hyperlink")
 "hyperlink"
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 "What is XPointer?"
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 "Models for using XLink/XPointer "
 (p "There are important keywords.")
 "There are important keywords."
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 "samples"
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 "Conclusion"
 (p "Thanks a lot.")
 "Thanks a lot."
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 "Who1"
 (title "XML Linking Language (XLink)")
 "XML Linking Language (XLink)"
 (ref "foo.com")
 "foo.com"
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 "Who2"
 (title "XML Pointing Language (XPointer)")
 "XML Pointing Language (XPointer)"
 (ref "boo.com")
 "boo.com")
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/following::node()"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (p "This document describes about XLink Engine...")
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (item (@ (id "toc7")) "chapter7")
 (item (@ (id "toc6")) "chapter6")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/preceding::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/preceding::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com"))))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/parent::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/parent::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (*TOP*
  (@@
   (id-index
     ("toc1" item (@ (id "toc1")) "chapter1")
     ("toc2" item (@ (id "toc2")) "chapter2")
     ("toc3" item (@ (id "toc3")) "chapter3")
     ("toc4" item (@ (id "toc4")) "chapter4")
     ("toc5" item (@ (id "toc5")) "chapter5")
     ("toc6" item (@ (id "toc6")) "chapter6")
     ("toc7" item (@ (id "toc7")) "chapter7")
     ("chap1"
      chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
     ("chap2"
      chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
     ("sec2-1"
      section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0."))
     ("chap3"
      chapter
      (@ (id "chap3"))
      (title "What is XLink?")
      (p "hyperlink"))
     ("chap4"
      chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
     ("chap5"
      chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
     ("chap6" chapter (@ (id "chap6")) (title "samples"))
     ("chap7"
      chapter
      (@ (id "chap7"))
      (title "Conclusion")
      (p "Thanks a lot."))
     ("here"
      appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))
  (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
  (doc
   (multidirectional
     (@ (http://www.w3.org/1999/xlink:type "extended"))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 toc")
       (http://www.w3.org/1999/xlink:role "booboo")
       (http://www.w3.org/1999/xlink:label "boo")
       (http://www.w3.org/1999/xlink:href "#toc1")))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 cont")
       (http://www.w3.org/1999/xlink:role "text/xml")
       (http://www.w3.org/1999/xlink:label "hoge")
       (http://www.w3.org/1999/xlink:href "#chap1")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "hoge")
       (http://www.w3.org/1999/xlink:title "Traversal to content page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "boo")
       (http://www.w3.org/1999/xlink:actuate "onRequest")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "boo")
       (http://www.w3.org/1999/xlink:title "Traversal to toc page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "hoge")
       (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   (item (@ (id "toc1")) "chapter1")
   (item (@ (id "toc2")) "chapter2")
   (item (@ (id "toc3")) "chapter3")
   (item (@ (id "toc4")) "chapter4")
   (item (@ (id "toc5")) "chapter5")
   (item (@ (id "toc6")) "chapter6")
   (item (@ (id "toc7")) "chapter7")
   (body
    (chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
    (chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
    (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
    (chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
    (chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
    (chapter (@ (id "chap6")) (title "samples"))
    (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
    (appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/ancestor::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (*TOP*
  (@@
   (id-index
     ("toc1" item (@ (id "toc1")) "chapter1")
     ("toc2" item (@ (id "toc2")) "chapter2")
     ("toc3" item (@ (id "toc3")) "chapter3")
     ("toc4" item (@ (id "toc4")) "chapter4")
     ("toc5" item (@ (id "toc5")) "chapter5")
     ("toc6" item (@ (id "toc6")) "chapter6")
     ("toc7" item (@ (id "toc7")) "chapter7")
     ("chap1"
      chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
     ("chap2"
      chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
     ("sec2-1"
      section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0."))
     ("chap3"
      chapter
      (@ (id "chap3"))
      (title "What is XLink?")
      (p "hyperlink"))
     ("chap4"
      chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
     ("chap5"
      chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
     ("chap6" chapter (@ (id "chap6")) (title "samples"))
     ("chap7"
      chapter
      (@ (id "chap7"))
      (title "Conclusion")
      (p "Thanks a lot."))
     ("here"
      appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))
  (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
  (doc
   (multidirectional
     (@ (http://www.w3.org/1999/xlink:type "extended"))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 toc")
       (http://www.w3.org/1999/xlink:role "booboo")
       (http://www.w3.org/1999/xlink:label "boo")
       (http://www.w3.org/1999/xlink:href "#toc1")))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 cont")
       (http://www.w3.org/1999/xlink:role "text/xml")
       (http://www.w3.org/1999/xlink:label "hoge")
       (http://www.w3.org/1999/xlink:href "#chap1")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "hoge")
       (http://www.w3.org/1999/xlink:title "Traversal to content page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "boo")
       (http://www.w3.org/1999/xlink:actuate "onRequest")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "boo")
       (http://www.w3.org/1999/xlink:title "Traversal to toc page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "hoge")
       (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   (item (@ (id "toc1")) "chapter1")
   (item (@ (id "toc2")) "chapter2")
   (item (@ (id "toc3")) "chapter3")
   (item (@ (id "toc4")) "chapter4")
   (item (@ (id "toc5")) "chapter5")
   (item (@ (id "toc6")) "chapter6")
   (item (@ (id "toc7")) "chapter7")
   (body
    (chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
    (chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
    (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
    (chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
    (chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
    (chapter (@ (id "chap6")) (title "samples"))
    (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
    (appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/ancestor::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (*TOP*
  (@@
   (id-index
     ("toc1" item (@ (id "toc1")) "chapter1")
     ("toc2" item (@ (id "toc2")) "chapter2")
     ("toc3" item (@ (id "toc3")) "chapter3")
     ("toc4" item (@ (id "toc4")) "chapter4")
     ("toc5" item (@ (id "toc5")) "chapter5")
     ("toc6" item (@ (id "toc6")) "chapter6")
     ("toc7" item (@ (id "toc7")) "chapter7")
     ("chap1"
      chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
     ("chap2"
      chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
     ("sec2-1"
      section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0."))
     ("chap3"
      chapter
      (@ (id "chap3"))
      (title "What is XLink?")
      (p "hyperlink"))
     ("chap4"
      chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
     ("chap5"
      chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
     ("chap6" chapter (@ (id "chap6")) (title "samples"))
     ("chap7"
      chapter
      (@ (id "chap7"))
      (title "Conclusion")
      (p "Thanks a lot."))
     ("here"
      appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))
  (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
  (doc
   (multidirectional
     (@ (http://www.w3.org/1999/xlink:type "extended"))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 toc")
       (http://www.w3.org/1999/xlink:role "booboo")
       (http://www.w3.org/1999/xlink:label "boo")
       (http://www.w3.org/1999/xlink:href "#toc1")))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 cont")
       (http://www.w3.org/1999/xlink:role "text/xml")
       (http://www.w3.org/1999/xlink:label "hoge")
       (http://www.w3.org/1999/xlink:href "#chap1")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "hoge")
       (http://www.w3.org/1999/xlink:title "Traversal to content page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "boo")
       (http://www.w3.org/1999/xlink:actuate "onRequest")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "boo")
       (http://www.w3.org/1999/xlink:title "Traversal to toc page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "hoge")
       (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   (item (@ (id "toc1")) "chapter1")
   (item (@ (id "toc2")) "chapter2")
   (item (@ (id "toc3")) "chapter3")
   (item (@ (id "toc4")) "chapter4")
   (item (@ (id "toc5")) "chapter5")
   (item (@ (id "toc6")) "chapter6")
   (item (@ (id "toc7")) "chapter7")
   (body
    (chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
    (chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
    (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
    (chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
    (chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
    (chapter (@ (id "chap6")) (title "samples"))
    (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
    (appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/ancestor-or-self::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((id "toc3")
 (item (@ (id "toc3")) "chapter3")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (*TOP*
  (@@
   (id-index
     ("toc1" item (@ (id "toc1")) "chapter1")
     ("toc2" item (@ (id "toc2")) "chapter2")
     ("toc3" item (@ (id "toc3")) "chapter3")
     ("toc4" item (@ (id "toc4")) "chapter4")
     ("toc5" item (@ (id "toc5")) "chapter5")
     ("toc6" item (@ (id "toc6")) "chapter6")
     ("toc7" item (@ (id "toc7")) "chapter7")
     ("chap1"
      chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
     ("chap2"
      chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
     ("sec2-1"
      section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0."))
     ("chap3"
      chapter
      (@ (id "chap3"))
      (title "What is XLink?")
      (p "hyperlink"))
     ("chap4"
      chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
     ("chap5"
      chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
     ("chap6" chapter (@ (id "chap6")) (title "samples"))
     ("chap7"
      chapter
      (@ (id "chap7"))
      (title "Conclusion")
      (p "Thanks a lot."))
     ("here"
      appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))
  (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
  (doc
   (multidirectional
     (@ (http://www.w3.org/1999/xlink:type "extended"))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 toc")
       (http://www.w3.org/1999/xlink:role "booboo")
       (http://www.w3.org/1999/xlink:label "boo")
       (http://www.w3.org/1999/xlink:href "#toc1")))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 cont")
       (http://www.w3.org/1999/xlink:role "text/xml")
       (http://www.w3.org/1999/xlink:label "hoge")
       (http://www.w3.org/1999/xlink:href "#chap1")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "hoge")
       (http://www.w3.org/1999/xlink:title "Traversal to content page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "boo")
       (http://www.w3.org/1999/xlink:actuate "onRequest")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "boo")
       (http://www.w3.org/1999/xlink:title "Traversal to toc page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "hoge")
       (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   (item (@ (id "toc1")) "chapter1")
   (item (@ (id "toc2")) "chapter2")
   (item (@ (id "toc3")) "chapter3")
   (item (@ (id "toc4")) "chapter4")
   (item (@ (id "toc5")) "chapter5")
   (item (@ (id "toc6")) "chapter6")
   (item (@ (id "toc7")) "chapter7")
   (body
    (chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
    (chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
    (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
    (chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
    (chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
    (chapter (@ (id "chap6")) (title "samples"))
    (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
    (appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/ancestor-or-self::*"
)


;------------------------------------------------
; position() and last() functions

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"*/*[position()=2]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc4")) "chapter4"))
; <--- of:
sxml:test-xpath+index
"*/*[position()=last()-4]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc6")) "chapter6"))
; <--- of:
sxml:test-xpath+index
"*/*[position()>=4 and position()<last()-1]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
sxml:test-xpath+index
"*/*[9]/*[position()=last()-position()]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
sxml:test-xpath+index
"*/*[9]/*[position()=4]"
)

;DL, 05.02.04: new test cases recently added for position-based location paths

(xtest-assert ; Expected result:
'((p "hyperlink")
 (p "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (p "There are important keywords."))
; <--- of:
sxml:test-xpath+index
"doc/body/chapter[position()>=3 and position()<=5]/p[1]"
)

(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2")
 (item (author "Who2") (title "XML Pointing Language (XPointer)") (ref "boo.com")))
; <--- of:
sxml:test-xpath+index
"//item[2]"
)


;------------------------------------------------
; Predicates

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest"))))
; <--- of:
sxml:test-xpath+index
"*[1]/*[1]/*[attribute::xlink:*][attribute::* = 'boo']"
vtxp:ns-binding
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
sxml:test-xpath+index
"descendant-or-self::node()[self::chapter]\r\n                               [self::* = id(' chap1 chap3 chap6 chap7   ')]\r\n                               [*[1] = 'What is XLink?']"
)


;=========================================================================
; SXPointer testing

;------------------------------------------------
; Arithmetic operations

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'20
; <--- of:
sxml:test-xpointer+index
"xpointer( -2 + 4* (6-1) + 6 div 3 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( 7>4 and not( false() ) and ('err'= 12 or true() ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( true() > false() )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( '  15 ' = 15 = true() )"
)


;------------------------------------------------
; Variable reference

; sxml:test-xpointer+root+vars
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+root+vars
"xpointer(   $x and true() )"
(list (cons 'x #t))
)

; sxml:test-xpointer+root+vars
(xtest-assert ; Expected result:
'41
; <--- of:
sxml:test-xpointer+root+vars
"xpointer(   $y - 4)"
(list (cons 'y 45) (cons 'z '()))
)

; sxml:test-xpointer+root+vars
(xtest-assert ; Expected result:
'"variable value"
; <--- of:
sxml:test-xpointer+root+vars
"xpointer(   $z )"
(list (cons 'z "variable value"))
)

; sxml:test-xpointer+root+vars
(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2"))
; <--- of:
sxml:test-xpointer+root+vars
"xpointer(   $var[2] | id('toc2') )"
(list (cons 'var '()))
)


;------------------------------------------------
; Datatype convertion

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-34
; <--- of:
sxml:test-xpointer+index
"xpointer( number(' -34 ') )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-34.67
; <--- of:
sxml:test-xpointer+index
"xpointer( number(' -34.67 ') )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'1
; <--- of:
sxml:test-xpointer+index
"xpointer( number( true() ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'0
; <--- of:
sxml:test-xpointer+index
"xpointer( number(false()) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( boolean( -56 ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( boolean( 0 ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( boolean( 'ere' ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( boolean( '' ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'"0"
; <--- of:
sxml:test-xpointer+index
"xpointer( string( 0 ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'"-76"
; <--- of:
sxml:test-xpointer+index
"xpointer( string( -76 ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'"true"
; <--- of:
sxml:test-xpointer+index
"xpointer( string( true() ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'"false"
; <--- of:
sxml:test-xpointer+index
"xpointer( string( false() ) )"
)


;------------------------------------------------
; Core XPath function library

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'9
; <--- of:
sxml:test-xpointer+index
"xpointer( count( //item ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'0
; <--- of:
sxml:test-xpointer+index
"xpointer( count( id('no_such_element') ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'0
; <--- of:
sxml:test-xpointer+index
"xpointer( floor(.4) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-3
; <--- of:
sxml:test-xpointer+index
"xpointer( floor(-2.5) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'1
; <--- of:
sxml:test-xpointer+index
"xpointer( ceiling(.4) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-2
; <--- of:
sxml:test-xpointer+index
"xpointer( ceiling(-2.5) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'0
; <--- of:
sxml:test-xpointer+index
"xpointer( round(.4) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'5
; <--- of:
sxml:test-xpointer+index
"xpointer( round(4.51) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-3
; <--- of:
sxml:test-xpointer+index
"xpointer( round(-2.51) )"
)


;------------------------------------------------
; Comparison operations (for simple datatypes)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( false() = 12 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( true() = 12 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( true() != 0 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( false() = '' )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( false() = 'smth' )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( true() != 'smth' )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( '12  ' = 12 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( '123  ' != 12 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( true() > 0 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( false() < 1 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( '  12' <= 1 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( '  12' >= '  -2 ' )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( 'ee' <= 1 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( 'aaa' < 'bbb' )"
)


;------------------------------------------------
; Union operation

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((item (@ (id "toc6")) "chapter6")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0."))))
; <--- of:
sxml:test-xpointer+index
"xpointer( *[1]/*[7] | *[1]/*[9]/*[2] )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com")))))
; <--- of:
sxml:test-xpointer+index
"xpointer( id('toc2') | id('chap4') | id('here') )"
)


;------------------------------------------------
; Nodeset comparison

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( */*[9]/*[5]/following::node() = 'ConclusionThanks a lot.')"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( */*[9]/*[5]/following::node() = //appendix//item[1] )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( */*[9]/*[5]/following::node() = \r\n                    */*[9]/*[6]/preceding-sibling::node() )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( (/descendant::text()[23] | /descendant::text()[24]) = \r\n\t\t( //*[self::node() = 'foo.com'] | *[1]/*[1] ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( //xlink:label[self::* = 'hoge'] > id( 'chap6' ) )"
vtxp:ns-binding
)


;------------------------------------------------
; Child sequence

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com")))))
; <--- of:
sxml:test-xpointer+index
"here"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com")))
; <--- of:
sxml:test-xpointer+index
"here/1/1"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpointer+index
"toc1/2"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest"))))
; <--- of:
sxml:test-xpointer+index
"/1/1/3"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com")))
; <--- of:
sxml:test-xpointer+index
"/1/9/8/1/2"
)


;------------------------------------------------
; xmlns usage

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "extended")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "arc")
 (http://www.w3.org/1999/xlink:type "arc"))
; <--- of:
sxml:test-xpointer+index
"xmlns( xl = http://www.w3.org/1999/xlink) \r\n     xpointer( //attribute::xl:type )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:from "boo")
 (http://www.w3.org/1999/xlink:to "boo"))
; <--- of:
sxml:test-xpointer+index
"xmlns( xl = http://www.w3.org/1999/xlink) \r\n     xpointer( descendant::node()/attribute::xl:*[self::node() = 'boo'] )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpointer+index
"xmlns( smth = http://smth ) \r\n     xpointer( descendant::node()/attribute::smth:*[self::node() = 'boo'] )"
)


;------------------------------------------------
; Several XPointer parts

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((title "Abstract")
 (title "Introduction")
 (title "What is XLink?")
 (title "What is XPointer?")
 (title "Models for using XLink/XPointer ")
 (title "samples")
 (title "Conclusion"))
; <--- of:
sxml:test-xpointer+index
"xpointer( doc/multidirectional/loc[3] )\r\n     xpointer( doc/body/chapter/title )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:from "boo")
 (http://www.w3.org/1999/xlink:to "boo"))
; <--- of:
sxml:test-xpointer+index
"xpointer( doc/multidirectional/loc[3] )\r\n     xmlns( xlink=http://www.w3.org/1999/xlink)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:*[self::* = 'boo'] )\r\n     xpointer( doc/body/chapter/title )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:actuate "onRequest")
 (http://www.w3.org/1999/xlink:actuate "onRequest"))
; <--- of:
sxml:test-xpointer+index
"xmlns( xlink=http://first_try)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:actuate )\r\n     xmlns( xlink=http://second_try)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:actuate )\r\n     xmlns( xlink=http://www.w3.org/1999/xlink)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:actuate )\r\n     xpointer( false() )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((item (@ (id "toc7")) "chapter7"))
; <--- of:
sxml:test-xpointer+index
"xpointer( id( id('toc4')/text() ) )\r\n     xpointer( id( 'toc4' )/following-sibling::*[self::* = id('toc7')] )\r\n     xpointer( false() )"
)

; sxml:xpath
(xtest-assert ; Expected result:
(list vtxp:doc)
; <--- of:
(sxml:xpath "/")
vtxp:doc
)


; sxml:xpath+root+vars
(xtest-assert ; Expected result:
(list vtxp:doc)
;"dgdgf"
; <--- of:
(sxml:xpath+root+vars "/")
vtxp:doc
'()
)

(cout nl "TXPath tests passed successfully!" nl)
)

(define (run-sxpath-test)
  (run-sxpathlib-test)
  (run-sxpathext-test)
  (run-txpath-test)
  )
)