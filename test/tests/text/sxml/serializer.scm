;; -*- mode: scheme; coding: utf-8 -*-
#!nobacktrace
(import (rnrs)
	(text sxml serializer)
	(srfi :64))

;; not so good test cases.
    
(test-begin "(run-serializer-test)")
(let ((testdata '(foo (@ (a "b") (c "d\"e'f")) (g "h" (i "j" (k))))))
  (let-syntax
      ((t (syntax-rules () ((t p r) (test-equal 'p r (p testdata))))))
    (t srl:sxml->xml
       "<foo a=\"b\" c=\"d&quot;e&apos;f\">\n  <g>h<i>j<k /></i></g>\n</foo>")
    (t srl:sxml->xml-noindent
       "<foo a=\"b\" c=\"d&quot;e&apos;f\"><g>h<i>j<k /></i></g></foo>")
    (t srl:sxml->html
       "<foo a=\"b\" c=\"d&quot;e&apos;f\">\n  <g>h<i>j<k /></i></g>\n</foo>")
    (t srl:sxml->html-noindent
       "<foo a=\"b\" c=\"d&quot;e&apos;f\"><g>h<i>j<k /></i></g></foo>")
    ))
(test-end)