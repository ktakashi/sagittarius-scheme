(import (rnrs)
	(text json jmespath)
	(srfi :64))

(test-begin "JMESPath")

;; non builtin 
(test-equal '(#(("a" . #(("foo" . 1))) ("b" . #(("foo" . 1))))
	      #(("a" . #(("foo" . 1))) ("b" . #(("foo" . 1)))))
	    ((jmespath "*.foo.parent(@).parent(@)")
	     '#(("a" . #(("foo" . 1)))
		("b" . #(("foo" . 1))))))

(test-equal '()
	    ((jmespath "*.foo.parent(@).parent(@).parent(@)")
	     '#(("a" . #(("foo" . 1)))
		("b" . #(("foo" . 1))))))

(test-equal 'null
	    ((jmespath "a.foo.parent(@).parent(@).parent(@)")
	     '#(("a" . #(("foo" . 1)))
		("b" . #(("foo" . 1))))))

(test-end)
