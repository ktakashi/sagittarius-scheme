(import (rnrs)
	(text yaml)
	(srfi :64))

(test-begin "YAML")

(test-equal '(#(("foo" . "bar")
		("boo" 1 2 3 4)))
	    (yaml-read (open-string-input-port "
%YAML 1.2
---
foo: bar
boo:
- 1
- 2
- 3
- 4")))

(test-equal "%YAML 1.2\n---\nfoo: bar\nboo: \n- 1\n- 2\n- 3\n- 4\n...\n"
	    (let-values (((out extract) (open-string-output-port)))
	      (yaml-write '(#(("foo" . "bar")
			      ("boo" 1 2 3 4))) out)
	      (extract)))
(test-error (yaml-write '#(("key" . 1))))
(test-end)
