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

(test-end)
