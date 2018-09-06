(import (rnrs)
	(text yaml)
	(srfi :13)
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

(test-equal "%YAML 1.2\n---\nfoo: bar\nboo: \n  - 1\n  - 2\n  - 3\n  - 4\n...\n"
	    (let-values (((out extract) (open-string-output-port)))
	      (yaml-write '(#(("foo" . "bar")
			      ("boo" 1 2 3 4))) out)
	      (extract)))
(test-error (yaml-write '#(("key" . 1))))

(let ((yaml '#(("boo" .
		#(("bar" . #f)
		  ("foo" . "aa\nbb\n\tcc")
		  ("buz" . 1))))))
  (let-values (((out e) (open-string-output-port)))
    (yaml-write (list yaml) out)
    (let ((s (e)))
      (test-assert (string-contains s "|-"))
      (test-equal yaml (car (yaml-read (open-string-input-port s)))))))
      
(test-end)
