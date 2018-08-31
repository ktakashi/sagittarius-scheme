(import (rnrs)
	(text yaml writer)
	(text yaml parser)
	(text yaml builder)
	(srfi :64))

(test-begin "YAML writer")

(define (test-writer sexp)
  (let-values (((out extract) (open-string-output-port)))
    (test-assert (emit-yaml out sexp))
    (let ((yaml (car (parse-yaml (open-string-input-port (extract))))))
      (test-equal sexp (yaml->sexp yaml)))))

(define (test-read/write str)
  (let ((sexp (yaml->sexp (car (parse-yaml (open-string-input-port str))))))
    (let-values (((out extract) (open-string-output-port)))
      (test-assert (emit-yaml out sexp))
      (let ((yaml (car (parse-yaml (open-string-input-port (extract))))))
	(test-equal sexp (yaml->sexp yaml))))))

(test-writer '#(("foo"
		 ("just" "write some")
		 ("yaml"
		  (("here" "and")
		   #(("it" . "updates") ("in" . "real-time")))))))

(test-read/write "%YAML 1.2
---
foo: !!omap
- just: write some
- yaml: 
  - [here, and]
  - {it: updates, in: real-time}
")

(test-read/write "%YAML 1.2
---
? - foo
  - boo
: - bar
  - - buz
    - bla
")

(test-read/write "%YAML 1.2
---
foo: &key [ foo, bar ]
bar: &value [ bar, buz ]
*key : *value
")

(test-read/write "%YAML 1.2
---
foo: &key [ foo, bar ]
*key : bar
")

(test-read/write "%YAML 1.2
---
foo: false
bar: false
")

(test-end)
