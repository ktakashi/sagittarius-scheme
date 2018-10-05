(import (rnrs)
	(text yaml writer)
	(text yaml parser)
	(text yaml builder)
	(srfi :64))

(test-begin "YAML writer")

(define (test-writer sexp)
  (let-values (((out extract) (open-string-output-port)))
    (test-assert (list "test-writer" sexp) (emit-yaml out sexp))
    (let* ((written (extract))
	   (yaml (car (parse-yaml (open-string-input-port written)))))
      (test-equal (list "test-writer" sexp) sexp (yaml->sexp yaml)))))

(define (test-read/write str)
  (let ((sexp (yaml->sexp (car (parse-yaml (open-string-input-port str))))))
    (let-values (((out extract) (open-string-output-port)))
      (test-assert (list "test-read/write" str) (emit-yaml out sexp))
      (let* ((written (extract))
	     (yaml (car (parse-yaml (open-string-input-port written)))))
	(test-equal (list "test-read/write" str) sexp (yaml->sexp yaml))))))

(test-writer '#(("foo"
		 ("just" "write some")
		 ("yaml"
		  (("here" "and")
		   #(("it" . "updates") ("in" . "real-time")))))))

(test-writer '#(("foo" . #(("key" . 2) ("key2" . 2)))
		("bar" . #(("key" . 2) ("key2" . 2)))))

(test-writer '#(("foo" . null)))

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

(test-read/write "%YAML 1.2
---
shared:
")

(test-end)
