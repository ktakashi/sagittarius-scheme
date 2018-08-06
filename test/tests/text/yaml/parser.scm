(import (rnrs)
	(text yaml parser)
	(text yaml nodes)
	(srfi :127)
	(srfi :39)
	(srfi :64))

(test-begin "YAML")

(define (test-yaml-parser expected input)
  (test-equal input expected
	      (map yaml-document->sexp
		   (parse-yaml (open-string-input-port input)))))

(test-yaml-parser '((*yaml*
		     (*directives* (%YAML 1 1))
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" "foo")
		       ("tag:yaml.org,2002:seq"
			#(("tag:yaml.org,2002:str" "bar")
			  ("tag:yaml.org,2002:int" "1234")
			  ("tag:yaml.org,2002:null" "~")))))))
		  "%YAML 1.1\n\
                   ---\n\
                   foo:\n  \
                     - bar\n  \
                     - 1234\n  \
                     - ~")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:seq"
		      #(("tag:yaml.org,2002:str" "bar")))))
		  "  - bar")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:seq"
		      #(("tag:yaml.org,2002:str" "foo")))))
		  "[ foo ]")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:seq"
		      #(("tag:yaml.org,2002:str" "foo")
			("tag:yaml.org,2002:str" "bar")))))
		  "[ foo, bar ]")
;; extra comma
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:seq"
		      #(("tag:yaml.org,2002:str" "foo")))))
		  "[ foo, ]")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:seq"
		      #(("tag:yaml.org,2002:str" "foo")
			("tag:yaml.org,2002:str" "bar")))))
		  "[ foo, bar, ]")

;; mapping
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" "foo")
		       ("tag:yaml.org,2002:str" "bar")))))
		  "{ foo: bar }")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" "foo")
		       ("tag:yaml.org,2002:str" "bar"))
		      (("tag:yaml.org,2002:str" "buz")
		       ("tag:yaml.org,2002:str" "bla")))))
		  "{ foo: bar, buz: bla }")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" "foo")
		       ("tag:yaml.org,2002:str" "bar")))))
		  "{ foo: bar, }")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" "foo")
		       ("tag:yaml.org,2002:str" "bar"))
		      (("tag:yaml.org,2002:str" "buz")
		       ("tag:yaml.org,2002:str" "bla")))))
		  "{ foo: bar, buz: bla, }")
(test-end)

