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

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" "foo")
		       ("tag:yaml.org,2002:null" "")))))
		  "foo:")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:null" "")
		       ("tag:yaml.org,2002:null" "")))))
		  "?")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" "foo")
		       ("tag:yaml.org,2002:str" "bar")))))
		  "---\n\
                   !!map {\n  \
                     ? !!str \"foo\"\n  \
                     : !!str \"bar\",\n\
                   }")
;; corner cases
;; we support null scalar only on explicit document (differ from PyYAML)
(test-yaml-parser '() "")
(test-yaml-parser '((*yaml* ("tag:yaml.org,2002:null" ""))) "---")


;; resolver tests
;; float
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:seq"
		      #(("tag:yaml.org,2002:float" "1.2")
			("tag:yaml.org,2002:float" "6.8523015e+5")
			("tag:yaml.org,2002:float" "685.230_15e+03")
			("tag:yaml.org,2002:float" "685_230.15")
			("tag:yaml.org,2002:float" "190:20:30.15")
			("tag:yaml.org,2002:float" "+.inf")
			("tag:yaml.org,2002:str" "+.InF")
			("tag:yaml.org,2002:float" ".nan")
			("tag:yaml.org,2002:str" ".Nan")))))
		  "- 1.2\n\
                   - 6.8523015e+5\n\
                   - 685.230_15e+03\n\
                   - 685_230.15\n\
                   - 190:20:30.15\n\
                   - +.inf\n\
                   - +.InF # non fload\n\
                   - .nan\n\
                   - .Nan # non float")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:seq"
		      #(("tag:yaml.org,2002:bool" "ON")
			("tag:yaml.org,2002:bool" "OFF")
			("tag:yaml.org,2002:bool" "y")
			("tag:yaml.org,2002:bool" "n")
			("tag:yaml.org,2002:bool" "Yes")
			("tag:yaml.org,2002:bool" "No")
			("tag:yaml.org,2002:bool" "True")
			("tag:yaml.org,2002:bool" "False")
			("tag:yaml.org,2002:str"  "TrUe")))))
		  "- ON\n\
                   - OFF\n\
                   - y\n\
                   - n\n\
                   - Yes\n\
                   - No\n\
                   - True\n\
                   - False\n\
                   - TrUe")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:seq"
		      #(("tag:yaml.org,2002:int" "685230")
			("tag:yaml.org,2002:int" "+685_230")
			("tag:yaml.org,2002:int" "02472256")
			("tag:yaml.org,2002:int" "0x_0A_74_AE")
			("tag:yaml.org,2002:int" "0b1010_0111_0100_1010_1110")
			("tag:yaml.org,2002:int" "190:20:30")))))
		  "- 685230\n\
                   - +685_230\n\
                   - 02472256\n\
                   - 0x_0A_74_AE\n\
                   - 0b1010_0111_0100_1010_1110\n\
                   - 190:20:30")

(test-yaml-parser '((*yaml* ("tag:yaml.org,2002:merge" "<<"))) "<<")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:seq"
		      #(("tag:yaml.org,2002:null" "~")
			("tag:yaml.org,2002:null" "null")
			("tag:yaml.org,2002:null" "")
			("tag:yaml.org,2002:null" "Null")
			("tag:yaml.org,2002:null" "NULL")
			("tag:yaml.org,2002:str" "NuLL")))))
		  "- ~\n\
                   - null\n\
                   - \n\
                   - Null\n\
                   - NULL\n\
                   - NuLL")

(test-end)

