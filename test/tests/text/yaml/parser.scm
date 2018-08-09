(import (rnrs)
	(text yaml parser)
	(text yaml nodes)
	(srfi :127)
	(srfi :39)
	(srfi :64))

(test-begin "YAML")

(define (test-yaml-parser expected input)
  (test-equal input expected
	      (map yaml-document->canonical-sexp
		   (parse-yaml (open-string-input-port input))))
  (test-equal input expected
	      (map yaml-document->canonical-sexp
		   (map canonical-sexp->yaml-document
			(map yaml-document->canonical-sexp
			     (parse-yaml (open-string-input-port input)))))))

(test-yaml-parser '((*yaml*
		     (*directives* (%YAML 1 1))
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "foo")
		       #("tag:yaml.org,2002:seq"
			 ("tag:yaml.org,2002:str" . "bar")
			 ("tag:yaml.org,2002:int" . "1234")
			 ("tag:yaml.org,2002:null" . "~"))))))
		  "%YAML 1.1\n\
                   ---\n\
                   foo:\n  \
                     - bar\n  \
                     - 1234\n  \
                     - ~")

(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:str" . "bar"))))
		  "  - bar")

(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:str" . "foo"))))
		  "[ foo ]")
(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:str" . "foo")
		       ("tag:yaml.org,2002:str" . "bar"))))
		  "[ foo, bar ]")
;; extra comma
(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:str" . "foo"))))
		  "[ foo, ]")
(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:str" . "foo")
		       ("tag:yaml.org,2002:str" . "bar"))))
		  "[ foo, bar, ]")

;; mapping
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "foo")
		       ("tag:yaml.org,2002:str" . "bar")))))
		  "{ foo: bar }")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "foo")
		       ("tag:yaml.org,2002:str" . "bar"))
		      (("tag:yaml.org,2002:str" . "buz")
		       ("tag:yaml.org,2002:str" . "bla")))))
		  "{ foo: bar, buz: bla }")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "foo")
		       ("tag:yaml.org,2002:str" . "bar")))))
		  "{ foo: bar, }")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "foo")
		       ("tag:yaml.org,2002:str" . "bar"))
		      (("tag:yaml.org,2002:str" . "buz")
		       ("tag:yaml.org,2002:str" . "bla")))))
		  "{ foo: bar, buz: bla, }")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "foo")
		       ("tag:yaml.org,2002:null" . "")))))
		  "foo:")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:null" . "")
		       ("tag:yaml.org,2002:null" . "")))))
		  "?")
(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "foo")
		       ("tag:yaml.org,2002:str" . "bar")))))
		  "---\n\
                   !!map {\n  \
                     ? !!str \"foo\"\n  \
                     : !!str \"bar\",\n\
                   }")

(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:map"
			(("tag:yaml.org,2002:str" . "one")
			 ("tag:yaml.org,2002:int" . "1")))
		       ("tag:yaml.org,2002:map"
			(("tag:yaml.org,2002:str" . "two")
			 ("tag:yaml.org,2002:int" . "2")))
		       ("tag:yaml.org,2002:map"
			(("tag:yaml.org,2002:str" . "three")
			 ("tag:yaml.org,2002:int" . "3"))))))
		  "[ one: 1, two: 2, three : 3 ]")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		       (("tag:yaml.org,2002:str" . "one")
			("tag:yaml.org,2002:null" . "")))))
		  "{ one: , }")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		       (("tag:yaml.org,2002:str" . "generic")
			("tag:yaml.org,2002:binary" .
			 "R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
+f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=")))))
		  "generic: !!binary |
 R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
 OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
 +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
 AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=")

;; corner cases
;; we support null scalar only on explicit document (differ from PyYAML)
(test-yaml-parser '() "")
(test-yaml-parser '((*yaml* ("tag:yaml.org,2002:null" . ""))) "---")


;; resolver tests
;; float
(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:float" . "1.2")
		       ("tag:yaml.org,2002:float" . "6.8523015e+5")
		       ("tag:yaml.org,2002:float" . "685.230_15e+03")
		       ("tag:yaml.org,2002:float" . "685_230.15")
		       ("tag:yaml.org,2002:float" . "190:20:30.15")
		       ("tag:yaml.org,2002:float" . "+.inf")
		       ("tag:yaml.org,2002:str" . "+.InF")
		       ("tag:yaml.org,2002:float" . ".nan")
		       ("tag:yaml.org,2002:str" . ".Nan"))))
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
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:bool" . "ON")
		       ("tag:yaml.org,2002:bool" . "OFF")
		       ("tag:yaml.org,2002:str" . "y")
		       ("tag:yaml.org,2002:str" . "n")
		       ("tag:yaml.org,2002:bool" . "Yes")
		       ("tag:yaml.org,2002:bool" . "No")
		       ("tag:yaml.org,2002:bool" . "True")
		       ("tag:yaml.org,2002:bool" . "False")
		       ("tag:yaml.org,2002:str"  . "TrUe"))))
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
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:int" . "685230")
		       ("tag:yaml.org,2002:int" . "+685_230")
		       ("tag:yaml.org,2002:int" . "02472256")
		       ("tag:yaml.org,2002:int" . "0x_0A_74_AE")
		       ("tag:yaml.org,2002:int" . "0b1010_0111_0100_1010_1110")
		       ("tag:yaml.org,2002:int" . "190:20:30"))))
		  "- 685230\n\
                   - +685_230\n\
                   - 02472256\n\
                   - 0x_0A_74_AE\n\
                   - 0b1010_0111_0100_1010_1110\n\
                   - 190:20:30")

(test-yaml-parser '((*yaml* ("tag:yaml.org,2002:merge" . "<<"))) "<<")

(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:null" . "~")
		       ("tag:yaml.org,2002:null" . "null")
		       ("tag:yaml.org,2002:null" . "")
		       ("tag:yaml.org,2002:null" . "Null")
		       ("tag:yaml.org,2002:null" . "NULL")
		       ("tag:yaml.org,2002:str" . "NuLL"))))
		  "- ~\n\
                   - null\n\
                   - \n\
                   - Null\n\
                   - NULL\n\
                   - NuLL")

(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:seq"
		       ("tag:yaml.org,2002:timestamp" .
			"2001-12-15T02:59:43.1Z")
		       ("tag:yaml.org,2002:timestamp" .
			"2001-12-14t21:59:43.10-05:00")
		       ("tag:yaml.org,2002:timestamp" .
			"2001-12-14 21:59:43.10 -5")
		       ("tag:yaml.org,2002:timestamp" . "2001-12-15 2:59:43.10")
		       ("tag:yaml.org,2002:timestamp" . "2002-12-14"))))
		  "- 2001-12-15T02:59:43.1Z\n\
                   - 2001-12-14t21:59:43.10-05:00\n\
                   - 2001-12-14 21:59:43.10 -5\n\
                   - 2001-12-15 2:59:43.10\n\
                   - 2002-12-14")

(test-yaml-parser '((*yaml* ("tag:yaml.org,2002:value" . "="))) "=")

;; omap etc.
(test-yaml-parser '((*yaml*
		     (*directives* (%YAML 1 2))
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "foo")
		       #("tag:yaml.org,2002:omap"
			 ("tag:yaml.org,2002:map"
			  (("tag:yaml.org,2002:str" . "foo")
			   ("tag:yaml.org,2002:str" . "bar")))
			 ("tag:yaml.org,2002:map"
			  (("tag:yaml.org,2002:str" . "boo")
			   ("tag:yaml.org,2002:str" . "buz"))))))))
		  "%YAML 1.2\n\
                   ---\n\
                   foo: !!omap\n\
                    - foo: bar\n\
                    - boo: buz")

(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:omap"
		       ("tag:yaml.org,2002:map"
			(("tag:yaml.org,2002:str" . "one")
			 ("tag:yaml.org,2002:int" . "1")))
		       ("tag:yaml.org,2002:map"
			(("tag:yaml.org,2002:str" . "two")
			 ("tag:yaml.org,2002:int" . "2")))
		       ("tag:yaml.org,2002:map"
			(("tag:yaml.org,2002:str" . "three")
			 ("tag:yaml.org,2002:int" . "3"))))))
		  "!!omap [ one: 1, two: 2, three : 3 ]")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "Block tasks")
		       #("tag:yaml.org,2002:pairs"
			 ("tag:yaml.org,2002:map"
			  (("tag:yaml.org,2002:str" . "meeting")
			   ("tag:yaml.org,2002:str" . "with team.")))
			 ("tag:yaml.org,2002:map"
			  (("tag:yaml.org,2002:str" . "meeting")
			   ("tag:yaml.org,2002:str" . "with boss.")))
			 ("tag:yaml.org,2002:map"
			  (("tag:yaml.org,2002:str" . "break")
			   ("tag:yaml.org,2002:str" . "lunch.")))
			 ("tag:yaml.org,2002:map"
			  (("tag:yaml.org,2002:str" . "meeting")
			   ("tag:yaml.org,2002:str" . "with client."))))))))
		  "Block tasks: !!pairs\n  \
                     - meeting: with team.\n  \
                     - meeting: with boss.\n  \
                     - break: lunch.\n  \
                     - meeting: with client.")

(test-yaml-parser '((*yaml*
		     #("tag:yaml.org,2002:pairs"
		       ("tag:yaml.org,2002:map"
			(("tag:yaml.org,2002:str" . "meeting")
			 ("tag:yaml.org,2002:str" . "with team")))
		       ("tag:yaml.org,2002:map"
			(("tag:yaml.org,2002:str" . "meeting")
			 ("tag:yaml.org,2002:str" . "with boss"))))))
		  "!!pairs [ meeting: with team, meeting: with boss ]")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:map"
		      (("tag:yaml.org,2002:str" . "baseball players")
		       ("tag:yaml.org,2002:set"
			(("tag:yaml.org,2002:str" . "Mark McGwire")
			 ("tag:yaml.org,2002:null" . ""))
			(("tag:yaml.org,2002:str" . "Sammy Sosa")
			 ("tag:yaml.org,2002:null" . ""))
			(("tag:yaml.org,2002:str" . "Ken Griffey")
			 ("tag:yaml.org,2002:null" . "")))))))
		  "baseball players: !!set\n  \
                     ? Mark McGwire\n  \
                     ? Sammy Sosa\n  \
                     ? Ken Griffey")

(test-yaml-parser '((*yaml*
		     ("tag:yaml.org,2002:set"
		      (("tag:yaml.org,2002:str" . "Boston Red Sox")
		       ("tag:yaml.org,2002:null" . ""))
		      (("tag:yaml.org,2002:str" . "Detroit Tigers")
		       ("tag:yaml.org,2002:null" . ""))
		      (("tag:yaml.org,2002:str" . "New York Yankees")
		       ("tag:yaml.org,2002:null" . "")))))
		  "!!set { Boston Red Sox, Detroit Tigers, New York Yankees }")
;; TODO test parser errors

(test-end)

