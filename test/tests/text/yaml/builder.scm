(import (rnrs)
	(text yaml builder)
	(text yaml nodes)
	(text yaml tags)
	(text yaml parser)
	(rfc base64)
	(srfi :19)
	(srfi :64))

(test-begin "YAML builder")

(define (->yaml sexp . builders)
  (apply yaml->sexp (canonical-sexp->yaml-node sexp) builders))

(test-equal 'null (->yaml `(,+yaml-tag:null+ . "")))
(test-equal 'null (->yaml `(,+yaml-tag:null+ . "null")))
;; value doesn't matter on null tag
(test-equal 'null (->yaml `(,+yaml-tag:null+ . "string")))

(test-equal "string" (->yaml `(,+yaml-tag:str+ . "string")))
(test-equal "=" (->yaml `(,+yaml-tag:value+ . "=")))

(test-equal #vu8(1 2 3 4)
	    (->yaml `(,+yaml-tag:binary+
		      . ,(utf8->string (base64-encode #vu8(1 2 3 4))))))

;; (test-equal #t (->yaml `(,+yaml-tag:bool+ . "y")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "yes")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "Yes")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "true")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "True")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "On")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "on")))
;; (test-equal #f (->yaml `(,+yaml-tag:bool+ . "n")))
(test-equal #f (->yaml `(,+yaml-tag:bool+ . "no")))
(test-equal #f (->yaml `(,+yaml-tag:bool+ . "No")))
(test-equal #f (->yaml `(,+yaml-tag:bool+ . "false")))
(test-equal #f (->yaml `(,+yaml-tag:bool+ . "False")))
(test-equal #f (->yaml `(,+yaml-tag:bool+ . "off")))
(test-equal #f (->yaml `(,+yaml-tag:bool+ . "Off")))

(test-equal 685230 (->yaml `(,+yaml-tag:int+ . "685230")))
(test-equal 685230 (->yaml `(,+yaml-tag:int+ . "+685_230")))
(test-equal 685230 (->yaml `(,+yaml-tag:int+ . "02472256")))
(test-equal 685230 (->yaml `(,+yaml-tag:int+ . "0x_0A_74_AE")))
(test-equal 685230 (->yaml `(,+yaml-tag:int+ . "0b1010_0111_0100_1010_1110")))
(test-equal 685230 (->yaml `(,+yaml-tag:int+ . "190:20:30")))

(test-equal -685230 (->yaml `(,+yaml-tag:int+ . "-685230")))
(test-equal -685230 (->yaml `(,+yaml-tag:int+ . "-685_230")))
(test-equal -685230 (->yaml `(,+yaml-tag:int+ . "-02472256")))
(test-equal -685230 (->yaml `(,+yaml-tag:int+ . "-0x_0A_74_AE")))
(test-equal -685230 (->yaml `(,+yaml-tag:int+ . "-0b1010_0111_0100_1010_1110")))
(test-equal -685230 (->yaml `(,+yaml-tag:int+ . "-190:20:30")))

(test-equal 685230.15 (->yaml `(,+yaml-tag:float+ . "6.8523015e+5")))
(test-equal 685230.15 (->yaml `(,+yaml-tag:float+ . "685.230_15e+3")))
(test-equal 685230.15 (->yaml `(,+yaml-tag:float+ . "685_230.15")))
(test-equal 685230.15 (->yaml `(,+yaml-tag:float+ . "190:20:30.15")))
(test-equal +inf.0 (->yaml `(,+yaml-tag:float+ . "+.inf")))
(test-equal +inf.0 (->yaml `(,+yaml-tag:float+ . ".inf")))
(test-equal +inf.0 (->yaml `(,+yaml-tag:float+ . ".INF")))
(test-equal +inf.0 (->yaml `(,+yaml-tag:float+ . ".Inf")))
(test-equal +nan.0 (->yaml `(,+yaml-tag:float+ . ".nan")))
(test-equal +nan.0 (->yaml `(,+yaml-tag:float+ . ".NaN")))
(test-equal +nan.0 (->yaml `(,+yaml-tag:float+ . ".NAN")))

(test-equal -685230.15 (->yaml `(,+yaml-tag:float+ . "-6.8523015e+5")))
(test-equal -685230.15 (->yaml `(,+yaml-tag:float+ . "-685.230_15e+3")))
(test-equal -685230.15 (->yaml `(,+yaml-tag:float+ . "-685_230.15")))
(test-equal -685230.15 (->yaml `(,+yaml-tag:float+ . "-190:20:30.15")))
(test-equal -inf.0 (->yaml `(,+yaml-tag:float+ . "-.inf")))
(test-equal -inf.0 (->yaml `(,+yaml-tag:float+ . "-.INF")))
(test-equal -inf.0 (->yaml `(,+yaml-tag:float+ . "-.Inf")))

(define (date=? d1 d2) (time=? (date->time-utc d1) (date->time-utc d2)))
(define (test-yaml-date d str)
  (let ((s (->yaml `(,+yaml-tag:timestamp+ . ,str))))
    (test-equal (date->yaml-canonical-date d)  s)
    (test-assert (date=? d (->yaml `(,+yaml-tag:timestamp+ . ,s)
				   +scheme-object-yaml-builders+))))
  (test-assert (date=? d (->yaml `(,+yaml-tag:timestamp+ . ,str)
				 +scheme-object-yaml-builders+))))

(test-yaml-date (make-date 0 0 0 0 14 12 2002 0) "2002-12-14")
(test-yaml-date (make-date 100000000 43 59 2 15 12 2001 0)
		"2001-12-15T02:59:43.1Z")
(test-yaml-date (make-date 100000000 43 59 2 15 12 2001 3600)
		"2001-12-15T02:59:43.1+01:00")
(test-yaml-date (make-date 100000000 43 59 2 15 12 2001 3600)
		"2001-12-15t02:59:43.1+01:00")
(test-yaml-date (make-date 100000000 43 59 2 15 12 2001 3600)
		"2001-12-15 02:59:43.1+01:00")
(test-yaml-date (make-date 100000000 43 59 2 15 12 2001 (* -5 3600))
		"2001-12-15 02:59:43.1 -5")

(test-equal '(-inf.0 "str")
	    (->yaml `#(,+yaml-tag:seq+
		       (,+yaml-tag:float+ . "-.Inf")
		       (,+yaml-tag:str+ . "str"))))

(test-equal '#(("key" . "value")
	       ("key2" . "value"))
	    (->yaml `(,+yaml-tag:map+
		      ((,+yaml-tag:str+ . "key") (,+yaml-tag:str+ . "value"))
		      ((,+yaml-tag:str+ . "key2") (,+yaml-tag:str+ . "value"))
		      ((,+yaml-tag:str+ . "key") (,+yaml-tag:str+ . "value")))))

(test-equal '#(("key" "value1" "value2"))
	    (->yaml `(,+yaml-tag:map+
		      ((,+yaml-tag:str+ . "key")
		       #(,+yaml-tag:seq+
			 (,+yaml-tag:str+ . "value1")
			 (,+yaml-tag:str+ . "value2"))))))

(test-equal '(
	      ("meeting" "with team")
	      ("meeting" "with boss")
	      )
	    (->yaml `#(,+yaml-tag:pairs+
		       (,+yaml-tag:map+
			((,+yaml-tag:str+ . "meeting")
			 (,+yaml-tag:str+ . "with team")))
		       (,+yaml-tag:map+
			((,+yaml-tag:str+ . "meeting")
			 (,+yaml-tag:str+ . "with boss"))))))

(test-equal '(
	      ("meeting" "with team")
	      ("meeting" "with boss")
	      )
	    (->yaml `#(,+yaml-tag:omap+
		       (,+yaml-tag:map+
			((,+yaml-tag:str+ . "meeting")
			 (,+yaml-tag:str+ . "with team")))
		       (,+yaml-tag:map+
			((,+yaml-tag:str+ . "meeting")
			 (,+yaml-tag:str+ . "with boss"))))))

(test-equal '#(("One" . null) ("Two" . null) ("Three" . null))
	    (->yaml `(,+yaml-tag:set+
		      ((,+yaml-tag:str+ . "One") (,+yaml-tag:null+ . ""))
		      ((,+yaml-tag:str+ . "Two") (,+yaml-tag:null+ . ""))
		      ((,+yaml-tag:str+ . "Three") (,+yaml-tag:null+ . ""))
		      ((,+yaml-tag:str+ . "One") (,+yaml-tag:null+ . "")))))

(define (test-yaml/input sexp str)
  (test-equal sexp
	      (car (map yaml->sexp (parse-yaml (open-string-input-port str))))))

;; This test relay on the implementation since mapping doesn't
;; necessarily preserve order
(test-yaml/input '(#(("one" . 1))
		   #(("two" . null))
		   #(("one" . 1) ("two" . 2) ("r" . 10)))
		 "- &alias1 { one: 1 }\n\
                  - &alias2 !!set { two: 2 }\n\
                  - << : [ *alias1, *alias2 ]\n  \
                    r: 10")

(test-yaml/input '1.0 "!!float 1")

(test-yaml/input '#((("foo" "boo") . "bar")) "%YAML 1.2
---
? - foo
  - boo
: bar
")

(test-end)
