(import (rnrs)
	(text yaml builder)
	(text yaml nodes)
	(text yaml tags)
	(rfc base64)
	(srfi :19)
	(srfi :64))

(test-begin "YAML builder")

(define (->yaml sexp) (yaml->sexp (canonical-sexp->yaml-node sexp)))

(test-equal "string" (->yaml `(,+yaml-tag:str+ . "string")))

(test-equal #vu8(1 2 3 4)
	    (->yaml `(,+yaml-tag:binary+
		      . ,(utf8->string (base64-encode #vu8(1 2 3 4))))))

(test-equal #t (->yaml `(,+yaml-tag:bool+ . "y")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "yes")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "Yes")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "true")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "True")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "On")))
(test-equal #t (->yaml `(,+yaml-tag:bool+ . "on")))
(test-equal #f (->yaml `(,+yaml-tag:bool+ . "n")))
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
(test-assert (date=? (make-date 0 0 0 0 14 12 2002 0)
		     (->yaml `(,+yaml-tag:timestamp+ . "2002-12-14"))))
(test-assert (date=? (make-date 1 43 59 2 15 12 2001 0)
		     (->yaml `(,+yaml-tag:timestamp+ . "2001-12-15T02:59:43.1Z"))))
(test-assert
 (date=? (make-date 1 43 59 2 15 12 2001 3600)
	 (->yaml `(,+yaml-tag:timestamp+ . "2001-12-15T02:59:43.1+01:00"))))
(test-assert
 (date=? (make-date 1 43 59 2 15 12 2001 3600)
	 (->yaml `(,+yaml-tag:timestamp+ . "2001-12-15t02:59:43.1+01:00"))))
(test-assert
 (date=? (make-date 1 43 59 2 15 12 2001 3600)
	 (->yaml `(,+yaml-tag:timestamp+ . "2001-12-15 02:59:43.1+01:00"))))
(test-assert
 (date=? (make-date 1 43 59 2 15 12 2001 (* -5 3600))
	 (->yaml `(,+yaml-tag:timestamp+ . "2001-12-15 02:59:43.1 -5"))))


(test-end)
