;;; -*- mode: scheme; coding: utf-8 -*-
(import (rnrs)
	(text xml dom)
	(text xml errors)
	(text xml xpath fn)
	(text xml schema)
	(srfi :64))

(test-begin "XPath functions and operators")

(define xml-file (string-append (current-directory) "/test/data/test-xml.xml"))
(define dom (xml-file->dom-tree xml-file))
(define root-element (document-document-element dom))

(test-group "Accessors"
  (test-group "xpath-fn:node-name"
    (test-equal '() (xpath-fn:node-name '()))
    (test-error xqt-error? (xpath-fn:node-name "not a node"))
    (test-equal "root"
		(xs:qname->node-name (xpath-fn:node-name root-element))))

  (test-group "xpath-fn:nilled"
    (test-equal '() (xpath-fn:nilled '()))
    (test-error xqt-error? (xpath-fn:nilled "not a node"))
    (test-equal #f (xpath-fn:nilled root-element))
    (test-equal '() (xpath-fn:nilled dom)))

  (test-group "xpath-fn:string"
    (test-equal "" (xpath-fn:string '()))
    (test-equal "foo" (xpath-fn:string "foo"))
    (test-equal "1" (xpath-fn:string 1))
    (test-equal "true" (xpath-fn:string #t))
    (test-equal "false" (xpath-fn:string #f))
    (test-equal " cdata section "
		(xpath-fn:string
		 ;; get cdata section
		 (node-previous-sibling (node-last-child root-element))))
    (test-error xqt-error? (xpath-fn:string 'symbol))
    ;; TODO add error cases, array and map
    )

  (test-group "xpath-fn:data"
    (test-equal 123 (xpath-fn:data 123))
    (test-equal '(123 456) (xpath-fn:data '(123 456)))
    (test-equal " cdata section "
		(xpath-fn:data
		 ;; get cdata section (laziness)
		 (node-previous-sibling (node-last-child root-element))))
    (test-error xqt-error? (xpath-fn:data 'symbol))
    ;; TODO error cases
    )

  (test-group "xpath-fn:base-uri"
    (test-equal '() (xpath-fn:base-uri '()))
    (test-equal (absolute-path xml-file) (xpath-fn:base-uri dom))
    (test-equal '() (xpath-fn:base-uri root-element))
    (test-error xqt-error? (xpath-fn:base-uri 'symbol))
    )

  (test-group "xpath-fn:document-uri"
    (test-equal '() (xpath-fn:document-uri '()))
    (test-equal (absolute-path xml-file) (xpath-fn:document-uri dom))
    (test-equal '() (xpath-fn:document-uri root-element))
    (test-error xqt-error? (xpath-fn:document-uri 'symbol))
    )
  )
(define (test-xqt-error-runner code thunk)
  (guard (e ((xqt-error? e)
	     (test-equal code (xqt-error-code e)))
	    (else
	     (test-assert (condition-message e) #f)))
    (thunk)
    (test-assert "must be an error" #f)))
(define-syntax test-xqt-error
  (syntax-rules ()
    ((_ code expr)
     (test-xqt-error-runner 'code (lambda () expr)))))

(test-group "Errors and diagnostics"
  (test-group "fn:error"
    (test-xqt-error FOER0000 (xpath-fn:error))
    (test-xqt-error Unknown
		    (xpath-fn:error
		     (xs:make-qname "don't care for now" "Unknown")))
    )
  (test-group "fn:trace"
    (test-error implementation-restriction-violation?
		(xpath-fn:trace)))
  )

(test-group "Functions and operators on numerics"
  (test-group "Arithmetic operators on numeric values"
    (test-group "op:numeric-add"
      (test-equal 2 (xpath-op:numeric-add 1 1)))
    (test-group "op:numeric-subtract"
      (test-equal 0 (xpath-op:numeric-subtract 1 1)))
    (test-group "op:numeric-multiply"
      (test-equal 1 (xpath-op:numeric-multiply 1 1)))
    (test-group "op:numeric-divide"
      (test-equal 1 (xpath-op:numeric-divide 2 2)))

    (test-group "op:numeric-integer-divide"
      (test-equal 3 (xpath-op:numeric-integer-divide 10 3))
      (test-equal -1 (xpath-op:numeric-integer-divide 3 -2))
      (test-equal -1 (xpath-op:numeric-integer-divide -3 2))
      (test-equal 1 (xpath-op:numeric-integer-divide 3 2))
      (test-equal 3 (xpath-op:numeric-integer-divide 9.0 3))
      (test-equal -1 (xpath-op:numeric-integer-divide -3.5 3))
      (test-equal 0 (xpath-op:numeric-integer-divide 3.0 4))
      (test-equal 5 (xpath-op:numeric-integer-divide 3.1e1 6))
      (test-equal 4 (xpath-op:numeric-integer-divide 3.1e1 7))
      (test-xqt-error FOAR0001 (xpath-op:numeric-integer-divide 1 0))
      (test-xqt-error FOAR0002 (xpath-op:numeric-integer-divide +inf.0 1))
      )

    (test-group "op:numeric-mod"
      (test-equal 1   (xpath-op:numeric-mod 10 3))
      (test-equal 0   (xpath-op:numeric-mod 6 -2))
      (test-approximate 0.9 (xpath-op:numeric-mod 4.5 1.2) 0.0000005)
      (test-equal 3.0 (xpath-op:numeric-mod 1.23e2 0.6e1))
      (test-xqt-error FOAR0001 (xpath-op:numeric-mod 1 0)))

    (test-group "op:numeric-unary-plus"
      (test-equal 1 (xpath-op:numeric-unary-plus 1)))

    (test-group "op:numeric-unary-minus"
      (test-equal 0 (xpath-op:numeric-unary-minus 0))
      (test-equal -0.0 (xpath-op:numeric-unary-minus 0.0))
      (test-equal +nan.0 (xpath-op:numeric-unary-minus +nan.0))
      (test-equal -inf.0 (xpath-op:numeric-unary-minus +inf.0)))
    )
  
  (test-group "Comparison operators on numeric values"
    (test-group "op:numeric-equal"
      (test-assert (xpath-op:numeric-equal +inf.0 +inf.0))
      (test-assert (xpath-op:numeric-equal -inf.0 -inf.0))
      (test-assert (xpath-op:numeric-equal 0.0 -0.0))
      (test-assert (not (xpath-op:numeric-equal +nan.0 +nan.0))))
  
    (test-group "op:numeric-less-than"
      (test-assert (xpath-op:numeric-less-than 100 +inf.0))
      (test-assert (xpath-op:numeric-less-than -inf.0 100))
      (test-assert (not (xpath-op:numeric-less-than 0 +nan.0)))
      (test-assert (not (xpath-op:numeric-less-than +nan.0 0))))

    (test-group "op:numeric-greater-than"
      (test-assert (xpath-op:numeric-greater-than +inf.0 100))
      (test-assert (xpath-op:numeric-greater-than 100 -inf.0))
      (test-assert (not (xpath-op:numeric-greater-than 0 +nan.0)))
      (test-assert (not (xpath-op:numeric-greater-than +nan.0 0))))
    )

  (test-group "Functions on numeric values"
    (test-group "fn:abs"
      (test-equal 10.5 (xpath-fn:abs 10.5))
      (test-equal 10.5 (xpath-fn:abs -10.5)))

    (test-group "fn:ceiling"
      (test-equal 11.0 (xpath-fn:ceiling 10.5))
      (test-equal -10.0 (xpath-fn:ceiling -10.5)))
    
    (test-group "fn:floor"
      (test-equal 10.0 (xpath-fn:floor 10.5))
      (test-equal -11.0 (xpath-fn:floor -10.5)))
    
    (test-group "fn:round"
      (test-equal 3.0 (xpath-fn:round 2.5))
      (test-equal 2.0 (xpath-fn:round 2.49999))
      (test-equal -2.0 (xpath-fn:round -2.5))
      )

    (test-group "fn:round-half-to-even"
      (test-equal 0.0 (xpath-fn:round-half-to-even 0.5))
      (test-equal 2.0 (xpath-fn:round-half-to-even 1.5))
      (test-equal 2.0 (xpath-fn:round-half-to-even 2.5))
      (test-equal 3567.81 (xpath-fn:round-half-to-even 3.567812e3 2))
      (test-equal 0.0 (xpath-fn:round-half-to-even 4.7564e-3 2))
      (test-equal 35600.0 (xpath-fn:round-half-to-even 35612.25 -2))
      )
    )

  (test-group "Parsing numbers"
    (test-equal 1.5e1 (xpath-fn:number "15")))

  (test-group "Formatting integers"
    (test-assert "at least exported" xpath-fn:format-integer)
    (test-expect-fail 5)
    (test-equal "0123" (xpath-fn:format-integer 123 "0000"))
    (test-equal "21st" (xpath-fn:format-integer 21 "1;o" "en"))
    (test-equal "g" (xpath-fn:format-integer 7 "a"))
    (test-equal "LVII" (xpath-fn:format-integer 57 "I"))
    (test-equal "1;234" (xpath-fn:format-integer 123 "#;##0;"))
    )

  (test-group "Formatting numbers"
    (test-assert "at least exported" xpath-fn:format-number)
    (test-expect-fail 5)
    (test-equal "12,345.60" (xpath-fn:format-number 12345.6 "#,###.00"))
    (test-equal "12,345,678.90" (xpath-fn:format-number 12345678.9 "9,999.99"))
    (test-equal "0124" (xpath-fn:format-number 123.9 "9,999.99"))
    (test-equal "14%" (xpath-fn:format-number 0.14 "01%"))
    (test-equal "-006" (xpath-fn:format-number -6 "000"))
    ;; TODO should we support fortran at least?
    )

  (test-group "Trigonometric and exponential functions"
    (test-group "math:pi"
      (test-approximate 3.1415 (xpath-math:pi) 0.0001))
    
    (test-group "math:exp"
      (test-equal 1.0 (xpath-math:exp 0))
      (test-approximate 2.718281828 (xpath-math:exp 1) 0.000000001)
      (test-equal 7.38905609893065e0 (xpath-math:exp 2))
      (test-equal 0.36787944117144233e0 (xpath-math:exp -1))
      (test-equal 23.140692632779267e0 (xpath-math:exp (xpath-math:pi)))
      (test-equal +nan.0 (xpath-math:exp +nan.0))
      (test-equal +inf.0 (xpath-math:exp +inf.0))
      (test-equal 0.0 (xpath-math:exp -inf.0)))
      
    (test-group "math:exp10"
      (test-equal 1.0 (xpath-math:exp10 0))
      (test-equal 1.0e1 (xpath-math:exp10 1))
      (test-equal 3.1622776601683795e0 (xpath-math:exp10 0.5))
      (test-equal 1.0e-1 (xpath-math:exp10 -1))
      (test-equal +nan.0 (xpath-math:exp10 +nan.0))
      (test-equal +inf.0 (xpath-math:exp10 +inf.0))
      (test-equal 0.0 (xpath-math:exp10 -inf.0)))

    (test-group "math:log"
      (test-equal -inf.0 (xpath-math:log 0))
      (test-equal 1.0e0 (xpath-math:log (xpath-math:exp 1)))
      (test-equal -6.907755278982137e0 (xpath-math:log 1.0e-3))
      (test-equal 0.6931471805599453e0 (xpath-math:log 2))
      (test-equal +nan.0 (xpath-math:log -1))
      (test-equal +nan.0 (xpath-math:log +nan.0))
      (test-equal +inf.0 (xpath-math:log +inf.0))
      (test-equal +nan.0 (xpath-math:log -inf.0)))

    (test-group "math:log10"
      (test-equal -inf.0 (xpath-math:log10 0))
      ;; bah
      (test-approximate 3.0e0 (xpath-math:log10 1.0e3) 0.00000000000001)
      (test-approximate -3.0e0 (xpath-math:log10 1.0e-3) 0.00000000000001)
      (test-approximate 0.3010299956639812e0 (xpath-math:log10 2) 0.000000000001)
      (test-equal +nan.0 (xpath-math:log10 -1))
      (test-equal +nan.0 (xpath-math:log10 +nan.0))
      (test-equal +inf.0 (xpath-math:log10 +inf.0))
      (test-equal +nan.0 (xpath-math:log10 -inf.0)))

    (test-group "math:pow"
      (test-equal 8.0 (xpath-math:pow 2 3))
      (test-equal -8.0 (xpath-math:pow -2 3))
      (test-equal 0.125 (xpath-math:pow 2 -3))
      (test-equal -0.125 (xpath-math:pow -2 -3))
      (test-equal 1.0 (xpath-math:pow 2 0))
      (test-equal 1.0 (xpath-math:pow 0 0))
      (test-equal 1.0 (xpath-math:pow +inf.0 0))
      (test-equal 1.0 (xpath-math:pow +nan.0 0))
      (test-equal 1.0 (xpath-math:pow (- (xpath-math:pi)) 0))
      (test-equal 0.0 (xpath-math:pow 0 3))
      (test-equal 0.0 (xpath-math:pow 0 4))
      (test-equal -0.0 (xpath-math:pow -0.0 3))
      (test-equal +inf.0 (xpath-math:pow 0.0 -3))
      (test-equal +inf.0 (xpath-math:pow 0.0 -4))
      (test-equal -inf.0 (xpath-math:pow -0.0 -3))
      (test-equal +inf.0 (xpath-math:pow 0.0 -3.1))
      (test-equal +inf.0 (xpath-math:pow -0.0 -3.1))
      (test-equal 0.0 (xpath-math:pow 0.0 3.1))
      (test-equal -0.0 (xpath-math:pow -0.0 3.0))
      (test-equal 1.0 (xpath-math:pow -1 +inf.0))
      (test-equal 1.0 (xpath-math:pow -1 -inf.0))
      (test-equal 1.0 (xpath-math:pow 1 +inf.0))
      (test-equal 1.0 (xpath-math:pow 1 -inf.0))
      (test-equal 1.0 (xpath-math:pow 1 +nan.0))
      (test-equal 6.25 (xpath-math:pow -2.5 2.0))
      (test-equal +nan.0 (xpath-math:pow -2.5 2.00000001e0))
      )

    (test-group "math:sqrt"
      (test-equal 0.0 (xpath-math:sqrt 0.0))
      (test-equal -0.0 (xpath-math:sqrt -0.0))
      (test-equal 1.0e3 (xpath-math:sqrt 1.0e6))
      (test-equal 1.4142135623730951e0 (xpath-math:sqrt 2.0))
      (test-equal +nan.0 (xpath-math:sqrt -2.0))
      (test-equal +nan.0 (xpath-math:sqrt +nan.0))
      (test-equal +inf.0 (xpath-math:sqrt +inf.0))
      (test-equal +nan.0 (xpath-math:sqrt -inf.0))
      )

    (test-group "math:sin"
      (test-equal 0.0 (xpath-math:sin 0))
      (test-equal -0.0 (xpath-math:sin -0.0))
      (test-equal 1.0 (xpath-math:sin (/ (xpath-math:pi) 2)))
      (test-equal -1.0 (xpath-math:sin (/ (- (xpath-math:pi)) 2)))
      (test-approximate 0.0 (xpath-math:sin (xpath-math:pi)) 0.1e-10)
      (test-equal +nan.0 (xpath-math:sin +nan.0))
      (test-equal +nan.0 (xpath-math:sin +inf.0))
      (test-equal +nan.0 (xpath-math:sin -inf.0)))

    (test-group "math:cos"
      (test-equal 1.0 (xpath-math:cos 0))
      (test-equal 1.0 (xpath-math:cos -0.0))
      (test-approximate 0.0 (xpath-math:cos (/ (xpath-math:pi) 2)) 0.1e-10)
      (test-approximate 0.0 (xpath-math:cos (/ (- (xpath-math:pi)) 2)) 0.1e-10)
      (test-approximate -1.0 (xpath-math:cos (xpath-math:pi)) 0.1e-10)
      (test-equal +nan.0 (xpath-math:cos +nan.0))
      (test-equal +nan.0 (xpath-math:cos +inf.0))
      (test-equal +nan.0 (xpath-math:cos -inf.0)))

    (test-group "math:tan"
      (test-equal 0.0 (xpath-math:tan 0))
      (test-equal -0.0 (xpath-math:tan -0.0))
      (test-approximate 0.0 (/ 1 (xpath-math:tan (/ (xpath-math:pi) 2))) 0.1e-10)
      (test-approximate -0.0 (/ 1 (xpath-math:tan (/ (- (xpath-math:pi)) 2))) 0.1e-10)
      (test-approximate 0.0 (xpath-math:tan (xpath-math:pi)) 0.1e-10)
      (test-equal +nan.0 (xpath-math:tan +nan.0))
      (test-equal +nan.0 (xpath-math:tan +inf.0))
      (test-equal +nan.0 (xpath-math:tan -inf.0)))

    (test-group "math:asin"
      (test-equal 0.0 (xpath-math:asin 0))
      (test-equal -0.0 (xpath-math:asin -0.0))
      (test-equal 1.5707963267948966e0 (xpath-math:asin 1.0))
      (test-equal -1.5707963267948966e0 (xpath-math:asin -1.0))
      (test-equal +nan.0 (xpath-math:asin 2.0))
      (test-equal +nan.0 (xpath-math:asin +nan.0))
      (test-equal +nan.0 (xpath-math:asin +inf.0))
      (test-equal +nan.0 (xpath-math:asin -inf.0)))

    (test-group "math:acos"
      (test-approximate 1.5707963267948966e0 (xpath-math:acos 0) 0.1e-10)
      (test-approximate 1.5707963267948966e0 (xpath-math:acos -0.0) 0.1e-10)
      (test-equal 0.0 (xpath-math:acos 1.0))
      (test-approximate 3.141592653589793e0 (xpath-math:acos -1.0) 0.1e-10)
      (test-equal +nan.0 (xpath-math:acos 2.0))
      (test-equal +nan.0 (xpath-math:acos +nan.0))
      (test-equal +nan.0 (xpath-math:acos +inf.0))
      (test-equal +nan.0 (xpath-math:acos -inf.0)))

    (test-group "math:atan"
      (test-equal 0.0 (xpath-math:atan 0))
      (test-equal -0.0 (xpath-math:atan -0.0))
      (test-approximate 0.7853981633974483e0 (xpath-math:atan 1.0) 0.1e-10)
      (test-approximate -0.7853981633974483e0 (xpath-math:atan -1.0) 0.1e-10)
      (test-equal +nan.0 (xpath-math:atan +nan.0))
      (test-approximate 1.5707963267948966e0 (xpath-math:atan +inf.0) 0.1e-10)
      (test-approximate -1.5707963267948966e0 (xpath-math:atan -inf.0) 0.1e-10))

    (test-group "math:atan2"
      (test-equal 0.0 (xpath-math:atan2 0.0 0.0))
      (test-equal -0.0 (xpath-math:atan2 -0.0 0.0))
      (test-approximate 3.141592653589793e0 (xpath-math:atan2 0.0 -0.0) 0.1e-10)
      (test-approximate -3.141592653589793e0 (xpath-math:atan2 -0.0 -0.0) 0.1e-10)
      (test-approximate -1.5707963267948966e0 (xpath-math:atan2 -1 0.0) 0.1e-10)
      (test-approximate 1.5707963267948966e0 (xpath-math:atan2 1 0.0) 0.1e-10)
      (test-approximate -3.141592653589793e0 (xpath-math:atan2 -0.0 -1) 0.1e-10)
      (test-approximate 3.141592653589793e0 (xpath-math:atan2 0.0 -1) 0.1e-10)
      (test-equal -0.0 (xpath-math:atan2 -0.0 1))
      (test-equal 0.0 (xpath-math:atan2 0.0 1)))
    )
  )

(test-group "Functions on strings"
  (test-group "fn:codepoints-to-string"
    (test-equal "BACH" (xpath-fn:codepoints-to-string '(66 65 67 72)))
    (test-equal "अशॊक" (xpath-fn:codepoints-to-string '(2309 2358 2378 2325)))
    (test-equal "" (xpath-fn:codepoints-to-string '()))
    (test-xqt-error FOCH0001 (xpath-fn:codepoints-to-string '(0))))

  (test-group "fn:string-to-codepoints"
    (test-equal '(84 104 233 114 232 115 101) (xpath-fn:string-to-codepoints "Thérèse")))

  (test-group "fn:compare"
    (test-equal 0 (xpath-fn:compare "abc" "abc"))
    (test-equal 1 (xpath-fn:compare "Strassen" "Strasse"))
    ;; for now
    (test-xqt-error FOCH0002 (xpath-fn:compare "" "" "http://www.w3.org/2013/colltion/UCA?lang=de;strength=primary"))
    )

  (test-group "fn:codepoint-equal"
    (test-assert (xpath-fn:codepoint-equal "abcd" "abcd"))
    (test-assert (not (xpath-fn:codepoint-equal "abcd" "abcd ")))
    (test-assert (xpath-fn:codepoint-equal "" ""))
    (test-equal '() (xpath-fn:codepoint-equal "" '()))
    (test-equal '() (xpath-fn:codepoint-equal '() '())))
  )

(test-end)

;; Local Variables:
;; eval: (put 'test-group 'scheme-indent-function 1)
;; End:
