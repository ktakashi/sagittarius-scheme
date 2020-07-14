;;; -*- mode: scheme; coding: utf-8 -*-
(import (rnrs)
	(text xml dom)
	(text xml errors)
	(text xml xpath fn)
	(text xml schema)
	(sagittarius timezone)
	(srfi :39)
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
(define (test-xqt-error-runner msg code thunk)
  (guard (e ((xqt-error? e)
	     (test-equal msg code (xqt-error-code e)))
	    (else
	     (test-assert (condition-message e) #f)))
    (thunk)
    (test-assert "must be an error" #f)))
(define-syntax test-xqt-error
  (syntax-rules ()
    ((_ code expr)
     (test-xqt-error-runner 'expr 'code (lambda () expr)))))

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

  (test-group "fn:concat"
    (test-equal "ungrateful" (xpath-fn:concat "un" "grateful"))
    (test-equal "Thy old groans ring yet in my ancient ears."
		(xpath-fn:concat "Thy " '() "old " "groans" "" " ring"
				 " yet" " in" " my" " ancient" " ears."))
    (test-equal "Ciao!" (xpath-fn:concat "Ciao!" '()))
    (test-equal "Ingratitude, thou marble-hearted fiend!"
		(xpath-fn:concat "Ingratitude, " "thou " "marble-hearted" " fiend!"))
    (test-equal "1234true" (xpath-fn:concat 01 02 03 04 #t))
    )

  (test-group "fn:string-join"
    (test-equal "123456789" (xpath-fn:string-join '(1 2 3 4 5 6 7 8 9)))
    (test-equal "Now is the time ..."
		(xpath-fn:string-join '("Now" "is" "the" "time" "...") " "))
    (test-equal "Blow, blow, thou winter wind!"
		(xpath-fn:string-join '("Blow, " "blow, " "thou " "winter " "wind!") ""))
    (test-equal "" (xpath-fn:string-join '(()) "separator"))
    (test-equal "1, 2, 3, 4, 5" (xpath-fn:string-join '(1 2 3 4 5) ", "))
    )

  (test-group "fn:substring"
    (test-equal " car" (xpath-fn:substring "motor car" 6))
    (test-equal "ada" (xpath-fn:substring "metadata" 4 3))
    (test-equal "234" (xpath-fn:substring "12345" 1.5 2.6))
    (test-equal "12" (xpath-fn:substring "12345" 0 3))
    (test-equal "" (xpath-fn:substring "12345" 5 -3))
    (test-equal "1" (xpath-fn:substring "12345" -3 5))
    (test-equal "" (xpath-fn:substring "12345" +nan.0 3))
    (test-equal "" (xpath-fn:substring "12345" 1 +nan.0))
    (test-equal "" (xpath-fn:substring '() 1 3))
    (test-equal "12345" (xpath-fn:substring "12345" -42 (/ 1 0.0)))
    (test-equal "" (xpath-fn:substring "12345" (/ -1 0.0) (/ -1 0.0)))
    )

  (test-group "fn:string-length"
    (test-equal 45 (xpath-fn:string-length "Harp not on that string, madam; that is past."))
    (test-equal 0 (xpath-fn:string-length '())))

  (test-group "fn:normalize-space"
    (test-equal "The wealthy curled darlings of our nation."
		(xpath-fn:normalize-space " The    wealthy curled darlings                                         of    our    nation. "))
    (test-equal "" (xpath-fn:normalize-space '())))

  (test-group "fn:normalize-unicode"
    (test-xqt-error FOCH0003 (xpath-fn:normalize-unicode "foo" "bar")))

  (test-group "fn:upper-case"
    (test-equal "ABCD0" (xpath-fn:upper-case "abCd0")))
  (test-group "fn:lower-case"
    (test-equal "abc!d" (xpath-fn:lower-case "ABc!D")))

  (test-group "fn:translate"
    (test-equal "BAr" (xpath-fn:translate "bar" "abc" "ABC"))
    (test-equal "AAA" (xpath-fn:translate "--aaa--" "abc-" "ABC"))
    (test-equal "ABdAB" (xpath-fn:translate "abcdabc" "abc" "AB")))

  (test-group "fn:contains"
    (test-assert (xpath-fn:contains "tattoo" "t"))
    (test-assert (not (xpath-fn:contains "tattoo" "ttt")))
    (test-assert (xpath-fn:contains "" '()))

    (test-expect-fail 4)
    (test-assert
     (xpath-fn:contains "abcdefghi" "-d-e-f-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:contains "a*b*c*d*e*f*g*h*i*" "d-ef-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:contains "abcd***e---f*--*ghi" "def"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:contains '() "--***-*---"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    )

  (test-group "fn:starts-with"
    (test-assert (xpath-fn:starts-with "tattoo" "t"))
    (test-assert (not (xpath-fn:starts-with "tattoo" "att")))
    (test-assert (xpath-fn:starts-with '() '()))

    (test-expect-fail 5)
    (test-assert
     (xpath-fn:starts-with "abcdefghi" "-d-e-f-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:starts-with "a*b*c*d*e*f*g*h*i*" "a-bc-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:starts-with "abcd***e---f*--*ghi" "abcdef"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:starts-with '() "--***-*---"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:starts-with "-abcdefghi" "-abc"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    )

  (test-group "fn:ends-with"
    (test-assert (xpath-fn:ends-with "tattoo" "tattoo"))
    (test-assert (not (xpath-fn:ends-with "tattoo" "atto")))
    (test-assert (xpath-fn:ends-with '() '()))

    (test-expect-fail 5)
    (test-assert
     (xpath-fn:ends-with "abcdefghi" "-g-h-i-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:ends-with "a*b*c*d*e*f*g*h*i*" "defghi"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:ends-with "abcd***e---f*--*ghi" "defghi"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:ends-with '() "--***-*---"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-assert
     (xpath-fn:ends-with "abcdefghi" "ghi-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    )

  (test-group "fn:substring-before"
    (test-equal "t" (xpath-fn:substring-before "tattoo" "attoo"))
    (test-equal ""  (xpath-fn:substring-before "tattoo" "tatto"))
    (test-equal ""  (xpath-fn:substring-before '() '()))

    (test-expect-fail 4)
    (test-equal "abc"
     (xpath-fn:substring-before "abcdefghi" "--d-e-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-equal "abc--"
     (xpath-fn:substring-before "abc--d-e-fghi" "--d-e-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-equal "a*b*"
     (xpath-fn:substring-before "a*b*c*d*e*f*g*h*i*" "***cde"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-equal ""
     (xpath-fn:substring-before "Eureka!" "--***-*---"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    )

  (test-group "fn:substring-after"
    (test-equal "too" (xpath-fn:substring-after "tattoo" "tat"))
    (test-equal "" (xpath-fn:substring-after "tattoo" "tattoo"))
    (test-equal "" (xpath-fn:substring-after '() '()))

    (test-expect-fail 4)
    (test-equal "fghi"
     (xpath-fn:substring-after "abcdefghi" "--d-e-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-equal "-fghi"
     (xpath-fn:substring-after "abc--d-e-fghi" "--d-e-"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-equal "*f*g*h*i*"
     (xpath-fn:substring-after "a*b*c*d*e*f*g*h*i*" "***cde"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    (test-equal "Eureka!"
     (xpath-fn:substring-after "Eureka!" "--***-*---"
      "http://www.w3.org/2013/collation/UCA?lang=en;alternate=blanked;strength=primary"))
    )

  (test-group "fn:matches"
    (test-assert (xpath-fn:matches "abracadabra" "bra"))
    (test-assert (xpath-fn:matches "abracadabra" "^a.*a$"))
    (test-assert (not (xpath-fn:matches "abracadabra" "^bra")))
    (let ((s "Kaum hat dies der Hahn gesehen,\nFängt er auch schon an zu krähen:\nKikeriki! Kikikerikih!!\nTak, tak, tak! - da kommen sie."))
      (test-assert (not (xpath-fn:matches s "Kaum.*krähen")))
      (test-assert (xpath-fn:matches s "Kaum.*krähen" "s"))
      (test-assert (xpath-fn:matches s "^Kaum.*gesehen,$" "m"))
      (test-assert (not (xpath-fn:matches s "^Kaum.*gesehen,$")))
      (test-assert (xpath-fn:matches s "kiki" "i"))))

  (test-group "fn:replace"
    (test-equal "a*cada*" (xpath-fn:replace "abracadabra" "bra" "*"))
    (test-equal "*" (xpath-fn:replace "abracadabra" "a.*a" "*"))
    (test-equal "*c*bra" (xpath-fn:replace "abracadabra" "a.*?a" "*"))
    (test-equal "brcdbr" (xpath-fn:replace "abracadabra" "a" ""))
    (test-equal "abbraccaddabbra" (xpath-fn:replace "abracadabra" "a(.)" "a$1$1"))
    (test-xqt-error FORX0003 (xpath-fn:replace "abracadabra" ".*?" "$1"))
    (test-equal "b" (xpath-fn:replace "AAAA" "A+" "b"))
    (test-equal "bbbb" (xpath-fn:replace "AAAA" "A+?" "b"))
    (test-equal "carted" (xpath-fn:replace "darted" "^(.*?)d(.*)$" "$1c$2"))
    )

  (test-group "fn:tokenize"
    (test-equal '("" "r" "c" "d" "r" "") (xpath-fn:tokenize "abracadabra" "(ab)|(a)"))
    (test-equal '("red" "green" "blue") (xpath-fn:tokenize " red green blue "))
    (test-equal '("The" "cat" "sat" "on" "the" "mat") (xpath-fn:tokenize "The cat sat on the mat" "\\s+"))
    (test-equal '("" "red" "green" "blue" "") (xpath-fn:tokenize " red green blue " "\\s+"))
    (test-equal '("1" "15" "24" "50") (xpath-fn:tokenize "1, 15, 24, 50" ",\\s*"))
    (test-equal '("1" "15" "" "24" "50" "") (xpath-fn:tokenize "1,15,,24,50," ","))
    (test-xqt-error FORX0003 (xpath-fn:tokenize "abba" ".?"))
    (test-equal '("Some unparsed" "HTML" "text") (xpath-fn:tokenize "Some unparsed <br> HTML <BR> text" "\\s*<br>\\s*" "i"))
    )

  (test-group "fn:analyze-string"
    (test-xqt-error FORX0001 (xpath-fn:analyze-string "input" "pattern" "bn")))
  )

(test-group "Functions that manipulate URIs"
  (test-group "fn:resolve-uri"
    (test-equal '() (xpath-fn:resolve-uri '()))
    (test-equal "foo:ok" (xpath-fn:resolve-uri "foo:ok"))
    (test-xqt-error FONS0005 (xpath-fn:resolve-uri "not-ok"))
    (test-equal "http://a/b/c/g" (xpath-fn:resolve-uri "g" "http://a/b/c/d;p?q")))

  (test-group "fn:encode-for-uri"
    (test-equal
     "http%3A%2F%2Fwww.example.com%2F00%2FWeather%2FCA%2FLos%2520Angeles%23ocean"
     (xpath-fn:encode-for-uri "http://www.example.com/00/Weather/CA/Los%20Angeles#ocean"))
    (test-equal "~b%C3%A9b%C3%A9" (xpath-fn:encode-for-uri "~bébé"))
    (test-equal "100%25%20organic" (xpath-fn:encode-for-uri "100% organic")))

  (test-group "fn:iri-to-uri"
    (test-equal "http://www.example.com/00/Weather/CA/Los%20Angeles#ocean"
		(xpath-fn:iri-to-uri "http://www.example.com/00/Weather/CA/Los%20Angeles#ocean"))
    (test-equal "http://www.example.com/~b%C3%A9b%C3%A9"
		(xpath-fn:iri-to-uri "http://www.example.com/~bébé")))

  (test-group "fn:escape-html-uri"
    (test-equal "http://www.example.com/00/Weather/CA/Los Angeles#ocean"
		(xpath-fn:escape-html-uri "http://www.example.com/00/Weather/CA/Los Angeles#ocean"))
    (test-equal "javascript:if (navigator.browserLanguage == 'fr') window.open('http://www.example.com/~b%C3%A9b%C3%A9');"
		(xpath-fn:escape-html-uri "javascript:if (navigator.browserLanguage == 'fr') window.open('http://www.example.com/~bébé');")))
  )

(test-group "Functions and operators on Boolean values"
  (test-group "fn:true"
    (test-assert (xpath-fn:true)))
  (test-group "fn:false"
    (test-assert (not (xpath-fn:false))))
  (test-group "op:boolean-equal"
    (test-assert (xpath-op:boolean-equal #t #t))
    (test-assert (xpath-op:boolean-equal #f #f))
    (test-assert (not (xpath-op:boolean-equal #t #f)))
    (test-assert (not (xpath-op:boolean-equal #f #t))))
  (test-group "op:boolean-less-than"
    (test-assert (xpath-op:boolean-less-than #f #t))
    (test-assert (not (xpath-op:boolean-less-than #t #t)))
    (test-assert (not (xpath-op:boolean-less-than #t #f)))
    (test-assert (not (xpath-op:boolean-less-than #f #f))))
  (test-group "op:boolean-greater-than"
    (test-assert (xpath-op:boolean-greater-than #t #f))
    (test-assert (not (xpath-op:boolean-greater-than #t #t)))
    (test-assert (not (xpath-op:boolean-greater-than #f #t)))
    (test-assert (not (xpath-op:boolean-greater-than #f #f))))

  (test-group "fn:boolean"
    (test-xqt-error FORG0006 (xpath-fn:boolean '("a" "b" "c")))
    (test-assert (xpath-fn:boolean "a"))
    (test-assert (xpath-fn:boolean 1))
    (test-assert (not (xpath-fn:boolean 0)))
    (test-assert (not (xpath-fn:boolean '())))
    (test-assert (not (xpath-fn:boolean ""))))

  (test-group "fn:not"
    (test-xqt-error FORG0006 (xpath-fn:not '("a" "b" "c")))
    (test-assert (not (xpath-fn:not "a")))
    (test-assert (not (xpath-fn:not 1)))
    (test-assert (xpath-fn:not 0))
    (test-assert (xpath-fn:not '()))
    (test-assert (xpath-fn:not "")))  
  )

(test-group "8 Functions and operators on durations"
  (test-group "op:yearMonthDuration-less-than"
    (test-assert (xpath-op:year-month-duration-less-than
		  (xs:make-year-month-duration 1)
		  (xs:make-year-month-duration 2)))
    (test-assert (not (xpath-op:year-month-duration-less-than
		       (xs:make-year-month-duration 2)
		       (xs:make-year-month-duration 1))))
    (test-assert (not (xpath-op:year-month-duration-less-than
		       (xs:make-year-month-duration 1)
		       (xs:make-year-month-duration 1)))))

  (test-group "op:yearMonthDuration-greater-than"
    (test-assert (xpath-op:year-month-duration-greater-than
		  (xs:make-year-month-duration 2)
		  (xs:make-year-month-duration 1)))
    (test-assert (not (xpath-op:year-month-duration-greater-than
		       (xs:make-year-month-duration 1)
		       (xs:make-year-month-duration 2))))
    (test-assert (not (xpath-op:year-month-duration-greater-than
		       (xs:make-year-month-duration 1)
		       (xs:make-year-month-duration 1)))))

  (test-group "op:dayTimeDuration-less-than"
    (test-assert (xpath-op:day-time-duration-less-than
		  (xs:make-day-time-duration 1)
		  (xs:make-day-time-duration 2)))
    (test-assert (not (xpath-op:day-time-duration-less-than
		       (xs:make-day-time-duration 2)
		       (xs:make-day-time-duration 1))))
    (test-assert (not (xpath-op:day-time-duration-less-than
		       (xs:make-day-time-duration 1)
		       (xs:make-day-time-duration 1)))))

  (test-group "op:dayTimeDuration-greater-than"
    (test-assert (xpath-op:day-time-duration-greater-than
		  (xs:make-day-time-duration 2)
		  (xs:make-day-time-duration 1)))
    (test-assert (not (xpath-op:day-time-duration-greater-than
		       (xs:make-day-time-duration 1)
		       (xs:make-day-time-duration 2))))
    (test-assert (not (xpath-op:day-time-duration-greater-than
		       (xs:make-day-time-duration 1)
		       (xs:make-day-time-duration 1)))))

  (test-group "op:duration-equal"
    (test-assert (xpath-op:duration-equal (xs:make-duration "P1Y")
					  (xs:make-duration "P12M")))
    (test-assert (xpath-op:duration-equal (xs:make-duration "PT24H")
					  (xs:make-duration "P1D")))
    (test-assert (not (xpath-op:duration-equal (xs:make-duration "P1Y")
					       (xs:make-duration "P365D"))))
    (test-assert (xpath-op:duration-equal (xs:make-year-month-duration "P0Y")
					  (xs:make-day-time-duration "P0D")))
    (test-assert (not (xpath-op:duration-equal
		       (xs:make-year-month-duration "P1Y")
		       (xs:make-day-time-duration "P365D"))))
    (test-assert (xpath-op:duration-equal (xs:make-year-month-duration "P2Y")
					  (xs:make-year-month-duration "P24M")))
    (test-assert (xpath-op:duration-equal (xs:make-day-time-duration "P10D")
					  (xs:make-day-time-duration "PT240H")))
    (test-assert (xpath-op:duration-equal (xs:make-duration "P2Y0M0DT0H0M0S")
					  (xs:make-year-month-duration "P24M")))
    (test-assert (xpath-op:duration-equal (xs:make-duration "P0Y0M10D")
					  (xs:make-duration "PT240H"))))

  (test-group "fn:years-from-duration"
    (test-equal 21 (xpath-fn:years-from-duration
		    (xs:make-year-month-duration "P20Y15M")))
    (test-equal -1 (xpath-fn:years-from-duration
		    (xs:make-year-month-duration "-P15M")))
    (test-equal 0 (xpath-fn:years-from-duration
		   (xs:make-day-time-duration "-P2DT15H"))))

  (test-group "fn:months-from-duration"
    (test-equal 3 (xpath-fn:months-from-duration
		   (xs:make-year-month-duration "P20Y15M")))
    (test-equal -6 (xpath-fn:months-from-duration
		    (xs:make-year-month-duration "-P20Y18M")))
    (test-equal 0 (xpath-fn:months-from-duration
		   (xs:make-day-time-duration "-P2DT15H0M0S"))))

  (test-group "fn:days-from-duration"
    (test-equal 3 (xpath-fn:days-from-duration
		   (xs:make-day-time-duration "P3DT10H")))
    (test-equal 5 (xpath-fn:days-from-duration
		   (xs:make-day-time-duration "P3DT55H")))
    (test-equal 0 (xpath-fn:days-from-duration
		   (xs:make-year-month-duration "P3Y5M"))))

  (test-group "fn:hours-from-duration"
    (test-equal 10 (xpath-fn:hours-from-duration
		   (xs:make-day-time-duration "P3DT10H")))
    (test-equal 12 (xpath-fn:hours-from-duration
		    (xs:make-day-time-duration "P3DT12H32M12S")))
    (test-equal 3 (xpath-fn:hours-from-duration
		   (xs:make-day-time-duration "PT123H")))
    (test-equal -10 (xpath-fn:hours-from-duration
		     (xs:make-day-time-duration "-P3DT10H"))))

  (test-group "fn:minutes-from-duration"
    (test-equal 0 (xpath-fn:minutes-from-duration
		   (xs:make-day-time-duration "P3DT10H")))
    (test-equal -30 (xpath-fn:minutes-from-duration
		     (xs:make-day-time-duration "-P5DT12H30M"))))

  (test-group "fn:seconds-from-duration"
    (test-equal 12.5 (xpath-fn:seconds-from-duration
		      (xs:make-day-time-duration "P3DT10H12.5S")))
    (test-equal -16.0 (xpath-fn:seconds-from-duration
		       (xs:make-day-time-duration "-PT256S"))))

  (test-group "op:add-yearMonthDurations"
    (test-assert
     (xpath-op:duration-equal
      (xs:make-year-month-duration "P6Y2M")
      (xpath-op:add-year-month-durations (xs:make-year-month-duration "P2Y11M")
					 (xs:make-year-month-duration "P3Y3M")))))

  (test-group "op:subtract-yearMonthDurations"
    (test-assert
     (xpath-op:duration-equal
      (xs:make-year-month-duration "-P4M")
      (xpath-op:subtract-year-month-durations
       (xs:make-year-month-duration "P2Y11M")
       (xs:make-year-month-duration "P3Y3M")))))

  (test-group "op:multiply-yearMonthDurations"
    (test-assert
     (xpath-op:duration-equal
      (xs:make-year-month-duration "P6Y9M")
      (xpath-op:multiply-year-month-duration
       (xs:make-year-month-duration "P2Y11M") 2.3))))

  (test-group "op:divide-yearMonthDurations"
    (test-assert
     (xpath-op:duration-equal
      (xs:make-year-month-duration "P1Y11M")
      (xpath-op:divide-year-month-duration
       (xs:make-year-month-duration "P2Y11M") 1.5))))

  (test-group "op:divide-yearMonthDuration-by-yearMonthDuration"
    (test-equal -2.5 (xpath-op:divide-year-month-duration-by-year-month-duration
		      (xs:make-year-month-duration "P3Y4M")
		      (xs:make-year-month-duration "-P1Y4M"))))

  (test-group "op:add-dayTimeDurations"
    (test-assert
     (xpath-op:duration-equal
      (xs:make-day-time-duration "P8DT5M")
      (xpath-op:add-day-time-durations (xs:make-day-time-duration "P2DT12H5M")
					 (xs:make-day-time-duration "P5DT12H")))))

  (test-group "op:subtract-dayTimeDurations"
    (test-assert
     (xpath-op:duration-equal
      (xs:make-day-time-duration "P1DT1H30M")
      (xpath-op:subtract-day-time-durations
       (xs:make-day-time-duration "P2DT12H")
       (xs:make-day-time-duration "P1DT10H30M")))))

  (test-group "op:multiply-dayTimeDurations"
    (test-assert
     (xpath-op:duration-equal
      (xs:make-day-time-duration "PT4H33M")
      (xpath-op:multiply-day-time-duration
       (xs:make-day-time-duration "PT2H10M") 2.1))))

  (test-group "op:divide-dayTimeDurations"
    (test-assert
     (xpath-op:duration-equal
      (xs:make-day-time-duration "PT17H40M7S")
      (xpath-op:divide-day-time-duration
       (xs:make-day-time-duration "P1DT2H30M10.5S") 1.5))))

  (test-group "op:divide-dayTimeDuration-by-dayTimeDuration"
    (test-equal 175991.0 (xpath-op:divide-day-time-duration-by-day-time-duration
			  (xs:make-day-time-duration "P2DT53M11S")
			  (xs:make-day-time-duration "PT1S"))))
  )

(test-group "Functions and operators on dates and times"
  (define (datetime=? a b)
    (and (= (xs:datetime-year a) (xs:datetime-year b))
	 (= (xs:datetime-month a) (xs:datetime-month b))
	 (= (xs:datetime-day a) (xs:datetime-day b))
	 (= (xs:datetime-hour a) (xs:datetime-hour b))
	 (= (xs:datetime-minute a) (xs:datetime-minute b))
	 (= (xs:datetime-second a) (xs:datetime-second b))
	 (eqv? (xs:datetime-timezone-offset a) (xs:datetime-timezone-offset b))))

  (test-group "fn:dateTime"
    (test-assert
     (datetime=? (xpath-fn:datetime
		  (xs:make-date "1999-12-31") (xs:make-time "12:00:00"))
		 (xs:make-datetime "1999-12-31T12:00:00")))
    (test-assert
     (datetime=? (xpath-fn:datetime
		  (xs:make-date "1999-12-31") (xs:make-time "24:00:00"))
		 (xs:make-datetime "1999-12-31T00:00:00"))))

  (test-group "op:dateTime-equal"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (xpath-op:datetime-equal
		    (xs:make-datetime "2002-04-02T12:00:00-01:00")
		    (xs:make-datetime "2002-04-02T17:00:00+04:00")))
      (test-assert (xpath-op:datetime-equal
		    (xs:make-datetime "2002-04-02T12:00:00")
		    (xs:make-datetime "2002-04-02T23:00:00+06:00")))
      (test-assert (xpath-op:datetime-equal
		    (xs:make-datetime "2002-04-02T12:00:00")
		    (xs:make-datetime "2002-04-02T12:00:00")))
      (test-assert (xpath-op:datetime-equal
		    (xs:make-datetime "2002-04-02T23:00:00-04:00")
		    (xs:make-datetime "2002-04-03T02:00:00-01:00")))
      (test-assert (xpath-op:datetime-equal
		    (xs:make-datetime "1999-12-31T24:00:00")
		    (xs:make-datetime "2000-01-01T00:00:00"))))
    )
  (test-group "op:dateTime-less-than"
    (test-assert (xpath-op:datetime-less-than
		  (xs:make-datetime "2002-04-02T11:00:00-01:00")
		  (xs:make-datetime "2002-04-02T17:00:00+04:00"))))
  (test-group "op:dateTime-greater-than"
    (test-assert (xpath-op:datetime-greater-than
		  (xs:make-datetime "2002-04-02T17:00:00+04:00")
		  (xs:make-datetime "2002-04-02T11:00:00-01:00"))))

  (test-group "op:date-equal"
    (test-assert (not (xpath-op:date-equal
		       (xs:make-date "2004-12-25Z")
		       (xs:make-date "2004-12-25+07:00"))))
    (test-assert (xpath-op:date-equal
		  (xs:make-date "2004-12-25-12:00")
		  (xs:make-date "2004-12-26+12:00"))))
  (test-group "op:date-less-than"
    (test-assert (xpath-op:date-less-than
		  (xs:make-date "2004-12-25Z")
		  (xs:make-date "2004-12-25-05:00")))
    (test-assert (not (xpath-op:date-less-than
		       (xs:make-date "2004-12-25-12:00")
		       (xs:make-date "2004-12-26+12:00"))))
    )
  (test-group "op:date-greater-than"
    (test-assert (xpath-op:date-greater-than
		  (xs:make-date "2004-12-25Z")
		  (xs:make-date "2004-12-25+07:00")))
    (test-assert (not (xpath-op:date-greater-than
		       (xs:make-date "2004-12-25-12:00")
		       (xs:make-date "2004-12-26+12:00"))))
    )

  (test-group "op:time-equal"
    (test-assert (not (xpath-op:time-equal
		       (xs:make-time "08:00:00+09:00")
		       (xs:make-time "17:00:00-06:00"))))
    (test-assert (xpath-op:time-equal
		  (xs:make-time "21:30:00+10:30")
		  (xs:make-time "06:00:00-05:00")))
    (test-assert (xpath-op:time-equal
		  (xs:make-time "24:00:00+01:00")
		  (xs:make-time "00:00:00+01:00"))))
  (test-group "op:time-less-than"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (not (xpath-op:time-less-than
			 (xs:make-time "12:00:00")
			 (xs:make-time "23:00:00+06:00"))))
      (test-assert (xpath-op:time-less-than
		    (xs:make-time "11:00:00")
		    (xs:make-time "17:00:00Z")))
      (test-assert (not (xpath-op:time-less-than
			 (xs:make-time "23:59:59")
			 (xs:make-time "24:00:00"))))))

  (test-group "op:time-greater-than"
    (test-assert (not (xpath-op:time-greater-than
		       (xs:make-time "08:00:00+09:00")
		       (xs:make-time "17:00:00-06:00")))))

  (test-group "op:gYearMonth-equal"
    (test-assert (not (xpath-op:g-year-month-equal
		       (xs:make-g-year-month "1986-02")
		       (xs:make-g-year-month "1986-03"))))
    (test-assert (not (xpath-op:g-year-month-equal
		       (xs:make-g-year-month "1978-03")
		       (xs:make-g-year-month "1986-03Z")))))

  (test-group "op:gYear-equal"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (not (xpath-op:g-year-equal
			 (xs:make-g-year "2005-12:00")
			 (xs:make-g-year "2005+12:00"))))
      (test-assert (xpath-op:g-year-equal
		    (xs:make-g-year "1976-05:00")
		    (xs:make-g-year "1976")))))

  (test-group "op:gMonthDay-equal"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (xpath-op:g-month-day-equal
		    (xs:make-g-month-day "--12-25-14:00")
		    (xs:make-g-month-day "--12-26+10:00")))
      (test-assert (xpath-op:g-month-day-equal
		    (xs:make-g-month-day "--12-25-12:00")
		    (xs:make-g-month-day "--12-26+12:00")))
      (test-assert (not (xpath-op:g-month-day-equal
			 (xs:make-g-month-day "--12-25")
			 (xs:make-g-month-day "--12-26Z"))))))

  (test-group "op:gMonth-equal"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (not (xpath-op:g-month-equal
			 (xs:make-g-month "--12-14:00")
			 (xs:make-g-month "--12+10:00"))))
      (test-assert (not (xpath-op:g-month-equal
			 (xs:make-g-month "--12-12:00")
			 (xs:make-g-month "--12+12:00"))))
      (test-assert (not (xpath-op:g-month-equal
			 (xs:make-g-month "--12")
			 (xs:make-g-month "--12Z"))))))

  (test-group "op:gDay-equal"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (not (xpath-op:g-day-equal
			 (xs:make-g-day "---25-14:00")
			 (xs:make-g-day "---25+10:00"))))
      (test-assert (not (xpath-op:g-day-equal
			 (xs:make-g-day "---12")
			 (xs:make-g-day "---12Z"))))))

  (test-group "fn:year-from-dateTime"
    (test-equal 1999 (xpath-fn:year-from-datetime
		      (xs:make-datetime "1999-05-31T13:20:00-05:00")))
    (test-equal 1999 (xpath-fn:year-from-datetime
		      (xs:make-datetime "1999-05-31T21:30:00-05:00")))
    (test-equal 1999 (xpath-fn:year-from-datetime
		      (xs:make-datetime "1999-12-31T19:20:00")))
    (test-equal 2000 (xpath-fn:year-from-datetime
		      (xs:make-datetime "1999-12-31T24:00:00")))
    (test-equal -2   (xpath-fn:year-from-datetime
		      (xs:make-datetime "-0002-06-06T00:00:00"))))

  (test-group "fn:month-from-dateTime"
    (test-equal 5 (xpath-fn:month-from-datetime
		   (xs:make-datetime "1999-05-31T13:20:00-05:00")))
    (test-equal 12 (xpath-fn:month-from-datetime
		    (xs:make-datetime "1999-12-31T19:20:00-05:00")))
    (test-equal 1 (xpath-fn:month-from-datetime
		   (xpath-fn:adjust-datetime-to-timezone
		    (xs:make-datetime "1999-12-31T19:20:00-05:00")
		    (xs:make-day-time-duration "PT0S")))))
  (test-group "fn:day-from-dateTime"
    (test-equal 31 (xpath-fn:day-from-datetime
		    (xs:make-datetime "1999-05-31T13:20:00-05:00")))
    (test-equal 31 (xpath-fn:day-from-datetime
		    (xs:make-datetime "1999-12-31T20:00:00-05:00")))
    (test-equal 1 (xpath-fn:day-from-datetime
		   (xpath-fn:adjust-datetime-to-timezone
		    (xs:make-datetime "1999-12-31T19:20:00-05:00")
		    (xs:make-day-time-duration "PT0S")))))

  (test-group "fn:hours-from-dateTime"
    (test-equal 8 (xpath-fn:hours-from-datetime
		   (xs:make-datetime "1999-05-31T08:20:00-05:00")))
    (test-equal 21 (xpath-fn:hours-from-datetime
		    (xs:make-datetime "1999-12-31T21:20:00-05:00")))
    (test-equal 2 (xpath-fn:hours-from-datetime
		   (xpath-fn:adjust-datetime-to-timezone
		    (xs:make-datetime "1999-12-31T21:20:00-05:00")
		    (xs:make-day-time-duration "PT0S"))))
    (test-equal 12 (xpath-fn:hours-from-datetime
		    (xs:make-datetime "1999-12-31T12:00:00")))
    (test-equal 0 (xpath-fn:hours-from-datetime
		   (xs:make-datetime "1999-12-31T24:00:00"))))

  (test-group "fn:minutes-from-dateTime"
    (test-equal 20 (xpath-fn:minutes-from-datetime
		    (xs:make-datetime "1999-05-31T13:20:00-05:00")))
    (test-equal 30 (xpath-fn:minutes-from-datetime
		    (xs:make-datetime "1999-05-31T13:30:00+05:30"))))

  (test-group "fn:seconds-from-dateTime"
    (test-equal 0 (xpath-fn:seconds-from-datetime
		   (xs:make-datetime "1999-05-31T13:20:00-05:00"))))

  (test-group "fn:timezone-from-dateTime"
    (test-equal (xs:duration-seconds (xs:make-day-time-duration "-PT5H"))
		(xs:duration-seconds
		 (xpath-fn:timezone-from-datetime
		  (xs:make-datetime "1999-05-31T13:20:00-05:00"))))  
    (test-equal (xs:duration-seconds (xs:make-day-time-duration "PT0S"))
		(xs:duration-seconds
		 (xpath-fn:timezone-from-datetime
		  (xs:make-datetime "2000-06-12T13:20:00Z"))))
    (test-equal '()
		(xpath-fn:timezone-from-datetime
		 (xs:make-datetime "2004-08-27T00:00:00"))))

  (test-group "fn:adjust-dateTime-to-timezone"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (xs:datetime=?
		    (xs:make-datetime "2002-03-07T10:00:00-05:00")
		    (xpath-fn:adjust-datetime-to-timezone
		     (xs:make-datetime "2002-03-07T10:00:00"))))
      (test-assert (xs:datetime=?
		    (xs:make-datetime "2002-03-07T12:00:00-05:00")
		    (xpath-fn:adjust-datetime-to-timezone
		     (xs:make-datetime "2002-03-07T10:00:00-07:00"))))
      (test-assert (xs:datetime=?
		    (xs:make-datetime "2002-03-07T10:00:00-10:00")
		    (xpath-fn:adjust-datetime-to-timezone
		     (xs:make-datetime "2002-03-07T10:00:00")
		     (xs:make-day-time-duration "-PT10H"))))
      (test-assert (xs:datetime=?
		    (xs:make-datetime "2002-03-07T07:00:00-10:00")
		    (xpath-fn:adjust-datetime-to-timezone
		     (xs:make-datetime "2002-03-07T10:00:00-07:00")
		     (xs:make-day-time-duration "-PT10H"))))
      (test-assert (xs:datetime=?
		    (xs:make-datetime "2002-03-08T03:00:00+10:00")
		    (xpath-fn:adjust-datetime-to-timezone
		     (xs:make-datetime "2002-03-07T10:00:00-07:00")
		     (xs:make-day-time-duration "PT10H"))))
      (test-assert (xs:datetime=?
		    (xs:make-datetime "2002-03-07T10:00:00")
		    (xpath-fn:adjust-datetime-to-timezone
		     (xs:make-datetime "2002-03-07T10:00:00")
		     '())))
      (test-assert (xs:datetime=?
		    (xs:make-datetime "2002-03-07T10:00:00")
		    (xpath-fn:adjust-datetime-to-timezone
		     (xs:make-datetime "2002-03-07T10:00:00-07:00")
		     '())))))
  
  (test-group "fn:;adjust-date-to-timezone"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (xs:date=?
		    (xs:make-date "2002-03-07-05:00")
		    (xpath-fn:adjust-date-to-timezone
		     (xs:make-date "2002-03-07"))))
      (test-assert (xs:date=?
		    (xs:make-date "2002-03-07-05:00")
		    (xpath-fn:adjust-date-to-timezone
		     (xs:make-date "2002-03-07-07:00"))))
      (test-assert (xs:date=?
		    (xs:make-date "2002-03-07-10:00")
		    (xpath-fn:adjust-date-to-timezone
		     (xs:make-date "2002-03-07")
		     (xs:make-day-time-duration "-PT10H"))))
      (test-assert (xs:date=?
		    (xs:make-date "2002-03-06-10:00")
		    (xpath-fn:adjust-date-to-timezone
		     (xs:make-date "2002-03-07-07:00")
		     (xs:make-day-time-duration "-PT10H"))))
      (test-assert (xs:date=?
		    (xs:make-date "2002-03-07")
		    (xpath-fn:adjust-date-to-timezone
		     (xs:make-date "2002-03-07") '())))
      (test-assert (xs:date=?
		    (xs:make-date "2002-03-07")
		    (xpath-fn:adjust-date-to-timezone
		     (xs:make-date "2002-03-07-07:00") '())))))

  (test-group "fn:;adjust-time-to-timezone"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (xs:time=?
		    (xs:make-time "10:00:00-05:00")
		    (xpath-fn:adjust-time-to-timezone
		     (xs:make-time "10:00:00"))))
      (test-assert (xs:time=?
		    (xs:make-time "12:00:00-05:00")
		    (xpath-fn:adjust-time-to-timezone
		     (xs:make-time "10:00:00-07:00"))))
      (test-assert (xs:time=?
		    (xs:make-time "10:00:00-10:00")
		    (xpath-fn:adjust-time-to-timezone
		     (xs:make-time "10:00:00")
		     (xs:make-day-time-duration "-PT10H"))))
      (test-assert (xs:time=?
		    (xs:make-time "07:00:00-10:00")
		    (xpath-fn:adjust-time-to-timezone
		     (xs:make-time "10:00:00-07:00")
		     (xs:make-day-time-duration "-PT10H"))))
      (test-assert (xs:time=?
		    (xs:make-time "10:00:00")
		    (xpath-fn:adjust-time-to-timezone
		     (xs:make-time "10:00:00") '())))
      (test-assert (xs:time=?
		    (xs:make-time "10:00:00")
		    (xpath-fn:adjust-time-to-timezone
		     (xs:make-time "10:00:00-07:00") '())))
      (test-assert (xs:time=?
		    (xs:make-time "03:00:00+10:00")
		    (xpath-fn:adjust-time-to-timezone
		     (xs:make-time "10:00:00-07:00")
		     (xs:make-day-time-duration "PT10H"))))))

  (test-group "op:subtract-dateTimes"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (xpath-op:duration-equal
		    (xs:make-day-time-duration "P337DT2H12M")
		    (xpath-op:subtract-datetimes
		     (xs:make-datetime "2000-10-30T06:12:00")
		     (xs:make-datetime "1999-11-28T09:00:00Z"))))))
  (test-group "op:subtract-dates"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (xpath-op:duration-equal
		    (xs:make-day-time-duration "P5DT7H")
		    (xpath-op:subtract-dates
		     (xs:make-date "2000-10-15-05:00")
		     (xs:make-date "2000-10-10+02:00"))))))
  (test-group "op:subtract-times"
    (parameterize ((*xs:dynamic-timezone* (* -5 3600)))
      (test-assert (xpath-op:duration-equal
		    (xs:make-day-time-duration "PT2H12M")
		    (xpath-op:subtract-times
		     (xs:make-time "11:12:00Z")
		     (xs:make-time "04:00:00"))))
      (test-assert (xpath-op:duration-equal
		    (xs:make-day-time-duration "PT0S")
		    (xpath-op:subtract-times
		     (xs:make-time "11:00:00-05:00")
		     (xs:make-time "21:30:00+05:30"))))
      (test-assert (xpath-op:duration-equal
		    (xs:make-day-time-duration "P1D")
		    (xpath-op:subtract-times
		     (xs:make-time "17:00:00-06:00")
		     (xs:make-time "08:00:00+09:00"))))
      (test-assert (xpath-op:duration-equal
		    (xs:make-day-time-duration "-PT23H59M59S")
		    (xpath-op:subtract-times
		     (xs:make-time "24:00:00")
		     (xs:make-time "23:59:59"))))))

  (test-group "op:add-yearMonthDuration-to-dateTime"
    (test-assert (xs:datetime=?
		  (xs:make-datetime "2001-12-30T11:12:00")
		  (xpath-op:add-year-month-duration-to-datetime
		   (xs:make-datetime "2000-10-30T11:12:00")
		   (xs:make-year-month-duration "P1Y2M")))))
  (test-group "op:add-dayTimeDuration-to-dateTime"
    (test-assert (xs:datetime=?
		  (xs:make-datetime "2000-11-02T12:27:00")
		  (xpath-op:add-day-time-duration-to-datetime
		   (xs:make-datetime "2000-10-30T11:12:00")
		   (xs:make-day-time-duration "P3DT1H15M")))))
  (test-group "op:subtract-yearMonthDuration-from-dateTime"
    (test-assert (xs:datetime=?
		  (xs:make-datetime "1999-08-30T11:12:00")
		  (xpath-op:subtract-year-month-duration-from-datetime
		   (xs:make-datetime "2000-10-30T11:12:00")
		   (xs:make-year-month-duration "P1Y2M")))))
  (test-group "op:subtract-dayTimeDuration-from-dateTime"
    (test-assert (xs:datetime=?
		  (xs:make-datetime "2000-10-27T09:57:00")
		  (xpath-op:subtract-day-time-duration-from-datetime
		   (xs:make-datetime "2000-10-30T11:12:00")
		   (xs:make-day-time-duration "P3DT1H15M")))))

  (test-group "op:add-yearMonthDuration-to-date"
    (test-assert (xs:date=?
		  (xs:make-date "2001-12-30")
		  (xpath-op:add-year-month-duration-to-date
		   (xs:make-date "2000-10-30")
		   (xs:make-year-month-duration "P1Y2M")))))
  (test-group "op:add-dayTimeDuration-to-date"
    (test-assert (xs:date=?
		  (xs:make-date "2004-11-01Z")
		  (xpath-op:add-day-time-duration-to-date
		   (xs:make-date "2004-10-30Z")
		   (xs:make-day-time-duration "P2DT2H30M0S")))))
  (test-group "op:subtract-yearMonthDuration-from-date"
    (test-assert (xs:date=?
		  (xs:make-date "1999-08-30")
		  (xpath-op:subtract-year-month-duration-from-date
		   (xs:make-date "2000-10-30")
		   (xs:make-year-month-duration "P1Y2M"))))
    (test-expect-fail 1) ;; the result is 1999-03-01, seems more logical to me
    (test-assert (xs:date=?
		  (xs:make-date "1999-02-28Z")
		  (xpath-op:subtract-year-month-duration-from-date
		   (xs:make-date "2000-02-29Z")
		   (xs:make-year-month-duration "P1Y"))))
    (test-expect-fail 1) ;; the result is 1999-10-01, seems valid to me
    (test-assert (xs:date=?
		  (xs:make-date "1999-09-30-05:00")
		  (xpath-op:subtract-year-month-duration-from-date
		   (xs:make-date "2000-10-31-05:00")
		   (xs:make-year-month-duration "P1Y1M")))))

  (test-group "op:add-dayTimeDuration-to-time"
    (test-assert (xs:time=?
		  (xs:make-time "12:27:00")
		  (xpath-op:add-day-time-duration-to-time
		   (xs:make-time "11:12:00")
		   (xs:make-day-time-duration "P3DT1H15M"))))
    (test-assert (xs:time=?
		  (xs:make-time "02:27:00+03:00")
		  (xpath-op:add-day-time-duration-to-time
		   (xs:make-time "23:12:00+03:00")
		   (xs:make-day-time-duration "P1DT3H15M")))))
  (test-group "op:subtract-dayTimeDuration-from-time"
    (test-assert (xs:time=?
		  (xs:make-time "09:57:00")
		  (xpath-op:subtract-day-time-duration-from-time
		   (xs:make-time "11:12:00")
		   (xs:make-day-time-duration "P3DT1H15M"))))
    (test-assert (xs:time=?
		  (xs:make-time "22:10:00-05:00")
		  (xpath-op:subtract-day-time-duration-from-time
		   (xs:make-time "08:20:00-05:00")
		   (xs:make-day-time-duration "P23DT10H10M")))))

  (test-group "fn:parse-ietf-date"
    (test-assert (xs:datetime=?
		  (xs:make-datetime "1994-06-06T07:29:35Z")
		  (xpath-fn:parse-ietf-date "Wed, 06 Jun 1994 07:29:35 GMT")))
    (test-assert (xs:datetime=?
		  (xs:make-datetime "1994-06-06T07:29:35Z")
		  (xpath-fn:parse-ietf-date "Wed, 6 Jun 94 07:29:35 GMT")))
    (test-assert (xs:datetime=?
		  (xs:make-datetime "2013-06-06T11:54:45-05:00")
		  (xpath-fn:parse-ietf-date "Wed Jun 06 11:54:45 EST 2013")))
    (test-assert (xs:datetime=?
		  (xs:make-datetime "1994-11-06T08:49:37Z")
		  (xpath-fn:parse-ietf-date "Sunday, 06-Nov-94 08:49:37 GMT")))
    (test-assert (xs:datetime=?
		  (xs:make-datetime "1994-06-06T07:29:35+05:00")
		  (xpath-fn:parse-ietf-date "Wed, 6 Jun 94 07:29:35 +0500"))))
  
  )

(define (qname->list qname)
  (list (xs:qname-namespace-uri qname)
	(xs:qname-local-part qname)
	(xs:qname-prefix qname)))

(define (string->dom xml)
  (input-port->dom-tree (open-string-input-port xml)))

(test-group "Functions related to QNames"
  (test-group "fn:QName"
    (test-equal '("http://www.example.com/example" "person" "")
		(qname->list
		 (xpath-fn:qname "http://www.example.com/example" "person")))
    (test-equal '("http://www.example.com/example" "person" "ht")
		(qname->list
		 (xpath-fn:qname "http://www.example.com/example" "ht:person")))
    )
  (test-group "op:QName-equal"
    (test-assert (xpath-op:qname-equal
		  (xpath-fn:qname "http://www.example.com/example" "ht:person")
		  (xpath-fn:qname "http://www.example.com/example" "person"))))
  (test-group "fn:prefix-from-QName"
    (test-equal "ht"
		(xpath-fn:prefix-from-qname
		 (xpath-fn:qname "http://www.example.com/example" "ht:person")))
    (test-equal '()
		(xpath-fn:prefix-from-qname
		 (xpath-fn:qname "http://www.example.com/example" "person")))
    (test-equal '()
		(xpath-fn:prefix-from-qname '())))
  (test-group "fn:local-name-from-QName"
    (test-equal "person"
		(xpath-fn:local-name-from-qname
		 (xpath-fn:qname "http://www.example.com/example" "person"))))
  (test-group "fn:namespace-uri-from-QName"
    (test-equal "http://www.example.com/example"
		(xpath-fn:namespace-uri-from-qname
		 (xpath-fn:qname "http://www.example.com/example" "person"))))

  (test-group "fn:namespace-uri-for-prefix"
    (let* ((dom (string->dom "<z:a xmlns=\"http://example.org/one\" xmlns:z=\"http://example.org/two\">
  <b xmlns=\"\"/>
</z:a>"))
	   (e (document-document-element dom)))
      (test-equal "http://example.org/two"
		  (xpath-fn:namespace-uri-for-prefix "z" e))
      (test-equal "http://example.org/one"
		  (xpath-fn:namespace-uri-for-prefix "" e))
      (test-equal "http://example.org/one"
		  (xpath-fn:namespace-uri-for-prefix '() e))
      (test-equal "http://www.w3.org/XML/1998/namespace"
		  (xpath-fn:namespace-uri-for-prefix "xml" e))
      (test-equal "http://www.w3.org/2000/xmlns/"
		  (xpath-fn:namespace-uri-for-prefix "xmlns" e))))

  (test-group "fn:in-scope-prefixes"
    (let* ((dom (string->dom "<z:a xmlns=\"http://example.org/one\" xmlns:z=\"http://example.org/two\">
  <b xmlns=\"\"/>
</z:a>"))
	   (e (document-document-element dom)))
      (test-equal '("" "z" "xml" "xmlns") (xpath-fn:in-scope-prefixes (list e)))))
  )

(test-group "Operators on base64Binary and hexBinary"
  (test-group "op:hexBinary-equal"
    (test-assert (xpath-op:hex-binary-equal #vu8(1 2) #vu8(1 2))))
  (test-group "op:hexBinary-less-than"
    (test-assert (xpath-op:hex-binary-less-than #vu8(1 1) #vu8(1 2))))
  (test-group "op:hexBinary-greater-than"
    (test-assert (xpath-op:hex-binary-greater-than #vu8(1 2) #vu8(1 1))))

  (test-group "op:base64Binary-equal"
    (test-assert (xpath-op:base64-binary-equal "AQI=" "AQI=")))
  (test-group "op:base64Binary-less-than"
    (test-assert (xpath-op:base64-binary-less-than "AQI=" "AgE=")))
  (test-group "op:base64Binary-greater-than"
    (test-assert (xpath-op:base64-binary-greater-than "AgE=" "AQI=")))
  )


(test-group "Functions and operators on nodes"
  (test-group "fn:lang"
    (letrec ((check-lang (lambda (s lang)
			   (let* ((dom (string->dom s))
				  (e (document-document-element dom)))
			     (xpath-fn:lang lang e)))))
      (test-assert (check-lang "<para xml:lang=\"en\"/>" "en"))
      (test-assert (check-lang "<div xml:lang=\"en\"><para>And now, and forever!</para></div>" "en"))
      (test-assert (check-lang "<para xml:lang=\"EN\"/>" "en"))
      (test-assert (check-lang "<para xml:lang=\"en-us\"/>" "en"))
      (test-assert (not (check-lang "<para xml:lang=\"EN\"/>" "fr")))))
  (test-group "fn:root"
    (let* ((idoc (string->dom "<tool>wrench</tool>"))
	   (i (document-document-element idoc))
	   (odoc (string->dom "<order> <tool>wrench</tool> <quantity>5</quantity> </order>"))
	   (o (document-document-element odoc))
	   (o/quantity (node-list:item (document:get-elements-by-tag-name odoc "quantity") 0))
	   (newi (node-list:item (document:get-elements-by-tag-name odoc "tool") 0)))
      ;; DOM always contains document so fn:root always returns root document...
      (test-equal idoc (xpath-fn:root i))
      (test-equal odoc (xpath-fn:root o/quantity))))    
  )

(test-group "Functions and operators on sequences"
  (test-group "fn:empty"
    (test-assert (xpath-fn:empty '())))
  (test-group "fn:exists"
    (test-assert (xpath-fn:exists '(1)))
    (test-assert (xpath-fn:exists "")))
  (test-group "fn:head"
    (test-equal 1 (xpath-fn:head '(1 2 3 4 5)))
    (test-equal "a" (xpath-fn:head '("a" "b" "c")))
    (test-equal '() (xpath-fn:head '()))
    (test-equal '#(1 2 3) (xpath-fn:head '#(1 2 3))))
  (test-group "fn:tail"
    (test-equal '(2 3 4 5) (xpath-fn:tail '(1 2 3 4 5)))
    (test-equal '("b" "c") (xpath-fn:tail '("a" "b" "c")))
    (test-equal '() (xpath-fn:tail '("a")))
    (test-equal '() (xpath-fn:tail '()))
    (test-equal '() (xpath-fn:tail '#(1 2 3))))
  (test-group "fn:insert-before"
    (let ((abc '("a" "b" "c")))
      (test-equal '("z" "a" "b" "c") (xpath-fn:insert-before abc 0 "z"))
      (test-equal '("z" "a" "b" "c") (xpath-fn:insert-before abc 1 "z"))
      (test-equal '("a" "z" "b" "c") (xpath-fn:insert-before abc 2 "z"))
      (test-equal '("a" "b" "z" "c") (xpath-fn:insert-before abc 3 "z"))
      (test-equal '("a" "b" "c" "z") (xpath-fn:insert-before abc 4 "z"))))
  (test-group "fn:remove"
    (let ((abc '("a" "b" "c")))
      (test-group '("a" "b" "c") (xpath-fn:remove abc 0))
      (test-group '("b" "c") (xpath-fn:remove abc 1))
      (test-group '("a" "b" "c") (xpath-fn:remove abc 6))
      (test-group '() (xpath-fn:remove '() 3))))
  )
	  
  
(test-end)

;; Local Variables:
;; eval: (put 'test-group 'scheme-indent-function 1)
;; End:

