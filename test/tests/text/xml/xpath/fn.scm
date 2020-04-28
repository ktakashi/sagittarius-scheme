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
  
  )

(test-end)

;; Local Variables:
;; eval: (put 'test-group 'scheme-indent-function 1)
;; End:
