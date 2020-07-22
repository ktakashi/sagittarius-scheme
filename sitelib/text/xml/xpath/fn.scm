;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/xpath/fn.scm - XPath Functions and Operators
;;;
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; ref:
;;  XPath and XQuery Functions and Operators 3.1
;;  https://www.w3.org/TR/xpath-functions-31/

#!nounbound
(library (text xml xpath fn)
    (export xpath-fn:node-name
	    xpath-fn:nilled
	    xpath-fn:string
	    xpath-fn:data
	    xpath-fn:base-uri
	    xpath-fn:document-uri
	    xpath-fn:error
	    xpath-fn:trace
	    xpath-op:numeric-add
	    xpath-op:numeric-subtract
	    xpath-op:numeric-multiply
	    xpath-op:numeric-divide
	    xpath-op:numeric-integer-divide
	    xpath-op:numeric-mod
	    xpath-op:numeric-unary-plus
	    xpath-op:numeric-unary-minus
	    xpath-op:numeric-equal
	    xpath-op:numeric-less-than
	    xpath-op:numeric-greater-than
	    xpath-fn:abs
	    xpath-fn:ceiling
	    xpath-fn:floor
	    xpath-fn:round
	    xpath-fn:round-half-to-even
	    xpath-fn:number
	    xpath-fn:format-integer
	    xpath-fn:format-number
	    xpath-math:pi
	    xpath-math:exp
	    xpath-math:exp10
	    xpath-math:log
	    xpath-math:log10
	    xpath-math:pow
	    xpath-math:sqrt
	    xpath-math:sin
	    xpath-math:cos
	    xpath-math:tan
	    xpath-math:asin
	    xpath-math:acos
	    xpath-math:atan
	    xpath-math:atan2
	    xpath-fn:random-number-generator
	    xpath-fn:codepoints-to-string
	    xpath-fn:string-to-codepoints
	    xpath-fn:compare
	    xpath-fn:codepoint-equal
	    xpath-fn:collation-key
	    xpath-fn:contains-token
	    xpath-fn:concat
	    xpath-fn:string-join
	    xpath-fn:substring
	    xpath-fn:string-length
	    xpath-fn:normalize-space
	    xpath-fn:normalize-unicode
	    xpath-fn:upper-case
	    xpath-fn:lower-case
	    xpath-fn:translate
	    xpath-fn:contains
	    xpath-fn:starts-with
	    xpath-fn:ends-with
	    xpath-fn:substring-before
	    xpath-fn:substring-after
	    xpath-fn:matches
	    xpath-fn:replace
	    xpath-fn:tokenize
	    xpath-fn:analyze-string
	    xpath-fn:resolve-uri
	    xpath-fn:encode-for-uri
	    xpath-fn:iri-to-uri
	    xpath-fn:escape-html-uri
	    xpath-fn:true
	    xpath-fn:false
	    xpath-op:boolean-equal
	    xpath-op:boolean-less-than
	    xpath-op:boolean-greater-than
	    xpath-fn:boolean
	    xpath-fn:not
	    xpath-op:year-month-duration-less-than
	    xpath-op:year-month-duration-greater-than
	    xpath-op:day-time-duration-less-than
	    xpath-op:day-time-duration-greater-than
	    xpath-op:duration-equal
	    xpath-fn:years-from-duration
	    xpath-fn:months-from-duration
	    xpath-fn:days-from-duration
	    xpath-fn:hours-from-duration
	    xpath-fn:minutes-from-duration
	    xpath-fn:seconds-from-duration
	    xpath-op:add-year-month-durations
	    xpath-op:subtract-year-month-durations
	    xpath-op:multiply-year-month-duration
	    xpath-op:divide-year-month-duration
	    xpath-op:divide-year-month-duration-by-year-month-duration
	    xpath-op:add-day-time-durations
	    xpath-op:subtract-day-time-durations
	    xpath-op:multiply-day-time-duration
	    xpath-op:divide-day-time-duration
	    xpath-op:divide-day-time-duration-by-day-time-duration
	    xpath-fn:datetime
	    xpath-op:datetime-equal
	    xpath-op:datetime-less-than
	    xpath-op:datetime-greater-than
	    xpath-op:date-equal
	    xpath-op:date-less-than
	    xpath-op:date-greater-than
	    xpath-op:time-equal
	    xpath-op:time-less-than
	    xpath-op:time-greater-than
	    xpath-op:g-year-month-equal
	    xpath-op:g-year-equal
	    xpath-op:g-month-day-equal
	    xpath-op:g-month-equal
	    xpath-op:g-day-equal
	    xpath-fn:year-from-datetime
	    xpath-fn:month-from-datetime
	    xpath-fn:day-from-datetime
	    xpath-fn:hours-from-datetime
	    xpath-fn:minutes-from-datetime
	    xpath-fn:seconds-from-datetime
	    xpath-fn:timezone-from-datetime
	    xpath-fn:year-from-date
	    xpath-fn:month-from-date
	    xpath-fn:day-from-date
	    xpath-fn:timezone-from-date
	    xpath-fn:hours-from-time
	    xpath-fn:minutes-from-time
	    xpath-fn:seconds-from-time
	    xpath-fn:timezone-from-time
	    
	    xpath-fn:adjust-datetime-to-timezone
	    xpath-fn:adjust-date-to-timezone
	    xpath-fn:adjust-time-to-timezone
	    xpath-op:subtract-datetimes
	    xpath-op:subtract-dates
	    xpath-op:subtract-times
	    xpath-op:add-year-month-duration-to-datetime
	    xpath-op:add-day-time-duration-to-datetime
	    xpath-op:subtract-year-month-duration-from-datetime
	    xpath-op:subtract-day-time-duration-from-datetime
	    xpath-op:add-year-month-duration-to-date
	    xpath-op:add-day-time-duration-to-date
	    xpath-op:subtract-year-month-duration-from-date
	    xpath-op:subtract-day-time-duration-from-date
	    xpath-op:add-day-time-duration-to-time
	    xpath-op:subtract-day-time-duration-from-time
	    
	    xpath-fn:format-datetime
	    xpath-fn:format-date
	    xpath-fn:format-time
	    xpath-fn:parse-ietf-date
	    xpath-fn:resolve-qname
	    xpath-fn:qname
	    xpath-op:qname-equal
	    xpath-fn:prefix-from-qname
	    xpath-fn:local-name-from-qname
	    xpath-fn:namespace-uri-from-qname
	    xpath-fn:namespace-uri-for-prefix
	    xpath-fn:in-scope-prefixes
	    xpath-op:hex-binary-equal
	    xpath-op:hex-binary-less-than
	    xpath-op:hex-binary-greater-than
	    xpath-op:base64-binary-equal
	    xpath-op:base64-binary-less-than
	    xpath-op:base64-binary-greater-than
	    xpath-op:notation-equal
	    xpath-fn:name
	    xpath-fn:local-name
	    xpath-fn:namespace-uri
	    xpath-fn:lang
	    xpath-fn:root
	    xpath-fn:path
	    xpath-fn:has-children
	    xpath-fn:outermost
	    xpath-fn:empty
	    xpath-fn:exists
	    xpath-fn:head
	    xpath-fn:tail
	    xpath-fn:insert-before
	    xpath-fn:remove
	    xpath-fn:reverse
	    xpath-fn:subsequence
	    xpath-fn:unordered
	    xpath-fn:distinct-values
	    xpath-fn:index-of
	    xpath-fn:deep-equal
	    xpath-fn:zero-or-one
	    xpath-fn:one-or-more
	    xpath-fn:exactly-one
	    xpath-fn:count)
    (import (rnrs)
	    (rnrs r5rs)
	    (peg)
	    (peg chars)
	    (rfc base64)
	    (rfc uri)
	    (sagittarius generators)
	    (sagittarius regex)
	    (sagittarius timezone)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :43 vectors)
	    (srfi :115 regexp)
	    (srfi :127 lseqs)
	    (srfi :144 flonums)	    
	    (text xml errors)
	    (text xml dom)
	    (only (text xml dom parser) +xml:char-set+)
	    (text xml schema)
	    (text xml xpath dm)
	    (text xml xpath tools)
	    (util bytevector))

;;; 2 Accessors
;;; All accessor requires the $arg argument, the XPath evaluator
;;; must handle the context item.
(define (xpty0004-error who arg)
  (xqt-error 'XPTY0004 who "Invalid argument" arg))
(define-syntax dm:delegate
  (syntax-rules ()
    ((_ who delegate)
     (let ((proc delegate))
       (lambda (arg)
	 (cond ((null? arg) '())
	       ((not (node? arg))
		(xqt-error 'XPTY0004 'who "Not a node" arg))
	       (else (proc arg))))))))

;;;; 2.1 fn:node-name
;;;; fn:node-name($arg as node()?) as xs:QName?
(define xpath-fn:node-name (dm:delegate xpath-fn:node-name xpath-dm:node-name))

;;;; 2.2 fn:nilled
(define xpath-fn:nilled (dm:delegate xpath-fn:nilled xpath-dm:nilled))

;;;; 2.3 fn:string
(define xpath-fn:string
  (let ((delegate (dm:delegate xpath-fn:string xpath-dm:string-value)))
    (lambda (arg)
      (cond ((null? arg) "")
	    ((xs:any-atomic-type? arg) (atomic->string 'xpath-fn:string arg))
	    (else (delegate arg))))))

;;;; 2.4 fn:data
(define xpath-fn:data
  (let ((delegate (dm:delegate xpath-fn:data xpath-dm:typed-value)))
    (lambda (arg)
      (cond ((pair? arg) (map xpath-fn:data arg))
	    ;; ((array? args) ...)
	    ((xs:any-atomic-type? arg) arg) ;; correct?
	    (else (delegate arg))))))

;;;; 2.5 fn:base-uri
(define xpath-fn:base-uri (dm:delegate xpath-fn:base-uri xpath-dm:base-uri))

;;;; 2.6 fn:document-uri
(define xpath-fn:document-uri
  (dm:delegate xpath-fn:document-uri xpath-dm:document-uri))

;;; 3 Errors and diagnostics
;;;; 3.1.1 fn:error
;;;; fn:error() as none
;;;; fn:error($code as xs:QName?) as none
;;;; fn:error($code as xs:QName?, $description as xs:string) as none
;;;; fn:error($code         as xs:QName?,
;;;;          $description  as xs:string,
;;;;          $error-object as item()*) as none
(define +default-error-code+
  (xs:make-qname "http://www.w3.org/2005/xqt-errors" "FOER0000" "err"))
(define (search-error-description type)
  (cond ((assq (string->symbol type) +xqt-errors+) => cadr)
	(else "Unknown reason")))
(define xpath-fn:error
  (case-lambda
   (() (xpath-fn:error +default-error-code+))
   ((qname)
    (xpath-fn:error qname
		    (search-error-description (xs:qname-local-part qname)) '()))
   ((qname description error-object)
    ;; TODO should we check the namespace?
    (if (null? error-object)
	(xqt-error (string->symbol (xs:qname-local-part qname))
		   'xpath-fn:error description)
	(xqt-error (string->symbol (xs:qname-local-part qname))
		   'xpath-fn:error description error-object)))))

;;;; 3.2.1 fn:trace
(define (xpath-fn:trace . args)
  (implementation-restriction-violation 'xpath-fn:trace
					"xpath-fn:trace is not supported"))

;;; 4 Functions and operators on numerics
(define-syntax fn:delegate-numeric-op
  (syntax-rules ()
    ((_ op) (lambda (v1 v2) (op v1 v2)))))
;;;; 4.2.1 op:numeric-add
(define xpath-op:numeric-add (fn:delegate-numeric-op +))
;;;; 4.2.2 op:numeric-subtract
(define xpath-op:numeric-subtract (fn:delegate-numeric-op -))
;;;; 4.2.3 op:numeric-multiply
(define xpath-op:numeric-multiply (fn:delegate-numeric-op *))
;;;; 4.2.4 op:numeric-divide
(define (xpath-op:numeric-divide v1 v2)
  (and (integer? v2) (zero? v2)
       (xqt-error 'FOAR0001 'xpath-op:numeric-divide "Dividing by 0" v1 v2))
  (let ((r (/ v1 v2)))
    (if (and (not (flonum? r)) (not (= (denominator r) 1)))
	(inexact r)
	r)))
;;;; 4.2.5 op:numeric-integer-divide
(define (xpath-op:numeric-integer-divide x y)
  (when (zero? y)
    (xqt-error 'FOAR0001 'xpath-op:numeric-integer-divide "Dividing by 0" x y))
  (when (infinite? x)
    (xqt-error 'FOAR0002 'xpath-op:numeric-integer-divide "Argument is INF" x y))
  (if (infinite? x)
      0
      (let ((r (/ x y)))
	(if (negative? r)
	    (exact (ceiling r))
	    (exact (floor r))))))
;;;; 4.2.6 op:numeric-mod
(define (xpath-op:numeric-mod v1 v2)
  (and (zero? v2)
       (xqt-error 'FOAR0001 'xpath-op:numeric-mod "Dividing by 0" v1 v2))
  (mod v1 v2))

;;;; 4.2.7 op:numeric-unary-plus
(define (xpath-op:numeric-unary-plus x) (+ x))
;;;; 4.2.8 op:numeric-unary-minus
(define (xpath-op:numeric-unary-minus x) (- x))

;;;; 4.3.1 op:numeric-equal
(define xpath-op:numeric-equal (fn:delegate-numeric-op =))
;;;; 4.3.2 op:numeric-less-than
(define xpath-op:numeric-less-than (fn:delegate-numeric-op <))
;;;; 4.3.3 op:numeric-greater-than
(define xpath-op:numeric-greater-than (fn:delegate-numeric-op >))

(define-syntax fn:delegate-numeric-unary-fn
  (syntax-rules ()
    ;; TODO type check but which error?
    ((_ fn) (lambda (v1) (fn v1)))))
;;;; 4.4.1 fn:abs
(define xpath-fn:abs (fn:delegate-numeric-unary-fn abs))
;;;; 4.4.2 fn:ceiling
(define xpath-fn:ceiling (fn:delegate-numeric-unary-fn ceiling))
;;;; 4.4.3 fn:floor
(define xpath-fn:floor (fn:delegate-numeric-unary-fn floor))
;;;; 4.4.4 fn:round
(define xpath-fn:round
  (case-lambda
   ((arg) (xpath-fn:round arg 0))
   ((arg precision)
    ;; for now it's not a good way of doing it :(
    (let ((lift (expt 10.0 precision)))
      (let-values (((i f) (flinteger-fraction (* arg lift))))
	(/ (if (>= f 0.5) (+ i 1) i) lift))))))
;;;; 4.4.5 fn:round-half-to-even
(define xpath-fn:round-half-to-even
  (case-lambda
   ((arg) (xpath-fn:round-half-to-even arg 0))
   ((arg precision)
    (let ((lift (expt 10.0 precision)))
      (/ (round (* arg lift)) lift)))))

;;;; 4.5.1 fn:number
;;;; fn:number($arg as xs:anyAtomicType?) as xs:double
(define (xpath-fn:number arg)
  (cond ((string->number (xpath-fn:string arg)) => inexact)
	(else +nan.0)))

;;;; 4.6.1 fn:format-integer
(define xpath-fn:format-integer
  (case-lambda
   ((value picture)
    (xpath-fn:format-integer value picture "en"))
   ((value picture lang)
    (implementation-restriction-violation 'xpath-fn:format-integer
					  "Not supported yet"))))

;;;; 4.7.2 fn:format-number
(define xpath-fn:format-number
  (case-lambda
   ((value picture)
    (xpath-fn:format-number value picture "default"))
   ((value picture decimal-format-name)
    (implementation-restriction-violation 'xpath-fn:format-number
					  "Not supported yet"))))

;;;; 4.8.1 math:pi
(define (xpath-math:pi) fl-pi)

;;;; 4.8.2 math:exp
(define (xpath-math:exp x) (exp (inexact x)))
;;;; 4.8.3 math:exp10
(define (xpath-math:exp10 x) (expt 10.0 x))
(define-syntax ->nan
  (syntax-rules ()
    ((_ exp)
     (let ((r exp)) (if (real? r) r +nan.0)))))
;;;; 4.8.4 math:log
(define (xpath-math:log x) (->nan (log (inexact x))))
;;;; 4.8.5 math:log10
(define (xpath-math:log10 x) (->nan (log (inexact x) 10)))
;;;; 4.8.6 math:pow
(define (xpath-math:pow x y)
  (inexact (expt x y)))
;;;; 4.8.7 math:sqrt
(define (xpath-math:sqrt x) (->nan (inexact (sqrt x))))
;;;; 4.8.8 math:sin
(define (xpath-math:sin x) (->nan (inexact (sin x))))
;;;; 4.8.9 math:cos
(define (xpath-math:cos x) (->nan (inexact (cos x))))
;;;; 4.8.10 math:tan
(define (xpath-math:tan x) (->nan (inexact (tan x))))
;;;; 4.8.11 math:asin
(define (xpath-math:asin x) (->nan (inexact (asin x))))
;;;; 4.8.12 math:acos
(define (xpath-math:acos x) (->nan (inexact (acos x))))
;;;; 4.8.13 math:atan
(define (xpath-math:atan x) (->nan (inexact (atan x))))
;;;; 4.8.14 math:atan2
(define (xpath-math:atan2 x y) (->nan (inexact (atan x y))))

;;;; 4.9.1 fn:random-number-generator
(define (xpath-fn:random-number-generator seed)
  (implementation-restriction-violation 'xpath-fn:random-number-generator
					"Not supported"))

;;; 5 Functions on strings
;;;; 5.2.1 fn:codepoints-to-string
(define (xpath-fn:codepoints-to-string codepoints)
  (define (integer->xml-char i)
    (let ((c (integer->char i)))
      (unless (char-set-contains? +xml:char-set+ c)
	(xqt-error 'FOCH0001 'xpath-fn:codepoints-to-string
		   "Invalid XML char" c))
      c))
  (list->string (map integer->xml-char codepoints)))

;;;; 5.2.2 fn:string-to-codepoints
(define (xpath-fn:string-to-codepoints str)
  (map char->integer (string->list str)))


(define +default-collation+ "default")
;;;; 5.3.6 fn:compare
(define xpath-fn:compare
  (case-lambda
   ((arg0 arg1) (xpath-fn:compare arg0 arg1 +default-collation+))
   ((s0 s1 collation)
    ;; TODO support collation
    (unless (string=? collation +default-collation+)
      (xqt-error 'FOCH0002 'xpath-fn:compare "Not supported" collation))
    (string-compare s0 s1 (lambda (_) -1) (lambda (_) 0) (lambda (_) 1)))))

;;;; 5.3.7 fn:codepoint-equal
(define (xpath-fn:codepoint-equal s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (string=? s1 s2)))

;;;; 5.3.8 fn:collation-key
(define xpath-fn:collation-key
  (case-lambda
   ((key) (xpath-fn:collation-key key +default-collation+))
   ((key collation)
    (implementation-restriction-violation 'xpath-fn:collation-key "Not supported yet"))))

;;;; 5.3.9 fn:contains-token
(define xpath-fn:contains-token
  (case-lambda
   ((input token) (xpath-fn:contains-token input token +default-collation+))
   ((input token collation)
    (implementation-restriction-violation 'xpath-fn:collation-token
					  "Not supported yet"))))

;;;; 5.4.1 fn:concat
(define (xpath-fn:concat s1 s2 . s*)
  ;; TODO very inefficient...
  (string-concatenate
   (map (lambda (e) (atomic->string 'xpath-fn:concat e)) (cons* s1 s2 s*))))

;;;; 5.4.2 fn:string-join
(define xpath-fn:string-join
  (case-lambda
   ((s*) (xpath-fn:string-join s* ""))
   ((s* delim)
    (string-join (map (lambda (e) (atomic->string 'xpath-fn:string-join e)) s*) delim))))

;;;; 5.4.3 fn:substring
(define xpath-fn:substring
  (case-lambda
   ((src start)
    (let ((len (string-length src)))
      (xpath-fn:substring src start (+ (- len start) 1))))
   ((src start length)
    (define s (xpath-fn:round start))
    (define l (xpath-fn:round length))
    (cond ((null? src) "")
	  ((or (nan? s) (nan? l)) "")
	  (else
	   (let ((start (max 1 s))
		 (end   (max start (+ s l))))
	     (cond ((nan? end) "") ;; (+ +inf.0 -inf.0)
		   ((infinite? start) "")
		   (else
		    (let* ((s (exact start))
			   (e (if (infinite? end)
				  ;; handling inifinate is a bit silly here...
				  (if (negative? end)
				      s
				      (+ (- (string-length src) (- s 1)) 1))
				  (exact end))))
		      (substring src (- s 1) (- e 1)))))))))))

;;;; 5.4.4 fn:string-length
(define (xpath-fn:string-length arg)
  (if (null? arg)
      0
      (string-length arg)))

;;;; 5.4.5 fn:normalize-space
(define (xpath-fn:normalize-space arg)
  (define (space? c) (memv c '(#\x20 #\x9 #\xD #\xA)))
  (if (null? arg)
      ""
      (let-values (((out e) (open-string-output-port)))
	;; TODO a bit inefficient...
	(define str (string-trim-both arg space?))
	;; TODO maybe should use cursor SRFI for better portability
	;; but no plan to make this portable so forget about it for now
	(let loop ((i 0) (prev-space? #f))
	  (cond ((= (string-length str) i) (e))
		((space? (string-ref str i))
		 (unless prev-space? (put-char out #\x20))
		 (loop (+ i 1) #t))
		(else (put-char out (string-ref str i)) (loop (+ i 1) #f)))))))

;;;; 5.4.6 fn:normalize-unicode
(define xpath-fn:normalize-unicode
  (case-lambda
   ((arg) (xpath-fn:normalize-unicode arg "NFC"))
   ((arg form)
    (case (string->symbol form)
      ((NFC) (string-normalize-nfc arg))
      ((NFD) (string-normalize-nfd arg))
      ((NFKC) (string-normalize-nfkc arg))
      ((NFKD) (string-normalize-nfkd arg))
      (else (xqt-error 'FOCH0003 'xpath-fn:normalize-unicode
		       "Unsupported normalization" form))))))

;;;; 5.4.7 fn:upper-case
(define (xpath-fn:upper-case arg) (string-upcase arg))
;;;; 5.4.8 fn:lower-case
(define (xpath-fn:lower-case arg) (string-downcase arg))

;;;; 5.4.9 fn:translate
(define (xpath-fn:translate arg map-s trans-s)
  (list->string
   (filter-map
    (lambda (c)
      (let ((i (string-index map-s c)))
	(cond ((and i (< i (string-length trans-s)) (string-ref trans-s i)))
	      ((and i (>= i (string-length trans-s))) #f)
	      (else c)))) (string->list arg))))

;;;; 5.5.1 fn:contains
(define xpath-fn:contains
  (case-lambda
   ((s1 s2) (xpath-fn:contains s1 s2 +default-collation+))
   ((s1 s2 collation)
    (unless (string=? collation +default-collation+)
      (xqt-error 'FOCH0004 'xpath-fn:contains "Not supported" collation))
    (cond ((and (null? s1) (null? s2))) ;; "" contains ""
	  ((null? s2))			;; s1 contains ""
	  ((null? s1) #f)		;; "" contains s2
	  (else (and (string-contains s1 s2) #t))))))

;;;; 5.5.2 fn:starts-with
(define xpath-fn:starts-with
  (case-lambda
   ((s1 s2) (xpath-fn:starts-with s1 s2 +default-collation+))
   ((s1 s2 collation)
    (unless (string=? collation +default-collation+)
      (xqt-error 'FOCH0004 'xpath-fn:starts-with "Not supported" collation))
    (cond ((and (null? s1) (null? s2))) ;; "" starts with ""
	  ((null? s2))			;; s1 starts with ""
	  ((null? s1) #f)		;; "" starts with s2
	  (else (string-prefix? s2 s1))))))

;;;; 5.5.3 fn:ends-with
(define xpath-fn:ends-with
  (case-lambda
   ((s1 s2) (xpath-fn:ends-with s1 s2 +default-collation+))
   ((s1 s2 collation)
    (unless (string=? collation +default-collation+)
      (xqt-error 'FOCH0004 'xpath-fn:ends-with "Not supported" collation))
    (cond ((and (null? s1) (null? s2))) ;; "" ends with ""
	  ((null? s2))			;; s1 ends with ""
	  ((null? s1) #f)		;; "" ends with s2
	  (else (string-suffix? s2 s1))))))

;;;; 5.5.4 fn:substring-before
(define xpath-fn:substring-before
  (case-lambda
   ((s1 s2) (xpath-fn:substring-before s1 s2 +default-collation+))
   ((s1 s2 collation)
    (unless (string=? collation +default-collation+)
      (xqt-error 'FOCH0004 'xpath-fn:substring-before "Not supported" collation))
    (cond ((and (null? s1) (null? s2)) "") ;; "" substring before ""
	  ((null? s2) "")		   ;; s1 substring before ""
	  ((null? s1) "")		   ;; "" substring before s2
	  ((string-contains s1 s2) => (lambda (i) (substring s1 0 i)))
	  (else "")))))

;;;; 5.5.5 fn:substring-after
(define xpath-fn:substring-after
  (case-lambda
   ((s1 s2) (xpath-fn:substring-after s1 s2 +default-collation+))
   ((s1 s2 collation)
    (unless (string=? collation +default-collation+)
      (xqt-error 'FOCH0004 'xpath-fn:substring-after "Not supported" collation))
    (cond ((and (null? s1) (null? s2)) "") ;; "" substring before ""
	  ((null? s2) "")		   ;; s1 substring before ""
	  ((null? s1) "")		   ;; "" substring before s2
	  ((string-contains s1 s2) =>
	   (lambda (i) (substring s1 (+ i (string-length s2)) (string-length s1))))
	  (else "")))))

;; helper
(define +regex-flags+
  `((#\s . ,DOTALL)
    (#\m . ,MULTILINE)
    (#\i . ,CASE-INSENSITIVE)
    (#\x . ,COMMENTS)
    (#\q . ,LITERAL)))
(define (->regex-flag who c)
  (cond ((assv c +regex-flags+) => cdr)
	(else (xqt-error 'FORX0001 who "Invalid flag" c))))
(define (string-flags->flags who flags)
  (define (->flag c) (->regex-flag who c))
  (fold-left bitwise-ior 0 (map ->flag (string->list flags))))
;;;; 5.6.3 fn:matches
(define xpath-fn:matches
  (case-lambda
   ((input pattern) (xpath-fn:matches input pattern ""))
   ((input pattern flags)
    (let ((flags (string-flags->flags 'xpath-fn:matches flags)))
      (guard (e (else (xqt-error 'FORX0002 'xpath-fn:matches "Invalid pattern" pattern)))
	(looking-at (regex pattern flags) input))))))

(define (check-pattern who input pattern flags)
  (when (looking-at (regex pattern flags) "")
    (xqt-error 'FORX0003 who "Pattern matches empty string" pattern)))
;;;; 5.6.4 fn:replace
(define xpath-fn:replace
  (case-lambda
   ((input pattern replacement) (xpath-fn:replace input pattern replacement ""))
   ((input pattern replacement flags)
    (let ((flags (string-flags->flags 'xpath-fn:replace flags)))
      (check-pattern 'xpath-fn:replace input pattern flags)
      ;; TODO not really correct..
      (guard (e (else (xqt-error 'FORX0004 'xpath-fn:replace (condition-message e))))
	(regex-replace-all (regex pattern flags) input replacement))))))

;;;; 5.6.5 fn:tokenize
(define xpath-fn:tokenize
  (case-lambda
   ((input) (xpath-fn:tokenize (xpath-fn:normalize-space input) " "))
   ((input pattern) (xpath-fn:tokenize input pattern ""))
   ((input pattern flags)
    (let ((flags (string-flags->flags 'xpath-fn:tokenize flags)))
      (check-pattern 'xpath-fn:tokenize input pattern flags)
      (guard (e (else (xqt-error 'FORX0002 'xpath-fn:tokenize (condition-message e))))
	(regexp-split (regex pattern flags) input))))))

;;;; 5.6.6 fn:analyze-string
(define xpath-fn:analyze-string
  (case-lambda
   ((input pattern) (xpath-fn:analyze-string input pattern ""))
   ((input pattern flags)
    (string-flags->flags 'xpath-fn:analyze-string flags) ;; for fun
    (implementation-restriction-violation 'xpath-fn:analyze-string "Not supported"))))

;;;; 6.1 fn:resolve-uri
(define not-supplied (list '()))
(define xpath-fn:resolve-uri
  (case-lambda
   ((relative) (xpath-fn:resolve-uri relative not-supplied))
   ((relative base)
    (define (absoluete-iri? uri)
      (let-values (((scheme specific) (uri-scheme&specific uri)))
	(and scheme #t)))
    (cond ((null? relative) '())
	  ((absoluete-iri? relative) relative)
	  ((eq? not-supplied base)
	   (xqt-error 'FONS0005 xpath-fn:resolve-uri "Base is not provided" relative))
	  (else (uri-merge base relative))))))

;;;; 6.2 fn:encode-for-uri
(define (xpath-fn:encode-for-uri uri-part)
  (if (null? uri-part)
      ""
      (uri-encode-string uri-part)))

;;;; 6.3 fn:iri-to-uri
(define (xpath-fn:iri-to-uri iri)
  (if (null? iri)
      ""
      (let*-values (((scheme specific) (uri-scheme&specific iri))
		    ((auth path query frag) (uri-decompose-hierarchical specific)))
	(define (encode p) (uri-encode-string (uri-decode-string p)))
	(uri-compose :scheme scheme
		     :authority auth
		     :path (and path
				(string-join (map encode (string-split path "/")) "/"))
		     :query (and query (uri-encode-string query))
		     :fragment (and frag (uri-encode-string frag))))))

;;;; 6.4 fn:escape-html-uri
(define us-ascii-printables (char-set-intersection char-set:ascii char-set:printing))
(define (xpath-fn:escape-html-uri uri)
  (if (null? uri)
      ""
      (uri-encode-string uri :noescape us-ascii-printables)))

;;;; 7.1.1 fn:true
(define (xpath-fn:true) #t)
;;;; 7.1.2 fn:false
(define (xpath-fn:false) #f)
;;;; 7.2.1 op:boolean-equal
(define (xpath-op:boolean-equal v1 v2) (boolean=? v1 v2))
;;;; 7.2.2 op:boolean-less-than
(define (xpath-op:boolean-less-than v1 v2)
  (and (boolean=? v1 #f) (boolean=? v2 #t)))
;;;; 7.2.3 op:boolean-greater-than
(define (xpath-op:boolean-greater-than v1 v2)
  (xpath-op:boolean-less-than v2 v1))
;;;; 7.3.1 fn:boolean
(define (xpath-fn:boolean arg*)
  (cond ((null? arg*) #f)
	((and (pair? arg*) (node? (car arg*))))
	((boolean? arg*) arg*)
	((string? arg*) (not (zero? (string-length arg*))))
	((number? arg*) (not (or (zero? arg*) (nan? arg*))))
	(else (xqt-error 'FORG0006 'xpath-fn:boolean "Unknown value" arg*))))
;;;; 7.3.2 fn:not
(define (xpath-fn:not arg*) (not (xpath-fn:boolean arg*)))


;;;; 8.2.1 op:yearMonthDuration-less-than
(define (xpath-op:year-month-duration-less-than v1 v2)
  ;; TODO type check
  (< (xs:duration-months v1) (xs:duration-months v2)))
;;;; 8.2.2 op:yearMonthDuration-greater-than
(define (xpath-op:year-month-duration-greater-than v1 v2)
  (xpath-op:year-month-duration-less-than v2 v1))
;;;; 8.2.3 op:dayTimeDuration-less-than
(define (xpath-op:day-time-duration-less-than v1 v2)
  (< (xs:duration-seconds v1) (xs:duration-seconds v2)))
;;;; 8.2.4 op:dayTimeDuration-greater-than
(define (xpath-op:day-time-duration-greater-than v1 v2)
  (xpath-op:day-time-duration-less-than v2 v1))
;;;; 8.2.5 op:duration-equal
(define (xpath-op:duration-equal v1 v2)
  (unless (and (xs:duration? v1) (xs:duration? v2))
    ;; FIXME which error?
    (assertion-violation 'xpath-op:duration-equal "Invalid arguments" v1 v2))
  (and (= (xs:duration-months v1) (xs:duration-months v2))
       (= (xs:duration-seconds v1) (xs:duration-seconds v2))))

;;;; 8.3.1 fn:years-from-duration
(define (xpath-fn:years-from-duration arg)
  (if (xs:day-time-duration? arg)
      0
      (quotient (xs:duration-months arg) 12)))
;;;; 8.3.2 fn:months-from-duration
(define (xpath-fn:months-from-duration arg)
  (if (xs:day-time-duration? arg)
      0
      (remainder (xs:duration-months arg) 12)))
(define (exact-floor d) (exact (floor d)))
;;;; 8.3.3 fn:days-from-duration
(define (xpath-fn:days-from-duration arg)
  (if (xs:year-month-duration? arg)
      0
      (quotient (exact-floor (xs:duration-seconds arg)) 86400)))
;;;; 8.3.4 fn:hours-from-duration
(define (xpath-fn:hours-from-duration arg)
  (if (xs:year-month-duration? arg)
      0
      (quotient (remainder (exact-floor (xs:duration-seconds arg)) 86400) 3600)))
;;;; 8.3.5 fn:minutes-from-duration
(define (xpath-fn:minutes-from-duration arg)
  (if (xs:year-month-duration? arg)
      0
      (quotient (remainder (exact-floor (xs:duration-seconds arg)) 3600) 60)))
;;;; 8.3.6 fn:seconds-from-duration
(define (xpath-fn:seconds-from-duration arg)
  (if (xs:year-month-duration? arg)
      0
      (let-values (((s f) (flinteger-fraction (xs:duration-seconds arg))))
	(+ (remainder s 60) f))))

(define-syntax define-duration-arithmetic-operators
  (lambda (x)
    (define (op-name k type op plural)
      (datum->syntax k
       (string->symbol (string-append "xpath-op:" op "-"
				      (symbol->string (syntax->datum type))
				      plural))))
    (define (div-by-name k type)
      (define type-str (symbol->string (syntax->datum type)))
      (datum->syntax k
       (string->symbol
	(string-append "xpath-op:divide-" type-str "-by-" type-str))))
    (define (ctr-name k type)
      (datum->syntax k
       (string->symbol
	(string-append "xs:make-" (symbol->string (syntax->datum type))))))
    (syntax-case x ()
      ((k type getter)
       (with-syntax ((add (op-name #'k #'type "add" "s"))
		     (sub (op-name #'k #'type "subtract" "s"))
		     (mul (op-name #'k #'type "multiply" ""))
		     (div (op-name #'k #'type "divide" ""))
		     (div-by (div-by-name #'k #'type))
		     (make (ctr-name #'k #'type)))
	 #'(begin
	     (define (add v1 v2) (make (+ (getter v1) (getter v2))))
	     (define (sub v1 v2) (make (- (getter v1) (getter v2))))
	     (define (mul v1 arg)
	       (when (nan? arg)
		 (xqt-error 'FOCA0005 'mul
			    "Multiplier must be a real number" arg))
	       (make (exact (ceiling (* (getter v1) arg)))))
	     (define (div v1 arg)
	       (when (nan? arg)
		 (xqt-error 'FOCA0005 'div
			    "Multiplier must be a real number" arg))
	       (make (exact (xpath-fn:round (/ (getter v1) arg)))))
	     (define (div-by v1 v2)
	       (xpath-op:numeric-divide (getter v1) (getter v2)))))))))
;;;; 8.4.1 op:add-yearMonthDurations
;;;; 8.4.2 op:subtract-yearMonthDurations
;;;; 8.4.3 op:multiply-yearMonthDuration
;;;; 8.4.4 op:divide-yearMonthDuration
;;;; 8.4.5 op:divide-yearMonthDuration-by-yearMonthDuration
(define-duration-arithmetic-operators year-month-duration xs:duration-months)
;;;; 8.4.6 op:add-dayTimeDurations
;;;; 8.4.7 op:subtract-dayTimeDurations
;;;; 8.4.8 op:multiply-dayTimeDuration
;;;; 8.4.9 op:divide-dayTimeDuration
;;;; 8.4.10 op:divide-dayTimeDuration-by-dayTimeDuration
(define-duration-arithmetic-operators day-time-duration xs:duration-seconds)


;;;; 9.3.1 fn:dateTime
(define (xpath-fn:datetime d t)
  (unless (eqv? (xs:date-timezone-offset d) (xs:time-timezone-offset t))
    (xqt-error 'FORG0008 'xpath-fn:datetime
	       "Date and time has different timezones" d t))
  (xs:make-datetime (xs:date-year d) (xs:date-month d) (xs:date-day d)
		    (xs:time-hour t) (xs:time-minute t) (xs:time-second t)
		    (xs:date-timezone-offset d)))

(define-syntax define-date-comparison
  (lambda (x)
    (define (gen k type)
      (define name (symbol->string (syntax->datum type)))
      (datum->syntax k
       (map (lambda (suffix op)
	      (list (string->symbol (string-append "xpath-op:" name suffix))
		    (string->symbol (string-append "xs:" name op))))
	    '("-equal" "-less-than" "-greater-than")
	    '("-w/o-tz=?" "<?" ">?"))))
    (syntax-case x ()
      ((k type)
       (with-syntax ((((name op) ...) (gen #'k #'type)))
	 #'(begin
	     (define (name d1 d2) (op d1 d2))
	     ...))))))
;;;; 9.4.1 op:dateTime-equal
;;;; 9.4.2 op:dateTime-less-than
;;;; 9.4.3 op:dateTime-greater-than
(define-date-comparison datetime)
;;;; 9.4.4 op:date-equal
;;;; 9.4.5 op:date-less-than
;;;; 9.4.6 op:date-greater-than
(define-date-comparison date)
;;;; 9.4.7 op:time-equal
;;;; 9.4.8 op:time-less-than
;;;; 9.4.9 op:time-greater-than
(define-date-comparison time)

;; we define extra procedures but don't export it ;)
;;;; 9.4.10 op:gYearMonth-equal
(define-date-comparison g-year-month)
;;;; 9.4.11 op:gYear-equal
(define-date-comparison g-year)
;;;; 9.4.12 op:gMonthDay-equal
(define-date-comparison g-month-day)
;;;; 9.4.13 op:gMonth-equal
(define-date-comparison g-month)
;;;; 9.4.14 op:gDay-equal
(define-date-comparison g-day)

(define-syntax define-date-accessor
  (lambda (x)
    (define (gen k type prop1 prop2)
      (define t (symbol->string (syntax->datum type)))
      (define p1 (symbol->string (syntax->datum prop1)))
      (define p2 (symbol->string (syntax->datum prop2)))
      (datum->syntax k
       (list (string->symbol (string-append "xpath-fn:" p2 "-from-" t))
	     (string->symbol (string-append "xs:" t "-" p1)))))
    (syntax-case x ()
      ((k type (prop1 prop2) prop* ...)
       (with-syntax (((name acc) (gen #'k #'type #'prop1 #'prop2)))
	 #'(begin
	     (define (name o) (acc o))
	     (k type prop* ...))))
      ((k type prop prop* ...) #'(k type (prop prop) prop* ...))
      ((k type)                #'(begin)))))


;;;; 9.5.1 fn:year-from-dateTime
;;;; 9.5.2 fn:month-from-dateTime
;;;; 9.5.3 fn:day-from-dateTime
;;;; 9.5.4 fn:hours-from-dateTime
;;;; 9.5.5 fn:minutes-from-dateTime
;;;; 9.5.6 fn:seconds-from-dateTime
(define-date-accessor datetime year month day
  (hour hours) (minute minutes) (second seconds))

(define-syntax define-timezone-from-*
  (syntax-rules ()
    ((_ name acc)
     (define (name dt)
       (let ((tz (acc dt)))
	 (if (not tz)
	     '()
	     (xs:make-day-time-duration (* tz 60))))))))
;;;; 9.5.7 fn:timezone-from-dateTime
(define-timezone-from-* xpath-fn:timezone-from-datetime
  xs:datetime-timezone-offset)

;;;; 9.5.8 fn:year-from-date
;;;; 9.5.9 fn:month-from-date
;;;; 9.5.10 fn:day-from-date
(define-date-accessor date year month day)
;;;; 9.5.11 fn:timezone-from-date
(define-timezone-from-* xpath-fn:timezone-from-date xs:date-timezone-offset)

;;;; 9.5.12 fn:hours-from-time
;;;; 9.5.13 fn:minutes-from-time
;;;; 9.5.14 fn:seconds-from-time
(define-date-accessor time (hour hours) (minute minutes) (second seconds))
;;;; 9.5.15 fn:timezone-from-time
(define-timezone-from-* xpath-fn:timezone-from-time xs:time-timezone-offset)

;;;; 9.6.1 fn:adjust-dateTime-to-timezone
(define (adjust-datetime dt offset)
  (if (null? dt)
      '()
      (let ((zone (xs:datetime-timezone-offset dt)))
	(cond ((and (null? offset) (not zone)) dt)
	      ((and (null? offset) zone)
	       (xs:make-datetime (xs:datetime-year dt)
				 (xs:datetime-month dt)
				 (xs:datetime-day dt)
				 (xs:datetime-hour dt)
				 (xs:datetime-minute dt)
				 (xs:datetime-second dt)))
	      ((not zone)
	       (xs:make-datetime (xs:datetime-year dt)
				 (xs:datetime-month dt)
				 (xs:datetime-day dt)
				 (xs:datetime-hour dt)
				 (xs:datetime-minute dt)
				 (xs:datetime-second dt)
				 (div offset 60)))
	      (else
	       (let* ((new-off (div offset 60))
		      (diff (- zone new-off)))
		 (xs:make-datetime (xs:datetime-year dt)
				   (xs:datetime-month dt)
				   (xs:datetime-day dt)
				   (xs:datetime-hour dt)
				   (- (xs:datetime-minute dt) diff)
				   (xs:datetime-second dt)
				   new-off)))))))
  
(define xpath-fn:adjust-datetime-to-timezone
  (case-lambda
   ((dt)
    (adjust-datetime dt (timezone-offset (or (*xs:dynamic-timezone*)
					     (local-timezone)))))
   ((dt dtd)
    (cond ((null? dtd) (adjust-datetime dt dtd))
	  (else
	   (unless (xs:day-time-duration? dtd)
	     (assertion-violation 'xpath-fn:adjust-datetime-to-timezone
				  "DayTimeDuration required" dtd))
	   (let ((sec (xs:duration-seconds dtd)))
	     (when (or (<  sec (* -14 3600)) (< (* 14 3600) sec))
	       (xqt-error 'FODT0003 'xpath-fn:adjust-datetime-to-timezone
			  "Range error (-PT14H < n < PT14H)" dtd))
	     (adjust-datetime dt sec)))))))

;;;; 9.6.2 fn:adjust-date-to-timezone
#|
* Let $dt be the value of fn:dateTime($arg, xs:time('00:00:00')). 
* Let $adt be the value of fn:adjust-dateTime-to-timezone($dt, $timezone)
* The function returns the value of xs:date($adt)
|#
(define xpath-fn:adjust-date-to-timezone
  (case-lambda
   ((d)
    (let* ((dt (xs:make-datetime (xs:date-year d)
				 (xs:date-month d)
				 (xs:date-day d)
				 0 0 0 (xs:date-timezone-offset d)))
	   (adt (xpath-fn:adjust-datetime-to-timezone dt)))
      (xs:make-date (xs:datetime-year adt) (xs:datetime-month adt)
		    (xs:datetime-day adt)
		    (xs:date-timezone-offset adt))))
   ((d tz)
    (let* ((dt (xs:make-datetime (xs:date-year d)
				 (xs:date-month d)
				 (xs:date-day d)
				 0 0 0 (xs:date-timezone-offset d)))
	   (adt (xpath-fn:adjust-datetime-to-timezone dt tz)))
      (xs:make-date (xs:datetime-year adt) (xs:datetime-month adt)
		    (xs:datetime-day adt)
		    (xs:date-timezone-offset adt))))))

;;;; 9.6.3 fn:adjust-time-to-timezone
#|
* Let $dt be the xs:dateTime value fn:dateTime(xs:date('1972-12-31'), $arg). 
* Let $adt be the value of fn:adjust-dateTime-to-timezone($dt, $timezone) 
* The function returns the xs:time value xs:time($adt). 
|#
(define xpath-fn:adjust-time-to-timezone
  (case-lambda
   ((t)
    (let* ((dt (xs:make-datetime 1972 12 31
				 (xs:time-hour t)
				 (xs:time-minute t)
				 (xs:time-second t)
				 (xs:time-timezone-offset t)))
	   (adt (xpath-fn:adjust-datetime-to-timezone dt)))
      (xs:make-time (xs:datetime-hour adt) (xs:datetime-minute adt)
		    (xs:datetime-second adt)
		    (xs:date-timezone-offset adt))))
   ((t tz)
    (let* ((dt (xs:make-datetime 1972 12 31
				 (xs:time-hour t)
				 (xs:time-minute t)
				 (xs:time-second t)
				 (xs:time-timezone-offset t)))
	   (adt (xpath-fn:adjust-datetime-to-timezone dt tz)))
      (xs:make-time (xs:datetime-hour adt) (xs:datetime-minute adt)
		    (xs:datetime-second adt)
		    (xs:date-timezone-offset adt))))))

;;;; 9.7.2 op:subtract-dateTimes
(define (xpath-op:subtract-datetimes dt1 dt2)
  (unless (and (xs:datetime? dt1) (xs:datetime? dt2))
    (assertion-violation 'xpath-op:subtract-datetimes "Datetime required"
			 dt1 dt2))
  (xs:datetime-subtract dt1 dt2))
;;;; 9.7.3 op:subtract-dates
(define (xpath-op:subtract-dates d1 d2)
  (unless (and (xs:date? d1) (xs:date? d1))
    (assertion-violation 'xpath-op:subtract-dates "Date required" d1 d2))
  (xs:date-subtract d1 d2))
;;;; 9.7.4 op:subtract-times
(define (xpath-op:subtract-times t1 t2)
  (unless (and (xs:time? t1) (xs:time? t2))
    (assertion-violation 'xpath-op:subtract-dates "Time required" t1 t2))
  (xs:time-subtract t1 t2))


(define-syntax define-date-add/sub-duration
  (lambda (x)
    (define (gen-name k type op conj)
      (define t (symbol->string (syntax->datum type)))
      (datum->syntax k
       (map string->symbol 
	    (list
	     (string-append "xpath-op:" op "-year-month-duration-" conj "-" t)
	     (string-append "xpath-op:" op "-day-time-duration-" conj "-" t)
	     (string-append "xs:" t "-" op "-duration")))))
    (syntax-case x ()
      ((k type)
       (with-syntax (((ymd-add dt-add d-add) (gen-name #'k #'type "add" "to"))
		     ((ymd-sub dt-sub d-sub)
		      (gen-name #'k #'type "subtract" "from")))
	 #'(begin
	     (define (ymd-add dt d) (d-add dt d))
	     (define (dt-add dt d) (d-add dt d))
	     (define (ymd-sub dt d) (d-sub dt d))
	     (define (dt-sub dt d) (d-sub dt d))))))))
	 
;;;; 9.7.5 op:add-yearMonthDuration-to-dateTime
;;;; 9.7.6 op:add-dayTimeDuration-to-dateTime
;;;; 9.7.7 op:subtract-yearMonthDuration-from-dateTime
;;;; 9.7.8 op:subtract-dayTimeDuration-from-dateTime
(define-date-add/sub-duration datetime)

;;;; 9.7.9 op:add-yearMonthDuration-to-date
;;;; 9.7.10 op:add-dayTimeDuration-to-date
;;;; 9.7.11 op:subtract-yearMonthDuration-from-date
;;;; 9.7.12 op:subtract-dayTimeDuration-from-datew
(define-date-add/sub-duration date)

;;;; 9.7.13 op:add-dayTimeDuration-to-time
;;;; 9.7.14 op:subtract-dayTimeDuration-from-time
(define-date-add/sub-duration time)

;;;; 9.8.1 fn:format-dateTime
;;;; 9.8.2 fn:format-date
;;;; 9.8.3 fn:format-time
(define (xpath-fn:format-datetime . args)
  (implementation-restriction-violation 'xpath-fn:format-datetime
					"Not supported yet"))
(define (xpath-fn:format-date . args)
  (implementation-restriction-violation 'xpath-fn:format-date
					"Not supported yet"))
(define (xpath-fn:format-time . args)
  (implementation-restriction-violation 'xpath-fn:format-time
					"Not supported yet"))

;;;; 9.9.1 fn:parse-ietf-date
;; apparently, the definition doesn't meet with the RFC 5322, so
;; we define it separately... damn another date format...
;;; S ::= ( x09 | x0A | x0D | x20 )+
(define $xpath:S ($or ($eqv? #\x09) ($eqv? #\x0A) ($eqv? #\x0D) ($eqv? #\x20)))
;;; digit ::= [0-9]
(define $xpath:digit
  ($or ($eqv? #\0) ($eqv? #\1) ($eqv? #\2) ($eqv? #\3) ($eqv? #\4)
       ($eqv? #\5) ($eqv? #\6) ($eqv? #\7) ($eqv? #\8) ($eqv? #\9)))
;;; hours ::= digit digit?
(define $xpath:hours
  ($let ((d0 $xpath:digit)
	 (d1 ($optional $xpath:digit #f)))
   ($return (if d1 (string->number (string d0 d1)) (- (char->integer d0) 48)))))
;;; minutes ::= digit digit
(define $xpath:minutes
  ($let ((d0 $xpath:digit)
	 (d1 $xpath:digit))
   ($return (string->number (string d0 d1)))))
;;; seconds ::= digit digit ("." digit+)?
(define $xpath:seconds
  ($let ((d0 $xpath:digit)
	 (d1 $xpath:digit)
	 (d2 ($optional ($seq ($eqv? #\.) ($many $xpath:digit)) #f)))
   (if d2
       ($return (string->number (apply string d0 d1 #\. d2)))
       ($return (string->number (string d0 d1 #\.))))))
;;; year ::= digit digit (digit digit)?
(define $xpath:year
  ($let ((d0 $xpath:digit)
	 (d1 $xpath:digit)
	 (d2-3 ($optional ($repeat $xpath:digit 2) #f)))
    ($return (if d2-3
		 (string->number (apply string d0 d1 d2-3))
		 (+ 1900 (string->number (string d0 d1)))))))
;;; daynum ::= digit digit?
(define $xpath:daynum $xpath:hours)
;;; dayname ::= "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun" |
;;;             "Monday | "Tuesday" | "Wednesday" | "Thursday" | "Friday" |
;;;             "Saturday" | "Sunday"
(define $xpath:dayname
  ($or ($token "Monday") ($token "Tuesday") ($token "Wednesday")
       ($token "Thursday") ($token "Friday") ($token "Saturday")
       ($token "Sunday")
       ($token "Mon") ($token "Tue") ($token "Wed") ($token "Thu")
       ($token "Fri") ($token "Sat") ($token "Sun")))
;;; dsep ::= S | (S? "-" S?)
(define $xpath:desp
  ($or $xpath:S
       ($seq ($optional $xpath:S) ($eqv? #\-) ($optional $xpath:S))))
;;; monthname ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" |
;;;               "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
(define $xpath:monthname
  ($or ($seq ($token "Jan") ($return 1)) ($seq ($token "Feb") ($return 2))
       ($seq ($token "Mar") ($return 3)) ($seq ($token "Apr") ($return 4))
       ($seq ($token "May") ($return 5)) ($seq ($token "Jun") ($return 6))
       ($seq ($token "Jul") ($return 7)) ($seq ($token "Aug") ($return 8))
       ($seq ($token "Sep") ($return 9)) ($seq ($token "Oct") ($return 10))
       ($seq ($token "Nov") ($return 11)) ($seq ($token "Dec") ($return 12))))
;;; datespec ::= daynum dsep monthname dsep year
(define $xpath:datespec
  ($let ((d $xpath:daynum)
	 $xpath:desp
	 (m $xpath:monthname)
	 $xpath:desp
	 (y $xpath:year))
   ($return (list y m d))))
;;; tzoffset ::= ("+"|"-") hours ":"? minutes?
(define $xpath:tzoffset
  ($let ((s ($or ($eqv? #\+) ($eqv? #\-)))
	 (h $xpath:hours)
	 (($optional ($eqv? #\:)))
	 (m ($optional $xpath:minutes)))
   (let ((off (+ (* h 60) m))) ;; offset of XML date...
     (if (eqv? #\- s)
	 ($return (- off))
	 ($return off)))))
;;; tzname ::= "UT" | "UTC" | "GMT" | "EST" | "EDT"
;;;          | "CST" | "CDT" | "MST" | "MDT" | "PST" | "PDT"
(define $xpath:tzname
  ($or ($seq ($token "UT")  ($return 0)) ($seq ($token "UTC") ($return 0))
       ($seq ($token "GMT") ($return 0))
       ($seq ($token "EST") ($return -300)) ($seq ($token "EDT") ($return -240))
       ($seq ($token "CST") ($return -360)) ($seq ($token "CDT") ($return -300))
       ($seq ($token "MST") ($return -420)) ($seq ($token "MDT") ($return -360))
       ($seq ($token "PST") ($return -480)) ($seq ($token "PDT") ($return -420)))
  )
;;; timezone ::= tzname | tzoffset (S? "(" S? tzname S? ")")?
(define $xpath:timezone
  ($or $xpath:tzname
       ($let ((off $xpath:tzoffset)
	      (($optional ($let ($xpath:S
				 (($eqv? #\())
				 (n $xpath:tzname)
				 (($eqv? #\))))
			    n) #f)))
	 ($return off))))
;;; time ::= hours ":" minutes (":" seconds)? (S? timezone)?
(define $xpath:time
  ($let ((h $xpath:hours)
	 (($eqv? #\:))
	 (m $xpath:minutes)
	 (s ($optional ($seq ($eqv? #\:) $xpath:seconds) 0))
	 (t ($optional ($seq ($optional $xpath:S) $xpath:timezone) #f)))
    ($return `(,h ,m ,s ,t))))
;;; asctime ::= monthname dsep daynum S time S year
(define $xpath:asctime
  ($let ((m $xpath:monthname)
	 $xpath:desp
	 (d $xpath:daynum)
	 $xpath:S
	 (t $xpath:time)
	 $xpath:S
	 (y $xpath:year))
   ($return `(,y ,m ,d ,@t))))
;;; input ::= S? (dayname ","? S)? ((datespec S time) | asctime) S?
(define $xpath:input
  ($let ((($optional $xpath:S))
	 (dow ($optional ($let ((d $xpath:dayname)
				(($optional ($eqv? #\,)))
				$xpath:S) ($return d)) #f))
	 (t ($or ($let ((s $xpath:datespec)
			$xpath:S
			(t $xpath:time))
		   ($return `(,@s ,@t)))
		 $xpath:asctime))
	 (($optional $xpath:S)))
    ($return t)))

(define (xpath-fn:parse-ietf-date value)
  (define lseq (generator->lseq (string->generator value)))
  (let-values (((s v nl) ($xpath:input lseq)))
    (unless (parse-success? s)
      (assertion-violation 'xpath-fn:parse-ietf-date "Invalid format" value))
    (apply xs:make-datetime v)))

;;;; 10.1.1 fn:resolve-QName
(define (xpath-fn:resolve-qname qname element)
  (if (null? qname)
      '()
      ;; no idea what to do here. finding the prefix?
      (implementation-restriction-violation 'xpath-fn:resolve-qname "Not yet")))

;;;; 10.1.2 fn:QName
(define (xpath-fn:qname uri name)
  (when (or (null? uri) (zero? (string-length uri)))
    (xqt-error 'FOCA0002 "Namespace URI must not be empty" uri))
  (cond ((string-index name #\:) =>
	 (lambda (index)
	   (xs:make-qname uri (substring name (+ index 1) (string-length name))
			  (substring name 0 index))))
	(else (xs:make-qname uri name))))

;;;; 10.2.1 op:QName-equal
(define (xpath-op:qname-equal qn1 qn2)
  (and (equal? (xs:qname-namespace-uri qn1) (xs:qname-namespace-uri qn2))
       (equal? (xs:qname-local-part qn1) (xs:qname-local-part qn2))))

;;;; 10.2.2 fn:prefix-from-QName
(define (xpath-fn:prefix-from-qname qn)
  (cond ((null? qn) '())
	((xs:qname-prefix qn) =>
	 (lambda (p)
	   (if (zero? (string-length p))
	       '()
	       p)))))

;;;; 10.2.3 fn:local-name-from-QName
(define (xpath-fn:local-name-from-qname qn)
  (cond ((null? qn) '())
	((xs:qname-local-part qn))))

;;;; 10.2.4 fn:namespace-uri-from-QName
(define (xpath-fn:namespace-uri-from-qname qn)
  (cond ((null? qn) '())
	((xs:qname-namespace-uri qn))))

;;;; 10.2.5 fn:namespace-uri-for-prefix
(define (xpath-fn:namespace-uri-for-prefix prefix element)
  (define fixed-up (if (null? prefix) "" prefix))
  (define (prefix=? n)
    (and (equal? (namespace-prefix n) fixed-up)
	 (namespace-uri n)))
  (let ((namespaces (node-list->list (element:namespace-nodes element))))
    (cond ((exists prefix=? namespaces))
	  (else '()))))

;;;; 10.2.6 fn:in-scope-prefixes
(define (xpath-fn:in-scope-prefixes element*)
  (if (null? element*)
      '()
      (delete-duplicates
       (append-map (lambda (e)
		     (map namespace-prefix
			  (node-list->list (element:namespace-nodes e))))
		   element*))))

;;;; 11.1.1 op:hexBinary-equal
(define xpath-op:hex-binary-equal bytevector=?)
;;;; 11.1.2 op:hexBinary-less-than
(define xpath-op:hex-binary-less-than bytevector<?)
;;;; 11.1.3 op:hexBinary-greater-than
(define xpath-op:hex-binary-greater-than bytevector>?)

(define (base64-binary-comparison =)
  (lambda (a b)
    (= (base64-decode (string->utf8 a))
       (base64-decode (string->utf8 b)))))
;;;; 11.1.4 op:base64Binary-equal
(define xpath-op:base64-binary-equal (base64-binary-comparison bytevector=?))
;;;; 11.1.5 op:base64Binary-less-than
(define xpath-op:base64-binary-less-than (base64-binary-comparison bytevector<?))
;;;; 11.1.6 op:base64Binary-greater-than
(define xpath-op:base64-binary-greater-than
  (base64-binary-comparison bytevector>?))

;;;; 12.1 op:NOTATION-equal
(define (xpath-op:notation-equal arg1 arg2)
  (implementation-restriction-violation 'xpath-op:notation-equal
					"Not supported yet"))

;;;; 13.1 fn:name
(define (xpath-fn:name arg) (xpath-fn:string (xpath-fn:node-name arg)))

;;;; 13.2 fn:local-name
(define (xpath-fn:local-name arg)
  (if (null? arg)
      ""
      (let ((n (xpath-dm:node-name arg)))
	(if (null? n)
	    ""
	    (xs:qname-local-part n)))))

;;;; 13.3 fn:namespace-uri
(define (xpath-fn:namespace-uri arg)
  (xs:qname-namespace-uri (xpath-dm:node-name arg)))

;;;; 13.4 fn:lang	     
(define (xpath-fn:lang testlang node)
  (define lang (string-downcase testlang))
  (define (has-testlang n)
    (exists (lambda (attr)
	      (and (string=? (attr-name attr) "xml:lang")
		   (string-prefix? lang (string-downcase (attr-value attr)))))
	    (xpath-dm:attributes n)))
  (let ((selector (xml:ancestor-or-self has-testlang)))
      ;; (ansestor-or-self::*/@xml:lang)[last()] = testlang
    (let ((nl (selector node)))
      (not (zero? (node-list-length nl))))))

;;;; 13.5 fn:root
(define xpath-fn:root
  (let ((selector (xml:ancestor-or-self node?)))
    (lambda (arg)
      ;; (ansestor-or-self::node())[1]
      (let ((node-list (selector arg)))
	(node-list:item node-list 0)))))

;;;; 13.6 fn:path
(define (xpath-fn:path node)
  (implementation-restriction-violation 'xpath-fn:path "Not supported yet"))

;;;; 13.7 fn:has-children
(define (xpath-fn:has-children node)
  (and (not (null? node))
       ;; = fn:exists($node/child::node())
       (not (zero? (node-list-length (node-child-nodes node))))))

;;;; 13.8 fn:innermost
(define (xpath-fn:innermost nodes)
  (implementation-restriction-violation 'xpath-fn:innermost "Not supported yet"))

;;;; 13.9 fn:outermost
(define (xpath-fn:outermost nodes)
  (implementation-restriction-violation 'xpath-fn:outermost "Not supported yet"))


;;;; 14.1.1 fn:empty
(define (xpath-fn:empty arg) (null? arg))

;;;; 14.1.2 fn:exists
(define (xpath-fn:exists arg*) (not (null? arg*)))

;;;; 14.1.3 fn:head
(define (xpath-fn:head arg)
  (cond ((null? arg) '())
	((pair? arg) (car arg))
	(else arg)))

;;;; 14.1.4 fn:tail
(define (xpath-fn:tail arg)
  (cond ((null? arg) '())
	((pair? arg) (cdr arg))
	(else '())))

;;;; 14.1.5 fn:insert-before
(define (xpath-fn:insert-before target position inserts)
  ;; lazy implementation
  (let-values (((f e) (split-at target (max 0 (- position 1)))))
    (if (pair? inserts)
	`(,@f ,@inserts ,@e)
	`(,@f ,inserts ,@e))))

;;;; 14.1.6 fn:remove
(define (xpath-fn:remove target position)
  (define p (- position 1))
  (if (negative? p)
      target
      (let loop ((r '()) (t target) (i 0))
	(cond ((null? t) (reverse! r))
	      ((= i p) (loop r (cdr t) (+ i 1)))
	      (else (loop (cons (car t) r) (cdr t) (+ i 1)))))))

;;;; 14.1.7 fn:reverse
(define (xpath-fn:reverse args) (if (pair? args) (reverse args) args))

;;;; 14.1.8 fn:subsequence
(define xpath-fn:subsequence
  (case-lambda
   ((l start n)
    (define offset (- start 1))
    (take (drop l offset) n))
   ((l start)
    (define offset (- start 1))
    (drop l offset))))

;;;; 14.1.9 fn:unordered
;; this is still permutation of the input list...
(define (xpath-fn:unordered args) args)

;;;; 14.2.1 fn:distinct-values
(define xpath-fn:distinct-values
  (case-lambda
   ((args) (delete-duplicates args equal?))
   ((args collation) (xpath-fn:distinct-values args))))

;;;; 14.2.2 fn:index-of
(define xpath-fn:index-of
  (case-lambda
   ((seq search)
    (if (pair? seq)
	(do ((r '() (if (equal? (car seq) search) (cons i r) r))
	     (i 1 (+ i 1))
	     (seq seq (cdr seq)))
	    ((null? seq) (reverse! r)))
	'()))
   ((seq search collation) (xpath-fn:index-of seq search))))

;;;; 14.2.3 fn:deep-equal
(define (%xpath-fn:deep-equal a b)
  (cond ((and (null? a) (null? b)))
	((and (pair? a) (pair? b))
	 (and (= (length a) (length b))
	      (for-all %xpath-fn:deep-equal a b)))
	((equal? a b)) ;; atomic can be compared like this ;)
	((and (vector? a) (vector? b))
	 (vector= %xpath-fn:deep-equal a b))
	((and (hashtable? a) (hashtable? b))
	 (and (= (hashtable-size a) (hashtable-size b))
	      (for-all (lambda (k)
			 (%xpath-fn:deep-equal (hashtable-ref a k #f)
					       (hashtable-ref b k #f)))
		       (vector->list (hashtable-keys a)))))
	((and (node? a) (node? b))
	 (and (eqv? (node-node-type a) (node-node-type b))
	      (cond ((document? a)
		     (string=? (xpath-dm:string-value a)
			       (xpath-dm:string-value b)))
		    ((element? a)
		     (implementation-restriction-violation 'xpath-fn:deep-equal
							   "not yet"))
		    ((attr? a)
		     (implementation-restriction-violation 'xpath-fn:deep-equal
							   "not yet"))
		    ((processing-instruction? a)
		     (and (xpath-op:qname-equal (xpath-dm:node-name a)
						(xpath-dm:node-name b))
			  (string=? (xpath-dm:string-value a)
				    (xpath-dm:string-value b))))
		    ((namespace? a)
		     (and (%xpath-fn:deep-equal (xpath-dm:node-name a)
						(xpath-dm:node-name b))
			  (string=? (xpath-dm:string-value a)
				    (xpath-dm:string-value b))))
		    ((text? a)
		     (string=? (xpath-dm:string-value a)
			       (xpath-dm:string-value b)))
		    (else #f))))
	((and (xs:qname? a) (xs:qname? b))
	 ;; not sure how it suppose to be
	 (and (xpath-op:qname-equal a b)
	      (equal? (xs:qname-prefix a) (xs:qname-prefix b))))
	(else #f)))

(define xpath-fn:deep-equal
  (case-lambda
   ((a b) (%xpath-fn:deep-equal a b))
   ;; ignore collation for now
   ((a b collation) (%xpath-fn:deep-equal a b))))

;;;; 14.3.1 fn:zero-or-one
(define (xpath-fn:zero-or-one arg)
  (cond ((null? arg) arg)
	((and (pair? arg) (null? (cdr arg))) arg)
	(else (xqt-error 'FORG0003 'xpath-fn:zero-or-one
			 "More than one or not a sequence" arg))))

;;;; 14.3.2 fn:one-or-more
(define (xpath-fn:one-or-more arg)
  (if (and (pair? arg) (not (null? arg)))
      arg
      (xqt-error 'FORG0004 'xpath-fn:one-or-more "Empty or not a sequence" arg)))

;;;; 14.3.3 fn:exactly-one
(define (xpath-fn:exactly-one arg)
  (if (and (pair? arg) (= 1 (length arg)))
      arg
      (xqt-error 'FORG0005 'xpath-fn:exactly-one
		 "Not an exacely one element sequence" arg)))

;;;; 14.4.1 fn:count
(define (xpath-fn:count arg)
  (if (pair? arg)
      (length arg)
      1))

;;; 19 Casting
(define (atomic->string who atomic)
  (cond ((string? atomic) atomic)
	((null? atomic) "")
	((or (integer? atomic) (flonum? atomic)) (number->string atomic))
	;; this may loose the original information when the value is
	;; either 0 or 1...
	((boolean? atomic) (if atomic "true" "false"))
	(else (xpty0004-error who atomic))))

(define (implementation-restriction-violation who msg)
  (raise (condition (make-implementation-restriction-violation)
		    (make-who-condition who)
		    (make-message-condition msg))))

)
