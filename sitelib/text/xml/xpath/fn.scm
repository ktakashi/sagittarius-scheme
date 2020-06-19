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
	    xpath-op:subtract-times)
    (import (rnrs)
	    (rnrs r5rs)
	    (rfc uri)
	    (sagittarius regex)
	    (sagittarius timezone)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :115 regexp)
	    (srfi :144 flonums)
	    (text xml errors)
	    (text xml dom)
	    (only (text xml dom parser) +xml:char-set+)
	    (text xml schema)
	    (text xml xpath dm))

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
