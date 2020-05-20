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
	    )
    (import (rnrs)
	    (sagittarius regex)
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
  (div v1 v2))
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
       (xqt-error 'FOAR0001 'xpath-op:numeric-divide "Dividing by 0" v1 v2))
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
