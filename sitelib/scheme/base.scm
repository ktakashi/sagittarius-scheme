;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme base)
    (export
     * + - ... / < <= = => > >=

     abs and append apply assoc assq assv

     begin binary-port? boolean=? boolean? bytevector
     bytevector-append bytevector-copy bytevector-copy! bytevector-length
     bytevector-u8-ref bytevector-u8-set! bytevector?

     caar cadr call-with-current-continuation call-with-port call-with-values
     call/cc car case cdar cddr cdr ceiling char->integer char-ready? char<=?
     char<? char=? char>=? char>? char? close-input-port
     close-output-port close-port complex? cond
     cond-expand cons current-error-port current-input-port current-output-port

     define define-record-type define-syntax define-values denominator
     do dynamic-wind

     else eof-object eof-object? eq? equal? eqv? error error-object-irritants
     error-object-message error-object? even? exact
     exact-integer-sqrt exact-integer? exact? expt

     (rename (cond-features features)) file-error?
     floor floor-quotient floor-remainder floor/ flush-output-port for-each

     gcd get-output-bytevector get-output-string guard

     if import include include-ci inexact inexact? input-port-open? input-port?
     integer->char integer?

     lambda lcm length let let* let*-values let-syntax let-values
     letrec letrec* letrec-syntax list list->string list->vector
     list-copy list-ref list-set! list-tail list?

     make-bytevector make-list make-parameter make-string make-vector map max
     member memq memv min modulo

     negative? newline not null? number->string number? numerator

     odd? open-input-bytevector open-input-string open-output-bytevector
     open-output-string or output-port-open? output-port?

     pair? parameterize peek-char peek-u8 port? positive? procedure? quasiquote
     quote quotient

     raise raise-continuable rational? rationalize
     read-bytevector read-bytevector! read-char read-error?
     read-line read-string read-u8 real? remainder reverse round set!

     set-car! set-cdr! square string string->list string->number
     string->symbol string->utf8 string->vector string-append
     string-copy string-copy! string-fill! string-for-each
     string-length string-map string-ref string-set!
     string<=? string<? string=? string>=? string>? string?
     substring symbol->string symbol=? symbol? syntax-error syntax-rules

     textual-port? truncate truncate-quotient truncate-remainder
     truncate/ u8-ready?

     unless unquote unquote-splicing utf8->string

     values vector vector->list vector->string vector-append vector-copy
     vector-copy! vector-fill! vector-for-each vector-length
     vector-map vector-ref vector-set! vector?

     when with-exception-handler
     write-bytevector write-char
     write-string write-u8

     zero?)
  (import (rename (except (rnrs) syntax-rules define-record-type)
		  (error r6rs:error))
	  (rnrs mutable-pairs)
	  (rnrs mutable-strings)
	  (rnrs r5rs)
	  (compat r7rs)
	  (srfi :0)
	  (rename (srfi :1) (make-list srfi:make-list))
	  (srfi :6)
	  (srfi :9)
	  (srfi :23)
	  (srfi :39)
	  (only (scheme private) define-values)
	  ;; for undefined
	  (sagittarius))

  (define-syntax syntax-error
    (syntax-rules ()
      ((_ msg args ...)
       (syntax-violation 'syntax-error msg (quote args ...)))))

  (define (bytevector . bytes) (u8-list->bytevector bytes))

  (define-syntax define-copy!
    (syntax-rules (body)
      ((_ name length set! ref)
       (define name
	 (case-lambda
	  ((to at from) (name to at from 0))
	  ((to at from start) (name to at from start (length from)))
	  ((to at from start end)
	   (if (<= at start)
	       (do ((i start (+ i 1)) (j at (+ j 1)))
		   ((= i end))
		 (set! to j (ref from i)))
	       (do ((i (+ at (- end start 1)) (- i 1)) (j (- end 1) (- j 1)))
		   ((< j start))
		 (set! to i (ref from j))))))))))

  (define-copy! bytevector-copy! bytevector-length
    bytevector-u8-set! bytevector-u8-ref)
  (define-copy! string-copy! string-length string-set! string-ref)
  (define-copy! vector-copy! vector-length vector-set! vector-ref)

  ;; for now error object is r6rs' condition
  (define (error message . irr)
    (apply r6rs:error 'error message irr))
  (define error-object? condition?)

  (define (error-object-irritants obj)
    (and (irritants-condition? obj)
	 (condition-irritants obj)))

  (define (error-object-message obj)
    (and (message-condition? obj)
	 (condition-message obj)))

  (define read-error? i/o-read-error?)
  (define (file-error? c)
    (or (i/o-file-already-exists-error? c)
	(i/o-file-does-not-exist-error? c)
	(i/o-file-is-read-only-error? c)
	(i/o-file-protection-error? c)
	(i/o-filename-error? c)))

  ;; ports
  (define (input-port-open? p)
    (or (input-port? p)
	(assertion-violation 'input-port-open? "input-port required" p))
    (not (port-closed? p)))

  (define (output-port-open? p)
    (or (output-port? p)
	(assertion-violation 'output-port-open? "output-port required" p))
    (not (port-closed? p)))

  (define (open-input-bytevector bv) (open-bytevector-input-port bv))

  (define (exact-integer? i) (and (integer? i) (exact? i)))

  (define (list-set! l k obj)
    (define (itr cur count)
      (if (= count k)
	  (set-car! cur obj)
	  (itr (cdr cur) (+ count 1))))
    (when (constant-literal? l)
      (assertion-violation 'list-set!
			   "attempt to modify literal constant" l))
    (itr l 0))

  (define make-list
    (case-lambda
     ((k fill) (srfi:make-list k fill))
     ((k) (srfi:make-list k (undefined)))))

  (define peek-u8
    (case-lambda
     (() (peek-u8 (current-input-port)))
     ((port) (lookahead-u8 port))))

  (define read-u8
    (case-lambda
     (() (read-u8 (current-input-port)))
     ((port) (get-u8 port))))

  (define read-bytevector
    (case-lambda
     ((len) (read-bytevector len (current-input-port)))
     ((len port) (get-bytevector-n port len))))

  (define read-bytevector!
    (case-lambda
     ((bv start end) (read-bytevector! bv start end (current-input-port)))
     ((bv start end port) (get-bytevector-n! port bv start (- end start)))))

  (define write-u8
    (case-lambda
     ((u8) (write-u8 u8 (current-output-port)))
     ((u8 port) (put-u8 port u8))))

  (define write-bytevector
    (case-lambda
     ((bv) (write-bytevector bv (current-output-port)))
     ((bv port) (put-bytevector port bv))
     ((bv port start) (put-bytevector port bv start))
     ((bv port start end) (put-bytevector port bv start (- end start)))))

  (define write-string
    (case-lambda
     ((s) (write-string s (current-output-port)))
     ((s port) (put-string port s))
     ((s port start) (put-string port s start))
     ((s port start end) (put-string port s start (- end start)))))

  (define read-line
    (case-lambda
     (() (read-line (current-input-port)))
     ((port) (get-line port))))

  (define read-string
    (case-lambda
     ((k) (read-string k (current-input-port)))
     ((k port) (get-string-n port k))))

  (define (string->vector s :optional (start 0) (end (string-length s)))
    (let* ((len (- end start))
	   (v   (make-vector len)))
      (do ((i start (+ i 1)))
	  ((= i end) v)
	(vector-set! v (- i start) (string-ref s i)))))

  (define (vector->string v :optional (start 0) (end (vector-length v)))
    (let* ((len (- end start))
	   (s   (make-string len)))
      (do ((i start (+ i 1)))
	  ((= i end) s)
	(let ((e (vector-ref v i)))
	  (unless (char? e)
	    (assertion-violation 'vector->string
				 "vector contains non character object"
				 e))
	  (string-set! s (- i start) e)))))

  (define (string-map proc str1 . strs)
    (list->string
     (apply map proc (string->list str1)
	    (map string->list strs))))

  (define char-ready? port-ready?)
  (define u8-ready? port-ready?)

  ;; misc
  (define (square z) (* z z))
  (define (boolean=? x y . rest)
    (unless (and (boolean? x) (boolean? y))
      (assertion-violation 'boolean=? "boolean required" x y))
    (if (null? rest)
	(eq? x y)
	(and (eq? x y)
	     (apply boolean=? y (car rest) (cdr rest)))))
  (define (symbol=? x y . rest)
    (unless (and (symbol? x) (symbol? y))
      (assertion-violation 'symbol=? "symbol required" x y))
    (if (null? rest)
	(eq? x y)
	(and (eq? x y)
	     (apply symbol=? y (car rest) (cdr rest)))))

  ;; moved from divisions (it's no longer supported)
  ;; From chibi-scheme

  ;; The builtin quotient and remainder implement truncation - the
  ;; fractional part is always discarded.

  (define truncate-quotient quotient)
  (define truncate-remainder remainder)
  (define (truncate/ n m)
    (values (truncate-quotient n m) (truncate-remainder n m)))

  ;; Floor, ceiling and round just compose their corresponding function
  ;; with division to determine the quotient, and compute the remainder
  ;; from that.

  (define (floor-quotient n m)
    (inexact->exact (floor (/ n m))))
  (define (floor-remainder n m)
    (- n (* m (floor-quotient n m))))
  (define (floor/ n m)
    (values (floor-quotient n m) (floor-remainder n m)))

;; these are removed since draft 7
;;  (define (ceiling-quotient n m)
;;    (inexact->exact (ceiling (/ n m))))
;;  (define (ceiling-remainder n m)
;;    (- n (* m (ceiling-quotient n m))))
;;  (define (ceiling/ n m)
;;    (values (ceiling-quotient n m) (ceiling-remainder n m)))
;;
;;  (define (round-quotient n m)
;;    (inexact->exact (round (/ n m))))
;;  (define (round-remainder n m)
;;    (- n (* m (round-quotient n m))))
;;  (define (round/ n m)
;;    (values (round-quotient n m) (round-remainder n m)))

  ;; Euclidean is defined as floor if the divisor is negative, and
  ;; ceiling otherwise.

;;  (define (euclidean-quotient n m)
;;    (if (> m 0) (floor-quotient n m) (ceiling-quotient n m)))
;;  (define (euclidean-remainder n m)
;;    (- n (* m (euclidean-quotient n m))))
;;  (define (euclidean/ n m)
;;    (values (euclidean-quotient n m) (euclidean-remainder n m)))

  ;; Centered places the remainder in the half-open interval
  ;; [-m/2, m/2).

;;  (define (centered-remainder n m)
;;    (let ((r (euclidean-remainder n m))
;;	  (m/2 (abs (/ m 2))))
;;      (cond ((< r (- m/2)) (+ r (abs m)))
;;	    ((>= r m/2) (- r (abs m)))
;;	    (else r))))
;;  (define (centered-quotient n m)
;;    (quotient (- n (centered-remainder n m)) m))
;;  (define (centered/ n m)
;;    (values (centered-quotient n m) (centered-remainder n m)))

)
