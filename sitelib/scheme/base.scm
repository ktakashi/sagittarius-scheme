;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme base)
  (export
   * + - ... / < <= = => > >= abs and append apply assoc assq assv begin
   binary-port? boolean? bytevector-copy bytevector-copy!
   bytevector-copy-partial! bytevector-length bytevector-u8-ref
   bytevector-u8-set!
   bytevector? caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr
   cadar caddar cadddr caddr cadr call-with-current-continuation
   call-with-port call-with-values call/cc car case cdaaar cdaadr cdaar cdadar
   cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr ceiling
   char->integer char-ready? char<=? char<? char=? char>=? char>? char?
   close-input-port close-output-port close-port complex? cond cond-expand cons
   current-error-port current-input-port current-output-port define
   define-record-type define-syntax define-values denominator do dynamic-wind
   else eof-object? eq? equal? eqv? error error-object-irritants
   error-object-message error-object? even? exact->inexact exact-integer-sqrt
   exact-integer? exact? expt floor flush-output-port for-each gcd
   get-output-bytevector get-output-string guard if import inexact->exact
   inexact? input-port? integer->char integer? lambda lcm length let let*
   let*-values let-syntax let-values letrec letrec* letrec-syntax list
   list->string
   list->vector list-copy list-ref list-set! list-tail list? make-bytevector
   make-list make-parameter make-string make-vector map max member memq memv min
   modulo negative? newline not null? number->string number? numerator odd?
   open-input-bytevector open-input-string open-output-bytevector
   open-output-string or output-port? pair? parameterize peek-char peek-u8
   port-open? port? positive? procedure? quasiquote quote quotient raise
   raise-continuable rational? rationalize read-bytevector read-bytevector!
   read-char read-line read-u8 real? remainder reverse round set! set-car!
   set-cdr! string string->list string->number string->symbol string->utf8
   string->vector string-append string-copy string-fill! string-for-each
   string-length string-map string-ref string-set! string<=? string<? string=?
   string>=? string>? string? substring symbol->string symbol? syntax-error
   syntax-rules textual-port? truncate u8-ready? unless unquote
   unquote-splicing utf8->string values vector vector->list
   vector->string vector-copy vector-fill! vector-for-each vector-length
   vector-map vector-ref vector-set! vector? when with-exception-handler
   write-bytevector write-char write-u8 zero?)
  (import (except (rnrs) syntax-rules error define-record-type)
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
  
  ;; for now error object is r6rs' condition
  (define error-object? condition?)

  (define (error-object-irritants obj)
    (and (irritants-condition? obj)
	 (condition-irritants obj)))

  (define (error-object-message obj)
    (and (message-condition? obj)
	 (condition-message obj)))

  ;; ports
  (define (port-open? p)
    (or (port? p)
	(assertion-violation 'port-open?
			     "port required" p))
    (not (port-closed? p)))

  (define (open-input-bytevector bv) (open-bytevector-input-port bv))

  (define (bytevector-copy-partial! from start end to at)
    (bytevector-copy! from start to at (- end start)))


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
     ((bv start end port) (get-bytevector-n! port bv start end))))

  (define write-u8
    (case-lambda
     ((u8) (write-u8 u8 (current-output-port)))
     ((u8 port) (put-u8 port u8))))

  (define write-bytevector
    (case-lambda
     ((bv) (write-bytevector bv (current-output-port)))
     ((bv port) (put-bytevector port bv))
     ((bv port start) put-bytevector port bv start)
     ((bv port start end) put-bytevector port bv start end)))

  (define read-line
    (case-lambda
     (() (read-line (current-input-port)))
     ((port) (get-line port))))

  (define (string->vector s :optional (start 0) (end (string-length s)))
    (let* ((len (- end start))
	   (v   (make-vector len)))
      (do ((i start (+ i 1)))
	  ((= i end) v)
	(vector-set! v i (string-ref s i)))))

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
	  (string-set! s i e)))))

  (define (string-map proc str1 . strs)
    (list->string
     (apply map proc (string->list str1)
	    (map string->list strs))))

  ;; FIXME: support non blocking IO
  (define char-ready?
    (case-lambda
     ((port) #f)
     (() #f)))

  (define u8-ready?
    (case-lambda
     ((port) #f)
     (() #f)))

)
