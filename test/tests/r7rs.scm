(import (scheme base) (scheme eval) (scheme char) (scheme lazy) (srfi 64))

(test-begin "R7RS extra")

;; #95 removing import from (scheme base)
(test-assert "overwrite import syntax" 
	     (eval '(define-library (lib)
		      (export foo)
		      (import (scheme base))
		     (begin
		       (define-syntax import (syntax-rules ()))
		       (define-syntax foo
			 (syntax-rules (import)
			   ((_ import) 'ng)
			   ((_ _) 'ok))))
		     )
		   (environment '(sagittarius))))

;; (lib) is there
(import (lib))
(test-equal "syntax-rules literal comparison" 'ok (foo import))
(let ((r (foo import)))
  (test-equal "syntax-rules literal comparison (symbol)" 'ok r))

;; the same test as contrib tests on r7rs-tests.scm 
;; but wrapped by syntax-case (test-equal in srfi-64)
;; http://saito.hatenablog.jp/entry/2014/03/24/070839
;; disabled for pattern-match-lambda. this is written in
;; portable R7RS macro and the test case is for affinity
;; between R6RS and R7RS. latter case is much lower priority
;; for me.

(let ()
  (define-syntax bar
    (syntax-rules ()
      ((_ m body)
       (let ((m 1))
	 (body)))))

  (define-syntax foo
    (syntax-rules ()
      ((_ m body)
       (let ((n 2))
	 (let-syntax ((%body
		       (syntax-rules ()
			 ((_) body))))
	   (bar m %body))))))

  (let ((n 3))
    (test-equal "should be 3" 3 (foo n (values n)))))

;; extracted from
;; https://github.com/SaitoAtsushi/pattern-match-lambda
(let ()
  (define-syntax if-identifier
    (syntax-rules ()
      ((_ condition seq alt)
       (let-syntax ((foo (syntax-rules () ((_) seq))))
	 (let-syntax ((test (syntax-rules ()
			      ((_ condition) (foo))
			      ((_ foo) alt))))
	   (test foo))))))
  
  (define-syntax %if-match
    (syntax-rules ()
      ((_  (p . r) e seq alt)
       (let ((temp e))
	 (if (pair? temp)
	     (%if-match p (car temp)
			(%if-match r (cdr temp) seq alt)
			alt)
	     (alt))))
      ((_ () e seq alt)
       (if (null? e) seq (alt)))
      ((_ p e seq alt)
       (if-identifier p
		      (if (equal? 'p e) seq (alt))
		      (if (equal? p e) seq (alt))))))

  (define-syntax if-match
    (syntax-rules ()
      ((_ pattern lst seq alt)
       (let ((alt-thunk (lambda() alt)))
	 (%if-match pattern lst seq alt-thunk)))))

  (define foo
    (lambda lst
      (if-match (1 2 3) lst 'ok 'ng)))
  (test-equal "identifier renaming" 'ok (foo 1 2 3))
  )

;; call #110
(let ()
  (define-syntax begin-scope
    (syntax-rules ()
      ((begin-scope defer <body> ...)
       (let ((stuff '()))
	 (let-syntax ((defer (syntax-rules ()
			       ((defer <statement>)
				(set! stuff (cons <statement> stuff))
				))))
	   (begin <body> ...))
	 #;(for-each display stuff)
	 stuff))))

  (define (example-2)
    (define out (open-output-string))
    (begin-scope defer
		 (display "one" out)
		 (defer (display "two" out))
		 (display "three" out))
    (get-output-string out))
  (test-equal "unbound variable error on let-syntax"
	      "onetwothree"
	      (example-2))
)

;; call #119
(test-assert "not an error" (not (input-port-open? (current-output-port))))
(test-assert "not an error" (not (output-port-open? (current-input-port))))

;; call #120
;; from Larceny
(let ()
  (define (filter-all-chars p?)
    (do ((i 0 (+ i 1))
	 (chars '()
		(if (and (not (<= #xd800 i #xdfff))
			 (p? (integer->char i)))
		    (cons (integer->char i) chars)
		    chars)))
	((= i #x110000)
	 (reverse chars))))
  (define (filter p? xs)
    (do ((xs (reverse xs) (cdr xs))
	 (ys '() (if (p? (car xs))
		     (cons (car xs) ys)
		     ys)))
	((null? xs)
	 ys)))
  (test-equal "digit-value and char-numeric?" '()
	      (let* ((chars (filter-all-chars
			     (lambda (c) (not (char-numeric? c)))))
		     (vals (map digit-value chars)))
		(filter values
			(map (lambda (char is-bad?)
			       (and is-bad? char))
			     chars vals)))))

(test-equal "promise" '(1 1 101)
	    (let ((n 100))
	      (define r (delay (begin (set! n (+ n 1)) 1)))
	      (define s (delay-force r))
	      (define t (delay-force s))
	      
	      (let ((result1 (force t))
		    (result2 (force r)))
		(list result1 result2 n))))

(test-equal "promise(2)" '(5 0 10)
	    (let ()
	      (define q
		(let ((count 5))
		  (define (get-count) count)
		  (define p (delay (if (<= count 0)
				       count
				       (begin (set! count (- count 1))
					      (force p)
					      (set! count (+ count 2))
					      count))))
		  (list get-count p)))
	      (define get-count (car q))
	      (define p (cadr q))
	      
	      (let* ((result1 (get-count))  ; =>   5
		     (result2 (force p))    ; =>   0
		     (result3 (get-count))) ; =>   10
		(list result1 result2 result3))))

;; call #151
(test-error "different input form"
	    syntax-violation?
	    (eval '(let ()
		     (define-syntax zipm
		       (syntax-rules ()
			 ((_ (x ...) (y ...))
			  (list '(x y) ...))))
		     (zipm (1 2 3) (1 2 3 4)))
		  (environment '(scheme base))))

;; found on Chibi's repository
(let ()
  (define E1 1)

  (define-syntax M
    (syntax-rules E1 ()
		  ((M x E1) (quote (x E1)))))

  (test-equal '(1 2 3) (M 1 2 3))

  (let ((E2 2))
    (define-syntax N
      (syntax-rules E2 ()
		    ((N y E2) (quote (y E2)))))
    (test-equal '(1 2 3) (N 1 2 3)))
  )

(test-assert "nested user specified ellipsis"
	     (eval
	      '(let ()
		 (define-syntax ell
		   (syntax-rules ()
		     ((ell body)
		      (define-syntax emm
			(syntax-rules ...1 ()
			  ((emm) body))))))
		 (ell
		  (define-syntax enn
		    (syntax-rules ...1 () 
		      ((enn args ...1) (quote (args ...1)))))))
	      (environment '(scheme base))))

;; From https://groups.google.com/forum/#!topic/scheme-reports-wg2/GaOyVX2faAg
(let ()
  (define-syntax define-tuple-type
    (syntax-rules ()
      ((define-tuple-type name make pred x->vec (defaults ...))
       (deftuple name (make) pred x->vec (defaults ...) (defaults ...) ()))))

  (define-syntax deftuple
    (syntax-rules ()
      ((deftuple name (make args ...) pred x->vec defaults (default . rest)
	 (fields ...))
       (deftuple name (make args ... tmp) pred x->vec  defaults rest
	 (fields ... (tmp tmp))))
      ((deftuple name (make args ...) pred x->vec (defaults ...) ()
	 ((field-name get) ...))
       (begin
	 (define-record-type name (make-tmp args ...) pred
	   (field-name get) ...)
	 (define (make . o)
	   (if (pair? o) (apply make-tmp o) (make-tmp defaults ...)))
	 (define (x->vec x)
	   (vector (get x) ...))))))

  (define-tuple-type point make-point point? point->vector (0 0))

  (test-equal "point->vector (1)" #(0 0) (point->vector (make-point)))
  (test-equal "point->vector (2)" #(1 2) (point->vector (make-point 1 2))))

(test-end)
