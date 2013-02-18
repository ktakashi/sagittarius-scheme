#!compatible
(import (rnrs)
	(rnrs mutable-pairs)
	(srfi :17 generalized-set!)
	(srfi :64 testing))

(test-begin "SRFI-17 generalized set! test")

(define x (cons 1 2))
(test-equal "(setter car)"
	    '((3 3) . 2)
	    (begin
	      (set! (car x) (list 3 3))
	      x))
(test-equal "(setter cdr)"
	    '((3 3) 4 5)
	    (begin
	      (set! (cdr x) (list 4 5))
	      x))
(test-equal "(setter caar)"
	    '(((8 9) 3) 4 5)
	    (begin
	      (set! (caar x) (list 8 9))
	      x))
(test-equal "(setter cadr)"
	    '(((8 9) 3) (7 6) 5)
	    (begin
	      (set! (cadr x) (list 7 6))
	      x))
(test-equal "(setter cdar)"
	    '(((8 9) 4 5) (7 6) 5)
	    (begin
	      (set! (cdar x) (list 4 5))
	      x))
(test-equal "(setter cddr)"
	    '(((8 9) 4 5) (7 6) 11 12)
	    (begin
	      (set! (cddr x) (list 11 12))
	      x))
(test-equal "(setter caaar)" '((((13 14) 9) 4 5) (7 6) 11 12)
      (begin (set! (caaar x) (list 13 14)) x))
(test-equal "(setter caadr)" '((((13 14) 9) 4 5) ((0 1) 6) 11 12)
      (begin (set! (caadr x) (list 0 1)) x))
(test-equal "(setter cadar)" '((((13 14) 9) (2 3) 5) ((0 1) 6) 11 12)
      (begin (set! (cadar x) (list 2 3)) x))
(test-equal "(setter caddr)" '((((13 14) 9) (2 3) 5) ((0 1) 6) (4 5) 12)
      (begin (set! (caddr x) (list 4 5)) x))
(test-equal "(setter cdaar)" '((((13 14) 5 6) (2 3) 5) ((0 1) 6) (4 5) 12)
      (begin (set! (cdaar x) (list 5 6)) x))
(test-equal "(setter cdadr)" '((((13 14) 5 6) (2 3) 5) ((0 1) 7 8) (4 5) 12)
      (begin (set! (cdadr x) (list 7 8)) x))
(test-equal "(setter cddar)" '((((13 14) 5 6) (2 3) 9 10) ((0 1) 7 8) (4 5) 12)
      (begin (set! (cddar x) (list 9 10)) x))
(test-equal "(setter cdddr)" '((((13 14) 5 6) (2 3) 9 10) ((0 1) 7 8) (4 5) -1 -2)
      (begin (set! (cdddr x) (list -1 -2)) x))
(test-equal "(setter caaaar)" '(((((1 3) 14) 5 6) (2 3) 9 10) ((0 1) 7 8) (4 5) -1 -2)
      (begin (set! (caaaar x) (list 1 3)) x))
(test-equal "(setter caaadr)" '(((((1 3) 14) 5 6) (2 3) 9 10) (((2 3) 1) 7 8) (4 5) -1 -2)
      (begin (set! (caaadr x) (list 2 3)) x))
(test-equal "(setter caadar)" '(((((1 3) 14) 5 6) ((0 1) 3) 9 10) (((2 3) 1) 7 8) (4 5) -1 -2)
      (begin (set! (caadar x) (list 0 1)) x))
(test-equal "(setter caaddr)" '(((((1 3) 14) 5 6) ((0 1) 3) 9 10) (((2 3) 1) 7 8) ((0 1) 5) -1 -2)
      (begin (set! (caaddr x) (list 0 1)) x))
(test-equal "(setter cadaar)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) 9 10) (((2 3) 1) 7 8) ((0 1) 5) -1 -2)
      (begin (set! (cadaar x) (list 0 1)) x))
(test-equal "(setter cadadr)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) 9 10) (((2 3) 1) (0 1) 8) ((0 1) 5) -1 -2)
      (begin (set! (cadadr x) (list 0 1)) x))
(test-equal "(setter caddar)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 1) (0 1) 8) ((0 1) 5) -1 -2)
      (begin (set! (caddar x) (list 0 1)) x))
(test-equal "(setter cadddr)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (begin (set! (cadddr x) (list 0 1)) x))
(test-equal "(setter cdaaar)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (begin (set! (cdaaar x) (list 0 1)) x))
(test-equal "(setter cdaadr)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (begin (set! (cdaadr x) (list 0 1)) x))
(test-equal "(setter cdadar)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (begin (set! (cdadar x) (list 0 1)) x))
(test-equal "(setter cdaddr)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 0 1) (0 1) -2)
      (begin (set! (cdaddr x) (list 0 1)) x))
(test-equal "(setter cddaar)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 0 1) (0 1) -2)
      (begin (set! (cddaar x) (list 0 1)) x))
(test-equal "(setter cddadr)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) -2)
      (begin (set! (cddadr x) (list 0 1)) x))
(test-equal "(setter cdddar)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 0 1) (((2 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) -2)
      (begin (set! (cdddar x) (list 0 1)) x))
(test-equal "(setter cddddr)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 0 1) (((2 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 0 1)
      (begin (set! (cddddr x) (list 0 1)) x))

;; vector
(define x (vector 1 2 3 4 5))
(test-equal "(setter vector-ref)"
	    '#(1 2 3 #f 5)
	    (begin
	      (set! (vector-ref x 3) #f)
	      x))

;; string
(define x (string-copy "abcde"))
(test-equal "(setter string-ref)"
	    "abcQe"
	    (begin
	      (set! (string-ref x 3) #\Q)
	      x))

;; bytevectors
;; usage (bytevector-test u8 expect add)
(define-syntax bytevector-test
  (lambda (x)
    (define (build type expect num native?)
      (with-syntax
	  ((name (datum->syntax
		  type (string->symbol (format "bytevector-~a~a-ref"
					       (syntax->datum type)
					       (if native? "-native" "")))))
	   (expect expect)
	   (num num))
	#'(test-equal (format "(setter ~a)" 'name)
		      expect
		      (begin
			(let ((bv (make-bytevector (bytevector-length expect))))
			  (set! (name bv 0) num)
			  bv)))))
    (syntax-case x (native)
      ((_ type native expect num)
       (build #'type #'expect #'num #f))
      ((_ type expect num)
       (build #'type #'expect #'num #f)))))

(bytevector-test u8 #vu8(1) 1)
(bytevector-test s8 #vu8(255) -1)
;; TODO we also support native ref but.

;; getter-with-setter
(define (set-kar! p v) (set-car! p v))
(define kar (getter-with-setter (lambda (p) (car p)) set-kar!))
(define x (cons 1 2))
(test-equal "(setter kar)" '(3 . 2) (begin (set! (kar x) 3) x))

(test-equal "set!" #f (begin (set! x #f) x))

(test-end)