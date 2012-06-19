(import (rnrs)
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
;; TODO

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