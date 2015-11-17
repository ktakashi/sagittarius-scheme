(import (rnrs)
	(rnrs r5rs)	 ;; for quotient
	(sagittarius io) ;; for with-input-from-string
	(srfi :1 lists)	 ;; for unfold
	(srfi :121 generators)
	(prefix (srfi :64) srfi:))

(srfi:test-begin "SRFI-121 Generators")

(define-syntax test-group
  (syntax-rules ()
    ((_ name exprs ...)
     (begin
       (srfi:test-group name)
       exprs ...))))

(define-syntax test
  (syntax-rules ()
    ((_ expect expr)
     (test 'expr expect expr))
    ((_ name expect expr)
     (srfi:test-equal name expect expr))))

(define-syntax test-assert
  (syntax-rules ()
    ((_ expr)
     (srfi:test-assert 'expr expr))))

(define-syntax test-error
  (syntax-rules ()
    ((_ expr)
     (srfi:test-error 'expr condition? expr))))

;; The tests are from sample implementation of the SRFI
(test-group "generators"
  (test-group "generators/constructors"
    (test '() (generator->list (make-generator)))
    (test '(1 2 3) (generator->list (make-generator 1 2 3)))
    (test '(8 9 10) (generator->list (make-iota-generator 3 8)))
    (test '(8 10 12) (generator->list (make-iota-generator 3 8 2)))
    (test '(3 4 5 6) (generator->list (make-range-generator 3) 4))
    (test '(3 4 5 6 7) (generator->list (make-range-generator 3 8)))
    (test '(3 5 7) (generator->list (make-range-generator 3 8 2)))
    (define g
      (make-coroutine-generator
       (lambda (yield) (let loop ((i 0))
                   (when (< i 3) (yield i) (loop (+ i 1)))))))
    (test '(0 1 2) (generator->list g))
    (test '(1 2 3 4 5) (generator->list (list->generator '(1 2 3 4 5))))
    (test '(1 2 3 4 5) (generator->list (vector->generator '#(1 2 3 4 5))))
    (test '(5 4 3 2 1) (generator->list (reverse-vector->generator '#(1 2 3 4 5))))
    (test '(#\a #\b #\c #\d #\e) (generator->list (string->generator "abcde")))
    (test '(10 20 30) (generator->list (bytevector->generator #u8(10 20 30))))
    (test '() (generator->list (make-bits-generator 0)))
    (test '(#t) (generator->list (make-bits-generator 1)))
    (test '(#f #t) (generator->list (make-bits-generator 2)))
    (test '(#t #t) (generator->list (make-bits-generator 3)))
    (test '(#f #f #t) (generator->list (make-bits-generator 4)))
    (test '() (generator->list (make-bits-generator -1)))
    (test '(#f) (generator->list (make-bits-generator -2)))
    (test '(#t #f) (generator->list (make-bits-generator -3)))
    (test '(#f #f) (generator->list (make-bits-generator -4)))
    (test '(#t #t #f) (generator->list (make-bits-generator -5)))
    ;; no test for make-port-generator
    (test '(1 2 3 4 5) (generator->list
                         (make-for-each-generator for-each '(1 2 3 4 5))))
    (define (truncate/ a b)
      (values (quotient a b) (remainder a b)))
    ; for-each that generates digits in little-endian order
    (define (for-each-digit proc n)
      (when (> n 0)
        (let-values (((div rem) (truncate/ n 10)))
          (proc rem)
          (for-each-digit proc div))))
    (test '(5 4 3 2 1) (generator->list
                         (make-for-each-generator for-each-digit 12345)))
    (test '(0 2 4 6 8 10) (generator->list
                            (make-unfold-generator
                              (lambda (s) (> s 5))
                              (lambda (s) (* s 2))
                              (lambda (s) (+ s 1))
                              0)))
  ) ; end "generators/constructors"

  (test-group "generators/operators"
    (test '(a b 0 1) (generator->list (gcons* 'a 'b (make-range-generator 0 2))))
    (test '(0 1 2 0 1) (generator->list (gappend (make-range-generator 0 3)
                                                 (make-range-generator 0 2))))
    (test '() (generator->list (gappend)))
    (define g1 (make-generator 1 2 3))
    (define g2 (make-generator 4 5 6 7))
    (define (proc . args) (values (apply + args) (apply + args)))
    (test '(15 22 31) (generator->list (gcombine proc 10 g1 g2)))
    (test '(1 3 5 7 9) (generator->list (gfilter
                                           odd?
                                           (make-range-generator 1 11))))
    (test '(2 4 6 8 10) (generator->list (gremove
                                           odd?
                                           (make-range-generator 1 11))))
    (define g (make-range-generator 1 5))
    (test '(1 2 3) (generator->list (gtake g 3)))
    (test '(4) (generator->list g))
    (test '(1 2) (generator->list (gtake (make-range-generator 1 3) 3)))
    (test '(1 2 0) (generator->list (gtake (make-range-generator 1 3) 3 0)))
    (test '(3 4) (generator->list (gdrop (make-range-generator 1 5) 2)))
    (define g (make-range-generator 1 5))
    (define (small? x) (< x 3))
    (test '(1 2) (generator->list (gtake-while small? g)))
    (define g (make-range-generator 1 5))
    (test '(3 4) (generator->list (gdrop-while small? g)))
    (test '(0.0 1.0 0 2) (generator->list (gdelete 1
                                                   (make-generator 0.0 1.0 0 1 2))))
    (test '(0.0 0 2) (generator->list (gdelete 1
                                               (make-generator 0.0 1.0 0 1 2) =)))

    (test '(1 2 3 4) (generator->list (gdelete-neighbor-dups
                                        (make-generator 1 2 2 3 3 3 4 4 4 4))))
    (test '(a c e) (generator->list (gindex (list->generator '(a b c d e f))
                                            (list->generator '(0 2 4)))))
    (test '(a d e) (generator->list (gselect (list->generator '(a b c d e f))
                                             (list->generator '(#t #f #f #t #t #f)))))

  ) ; end "generators/operators"

  (test-group "generators/consumers"
    ;; no test for plain generator->list (used throughout)
    (test '(1 2 3) (generator->list (make-generator 1 2 3 4 5) 3))
    (test '(5 4 3 2 1) (generator->reverse-list (make-generator 1 2 3 4 5)))
    (test '#(1 2 3 4 5) (generator->vector (make-generator 1 2 3 4 5)))
    (test '#(1 2 3) (generator->vector (make-generator 1 2 3 4 5) 3))
    (define v (vector 1 2 0 0 0))
    (generator->vector! v 2 (make-generator 3 4 5))
    (test v '#(1 2 3 4 5))
    (test "abc" (generator->string (make-generator #\a #\b #\c)))
    (test '(e d c b a . z) (with-input-from-string "a b c d e"
                             (lambda () (generator-fold cons 'z read))))

    (define n 0)
    (generator-for-each (lambda values (set! n (apply + values)))
      (make-generator 1) (make-generator 2) (make-generator 3))
    (test 6 n)
    (test 3 (generator-find (lambda (x) (> x 2)) (make-range-generator 1 5)))
    (test 4 (generator-length (make-range-generator 1 5)))
    (test 2 (generator-count odd? (make-range-generator 1 5)))
    (define g (make-range-generator 2 5))
    (test #t (generator-any odd? g))
    (test '(4) (generator->list g))
    (define g (make-range-generator 2 5))
    (test 2  (generator-any values g))
    (test '(3 4) (generator->list g))
    (define g (make-range-generator 2 5))
    (test #f (generator-every odd? g))
    (test '(3 4) (generator->list g))
    (test #t (generator-every odd? (make-generator))) ; boundary case
    (test 5 (generator-every values (make-generator 1 3 5)))
    (test '(0 1 2 3) (generator-unfold (make-range-generator 1 4) unfold 0))

  ) ; end "generators/consumers"

) ; end "generators"


(srfi:test-end)
