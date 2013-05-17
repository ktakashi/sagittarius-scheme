(import (rnrs) (srfi :64) (srfi :39)
	(sagittarius control) (sagittarius object))

(test-begin "SRFI 39 test")

;; issue 107
(let ()
  (define x (make-parameter 3 (lambda (x) (+ x 3))))
  (test-equal "param" 6 (x))
  (parameterize ((x 4)) (test-equal "in parameterize" 7 (x)))
  (test-equal "after" 6 (x)))

;; from Gauche
(define-syntax test* (identifier-syntax test-equal))
(define identity values)

(define a #f)
(define b #f)

(test* "make-parameter" 3
       (begin
         (set! a (make-parameter 3))
         (a)))
(test* "make-parameter" 88
       (begin
         (set! b (make-parameter 88 ->integer))
         (b)))
(test* "parameter" "abc" (begin (a "abc") (a)))
(test* "parameter" 34    (begin (b "34") (b)))

(test* "parameterize & dynamic-wind" '(("93" 112)
                                       (34 0)
                                       ("93" 112)
                                       (34 0))
       (let* ([z '()]
              [k (parameterize ([a "93"] [b 112])
                   (rlet1 k (call/cc identity)
                     (push! z (list (a) (b)))))])
         (parameterize ([a (b)] [b (a)])
           (push! z (list (a) (b)))
           (if k (k #f)))
         (reverse z)))


;; The dynamic environment needs to work on locations, not the values.
;; In the following code, when the continuation is invoked, the value of
;; f should be restored to c, not b.
;; Test code by Joo ChurlSoo.

(test* "call/cc and side effect" '(a b c d c c d)
       (let ([f (make-parameter 'a)]
             [path '()]
             [c #f])
         (let1 add (^[] (set! path (cons (f) path)))
           (add)
           (parameterize ([f 'b])
             (call/cc (^[c0] (set! c c0)))
             (add) (f 'c) (add))
           (f 'd)
           (add)
           (if (< (length path) 5)
             (c 'end)
             (reverse path)))))

;; Another bug reported by Joo ChurlSoo.

(test* "reassignment of global variable holding a parameter"
       '((10 20) (10 200) (1000 2000) (1 2000))
       (let* ([init '()]
              [add-init (^x (set! init (cons x init)))]
              [a (make-parameter 1)]
              [b (make-parameter 2)])
         (parameterize ([a 10] [b 20])
           (add-init (list (a) (b)))
           (set! b (make-parameter 200))
           (add-init (list (a) (b)))
           (a 1000) (b 2000)
           (add-init (list (a) (b))))
         (add-init (list (a) (b)))
         (reverse init)))


(test-end)