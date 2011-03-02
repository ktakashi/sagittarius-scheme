;(import (rnrs) (rnrs mutable-pairs))
(define-syntax print
  (syntax-rules ()
    ((_)
     (newline))
    ((_ o)
     (begin
       (display o)
       (print)))
    ((_ o1 o2 ...)
     (begin
       (display o1)
       (print o2 ...)))))

(define cont #f)
(display (+ 5 (call/cc (lambda (c) (begin (set! cont c) 10)))))(newline)
(display (cont 10))(newline)
(display (cont 20))(newline)
(display (cont 5))(newline)


(define (f1 a b c)
    (call/cc (lambda (escape)
        (begin 
	  (display a) (newline)
	  (display b) (newline)
	  (escape 10)
	  (display c) (newline)))))
(f1 1 2 3)


;; infinity loop
#;(let* ((yin ((lambda (foo) (newline) foo)
             (call/cc (lambda (bar) bar))))
       (yang ((lambda (foo) (display "*") foo)
              (call/cc (lambda (bar) bar)))))
  (yin yang))

#;((lambda (yin)
     ((lambda (yang)
        (yin yang))
      ((lambda (foo) (begin (display "*") foo))
       (call/cc (lambda (bar) bar)))))
   ((lambda (foo) (begin(newline) foo))
    (call/cc (lambda (bar) bar))))

(define x (cons 1 2))
(define coroutine (cons #t  #t))
(define (resume)
    (begin (set-cdr! coroutine #t)
           ((car coroutine) 'dummy)
           (newline)))

(define (suspend escape)
    (set-car! coroutine (begin (set-cdr! coroutine #f) (call/cc (lambda (x) x))))
    (if (cdr coroutine) #f (escape #f)))

(define (f dummy)
    (call/cc (lambda (escape)
        (letrec ((iter (lambda (i) (begin (display i) (suspend escape) (iter (+ i 1))))))
        (iter 0)))))

(set-car! coroutine f)
(resume)

(print 'here)

(define fail #f)

(define (choose . ls)
  (if (null? ls)
      (fail)
    (let ((fail0 fail))
      (call/cc
       (lambda (cc)
          (set! fail
                (lambda ()
                  (set! fail fail0)
                  (cc (apply choose (cdr ls)))))
          (cc (car ls)))))))
      
(call/cc
 (lambda (cc)
   (set! fail
         (lambda ()
           (cc 'no-choise)))))
(define (sq x)
  (* x x))
(define (pythag a b c)
  (if (= (+ (sq a) (sq b)) (sq c))
      (list a b c)
      (choose)))

(pythag (choose 1 2 3) (choose 3 4 5) (choose  4 5 6))

(define (mul-cont x)
  (call/cc (lambda (escape) 
	     (letrec ((mul-aux (lambda (y)
				 (display y)
				 (newline)
				 (let ((result
					(cond ((null? y) 1)
					      ((= (car y) 0) (escape 0))
					      (#t (* (car y) (mul-aux (cdr y)))))))
			           (display (list y result))
				   (newline)
				   result)
				 )))
	       (mul-aux x)))))
(mul-cont '(1 2 3 4))
(mul-cont '(1 0 3 4))


(define cnt #f)
(+ 5 (call/cc (lambda (c) (set! cnt c) 5)))
(display cnt)(newline)
(display
 (- 5 (call/cc (lambda (c)
		(c 10))))
)
(newline)
