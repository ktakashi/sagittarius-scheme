(import (except (rnrs) call/cc call-with-current-continuation)
	(rename (sagittarius continuations)
		(call/delimited-cc call/cc)
		(call-with-delimited-current-continuation
		 call-with-current-continuation))
	(sagittarius vm))

(define call/cc-via-composable 
  (case-lambda 
   [(f) (call/cc-via-composable f (default-continuation-prompt-tag))]
   [(f tag)
    (call-with-composable-continuation
     (lambda (k)
       (f (lambda vs
            (abort-current-continuation 
             tag 
             (lambda () 
               (call-with-continuation-prompt
                (lambda ()
                  (apply k vs))
                tag
                (lambda (thunk) (thunk)))))))))]))

(define call-with-continuation-prompt-for-composable
  (case-lambda
   [(f) (call-with-continuation-prompt-for-composable
         f
         (default-continuation-prompt-tag))]
   [(f tag)
    (call-with-continuation-prompt f
                                   tag
                                   (lambda (thunk) (thunk)))]))
(define-syntax test
  (syntax-rules (quote)
    ((_ expect (quote name) expr)
     (let ((r expr))
       (unless (equal? expect r)
	 (format #t "~a - expected: ~s, result: ~s~%" 'name expect r))
       r))
    ((_ expect expr args ...)
     (test expect 'expr (expr args ...)))))
(define-syntax test-values
  (syntax-rules ()
    ((_ expect expr args ...)
     (let-values ((r (expr args ...)))
       (format #t "expected: ~s, result: ~s~%" expect r)
       (apply values r)))))

(define-syntax let/cc
  (syntax-rules ()
    ((_ k expr ...)
     (call/cc (lambda (k) expr ...)))))
(define-syntax let/prompt
  (syntax-rules ()
    ((_ ((var val)  ...) body ...)
     (let/prompt (default-continuation-prompt-tag) ((var val) ...) body ...))
    ((_ tag ((var val)  ...) body ...)
     (call-with-continuation-prompt
      (lambda ()
	(let ((var val) ...) body ...))
      tag))))

(define null '())
(define (add1 n) (+ n 1))
(define void values)

(print call/cc)
(let/prompt ([output null])
  (call-with-continuation-prompt
   (lambda ()
     (define in (lambda () (set! output (cons 'in output)) (print 'in)))
     (define out (lambda () (set! output (cons 'out output))))
     (dynamic-wind
         in
         (lambda ()
           (let ([p1 (make-continuation-prompt-tag)])
             (let/cc esc
               (let ([k
                      (call-with-continuation-prompt
                       (lambda ()
                         ((call-with-composable-continuation
                           (lambda (k)
                             (lambda () k))
                           p1)))
                       p1)])
                 (/ (k (lambda () (esc 0))))))))
         out))
   (default-continuation-prompt-tag)
   void)
  (test '(out in) values output))
