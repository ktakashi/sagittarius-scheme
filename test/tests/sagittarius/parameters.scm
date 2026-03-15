(import (rnrs)
	(sagittarius continuations)
	(sagittarius parameters)
	(srfi :64))

(test-begin "Parameters")

(test-assert (parameter? current-input-port))
(test-assert (parameter? current-output-port))
(test-assert (parameter? current-error-port))


(let ()
  (define p (make-parameter 10 (lambda (x) (* x x))))
  (define ps #f)

  (test-equal 100 (p))
  (p 12)
  (test-equal 144 (p))
  (parameterize ([p (p)])
    (set! ps (current-parameterization))
    (test-equal 20736 (p)))
  (test-equal 144 (p))

  (test-assert #t (parameter? p))
  (test-assert #t (parameterization? ps))

  (test-equal '(20736 0)
	      (call-with-parameterization ps
	        (lambda ()
		  (let ([x (p)])
		    (p 0)
		    (list x (p))))))
  (test-equal 144 (p)))

(test-assert (with-continuation-mark 'in-tail-context? #t
	       (parameterize ([(make-parameter 0) 1])
		 (call-with-immediate-continuation-mark 'in-tail-context?
							values))))
;; dynamic-wind parameterize is not tail context
(test-assert (not (with-continuation-mark 'in-tail-context? #t
		    (parameterize/dw ([(make-parameter 0) 1])
		      (call-with-immediate-continuation-mark 'in-tail-context?
							     values)))))

(let ([p (make-parameter 1 (lambda (x) (+ x 1)))])
  (temporarily ([p 4]) (values))
  (test-equal 3 (p)))

(let ([p (make-parameter 1 (lambda (x) (+ x 1)))])
  (parameterize ([p 4]) (values))
  (test-equal 2 (p)))

;; These tests requires thread incooporation, which is not done yet
;; (let ([p (make-parameter 1)])
;;   (define t 
;;     (temporarily ((p 2))
;;       (make-thread (lambda () (p)))))
;;   (test-equal 1 (thread-join! (thread-start! t))))

;; (let ([p (make-parameter 1)])
;;   (define t
;;     (parameterize ([p 2])
;;       (make-thread (lambda () (p)))))
;;   (test-equal 2 (thread-join! (thread-start! t))))

(define-syntax with-variants
  (lambda (stx)
    (syntax-case stx ()
      ((k body)
       (with-syntax ([call/cc (datum->syntax #'k 'call/cc)]
                     [parameterize (datum->syntax #'k 'parameterize)])
	 #'(begin
             (define-syntax a-test
	       (syntax-rules ()
		 ((_ call/cc parameterize)
		  (call/prompt (lambda () body)))))
             (a-test call/cc parameterize)
             (a-test call/delim-cc parameterize)
             (a-test call/cc parameterize/dw)
	     (a-test call/delim-cc parameterize/dw)))))))

(with-variants
 (test-equal
  '((1 . 5) (2 . 6) (3 . 5) (1 . 5) (2 . 6) (3 . 5))
  (let* ([x (make-parameter 0)]
         [l '()]
         [void (lambda arg* #f)]
         [add! (lambda (a b)
                 (set! l (append l (list (cons a b)))))])
    (let ([k (parameterize ([x 5])
	       (dynamic-wind
                   (lambda () (add! 1 (x)))
                   (lambda () (parameterize ([x 6])
				(let ([k+e (call/cc (lambda (k) (cons k void)))])
                                  (add! 2 (x))
                                  ((cdr k+e))
                                  (car k+e))))
                   (lambda () (add! 3 (x)))))])
      (parameterize ([x 7])
	(call/cc
         (lambda (c)
           (k (cons void c))))))
    l)))

(test-end)
