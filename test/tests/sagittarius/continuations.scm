(import (rnrs)
	(sagittarius continuations)
	(srfi :64))

(test-begin "Continuations")

(test-assert "continuation? (call/cc)"
	     (continuation? (call/cc (lambda (k) k))))
(test-assert "continuation? (call/comp)"
	     (continuation?
	      (call/prompt
	       (lambda ()
		 (call/comp (lambda (k) k))))))
(test-assert "continuation? (call/delimited-cc)"
	     (continuation?
	      (call/prompt
	       (lambda ()
		 (call/delim-cc (lambda (k) k))))))

(test-assert "continuation? (symbol)" (not (continuation? 'a)))
(test-assert "continuation? (symbol)" (not (continuation? (lambda args args))))

(test-assert (continuation-prompt-tag? (default-continuation-prompt-tag)))
(test-assert (continuation-prompt-tag? (make-continuation-prompt-tag)))

(let ((p1 (make-continuation-prompt-tag 'p1)))
  (define (check)
    (let ((k (call-with-continuation-prompt
	      (lambda ()
		((call/delim-cc
		  (lambda (k) (lambda () k))
		  p1)))
	      p1
	      (lambda (x) (display x) (newline) x))))
      (call-with-continuation-prompt
       (lambda () (k (lambda () 'invoked)))
       p1
       (lambda (x) 'outer x))))
  (test-equal 'invoked (check)))

(define-syntax test
  (lambda (x)
    (syntax-case x ()
      ((k expr expected)
       (with-syntax ((call/cc (datum->syntax #'k 'call/cc))
		     (call-with-current-continuation
		      (datum->syntax #'k 'call-with-current-continuation)))
	 #'(begin
	     (define (a-test call/cc call-with-current-continuation)
	       (call-with-continuation-prompt
		(lambda () (test-equal expected expr))))
	     ;; should work the same
	     (a-test call/cc call-with-current-continuation)
	     (a-test call/delim-cc
		     call-with-delimited-current-continuation)))))))

;; From R6RS test
(test (let ((path '())
            (c #f))
        (let ((add (lambda (s)
                     (set! path (cons s path)))))
          (dynamic-wind
              (lambda () (add 'connect))
              (lambda ()
                (add (call-with-current-continuation
                      (lambda (c0)
                        (set! c c0)
                        'talk1))))
              (lambda () (add 'disconnect)))
          (if (< (length path) 4)
              (c 'talk2)
              (reverse path))))
      '(connect talk1 disconnect
                connect talk2 disconnect))

(test (let ((n 0))
        (call-with-current-continuation
         (lambda (k)
           (dynamic-wind
               (lambda ()
                 (set! n (+ n 1))
                 (k))
               (lambda ()
                 (set! n (+ n 2)))
               (lambda ()
                 (set! n (+ n 4))))))
        n) 
      1)

(test (let ((n 0))
        (call-with-current-continuation
         (lambda (k)
           (dynamic-wind
               values
               (lambda ()
                 (dynamic-wind
                     values
                     (lambda ()
                       (set! n (+ n 1))
                       (k))
                     (lambda ()
                       (set! n (+ n 2))
                       (k))))
               (lambda ()
                 (set! n (+ n 4))))))
        n) 
      7)

;; Deviation of full call/cc
;; Full call/cc
;; (e) = (post mid pre)(post mid pre post mid pre)(post mid pre post mid pre)
;; v = #<unspecified>
;; NOTE: Racket's call/cc behaves the same as call/delim-cc
(let-values (((out e) (open-string-output-port)))
  (let ((v (call/prompt
	    (lambda ()
	      (define l '())
	      (define k #f)
	      (define count 0)
	      (let ((v (call/prompt
			(lambda ()
			  (dynamic-wind
			      (lambda () (set! l (cons 'pre l)))
			      (lambda ()
				(call/delim-cc (lambda (k0) (set! k k0)))
				(set! l (cons 'mid l))
				l)
			      (lambda () (set! l (cons 'post l))))))))
		(display l out)
		(set! count (+ count 1))
		(unless (= count 2) (k l))
		(display l out))))))
    (test-equal "(post mid pre)" (e))
    (test-equal '(mid pre post mid pre) v)))

(test-end)
