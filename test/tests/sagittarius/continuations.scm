(import (rnrs)
	(sagittarius continuations)
	(srfi :64))

(test-begin "Continuations")

(test-assert "continuation? (call/cc)"
	     (continuation? (call/cc values)))
(test-assert "continuation? (call/comp)"
	     (continuation? (call/prompt (lambda () (call/comp values)))))
(test-assert "continuation? (call/delimited-cc)"
	     (continuation? (call/prompt (lambda () (call/delim-cc values)))))

(test-assert "continuation? (symbol)" (not (continuation? 'a)))
(test-assert "continuation? (symbol)" (not (continuation? (lambda args args))))

(test-assert (not (composable-continuation? (call/cc values))))
(test-assert (not (composable-continuation?
		   (call/prompt (lambda () (call/delim-cc values))))))

(test-assert (composable-continuation?
	      (call/prompt (lambda () (call/comp values)))))

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

(let ((p1 (make-continuation-prompt-tag 'p1))
      (p2 (make-continuation-prompt-tag 'p2)))
  ;; using delim-cc
  (let ((k (call-with-continuation-prompt
	    (lambda ()
	      (test-assert "p1 (#t)" (continuation-prompt-available? p1))
	      (test-assert "p2 (#f)" (not (continuation-prompt-available? p2)))
	      (call-with-continuation-prompt
	       (lambda ()
		 (call/delim-cc values p2))
	       p2))
	    p1)))
    (test-assert "delim-cc p1 (#f)" (not (continuation-prompt-available? p1 k)))
    (test-assert "delim-cc p2 (#t)" (continuation-prompt-available? p2 k)))

  ;; using full continuation
  (let ((k (call-with-continuation-prompt
	    (lambda ()
	      (call-with-continuation-prompt
	       (lambda () (call/cc values))
	       p2))
	    p1)))
    (test-assert "call/cc p1 (#t)" (continuation-prompt-available? p1 k))
    (test-assert "call/cc p2 (#t)" (continuation-prompt-available? p2 k))))

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

;; From SRFI-226

(test-equal 4 (+ 1 (reset 3)))
(test-equal 5 (+ 1 (reset (* 2 (shift k 4)))))
(test-equal 9 (+ 1 (reset (* 2 (shift k (k 4))))))
(test-equal 17 (+ 1 (reset (* 2 (shift k (k (k 4)))))))
(test-equal 25 (+ 1 (reset (* 2 (shift k1 (* 3 (shift k2 (k1 (k2 4)))))))))

(let ()
  (define call-with-non-composable-continuation call/delim-cc)
  (test-equal 990
	      (let ([tag (make-continuation-prompt-tag)])
		(* 2
		   (call-with-continuation-prompt
		    (lambda ()
		      (* 3
			 (call-with-non-composable-continuation
			  (lambda (k)
			    (* 5
			       (call-with-continuation-prompt
				(lambda ()
				  (* 7 (k 11)))
				tag)))
			  tag)))
		    tag)))))

(test-equal 6930
	    (let ([tag (make-continuation-prompt-tag)])
	      (* 2
		 (call-with-continuation-prompt
		  (lambda ()
		    (* 3
		       (call-with-composable-continuation
			(lambda (k)
			  (* 5
			     (call-with-continuation-prompt
			      (lambda ()
				(* 7 (k 11)))
			      tag)))
			tag)))
		  tag))))

(test-equal 7 (prompt (+ 2 (control k (k 5)))))
(test-equal 5 (prompt (+ 2 (control k 5))))
(test-equal 12 (prompt (+ 5 (prompt (+ 2 (control k1 (+ 1 (control k2 (k2 6)))))))))
(test-equal 8 (prompt (+ 5 (prompt (+ 2 (control k1 (+ 1 (control k2 (k1 6)))))))))
(test-equal 18 (prompt
		(+ 12 (prompt (+ 5 (prompt (+ 2 (control k1 (control k2 (control k3 (k3 6)))))))))))

(define-syntax let/prompt
  (syntax-rules ()
    ((_ ((var val)  ...) body ...)
     (let/prompt (default-continuation-prompt-tag) ((var val) ...) body ...))
    ((_ tag ((var val)  ...) body ...)
     (call-with-continuation-prompt
      (lambda ()
	(let ((var val) ...) body ...))
      tag))))

(let/prompt ()
  (define call-with-non-composable-continuation call/delim-cc)
  (define tag (make-continuation-prompt-tag))
  (call-with-continuation-prompt
   (lambda ()
     (test-assert
      (continuation-prompt-available? tag
       (call-with-non-composable-continuation values))))
   tag)
  (call-with-continuation-prompt
   (lambda ()
     (test-assert
      (continuation-prompt-available? tag
       (call-with-non-composable-continuation values tag))))
   tag)
  (call-with-continuation-prompt
   (lambda ()
     (test-assert
      (not (continuation-prompt-available? tag
	    (call-with-composable-continuation values tag)))))
   tag))

(test-end)
