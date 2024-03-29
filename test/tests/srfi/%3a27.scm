(import (rnrs)
        (srfi :27)
	(srfi :64))


(test-begin "SRFI 27")

(test-assert "in range (1)"
	     (let loop ((k 0) (n 1))
	       (cond ((> k 1024) #t)
		     ((<= 0 (random-integer n) (- n 1))
		      (loop (+ k 1) (* n 2)))
		     (else #f))))

(test-assert "in range (2)"
	     (let loop ((k 0) (n 1))
	       (if (> k 1000)
		   #t
		   (let ((x (random-real)))
		     (if (< 0 x 1)
			 (loop (+ k 1) (* n 2))
			 #f)))))

(let ()
  (define my-random-source (make-random-source))
  (define my-random-integer
    (let ((random-integer (random-source-make-integers my-random-source)))
      (lambda (n)
	(let ((x (random-integer n)))
	  (if (<= 0 x (- n 1))
	      x
	      (error "(random-integer n) returned illegal value" x))))))
  (let ((state1 (random-source-state-ref my-random-source))
	(x1 (my-random-integer (expt 2 32)))
	(state2 (random-source-state-ref my-random-source))
	(x2 (my-random-integer (expt 2 32))))
    (random-source-state-set! my-random-source state1)
    (test-equal "state restore(1)" x1 (my-random-integer (expt 2 32)))
    (random-source-state-set! my-random-source state2)
    (test-equal "state restore(2)" x2 (my-random-integer (expt 2 32)))))

(test-end)
