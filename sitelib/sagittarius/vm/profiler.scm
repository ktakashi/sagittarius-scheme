;; -*- scheme -*-
(library (sagittarius vm profiler)
    (export profiler-start
	    profiler-stop
	    profiler-show
	    profiler-get-result
	    profiling)
    (import (core)
	    (core base)
	    (sagittarius))

  (define-syntax profiling
    (er-macro-transformer
     (lambda (form rename compare)
       (or (= (length form) 2)
	   (assertion-violation 'profiler-show 
				(format "wrong number of arguments, required 2, but got ~a"
					(length form))
				form))
       `(,(rename 'dynamic-wind)
	 ,(rename 'profiler-start)
	 (,(rename 'lambda) () ,(cadr form))
	 ,(rename 'profiler-stop)))))

  (define (profiler-get-result)
    (cond ((profiler-raw-result)
	   => (lambda (result)
		(hashtable-map (lambda (k v)
				 (cons k v))
			       result)))
	  (else #f)))

  (define (profiler-show results sort-by max-rows)
    (if (not results)
	(cond ((profiler-get-result)
		=> (lambda (result)
		     (show-stats result sort-by max-rows)))
	      (else
	       (print "No profiling data has been gathered.")))
	(print "No profiling data has been gathered.")))

  (define (show-stats stat sort-by max-rows)
    (let* ((num-samples (fold (lambda (entry cnt) (+ (cddr entry) cnt)) 0 stat))
	   (sum-time (* num-samples 0.01))
	   (sorter (case sort-by
		     [(time)
		      (lambda (a b)
			(or (> (cddr a) (cddr b))
			    (and (= (cddr a) (cddr b))
				 (> (cadr a) (cadr b)))))]
		     [(count)
		      (lambda (a b)
			(or (> (cadr a) (cadr b))
			    (and (= (cadr a) (cadr b))
				 (> (cddr a) (cddr b)))))]
		     [(time-per-call)
		      (lambda (a b)
			(> (/ (cddr a) (cadr a)) (/ (cddr b) (cadr b))))]
		     [else
		      (error 'profiler-show 
			     "sort-by argument must be either one of time, count, or time-per-call, but got:" sort-by)]))
	   (sorted (list-sort sorter stat)))

      (print "Profiler statistics (total "num-samples" samples, "
	     sum-time " seconds)")
      (print "                                                   num     time/    total")
      (print "Name                                               calls   call(ms) samples")
      (print "--------------------------------------------------+-------+--------+-----------")
      (for-each
       (lambda (e)
	 (let* ((name (car e))
		(ncalls  (cadr e))
		(samples (cddr e))
		)
	   (format #t "~50a ~7a ~8a ~5a(~3a%)\n"
		   name ncalls (time/call samples ncalls) samples
		   (if (zero? num-samples)
		       0
		       (exact (round (* 100 (/ samples num-samples))))))))
       (if (integer? max-rows)
	   (if (> (length sorted) max-rows)
	       (take sorted max-rows)
	       sorted)
	   sorted)))
    )

  (define (time/call samples ncalls)
    (* samples 10))

  #;(define (time/call samples ncalls)
    (let ((time (* 10.0 (/ samples ncalls)))) ;; in ms
      (receive (frac int) (modf (* time 10000))
	(let ((val (inexact->exact (if (>= frac 0.5) (+ int 1) int))))
	  (let ((q (quotient val 10000))
		(r (remainder val 10000)))
	    (format "~2d.~4,'0d" q r))))))
    
)