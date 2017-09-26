(import (rnrs)
	(sagittarius sandbox)
	(sagittarius)
	(srfi :18)
	(srfi :64))

(test-begin "Sagittarius Sandbox")

(define (global) 'global)

(define (global-check expected)
  (test-equal (format "check ~a" expected) expected (global)))

(global-check 'global)

(with-sandbox
 (lambda ()
   (define-in-sandbox (current-library) (global) 'sandbox)
   (global-check 'sandbox)))

(global-check 'global)

(test-error assertion-violation?
	    (let ()
	      (define-in-sandbox (current-library) (global) 'sandbox)
	      (global)))

(playground ((global (current-library) (lambda () 'playground)))
  (global-check 'playground))

;; threading test
(let ((lib (current-library)))
  (with-sandbox
   (lambda ()
     (define-in-sandbox lib (global) 'playground-root)
     (define threads 
       (map (lambda (v)
	      (make-thread
	       (lambda ()
		 (thread-sleep! 0.01)
		 (let ((root (global)))
		   (thread-sleep! 0.01)
		   (playground ((global lib (lambda () v)))
		     (list root (global))))))) '(1 2 3 4 5)))

    (for-each thread-start! threads)
    (test-equal '((playground-root 1)
		  (playground-root 2)
		  (playground-root 3)
		  (playground-root 4)
		  (playground-root 5))
		(map thread-join! threads)))))

(test-end)
