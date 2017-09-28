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

;; actual library
(library (sandboxing)
    (export boo (rename (reverse sandbox-reverse)
			(string-copy sandbox-string-copy)))
    (import (rnrs))
  (define (boo out) (display 'boo out)))

(import (sandboxing))
(define (test-global-sandboxing)
  (let-values (((out extract) (open-string-output-port)))
    (boo out)
    (test-equal "boo" (extract))
    (test-equal '(b a) (sandbox-reverse '(a b)))
    (test-equal "string" (sandbox-string-copy "string"))
    (test-equal "string" (string-copy "string"))))

(test-global-sandboxing)

(playground ((boo '(sandboxing) (lambda (ignore) 'sandboxed))
	     (sandbox-reverse '(sandboxing) (lambda (a) a))
	     (string-copy '(rnrs) (lambda (a) "copied")))
  (test-equal 'sandboxed (boo #t))
  (test-equal '(a b) (sandbox-reverse '(a b)))
  (test-equal '(b a) (reverse '(a b)))
  ;; even though, original binding is sandboxed, this renamed binding
  ;; won't be affected
  (test-equal "string" (sandbox-string-copy "string"))
  (test-equal "copied" (string-copy "string")))

(test-global-sandboxing)

(test-end)
