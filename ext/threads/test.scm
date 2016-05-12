;; -*- scheme -*-

;; testing mutil thread
;; this file will be called from one upper
;; so load path must be like this
(add-load-path "./threads")
(add-load-path "./time/")
(import (srfi :64 testing)
	(srfi :39)
	(rnrs)
	(sagittarius)
	(sagittarius threads)
	(sagittarius time))
;; because of internal define and macro problem...

(test-begin "(run-threads-test)")
;; most test cases are from Gauche 
;; basic API
(test-assert "current-thread" (eq? (current-thread) (current-thread)))
(test-equal "thread?" '(#t #f) (list (thread? (current-thread))
				     (thread? 'foo)))
(test-assert "make-thread" (thread? (make-thread (lambda () #f))))
(test-equal "thread-name" 'foo (thread-name (make-thread
					     (lambda () #f) 'foo)))
(test-equal "thread-specific" "hello"
	    (begin
	      (thread-specific-set! (current-thread) "hello")
	      (thread-specific (current-thread))))

(test-equal "thread-start!" "hello"
	    (call-with-string-output-port
	     (lambda (p)
	       (let ((t (thread-start! (make-thread 
					(lambda () (display "hello" p))))))
		 (thread-join! t)))))

;; calculate fibonacchi in awful way
(let ()
  (define (mt-fib n)
    (let ((threads (make-vector n)))
      (let loop ((i 0))
	(unless (= i n)
	  (vector-set! 
	   threads i
	   (make-thread
	    (case i
	      ((0) (lambda () 1))
	      ((1) (lambda () 1))
	      (else
	       (lambda ()
		 (+ (thread-join! (vector-ref threads (- i 1)))
		    (thread-join! (vector-ref threads (- i 2)))))))))
	  (loop (+ i 1))))
      (let loop ((i 0))
	(unless (= i n)
	  (thread-start! (vector-ref threads (- n i 1)))
	  (loop (+ i 1))))
      (thread-join! (vector-ref threads (- n 1)))))
  (test-equal "thread-join!" 1346269 (mt-fib 31)))

(let ((t1 (make-thread (lambda ()
			 (let loop ()
			   (sys-nanosleep #e5e8)
			   (loop))))))
  (test-equal "thread-state(1)" 'new (thread-state t1))
  (thread-start! t1)
  (test-equal "thread-state(2)" 'runnable (thread-state t1))
  (thread-stop! t1)
  (test-equal "thread-state(3)" 'stopped (thread-state t1))
  (thread-stop! t1) ;; duplicate stop test
  (test-equal "thread-state(4)" 'stopped (thread-state t1))
  (thread-cont! t1)
  (test-equal "thread-state(5)" 'runnable (thread-state t1))
  (thread-terminate! t1)
  (test-equal "thread-state(6)" 'terminated
	      (guard (e ((terminated-thread-exception? e)
			 (thread-state t1))
			(else
			 (print e)))
		(thread-join! t1))))

;; thread and error
(test-assert "uncaught-exception"
	     (let ((t (make-thread (lambda ()
				     (assertion-violation 'who "foo")))))
	       (thread-start! t)
	       (with-error-handler
		 (lambda (e)
		   (and (uncaught-exception? e)
			(assertion-violation?
			 (uncaught-exception-reason e))))
		 (lambda () (thread-join! t)))))

(test-assert "uncaught-exception"
	     (let ((t (make-thread (lambda () (raise 4)))))
	       (thread-start! t)
	       (with-error-handler
		 (lambda (e)
		   (and (uncaught-exception? e)
			(eqv? (uncaught-exception-reason e) 4)))
		 (lambda () (thread-join! t)))))

(test-assert "uncaught-exception"
	     (let ((t (make-thread
		       (lambda ()
			 (with-error-handler
			   (lambda (e) e)
			   (lambda () (assertion-violation 'who "foo")))))))
	       (thread-start! t)
	       (with-error-handler
		 (lambda (e) e)
		 (lambda ()
		   (assertion-violation? (thread-join! t))))))

(let ((m (make-mutex 'mutex-1)))
  (test-assert 'mutex? (mutex? m))
  (test-equal "mutex name" (mutex-name m) 'mutex-1)
  (mutex-specific-set! m 'specific-data)
  (test-equal "mutex specific" (mutex-specific m) 'specific-data)
  (test-equal "mutex state" (mutex-state m) 'not-abandoned)
  (test-assert "mutex locked" (mutex-lock! m))
  (test-equal "mutex state 2" (mutex-state m) (current-thread))
  (mutex-unlock! m)
  (mutex-lock! m #f #f)
  (test-equal "mutex state 3" (mutex-state m) 'not-owned)
  (test-assert "mutex unlocked" (mutex-unlock! m))
  (test-equal "mutex state 4" (mutex-state m) 'not-abandoned)
  )

(test-equal "lock and unlock - blocking (simple spin-lock)" 
	    '((put a) (get a) (put b) (get b) (put c) (get c))
	    (let ((log '())
		  (cell #f)
		  (m (make-mutex)))
	      (define (put! msg)
		(mutex-lock! m)
		(if cell
		    (begin (mutex-unlock! m) (put! msg))
		    (begin (set! cell msg)
			   (set! log (cons `(put ,msg) log))
			   (mutex-unlock! m))))
	      (define (get!)
		(mutex-lock! m)
		(if cell
		    (let ((r cell))
		      (set! cell #f)
		      (set! log (cons `(get ,r) log))
		      (mutex-unlock! m)
		      r)
		    (begin (mutex-unlock! m) (get!))))
	      (define (producer)
		(put! 'a)
		(put! 'b)
		(put! 'c))
	      (define (consumer)
		(get!)
		(get!)
		(get!))
	      (let ((tp (thread-start! (make-thread producer 'producer)))
		    (tc (thread-start! (make-thread consumer 'consumer))))
		(thread-join! tp)
		(thread-join! tc)
		(reverse log))))

(test-equal "lock with timeout"
	    '(#t #f #f #f #f #t #t)
	    (let ((m (make-mutex)))
	      (let* ((r0 (mutex-lock! m))
		     (r1 (mutex-lock! m 0))
		     (r2 (mutex-lock! m 0.05))
		     ;; we do not support time object for mutex-lock
		     (r3 (mutex-lock! 
			  m (seconds->time (+ (time->seconds (current-time))
					      0.05))))
		     (r4 (mutex-lock!
			  m (seconds->time (- (time->seconds (current-time))
					      0.05))))
		     (r5 (mutex-unlock! m))
		     (r6 (mutex-lock! m 0)))
		(mutex-unlock! m)
		(list r0 r1 r2 r3 r4 r5 r6))))

(test-equal "recursive mutex" (list (current-thread) 0 'not-abandoned)
	    (let ((m (make-mutex)))
	      (mutex-specific-set! m 0)
	      (mutex-lock-recursively! m)
	      (mutex-lock-recursively! m)
	      (mutex-lock-recursively! m)
	      (let ((r0 (mutex-state m)))
		(mutex-unlock-recursively! m)
		(mutex-unlock-recursively! m)
		(let ((r1 (mutex-specific m)))
		  (mutex-unlock-recursively! m)
		  (list r0 r1 (mutex-state m))))))

;; condition variables
(test-assert "make-condition-variable"
	     (condition-variable? (make-condition-variable)))

(test-equal "condition-variable-name" 'foo
	    (condition-variable-name (make-condition-variable 'foo)))

(test-equal "condition-variable-specific" "hello"
	    (let ((c (make-condition-variable 'foo)))
	      (condition-variable-specific-set! c "hello")
	      (condition-variable-specific c)))

;; Producer-consumer model using condition variable
(test-equal "condition-variable-signal!"
	    '((put a) (get a) (put b) (get b) (put c) (get c))
	    (let ((log '())
		  (cell #f)
		  (m  (make-mutex))
		  (put-cv (make-condition-variable))
		  (get-cv (make-condition-variable)))
	      (define (put! msg)
		(mutex-lock! m)
		(if cell
		    (begin (mutex-unlock! m put-cv) (put! msg))
		    (begin (set! cell msg)
			   (set! log (cons `(put ,msg) log))
			   (condition-variable-signal! get-cv)
			   (mutex-unlock! m))))
	      (define (get!)
		(mutex-lock! m)
		(if cell
		    (let ((r cell))
		      (set! cell #f)
		      (set! log (cons `(get ,r) log))
		      (condition-variable-signal! put-cv)
		      (mutex-unlock! m)
		      r)
		    (begin
		      (mutex-unlock! m get-cv) (get!))))
	      (define (producer)
		(put! 'a)
		(put! 'b)
		(put! 'c))
	      (define (consumer)
		(get!)
		(get!)
		(get!))
	      (let ((tp (thread-start! (make-thread producer 'producer)))
		    (tc (thread-start! (make-thread consumer 'consumer))))
		(thread-join! tp)
		(thread-join! tc)
		(reverse log))))

;; thread local parameters
(let ()        
  (define *thr1-val* #f)
  (define *thr2-val* #f)
  (define p (make-parameter 3))
  
  (test-equal "check locality of parameters"
	      '(3 4 5)
	      (let ((th1 (make-thread
			  (lambda ()
			    (p 4)
			    (set! *thr1-val* (p)))))
		    (th2 (make-thread
			  (lambda ()
			    (p 5)
			    (set! *thr2-val* (p))))))
		(thread-start! th1)
		(thread-start! th2)
		(thread-join! th1)
		(thread-join! th2)
		(list (p) *thr1-val* *thr2-val*))))

;; #41
(test-assert "real number timeout value" (thread-sleep! 0.001))

;; CLOS extra
;; need find-library
(import (clos user) (sagittarius vm))
(define-method local (a) a)
(let ()
  (define (thunk)
    (let-method ((local ((a <integer>)) (+ a 1)))
      (local 1)
      (sys-nanosleep 10))
    (local 1))
  (test-equal "let-method" '(1 1 1 1 1)
	      (let ((ts (map thread-start! 
			     (let loop ((i 0) (r '()))
			       (if (= i 5)
				   r
				   (loop (+ i 1)
					 (cons (make-thread thunk) r)))))))
		(map thread-join! ts))))

(let ()
  (define (thunk)
    (let-method ((local ((a <integer>)) (+ a 1)))
      (test-equal "local <symbol>" 'a (local 'a)))
    (local 1))
  (let-method ((local ((a <symbol>)) a))
    (test-equal "let-method (2)" 1
		(let ((ts (thread-start! (make-thread thunk))))
		  (thread-join! ts)))))

;; interactive thing
(let ()
  (define eval1 '(define-method local ((a <symbol>)) 'eval1))
  (define eval2 '(define-method local ((a <symbol>)) 'eval2))
  (define eval3 '(define-method local ((a <symbol>)) 'eval3))
  (define (make-thunk expr)
    (lambda () 
      (eval expr (current-library))
      (local 'a)))
  (define (make-thunk2 expr)
    (lambda () 
      (eval expr (find-library 'user #f))
      (local 'a)))

  (let ((t1 (make-thread (make-thunk eval1)))
	(t2 (make-thread (make-thunk eval2)))
	(t3 (make-thread (make-thunk2 eval3))))
    (thread-start! t1)
    (thread-start! t2)
    (test-equal "local eval1" 'eval1 (thread-join! t1))
    (test-equal "local eval2" 'eval2 (thread-join! t2))
    (test-equal "local (3)" 'a (local 'a))
    ;; must be done after all other thread is done
    ;; this should affect globally
    (thread-start! t3)
    (test-equal "local eval3" 'eval3 (thread-join! t3))
    (test-equal "local (4)" 'eval3 (local 'a))
    ))

;; error case
(let ()
  (define eval1 '(define-method local (a) 'wont-return))
  (define eval2 '(define-method local ((a <symbol>)) 'replaced))
  (define (make-thunk expr)
    (lambda () 
      (eval expr (current-library))
      (local 'a)))
  (define (make-thunk2 expr)
    (lambda () 
      (eval expr (find-library 'user #f))
      (local 'a)))

  (let ((t1 (make-thread (make-thunk eval1)))
	(t2 (make-thread (make-thunk2 eval2))))
    (thread-start! t1)
    (test-error "local error" condition? (thread-join! t1))
    (test-equal "local (5)" 'eval3 (local 'a))
    ;; must be done after all other thread is done
    ;; this should affect globally
    (thread-start! t2)
    (test-equal "local replace" 'replaced (thread-join! t2))
    (test-equal "local (6)" 'replaced (local 'a))
    ))

;; replace in local context
(let ()
  (define eval1 '(define-method local ((a <integer>)) a))
  (define eval2 '(define-method local ((a <integer>)) (+ a 1)))
  (define thunk
    (lambda () 
      (eval eval1 (current-library))
      ;; replace it
      (eval eval2 (current-library))
      (local 1)))
  (let ((t1 (make-thread thunk)))
    (thread-start! t1)
    (test-equal "local replace in context" 2 (thread-join! t1))))

;; class redefintion
(import (clos core))
(let ()
  (define eval1 '(define-class <foo> () ()))
  (define eval2 '(define-class <foo> () (a)))
  (define thunk
    (lambda () 
      (eval eval1 (current-library))
      ;; replace it
      (eval eval2 (current-library))
      (let ((class (eval '<foo> (current-library))))
	(slot-exists? (make class) 'a))))
  (let ((t1 (make-thread thunk)))
    (thread-start! t1)
    (test-assert "local <foo> redefinition" (thread-join! t1))))

;; now define it globally
(define-class <foo> () ())
(let ()
  (define eval1 '(define-class <foo> () (a)))
  (define thunk
    (lambda () 
      (eval eval1 (current-library))))
  (let ((t1 (make-thread thunk)))
    (thread-start! t1)
    (test-error "global <foo> redefinition" condition? (thread-join! t1))))

;; some of global defined methods
(let ()
  (define def-lib '(library (a-method) 
		       (export a-method)
		       (import (clos user))
		     (define-generic a-method)))
  (define ext-lib '(library (a-method-impl) 
		       (export a-method)
		       (import (a-method) (clos user))
		     (define-method a-method (a) a)))
  (eval def-lib (current-library))
  (let ((t (make-thread (lambda () (eval ext-lib (current-library))))))
    (thread-start! t)
    (thread-join! t)
    (test-equal "a-method" 'a (eval '(a-method 'a)
				    (environment '(rnrs) '(a-method))))))

;; port
(let ((rr '())  ;; read count
      (wr '()))  ;; write count

  (define (make-custom)
    (define (read! bv start count)
      (set! rr (cons 'read rr))
      (error 'dummy "dummy")
      count)
    (define (write! bv start count)
      (set! wr (cons 'write wr))
      count)
    (define (close) #t)
    (make-custom-binary-input/output-port "custom" read! write! #f #f close))

  (let ((in/out (make-custom)))
    (define (thunk read?)
      (lambda ()
	(if read?
	    (get-bytevector-n in/out 5)
	    (put-bytevector in/out #vu8(1 2 3 4 5)))))
    (define (safe-join! t)
      (guard (e (else #t))
	(thread-join! t)))
    (let ((ts (map thread-start!
		   (map (lambda (read?) (make-thread (thunk read?)))
			'(#t #f #t #f #t #f)))))
      (for-each safe-join! ts)
      (test-equal "read lock" '(read read read) rr)
      (test-equal "write lock" '(write write write) wr))))

;; call #126
(let ()
  (define box (make-vector 1))
  (define lock (make-mutex))
  (define waiter (make-condition-variable))
  
  (define t 
    (thread-start!
     (make-thread
      (lambda ()
	(mutex-unlock! lock waiter)
	((vector-ref box 0))))))
  
  (define (do-param v)
    (let ((r1 (eval '(*param*) (environment '(param)))))
      (eval `(*param* ,v) (environment '(param)))
      (list r1 (eval '(*param*) (environment '(param))))))
  (define (run-thread param)
    (thread-join! (thread-start! (make-thread (lambda () (do-param param))))))
  (eval '(library (param)
	     (export *param*)
	     (import (core) (sagittarius parameters))
	   (define *param* (make-parameter 10)))
	'(sagittarius))
  ;; child thread loads the parameter
  (test-equal "create parameter" '(10 :changed) (run-thread :changed))
  ;; main thread
  (test-equal "parent thread" '(10 :changed-main) (do-param :changed-main))
  ;; other thread?  
  (test-equal "child thread" '(:changed-main :changed-other)
	      (run-thread :changed-other))
  
  (vector-set! box 0 (lambda () (do-param :changed-before)))
  (condition-variable-broadcast! waiter)
  (test-equal "thread created before" '(10 :changed-before) (thread-join! t))
  )

;; semaphore
;; OSX doesn't support anonymous semaphore, THANK YOU VERY MUCH!
(cond-expand
 (darwin
  (test-error "anonymous semaphore" implementation-restriction-violation?
	      (make-semaphore #f 1))
  )
 (else (test-assert "semaphore?" (let* ((s (make-semaphore #f 1))
					(r (semaphore? s)))
				   (semaphore-destroy! s)))))

;; error
(test-error "make semaphore (error)"
	    assertion-violation? (make-semaphore #f -1))
(test-error "open semaphore (error 1)"
	    assertion-violation? (open-semaphore #f))
(test-error "open semaphore (error 2)"
	    i/o-error? (open-semaphore "/not exist"))

;; ditto
(cond-expand
 ((not darwin)
  (let ((sem (make-semaphore #f 1))) ;; anonymous binary semaphore
    (define counter 0)
    (define ts (map (lambda (index)
		      (make-thread
		       (lambda ()
			 (semaphore-wait! sem)
			 (set! counter (+ counter 1))
			 (semaphore-post! sem)))) '(1 2)))
    (for-each thread-start! ts)
    (for-each thread-join! ts)
    (test-equal "semaphore" 2 counter)
    (test-assert "name" (not (semaphore-name sem))) ;; anonymous returns #f
    (test-assert "destroy" (semaphore-destroy! sem))))
 (else #f))
		     


(test-end)
