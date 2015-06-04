(import (rnrs)
	(util concurrent)
	(srfi :18)
	(srfi :64)
	(match))

(test-begin "Concurrent utilities")

;; for my laziness...
(define make-executor make-thread-pool-executor)
(define executor-pool-size thread-pool-executor-pool-size)
(define executor-max-pool-size thread-pool-executor-max-pool-size)

(let ((executor (make-executor 1)))
  (test-assert "executor?" (executor? executor))
  (test-equal "max pool size" 1 (executor-max-pool-size executor))
  (test-equal "pool size" 0 (executor-pool-size executor))
  (test-equal "state" 'running (executor-state executor)))

(let ((future (make-executor-future (lambda () 1))))
  (test-assert "future?" (future? future))
  (test-assert "future-done?" (not (future-done? future)))
  (test-assert "future-cancelled?" (not (future-cancelled? future)))
  (test-error "future-get" condition? (future-get future))
  (test-error "future-cancel" condition? (future-cancel future))
  )

(let ((e (make-executor 1))
      (f1 (future (class <executor-future>) 1))
      (f2 (future (class <executor-future>) (thread-sleep! 10)))
      (f3 (future (class <executor-future>) (thread-sleep! 10))))
  (test-assert "execute" (executor? (execute-future! e f1)))
  (future-get f1)
  (test-equal "future-get(1)" 1 (future-get f1))
  (test-assert "future-done? (1)" (future-done? f1))
  (test-assert "future-cancelled? (1)" (not (future-cancelled? f1)))
  (test-assert "execute(2)" (executor? (execute-future! e f2)))
  (test-assert "available?" (not (executor-available? e)))
  (test-error "exeucte(3)" rejected-execution-error? (execute-future! e f3))
  (test-equal "pool size" 1 (executor-pool-size e))
  (test-assert "future-cancel" (future-cancel f2))
  (test-assert "future-cancelled? (2)" (future-cancelled? f2))
  (test-equal "pool size" 0 (executor-pool-size e))
  )

;; shutdown
(let ((e (make-executor 3))
      (f1 (future (class <executor-future>) (thread-sleep! 10)))
      (f2 (future (class <executor-future>) (thread-sleep! 10)))
      (f3 (future (class <executor-future>) (thread-sleep! 10))))
  (test-assert "execute(1)" (executor? (execute-future! e f1)))
  (test-assert "execute(2)" (executor? (execute-future! e f2)))
  (test-assert "exeucte(3)" (executor? (execute-future! e f3)))
  (test-equal "pool size" 3 (executor-pool-size e))
  (test-assert "shutodown" (shutdown-executor! e))
  (test-equal "pool size" 0 (executor-pool-size e))
  (test-assert "future-cancelled? (1)" (future-cancelled? f1))
  (test-assert "future-cancelled? (2)" (future-cancelled? f2))
  (test-assert "future-cancelled? (3)" (future-cancelled? f3))
  )

(let ((e (make-executor 1 terminate-oldest-handler))
      (f1 (future (class <executor-future>) (thread-sleep! 10)))
      (f2 (future (class <executor-future>) (thread-sleep! 10)))
      (f3 (future (class <executor-future>) (thread-sleep! 10))))
  (test-assert "execute(1)" (executor? (execute-future! e f1)))
  (test-assert "execute(2)" (executor? (execute-future! e f2)))
  (test-assert "future-cancelled? (1)" (future-cancelled? f1))
  (test-assert "exeucte(3)" (executor? (execute-future! e f3)))
  (test-assert "future-cancelled? (2)" (future-cancelled? f2))
  (test-equal "pool size" 1 (executor-pool-size e))
  (test-assert "shutodown" (shutdown-executor! e))
  (test-equal "pool size" 0 (executor-pool-size e))
  (test-assert "future-cancelled? (3)" (future-cancelled? f3))
  )

;; concurrent executor 
(let* ((e (make-executor 1))
       (t1 (make-thread (lambda ()
			  (let ((f (future (thread-sleep! 5))))
			    (execute-future! e f)))))
       (t2 (make-thread (lambda ()
			  (let ((f (future (thread-sleep! 5))))
			    (execute-future! e f))))))
  ;; hope we get there
  (map thread-start! (list t1 t2))
  (thread-sleep! 0.1)
  (test-equal "pool size" 1 (executor-pool-size e))
  (test-error "failed to add" uncaught-exception?
	      ;; one of them must be failed
	      (begin (thread-join! t1) (thread-join! t2)))
  )

;; shared-queue
;; TODO test case for max-length
(let ()
  (define (open-account initial-amount out)
    (define shared-queue (make-shared-queue))
    (define (process)
      (define balance initial-amount)
      (let loop ()
	(match (shared-queue-get! shared-queue)
	  (('withdrow how-much)
	   (if (< balance how-much)
	       (begin (shared-queue-put! out "invalid amount") (loop))
	       (begin
		 (set! balance (- balance how-much))
		 (shared-queue-put! out (cons how-much balance))
		 (loop))))
	  (('deposite a)
	   (if (negative? a)
	       (begin (shared-queue-put! out "invalid amount") (loop))
	       (begin
		 (set! balance (+ balance a))
		 (shared-queue-put! out (cons 0 balance))
		 (loop))))
	  (('close) #t)
	  (else "invalid message"0))))

    (let ((t (make-thread process)))
      (thread-start! t)
      shared-queue))

  (define recepit (make-shared-queue))
  (define client (open-account 1000 recepit))
  (define eager-client
    (thread-start!
     (make-thread
      (lambda ()
	(test-equal "shared-queue-get (wait)" '(100 . 900)
		    (shared-queue-get! recepit))))))

  (shared-queue-put! client '(withdrow 100))
  (shared-queue-put! client '(deposite 100))
  (shared-queue-put! client '(close))
  ;; wait until account closed
  (thread-sleep! 0.1)

  (test-equal "size"  1 (shared-queue-size recepit))
  (test-equal "shared-queue-get (2)" '(0 . 1000) (shared-queue-get! recepit))
  (test-assert "empty?" (shared-queue-empty? recepit))
  )

;; thread-pool
(let ((pool (make-thread-pool 5)))
  (define (crop sq)
    (let loop ((r '()))
      (if (shared-queue-empty? sq)
	  r
	  (loop (cons (shared-queue-get! sq) r)))))
  (define (custom-add-to-back n)
    (if (negative? n)
	(error 'dummy "failed to add")
	#f))
  (test-assert "thread-pool?" (thread-pool? pool))
  (test-assert "push!" (thread-pool-push-task! pool (lambda () #t)))
  (test-assert "wait!" (thread-pool-wait-all! pool))
  (let ((sq (make-shared-queue)))
    (do ((i 0 (+ i 1)))
	((= i 10)
	 (thread-pool-wait-all! pool)
	 (test-equal "result"
		     '(0 1 2 3 4 5 6 7 8 9)
		     (list-sort < (crop sq))))
      (thread-pool-push-task! pool
			      (lambda ()
				(thread-sleep! 0.1)
				(shared-queue-put! sq i)))))
  (test-error "optional handler" error?
	      (do ((i 0 (+ i 1)))
		  ((= i 10) (thread-pool-wait-all! pool) #f)
		(thread-pool-push-task! pool
					;; just wait
					(lambda () (thread-sleep! 1))
					custom-add-to-back)))
  )

(test-end)
