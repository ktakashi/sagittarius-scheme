(import (rnrs)
	(sagittarius) ;; for format
	(util concurrent)
	(srfi :18)
	(srfi :64)
	(match))

(test-begin "Concurrent utilities")

;; for my laziness...
(define make-executor make-thread-pool-executor)
(define executor-pool-size thread-pool-executor-pool-size)
(define executor-max-pool-size thread-pool-executor-max-pool-size)

;; simple future
(let ((f1 (future 'ok))
      (f2 (future (error 'dummy "dummy"))))
  (test-assert "simple-future?" (simple-future? f1))
  (test-assert "simple-future? (2)" (simple-future? f2))
  (test-equal "simple-future? get" 'ok (future-get f1))
  (test-error "simple-future? get" error? (future-get f2))
  )

(let ((executor (make-executor 1)))
  (test-assert "executor?" (executor? executor))
  (test-equal "max pool size" 1 (executor-max-pool-size executor))
  (test-equal "pool size (1)" 0 (executor-pool-size executor))
  (test-equal "state" 'running (executor-state executor)))

(let ((future (make-executor-future (lambda () 1))))
  (test-assert "future?" (future? future))
  (test-assert "future-done?" (not (future-done? future)))
  (test-assert "future-cancelled?" (not (future-cancelled? future)))
  (test-error "future-get" condition? (future-get future))
  ;; this won't raise an error anymore
  ;; (test-error "future-cancel" condition? (future-cancel future))
  )

(let ((e (make-executor 1))
      (f1 (future (class <executor-future>) 1))
      (f2 (future (class <executor-future>) (thread-sleep! 10)))
      (f3 (future (class <executor-future>) (thread-sleep! 10))))
  (test-assert "executor?" (executor? (execute-future! e f1)))
  (future-get f1)
  (test-equal "future-get(1)" 1 (future-get f1))
  (test-assert "future-done? (1)" (future-done? f1))
  (test-assert "future-cancelled? (1)" (not (future-cancelled? f1)))
  (test-assert "execute(2)" (executor? (execute-future! e f2)))
  (test-assert "available?" (not (executor-available? e)))
  (test-error "exeucte(3)" rejected-execution-error? (execute-future! e f3))
  (test-equal "pool size (2)" 1 (executor-pool-size e))
  (test-assert "future-cancel" (future-cancel f2))
  (test-assert "future-cancelled? (2)" (future-cancelled? f2))
  (test-error "future-terminated?" future-terminated? (future-get f2))
  (test-equal "pool size (3)" 0 (executor-pool-size e))
  )

;; shutdown
(let ((e (make-executor 3))
      (f1 (future (class <executor-future>) (thread-sleep! 10)))
      (f2 (future (class <executor-future>) (thread-sleep! 10)))
      (f3 (future (class <executor-future>) (thread-sleep! 10))))
  (test-assert "execute(1)" (executor? (execute-future! e f1)))
  (test-assert "execute(2)" (executor? (execute-future! e f2)))
  (test-assert "exeucte(3)" (executor? (execute-future! e f3)))
  (test-equal "pool size (4)" 3 (executor-pool-size e))
  (test-assert "shutodown" (shutdown-executor! e))
  (test-equal "pool size (5)" 0 (executor-pool-size e))
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
  (test-equal "pool size (6)" 1 (executor-pool-size e))
  (test-assert "shutodown" (shutdown-executor! e))
  (test-equal "pool size (7)" 0 (executor-pool-size e))
  (test-assert "future-cancelled? (3)" (future-cancelled? f3))
  )

(let* ((e (make-executor 1 push-future-handler))
       (sq1 (make-shared-queue))
       (sq2 (make-shared-queue))
       (f1 (future (class <executor-future>) (shared-queue-get! sq1)))
       (f2 (future (class <executor-future>) (shared-queue-get! sq2))))
  (test-assert "executor?" (executor? (execute-future! e f1)))
  (test-assert "executor? (2)" (executor? (execute-future! e f2)))
  (test-assert "available?" (not (executor-available? e)))
  ;; weird, huh?
  (test-equal "pool size" 2 (executor-pool-size e))
  ;; let it finish
  (shared-queue-put! sq1 #t)
  (shared-queue-put! sq2 #t)
  (test-assert "shutodown" (shutdown-executor! e))  
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

;; max-length test
(let ()
  (define shared-queue (make-shared-queue 1))
  (test-equal "max-length" 1 (shared-queue-max-length shared-queue))

  (test-equal "shared-queue-put!" 1 (shared-queue-put! shared-queue 1))
  (test-equal "shared-queue-put! (timeout)" 'boom 
	      (shared-queue-put! shared-queue 1 1 'boom))
  (test-assert "shared-queue-overflows?" 
	       (shared-queue-overflows? shared-queue 1))
  )

;; max-length 0
(let ()
  (define shared-queue (make-shared-queue 0))
  (define results '())
  (define reader
    (make-thread
     (lambda ()
       (set! results (cons 'r results))
       (shared-queue-get! shared-queue))))

  (define writer
    (thread-start!
     (make-thread
      (lambda ()
	(shared-queue-put! shared-queue 'wakeup)
	(set! results (cons 'w results))))))
  (test-equal "max-length" 0 (shared-queue-max-length shared-queue))
  (test-assert "shared-queue-overflows?" 
	       (shared-queue-overflows? shared-queue 1))
  ;; queue dosn't have reader
  (test-equal "shared-queue-put!  (timeout)"
	      #f (shared-queue-put! shared-queue 1 0 #f))
  (thread-start! reader)
  (test-equal "join!" 'wakeup (thread-join! reader))
  (test-equal "results" '(w r) results)
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
  (test-equal "size" 5 (thread-pool-size pool))
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

;; shared-priority-queue
(let ()
  (define spq (make-shared-priority-queue compare))
  (define (push . args)
    (for-each (lambda (arg)
		(test-assert (format "shared-priority-queue-put! ~a" arg)
			     (shared-priority-queue-put! spq arg)))
	      args))
  (define (pop . args)
    (for-each (lambda (arg)
		(test-equal (format "shared-priority-queue-get! ~a" arg)
			    arg
			    (shared-priority-queue-get! spq)))
	      args))
  (push 10 8 1 9 7 5 6)
  (pop 1 5 6 7 8 9 10)

  (push 9 8 10)
  (test-assert "shared-priority-queue-remove! (1)"
	       (shared-priority-queue-remove! spq 9))
  (test-assert "shared-priority-queue-remove! (2)"
	       (not (shared-priority-queue-remove! spq 9)))
  (pop 8 10)

  (let* ((e (make-thread-pool-executor 1))
	 (lock (make-mutex))
	 (cv (make-condition-variable))
	 (ready? #f)
	 (f (make-executor-future
	      (lambda ()
		;; wait until ready
		(unless ready? (mutex-unlock! lock cv))
		;; get all
		(let loop ((r '()))
		  (if (shared-priority-queue-empty? spq)
		      (reverse! r)
		      (loop (cons (shared-priority-queue-get! spq) r))))))))
    (mutex-lock! lock)
    (execute-future! e f)
    (push 5 1 7 2 8 4 6 3)
    ;; on single core environment, the thread would be executed after
    ;; the main thread process is done. so we need to do this trick,
    ;; otherwise it wait until condition-variable is signaled which
    ;; is already done by main thread...
    (set! ready? #t)
    (condition-variable-broadcast! cv)
    (mutex-unlock! lock)
    (test-equal "sync" '(1 2 3 4 5 6 7 8) (future-get f))))

(test-end)
