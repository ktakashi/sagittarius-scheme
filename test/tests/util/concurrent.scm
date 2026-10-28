(import (rnrs)
	(sagittarius) ;; for format
	(sagittarius control) ;; for dotimes
	(util concurrent)
	(clos user)
	(only (srfi :1) iota lset=)
	(srfi :18)
	(srfi :64)
	(srfi :197)
	(match))
#|
(define test-begin print)
(define test-equal print)
(define test-assert print)
(define-syntax test-error
  (syntax-rules ()
    ((_ expr ...)
     (guard (e (else (print e)))
       expr ...))))
(define test-end #t)
|#
(test-begin "Concurrent utilities")

;; for my laziness...
(define make-executor make-thread-pool-executor)
(define executor-pool-size thread-pool-executor-pool-size)
(define executor-max-pool-size thread-pool-executor-max-pool-size)

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
	  (else "invalid message"))))

    (let ((t (make-thread process)))
      (thread-start! t)
      (values t shared-queue)))

  (define recepit (make-shared-queue))
  (define-values (thread client) (open-account 1000 recepit))
  (define eager-client
    (thread-start!
     (make-thread
      (lambda ()
	(test-equal "shared-queue-get (wait)" '(100 . 900)
		    (shared-queue-get! recepit))))))
  ;; wait until the eager client is ready
  (shared-queue-locked? recepit #t)
  (shared-queue-put! client '(withdrow 100))
  (shared-queue-put! client '(deposite 100))
  (shared-queue-put! client '(close))
  ;; wait until eager client is done
  (thread-join! eager-client)
  (thread-join! thread)

  (test-equal "size"  1 (shared-queue-size recepit))
  (test-equal "shared-queue-get (2)" '(0 . 1000) (shared-queue-get! recepit))
  (test-assert "empty?" (shared-queue-empty? recepit))
  )

;; pop
(let ()
  (define shared-queue (make-shared-queue))
  (test-equal "shared-queue-put!" 1 (shared-queue-put! shared-queue 1))
  (test-equal "shared-queue-put!" 2 (shared-queue-put! shared-queue 2))
  (test-equal "shared-queue-pop!" 2 (shared-queue-pop! shared-queue))
  (test-equal "shared-queue-pop!" 1 (shared-queue-pop! shared-queue))
  (test-equal "shared-queue-pop!" #f (shared-queue-pop! shared-queue 0 #f))
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
  (test-equal '(1) (shared-queue->list shared-queue))
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
	(set! results (cons 'w results))
	'awake))))
  (test-equal "max-length" 0 (shared-queue-max-length shared-queue))
  (test-assert "shared-queue-overflows?" 
	       (shared-queue-overflows? shared-queue 1))
  ;; queue dosn't have reader
  (test-equal "shared-queue-put!  (timeout)"
	      #f (shared-queue-put! shared-queue 1 0 #f))
  (thread-start! reader)
  (test-equal "join!" 'wakeup (thread-join! reader))
  (test-equal "join!" 'awake (thread-join! writer))
  (test-equal "results" '(w r) results)
  )

;; lock / unlock
(let ((sq (make-shared-queue 0)))
  (test-assert (shared-queue-lock! sq))
  (test-assert (shared-queue-locked? sq))
  (test-assert (shared-queue-unlock! sq)))

;; remove!
(define (test-remove! lis o)
  (define len (length lis))
  (let ((sq (make-shared-queue)))
    (for-each (lambda (o) (shared-queue-put! sq o)) lis)
    (test-assert `(shared-queue-remove! sq ,o) (shared-queue-remove! sq o))
    (test-assert `(not (shared-queue-remove! sq ,o))
		 (not (shared-queue-remove! sq o)))
    (test-equal `(,lis ,o) (- len 1) (shared-queue-size sq))
    (test-equal `(,lis ,o)
		(remove o lis)
		(let loop ((r '()))
		  (if (shared-queue-empty? sq)
		      (reverse r)
		      (loop (cons (shared-queue-get! sq) r)))))))
(test-remove! '(1 2 3 4 5) 3)
(test-remove! '(1 2 3 4 5) 1)
(test-remove! '(1 2 3 4 5) 5)

;; shared-priority-queue
(let ()
  (define spq (make-shared-priority-queue compare))
  (define (push . args)
    (for-each (lambda (arg)
		(test-equal (format "shared-priority-queue-put! ~a" arg) arg
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

  (let* ((lock (make-mutex))
	 (cv (make-condition-variable))
	 (ready? #f)
	 (f (make-thread
	      (lambda ()
		;; wait until ready
		(unless ready? (mutex-unlock! lock cv))
		;; get all
		(let loop ((r '()))
		  (if (shared-priority-queue-empty? spq)
		      (reverse! r)
		      (loop (cons (shared-priority-queue-get! spq) r))))))))
    (mutex-lock! lock)
    (thread-start! f)
    (push 5 1 7 2 8 4 6 3)
    ;; on single core environment, the thread would be executed after
    ;; the main thread process is done. so we need to do this trick,
    ;; otherwise it wait until condition-variable is signaled which
    ;; is already done by main thread...
    (set! ready? #t)
    (condition-variable-broadcast! cv)
    (mutex-unlock! lock)
    (test-equal "sync" '(1 2 3 4 5 6 7 8) (thread-join! f))))

(let ()
  (define spq (make-shared-priority-queue compare 5))
  (test-equal "max-length" 5 (shared-priority-queue-max-length spq))

  (for-each (lambda (i) 
	      (test-equal "shared-priority-queue-put!" i
			  (shared-priority-queue-put! spq i)))
	    '(0 1 2 3 4))
  (for-each (lambda (i)
	      (test-assert (shared-priority-queue-remove! spq i))
	      (test-equal "shared-priority-queue-put!" i
			  (shared-priority-queue-put! spq i)))
	    '(0 1 2 3 4))
  (test-equal 5 (shared-priority-queue-size spq))
  (test-equal "shared-priority-queue-put! (timeout)" 'boom 
	      (shared-priority-queue-put! spq 1 1 'boom))
  (test-assert "shared-queue-overflows?" 
	       (shared-priority-queue-overflows? spq 1))
  )

(let ((data (iota 50)))
  (define spq (make-shared-priority-queue compare (length data)))
  (define thread1
    (make-thread
     (lambda ()
       (for-each (lambda (i) (shared-priority-queue-put! spq i)) data))))
  (define thread2
    (make-thread
     (lambda ()
       (for-each (lambda (i) (shared-priority-queue-remove! spq i)) data))))
  (for-each (lambda (i) (shared-priority-queue-put! spq i)) data)
  (thread-start! thread1)
  (thread-start! thread2)
  (thread-join! thread2)
  (thread-join! thread1)
  (test-assert "shared-queue-overflows?"
	       (shared-priority-queue-overflows? spq 1))
  (test-equal 50 (shared-priority-queue-capacity spq))
  (test-assert (lset= = data (shared-priority-queue->list spq))))

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
  (test-equal "idling count" 5 (thread-pool-idling-count pool))
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
  (test-assert (thread-pool-release! pool))
  )

(let* ((pool (make-thread-pool 1 raise))
       (id (thread-pool-push-task! pool (lambda () (error 'dummy "msg")))))
  
  (test-assert "wait after error" (thread-pool-wait-all! pool))
  (test-assert "no task is running" 
	       (not (thread-pool-thread-task-running? pool id)))
  (test-assert (thread-pool-release! pool))
  )

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
  (test-equal "state" 'running (executor-state executor))
  (test-assert "shutdown! (1)" (shutdown-executor! executor)))

(let ((future (make-executor-future (lambda () 1))))
  (test-assert "future?" (future? future))
  (test-assert "future-done?" (not (future-done? future)))
  (test-assert "future-cancelled?" (not (future-cancelled? future)))
  (test-error "future-get" condition? (future-get future))
  ;; this won't raise an error anymore
  ;; (test-error "future-cancel" condition? (future-cancel future))
  )

;; completable future
(letrec* ((v '())
	  (f (thunk->future (lambda () (set! v (cons 1 v)) 1))))
  (let ((f2
	 (future-map (lambda (r) (thread-sleep! 0.1) (set! v (cons 4 v)) 4)
	  (future-map (lambda (r) (thread-sleep! 0.1) (set! v (cons 3 v)) 3)
	   (future-map (lambda (r) (thread-sleep! 0.1) (set! v (cons 2 v)) 2) f)))))
  (thread-sleep! 0.5)
  (test-equal 4 (future-get f2))
  (test-equal '(4 3 2 1) v)))

(test-equal 'ok (future-get (future-guard
			     (lambda (e) 'ok)
			     (thunk->future (lambda () (raise 'boo))))))

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
  (test-assert "shutdown! (2)" (shutdown-executor! e))
  )

;; shutdown
(let ((e (make-executor 3))
      (f1 (future (class <executor-future>) (thread-sleep! 10)))
      (f2 (future (class <executor-future>) (thread-sleep! 10)))
      (f3 (future (class <executor-future>) (thread-sleep! 10))))
  (test-assert "shutdown execute(1)" (executor? (execute-future! e f1)))
  (test-assert "shutdown execute(2)" (executor? (execute-future! e f2)))
  (test-assert "shutdown exeucte(3)" (executor? (execute-future! e f3)))
  (test-equal "future timeout" 'timeout (future-get f1 0 'timeout))
  (test-equal "shutdown pool size (4)" 3 (executor-pool-size e))
  (test-assert "shutdown shutodown" (shutdown-executor! e))
  (test-equal "shutdown pool size (5)" 0 (executor-pool-size e))
  (test-assert "shutdown future-cancelled? (1)" (future-cancelled? f1))
  (test-assert "shutdown future-cancelled? (2)" (future-cancelled? f2))
  (test-assert "shutdown future-cancelled? (3)" (future-cancelled? f3))
  )

(let ((e (make-executor 1 terminate-oldest-handler))
      (f1 (future (class <executor-future>) (thread-sleep! 10)))
      (f2 (future (class <executor-future>) (thread-sleep! 10)))
      (f3 (future (class <executor-future>) (thread-sleep! 10))))
  (test-assert "terminate execute(1)" (executor? (execute-future! e f1)))
  (test-assert "terminate execute(2)" (executor? (execute-future! e f2)))
  (test-assert "terminate future-cancelled? (1)" (future-cancelled? f1))
  (test-assert "terminate exeucte(3)" (executor? (execute-future! e f3)))
  (test-assert "terminate future-cancelled? (2)" (future-cancelled? f2))
  (test-equal "terminate pool size (6)" 1 (executor-pool-size e))
  (test-assert "terminate shutodown" (shutdown-executor! e))
  (test-equal "terminate pool size (7)" 0 (executor-pool-size e))
  (test-assert "terminate future-cancelled? (3)" (future-cancelled? f3))
  )

(let ((e (make-executor 1 terminate-oldest-handler)))
  (test-assert "avoiding &abandoned-mutex-exception"
	       (dotimes (i 100)
		 (let ((f1 (future (class <executor-future>) 
				   (thread-sleep! 10))))
		   (execute-future! e f1))))
  (test-assert "shutdown for god sake" (shutdown-executor! e)))

(let* ((e (make-executor 1 push-future-handler))
       (sq1 (make-shared-queue))
       (sq2 (make-shared-queue))
       (f1 (future (class <executor-future>) (shared-queue-get! sq1)))
       (f2 (future (class <executor-future>) (shared-queue-get! sq2))))
  (test-assert "push executor?" (executor? (execute-future! e f1)))
  (test-assert "push executor? (2)" (executor? (execute-future! e f2)))
  (test-assert "push available?" (not (executor-available? e)))
  ;; weird, huh?
  (test-equal "push pool size" 2 (executor-pool-size e))
  ;; let it finish
  (shared-queue-put! sq1 #t)
  (shared-queue-put! sq2 #t)
  (test-assert "push shutodown" (shutdown-executor! e))  
  )

;; concurrent executor 
(let* ((e (make-executor 1))
       (t1 (make-thread (lambda ()
			  (let ((f (future (class <executor-future>) 
					   (thread-sleep! 5))))
			    (execute-future! e f)))))
       (t2 (make-thread (lambda ()
			  (let ((f (future (class <executor-future>)
					   (thread-sleep! 5))))
			    (execute-future! e f))))))
  ;; hope we get there
  (map thread-start! (list t1 t2))
  (thread-sleep! 0.1)
  (test-equal "pool size" 1 (executor-pool-size e))
  (test-error "failed to add" uncaught-exception?
	      ;; one of them must be failed
	      (begin (thread-join! t1) (thread-join! t2)))
  (shutdown-executor! e)
  )

(let ((e (make-executor 1 push-future-handler))
      (f* '()))
  (test-assert (let ((f (executor-submit! e (lambda () 1))))
		 (set! f* (cons f f*))
		 (future? f)))
  (test-assert (let ((f (executor-submit! e (lambda () 2))))
		 (set! f* (cons f f*))
		 (future? f)))
  (test-assert (let ((f (executor-submit! e (lambda () 3))))
		 (set! f* (cons f f*))
		 (future? f)))
  (test-equal '(3 2 1) (map future-get f*))
  (shutdown-executor! e)
  )

(let ((e (make-fork-join-executor))
      (f* '()))
  (test-assert (let ((f (executor-submit! e (lambda () 1))))
		 (set! f* (cons f f*))
		 (future? f)))
  (test-assert (let ((f (executor-submit! e (lambda () 2))))
		 (set! f* (cons f f*))
		 (future? f)))
  (test-assert (let ((f (executor-submit! e (lambda () 3))))
		 (set! f* (cons f f*))
		 (future? f)))
  (map future-get f*)
  (test-equal '(3 2 1) (map future-get f*))
  (shutdown-executor! e))

;; future transition regressions
(let ()
  (define worker-count 16)
  (define execute-count 0)
  (define f (make-executor-future
	     (lambda ()
	       (set! execute-count (+ execute-count 1))
	       'executed)))
  (define start (make-shared-queue 0))
  (define workers
    (map (lambda (i)
	   (make-thread
	    (lambda ()
	      (shared-queue-get! start)
	      (future-execute-task! f))))
	 (iota worker-count)))
  (future-prepare-execution! f)
  (for-each thread-start! workers)
  (for-each (lambda (i) (shared-queue-put! start i)) (iota worker-count))
  (for-each thread-join! workers)
  (test-equal "future executes once" 1 execute-count)
  (test-equal "future execute-once result" 'executed (future-get f)))

(let ()
  (define (run-once)
    (define executed 0)
    (define cancel-result #f)
    (define f (make-executor-future
	       (lambda ()
		 (set! executed (+ executed 1))
		 'ok)))
    (define start (make-shared-queue 0))
    (define executor-thread
      (make-thread
       (lambda ()
	 (shared-queue-get! start)
	 (future-execute-task! f))))
    (define cancel-thread
      (make-thread
       (lambda ()
	 (shared-queue-get! start)
	 (set! cancel-result (future-cancel f)))))
    (future-prepare-execution! f)
    (thread-start! executor-thread)
    (thread-start! cancel-thread)
    (shared-queue-put! start 'go)
    (shared-queue-put! start 'go)
    (thread-join! executor-thread)
    (thread-join! cancel-thread)
    (and (<= executed 1)
	 (if cancel-result
	     (and (future-cancelled? f)
		  (guard (e ((future-terminated? e) #t)
			    (else #f))
		    (future-get f)
		    #f))
	     (and (= executed 1)
		  (eq? (future-get f) 'ok)
		  (not (future-cancelled? f))))))
  (test-assert "cancel/startup race has a single winner"
	       (let loop ((i 0))
		 (or (= i 100)
		     (and (run-once)
			  (loop (+ i 1)))))))

(let ()
  (define started (make-shared-queue 0))
  (define release (make-shared-queue 0))
  (define f (make-executor-future
	     (lambda ()
	       (shared-queue-put! started #t)
	       (shared-queue-get! release)
	       'done)))
  (define runner (make-thread (lambda () (future-execute-task! f))))
  (future-prepare-execution! f)
  (thread-start! runner)
  (shared-queue-get! started)
  (test-assert "cancel running task" (future-cancel f))
  (shared-queue-put! release #t)
  (thread-join! runner)
  (test-assert "cancelled state is preserved" (future-cancelled? f))
  (test-error "cancelled future doesn't return completion"
	      future-terminated?
	      (future-get f)))

(let ()
  (define waiter-count 8)
  (define started (make-shared-queue 0))
  (define release (make-shared-queue 0))
  (define results (make-shared-queue))
  (define f (make-executor-future
	     (lambda ()
	       (shared-queue-put! started #t)
	       (shared-queue-get! release)
	       'all-waiters)))
  (define runner (make-thread (lambda () (future-execute-task! f))))
  (define waiters
    (map (lambda (i)
	   (make-thread
	    (lambda ()
	      (shared-queue-put! results (future-get f)))))
	 (iota waiter-count)))
  (future-prepare-execution! f)
  (thread-start! runner)
  (shared-queue-get! started)
  (for-each thread-start! waiters)
  (shared-queue-put! release #t)
  (for-each thread-join! waiters)
  (thread-join! runner)
  (let ((observed (map (lambda (i) (shared-queue-get! results))
		       (iota waiter-count))))
    (test-assert "all waiters get the same completion"
		 (for-all (lambda (v) (eq? v 'all-waiters)) observed))))

(let ()
  (define waiter-count 8)
  (define started (make-shared-queue 0))
  (define release (make-shared-queue 0))
  (define results (make-shared-queue))
  (define f (make-executor-future
	     (lambda ()
	       (shared-queue-put! started #t)
	       (shared-queue-get! release)
	       (error 'future-race "boom"))))
  (define runner (make-thread (lambda () (future-execute-task! f))))
  (define waiters
    (map (lambda (i)
	   (make-thread
	    (lambda ()
	      (guard (e (else (shared-queue-put! results 'raised)))
		(future-get f)
		(shared-queue-put! results 'returned)))))
	 (iota waiter-count)))
  (future-prepare-execution! f)
  (thread-start! runner)
  (shared-queue-get! started)
  (for-each thread-start! waiters)
  (shared-queue-put! release #t)
  (for-each thread-join! waiters)
  (thread-join! runner)
  (let ((observed (map (lambda (i) (shared-queue-get! results))
		       (iota waiter-count))))
    (test-assert "exception completion wakes all waiters"
		 (for-all (lambda (v) (eq? v 'raised)) observed)))
  (test-error "exception completion remains exceptional"
	      error?
	      (future-get f)))

(let ()
  (define executor (make-executor 2 push-future-handler))
  (define release (make-shared-queue))
  (define futures
    (map (lambda (i)
	   (future (class <executor-future>)
		   (shared-queue-get! release)))
	 (iota 6)))
  (define shutdown-thread (make-thread (lambda () (shutdown-executor! executor))))
  (define cancel-thread (make-thread (lambda () (for-each future-cancel futures))))
  (for-each (lambda (f) (execute-future! executor f)) futures)
  (thread-start! shutdown-thread)
  (thread-start! cancel-thread)
  (thread-join! shutdown-thread)
  (thread-join! cancel-thread)
  (for-each (lambda (i) (shared-queue-put! release #t)) (iota 6))
  (test-assert "shutdown race leaves no intermediate states"
	       (for-all (lambda (f)
			  (not (memq (future-state f) '(created executing))))
			futures))
  (test-assert "shutdown race resolves all futures"
	       (for-all
		(lambda (f)
		  (not (eq? (guard (e ((future-terminated? e) 'terminated)
				      (else 'error))
				  (future-get f 0 'timeout))
			    'timeout)))
		futures)))

;; actor
(let ()
  (define (actor-test make-actor)
    (define actor
      (make-actor
       (lambda (in out)
	 (let ((v (in)))
	   (out (+ v 1))))))
    (test-assert "actor? (base)" (actor? actor))
    (test-assert "actor-running? (base)" (actor-running? actor))
    (test-equal "actor state (base)" 'running (actor-state actor))
    (test-assert (actor-send-message! actor 1))
    (test-equal "actor message (base)" 2 (actor-receive-message! actor))
    ;; make sure the actor finished
    (thread-yield!)
    (thread-sleep! 0.1) ;; wait for 100ms
    (test-assert "actor-running? (2) (base)" (not (actor-running? actor)))
    (test-equal "actor state (2) (base)" 'finished (actor-state actor))
    (test-assert (actor-wait! actor)))

  (actor-test make-shared-queue-channel-actor)
  (actor-test (lambda (task)
		(make-shared-priority-queue-channel-actor
		 task
		 (lambda (a b) (cond ((= a b) 0)
				     ((< a b) -1)
				     (else    1)))))))

(let ()
  (define actor
    (make-shared-queue-channel-actor
     (lambda (in out)
       (let ((v (in)))
	 (out (+ v 1))))))
  (test-assert "terminate" (actor-terminate! actor))
  (test-assert "terminated (running check)" (not (actor-running? actor)))
  (test-equal "terminated (stete check)" 'terminated (actor-state actor)))

(let ()
  (define actor
    (make-shared-queue-channel-actor
     (lambda (in out) (in))
     #f))
  (test-equal 'created (actor-state actor))
  (test-assert (actor? (actor-start! actor)))
  (actor-start! actor)
  (test-assert (actor? (actor-start! actor)))
  (test-assert (actor-running? actor))
  (test-assert "terminate (2)" (actor-terminate! actor))
  (test-equal "terminated (state check) (2)" 'terminated (actor-state actor)))


;; future-map et.al.
(let ()
  (define test-executor (make-thread-pool-executor 1 push-future-handler))

  (define (sleeping-future n)
    (thunk->future (lambda () (thread-sleep! .1) n)
		   test-executor))

  (define (problematic n)
    (chain (sleeping-future (+ n 1))
	   (future-map/executor test-executor (lambda (n) (+ n 1)) _)))

  (test-equal 4 (chain (sleeping-future 1)
		       (future-flatmap/executor test-executor problematic _)
		       (future-map/executor test-executor (lambda (a) (+ a 1)) _)
		       (future-get _ 1 #f)))
  (shutdown-executor! test-executor))

(let ((e (make-fork-join-executor (fork-join-pool-parameters-builder
				   (max-threads 5)))))
  (dotimes (i 100) (executor-submit! e (lambda () (thread-sleep! 0.5))))
  (test-assert (<= (fork-join-executor-thread-count e)
		   (fork-join-executor-max-thread-count e)))
  (shutdown-executor! e))

(test-end)
